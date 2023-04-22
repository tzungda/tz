
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "common.h"
#include "compiler.h"
#include "scanner.h"

#ifdef DEBUG_PRINT_CODE
#include "debug.h"
#endif



typedef struct
{
    Token current;
    Token previous;
    bool hadError;
    bool panicMode;
} Parser;

typedef enum
{
    PREC_NONE,
    PREC_ASSIGNMENT,    // =
    PREC_OR,            // or
    PREC_AND,           // and
    PREC_EQUALITY,      // == !=
    PREC_COMPARISON,    // < > <= >=
    PREC_TERM,          // + -
    PREC_FACTOR,        // * /
    PREC_UNARY,         // ! -
    PREC_CALL,          // . ()
    PREC_PRIMARY
} Precedence; // precedence levels in order from lowest to highest

// (p.318)
// - the function to compile a prefix expression starting with a token of that type,
// - the function to compile an infix expression whose left operand is followed by a token of that type, and
// - the precedence of an infix expression that uses that token as an operator
typedef void (*ParseFn)( bool canAssign );
typedef struct
{
    ParseFn prefix;
    ParseFn infix;
    Precedence precedence;
} ParseRule;

typedef struct
{
    Token name;
    int depth;
} Local;

typedef enum
{
    TYPE_FUNCTION,
    TYPE_SCRIPT // for the cnode not in functions
} FunctionType; //p.437

typedef struct
{
    struct Compiler* enclosing;//p.448
    ObjFunction* function; //p.436: a reference to the function being built
    FunctionType type;
    Local locals[UINT8_COUNT];
    int localCount;
    int scopeDepth;
} Compiler;

Parser parser;
Compiler* current = NULL; 

static Chunk* currentChunk()
{
    return &current->function->chunk;
}

static void errorAt( Token* token, const char* message )
{
    if( parser.panicMode ) return;
    parser.panicMode = true;
    fprintf( stderr, "[line %d] Error", token->line );
    
    if ( token->type == TOKEN_EOF )
    {
        fprintf( stderr, " at end" );
    }
    else if ( token->type == TOKEN_ERROR )
    {
        // nothing
    }
    else
    {
        fprintf( stderr, " at '%.*s'", token->length, token->start );
    }
    
    fprintf( stderr, ": %s\n", message );
    parser.hadError = true;
}

static void error( const char* message )
{
    errorAt( &parser.previous, message );
}

static void errorAtCurrent( const char* message )
{
    errorAt( &parser.current, message );
}

static void advance()
{
    parser.previous = parser.current;
    for( ; ; )
    {
        parser.current = scanToken();
        if( parser.current.type != TOKEN_ERROR ) break;
        
        errorAtCurrent( parser.current.start );
    }
}

static void consume( TokenType type, const char* message )
{
    if( parser.current.type == type )
    {
        advance();
        return;
    }
    
    errorAtCurrent( message );
}

static bool check( TokenType type )
{
    return parser.current.type == type;
}

static bool match( TokenType type )
{
    if ( !check( type ) )
        return false;
    advance();
    return true;
}

static void parsePrecedence( Precedence precedence );
static void initCompiler( Compiler* compiler, FunctionType type );
static uint8_t identifierConstant( Token* name ); //p.388
static bool identifiersEqual( Token* a, Token* b ); //p.406
static int resolveLocal( Compiler* compiler, Token* name );// p.408
static uint8_t parseVariable( const char* errorMessage ); //p.388
static void defineVariable( uint8_t global ); // p.389
static uint8_t argumentList( ); //p.450
static void expression();
static void varDeclaration();//p.388
static void expressionStatement();
static void forStatement();//p.425
static void ifStatement( );//p.415
static void printStatement();
static void returnStatement( );
static void whileStatement();
static void synchronize();
static void statement();
static void declaration();
static ParseRule* getRule( TokenType type );
static ObjFunction* endCompiler( );
static void beginScope();
static void endScope();

ObjFunction* compile( const char* source )
{
    initScanner( source );
    Compiler compiler;
    initCompiler( &compiler, TYPE_SCRIPT );
    
    parser.hadError = false;
    parser.panicMode = false;
    
    advance();
    
    while( !match( TOKEN_EOF ) )
    {
        declaration();
    }

    ObjFunction* function = endCompiler();
    return parser.hadError ? NULL : function;
}

static void emitByte( uint8_t byte )
{
    // this sends in the previous token's line information so that runtime errors are associated with that line( p.311 )
    writeChunk( currentChunk(), byte, parser.previous.line );
}

static void emitLoop( int loopStart ) //p.423
{
    // emit a new loop instruction, which unconditionally jumps 'backwards' by a given offset
    emitByte( OP_LOOP );

    // the +2 is to take into account the size of the OP_LOOP instruction's own operands which we also need to jump over
    int offset = currentChunk()->count - loopStart + 2;
    if( offset > UINT16_MAX )
        error( "Loop body too large." );

    emitByte( (offset >> 8) & 0xff );
    emitByte( offset & 0xff );
}

static void emitBytes( uint8_t byte1, uint8_t byte2 )
{
    emitByte( byte1 );
    emitByte( byte2 );
}

static int emitJump( uint8_t instruction ) //p.416
{
    // emits a bytecode instruction and writes a placeholder operand for the jump offset
    //
    emitByte( instruction );
    // use two bytes for the jump offset operand. 
    // a 16-bit offset let us jump over up to 65,535 bytes of code
    emitByte( 0xff ); // 0xff is 8 bits
    emitByte( 0xff );

    // returns the offset of the emitted instruction in the chunk.
    return currentChunk()->count - 2;
}

static void emitReturn()
{
    //p.457 The compiler calls emitReturn() to write the OP_RETURN instruction at the end of a function body
    // Now, before that, it emits an instruction to push nil onto the stack.
    emitByte( OP_NIL );
    emitByte( OP_RETURN );
}

static uint8_t makeConstant( Value value )
{
    int constant = addConstant( currentChunk(), value );
    if( constant > UINT8_MAX )
    {
        error( "Too many constants in one chunk." );
        return 0;
    }
    
    return ( uint8_t )constant;
}

static void emitConstant( Value value )
{
    emitBytes( OP_CONSTANT, makeConstant( value ) );
}

// p.416 After compiling the then branch, we take that offset and pass it to this
// we call patchJump right before we emit the next instruction that we want the jump the land on
static void patchJump( int offset )
{
    // -2 to adjust for the bytecode for the jump offset itself
    int jump = currentChunk()->count - offset - 2;

    if ( jump > UINT16_MAX )
    {
        error( "Too much code to jump over." );
    }

    // goes back into the bytecode and replaces the operand at the given location with the calculated jump offset
    uint8_t tmp = ( jump >> 8 ) & 0xff;
    currentChunk()->code[offset] = tmp;
    tmp = jump & 0xff;
    currentChunk()->code[offset + 1] = tmp;
}

static void initCompiler( Compiler* compiler, FunctionType type )
{
    compiler->enclosing = current; //p.448 capture the about-to-no-longer-be-current one in that pointer

    compiler->function = NULL; //p.437
    compiler->type = type; 

    compiler->localCount = 0;
    compiler->scopeDepth = 0;

    compiler->function = newFunction(); //p.437 garbage collection-related paranoia

    current = compiler;
    if( type != TYPE_SCRIPT )
    {
        current->function->name = copyString( parser.previous.start, parser.previous.length );
    }

    // p.438
    Local* local = &current->locals[current->localCount++];
    local->depth = 0;
    local->name.start = "";
    local->name.length = 0;
}

static ObjFunction* endCompiler( )
{
    emitReturn();
    ObjFunction* function = current->function;//p.438 
#ifdef DEBUG_PRINT_CODE
    if( !parser.hadError )
    {
        disassembleChunk( currentChunk(), function->name != NULL ? function->name->chars : "<script>");
    }
#endif

    current = current->enclosing;//p.448 when a compiler finishes, it pops itself off the stack by restoring the previous compiler to the new current one
    return function;//p.438 the compile creates the function object itself, we return that function
}

static void beginScope()
{
    current->scopeDepth++;
}

static void endScope()
{
    current->scopeDepth--;

    while( current->localCount > 0 && 
        current->locals[current->localCount - 1].depth > current->scopeDepth )
    {
        emitByte( OP_POP );
        current->localCount--;
    }
}

static void binary( bool canAssign )
{
    TokenType operatorType = parser.previous.type;
    ParseRule* rule = getRule( operatorType );
    parsePrecedence( (Precedence)( rule->precedence + 1 ) );

    switch ( operatorType )
    {
    case TOKEN_BANG_EQUAL:      emitBytes( OP_EQUAL, OP_NOT ); break; // != is equal to !( == )
    case TOKEN_EQUAL_EQUAL:     emitByte( OP_EQUAL ); break;
    case TOKEN_GREATER:         emitByte( OP_GREATER ); break;
    case TOKEN_GREATER_EQUAL:   emitBytes( OP_LESS, OP_NOT ); break; // >= is equal to !( < )
    case TOKEN_LESS:            emitByte( OP_LESS ); break; 
    case TOKEN_LESS_EQUAL:      emitBytes( OP_GREATER, OP_NOT ); break; // <= is equal to !( > )
    case TOKEN_PLUS:            emitByte( OP_ADD ); break;
    case TOKEN_MINUS:           emitByte( OP_SUBTRACT ); break;
    case TOKEN_STAR:        emitByte( OP_MULTIPLY ); break;
    case TOKEN_SLASH:       emitByte( OP_DIVIDE ); break;
    }
}

static void call( bool canAssign ) //p.450
{
    uint8_t argCount = argumentList();
    emitBytes( OP_CALL, argCount ); // OP_CALL here is to invoke the function
}

static void literal( bool canAssign )
{
    switch ( parser.previous.type )
    {
    case TOKEN_FALSE:   emitByte( OP_FALSE ); break;
    case TOKEN_NIL:     emitByte( OP_NIL ); break;
    case TOKEN_TRUE:    emitByte( OP_TRUE ); break;
    default: return;// unreachable
    }
}

static void grouping( bool canAssign )
{
    expression();
    consume( TOKEN_RIGHT_PAREN, "Expect ')' after expression." );
}

static void number( bool canAssign )
{
    double value = strtod( parser.previous.start, NULL );
    emitConstant( NUMBER_VAL( value ) );
}

static void string( bool canAssign )
{
    emitConstant( OBJ_VAL( copyString( parser.previous.start + 1, parser.previous.length - 2 ) ) );
}

static void namedVariable( Token name, bool canAssign )
{
    uint8_t getOp, setOp;
    int arg = resolveLocal( current, &name );
    if ( arg != -1 )
    {
        getOp = OP_GET_LOCAL;
        setOp = OP_SET_LOCAL;
    }
    else
    {
        arg = identifierConstant( &name );
        getOp = OP_GET_GLOBAL;
        setOp = OP_SET_GLOBAL;
    }
    
    if ( canAssign && match( TOKEN_EQUAL ) )
    {
        expression();
        emitBytes( setOp, (uint8_t)arg );
    }
    else
    {
        emitBytes( getOp, (uint8_t)arg );
    }
}

static void variable( bool canAssign )
{
    namedVariable( parser.previous, canAssign );
}

static void unary( bool canAssign )
{
    TokenType operatorType = parser.previous.type;
    
    // compile the operand
    parsePrecedence( PREC_UNARY );
    
    // emit the operator instruction.
    switch( operatorType )
    {
    case TOKEN_BANG: emitByte( OP_NOT ); break;
    case TOKEN_MINUS: emitByte( OP_NEGATE ); break;
    default: return; // unreachable
    }
}

static void and_( bool canAssign ) // p.420
{
    int endJump = emitJump( OP_JUMP_IF_FALSE );

    emitByte( OP_POP );
    parsePrecedence( PREC_AND );

    patchJump( endJump );
}

static void or_( bool canAssign ) // p.421
{
    int elseJump = emitJump( OP_JUMP_IF_FALSE );
    int endJump = emitJump( OP_JUMP );

    patchJump( elseJump );
    emitByte( OP_POP );

    parsePrecedence( PREC_OR );
    patchJump( endJump );
}

// the table that drives whole parser is an array of ParseRules(p.319)
ParseRule rules[] = 
{
    [TOKEN_LEFT_PAREN]      = { grouping, call, PREC_CALL },
    [TOKEN_RIGHT_PAREN]     = { NULL, NULL, PREC_NONE},
    [TOKEN_LEFT_BRACE]      = { NULL, NULL, PREC_NONE},
    [TOKEN_RIGHT_BRACE]     = { NULL, NULL, PREC_NONE},
    [TOKEN_COMMA]           = { NULL, NULL, PREC_NONE},
    [TOKEN_DOT]             = { NULL, NULL, PREC_NONE},
    [TOKEN_MINUS]           = { unary, binary, PREC_TERM},
    [TOKEN_PLUS]            = { NULL, binary, PREC_TERM},
    [TOKEN_SEMICOLON]       = { NULL, NULL, PREC_NONE},
    [TOKEN_SLASH]           = { NULL, binary, PREC_FACTOR},
    [TOKEN_STAR]            = { NULL, binary, PREC_FACTOR},
    [TOKEN_BANG]            = { unary, NULL, PREC_NONE},
    [TOKEN_BANG_EQUAL]      = { NULL, binary, PREC_EQUALITY},
    [TOKEN_EQUAL]           = { NULL, NULL, PREC_NONE},
    [TOKEN_EQUAL_EQUAL]     = { NULL, binary, PREC_EQUALITY},
    [TOKEN_GREATER]         = { NULL, binary, PREC_COMPARISON},
    [TOKEN_GREATER_EQUAL]   = { NULL, binary, PREC_COMPARISON},
    [TOKEN_LESS]            = { NULL, binary, PREC_COMPARISON},
    [TOKEN_LESS_EQUAL]      = { NULL, binary, PREC_COMPARISON},
    [TOKEN_IDENTIFIER]      = { variable, NULL, PREC_NONE},
    [TOKEN_STRING]          = { string, NULL, PREC_NONE},
    [TOKEN_NUMBER]          = { number, NULL, PREC_NONE},
    [TOKEN_AND]             = { NULL, and_, PREC_NONE},
    [TOKEN_CLASS]           = { NULL, NULL, PREC_NONE},
    [TOKEN_ELSE]            = { NULL, NULL, PREC_NONE},
    [TOKEN_FALSE]           = { literal, NULL, PREC_NONE},
    [TOKEN_FOR]             = { NULL, NULL, PREC_NONE},
    [TOKEN_FUN]             = { NULL, NULL, PREC_NONE},
    [TOKEN_IF]              = { NULL, NULL, PREC_NONE},
    [TOKEN_NIL]             = { literal, NULL, PREC_NONE},
    [TOKEN_OR]              = { NULL, or_, PREC_NONE},
    [TOKEN_PRINT]           = { NULL, NULL, PREC_NONE},
    [TOKEN_RETURN]          = { NULL, NULL, PREC_NONE},
    [TOKEN_SUPER]           = { NULL, NULL, PREC_NONE},
    [TOKEN_THIS]            = { NULL, NULL, PREC_NONE},
    [TOKEN_TRUE]            = { literal, NULL, PREC_NONE},
    [TOKEN_VAR]             = { NULL, NULL, PREC_NONE},
    [TOKEN_WHILE]           = { NULL, NULL, PREC_NONE},
    [TOKEN_ERROR]           = { NULL, NULL, PREC_NONE},
    [TOKEN_EOF]             = { NULL, NULL, PREC_NONE},
};

static void parsePrecedence( Precedence precedence )
{
    advance();
    ParseFn prefixRule = getRule( parser.previous.type )->prefix;
    if ( prefixRule == NULL )
    {
        error( "Expect expression." );
        return;
    }

    bool canAssign = precedence <= PREC_ASSIGNMENT; //p.394
    prefixRule( canAssign );

    while ( precedence <= getRule( parser.current.type )->precedence )
    {
        advance();
        ParseFn infixRule = getRule( parser.previous.type )->infix;
        infixRule( canAssign );
    }

    if ( canAssign && match( TOKEN_EQUAL ) )
    {
        error( "Invalid assignment target." );
    }
}

static uint8_t identifierConstant( Token* name )
{
    return makeConstant( OBJ_VAL( copyString( name->start, name->length ) ) );
}

static bool identifiersEqual( Token* a, Token* b ) //p.406
{
    if ( a->length != b->length ) 
        return false;

    return ( memcmp( a->start, b->start, a->length ) == 0 );
}

static int resolveLocal( Compiler* compiler, Token* name )
{
    // - walk the array backward so taht we find the 'last' declared variable with the identifier
    // - locals array in the compiler has the 'exact' same layout as the VM's stack will have at runtime
    for ( int i = compiler->localCount - 1; i >=0; i-- )
    {
        Local* local = &compiler->locals[i];
        if ( identifiersEqual( name, &local->name ) )
        {
            if ( local->depth == -1 ) // p.411
            {
                error( "Can't read local variable in its own initializer." );
            }
            return i;
        }
    }
    return -1;
}

static void addLocal( Token name ) // p.405
{
    if ( current->localCount == UINT8_COUNT )
    {
        error( "Too many local variables in function." );
        return;
    }
    Local* local = &current->locals[current->localCount++];
    local->name = name;

    // p.411
    // - If at any point in that expression we reolve an identifier that points back to this variable,
    // we'll see that it is not initialized yet and report an error
    // - When we declare a local, we need to indicate the "uninitialized" state somehow
    // we'll set the variable's scope depth to a special sentinel value, -1.
    local->depth = -1;
}

static void declareVariable() // p.405
{
    if ( current->scopeDepth == 0 )
        return;

    Token* name = &parser.previous;
    // p.406: start at the end and work backward, for the current scope is always at the end of the array
    for ( int i = current->localCount - 1; i >= 0; i-- )
    {
        Local* local = &current->locals[i];
        if ( local->depth != -1 && local->depth < current->scopeDepth )
        {
            break;
        }

        if ( identifiersEqual( name, &local->name ) )
        {
            error( "Already a variable with this name in this scope." );
        }
    }

    addLocal( *name );
}

static uint8_t parseVariable( const char* errorMessage ) //p.388
{
    consume( TOKEN_IDENTIFIER, errorMessage );

    declareVariable( );
    if ( current->scopeDepth > 0 ) 
        return 0;

    return identifierConstant( &parser.previous );
}

static void markInitialized()
{
    if( current->scopeDepth == 0 ) return;
    current->locals[current->localCount - 1].depth = current->scopeDepth;
}

static void defineVariable( uint8_t global ) // p.389
{
    if( current->scopeDepth > 0 ) // p.404
    {
        markInitialized( );
        return;
    }

    emitBytes( OP_DEFINE_GLOBAL, global );
}

static uint8_t argumentList( ) //p.450
{
    uint8_t argCount = 0;
    if( !check( TOKEN_RIGHT_PAREN ) )
    {
        do 
        {
            expression();
            if ( argCount == 255 )
            {
                error( "Can't have more than 255 arguments." );
            }
            argCount++;
        } while( match( TOKEN_COMMA ) );
    }

    consume( TOKEN_RIGHT_PAREN, "Expect ')' after arguments." );
    return argCount;
}

static ParseRule* getRule( TokenType type )
{
    return &rules[type];
}

static void expression()
{
    // (p.316) simply parse the lowest precedence level, 
    // which subsumes all of the higher-precedence expressions too
    parsePrecedence( PREC_ASSIGNMENT );
}

static void varDeclaration( ) // p.388
{
    uint8_t global = parseVariable( "Expect variable name." );

    if ( match( TOKEN_EQUAL ) )
    {
        expression();
    }
    else
    {
        emitByte( OP_NIL );
    }
    consume( TOKEN_SEMICOLON, "Expect ';' after variable declaration." );

    defineVariable( global );
}

static void expressionStatement() // p.386
{
    expression();
    consume( TOKEN_SEMICOLON, "Expect ';' after expression." );
    emitByte( OP_POP );
}

static void forStatement() //p.425
{
    // if a 'for' statement declares a variable, that variable should be scoped to the loop body
    beginScope();

    consume( TOKEN_LEFT_PAREN, "Expect '(' after 'for'." );

    // we allow eith a variable declaration or an expression
    if( match( TOKEN_SEMICOLON ) )
    {
        // no initializer
    }
    else if ( match( TOKEN_VAR ) )
    {
        varDeclaration();
    }
    else
    {
        // here we call expressionStatement() instead of expression(). That looks for a semicolon, which we need here too,
        // and also emits an OP_POP instruction to discard the value. We don't want the initializer to leave anything on the stack.
        expressionStatement();
    }

    int loopStart = currentChunk()->count;
    int exitJump = -1;// for the condition expression that can be used to exit the loop
    if( !match( TOKEN_SEMICOLON ) )
    {
        expression();
        consume( TOKEN_SEMICOLON, "Expect ';' after loop condition." );

        // jump out of the loop if the condition is false
        exitJump = emitJump( OP_JUMP_IF_FALSE );
        emitByte( OP_POP ); // condition
    }
    
    // for increment clause
    if ( !match( TOKEN_RIGHT_PAREN ) ) // p.427
    {
        // first we imit an unconditional jump that hops over the increment clause's code to the body of the loop
        int bodyJump = emitJump( OP_JUMP );

        // next we compile the increment expression itself. this is usually an assignment
        int incrementStart = currentChunk()->count;
        expression();
        // we only execute it for its side-effect, so we also emit a pop to discard its value
        emitByte( OP_POP );
        consume( TOKEN_RIGHT_PAREN, "Expect ')' after for clauses." );

        // emit the loop instruction. this is the main loop that takes us back to the top of the 'for' loop
        // right before the condition expression if there is one
        // ( the increment executes at the end of each loop iteration )
        emitLoop( loopStart );
        // then we change loopStart to point to the offset where the increment expression begins.
        // later, when we emit the loop instruction after the body statement, this will cause it jump up to
        // the 'increment' expression instead of the top of the loop like it does when there is no increment
        loopStart = incrementStart;
        patchJump( bodyJump );
    }

    statement();
    emitLoop( loopStart );

    if ( exitJump != -1 ) //<- check there is a condition clause
    {
        patchJump( exitJump );
        emitByte( OP_POP ); //condition
    }

    //
    endScope();
}

static void ifStatement( )
{
    consume( TOKEN_LEFT_PAREN, "Expect '(' after 'if'.");
    expression();
    consume( TOKEN_RIGHT_PAREN, "Expect ')' after condition." );

    // p.415: Emit a new OP_JUMP_IF_FALSE instruction. It has an operand for how much to offset the ip
    // i.e. how many bytes of code to skip.
    int thenJump = emitJump( OP_JUMP_IF_FALSE );
    //p.419 when the condition is truthy, we pop it right before the code inside the then branch
    emitByte( OP_POP );
    statement();

    //we need another jump from the end of the then branch
    int elseJump = emitJump( OP_JUMP );

    patchJump( thenJump );
    //p.419 Otherwise, we pop it at the beginning of the else branch.
    // every 'if' statement has an implicit else branch even if the user didn't write and 'else' clause
    emitByte( OP_POP );

    // compile the else keyword if we find one
    if( match( TOKEN_ELSE ) )
        statement();
    patchJump( elseJump );
}

static void block( )
{
    while ( !check( TOKEN_RIGHT_BRACE ) && !check( TOKEN_EOF ) )
    {
        declaration();
    }

    consume( TOKEN_RIGHT_BRACE, "Expect '}' after block." );
}

static void function( FunctionType type ) //p.447
{
    Compiler compiler;
    initCompiler( &compiler, type );
    beginScope();

    consume( TOKEN_LEFT_PAREN, "Expect '(' after function name." );
    if ( !check( TOKEN_RIGHT_PAREN ) )
    {
        do 
        {
            current->function->arity++;
            if( current->function->arity > 255 )
            {
                errorAtCurrent( "Can't have more than 255 parameters." );
            }
            uint8_t constant = parseVariable( "Expect parameter name." );
            defineVariable( constant );
        }while( match( TOKEN_COMMA ) );
    }
    consume( TOKEN_RIGHT_PAREN, "Expect ')' after parameters." );
    consume( TOKEN_LEFT_BRACE, "Expect '{' before function body." );
    block();

    ObjFunction* function = endCompiler();
    emitBytes(  OP_CONSTANT, makeConstant( OBJ_VAL(function) ) );
}

static void funDeclaration() //p.446
{
    uint8_t global = parseVariable( "Expect function name." );
    markInitialized(); // mark the function declaration's variable "initialized" as soon as we compile the name
    function( TYPE_FUNCTION );
    defineVariable( global );
}

static void printStatement()
{
    expression();
    consume( TOKEN_SEMICOLON, "Expect ';' after value." );
    emitByte( OP_PRINT );
}

static void returnStatement( ) //p.457
{
    if ( current->type == TYPE_SCRIPT )
    {
        error( "Can't return from top-level code." );
    }

    if ( match( TOKEN_SEMICOLON ) )
    {
        emitReturn( );
    }
    else
    {
        expression();
        consume( TOKEN_SEMICOLON, "Expect ';' after return values." );
        emitByte( OP_RETURN );
    }
}

static void whileStatement()//p.422
{
    //capture the loopStart(right before the condition expression) position.
    // We store the chunk's current instruction count in loopStart to record the offset in the bytecode right before
    // the condition expression we're about the compile
    int loopStart = currentChunk()->count;

    consume( TOKEN_LEFT_PAREN, "Expect '(' after 'while'. " );
    expression( );
    consume( TOKEN_RIGHT_PAREN, "Expect ')' after condition. " );

    int exitJump = emitJump( OP_JUMP_IF_FALSE );
    emitByte( OP_POP );
    statement();

    // the only difference from an 'if' statement is the loop
    emitLoop( loopStart );

    patchJump( exitJump );
    emitByte( OP_POP );
}

static void synchronize() // p.387
{
    parser.panicMode = false;

    while( parser.current.type != TOKEN_EOF )
    {
        if ( parser.previous.type == TOKEN_SEMICOLON )
            return;
        switch( parser.current.type )
        {
        case TOKEN_CLASS:
        case TOKEN_FUN:
        case TOKEN_VAR:
        case TOKEN_FOR:
        case TOKEN_IF:
        case TOKEN_WHILE:
        case TOKEN_PRINT:
        case TOKEN_RETURN:
            return;
        default:
            ; // do nothing
        }

        advance();
    }
}

static void declaration()
{
    if ( match( TOKEN_FUN ) )
    {
        funDeclaration( );
    }
    else if ( match( TOKEN_VAR ) )
    {
        varDeclaration();
    }
    else
    {
        statement();
    }

    if( parser.panicMode )
        synchronize();
}

static void statement()
{
    if( match( TOKEN_PRINT ) )
    {
        printStatement();
    }
    else if ( match( TOKEN_FOR ) )
    {
        forStatement();
    }
    else if ( match( TOKEN_IF ) )
    {
        ifStatement(); //p.414
    }
    else if ( match( TOKEN_RETURN ) )//p.457
    {
        returnStatement( );
    }
    else if ( match( TOKEN_WHILE ) ) //p.422
    {
        whileStatement();
    }
    else if( match( TOKEN_LEFT_BRACE ) ) // p.403
    {
        beginScope( );
        block();
        endScope();
    }
    else
    {
        expressionStatement( );
    }
}


