
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "common.h"
#include "compiler.h"
#include "memory.h"
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
    bool isCaptured;//p.486
} Local;

//p.474
typedef struct
{
    uint8_t index;
    bool isLocal;
} Upvalue;

typedef enum
{
    TYPE_FUNCTION,
    //p.558 a new function type to distinguish initializers from other methods
    TYPE_INITIALIZER,
    //p.553 To decide what name to give to local slot zero, the compiler needs to know whether it's compiling a function or method declaration,
    // so we add a new case to our FunctionType enum to distinguish methods.
    TYPE_METHOD,
    TYPE_SCRIPT // for the cnode not in functions
} FunctionType; //p.437

typedef struct Compiler
{
    struct Compiler* enclosing;//p.448
    ObjFunction* function; //p.436: a reference to the function being built
    FunctionType type;
    Local locals[UINT8_COUNT];
    int localCount;
    //p.474 the OP_GET_UPVALUE and OP_SET_UPVALUE instructions encode an upvalue index using a single byte operand,
    // so there is a restriction on how many upvalues a function can have -- how many unique variables it can close over
    Upvalue upvalues[UINT8_COUNT];
    int scopeDepth;
} Compiler;

//p.554
typedef struct ClassCompiler
{
    struct ClassCompiler* enclosing;
    //p.573
    bool hasSuperclass;
} ClassCompiler;

Parser parser;
Compiler* current = NULL; 
//p.554 This module variable points to a struct representing the current, innermost class being compiled.
// If we aren't inside any class declaration at all, the module variable currentClass is NULL.
ClassCompiler* currentClass = NULL;

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
static int resolveUpvalue( Compiler* compiler, Token* name );//p.472

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

void markCompilerRoots( ) //p.508
{
    Compiler* compiler = current;
    while( compiler != NULL )
    {
        markObject( (Obj*)compiler->function );
        compiler = compiler->enclosing;
    }
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
    // p.558 Whenever the compiler emits the implicit return at the end of a body, we check the type to decide 
    // whether to insert the initializer-specific behavior
    if ( current->type == TYPE_INITIALIZER )
    {
        emitBytes( OP_GET_LOCAL, 0 );
    }
    else
    {
        emitByte( OP_NIL );
    }
    //p.457 The compiler calls emitReturn() to write the OP_RETURN instruction at the end of a function body
    // Now, before that, it emits an instruction to push nil onto the stack.
    //emitByte( OP_NIL );
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
    local->isCaptured = false;//p.486
    //p.552 In order to compile [this] expressions, the compiler simply needs to give the correct name to that local variable.
    if ( type != TYPE_FUNCTION )
    {
        local->name.start = "this";
        local->name.length = 4;
    }
    else
    {
        local->name.start = "";
        local->name.length = 0;
    }
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
        // p.486 the instruction requires no operand. 
        // we know that that variable will always be right on top of the stack at the point that this 
        if ( current->locals[current->localCount - 1].isCaptured )
        {
            emitByte( OP_CLOSE_UPVALUE );
        }
        else
        {
            emitByte( OP_POP );
        }

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

static void dot( bool canAssign ) //p.535
{
    // The parser expects to find a property name immediately after the dot
    consume( TOKEN_IDENTIFIER, "Expect property name after '.'." );
    uint8_t name = identifierConstant( &parser.previous );

    // if we see an equals sign after the field name, it must be a set expression that is assigning to a field
    if( canAssign && match( TOKEN_EQUAL ) )
    {
        expression( );
        emitBytes( OP_SET_PROPERTY, name );
    }
    //p.560 After the compiler has parsed the property name, we look for a left paranthesis.
    // If we match one, we switch to a new code path.
    else if ( match( TOKEN_LEFT_PAREN ) )
    {
        uint8_t argCount = argumentList();
        // It takes two operands
        // (In other words, this single instruction combines the operands of the OP_GET_PROPERTY and OP_CALL instructions it replaces, in that order)
        //1. The index of the property name in the constant table
        emitBytes( OP_INVOKE, name );
        //2. The number of arguemnt passed to the method
        emitByte( argCount );
    }
    else
    {
        emitBytes( OP_GET_PROPERTY, name );
    }
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
    else if ( ( arg = resolveUpvalue( current, &name ) ) != -1 )
    {
        getOp = OP_GET_UPVALUE;
        setOp = OP_SET_UPVALUE;
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

//p.573 a little helper function to create a synthetic token for the given constant string
static Token syntheticToken( const char* text )
{
    Token token;
    token.start = text;
    token.length = (int)strlen( text );
    return token;
}

//p.574 
static void super_( bool canAssign )
{
    // p.576 A super call is meaningful only inside the body of a method, 
    // and only inside the method of a class that has a superclass
    if ( currentClass == NULL )
    {
        error( "Can't use 'super' outside of a class." );
    }
    else if ( !currentClass->hasSuperclass )
    {
        error( "Can't use 'super' in a class with no superclass." );
    }

    // a super call begins, naturally enough, with the super keyword.
    consume( TOKEN_DOT, "Expect '.' after 'super'." );
    // unlike [this], and [super] token is not a standalone expression.
    // when the compiler hits a [super] token, we consume the subsequent . token and then look for a method name.
    consume( TOKEN_IDENTIFIER, "Expect superclass method name." );
    uint8_t name = identifierConstant( &parser.previous );

    //p.575
    // In order to access a superclass method on the current instance, the runtime needs both the receiver and the superclass of the surrounding method's class.
    // The first namedVariable() call generates code to look up the current receiver stored in the hidden variable "this" and push it onto the stack.
    // The second namedVariable() call emits code to look up the superclass from its "super" variable and push that on top
    namedVariable( syntheticToken("this"), false );
    //p.578 before we emit anything, we look for a parenthesized argument list. If we find one, we compile that.
    // Then we load the superclass. After that, we emit a new OP_SUPER_INVOKE instruction. This superinstruction combines
    // the behavior of OP_GET_SUPER and OP_CALL, so it take two operands: the constant table index of the method name
    // to look up and the number of arguments to pass to it
    if ( match( TOKEN_LEFT_PAREN ) )
    {
        uint8_t argCount = argumentList();
        namedVariable( syntheticToken("super"), false );
        emitBytes( OP_SUPER_INVOKE, name );
        emitByte( argCount );
    }
    else // otherwise, if we don't find a '(', we continue to compile the expression as a super access like we did before and emit an OP_GET_SUPER
    {
        namedVariable( syntheticToken( "super" ), false );
        emitBytes( OP_GET_SUPER, name );
    }

}

static void this_( bool canAssign ) //p.551
{
    //p.555 to see if we are inside a class - and therefore inside a method - we simply check that module variable.
    if( currentClass == NULL )
    {
        error( "Can't use 'this' outside of a class." );
        return;
    }

    // When the parser function is called, the [this] token has just been consumed and is stored as the previous token.
    // We call our existing variable() function which compiles identifier expressions as variable accesses.
    // You can't assign the [this], so we pass 'false' to disallow that.
    variable( false );
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
    [TOKEN_DOT]             = { NULL, dot, PREC_CALL},//p.535 precedece as high as the parenthese
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
    [TOKEN_SUPER]           = { super_, NULL, PREC_NONE},//p.574
    [TOKEN_THIS]            = { this_, NULL, PREC_NONE},//p.551
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

static int addUpvalue( Compiler* compiler, uint8_t index, bool isLocal )//p.473
{
    int upvalueCount = compiler->function->upvalueCount;

    if ( upvalueCount == UINT8_COUNT )
    {
        error( "Too many closure variables in function." );
        return 0;
    }

    // we first check to see if the function already has an upvalue that closes over that variable
    for( int i = 0; i < upvalueCount; i++ )
    {
        Upvalue* upvalue = &compiler->upvalues[i];
        if ( upvalue->index == index && upvalue->isLocal == isLocal )
        {
            return i;
        }
    }

    compiler->upvalues[upvalueCount].isLocal = isLocal;
    compiler->upvalues[upvalueCount].index = index;
    return compiler->function->upvalueCount++;
}

static int resolveUpvalue( Compiler* compiler, Token* name ) //p.472
{
    if( compiler->enclosing == NULL )
        return -1;

    int local = resolveLocal( compiler->enclosing, name );
    if( local != -1 )
    {
        // p.486
        // if we end up creating an upvalue for a local variable, we mark itas captured
        compiler->enclosing->locals[local].isCaptured = true;

        return addUpvalue( compiler, (uint8_t)local, true );
    }

    //p.477
    int upvalue = resolveUpvalue( compiler->enclosing, name );
    if( upvalue != -1 )
        return addUpvalue( compiler, (uint8_t)upvalue, false );

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

    // p.486
    local->isCaptured = false;
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
    emitBytes( OP_CLOSURE/*p.467*/, makeConstant( OBJ_VAL(function) ) );

    //p.478 The OP_CLOSURE instruction is unique in that it has a variably sized encoding.
    // For each upvalue the closure captures, there are two single-byte operands. 
    // Each pair of operands specifies what that upvalue captures
    for ( int i = 0; i < function->upvalueCount; i++ )
    {
        emitByte( compiler.upvalues[i].isLocal ? 1 : 0 );
        emitByte( compiler.upvalues[i].index );
    }
}

//p.543 
static void method( )
{
    consume( TOKEN_IDENTIFIER, "Expect method name." );
    // Like OP_GET_PROPERTY and other instructions that need names at runtime,
    // the compiler adds the method name token's lexeme to the constant table, getting back a table index
    uint8_t constant = identifierConstant( &parser.previous );

    FunctionType type = TYPE_METHOD;//p.553 when we compile a method, we use that type
    // p.558 We detect that by checking to see if the name of the method we're compiling is "init"
    if( parser.previous.length == 4 && memcmp( parser.previous.start, "init", 4 ) == 0 )
    {
        type = TYPE_INITIALIZER;
    }

    function( type );// closure for the method body

    // name of the method
    emitBytes( OP_METHOD, constant );
}

//p.530
static void classDeclaration()
{
    //immediately afer the class keyword is the class's name
    consume( TOKEN_IDENTIFIER, "Expect class name." );
    //p.544 The compiler does know the name of the class. We can capture it right after we consume its token
    Token className = parser.previous;
    //the compiler needs to stuff the name string somewhere that the runtime can find
    uint8_t nameConstant = identifierConstant( &parser.previous );
    //the class's name is also used to bind the class object to a variable of the same name
    declareVariable();

    //emit a new instruction to actually create the class object at runtime
    emitBytes( OP_CLASS, nameConstant );

    //Declaring the variable adds it to the scope, but recall from a previous chapter that we can't use
    //the variable until it's defined.
    defineVariable( nameConstant );

    //p.555 The memory for the ClassCompiler struct livess right on the C stack, 
    // a handy capability we get by writing our compiler using recursive descent.
    ClassCompiler classCompiler;
    classCompiler.hasSuperclass = false;//p.573
    classCompiler.enclosing = currentClass;
    currentClass = &classCompiler;

    // p.568
    if ( match( TOKEN_LESS ) )
    {
        // After we compile the class name, if the next token is a <, then we found a supperclass clause
        consume( TOKEN_IDENTIFIER, "Expect superclass name." );
        // We consume the superclass's identifier token, then call variable().
        // The function takes the previously consumed token, treats it as a variable reference, and emits code to loadthe variable's value
        // In other words, it looks up the superclass by name and pushes it onto the stack.
        variable( false );

        //p.569
        if ( identifiersEqual( &className, &parser.previous ) )
        {
            error( "A class can't inherit from itself." );
        }

        //p.572 we create a new scope and make it a local variable.
        // Createing a new lexical scope ensures that if we declare two classes in the same scope,
        // each has a different local slot to store its superclass.
        beginScope();
        addLocal( syntheticToken( "super" ) );
        defineVariable( 0 );

        // we call namedVariable() to load the subclass doing the inheritng onto the stack, followed by an OP_INHERIT instruction
        namedVariable( className, false );
        // That instruction wires up the superclass to the new subclass.
        // The OP_INHERIT instruction takes an existing class and applies the effect of inheritance to it.
        emitByte( OP_INHERIT );
        // p.574 if we see a superclass clause, we know we are compiling a subclass
        classCompiler.hasSuperclass = true;
    }

    //p.544
    // Before we start binding methods, we emit whatever code is necessary to load the class back on top of the stack
    // This helper function generates code to load a variable with the given name onto the stack. 
    // Then we compile the methods
    namedVariable( className, false );

    // compile the body
    consume( TOKEN_LEFT_BRACE, "Expect '{' before class body." );
    //p.543 anything before the closing brace at the end of the class body must be a method
    while ( !check( TOKEN_RIGHT_BRACE ) && !check( TOKEN_EOF ) )
    {
        method();
    }
    consume( TOKEN_RIGHT_BRACE, "Expect '}' before class body." );

    //p.544 Once we've reached the end of the methods, we no longer need the class and tell the VM to pop it off the stack
    emitByte( OP_POP );

    //p.573 we pop the scope and discard the "super" variable after compiling the class body and its methods.
    if ( classCompiler.hasSuperclass )
    {
        endScope();
    }

    //p.555 When an outermost class body ends, enclosing will be NULL, so this resets currentClass to NULL.
    currentClass = currentClass->enclosing;
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
        //p.559 making it an error to try to return anything else from an initializer
        if ( current->type == TYPE_INITIALIZER )
        {
            error( "Can't return a value from an initializer." );
        }

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
    if ( match( TOKEN_CLASS ) )
    {
        classDeclaration();
    }
    else if ( match( TOKEN_FUN ) )
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


