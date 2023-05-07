
#include <stdio.h>
#include <stdarg.h>
#include <string.h>
#include <time.h>

#include "common.h"
#include "compiler.h"
#include "vm.h"
#include "debug.h"
//#include "object.h"
#include "memory.h"


VM vm; // it will only use one VM, so declaring it as a global variable is fine

static Value clockNative( int argCount, Value* args ) //.461
{
	return NUMBER_VAL( (double)clock()/CLOCKS_PER_SEC );
}

static void resetStack( )
{
	vm.stackTop = vm.stack;
	vm.frameCount = 0;
	vm.openUpvalues = NULL;//p.488
}

static void runtimeError( const char* format, ... )
{
	va_list args;
	va_start( args, format );
	vfprintf( stderr, format, args );
	va_end( args );
	fputs( "\n", stderr );

	// walk the call stack from top(the most recently called function) to bottom(the top-level code).
	for( int i = vm.frameCount - 1; i >= 0; i-- ) //p.455
	{
		CallFrame* frame = &vm.frames[i];
		ObjFunction* function = frame->closure->function;
		size_t instruction = frame->ip - function->chunk.code - 1;
		fprintf( stderr, "[line %d] in ", 
			function->chunk.lines[instruction] );
		if( function->name == NULL )
		{
			fprintf( stderr, "script\n" );
		}
		else
		{
			fprintf( stderr, "%s()\n", function->name->chars );
		}
	}

	resetStack();
}

static void defineNative( const char* name, NativeFn function ) //p.461
{
	push( OBJ_VAL( copyString( name, (int)strlen(name) ) ) );
	push( OBJ_VAL( newNative(function) ) );
	tableSet( &vm.globals, AS_STRING( vm.stack[0] ), vm.stack[1] );
	pop();
	pop();
}

void initVM()
{
	resetStack();
	vm.objects = NULL;

	// p.519
	vm.bytesAllocated = 0;
	vm.nextGC = 1024*1024;

	//p.511
	vm.grayCount = 0;
	vm.grayCapacity = 0;
	vm.grayStack = NULL;

	initTable( &vm.globals );
	initTable( &vm.strings );

	// p.557 If the collector ran at just the wrong time, it would read vm.initString before it had been initialized
	vm.initString = NULL;
	// p.557 create and intern the string when the VM boos up
	vm.initString = copyString( "init", 4 );

	defineNative( "clock", clockNative );//p.461
}

void freeVM()
{
	freeTable( &vm.globals );
	freeTable( &vm.strings );
	// p.557
	vm.initString = NULL;
	freeObjects();
}

void push( Value value )
{
	*vm.stackTop = value;
	vm.stackTop++;
}

Value pop( )
{
	vm.stackTop--;
	return *vm.stackTop;
}

static Value peek( int distance )
{
	return vm.stackTop[ -1 - distance ];
}

static bool call( ObjClosure* closure/*p.468*/, int argCount )//p.453
{
	if ( argCount != closure->function->arity ) //p.454
	{
		runtimeError( "Expected %d arguments but got %d.", closure->function->arity, argCount );
		return false;
	}
	
	if ( vm.frameCount == FRAMES_MAX )
	{
		runtimeError( "Stack overflow" );
		return false;
	}

	CallFrame* frame = &vm.frames[vm.frameCount++];
	frame->closure = closure;
	frame->ip = closure->function->chunk.code;
	frame->slots = vm.stackTop - argCount - 1; // here 1 is for function name
	return true;
}

static bool callValue( Value callee, int argCount )//p.452
{
	if( IS_OBJ( callee ) )
	{
		switch ( OBJ_TYPE( callee ) )
		{
		case OBJ_BOUND_METHOD: //p.551
		{
			// Users can declare methods on classes, access them on instances, and get bound methods onto the stack.
			// We pull the raw closure back out of the ObjBoundMethod and use the existing call() helper to begin an invocation 
			// of that closure by pushing a CallFrame for it onto the call stack.
			ObjBoundMethod* bound = AS_BOUND_METHOD( callee );
			//p.553 When a method is called, the top of the stack contains all of the arguments, 
			//and then just under those is the closure of the called method.
			vm.stackTop[-argCount - 1] = bound->receiver;
			return call( bound->method, argCount );
		}
		case OBJ_CLASS://p.534
		{
			// The way to create an instance of a class is to invoke the class itself as if it were a function
			// If the value being called - the object that results when evaluating the expression to the left of the 
			// opening parenthesis - is a class, then we treat it as a constructor
			ObjClass* klass = AS_CLASS( callee );
			vm.stackTop[-argCount - 1] = OBJ_VAL( newInstance( klass ) );
			//p.556 First, automatically calling init() on new instances
			Value initializer;
			if( tableGet( &klass->methods, vm.initString, &initializer ) )
			{
				return call( AS_CLOSURE( initializer ), argCount );
			}
			else if ( argCount != 0 )
			{
				// we also need to ensure that the number of the arguments passed matches the initializer's arity
				runtimeError( "Expected 0 arguments but got %d.", argCount );
				return false;
			}
			return true;
		}
		case OBJ_CLOSURE: // p.468
			return call( AS_CLOSURE( callee ), argCount );
		case OBJ_NATIVE:
		{
			NativeFn native = AS_NATIVE( callee );
			Value result = native( argCount, vm.stackTop - argCount );
			vm.stackTop -= argCount + 1;
			push( result );
			return true;
		}
		default:
			break; // non-callable object type
		}
	}
	runtimeError( "Can only call functions and classes." );
	return false;
}

//p.562 This function combines the logic of how the VM implements OP_GET_PROPERTY and OP_ALL instructions
static bool invokeFromClass( ObjClass* klass, ObjString* name, int argCount )
{
	Value method;
	if ( !tableGet( &klass->methods, name, &method ) )
	{
		runtimeError( "Undefined property '%s'.", name->chars );
		return false;
	}
	// we take the method's closure and push a call to it onto the CallFrame stack
	return call( AS_CLOSURE( method ), argCount );
}

// p.561
static bool invoke( ObjString* name, int argCount )
{
	Value receiver = peek( argCount );
	//p.562 that does assume the object is an instance
	if( !IS_INSTANCE( receiver ) )
	{
		runtimeError("Only instances have methods.");
		return false;
	}

	ObjInstance* instance = AS_INSTANCE( receiver );

	//p.563 instead of executing taht correctly, our VM reports a runtime error when it can't find a method named "field"
	Value value;
	if ( tableGet( &instance->fields, name, &value ) )
	{
		vm.stackTop[-argCount - 1] = value;
		return callValue( value, argCount );
	}

	return invokeFromClass( instance->klass, name, argCount );
}

// p.550
static bool bindMethod( ObjClass* klass, ObjString* name )
{
	Value method;
	// First we look for a method with the given name in the class's method table
	if( !tableGet( &klass->methods, name, &method ) )
	{
		runtimeError( "Undefined property '%s'.", name->chars );
		return false;
	}

	// we take the method and wrap it in a new ObjBoundMethod
	ObjBoundMethod* bound = newBoundMethod( peek(0), AS_CLOSURE( method ) );

	// Finally, we pop the instance and replace the top of the stack with the bound method
	pop();
	push( OBJ_VAL( bound ) );
	return true;
}

static ObjUpvalue* captureUpvalue( Value* local )
{
	// p.489
	ObjUpvalue* preUpvalue = NULL;
	ObjUpvalue* upvalue = vm.openUpvalues;
	// iterate past every upvalue pointing to slots above the one we're looking for
	while ( upvalue != NULL && upvalue->location > local )
	{
		preUpvalue = upvalue;
		upvalue = upvalue->next;
	}
	//
	if ( upvalue != NULL && upvalue->location == local )
	{
		return upvalue;
	}

	ObjUpvalue* createdUpvalue = newUpvalue( local );

	//p.489 Otherwise, we create a new upvalue for our local slot and insert it into the list at the right location
	createdUpvalue->next = upvalue;
	if( preUpvalue == NULL )
	{
		vm.openUpvalues = createdUpvalue;
	}
	else
	{
		preUpvalue->next = createdUpvalue;
	}

	return createdUpvalue;
}

static void closeUpvalues( Value* last ) //p.490
{
	// we walk the VM's list of open upvalues, again from top to bottom
	// If an upvalue's location points into the range of slots we're closing, we close the upvalue.
	// Otherwise, once we reach an upvalue outside of the range, we know the rest will be too. so we stop iterating.
	while( vm.openUpvalues != NULL && vm.openUpvalues->location >= last )
	{
		ObjUpvalue* upvalue = vm.openUpvalues;
		// the variable moves from the stack tothe closed field
		upvalue->closed = *upvalue->location;
		upvalue->location = &upvalue->closed;
		vm.openUpvalues = upvalue->next;
	}
}

//p.546 
static void defineMethod( ObjString* name )
{
	// The method closure is on top of the stack, above the class it will be bound to.
	// We read those two stack slots and store the closure in the class's method table.
	// Then we pop the closure since we're done with it
	Value method = peek( 0 );
	ObjClass* klass = AS_CLASS( peek(1) );
	tableSet( &klass->methods, name, method );
	pop();
}

static bool isFalsey( Value value )
{
	return IS_NIL( value ) || ( (IS_BOOL(value) && !AS_BOOL(value) ) );
}

static void concatenate( )
{
	//p.523 instead of popping them off the stack eagerly, we peeo them.
	// that way, they are still hanging out on the stack when we create the result string.
	ObjString* b = AS_STRING( peek( 0 ) );
	ObjString* a = AS_STRING( peek( 1 ) );

	//
	int length = a->length + b->length;
	char* chars = ALLOCATE( char, length + 1 );
	memcpy( chars, a->chars, a->length );
	memcpy( chars + a->length, b->chars, b->length );
	chars[length] = '\0';

	ObjString* result = takeString( chars, length );

	// p.523 Once that's done, we can safely pop them off and replace them with the result
	pop();
	pop();

	push( OBJ_VAL( result ) );
}

static InterpretResult run()
{
	CallFrame* frame = &vm.frames[ vm.frameCount - 1 ];
#define READ_BYTE() (*frame->ip++ )
#define READ_CONSTANT() ( frame->closure->function->chunk.constants.values[READ_BYTE()] )
#define READ_SHORT( ) \
	( frame->ip += 2, (uint16_t)( (frame->ip[-2] << 8) | frame->ip[-1]) )
#define READ_STRING( ) AS_STRING( READ_CONSTANT() )
#define BINARY_OP( valueType, op ) \
	do { \
		if( !IS_NUMBER( peek( 0 ) ) || !IS_NUMBER( peek( 1 ) ) )\
		{\
			runtimeError( "Operands must be numbers." );\
			return INTERPRET_RUNTIME_ERROR; \
		}\
		double b = AS_NUMBER( pop() ); \
		double a = AS_NUMBER( pop() ); \
		push( valueType( a op b ) );\
	} while(false)

	for ( ; ; )
	{
#ifdef DEBUG_TRACE_EXECUTION
		printf( "        " );
		for ( Value* slot = vm.stack; slot < vm.stackTop; slot++ )
		{
			printf("[ ");
			printValue( *slot );
			printf(" ]");
		}
		printf( "\n" );
		//
		disassembleInstruction( &frame->closure->function->chunk, (int)(frame->ip - frame->closure->function->chunk.code) );
#endif
		uint8_t instruction;
		switch ( instruction = READ_BYTE() )
		{
		case OP_CONSTANT:
		{
			Value constant = READ_CONSTANT();
			push( constant );
			break;
		}
		case OP_NIL: push( NIL_VAL ); break;
		case OP_TRUE: push( BOOL_VAL(true) ); break;
		case OP_FALSE: push( BOOL_VAL(false) ); break;
		case OP_POP: pop(); break;
		case OP_GET_LOCAL:
		{
			uint8_t slot = READ_BYTE();
			push( frame->slots[slot] );
			break;
		}
		case OP_SET_LOCAL:
		{
			uint8_t slot = READ_BYTE();
			vm.stack[slot] = peek( 0 );//p.409: note that it doesn't pop the value from the stack
			break;
		}
		case OP_GET_GLOBAL:
		{
			ObjString* name = READ_STRING();
			Value value;
			if ( !tableGet( &vm.globals, name, &value ) )
			{
				runtimeError( "Undefined variable '%s'.", name->chars );
				return INTERPRET_RUNTIME_ERROR;
			}
			push( value );
			break;
		}
		case OP_DEFINE_GLOBAL:
		{
			ObjString* name = READ_STRING();
			tableSet( &vm.globals, name, peek( 0 ) );//p.389
			pop();
			break;
		}
		case OP_SET_GLOBAL:
		{
			ObjString* name = READ_STRING();
			if( tableSet( &vm.globals, name, peek( 0 ) ) )
			{
				tableDelete( &vm.globals, name );
				runtimeError( "Undefined variable '%s'.", name->chars );
				return INTERPRET_RUNTIME_ERROR;
			}
			break;
		}
		case OP_GET_UPVALUE: //p.483
		{
			// we simply look up the corresponding upvalue and dereference its location pointer to read the value in that slot.
			uint8_t slot = READ_BYTE();
			push( *frame->closure->upvalues[slot]->location );
			break;
		}
		case OP_GET_PROPERTY://p.536
		{
			//only instances are allowed to have fields. You can't stuff a field onto a string or number
			if( !IS_INSTANCE( peek(0) ) )
			{
				runtimeError( "Only instances have properties." );
				return INTERPRET_RUNTIME_ERROR;
			}

			// When the interpreter reaches this instruction, the expression to the left of the dot
			// has already been executed and the resulting instance is on top of the stack
			ObjInstance* instance = AS_INSTANCE( peek( 0 ) );
			ObjString* name = READ_STRING();

			Value value;
			if ( tableGet( &instance->fields, name, &value ) )
			{
				pop();//instance
				push(value);
				break;
			}

			//p.549 The instruction's job is to find a field or a method with the given name and replace the top
			// of the stack with the accessed property.
			// Fields take priority over and shadow methods, so we look for a field first.
			// If that function finds a method, it places the method on the stack and returns true.
			if ( !bindMethod( instance->klass, name ) )
			{
				return INTERPRET_RUNTIME_ERROR;
			}
			break;
		}
		case OP_SET_PROPERTY://p.537
		{
			// A setter implicitly creates the field if needed. 
			// We do need to handle the user incorrectly trying to store a field on a value that isn't an instance
			if ( !IS_INSTANCE( peek( 1 ) ) )
			{
				runtimeError( "Only instances have fields." );
				return INTERPRET_RUNTIME_ERROR;
			}

			ObjInstance* instance = AS_INSTANCE( peek( 1 ) );
			tableSet( &instance->fields, READ_STRING(), peek( 0 ) );
			// We pop the stored value off, then pop the instance, and finally push the value back on.
			// A setter is itself an expression whose result is the assigned value, so we need to leave that value on the stack
			Value value = pop();
			pop();
			push( value );
			break;
		}
		case OP_GET_SUPER: //p.577
		{
			// interpreting the new instruction is similar to executing a normal property access.
			// As with properties, we read the method name from the constant table. Then we pass
			// that to bindMethod() which looks up the method in the given class's method table and 
			// creates an ObjBoundMethod to bundle the resulting closure to the current instance.
			ObjString* name = READ_STRING();
			ObjClass* superclass = AS_CLASS( pop() );

			// the key difference is which class we pass to bindMethod()
			// we use the statically resolved superclass of the containing class, which the compiler has
			// conveniently ensured is sitting on top of the stack waiting for us
			if ( !bindMethod( superclass, name ) )
			{
				return INTERPRET_RUNTIME_ERROR;
			}
			break;
		}
		case OP_EQUAL: 
		{
			Value a = pop();
			Value b = pop();
			push( BOOL_VAL( valuesEqual( a, b ) ) );
			break;
		}
		case OP_GREATER:	BINARY_OP( BOOL_VAL, > ); break;
		case OP_LESS:		BINARY_OP( BOOL_VAL, < ); break;
		case OP_ADD:		
		{
			if ( IS_STRING( peek( 0 ) ) && IS_STRING( peek( 1 ) ) )
			{
				concatenate( );
			}
			else if ( IS_NUMBER( peek( 0 ) ) && IS_NUMBER( peek( 1 ) ) )
			{
				double b = AS_NUMBER( pop() );
				double a = AS_NUMBER( pop() );
				push( NUMBER_VAL( a + b ) );
			}
			else
			{
				runtimeError( "Operands must be two numbers or two strings." );
				return INTERPRET_RUNTIME_ERROR;
			}
			break;
		}
		case OP_SUBTRACT:	BINARY_OP( NUMBER_VAL, - ); break;
		case OP_MULTIPLY:	BINARY_OP( NUMBER_VAL, * ); break;
		case OP_DIVIDE:		BINARY_OP( NUMBER_VAL, / ); break;
		case OP_NOT:
			push( BOOL_VAL( isFalsey( pop() ) ) );
			break;
		case OP_NEGATE:		
			if ( !IS_NUMBER( peek( 0 ) ) )
			{
				runtimeError( "Operand must be a number." );
				return INTERPRET_RUNTIME_ERROR;
			}
			push( NUMBER_VAL( -AS_NUMBER( pop() ) ) ); 
			break;
		case OP_PRINT:
		{
			printValue( pop() );
			printf( "\n" );
			break;
		}
		case OP_JUMP:
		{
			uint16_t offset = READ_SHORT();
			frame->ip += offset;
			break;
		}
		case OP_JUMP_IF_FALSE:
		{
			uint16_t offset = READ_SHORT();
			//p.417 after reading the offet, we check the condition value on top of the stack.
			// if it's falsey, we apply the jump offet to the ip
			if( isFalsey( peek( 0 ) ) ) 
				frame->ip += offset;
			break;
		}
		case OP_LOOP: // p.423
		{
			// the only difference from OP_JUMP is a substraction instead of an addition
			uint16_t offset = READ_SHORT();
			frame->ip -= offset;
			break;
		}
		case OP_CALL://p.452
		{
			int argCount = READ_BYTE();
			if( !callValue( peek( argCount ), argCount ) )
			{
				return INTERPRET_RUNTIME_ERROR;
			}
			frame = &vm.frames[vm.frameCount - 1];// cached pointer to the current frame
			break;
		}
		case OP_INVOKE: //p.561
		{
			ObjString* method = READ_STRING( );
			int argCount = READ_BYTE();
			if( !invoke( method, argCount ) )
			{
				return INTERPRET_RUNTIME_ERROR;
			}
			// assuming the invocation succeeded, then there is a new CallFrame on the stack,
			// so we refresh our cached copy of the current frame in [frame].
			frame = &vm.frames[vm.frameCount - 1];
			break;
		}
		case OP_SUPER_INVOKE: //p.578
		{
			// This handful of code is basically our implementation of OP_INVOKE mixed together with a dash of OP_GET_SUPER
			ObjString* method = READ_STRING();
			int argCount = READ_BYTE();
			ObjClass* superclass = AS_CLASS( pop() );
			// We pass the superclass, method name, and argument count to our existing invokeFromClass function.
			// invokeFromClass() pushes a new CallFrame onto the call stack for the method's closure. 
			if ( !invokeFromClass( superclass, method, argCount ) )
			{
				return INTERPRET_RUNTIME_ERROR;
			}
			// That invalidates the interpreter's cached CallFrame pointer, so ew refresh frame
			frame = &vm.frames[vm.frameCount - 1];
			break;
		}
		case OP_CLOSURE: 
		{
			// p.468
			// first we load the compiled function from the constant table
			ObjFunction* function = AS_FUNCTION( READ_CONSTANT() );
			// wrap the function in a new ObjClosure and push the result onto the stack
			ObjClosure* closure = newClosure( function );
			push( OBJ_VAL( closure ) );
			// p.482 We iterate over each upvalue the closure expects. For each one, we read a pair of operand bytes.
			// If the upvalue closes over a local variable in the enclosing function, we let captureUpvalue() do the work.
			// Otherwise, we capture an upvalue from the surrounding function.
			for ( int i = 0; i < closure->upvalueCount; i++ )
			{
				uint8_t isLocal = READ_BYTE();
				uint8_t index = READ_BYTE();
				if( isLocal )
				{
					// p.482 We need to grab a pointer to the captured local's slot in the surrounding function's stack window.
					// That window begins at frame->slots, which points to slot zero.
					closure->upvalues[i] = captureUpvalue( frame->slots + index );
				}
				else
				{
					closure->upvalues[i] = frame->closure->upvalues[index];
				}
			}
			break;
		}
		case OP_CLOSE_UPVALUE: //p.490
		{
			// this function is responsible for closing the upvalue and moving the local from the stack to the heap
			closeUpvalues( vm.stackTop - 1 );
			pop();
			break;
		}
		case OP_RETURN:
		{
			Value result = pop();
			// p.491 When a function returns, we call that same helper and pass in the first stack slot owned by the function
			closeUpvalues( frame->slots );

			vm.frameCount--;
			if( vm.frameCount == 0 )
			{
				pop();
				return INTERPRET_OK;
			}

			vm.stackTop = frame->slots;
			push( result ); // push return value 
			// p.456 update the run() function's cached pointer to the current frame
			frame = &vm.frames[vm.frameCount - 1];
			break;
		}
		case OP_CLASS://p.531
		{
			//creates a new class object with the given name
			push( OBJ_VAL( newClass( READ_STRING() ) ) );
			break;
		}
		// p.569
		case OP_INHERIT:
		{
			// From the top of the stack down, we have the subclass then the superclass. 
			// We grab both of those and then do the inherit-y bit.
			// When the subclass is declared, we copy all of the inherited class's methods down into the subclass's own method table
			// Later, when calling a method, any method inherited from a superclass will be found right in the subclass's own method table.
			Value superclass = peek( 1 );
			if ( !IS_CLASS( superclass ) )
			{
				runtimeError( "Superclass msut be a class." );
				return INTERPRET_RUNTIME_ERROR;
			}
			ObjClass* subclass = AS_CLASS( peek(0) );
			tableAddAll( &AS_CLASS( superclass )->methods, &subclass->methods );
			pop();// subclass
			break;
		}
		case OP_METHOD://p.545
		{
			// we read the method name from the constant table
			defineMethod( READ_STRING() );
			break;
		}
		}
	}

#undef READ_BYTE
#undef READ_CONSTANT
#undef READ_SHORT
#undef READ_STRING
#undef BINARY_OP
}

InterpretResult interpret( const char* source )
{
	ObjFunction* function = compile( source );
	if ( function == NULL )
		return INTERPRET_COMPILE_ERROR;
	
	push( OBJ_VAL( function ) );

	ObjClosure* closure = newClosure( function ); //p.469
	pop();
	push( OBJ_VAL( closure ) );
	// //p.453 set up to first frame for executing the top-level code
	call( closure/*p.469*/, 0 );
	
	return run();
}
