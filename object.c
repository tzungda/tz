
#include <stdio.h>
#include <string.h>

#include "memory.h"
#include "object.h"
#include "table.h"
#include "value.h"
#include "vm.h"

#define ALLOCATE_OBJ( type, objectType ) \
	( type* ) allocateObject( sizeof( type ), objectType )

static Obj* allocateObject( size_t size, ObjType type )
{
	Obj* object = (Obj*)reallocate( NULL, 0, size );
	object->type = type;

	//p.506 Every new object begins life unmarked 
	object->isMarked = false;

	object->next = vm.objects;

#ifdef DEBUG_LOG_GC
	printf( "%p allocate %zu for %d\n", (void*)object, size, type ); //p.505
#endif

	vm.objects = object;
	return object;
}

//p.548 The constructor-like function simply stores the given closure and receiver
ObjBoundMethod* newBoundMethod( Value receiver, ObjClosure* method )
{
	ObjBoundMethod* bound = ALLOCATE_OBJ( ObjBoundMethod, OBJ_BOUND_METHOD );
	bound->receiver = receiver;
	bound->method = method;
	return bound;
}

//p.528
ObjClass* newClass( ObjString* name )
{
	ObjClass* klass = ALLOCATE_OBJ( ObjClass, OBJ_CLASS );
	klass->name = name;
	initTable( &klass->methods );//p.542
	return klass;
}

ObjClosure* newClosure( ObjFunction* function ) //p.466
{
	//p.481 Before creating the closure object itself, we allocate the array of upvalues and initialize them all to NULL
	ObjUpvalue** upvalues = ALLOCATE( ObjUpvalue*, function->upvalueCount );
	for ( int i = 0; i < function->upvalueCount; i++ )
	{
		upvalues[i] = NULL;
	}

	ObjClosure* closure = ALLOCATE_OBJ( ObjClosure, OBJ_CLOSURE );
	closure->function = function;
	closure->upvalues = upvalues;//p.481
	closure->upvalueCount = function->upvalueCount;
	return closure;
}

ObjFunction* newFunction() // p.435
{
	ObjFunction* function = ALLOCATE_OBJ( ObjFunction, OBJ_FUNCTION );
	function->arity = 0;
	function->name = NULL;
	function->upvalueCount = 0;
	initChunk( &function->chunk );
	return function;
}

ObjInstance* newInstance( ObjClass* klass )//p.533
{
	ObjInstance* instance = ALLOCATE_OBJ( ObjInstance, OBJ_INSTANCE );
	//store a reference to the instance's class
	instance->klass = klass;
	//initialize the field table to an empty hash table
	initTable( &instance->fields );
	return instance;
}

ObjNative* newNative( NativeFn function )
{
	ObjNative* native = ALLOCATE_OBJ( ObjNative, OBJ_NATIVE );
	native->function = function;
	return native;
}

static ObjString* allocateString( char* chars, int length, uint32_t hash )
{
	ObjString* string = ALLOCATE_OBJ( ObjString, OBJ_STRING );
	string->length = length;
	string->chars = chars;
	string->hash = hash;

	//p.522
	push( OBJ_VAL( string ) );

	tableSet( &vm.strings, string, NIL_VAL );

	//p.523
	pop();

	return string;
}

// FNV-1a method
static uint32_t hashString( const char* key, int length )
{
	uint32_t hash = 216613626u;
	for ( int i = 0; i < length; i++ )
	{
		hash ^= ( uint8_t )key[i];
		hash *= 16777619;
	}

	return hash;
}

ObjString* takeString( char* chars, int length )
{
	uint32_t hash = hashString( chars, length );
	ObjString* interned = tableFindString( &vm.strings, chars, length, hash );
	if ( interned )
	{
		FREE_ARRAY( char, chars, length + 1 );
		return interned;
	}
	return allocateString( chars, length, hash );
}

ObjString* copyString( const char* chars, int length )
{
	uint32_t hash = hashString( chars, length );
	ObjString* interned = tableFindString( &vm.strings, chars, length, hash );
	if ( interned )
		return interned;

	char* heapChars = ALLOCATE( char, length + 1 );
	memcpy( heapChars, chars, length );
	heapChars[length] = '\0';
	return allocateString( heapChars, length, hash );
}

//p.480
ObjUpvalue* newUpvalue( Value* slot )
{
	ObjUpvalue* upvalue = ALLOCATE_OBJ( ObjUpvalue, OBJ_UPVALUE );
	upvalue->closed = NIL_VAL;//p.491
	upvalue->location = slot;
	upvalue->next = NULL;
	return upvalue;
}

static void printFunction( ObjFunction* function )
{
	if ( function->name == NULL )
	{
		printf( "<script>" );
		return;
	}
	printf( "<fn %s>", function->name->chars );
}

void printObject( Value value )
{
	switch ( OBJ_TYPE( value ) )
	{
	case OBJ_BOUND_METHOD://p.549 A bound method prints exactly the same way as a function
		printFunction( AS_BOUND_METHOD(value)->method->function );
		break;
	case OBJ_CLASS://p.529
		printf( "%s", AS_CLASS( value )->name->chars );
		break;
	case OBJ_CLOSURE:
		printFunction( AS_CLOSURE( value )->function );
		break;
	case OBJ_FUNCTION:
		printFunction( AS_FUNCTION( value ) );
		break;
	case OBJ_INSTANCE:
		printf( "%s instance", AS_INSTANCE(value)->klass->name->chars );
		break;
	case OBJ_NATIVE:
		printf( "<native fn>" );
		break;
	case OBJ_STRING:
		printf( "%s", AS_CSTRING( value ) );
		break;
	case OBJ_UPVALUE:
		printf( "upvalue" );
		break;
	}
}
