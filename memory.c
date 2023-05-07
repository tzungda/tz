
#include <stdlib.h>

#include "compiler.h"
#include "memory.h"
#include "vm.h"

#ifdef DEBUG_LOG_GC
#include <stdio.h>
#include "debug.h"
#endif

#define GC_HEAP_GROW_FACTOR 2 //p.520

void* reallocate( void* pointer, size_t oldSize, size_t newSize )
{
	//p.520 adjust the counter by the delta
	vm.bytesAllocated += newSize - oldSize;
	//p.503 collecting right before allocation is the classic way to wire a GC into a VM
	if ( newSize > oldSize )
	{
#ifdef DEBUG_STRESS_GC
		collectGarbage( );
#endif
		//p.520 when the total crosses the limit, we run the collector
		if ( vm.bytesAllocated > vm.nextGC )
		{
			collectGarbage();
		}
	}

	if( newSize == 0 )
	{
		free( pointer );
		return NULL;
	}

	void* result = realloc( pointer, newSize );
	if ( !result ) exit( 1 );

	return result;
}

void markObject( Obj* object ) //p.506
{
	if( object == NULL ) 
		return;
	// p.514 we need to ensure our collector doesn't get stuck in an infinite loop as it continually re-adds
	// the same series of objects to the gray stack.
	if( object->isMarked )
		return;
#ifdef DEBUG_LOG_GC
	printf( "%p mark ", (void*)object );
	printValue( OBJ_VAL( object ) );
	printf( "\n" );
#endif
	object->isMarked = true;

	//p.510
	if( vm.grayCapacity < vm.grayCount + 1 )
	{
		vm.grayCapacity = GROW_CAPACITY( vm.grayCapacity );
		//p.511 not that it calls the system realloc function and not our own reallocate() wrapper.
		// The memory for the gray stack itself is not managed by the garbage collector
		vm.grayStack = (Obj**)realloc(vm.grayStack, sizeof( Obj* ) * vm.grayCapacity );

		// p.511 
		if( vm.grayStack == NULL ) exit( 1 );
	}
	vm.grayStack[vm.grayCount++] = object;
}

void markValue( Value value ) 
{
	//p.506 the first thing we do is ensure that the value is an actual heap object
	if ( IS_OBJ( value ) )
		markObject( AS_OBJ( value ) );
}

//p.513
static void markArray( ValueArray* array )
{
	for( int i = 0; i < array->count; i++ )
	{
		markValue( array->values[i] );
	}
}

//p.512
// Note that we don't set any state in the traversed object itself. 
// There is no direct encoding of "black" in the object's state
static blackenObject( Obj* object )
{
#ifdef DEBUG_LOG_GC
	printf( "%p blacken ", (void*)object );
	printValue( OBJ_VAL(object) );
	printf( "\n" );
#endif
	switch ( object->type )
	{
	case OBJ_BOUND_METHOD:
	{
		// This ensures that a handle to a method keeps the receiver around in memory so that [this] can still find
		// the object when you invoke the handle later. We also trace the method closure
		ObjBoundMethod* bound = (ObjBoundMethod*)object;
		markValue( bound->receiver );
		markObject( (Obj*)bound->method );
		break;
	}
	case OBJ_CLASS:
	{
		// p.529 when the GC reaches a class object, it marks the class's name to keep that string alive too
		ObjClass* klass = ( ObjClass* )object;
		markObject( (Obj*)klass->name );
		//p. 542 speaking of memory managers, the GC needs to trace through classes into the method table
		markTable( &klass->methods );
		break;
	}
	case OBJ_CLOSURE:
	{
		//p.513 each closure has a reference to the bare function it wraps, 
		// as well as an array of pointers to the upvalues it captures
		ObjClosure* closure = (ObjClosure*)object;
		markObject( (Obj*)closure->function );
		for( int i = 0; i < closure->upvalueCount; i++ )
		{
			markObject( (Obj*)closure->upvalues[i] );
		}
		break;
		
	}
	case OBJ_FUNCTION:
	{
		ObjFunction* function = (ObjFunction*)object;
		markObject( (Obj*)function->name );
		markArray( &function->chunk.constants );
		break;
	}
	case OBJ_INSTANCE://p.534
	{
		ObjInstance* instance = (ObjInstance*)object;
		markObject( (Obj*)instance->klass );
		//we need to keep every object referenced by the instance's fields
		markTable( &instance->fields );
		break;
	}
		// When an upvalue is closed, it contains a reference to the closed-over value.
	case OBJ_UPVALUE:
		markValue( ((ObjUpvalue*)object)->closed );
		break;
		// strings and native function objects contain no outgoing references so there is nothing to traverse
	case OBJ_NATIVE:
	case OBJ_STRING:
		break;
	}
}

static void freeObject( Obj* object )
{
#ifdef DEBUG_LOG_GC
	printf( "%p free type %d\n", (void*)object, object->type ); //p.505
#endif
	switch( object->type )
	{
	case OBJ_BOUND_METHOD://p.548
	{
		// The bound method has a couple of references, but it doesn't own them, so it frees nothing but itself.
		FREE( ObjBoundMethod, object );
		break;
	}
	case OBJ_CLASS:
	{
		//p.542 The ObjClass struct owns the memory for this table, 
		// so when the memory manager deallocates a class, the table should be freed too.
		ObjClass* klass = ( ObjClass* )object;
		freeTable( &klass->methods );
		//p.529
		FREE( ObjClass, object );
		break;
	}
	case OBJ_CLOSURE:
	{
		//p.482
		ObjClosure* closure = (ObjClosure*)object;
		FREE_ARRAY( ObjUpvalue*, closure->upvalues, closure->upvalueCount );
		// p.466
		// we free only the ObjClosure it self, not the ObjFunction. That's because the closure doesn't own the function
		FREE( ObjClosure, object );
		break;
	}
	case OBJ_FUNCTION:
	{
		ObjFunction *function = (ObjFunction*)object;
		freeChunk( &function->chunk );
		FREE( ObjFunction, object );
		break;
	}
	case OBJ_NATIVE:
		FREE( ObjNative, object );
		break;
	case OBJ_STRING:
	{
		ObjString* string = ( ObjString* )object;
		FREE_ARRAY( char, string->chars, string->length + 1 );
		FREE( ObjString, object );
		break;
	}
	case OBJ_UPVALUE:
	{
		FREE( ObjUpvalue, object );
		break;
	}
	}
}

//p.505 Most roots are local variables or temporaries sitting right in the VM's stack, so we start by walking that
static void markRoots( )
{
	for ( Value* slot = vm.stack; slot < vm.stackTop; slot++ )
	{
		markValue( *slot );
	}

	//p.507 Each CallFrame contains a pointer to the closure being called.
	// The VM uses those pointers to access constants and upvalues, so those closures need to be kept around too
	for ( int i = 0; i < vm.frameCount; i++ )
	{
		markObject( (Obj*)vm.frames[i].closure );
	}

	// p.508 the open value list is another set of values that the VM can directly reach
	for ( ObjUpvalue* upvalue = vm.openUpvalues; upvalue != NULL; upvalue = upvalue->next )
	{
		markObject( (Obj*)upvalue );
	}
	

	// p.507 Makring the stack takes care of local variables and temporaries. The other main source of roots are the global variables
	markTable( &vm.globals );
	// p.508
	markCompilerRoots( );
	//p.557 GC consider the init( constructor ) a root
	markObject( (Obj*)vm.initString );
}

// p.512 Until the stack empties, we keep pulling out gray objects, 
// traversing their references, and then marking them black
static void traceReferences()
{
	while ( vm.grayCount > 0 )
	{
		Obj* object = vm.grayStack[--vm.grayCount];
		blackenObject( object );
	}
}

//p.514
static void sweep()
{
	Obj* previous = NULL;
	Obj* object = vm.objects;
	while( object != NULL )
	{
		if ( object->isMarked )
		{
			//p.515 when the next collection cycle starts, we need every object to be white
			object->isMarked = false;

			previous = object;
			object = object->next;
		}
		else
		{
			Obj* unreached = object;
			object = object->next;
			if( previous != NULL )
			{
				previous->next = object;
			}
			else
			{
				vm.objects = object;
			}

			freeObject( unreached );
		}
	}
}

void freeObjects()
{
	Obj* object = vm.objects;
	while ( object )
	{
		Obj* next = object->next;
		freeObject( object );
		object = next;
	}

	//p.511
	free(vm.grayStack);
}

void collectGarbage()
{
#ifdef DEBUG_LOG_GC //p.504
	printf( "-- gc begin\n" );
	//p.520
	size_t before = vm.bytesAllocated;
#endif

	// p.505 Marking begins at the roots
	markRoots();
	// p.512
	traceReferences();
	//p.516 the right time is exactly between the marking and sweeping phases
	tableRemoveWhite( &vm.strings );
	//p.514 
	sweep();

	//p.520 we adjust the threshold of the next GC based on this
	vm.nextGC = vm.bytesAllocated * GC_HEAP_GROW_FACTOR;

#ifdef DEBUG_LOG_GC //p.504
	printf( "-- gc end\n" );
	//p.520
	printf( "   collected %zu bytes ( from %zu to %zu ) next at %zu\n", before - vm.bytesAllocated, 
		before, vm.bytesAllocated, vm.nextGC );
#endif
}
