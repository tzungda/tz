#ifndef tz_object_h
#define tz_object_h

#include "common.h"
#include "value.h"
#include "chunk.h"
#include "table.h"

#define OBJ_TYPE( value )			( AS_OBJ(value)->type )

#define IS_BOUND_METHOD( value )	isObjType( value, OBJ_BOUND_METHOD )//p.547
#define IS_CLASS( value )			isObjType( value, OBJ_CLASS )//p.528
#define IS_CLOSURE( value )			isObjType( value, OBJ_CLOSURE )
#define IS_FUNCTION( value )		isObjType( value, OBJ_FUNCTION )// p.436
#define IS_INSTANCE( value )		isObjType( value, OBJ_INSTANCE )// p.533
#define IS_NATIVE( value )			isObjType( value, OBJ_NATIVE )//p.460
// p.346: using a function to avoid the expression getting evaluated multiple times
#define IS_STRING( value )			isObjType( value, OBJ_STRING ) 

#define AS_BOUND_METHOD( value )	( (ObjBoundMethod*)AS_OBJ(value) ) //p.548
#define AS_CLASS( value )			( (ObjClass*)AS_OBJ(value) )
#define AS_CLOSURE( value )			( (ObjClosure*)AS_OBJ(value) )
#define AS_FUNCTION( value )		( (ObjFunction*)AS_OBJ(value) )
#define AS_INSTANCE( value )		( (ObjInstance*)AS_OBJ(value) )// p.533
#define AS_NATIVE( value )			( ((ObjNative*)AS_OBJ(value))->function )
#define AS_STRING( value )			( (ObjString*)AS_OBJ(value) )
#define AS_CSTRING( value )			( ((ObjString*)AS_OBJ(value))->chars )

typedef enum
{
	OBJ_BOUND_METHOD, //p.547
	OBJ_CLASS,//p.528
	OBJ_CLOSURE, //p.466
	OBJ_FUNCTION, //p.435
	OBJ_INSTANCE, //p.532
	OBJ_NATIVE, //p.459
	OBJ_STRING,
	OBJ_UPVALUE,//p.480
} ObjType;

struct Obj
{
	ObjType type;
	bool isMarked; //p.506
	struct Obj* next;
};

typedef struct
{
	Obj obj;
	int arity;
	int upvalueCount;
	Chunk chunk;
	ObjString* name;
} ObjFunction; // p.434

typedef Value (*NativeFn)(int argCount, Value* args ); // p.459

typedef struct //p.459
{
	Obj obj;
	NativeFn function;
} ObjNative;

struct ObjString
{
	Obj obj;
	int length;
	char* chars; 
	uint32_t hash;
};

typedef struct ObjUpvalue //p.479
{
	Obj obj;
	Value* location;//p.479 location field that points to the closed-over variable
	Value closed;//p.491 
	struct ObjUpvalue* next; //p.488
} ObjUpvalue;

// p.465
// we'll wrap every fuction in an ObjClosure, even if the function doesn't actually close over and 
// capture any surrounding local variables.
typedef struct
{
	Obj obj;
	ObjFunction* function;
	ObjUpvalue** upvalues;//p.481
	int upvalueCount;//p.481
} ObjClosure;

//p.528
typedef struct
{
	Obj obj;
	ObjString* name;
	Table methods;//p.542 Keys are method names, and each value is an ObjClosure for the body of the method
} ObjClass;

//p.531
typedef struct
{
	Obj obj;
	ObjClass* klass;// each instance has a pointer to the class that is an instance of
	Table fields;//lets users freely add fields to an instance at runtime
} ObjInstance;

//p.547 When the user executes a method access, we'll find the closure for that method and wrap it in a new "bound method" object
// that tracks the instance that the method was accessed from
typedef struct
{
	Obj obj;
	// The receiver's type is Value even though methods can be called only on ObjInstance.
	// Since the VM doesn't care what kind of receiver it has anyway, using Value means we don't have to 
	// keep converting the pointer back to a Value when it gets passed to more general functions.
	Value receiver;
	ObjClosure* method;
} ObjBoundMethod;

//p.548
ObjBoundMethod* newBoundMethod( Value receiver, ObjClosure* method );
//p.528
ObjClass* newClass( ObjString* name );
//p.466
ObjClosure* newClosure( ObjFunction* function );


ObjFunction* newFunction();
ObjInstance* newInstance( ObjClass* klass );
ObjNative* newNative( NativeFn function );
ObjString* takeString( char* chars, int length );
ObjString* copyString( const char* chars, int length );
ObjUpvalue* newUpvalue( Value* slot );
void printObject( Value value );

static inline bool isObjType( Value value, ObjType type )
{
	return IS_OBJ( value ) && AS_OBJ( value )->type == type;
}

#endif
