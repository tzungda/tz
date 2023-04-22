#ifndef tz_object_h
#define tz_object_h

#include "common.h"
#include "value.h"
#include "chunk.h"

#define OBJ_TYPE( value )		( AS_OBJ(value)->type )

#define IS_FUNCTION( value )	isObjType( value, OBJ_FUNCTION );// p.436
#define IS_NATIVE( value )		isObjType( value, OBJ_NATIVE );//p.460
// p.346: using a function to avoid the expression getting evaluated multiple times
#define IS_STRING( value )		isObjType( value, OBJ_STRING ) 

#define AS_FUNCTION( value )	( (ObjFunction*)AS_OBJ(value) )
#define AS_NATIVE( value )		( ((ObjNative*)AS_OBJ(value))->function )
#define AS_STRING( value )		( (ObjString*)AS_OBJ(value) )
#define AS_CSTRING( value )		( ((ObjString*)AS_OBJ(value))->chars )

typedef enum
{
	OBJ_FUNCTION, //p.435
	OBJ_NATIVE, //p.459
	OBJ_STRING,
} ObjType;

struct Obj
{
	ObjType type;
	struct Obj* next;
};

typedef struct
{
	Obj obj;
	int arity;
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

ObjFunction* newFunction();
ObjNative* newNative( NativeFn function );
ObjString* takeString( char* chars, int length );
ObjString* copyString( const char* chars, int length );
void printObject( Value value );

static inline bool isObjType( Value value, ObjType type )
{
	return IS_OBJ( value ) && AS_OBJ( value )->type == type;
}

#endif
