#ifndef tz_vm_h
#define tz_vm_h

#include "object.h"
#include "table.h"

#define FRAMES_MAX 64
#define STACK_MAX ( FRAMES_MAX * UINT8_MAX )

//p.422: A CallFrame represents a single ongoing function call
typedef struct
{
	ObjFunction* function;
	uint8_t* ip; // Instead of storing the return address in the callee's frame, the caller stores its own ip
	Value* slots; // points into the VM's value stack at the first slot that this function can use
} CallFrame; 

typedef struct
{
	CallFrame frames[FRAMES_MAX]; //p.443
	int frameCount;
	Value stack[STACK_MAX];
	Value* stackTop;
	Table globals;
	Table strings;
	Obj* objects;
} VM;

typedef enum
{
	INTERPRET_OK,
	INTERPRET_COMPILE_ERROR,
	INTERPRET_RUNTIME_ERROR
} InterpretResult;

extern VM vm;

void initVM();
void freeVM(); 
InterpretResult interpret( const char* source );
void push( Value value );
Value pop();

#endif

