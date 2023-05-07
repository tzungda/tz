#ifndef tz_vm_h
#define tz_vm_h

#include "object.h"
#include "table.h"

#define FRAMES_MAX 64
#define STACK_MAX ( FRAMES_MAX * UINT8_MAX )

//p.422: A CallFrame represents a single ongoing function call
typedef struct
{
	ObjClosure* closure;
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
	ObjString* initString;//p.557
	ObjUpvalue* openUpvalues; //p.488
	Obj* objects;
	// p.511
	int grayCount;
	int grayCapacity;
	Obj** grayStack;
	//p.519
	size_t bytesAllocated;// running total of the number of bytes of managed memory the VM has allocated
	size_t nextGC;// the threshold that triggers the next collection

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

