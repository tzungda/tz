#ifndef tz_vm_h
#define tz_vm_h

#include "chunk.h"

#define STACK_MAX 256

typedef struct
{
	Chunk* chunk;
	uint8_t *ip; // instruction pointer, this points to the next instruction that about to execute
	Value stack[STACK_MAX];
	Value* stackTop;
} VM;

typedef enum
{
	INTERPRET_OK,
	INTERPRET_COMPLE_ERROR,
	INTERPRET_RUNTIME_ERROR
} InterpretResult;


void initVM();
void freeVM(); 
InterpretResult interpret( Chunk* chunk );
void push( Value value );
Value pop();

#endif

