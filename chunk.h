#ifndef tz_chunk_h
#define tz_chunk_h

#include "common.h"
#include "value.h"

typedef enum
{
    OP_CONSTANT,
    OP_NIL,
    // (p.334) it can still use OP_CONSTANT for both ture and false, but it will take two byte for each in the buffer.
    // separating them to save the space
    OP_TRUE,
    OP_FALSE, 
    OP_POP,
    OP_GET_GLOBAL, //p.391
    OP_DEFINE_GLOBAL,
    OP_SET_GLOBAL,
    OP_EQUAL,
    OP_GREATER,
    OP_LESS,
    OP_ADD,
    OP_SUBTRACT,
    OP_MULTIPLY,
    OP_DIVIDE,
    OP_NOT,
    OP_NEGATE,
    OP_PRINT,
    OP_RETURN,
} OpCode;

typedef struct
{
    int count;
    int capacity;
    uint8_t *code;
    int* lines; // for line numbers
    ValueArray constants;
} Chunk;

void initChunk( Chunk* chunk );
void writeChunk( Chunk* chunk, uint8_t byte, int line );
void freeChunk( Chunk* chunk );
int addConstant( Chunk* chunk, Value value );

#endif