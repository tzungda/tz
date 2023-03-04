#ifndef tz_value_h
#define tz_value_h

/*
* This is for handling constant values, eg. 1, 2
*/

#include "common.h"

typedef double Value;

typedef struct
{
	int capacity;
	int count;
	Value* values;
} ValueArray;

void initValueArray( ValueArray* array );
void writeValueArray( ValueArray* array, Value value );
void freeValueArray( ValueArray* array );
void printValue( Value value );

#endif
