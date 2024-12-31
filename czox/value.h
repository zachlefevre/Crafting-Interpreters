#ifndef clox_value_h
#define clox_value_h

#include "common.h"

typedef double Value;

typedef struct {
  int capacity;
  int count;
  Value* values;
} ValueArray;

void initValueArray(ValueArray* values);
void writeValueArray(ValueArray* values, Value value);
void freeValueArray(ValueArray* values);
void printValue(Value value);


#endif
