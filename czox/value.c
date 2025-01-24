#include <stdio.h>

#include "value.h"

#include "memory.h"

void initValueArray(ValueArray* values) {
  values->capacity = 0;
  values->count = 0;
  values->values = NULL;
}


void writeValueArray(ValueArray* array, Value value) {
  if (array->capacity < array->count + 1) {
    int oldCapacity = array->capacity;
    array->capacity = GROW_CAPACITY(oldCapacity);
    array->values = GROW_ARRAY(Value, array->values,
                                oldCapacity, array->capacity);
  }
  array->values[array->count] = value;
  array->count++;
}

void freeValueArray(ValueArray* values) {
  FREE_ARRAY(Value, values->values, values->capacity);
  initValueArray(values);
}

void printValue(Value value) {
  printf("\t");
  switch (value.type) {
  case VAL_BOOL: printf(AS_BOOL(value) ? "true" : "false"); break;
  case VAL_NUMBER: printf("%g", AS_NUMBER(value)); break;
  case VAL_NIL: printf("nil"); break;
  default: printf("????"); break;
  }
  printf("\n");
}
