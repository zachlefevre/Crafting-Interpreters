#ifndef clox_vm_h
#define clox_vm_h

#include "chunk.h"
#include "value.h"

#define STACK_MAX 256

typedef struct {
  Chunk* chunks;
  uint8_t* ip;
  Value stack[STACK_MAX];
  Value* stackTop; // points just past the end of the stack
} VM;


void initVM();
void freeVM();

typedef enum {
  INTERPRET_OK,
  INTERPRET_COMPILE_ERROR,
  INTERPRET_RUNTIME_ERROR,
} InterpretResult;


InterpretResult interpret(const char* source);

void push(Value value);
Value pop();

#endif
