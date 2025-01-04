#ifndef clox_vm_h
#define clox_vm_h

#include "chunk.h"

typedef struct {
  Chunk* chunks;
  uint8_t* ip;
} VM;


void initVM();
void freeVM();

typedef enum {
  INTERPRET_OK,
  INTERPRET_COMPILE_ERROR,
  INTERPRET_RUNTIME_ERROR,
} InterpretResult;


InterpretResult interpret(Chunk* chunk);


#endif
