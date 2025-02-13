#ifndef clox_chunk_h
#define clox_chunk_h

#include "common.h"
#include "value.h"

typedef enum {
  OP_CONSTANT,
  OP_NIL,
  OP_TRUE,
  OP_FALSE,
  OP_RETURN,

  OP_NEGATE,

  OP_MULT,
  OP_DIV,
  OP_ADD,
  OP_SUB,
} OpCode;

typedef struct {
  int count;
  int capacity;
  uint8_t* code; // A sequence of bytes.
  ValueArray constants;
} Chunk;

void initChunk(Chunk* chunk);
void freeChunk(Chunk* chunk);
void writeChunk(Chunk* chunk, uint8_t byte);
int addConstant(Chunk* chunk, Value value);


#endif
