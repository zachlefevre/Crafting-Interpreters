#include "common.h"
#include "chunk.h"
#include "debug.h"
#include <stdio.h>
#include "vm.h"


int main(int argc, const char* argv[]) {
  initVM();

  Chunk chunk;
  initChunk(&chunk);

  writeChunk(&chunk, OP_CONSTANT);
  int idx = addConstant(&chunk, 8.2);
  writeChunk(&chunk, idx);

  writeChunk(&chunk, OP_NEGATE);

  writeChunk(&chunk, OP_CONSTANT);
  idx = addConstant(&chunk, 1001);
  writeChunk(&chunk, idx);

  writeChunk(&chunk, OP_NEGATE);

  writeChunk(&chunk, OP_MULT);

  writeChunk(&chunk, OP_CONSTANT);
  idx = addConstant(&chunk, 2);
  writeChunk(&chunk, idx);

  writeChunk(&chunk, OP_MULT);

  writeChunk(&chunk, OP_RETURN);

  //  disassembleChunk(&chunk, "test chunk");

  interpret(&chunk);
  freeVM();
  freeChunk(&chunk);

  return 0;
}
