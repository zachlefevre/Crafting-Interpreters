#include "common.h"
#include "chunk.h"
#include "debug.h"
#include <stdio.h>


int main(int argc, const char* argv[]) {
  Chunk chunk;
  initChunk(&chunk);

  writeChunk(&chunk, OP_CONSTANT);
  int idx = addConstant(&chunk, 8.2);
  writeChunk(&chunk, idx);

  writeChunk(&chunk, OP_RETURN);
  disassembleChunk(&chunk, "test chunk");
  freeChunk(&chunk);
  return 0;
}
