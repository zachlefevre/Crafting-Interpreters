#include <stdio.h>

#include "debug.h"

void disassembleChunk(Chunk* chunk, const char* name) {
  printf("== %s ==\n", name);
  for (int offset = 0; offset < chunk->count;) {
    offset = disassembleInstruction(chunk, offset);
  }

}


int simpleInstruction(const char* name, uint8_t instruction, int offset) {
  printf("instruction: %s -- %d\n", name, instruction);
  return offset + 1;
}

int disassembleInstruction(Chunk* chunk, int offset) {
  printf("offset: %04d\n", offset);
  uint8_t instruction = chunk->code[offset];
  switch (instruction) {
  case OP_RETURN:
    return simpleInstruction("OP_RETURN", instruction, offset);
  default:
    printf("Unknown code %d\n", instruction);
    return offset + 1;
  }
}
