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

int constantInstruction(const char* name, Chunk* chunk, uint8_t instruction, int offset) {
  uint8_t constantIdx = chunk->code[offset + 1];
  printf("instruction: %s(%d) -- %d\n", name, constantIdx, instruction);
  printValue(chunk->constants.values[constantIdx]);
  return offset + 2;
}

int disassembleInstruction(Chunk* chunk, int offset) {
  printf("offset: %04d\n", offset);
  uint8_t instruction = chunk->code[offset];
  switch (instruction) {
  case OP_RETURN:
    return simpleInstruction("OP_RETURN", instruction, offset);
  case OP_CONSTANT:
    return constantInstruction("OP_CONSTANT", chunk, instruction, offset);
  default:
    printf("Unknown code %d\n", instruction);
    return offset + 1;
  }
}
