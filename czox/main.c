#include "common.h"
#include "chunk.h"
#include "debug.h"

#include <stdio.h>
#include <string.h>
#include <stdlib.h>

#include "vm.h"


void repl() {
  char line[1024];
  for (;;) {
    printf("> ");
    if (fgets(line, sizeof(line), stdin)) {
      interpret(line);
    } else {
      printf("\n");
      break;
    }
  }
}

static char* readFile(const char* path) {
  FILE* file = fopen(path, "rb");
  fseek(file, 0L, SEEK_END);
  size_t fileSize = ftell(file);
  rewind(file);

  char* buffer = (char*)malloc(fileSize + 1);
  size_t bytesRead = fread(buffer, sizeof(char), fileSize, file);
  buffer[bytesRead] = '\0';

  fclose(file);
  return buffer;

}

void runFile(const char* path) {
  char* source = readFile(path);
  InterpretResult result = interpret(source);
  free(source);


  if (result == INTERPRET_COMPILE_ERROR) exit(65);
  if (result == INTERPRET_RUNTIME_ERROR) exit(65);
}

int main(int argc, const char* argv[]) {
  initVM();

  if (argc == 1) {
    repl();
  } else if (argc == 2) {
    runFile(argv[1]);
  }else {
    fprintf(stderr, "Usage: clox [path]\n");
  }

  freeVM();
}
