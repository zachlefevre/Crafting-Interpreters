#include "vm.h"
#include "common.h"
#include "debug.h"

VM vm;

void initVM() {

}

void freeVM() {

}

InterpretResult run() {
#define READ_BYTE() (*vm.ip++)
#define READ_CONSTANT() (vm.chunks->constants.values[READ_BYTE()])

  for(;;) {
#ifdef DEBUG_TRACE_EXECUTION
    disassembleInstruction(vm.chunks, (int)(vm.ip - vm.chunks->code));
#endif
    uint8_t instruction;
    switch (instruction = READ_BYTE()) { // bytecode dispatch
    case OP_RETURN: {
      return INTERPRET_OK;
    }
    case OP_CONSTANT: {
      Value constant = READ_CONSTANT();
      break;
    }

    }
  }

#undef READ_BYTE
#undef READ_CONSTANT
}

InterpretResult interpret(Chunk* chunk) {
  vm.chunks = chunk;
  vm.ip = vm.chunks->code;
  return run();
}
