#include "vm.h"
#include "common.h"
#include "debug.h"

#include <stdio.h>
VM vm;

void resetStack() {
  vm.stackTop = vm.stack;
}

void push(Value value) {
  *vm.stackTop = value;
  vm.stackTop++;
}

Value pop() {
  vm.stackTop--;
  return *vm.stackTop;
}

void initVM() {
  resetStack();
}

void freeVM() {

}

InterpretResult run() {
#define READ_BYTE() (*vm.ip++)
#define READ_CONSTANT() (vm.chunks->constants.values[READ_BYTE()])

#define BINARY_OP(op) do \
    { \
    double b = pop(); \
    double a = pop(); \
    push(a op b); \
    } while (false)

  for(;;) {
#ifdef DEBUG_TRACE_EXECUTION
    printf("\t[\n");
    for (Value* slot = vm.stack; slot < vm.stackTop; slot++) {
      printf("\t");
      printValue(*slot);
    }
    printf("\t]\n");

    disassembleInstruction(vm.chunks, (int)(vm.ip - vm.chunks->code));
#endif

    uint8_t instruction;
    switch (instruction = READ_BYTE()) { // bytecode dispatch
    case OP_RETURN: {
      Value value = pop();
      printf("Returning value");
      printValue(value);
      return INTERPRET_OK;
    }
    case OP_NEGATE: {
      push(-pop());
      break;
    }
    case OP_CONSTANT: {
      Value constant = READ_CONSTANT();
      push(constant);
      break;
    }

    case OP_ADD: BINARY_OP(+); break;
    case OP_SUB: BINARY_OP(-); break;
    case OP_MULT: BINARY_OP(*); break;
    case OP_DIV: BINARY_OP(/); break;
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
