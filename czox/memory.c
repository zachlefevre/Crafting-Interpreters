#include <stdlib.h>

#include "memory.h"

// confused since reallocate does not even use oldSize

void* reallocate(void* pointer, size_t oldSize, size_t newSize) {
  if (newSize == 0) {
    free(pointer);
    return NULL;
  }

  // when oldSize is zero realloc is equivalent to malloc
  // realloc can always shrink the allocated block and return the additional memory to the system and return the current pointer
  // the realloc can sometimes allocate more memory, but might not be able to.
  void* result = realloc(pointer, newSize);

  if(result == NULL) exit(1);

  return result;
}
