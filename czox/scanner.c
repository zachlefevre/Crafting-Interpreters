#include <stdio.h>
#include <string.h>

#include "common.h"
#include "scanner.h"


typedef struct {
  const char* start;
  const char* current;
} Scanner;

Scanner scanner;


void initScanner(const char* source) {
  scanner.start = source;
  scanner.current = source;
}
