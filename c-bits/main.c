#include <stdio.h>
#include <stdlib.h>

extern int our_code_starts_here() asm("our_code_starts_here");

int print(int val) {
  if(val == 0xFFFFFFFF){
    printf("true\n");
  }
  else if(val == 0x7FFFFFFF){
    printf("false\n");
  }
  else{
    printf("%d\n", val >> 1);
  }
  //printf("Unknown value: %#010x\n", val);
  return val;
}

int main(int argc, char** argv) {
  int result = our_code_starts_here();
  print(result);
  return 0;
}

void error(int code, int val) {
  if (code == 0) {
    fprintf(stderr, "Error: expected a number but got %#010x\n", val);
  }
  else if (code == 1) {
    fprintf(stderr, "Error: expected a boolean but got %#010x\n", val);
    // print out message for errorcode 1 ...
  }
  else if (code == 2) {
    fprintf(stderr, "arithmetic overflow, got this: %#010x\n", val);
    // print out message for errorcode 1 ...
  }
  exit(1);
}
