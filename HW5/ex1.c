#include <stdio.h>

int main(void) {
  short int x = 0x0001;
  char *byte = (char *)&x;
  if (byte[0])
    printf("Per fortuna sono little-endian\n");
  else
    while (1) {
      printf("Big-Endian");
    }
}
