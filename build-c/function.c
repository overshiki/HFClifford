#include <stdio.h>
#include <stdbool.h> 

int factorial( int n )
{
  // if variable n equal 0 return 1
  // if not it will call myself 
  // that multiply n but with n - 1 as input
  return (n == 0) ? 1 : n * factorial(n - 1);
}

void bind_test(bool b, int i, float f);

void main() {
  bool b = false;
  int i = 10;
  float f = 3.14;
  printf("in c\n");
  printf("bool: %d\n", b);
  printf("integer: %i\n", i);
  printf("float: %f\n", f);

  printf("in fortran\n");
  bind_test(b, i, f);
}
