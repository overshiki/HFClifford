#include <stdio.h>
#include <stdbool.h> 

int factorial( int n )
{
  // if variable n equal 0 return 1
  // if not it will call myself 
  // that multiply n but with n - 1 as input
  return (n == 0) ? 1 : n * factorial(n - 1);
}

// void plus_test(bool n);

void call_plus(bool n) {
  printf("in c\n");
  printf("%d\n", n);
  printf("in fortran\n");
  plus_test(n);
}