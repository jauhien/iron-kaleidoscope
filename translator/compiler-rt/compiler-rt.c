#include <stdio.h>

double putchard(double x) {
   putchar(x);
   return x;
}

double printd(double x) {
   printf("> %f <", x);
   return x;
}
