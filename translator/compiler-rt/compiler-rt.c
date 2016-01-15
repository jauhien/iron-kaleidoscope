#include <stdio.h>

extern double run();

double putchard(double x) {
   putchar(x);
   return x;
}

double printd(double x) {
   printf("> %f <", x);
   return x;
}

int main(int arc, char** argv) {
   return run();
}
