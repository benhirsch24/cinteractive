#include <stdio.h>

int global_var;

int dup(int num) {
   int i;
   int a;
   for (i = 0; i < num; i++)
      a = num + 1;

   return a;
}

int main(int argc, char **argv) {
   int num = 5;
   num = dup(num);
   printf("%d\n", num);

   return 0;
}
