#include <stdio.h>
#include <windows.h>

static int a = 1;

int main(int argc, char* argv[])
{
   (void) argc; (void) argv;

   int b = a+1;

   printf("%i %i\n",a,b);

   while(a<0xf0000000) {
      ++a;
      printf("%i\n",a++);
      Sleep(1000);
   }

   return 0;
}

// Local Variables:
// mode:c++
// c-basic-offset: 3
// indent-tabs-mode: nil
// End:
