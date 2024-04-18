
#include <stdio.h>
#include <wchar.h>
#include "mcompile.h"

//-------------------------------
int main () {
  std::cout << "Hier ist main von mcompile-mac.cpp...\n";

  std::u16string character;

  for (int i=0; i<8; i++) {
    character  = get_character_From_Keycode(24,14,i );  // all Shiftstates for ´` + e
    //wprintf( L" ` + e   for SHIFTSTATE %i -> ...\n", i, wstring_from_u16string(character););
  }

  printf("\n................................ END ..............................\n");
  return 0;
}


//--------------------------
void fun2() {  std::cout << "Hier ist fun2 von mcompile.cpp ...\n";}
