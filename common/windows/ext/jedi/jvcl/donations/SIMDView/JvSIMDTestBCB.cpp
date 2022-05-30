//-----------------------------------------------------------------------------
//The contents of this file are subject to the Mozilla Public License
//Version 1.1 (the "License"); you may not use this file except in compliance
//with the License. You may obtain a copy of the License at
//http://www.mozilla.org/MPL/MPL-1.1.html

//Software distributed under the License is distributed on an "AS IS" basis,
//WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
//the specific language governing rights and limitations under the License.

//The Original Code is: JvSIMDTest.dpr, released on 2004-10-11.

//The Initial Developer of the Original Code is Florent Ouchet [ouchet dott florent att laposte dott net]
//Portions created by Florent Ouchet are Copyright (C) 2004 Florent Ouchet.
//All Rights Reserved.

//Contributor(s): -

//You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
//located at http://jvcl.sourceforge.net

//Known Issues:
//-----------------------------------------------------------------------------
// $Id$
//---------------------------------------------------------------------------

#pragma hdrstop

#include <iostream>
#include <iomanip>

//---------------------------------------------------------------------------

#pragma argsused

#if __BORLANDC__ == 1380
#define BCB6
#endif

#if __BORLANDC__ == 1360
#define BCB5
#endif

#ifdef BCB5
#define COMPILER5_UP
#define COMPILER5
#endif

#ifdef BCB6
#define COMPILER6_UP
#define COMPILER5_UP
#define COMPILER6
#endif

int main(int argc, char* argv[])
{
  using namespace std;
  float Values[4];
  int Index, ErrorCode;
  char Line[256];

  cout << "Streaming SIMD Extensions of Intel Pentium and AMD Athlon processors\n";
  cout << "By Ouchet Florent <outchy_at_users.sourceforge.net>\n";
  cout << "Released 2004,14,3\n";
  cout << "All rights free\n\n";

  for (Index=0; Index<4; Index++) {
    do {
      cout << "Enter the floating point value " << Index << " : ";
      gets(Line);
      ErrorCode = sscanf(Line,"%f",Values+Index);
    } while (ErrorCode!=1);
  }

  cout << "\nCheck values :\n";
  for (Index=0; Index<4; Index++)
    cout << "Value " << Index << " is : " << Values[Index] << "\n";

  cout << "\nStarting computations : Values*2 ...";
  __asm {
    // breakpoint here
    // hit ctrl+alt+D or go to View/Debug window and open the last item
    // these instructions operate on 4-packed-single-precision floating point values
    // so you should view these registers has single values
    LEA      EAX,  Values
#ifdef COMPILER6_UP
    movups   xmm0, [eax]      // moving Values to xmm0
    addps    xmm0, xmm0       // xmm0 <- xmm0 + xmm0
    movups   [eax], xmm0      // moving xmm0 to Values
#else
    DB       0Fh, 10h, 00h    // movups xmm0, [eax]
    DB       0Fh, 58h, 0C0h   // addps xmm0, xmm0
    DB       0Fh, 11h, 00h    // movups [eax], xmm0
#endif
  };
  cout << "Computations ended\nNow values are :\n";
  for (Index=0; Index<4; Index++)
    cout << "Value " << Index << " is : " << Values[Index] << "\n";
  cout << "\nProgram terminated\n";
  gets(Line);
  return 0;
}
//---------------------------------------------------------------------------
