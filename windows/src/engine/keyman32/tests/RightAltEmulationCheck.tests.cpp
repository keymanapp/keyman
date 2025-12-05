#include "pch.h"

extern BOOL ReadAltGrFlagFromKbdDll(const char *keyboardLayoutName);


TEST(RightAltEmulationCheck, ReadAltGrFlagFromKbdDll) {
  // GetKeyboardLayoutName only returns active keyboard layout name; there is no
  // Windows API to return an arbitrary keyboard layout, so we need to pass in
  // a known keyboard layout

  // These keyboards do not use AltGr

  EXPECT_EQ(ReadAltGrFlagFromKbdDll("00000409"), FALSE); // kbdus.dll - English (US)
  EXPECT_EQ(ReadAltGrFlagFromKbdDll("00000401"), FALSE); // kbda1.dll - Arabic 101

  // These keyboards use AltGr

  EXPECT_EQ(ReadAltGrFlagFromKbdDll("0000040C"), TRUE); // kbdfr.dll - French AZERTY (Legacy)
  EXPECT_EQ(ReadAltGrFlagFromKbdDll("00000405"), TRUE); // kbdcz.dll - Czech
}
