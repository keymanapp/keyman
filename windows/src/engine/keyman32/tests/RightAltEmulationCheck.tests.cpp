#include "pch.h"

extern BOOL ReadAltGrFlagFromKbdDll(const char *keyboardLayoutName, BOOL& result);


TEST(RightAltEmulationCheck, ReadAltGrFlagFromKbdDll) {
  // GetKeyboardLayoutName only returns active keyboard layout name; there is no
  // Windows API to return an arbitrary keyboard layout, so we need to pass in
  // a known keyboard layout

  // These keyboards do not use AltGr
  BOOL result = FALSE;

  // kbdus.dll - English (US)
  EXPECT_EQ(ReadAltGrFlagFromKbdDll("00000409", result), TRUE);
  EXPECT_EQ(result, FALSE);

  // kbda1.dll - Thai Kedmanee
  EXPECT_EQ(ReadAltGrFlagFromKbdDll("0000041e", result), TRUE);
  EXPECT_EQ(result, FALSE);

  // These keyboards use AltGr

  // kbdfr.dll - French AZERTY (Legacy)
  EXPECT_EQ(ReadAltGrFlagFromKbdDll("0000040C", result), TRUE);
  EXPECT_EQ(result, TRUE);

  // kbdcz.dll - Czech
  EXPECT_EQ(ReadAltGrFlagFromKbdDll("00000405", result), TRUE);
  EXPECT_EQ(result, TRUE);
}
