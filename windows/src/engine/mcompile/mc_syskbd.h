
#include "../../../../common/include/km_vkey.h"

#ifndef _SYSKBD_H
#define _SYSKBD_H

BOOL LoadNewLibrary(PWSTR filename);
WCHAR CharFromVK(WORD VKey, UINT ShiftFlags, WCHAR *PDeadKey);
WORD VKUSToVKUnderlyingLayout(WORD VKey);
WORD VKUnderlyingLayoutToVKUS(WORD VKey);
int GetDeadkeys(WORD DeadKey, WORD *OutputPairs);  // returns array of [USVK, ch] pairs

BOOL IsNumberPadKey(WORD VKey);

extern const UINT USVirtualKeyToScanCode[256];
extern const UINT ScanCodeToUSVirtualKey[128];
extern const int VKContextReset[256];

#endif
