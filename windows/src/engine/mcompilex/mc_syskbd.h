
#ifndef _SYSKBD_H
#define _SYSKBD_H

#define VK_COLON	0xBA
#define VK_EQUAL	0xBB
#define VK_COMMA	0xBC
#define VK_HYPHEN	0xBD
#define VK_PERIOD	0xBE
#define	VK_SLASH	0xBF
#define VK_ACCENT	0xC0
#define VK_LBRKT	0xDB
#define VK_BKSLASH	0xDC
#define VK_RBRKT	0xDD
#define VK_QUOTE	0xDE
#define VK_xDF		0xDF

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
