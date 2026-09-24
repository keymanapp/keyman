/*
 * Keyman is copyright (C) SIL Global. MIT License.
 *
 * Track Windows use of Left Control + Right Alt emulation for Right Alt key
 */

#include "pch.h"
#include "kbd.h"  /* DDK kbdlayout */

#ifndef _WIN64
BOOL IsWow64();

// Note that this has been simplified to remove types as we do not need any data
// other than the flag fLocaleFlags.
typedef struct tagKbdLayer_WOW64 {
    /*
     * Modifier keys
     */
    DWORD pCharModifiers;
    DWORD filler1;

    /*
     * Characters
     */
    DWORD pVkToWcharTable;  // ptr to tbl of ptrs to tbl
    DWORD filler2;

    /*
     * Diacritics
     */
    DWORD pDeadKey;
    DWORD filler3;

    /*
     * Names of Keys
     */
    DWORD pKeyNames;
    DWORD filler4;
    DWORD pKeyNamesExt;
    DWORD filler5;
    DWORD pKeyNamesDead;
    DWORD filler6;

    /*
     * Scan codes to Virtual Keys
     */
    DWORD pusVSCtoVK;
    DWORD filler7;
    DWORD bMaxVSCtoVK;
    DWORD filler8;
    DWORD pVSCtoVK_E0;  // Scancode has E0 prefix
    DWORD filler9;
    DWORD pVSCtoVK_E1;  // Scancode has E1 prefix
    DWORD fillerA;

    /*
     * Locale-specific special processing
     */
    DWORD fLocaleFlags;
    DWORD fillerB;
} KBDTABLES_WOW64, *PKBDTABLES_WOW64;

typedef PKBDTABLES_WOW64 (WINAPI *PKBDLAYERDESCRIPTORWOW64FUNC)(VOID);

#endif

HMODULE LoadKbdLibrary(const char* keyboardLayoutName);
BOOL ReadAltGrFlagFromKbdDll(const char *keyboardLayoutName, BOOL& result);

typedef PKBDTABLES (WINAPI *PKBDLAYERDESCRIPTORFUNC)(VOID);

static HKL lastActiveHkl = 0;
static BOOL altGrFlag;

/**
 * Checks whether the active Windows system keyboard has mappings for RightAlt,
 * according to the flag KLLF_ALTGR from KBD.h, which implies that
 * LeftCtrl+RightAlt will be generated.

 * Background: when a Windows system keyboard that maps RightAlt (AltGr) is
 * active, Windows will internally generate LeftCtrl+RightAlt events when the
 * RightAlt key is pressed, so we need to recognize this and mask out the Left
 * Control modifier state when it happens.
 */

BOOL KeyboardGivesCtrlRAltForRAlt() {
  HKL hkl = GetKeyboardLayout(0);

  if(hkl != lastActiveHkl) {
    lastActiveHkl = hkl;

		char keyboardLayoutName[KL_NAMELENGTH+1];
		GetKeyboardLayoutName(keyboardLayoutName);

		ReadAltGrFlagFromKbdDll(keyboardLayoutName, altGrFlag);
  }

	return altGrFlag;
}

BOOL ReadAltGrFlagFromKbdDll(const char *keyboardLayoutName, BOOL& result) {
  BOOL success = FALSE;
  result = FALSE;

  HMODULE hKbdLibrary = LoadKbdLibrary(keyboardLayoutName);
  if(!hKbdLibrary) {
		SendDebugMessageFormat("Exit -- could not load library (error=%d)", GetLastError());
    return FALSE;
  }

#ifndef _WIN64
  // In a WOW64 process (i.e. 32 bit process on a 64 bit architecture), the
  // KBDTABLES structure returned from a keyboard DLL is 64-bit aligned, so we
  // need to account for that.
  if(IsWow64()) {
		// 64 bit structure in 32 bit process!
		PKBDLAYERDESCRIPTORWOW64FUNC KbdLayerDescriptorFunc = (PKBDLAYERDESCRIPTORWOW64FUNC) GetProcAddress(hKbdLibrary, "KbdLayerDescriptor");
		if(KbdLayerDescriptorFunc) {
			PKBDTABLES_WOW64 KbdTables = (*KbdLayerDescriptorFunc)();
			if(KbdTables) {
				result = (KbdTables->fLocaleFlags & KLLF_ALTGR) ? TRUE : FALSE;
        success = TRUE;
			}
		}
  }	else {
#else
	{
#endif
		// Native architecture - 32 bit structure in 32 bit process, or 64 bit structure in 64 bit process
		PKBDLAYERDESCRIPTORFUNC KbdLayerDescriptorFunc = (PKBDLAYERDESCRIPTORFUNC) GetProcAddress(hKbdLibrary, "KbdLayerDescriptor");
		if(KbdLayerDescriptorFunc) {
			PKBDTABLES KbdTables = (*KbdLayerDescriptorFunc)();
			if(KbdTables) {
				result = (KbdTables->fLocaleFlags & KLLF_ALTGR) ? TRUE : FALSE;
        success = TRUE;
			}
		}
	}

  FreeLibrary(hKbdLibrary);

	return success;
}

HMODULE LoadKbdLibrary(const char* keyboardLayoutName) {
  char registryKey[128],  layoutFile[MAX_PATH+1];

  RegistryReadOnly r(HKEY_LOCAL_MACHINE);

  wsprintf(registryKey, "System\\CurrentControlSet\\Control\\keyboard layouts\\%s", keyboardLayoutName);
  if(!r.OpenKeyReadOnly(registryKey)) {
		return FALSE;
	}
	if(!r.ReadString("Layout File", layoutFile, sizeof(layoutFile)/sizeof(layoutFile[0]))) {
		return FALSE;
	}

  return LoadLibrary(layoutFile);
}

BOOL fStored = FALSE;
BOOL bIsWow64 = FALSE;

BOOL IsWow64() {
  if(fStored) {
		return bIsWow64;
	}
	if(!IsWow64Process(GetCurrentProcess(), &bIsWow64)) {
		// Assume 32 bit machine on failure
		bIsWow64 = FALSE;
	}
  fStored = TRUE;
  return bIsWow64;
}
