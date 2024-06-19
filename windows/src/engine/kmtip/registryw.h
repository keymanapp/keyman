/*
  Name:             registryw
  Copyright:        Copyright (C) 2003-2017 SIL International.
  Documentation:
  Description:
  Create Date:      1 Dec 2012

  Modified Date:    1 Dec 2012
  Authors:          mcdurdin
  Related Files:
  Dependencies:

  Bugs:
  Todo:
  Notes:
  History:          01 Dec 2012 - mcdurdin - I3622 - V9.0 - Add Registry*W classes for Unicode

*/
#ifndef _REGISTRYW_H   // I3622
#define _REGISTRYW_H

#include <windows.h>

#include "../../../../common/windows/cpp/include/keymanversion.h"

#ifdef _WIN64

#define REGSZ_KeymanCUW    			L"software\\keyman\\keyman engine\\" KEYMANVERSIONW
#define REGSZ_KeymanDeveloperCUW	L"software\\keyman\\keyman developer\\" KEYMANVERSIONW

#define REGSZ_KeymanLMW			L"software\\wow6432node\\keyman\\keyman engine\\" KEYMANVERSIONW
#define REGSZ_KeymanDeveloperLMW	L"software\\wow6432node\\keyman\\keyman developer\\" KEYMANVERSIONW

#else
// this below is same in NT 4 and Win95; others will be different
#define REGSZ_KeymanW			L"software\\keyman\\keyman engine\\" KEYMANVERSIONW
#define REGSZ_KeymanDeveloperW	L"software\\keyman\\keyman developer\\" KEYMANVERSIONW
#define REGSZ_KeymanCUW  REGSZ_KeymanW
#define REGSZ_KeymanLMW  REGSZ_KeymanW
#define REGSZ_KeymanDeveloperCUW  REGSZ_KeymanW
#define REGSZ_KeymanDeveloperLMW  REGSZ_KeymanW

//#define REGSZ_KeymanInstalledKeyboards	(REGSZ_KeymanCU "\\installed keyboards")

#endif

#define REGSZ_KeymanEngineDiagW REGSZ_KeymanCUW L"\\Diag"

#define REGSZ_KeymanInstalledKeyboardsCUW	REGSZ_KeymanCUW L"\\installed keyboards"
#define REGSZ_KeymanInstalledKeyboardsLMW	REGSZ_KeymanLMW L"\\installed keyboards"

#define REGSZ_SystemKeyboardLayoutsW	L"system\\currentcontrolset\\control\\keyboard layouts"
#define REGSZ_KeymanActiveKeyboardsW		REGSZ_KeymanCUW L"\\active keyboards"

#define REGSZ_KeyboardOptionsW L"options" // active keyboards\<keyboardname>\options
#define REGSZ_SharedKeyboardOptionsW REGSZ_KeyboardOptionsW L"\\shared"

#define REGSZ_KeymanActiveLanguagesW REGSZ_KeymanCUW L"\\active languages"
#define REGSZ_KeymanLanguageHotkeysW REGSZ_KeymanCUW L"\\language hotkeys"

#define REGSZ_KeymanHotkeysW				REGSZ_KeymanCUW L"\\hotkeys"

#define REGSZ_EvaluationW		L"evaluation"

//#define REGSZ_SystemNLSLocale "system\\currentcontrolset\\control\\Nls\\Locale"

#define REGSZ_UseOldKeyStuffingAlgorithmW L"use old key stuffing algorithm"  // I1797

#define REGSZ_RegistrationKeyW	L"registration key"
#define REGSZ_ProductStatusW		L"product status"

#define REGSZ_KeymanActiveHotkeyW		L"keyman active hotkey"

#define REGSZ_KeymanFileW		L"keyman file"
#define REGSZ_KeymanErrorW		L"keyman error"
#define REGSZ_KeymanKeyboardIDW	L"keyman keyboard id"

#define REGSZ_KeymanNameW		L"keyman name"
#define REGSZ_LayoutFileW		L"layout file"
#define REGSZ_LayoutTextW		L"layout text"
#define REGSZ_LayoutIDW			L"layout id"

#define REGSZ_UnknownKeyboardIDW	L"unknown layout id"
#define REGSZ_DebugW				L"debug"

#define REGSZ_UnderlyingLayoutW	L"underlying layout"

#define REGSZ_ShowVisualKeyboardW	L"show visual keyboard"

#define REGSZ_SwitchLanguageWithKeyboardW L"switch language with keyboard"

#define REGSZ_RootPathW			L"root path"

/*
#define REGSZ_KeymanOffHotkey				"keyman hotkey"
#define REGSZ_VisualKeyboardHotkey			"visual keyboard hotkey"
#define REGSZ_KeymanMenuHotkey				"keyman menu hotkey"
#define REGSZ_KeymanConfigurationHotkey		"keyman configuration hotkey"
*/

#define REGSZ_SimulateAltGrW     L"simulate altgr"
#define REGSZ_KeyboardHotkeysAreToggleW L"hotkeys are toggles"

#define REGSZ_ShouldShowStartupW		L"show keyman startup"
#define REGSZ_ShouldStartInternatW	L"should start internat"

#define REGSZ_KeymanEngineDebug_CU  L"software\\keyman\\keyman engine\\Debug"
#define REGSZ_Flag_UseAutoStartTask L"Flag_UseAutoStartTask"

/* Registry keys for upgrade purposes only */

//#define REGSZ_KeymanDeveloper50W	L"software\\tavultesoft\\keyman developer\\5.0"
//#define REGSZ_Keyman50W			L"software\\tavultesoft\\keyman\\5.0"

/* Splitting Registry into ReadOnly and FullAccess makes it much easier to ensure that we are using the registry
   correctly -- readonly wherever possible. */

class RegistryReadOnlyW
{
protected:
	HKEY FhRootKey;
	HKEY FhKey;

	BOOL WrapError(DWORD res);
	HKEY GetKeyReadOnly(LPCWSTR AKey);

public:
	RegistryReadOnlyW(HKEY AhRootKey);
	~RegistryReadOnlyW();

	HKEY hKey() { return FhKey; }

	BOOL CloseKey(void);
	BOOL GetKeyNames(LPWSTR AKey, int len, int n);
	BOOL GetValueNames(LPWSTR AName, int len, int n);
	BOOL KeyExists(LPCWSTR AKey);
	BOOL OpenKeyReadOnly(LPCWSTR AKey);
	int ReadInteger(LPCWSTR AName);
	BOOL ReadString(LPCWSTR AName, LPWSTR AValue, int len);
	BOOL ValueExists(LPCWSTR AName);
};

class RegistryFullAccessW: public RegistryReadOnlyW
{
protected:
	HKEY GetKey(LPCWSTR AKey);
	BOOL IntRecursiveDeleteKey(HKEY hkey);

public:
	RegistryFullAccessW(HKEY AhRootKey) : RegistryReadOnlyW(AhRootKey) {};
	~RegistryFullAccessW() {};

	BOOL RecursiveDeleteKey(LPCWSTR AKey);
	BOOL CreateKey(LPCWSTR AKey);
	BOOL DeleteKey(LPCWSTR AKey);
	BOOL DeleteValue(LPCWSTR AName);
	BOOL OpenKey(LPCWSTR AKey, BOOL ACreate);
	BOOL WriteInteger(LPCWSTR AName, const int AValue);
	BOOL WriteString(LPCWSTR AName, LPCWSTR AValue);
};

RegistryReadOnlyW *Reg_GetKeymanActiveKeyboardW(LPWSTR kbname);
RegistryReadOnlyW *Reg_GetKeymanInstalledKeyboardW(LPWSTR kbname);

#endif
