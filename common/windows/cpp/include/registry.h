/*
  Name:             registry
  Copyright:        Copyright (C) SIL International.
  Documentation:
  Description:
  Create Date:      20 Jun 2006

  Modified Date:    28 Mar 2016
  Authors:          mcdurdin
  Related Files:
  Dependencies:

  Bugs:
  Todo:
  Notes:
  History:          20 Jun 2006 - mcdurdin - Initial version
                    01 Aug 2006 - mcdurdin - Add KeymanHotkeys reg
                    05 Nov 2007 - mcdurdin - I1087 - Add hotkeys to switch languages
                    10 Sep 2008 - mcdurdin - I1627 - switch language with keyboard in keyman64
                    27 Jan 2009 - mcdurdin - I1797 - AIWin2000 registry fallback
                    30 Nov 2009 - mcdurdin - I934 - Prep for x64 - registry keys
                    29 Mar 2010 - mcdurdin - I1089 - One keyboard - all applications
                    25 May 2010 - mcdurdin - I1632 - Keyboard Options
                    01 Dec 2012 - mcdurdin - I3613 - V9.0 - System shadow keyboards obsolete, strip out remaining code
                    07 Nov 2013 - mcdurdin - I3951 - V9.0 - Add debug-to-console hidden option for Keyman32
                    16 Apr 2014 - mcdurdin - I4169 - V9.0 - Mnemonic layouts should be recompiled to positional based on user-selected base keyboard
                    28 May 2014 - mcdurdin - I4220 - V9.0 - Remove references to LoadKeyboardLayout, Preload, Substitutes, etc. and use only TSF
                    06 Feb 2015 - mcdurdin - I4552 - V9.0 - Add mnemonic recompile option to ignore deadkeys
                    28 Mar 2016 - mcdurdin - I4933 - Compat issue with Firefox 42 and IE and Keyman 9 TSF
*/

#ifndef _REGISTRY_H
#define _REGISTRY_H

#include <windows.h>

#ifdef _WIN64

#define REGSZ_KeymanCU    			"software\\keyman\\keyman engine"
#define REGSZ_KeymanDeveloperCU	"software\\keyman\\keyman developer"

#define REGSZ_KeymanLM			"software\\wow6432node\\keyman\\keyman engine"
#define REGSZ_KeymanDeveloperLM	"software\\wow6432node\\keyman\\keyman developer"

#else
// this below is same in NT 4 and Win95; others will be different
#define REGSZ_Keyman			"software\\keyman\\keyman engine"
#define REGSZ_KeymanDeveloper	"software\\keyman\\keyman developer"
#define REGSZ_KeymanCU  REGSZ_Keyman
#define REGSZ_KeymanLM  REGSZ_Keyman
#define REGSZ_KeymanDeveloperCU  REGSZ_Keyman
#define REGSZ_KeymanDeveloperLM  REGSZ_Keyman

//#define REGSZ_KeymanInstalledKeyboards	(REGSZ_KeymanCU "\\installed keyboards")

#endif

#define REGSZ_KeymanEngineDiag REGSZ_KeymanCU "\\Diag"

#define REGSZ_KeymanInstalledKeyboardsCU	REGSZ_KeymanCU "\\installed keyboards"
#define REGSZ_KeymanInstalledKeyboardsLM	REGSZ_KeymanLM "\\installed keyboards"

#define REGSZ_SystemKeyboardLayouts	"system\\currentcontrolset\\control\\keyboard layouts"
#define REGSZ_KeymanActiveKeyboards		REGSZ_KeymanCU "\\active keyboards"

#define REGSZ_KeyboardOptions "options" // active keyboards\<keyboardname>\options
#define REGSZ_SharedKeyboardOptions REGSZ_KeyboardOptions "\\shared"

#define REGSZ_KeymanActiveLanguages REGSZ_KeymanCU "\\active languages"
#define REGSZ_KeymanLanguageHotkeys REGSZ_KeymanCU "\\language hotkeys"

#ifdef _WIN64
#define REGSZ_KeymanAddinsCU         		REGSZ_KeymanCU "\\add-ins (x64)"
#define REGSZ_KeymanAddinsLM         		REGSZ_KeymanLM "\\add-ins (x64)"
#else
#define REGSZ_KeymanAddinsCU         		REGSZ_KeymanCU "\\add-ins"
#define REGSZ_KeymanAddinsLM         		REGSZ_KeymanLM "\\add-ins"
#endif

#define REGSZ_KeymanHotkeys				REGSZ_KeymanCU "\\hotkeys"

#define REGSZ_AppIntegration  REGSZ_KeymanLM "\\app integration"   // I4933

//#define REGSZ_SystemNLSLocale "system\\currentcontrolset\\control\\Nls\\Locale"
   // I4220

#define REGSZ_KeymanFile		"keyman file"
#define REGSZ_KeymanFile_MnemonicOverride "keyman file mnemonic override"   // I4169
#define REGSZ_KeymanFile_MnemonicOverride_Deadkey "keyman file mnemonic override deadkey"   // I4552
#define REGSZ_KeymanError		"keyman error"
   // I3613
#define REGSZ_LanguageProfiles "Language Profiles"
#define REGSZ_TransientLanguageProfiles "Transient Language Profiles"
#define REGSZ_ProfileGUID   "profile guid"
#define REGWSZ_ProfileGUID L"" ## REGSZ_ProfileGUID
#define REGSZ_LanguageProfiles_LangID "LangID"

#define REGSZ_LayoutID			"layout id"

#define REGSZ_Debug				"debug"
#define REGSZ_DebugToConsole  "debug to console"   // I3951

#define REGSZ_UnderlyingLayout	"underlying layout"

#define REGSZ_ShowVisualKeyboard	"show visual keyboard"

#define REGSZ_RootPath			"root path"

#define REGSZ_SimulateAltGr     "simulate altgr"
#define REGSZ_KeyboardHotkeysAreToggle "hotkeys are toggles"
#define REGSZ_DeadkeyConversionMode    "deadkey conversion mode"                // CU   // I4552
#define REGSZ_ZapVirtualKeyCode        "zap virtual key code"   // LM, defaults to 0x0E (_VK_PREFIX_DEFAULT)
/*
  Debug flags
  These are all stored in HKCU\Software\Keyman\Debug
*/

#define REGSZ_Keyman_Debug  REGSZ_KeymanCU "\\Debug"

/* REGSZ_Keyman_Debug DWORD: Enable/disable serialized input, default 1 */

#define REGSZ_Flag_ShouldSerializeInput "Flag_ShouldSerializeInput"

/* REGSZ_Keyman_Debug DWORD: Use old non-chiral Win32 API RegisterHotkey instead of left-only hotkeys */

#define REGSZ_Flag_UseRightModifierHotKey  "Flag_UseRightModifierHotKey"

/* DWORD: Enable/disable deep TSF integration, default enabled; 0 = disabled, 1 = enabled, 2 = default */

#define REGSZ_DeepTSFIntegration  "deep tsf integration"

/* Privacy settings - all DWORD 0/1 */

#define REGSZ_IDEOptions_CU   ( REGSZ_KeymanDeveloperCU "\\IDE\\Options" )

#define REGSZ_AutomaticallyReportErrors "automatically report errors"              // CU, SRegKey_IDEOptions and SRegKey_KeymanEngine_CU
#define REGSZ_AutomaticallyReportUsage  "automatically report usage"               // CU, SRegKey_IDEOptions and SRegKey_KeymanEngine_CU


/* Splitting Registry into ReadOnly and FullAccess makes it much easier to ensure that we are using the registry
   correctly -- readonly wherever possible. */

class RegistryReadOnly
{
protected:
	HKEY FhRootKey;
	HKEY FhKey;

	BOOL WrapError(DWORD res);
	HKEY GetKeyReadOnly(LPCSTR AKey);

public:
	RegistryReadOnly(HKEY AhRootKey);
	~RegistryReadOnly();

	HKEY hKey() { return FhKey; }

	BOOL CloseKey(void);
	BOOL GetKeyNames(LPSTR AKey, int len, int n);
	BOOL GetValueNames(LPSTR AName, int len, int n);
	BOOL GetValueNames(LPWSTR AName, int len, int n);
	BOOL KeyExists(LPCSTR AKey);
	BOOL OpenKeyReadOnly(LPCSTR AKey);
	int ReadInteger(LPCSTR AName);
	BOOL ReadString(LPCSTR AName, LPSTR AValue, int len);
	BOOL ReadString(LPCWSTR AName, LPWSTR AValue, int len);
	BOOL ValueExists(LPCSTR AName);
	BOOL ValueExists(LPCWSTR AName);
};

class RegistryFullAccess: public RegistryReadOnly
{
protected:
	HKEY GetKey(LPCSTR AKey);
	BOOL IntRecursiveDeleteKey(HKEY hkey);

public:
	RegistryFullAccess(HKEY AhRootKey) : RegistryReadOnly(AhRootKey) {};
	~RegistryFullAccess() {};

	BOOL RecursiveDeleteKey(LPCSTR AKey);
	BOOL CreateKey(LPCSTR AKey);
	BOOL DeleteKey(LPCSTR AKey);
	BOOL DeleteValue(LPCSTR AName);
	BOOL OpenKey(LPCSTR AKey, BOOL ACreate);
	BOOL WriteInteger(LPCSTR AName, const int AValue);
	BOOL WriteString(LPCSTR AName, LPCSTR AValue);
	BOOL WriteString(LPCWSTR AName, LPCWSTR AValue);
};

RegistryReadOnly *Reg_GetKeymanActiveKeyboard(LPSTR kbname);
RegistryReadOnly *Reg_GetKeymanInstalledKeyboard(LPSTR kbname);

#endif
