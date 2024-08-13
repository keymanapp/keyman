(*
  Name:             kmxfileconsts
  Copyright:        Copyright (C) SIL International.
  Documentation:
  Description:
  Create Date:      27 Mar 2008

  Modified Date:    27 Aug 2012
  Authors:          mcdurdin
  Related Files:
  Dependencies:

  Bugs:
  Todo:
  Notes:
  History:          27 Mar 2008 - mcdurdin - Initial version (Refactored)
                    25 May 2009 - mcdurdin - Use names for functions rather than codes in KMW compiler error messages
                    17 Aug 2012 - mcdurdin - I3429 - V9.0 - Add support for if, set, reset, save to KeymanWeb compiler
                    17 Aug 2012 - mcdurdin - I3430 - V9.0 - Add support for if(&platform) and if(&baselayout) to compilers
                    27 Aug 2012 - mcdurdin - I3437 - V9.0 - Add support for set(&layer) and layer()
                    27 Aug 2012 - mcdurdin - I3438 - V9.0 - Add support for custom virtual keys
*)
unit kmxfileconsts;

interface

const
  SZMAX_STORENAME	= 80;
  SZMAX_GROUPNAME = 80;
  SZMAX_DEADKEYNAME = 80;
  SZMAX_VKDICTIONARYNAME = 80;  // I3438

  SZMAX_LANGUAGENAME = 80;
  SZMAX_KEYBOARDNAME = 80;
  SZMAX_COPYRIGHT = 256;
  SZMAX_MESSAGE = 1024;

  BEGIN_ANSI = 0;
  BEGIN_UNICODE = 1;

  UC_SENTINEL = $FFFF;
  UC_SENTINEL_EXTENDEDEND	= $0010; // was ((CODE_LASTCODE)+1)... what was I thinking?

  CODE_FIRSTCODE = $01;

  CODE_ANY			= $01;
  CODE_INDEX			= $02;
  CODE_CONTEXT		= $03;
  CODE_NUL			= $04;
  CODE_USE			= $05;
  CODE_RETURN			= $06;
  CODE_BEEP			= $07;
  CODE_DEADKEY		= $08;
//	= $09 = bkspace.-- we don't need to keep this separate though with UC_SENTINEL
  CODE_EXTENDED		= $0A;
//#define CODE_EXTENDEDEND	= $0B  deprecated
  CODE_SWITCH			= $0C;
  CODE_KEY			= $0D;
  CODE_CLEARCONTEXT	= $0E;
  CODE_CALL		 =	$0F;
// UC_SENTINEL_EXTENDEDEND  0x10
  CODE_CONTEXTEX = $11;

  CODE_NOTANY   = $12;

  // Keyman 8
  CODE_SETOPT   = $13;  // I3429
  CODE_IFOPT    = $14;  // I3429
  CODE_SAVEOPT  = $15;  // I3429
  CODE_RESETOPT = $16;  // I3429

  // Keyman 9
  CODE_IFSYSTEMSTORE = $17;  // I3430
  CODE_SETSYSTEMSTORE = $18;  // I3437
  CODE_LASTCODE = $18;

const
  KMXCodeNames: array[CODE_FIRSTCODE..CODE_LASTCODE] of string = (
    'any', 'index', 'context', 'nul', 'use', 'return', 'beep', 'deadkey',
    '',
    'extended', '', 'switch', 'key', 'clearcontext', 'call',
    '', 'contextex', 'notany',
    'set', 'if', 'save', 'reset',  // I3429
    'if(&system)', 'set(&system)');  // I3430    // I3437

const
  VERSION_30 = $00000300;
  VERSION_31 = $00000301;
  VERSION_32 = $00000302;
  VERSION_40 = $00000400;
  VERSION_50 = $00000500;
  VERSION_501 = $00000501;
  VERSION_60 = $00000600;
  VERSION_70 = $00000700;
  VERSION_80 = $00000800;
  VERSION_90 = $00000900;
  VERSION_100 = $00000A00;
  VERSION_140 = $00000E00;
  VERSION_150 = $00000F00;
  VERSION_160 = $00001000;
  VERSION_170 = $00001100;

  VERSION_MIN	= VERSION_50;
  VERSION_MAX	= VERSION_170;

  VERSION_MASK_MINOR = $00FF;
  VERSION_MASK_MAJOR = $FF00;

const
    HK_ALT   = $00010000;
    HK_CTRL	 = $00020000;
    HK_SHIFT = $00040000;

const
  KMX_LCTRLFLAG      = $0001;	// Left Control flag
  KMX_RCTRLFLAG      = $0002;	// Right Control flag
  KMX_LALTFLAG       = $0004;	// Left Alt flag
  KMX_RALTFLAG       = $0008;	// Right Alt flag
  KMX_SHIFTFLAG      = $0010;	// Either shift flag
  KMX_CTRLFLAG       = $0020;	// Either ctrl flag
  KMX_ALTFLAG        = $0040;	// Either alt flag
  KMX_CAPITALFLAG    = $0100;	// Caps lock on
  KMX_NOTCAPITALFLAG = $0200;	// Caps lock NOT on
  KMX_NUMLOCKFLAG    = $0400;	// Num lock on
  KMX_NOTNUMLOCKFLAG = $0800;	// Num lock NOT on
  KMX_SCROLLFLAG     = $1000;	// Scroll lock on
  KMX_NOTSCROLLFLAG  = $2000;	// Scroll lock NOT on
  KMX_ISVIRTUALKEY   = $4000;	// It is a Virtual Key Sequence
  KMX_VIRTUALCHARKEY = $8000; // It is a virtual character key sequence - mnemonic layouts

  // Note: KMX_OTHER_MODIFIER = $10000, used by KMX+ for the
  // other modifier flag in layers, > 16 bit so not available here.
  // See keys_mod_other in keyman_core_ldml.ts

  // Combinations of key masks
  KMX_MASK_MODIFIER_CHIRAL = KMX_LCTRLFLAG or KMX_RCTRLFLAG or KMX_LALTFLAG or KMX_RALTFLAG;
  KMX_MASK_MODIFIER_SHIFT = KMX_SHIFTFLAG;
  KMX_MASK_MODIFIER_NONCHIRAL = KMX_CTRLFLAG or KMX_ALTFLAG;
  KMX_MASK_STATEKEY = KMX_CAPITALFLAG or KMX_NOTCAPITALFLAG or
                      KMX_NUMLOCKFLAG or KMX_NOTNUMLOCKFLAG or
                      KMX_SCROLLFLAG or KMX_NOTSCROLLFLAG;
  KMX_MASK_KEYTYPE  = KMX_ISVIRTUALKEY or KMX_VIRTUALCHARKEY;

  KMX_MASK_MODIFIER = KMX_MASK_MODIFIER_CHIRAL or KMX_MASK_MODIFIER_SHIFT or KMX_MASK_MODIFIER_NONCHIRAL;
  KMX_MASK_KEYS     = KMX_MASK_MODIFIER or KMX_MASK_STATEKEY;
  KMX_MASK_VALID    = KMX_MASK_KEYS or KMX_MASK_KEYTYPE;

const
  TSS_NONE                = 0;
  TSS_BITMAP              = 1;
  TSS_COPYRIGHT           = 2;
  TSS_HOTKEY              = 3;
  TSS_LANGUAGE            = 4;
  TSS_LAYOUT              = 5;
  TSS_MESSAGE             = 6;
  TSS_NAME                = 7;
  TSS_VERSION             = 8;
  TSS_CAPSONONLY          = 9;
  TSS_CAPSALWAYSOFF       = 10;
  TSS_SHIFTFREESCAPS      = 11;
  TSS_LANGUAGENAME        = 12;

  TSS_CALLDEFINITION	    = 13;
  TSS_CALLDEFINITION_LOADFAILED = 14;

  TSS_ETHNOLOGUECODE      = 15;

  TSS_DEBUG_LINE          = 16;

  TSS_MNEMONIC            = 17;

  TSS_INCLUDECODES        = 18;

  TSS_OLDCHARPOSMATCHING  = 19;

  TSS_COMPILEDVERSION     = 20;

  TSS_KEYMANCOPYRIGHT     = 21;

  TSS_CUSTOMKEYMANEDITION = 22;
  TSS_CUSTOMKEYMANEDITIONNAME = 23;

  { V7.0+ }
  TSS_VISUALKEYBOARD      = 24;
  TSS_KMW_RTL = 25;
  TSS_KMW_HELPFILE = 26;
  TSS_KMW_HELPTEXT = 27;
  TSS_KMW_EMBEDJS = 28;

  { V7.0.244.0+ }
  TSS_WINDOWSLANGUAGES    = 29;

  { V8.0 }

  TSS_COMPARISON = 30;

  { V9.0 }

  TSS_PLATFORM     = 31;  // I3430
  TSS_BASELAYOUT   = 32;  // I3430
  TSS_LAYER        = 33;  // I3437
  TSS_VKDICTIONARY = 34;  // I3438    // Dictionary of virtual key names for v9 dynamic layouts
  TSS_LAYOUTFILE   = 35;  // I3483
  TSS_KEYBOARDVERSION = 36;   // I4140
  TSS_KMW_EMBEDCSS = 37;   // I4368
  TSS_TARGETS = 38;   // I4504

  { V14.0 }

  TSS_CASEDKEYS = 39;

  { V15.0 }

  TSS_BEGIN_NEWCONTEXT = 40;
  TSS_BEGIN_POSTKEYSTROKE = 41;
  TSS_NEWLAYER = 42;
  TSS_OLDLAYER = 43;

  TSS__MAX = 43;

type
  TSystemStore = (ssNone = 0, ssBitmap = 1, ssCopyright = 2, ssHotkey = 3, ssLanguage = 4, ssLayout = 5, ssMessage = 6,
    ssName = 7, ssVersion = 8, ssCapsOnOnly = 9, ssCapsAlwaysOff = 10, ssShiftFreesCaps = 11, ssLanguageName = 12,
    ssCallDefinition = 13, ssCallDefinition_LoadFailed = 14, ssEthnologueCode= 15, ssDebugLine = 16,
    ssMnemonicLayout = 17, ssIncludeCodes = 18, ssOldCharPosMatching = 19,
    ssCompiledVersion = 20, ssKeymanCopyright = 21, ssCustomKeymanEdition = 22, ssCustomKeymanEditionName = 23,
    ssVisualKeyboard = 24, ssKMW_RTL = 25, ssKMW_HelpFile = 26, ssKMW_HelpText = 27, ssKMW_EmbedJS = 28,
    ssWindowsLanguages = 29,
    ssComparison = 30,
    ssPlatform = 31, ssBaseLayout = 32, ssLayer = 33, ssVKDictionary = 34, ssLayoutFile = 35,  // I3438 // I3483
    ssKeyboardVersion = 36, ssKMW_EmbedCSS = 37, ssTargets = 38,
    ssCasedKeys = 39,
    ssBegin_NewContext = 40, ssBegin_PostKeystroke = 41, ssNewLayer = 42, ssOldLayer = 43);   // I4140   // I4368   // I4504

const
  SystemStoreNames: array[TSystemStore] of WideString = (
    '', 'BITMAP', 'COPYRIGHT', 'HOTKEY', 'LANGUAGE', 'LAYOUT', 'MESSAGE',
    'NAME', 'VERSION', 'CAPSONONLY', 'CAPSALWAYSOFF', 'SHIFTFREESCAPS', 'LANGUAGENAME',
    '', '', 'ETHNOLOGUECODE', '',
    'MNEMONICLAYOUT', 'INCLUDECODES', 'OLDCHARPOSMATCHING',
    '', '', '', '',
    'VISUALKEYBOARD', 'KMW_RTL', 'KMW_HELPFILE', 'KMW_HELPTEXT', 'KMW_EMBEDJS',
    'WINDOWSLANGUAGES',
    '', //8.0
    '', '', '', '', 'LAYOUTFILE', 'KEYBOARDVERSION', 'KMW_EMBEDCSS',
    'TARGETS', //9.0  // I3483   // I4140   // I4368   // I4504
    'CASEDKEYS', //14.0,
    '', '', 'NEWLAYER', 'OLDLAYER'); //15.0

implementation

end.
