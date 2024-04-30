
#pragma once
#ifndef KMX_FILE_H
#define KMX_FILE_H

/*
  Copyright:        Copyright (C) 2003-2018 SIL International.
  Authors:          mcdurdin
*/


#define UNREFERENCED_PARAMETER(P)   (P)
#define TRUNCATE ((size_t)-1)
#ifdef KMN_KBP
// TODO: move this to a common namespace keyman::common::kmx_file or similar in the future
namespace km {
namespace kbp {
namespace kmx {
#endif

#define KMX_MAX_ALLOWED_FILE_SIZE (128 * 1024 * 1024)  // 128MB //

#define KEYMAN_LAYOUT_DEFAULT 0x000005FE

#define KEYMANID_NONKEYMAN  0xFFFFFFFF
#define KEYMANID_IGNORE     0xFFFFFFFE
#define KEYMANID_INVALID    0xFFFFFFFD

// Shift flags for hotkeys (version 1.0) //
//_S2 here values for mac
#define SHIFTFLAG 0x2000
#define CTRLFLAG 0x4000
#define ALTFLAG 0x8000

// Miscellaneous flags and defines //

#define MAXGROUPS 128

// File version identifiers //

#define VERSION_30  0x00000300
#define VERSION_31  0x00000301
#define VERSION_32  0x00000302
#define VERSION_40  0x00000400
#define VERSION_50  0x00000500
#define VERSION_501 0x00000501
#define VERSION_60  0x00000600
#define VERSION_70  0x00000700
#define VERSION_80  0x00000800
#define VERSION_90  0x00000900
#define VERSION_100 0x00000A00
#define VERSION_140 0x00000E00
#define VERSION_150 0x00000F00

#define VERSION_160 0x00001000

#define VERSION_MIN VERSION_50
#define VERSION_MAX VERSION_160

//
// Backspace types
//

#define BK_DEFAULT    0
#define BK_DEADKEY    1

// Different begin types
#define BEGIN_ANSI    0
#define BEGIN_UNICODE 1
#define BEGIN_NEWCONTEXT 2
#define BEGIN_POSTKEYSTROKE 3

#define TSS_NONE        0
#define TSS_BITMAP        1
#define TSS_COPYRIGHT     2
#define TSS_HOTKEY        3
#define TSS_LANGUAGE      4
#define TSS_LAYOUT        5
#define TSS_MESSAGE       6
#define TSS_NAME        7
#define TSS_VERSION       8
#define TSS_CAPSONONLY      9
#define TSS_CAPSALWAYSOFF   10
#define TSS_SHIFTFREESCAPS    11
#define TSS_LANGUAGENAME    12

#define TSS_CALLDEFINITION    13
#define TSS_CALLDEFINITION_LOADFAILED 14

#define TSS_ETHNOLOGUECODE    15

#define TSS_DEBUG_LINE      16

#define TSS_MNEMONIC      17

#define TSS_INCLUDECODES    18

#define TSS_OLDCHARPOSMATCHING  19

#define TSS_COMPILEDVERSION   20
#define TSS_KEYMANCOPYRIGHT   21

#define TSS_CUSTOMKEYMANEDITION   22
#define TSS_CUSTOMKEYMANEDITIONNAME 23

// Keyman 7.0 system stores //

#define TSS__KEYMAN_60_MAX    23

#define TSS_VISUALKEYBOARD  24
#define TSS_KMW_RTL     25
#define TSS_KMW_HELPFILE  26
#define TSS_KMW_HELPTEXT  27
#define TSS_KMW_EMBEDJS   28

#define TSS_WINDOWSLANGUAGES 29

#define TSS__KEYMAN_70_MAX  29

// Keyman 8.0 system stores //

#define TSS_COMPARISON 30

#define TSS__KEYMAN_80_MAX  30

// Keyman 9.0 system stores //

#define TSS_PLATFORM    31
#define TSS_BASELAYOUT  32
#define TSS_LAYER       33

#define TSS_PLATFORM_NOMATCH  0x8001  // Reserved for internal use - after platform statement is run, set to either TSS_PLATFORM_NOMATCH or TSS_PLATFORM_MATCH
#define TSS_PLATFORM_MATCH    0x8002  // Reserved for internal use - as the result will never change for the lifetime of the process.

#define TSS_VKDICTIONARY     34      // Dictionary of virtual key names for v9 dynamic layouts
#define TSS_LAYOUTFILE       35      // Keyman 9 layer-based JSON OSK
#define TSS_KEYBOARDVERSION  36      // &keyboardversion system store   // I4140
#define TSS_KMW_EMBEDCSS     37

#define TSS_TARGETS         38

#define TSS__KEYMAN_90_MAX  38

// Keyman 14.0 system stores //

#define TSS_CASEDKEYS       39

#define TSS__KEYMAN_140_MAX  39

// Keyman 15.0 system stores //

#define TSS_BEGIN_NEWCONTEXT    40
#define TSS_BEGIN_POSTKEYSTROKE 41
#define TSS_NEWLAYER     42
#define TSS_OLDLAYER     43

#define TSS__KEYMAN_150_MAX  43

#define TSS__MAX        43

// wm_keyman_control_internal message control codes //

#define KMCI_SELECTKEYBOARD     3   // I3933
#define KMCI_SELECTKEYBOARD_TSF 4   // I3933
#define KMCI_GETACTIVEKEYBOARD  5   // I3933
#define KMCI_SETFOREGROUND      6   // I3933
#define KMCI_SELECTKEYBOARD_BACKGROUND      7   // I4271
#define KMCI_SELECTKEYBOARD_BACKGROUND_TSF  8   // I4271

#define FILEID_COMPILED  0x5354584B

#define SZMAX_LANGUAGENAME 80
#define SZMAX_KEYBOARDNAME 80
#define SZMAX_COPYRIGHT 256
#define SZMAX_MESSAGE 1024

#define UC_SENTINEL       0xFFFF
#define UC_SENTINEL_EXTENDEDEND 0x10 // was ((CODE_LASTCODE)+1)... what was I thinking?

#define U_UC_SENTINEL u"\uFFFF"

/*
 VK__MAX defines the highest virtual key code defined in the system = 0xFF.  Custom VK codes start at 256
*/
#define VK__MAX 255

#define CODE_ANY      0x01
#define CODE_INDEX      0x02
#define CODE_CONTEXT    0x03
#define CODE_NUL      0x04
#define CODE_USE      0x05
#define CODE_RETURN     0x06
#define CODE_BEEP     0x07
#define CODE_DEADKEY    0x08
//  0x09 = bkspace.-- we don't need to keep this separate though with UC_SENTINEL
#define CODE_EXTENDED   0x0A
//#define CODE_EXTENDEDEND  0x0B  deprecated
#define CODE_SWITCH     0x0C
#define CODE_KEY      0x0D
#define CODE_CLEARCONTEXT 0x0E
#define CODE_CALL     0x0F
// UC_SENTINEL_EXTENDEDEND  0x10
#define CODE_CONTEXTEX    0x11

#define CODE_NOTANY   0x12

#define CODE_KEYMAN70_LASTCODE    0x12

#define CODE_SETOPT    0x13
#define CODE_IFOPT     0x14
#define CODE_SAVEOPT   0x15
#define CODE_RESETOPT  0x16

#define CODE_KEYMAN80_LASTCODE  0x16

// Keyman 9.0 codes //

#define CODE_IFSYSTEMSTORE  0x17
#define CODE_SETSYSTEMSTORE 0x18

#define CODE_LASTCODE   0x18

#define U_CODE_ANY u"\u0001"
#define U_CODE_INDEX u"\u0002"
#define U_CODE_CONTEXT u"\u0003"
#define U_CODE_NUL u"\u0004"
#define U_CODE_USE u"\u0005"
#define U_CODE_RETURN u"\u0006"
#define U_CODE_BEEP u"\u0007"
#define U_CODE_DEADKEY u"\u0008"
#define U_CODE_EXTENDED u"\u000A"
#define U_CODE_SWITCH u"\u000C"
#define U_CODE_CLEARCONTEXT u"\u000E"
#define U_CODE_CALL u"\u000F"
#define U_CODE_EXTENDEDEND u"\u0010"
#define U_CODE_CONTEXTEX u"\u0011"
#define U_CODE_NOTANY u"\u0012"
#define U_CODE_SETOPT u"\u0013"
#define U_CODE_IFOPT u"\u0014"
#define U_CODE_SAVEOPT u"\u0015"
#define U_CODE_RESETOPT u"\u0016"
#define U_CODE_IFSYSTEMSTORE u"\u0017"
#define U_CODE_SETSYSTEMSTORE u"\u0018"

#define C_CODE_ANY(store) U_UC_SENTINEL U_CODE_ANY store
#define C_CODE_INDEX(val1, val2) U_UC_SENTINEL U_CODE_INDEX val1 val2
#define C_CODE_CONTEXT() U_UC_SENTINEL U_CODE_CONTEXT
#define C_CODE_NUL() U_UC_SENTINEL U_CODE_NUL
#define C_CODE_USE(val) U_UC_SENTINEL U_CODE_USE val
#define C_CODE_RETURN() U_UC_SENTINEL U_CODE_RETURN
#define C_CODE_BEEP() U_UC_SENTINEL U_CODE_BEEP
#define C_CODE_DEADKEY(deadkey) U_UC_SENTINEL U_CODE_DEADKEY deadkey
#define C_CODE_EXTENDED(varargs) U_UC_SENTINEL U_CODE_EXTENDED varargs
#define C_CODE_SWITCH(val) U_UC_SENTINEL U_CODE_SWITCH val
#define C_CODE_CLEARCONTEXT() U_UC_SENTINEL U_CODE_CLEARCONTEXT
#define C_CODE_CALL(val) U_UC_SENTINEL U_CODE_CALL val
#define C_CODE_CONTEXTEX(val) U_UC_SENTINEL U_CODE_CONTEXTEX val
#define C_CODE_NOTANY(val) U_UC_SENTINEL U_CODE_NOTANY val
#define C_CODE_SETOPT(val1, val2) U_UC_SENTINEL U_CODE_SETOPT val1 val2
#define C_CODE_IFOPT(opt, val1, val2) U_UC_SENTINEL U_CODE_IFOPT opt val1 val2
#define C_CODE_SAVEOPT(opt) U_UC_SENTINEL U_CODE_SAVEOPT opt
#define C_CODE_RESETOPT(opt) U_UC_SENTINEL U_CODE_RESETOPT opt
#define C_CODE_IFSYSTEMSTORE(store, val1, val2) U_UC_SENTINEL U_CODE_IFSYSTEMSTORE store val1 val2
#define C_CODE_SETSYSTEMSTORE(store, val) U_UC_SENTINEL U_CODE_SETSYSTEMSTORE store val

#define KF_SHIFTFREESCAPS 0x0001
#define KF_CAPSONONLY   0x0002
#define KF_CAPSALWAYSOFF  0x0004
#define KF_LOGICALLAYOUT  0x0008
#define KF_AUTOMATICVERSION 0x0010

// 16.0: Support for LDML Keyboards in KMXPlus file format
#define KF_KMXPLUS  0x0020

#define HK_ALT      0x00010000
#define HK_CTRL     0x00020000
#define HK_SHIFT    0x00040000

#define LCTRLFLAG   0x0001    // Left Control flag
#define RCTRLFLAG   0x0002    // Right Control flag
#define LALTFLAG    0x0004    // Left Alt flag
#define RALTFLAG    0x0008    // Right Alt flag
#define K_SHIFTFLAG   0x0010    // Either shift flag
#define K_CTRLFLAG    0x0020    // Either ctrl flag
#define K_ALTFLAG   0x0040    // Either alt flag
//#define K_METAFLAG  0x0080    // Either Meta-key flag (tentative).  Not usable in keyboard rules;
                                // Used internally (currently, only by KMW) to ensure Meta-key
                                // shortcuts safely bypass rules
                                // Meta key = Command key on macOS, Windows key on Windows
#define CAPITALFLAG   0x0100    // Caps lock on
#define NOTCAPITALFLAG  0x0200    // Caps lock NOT on
#define NUMLOCKFLAG   0x0400    // Num lock on
#define NOTNUMLOCKFLAG  0x0800    // Num lock NOT on
#define SCROLLFLAG    0x1000    // Scroll lock on
#define NOTSCROLLFLAG 0x2000    // Scroll lock NOT on
#define ISVIRTUALKEY  0x4000    // It is a Virtual Key Sequence
#define VIRTUALCHARKEY  0x8000    // Keyman 6.0: Virtual Key Cap Sequence NOT YET

#define K_MODIFIERFLAG  0x007F
#define K_NOTMODIFIERFLAG 0xFF00   // I4548

struct KMX_COMP_STORE {
  KMX_DWORD dwSystemID;
  KMX_DWORD dpName;
  KMX_DWORD dpString;
  };

struct KMX_COMP_KEY {
  KMX_WORD Key;
  KMX_WORD _reserved;
  KMX_DWORD Line;
  KMX_DWORD ShiftFlags;
  KMX_DWORD dpOutput;
  KMX_DWORD dpContext;
  };


struct KMX_COMP_GROUP {
  KMX_DWORD dpName;
  KMX_DWORD dpKeyArray;   // [LPKEY] address of first item in key array
  KMX_DWORD dpMatch;
  KMX_DWORD dpNoMatch;
  KMX_DWORD cxKeyArray;   // in array entries
  KMX_BOOL  fUsingKeys;   // group(xx) [using keys] <-- specified or not
  };

struct KMX_COMP_KEYBOARD {
  KMX_DWORD dwIdentifier;   // 0000 Keyman compiled keyboard id

  KMX_DWORD dwFileVersion;  // 0004 Version of the file - Keyman 4.0 is 0x0400

  KMX_DWORD dwCheckSum;     // 0008 As stored in keyboard. DEPRECATED as of 16.0
  KMX_DWORD KeyboardID;     // 000C as stored in HKEY_LOCAL_MACHINE//system//currentcontrolset//control//keyboard layouts
  KMX_DWORD IsRegistered;   // 0010
  KMX_DWORD version;        // 0014 keyboard version

  KMX_DWORD cxStoreArray;   // 0018 in array entries
  KMX_DWORD cxGroupArray;   // 001C in array entries

  KMX_DWORD dpStoreArray;   // 0020 [LPSTORE] address of first item in store array
  KMX_DWORD dpGroupArray;   // 0024 [LPGROUP] address of first item in group array

  KMX_DWORD StartGroup[2];  // 0028 index of starting groups [2 of them]

  KMX_DWORD dwFlags;        // 0030 Flags for the keyboard file

  KMX_DWORD dwHotKey;       // 0034 standard windows hotkey (hiword=shift/ctrl/alt stuff, loword=vkey)

  KMX_DWORD dpBitmapOffset; // 0038 offset of the bitmaps in the file
  KMX_DWORD dwBitmapSize;   // 003C size in bytes of the bitmaps
};

struct KMX_COMP_KEYBOARD_KMXPLUSINFO {
  KMX_DWORD dpKMXPlus;      // 0040 offset of KMXPlus data, <sect> header is first
  KMX_DWORD dwKMXPlusSize;  // 0044 size in bytes of entire KMXPlus data
};

//
 //* Only valid if comp_keyboard.dwFlags&KF_KMXPLUS
 //
struct KMX_COMP_KEYBOARD_EX {
  KMX_COMP_KEYBOARD             header;    // 0000 see COMP_KEYBOARD
  KMX_COMP_KEYBOARD_KMXPLUSINFO kmxplus;   // 0040 see COMP_KEYBOARD_EXTRA
};

typedef KMX_COMP_KEYBOARD *PKMX_COMP_KEYBOARD;
typedef KMX_COMP_STORE *PKMX_COMP_STORE;
typedef KMX_COMP_KEY *PKMX_COMP_KEY;
typedef KMX_COMP_GROUP *PKMX_COMP_GROUP;


extern const int CODE__SIZE[];
#define CODE__SIZE_MAX 5

#define KEYBOARDFILEHEADER_SIZE 64
#define KEYBOARDFILESTORE_SIZE  12
#define KEYBOARDFILEGROUP_SIZE  24
#define KEYBOARDFILEKEY_SIZE    20

static_assert(sizeof(KMX_COMP_STORE) == KEYBOARDFILESTORE_SIZE, "COMP_STORE must be KEYBOARDFILESTORE_SIZE bytes");
static_assert(sizeof(KMX_COMP_KEY) == KEYBOARDFILEKEY_SIZE, "COMP_KEY must be KEYBOARDFILEKEY_SIZE bytes");
static_assert(sizeof(KMX_COMP_GROUP) == KEYBOARDFILEGROUP_SIZE, "COMP_GROUP must be KEYBOARDFILEGROUP_SIZE bytes");
static_assert(sizeof(KMX_COMP_KEYBOARD) == KEYBOARDFILEHEADER_SIZE, "COMP_KEYBOARD must be KEYBOARDFILEHEADER_SIZE bytes");

#ifdef KMN_KBP
} // namespace kmx
} // namespace kbp
} // namespace km
#endif
#endif //KMX_FILE_H

//################################################################################################################################################
//################################# Code beyond these lines needs to be included in mcompile #####################################################
//################################################################################################################################################
