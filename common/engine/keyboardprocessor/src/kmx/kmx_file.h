/*
  Copyright:        Copyright (C) 2003-2018 SIL International.
  Authors:          mcdurdin
*/

#pragma once

#include "kmx_base.h"

namespace km {
namespace kbp {
namespace kmx {

/* */

#define KEYMAN_LAYOUT_DEFAULT 0x000005FE

#define KEYMANID_NONKEYMAN  0xFFFFFFFF
#define KEYMANID_IGNORE     0xFFFFFFFE
#define KEYMANID_INVALID    0xFFFFFFFD

/* Shift flags for hotkeys (version 1.0) */ 

#define SHIFTFLAG 0x2000
#define CTRLFLAG 0x4000
#define ALTFLAG 0x8000

/* Miscellaneous flags and defines */ 

#define MAXGROUPS 128

/* File version identifiers */ 

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

#define VERSION_MIN VERSION_50
#define VERSION_MAX VERSION_100

//
// Backspace types
//

#define BK_DEFAULT    0
#define BK_DEADKEY    1
#define BK_BACKSPACE  2

// Different begin types
#define BEGIN_ANSI    0
#define BEGIN_UNICODE 1

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

/* Keyman 7.0 system stores */

#define TSS__KEYMAN_60_MAX    23

#define TSS_VISUALKEYBOARD  24
#define TSS_KMW_RTL     25
#define TSS_KMW_HELPFILE  26
#define TSS_KMW_HELPTEXT  27
#define TSS_KMW_EMBEDJS   28

#define TSS_WINDOWSLANGUAGES 29

#define TSS__KEYMAN_70_MAX  29

/* Keyman 8.0 system stores */

#define TSS_COMPARISON 30

/* Keyman 9.0 system stores */

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

#define TSS__MAX        38

/* wm_keyman_control_internal message control codes */ 

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

/*
 * VK__MAX defines the highest virtual key code defined in the system = 0xFF.  Custom VK codes start at 256
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

/* Keyman 9.0 codes */

#define CODE_IFSYSTEMSTORE  0x17
#define CODE_SETSYSTEMSTORE 0x18

#define CODE_LASTCODE   0x18

#define KF_SHIFTFREESCAPS 0x0001
#define KF_CAPSONONLY   0x0002
#define KF_CAPSALWAYSOFF  0x0004
#define KF_LOGICALLAYOUT  0x0008
#define KF_AUTOMATICVERSION 0x0010

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

struct COMP_STORE {
  KMX_DWORD dwSystemID;
  KMX_DWORD dpName; 
  KMX_DWORD dpString;
  };

struct COMP_KEY {
  KMX_WORD Key;
  KMX_DWORD Line;
  KMX_DWORD ShiftFlags;
  KMX_DWORD dpOutput;
  KMX_DWORD dpContext;
  };

struct COMP_GROUP {
  KMX_DWORD dpName;
  KMX_DWORD dpKeyArray;   // [LPKEY] address of first item in key array
  KMX_DWORD dpMatch;      
  KMX_DWORD dpNoMatch;    
  KMX_DWORD cxKeyArray;   // in array entries
  KMX_BOOL  fUsingKeys;   // group(xx) [using keys] <-- specified or not
  };

struct COMP_KEYBOARD {
  KMX_DWORD dwIdentifier;   // 0000 Keyman compiled keyboard id
  
  KMX_DWORD dwFileVersion;  // 0004 Version of the file - Keyman 4.0 is 0x0400
  
  KMX_DWORD dwCheckSum;     // 0008 As stored in keyboard
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

typedef COMP_KEYBOARD *PCOMP_KEYBOARD;
typedef COMP_STORE *PCOMP_STORE;
typedef COMP_KEY *PCOMP_KEY;
typedef COMP_GROUP *PCOMP_GROUP;

} // namespace kmx
} // namespace kbp
} // namespace km
