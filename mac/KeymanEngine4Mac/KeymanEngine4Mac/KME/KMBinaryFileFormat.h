//
//  KMBinaryFileFormat.h
//  KeymanEngine4Mac
//
//  Created by Serkan Kurt on 19/11/2014.
//  Copyright (c) 2017 SIL International. All rights reserved.
//

/**
 * Note that many of the constants defined here are also defined in the common header`kmx_file.h`
 * Keyman for Mac only reads the kmx file to pass the contents to Keyman Core and for meta data.
 * If the meta data can instead be returned by Keyman Core in the future, then the duplicate definitions
 * found here can be removed.
 */

#ifndef KMBinaryFileFormat_h
#define KMBinaryFileFormat_h

typedef uint16_t WORD;
typedef uint32_t DWORD;

struct COMP_KEYBOARD {
  DWORD dwIdentifier;     // 0000 Keyman compiled keyboard id
  DWORD dwFileVersion;    // 0004 Version of the file - Keyman 4.0 is 0x0400
  DWORD dwCheckSum;       // 0008 As stored in keyboard. DEPRECATED as of 16.0
  DWORD KeyboardID;       // 000C as stored in HKEY_LOCAL_MACHINE//system//currentcontrolset//control//keyboard layouts
  DWORD IsRegistered;     // 0010
  DWORD version;          // 0014 keyboard version
  DWORD cxStoreArray;     // 0018 in array entries
  DWORD cxGroupArray;     // 001C in array entries
  DWORD dpStoreArray;     // 0020 [LPCOMP_STORE] address of first item in store array
  DWORD dpGroupArray;     // 0024 [LPCOMP_GROUP] address of first item in group array
  DWORD StartGroup[2];    // 0028 index of starting groups [2 of them]
  DWORD dwFlags;          // 0030 Flags for the keyboard file
  DWORD dwHotKey;         // 0034 standard windows hotkey (hiword=shift/ctrl/alt stuff, loword=vkey)
  DWORD dpBitmapOffset;   // 0038 offset of the bitmaps in the file
  DWORD dwBitmapSize;     // 003C size in bytes of the bitmaps
};

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
#define VERSION_170 0x00001100
#define VERSION_190 0x00001300
#define VERSION_MIN VERSION_50
#define VERSION_MAX VERSION_190

struct COMP_STORE {
  DWORD dwSystemID;
  DWORD dpName;           // string (only debug)
  DWORD dpString;         // extended string
};

// dwSystemID definitions
#define TSS_NONE                        0
#define TSS_BITMAP                      1
#define TSS_COPYRIGHT                   2
#define TSS_HOTKEY                      3
#define TSS_LANGUAGE                    4
#define TSS_LAYOUT                      5
#define TSS_MESSAGE                     6
#define TSS_NAME                        7
#define TSS_VERSION                     8
#define TSS_CAPSONONLY                  9
#define TSS_CAPSALWAYSOFF               10
#define TSS_SHIFTFREESCAPS              11
#define TSS_LANGUAGENAME                12
#define TSS_CALLDEFINITION              13
#define TSS_CALLDEFINITION_LOADFAILED   14
#define TSS_ETHNOLOGUECODE              15
#define TSS_DEBUG_LINE                  16
#define TSS_MNEMONIC                    17
#define TSS_INCLUDECODES                18
#define TSS_OLDCHARPOSMATCHING          19
#define TSS_COMPILEDVERSION             20
#define TSS_KEYMANCOPYRIGHT             21
#define TSS_CUSTOMKEYMANEDITION         22
#define TSS_CUSTOMKEYMANEDITIONNAME     23
/* Keyman 7.0 system stores */
#define TSS__KEYMAN_60_MAX              23
#define TSS_VISUALKEYBOARD              24
#define TSS_KMW_RTL                     25
#define TSS_KMW_HELPFILE                26
#define TSS_KMW_HELPTEXT                27
#define TSS_KMW_EMBEDJS                 28
#define TSS_WINDOWSLANGUAGES            29
#define TSS__KEYMAN_70_MAX              29
/* Keyman 8.0 system stores */
#define TSS_COMPARISON                  30
#define TSS__KEYMAN_80_MAX              30
/* Keyman 9.0 system stores */
#define TSS_PLATFORM                    31
#define TSS_BASELAYOUT                  32
#define TSS_LAYER                       33
#define TSS_PLATFORM_NOMATCH            0x8001  // Reserved for internal use - after platform statement is run, set to either TSS_PLATFORM_NOMATCH or TSS_PLATFORM_MATCH
#define TSS_PLATFORM_MATCH              0x8002  // Reserved for internal use - as the result will never change for the lifetime of the process.
#define TSS_VKDICTIONARY                34      // Dictionary of virtual key names for v9 dynamic layouts
#define TSS_LAYOUTFILE                  35      // Keyman 9 layer-based JSON OSK
#define TSS_KEYBOARDVERSION             36      // &keyboardversion system store   // I4140
#define TSS_KMW_EMBEDCSS                37
#define TSS_TARGETS                     38
#define TSS__KEYMAN_90_MAX              38
/* Keyman 14.0 system stores */
#define TSS_CASEDKEYS                   39
#define TSS__KEYMAN_140_MAX             39
/* Keyman 15.0 system stores */
#define TSS_BEGIN_NEWCONTEXT            40
#define TSS_BEGIN_POSTKEYSTROKE         41
#define TSS_NEWLAYER                    42
#define TSS_OLDLAYER                    43
#define TSS__KEYMAN_150_MAX             43
/* Keyman 17.0 system stores */
#define TSS_DISPLAYMAP                  44

#define TSS__MAX                        44

/* KVK file format definitions */

// KVK header flags
#define KVKH_102                0x01
#define KVKH_DISPLAY_UNDERLYING 0x02
#define KVKH_USE_UNDERLYING     0x04
#define KVKH_ALTGR              0x08

// KVK key type flags
#define KVKK_BITMAP             0x01
#define KVKK_UNICODE            0x02

// KVK modifier flags
#define KVKS_NORMAL             0x00
#define KVKS_SHIFT              0x01
#define KVKS_CTRL               0x02
#define KVKS_ALT                0x04
#define KVKS_LCTRL              0x08
#define KVKS_RCTRL              0x10
#define KVKS_LALT               0x20
#define KVKS_RALT               0x40

#endif /* KMBinaryFileFormat_h */
