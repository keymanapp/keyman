//
//  KMBinaryFileFormat.h
//  KeymanEngine4Mac
//
//  Created by Serkan Kurt on 19/11/2014.
//  Copyright (c) 2017 SIL International. All rights reserved.
//

#ifndef KMBinaryFileFormat_h
#define KMBinaryFileFormat_h

typedef uint16_t WORD;
typedef uint32_t DWORD;
//typedef wchar_t* PWSTR;

struct COMP_KEYBOARD {
    DWORD dwIdentifier;     // 0000 Keyman compiled keyboard id
    DWORD dwFileVersion;    // 0004 Version of the file - Keyman 4.0 is 0x0400
    DWORD dwCheckSum;       // 0008 As stored in keyboard
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
#define VERSION_MIN VERSION_50
#define VERSION_MAX VERSION_100

struct COMP_GROUP {
    DWORD dpName;           // string (only debug)
    DWORD dpKeyArray;       // [LPCOMP_KEY] address of first item in key array
    DWORD dpMatch;          // extended string
    DWORD dpNoMatch;        // extended string
    DWORD cxKeyArray;       // in array entries
    BOOL  fUsingKeys;       // group(xx) [using keys] <-- specified or not
};

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
#define TSS__MAX                        38

// ShiftFlags
#define LCTRLFLAG       0x0001      // Left Control flag
#define RCTRLFLAG       0x0002      // Right Control flag
#define LALTFLAG        0x0004      // Left Alt flag
#define RALTFLAG        0x0008      // Right Alt flag
#define K_SHIFTFLAG     0x0010      // Either shift flag
#define K_CTRLFLAG      0x0020      // Either ctrl flag
#define K_ALTFLAG       0x0040      // Either alt flag
#define CAPITALFLAG     0x0100      // Caps lock on
#define NOTCAPITALFLAG  0x0200      // Caps lock NOT on
#define NUMLOCKFLAG     0x0400      // Num lock on
#define NOTNUMLOCKFLAG  0x0800      // Num lock NOT on
#define SCROLLFLAG      0x1000      // Scroll lock on
#define NOTSCROLLFLAG   0x2000      // Scroll lock NOT on
#define ISVIRTUALKEY    0x4000      // It is a Virtual Key Sequence
#define VIRTUALCHARKEY  0x8000      // Keyman 6.0: Virtual Key Cap Sequence
#define K_MODIFIERFLAG  0x007F

struct COMP_KEY {
    WORD Key;  // Windows VK code or character value (VIRTUALCHARKEY, ISVIRTUALKEY)
    DWORD Line;
    DWORD ShiftFlags;
    DWORD dpOutput;   // extended string
    DWORD dpContext;  // extended string
};

#define UC_SENTINEL             0xFFFF
#define UC_SENTINEL_EXTENDEDEND 0x10

#define CODE_ANY                0x01
#define CODE_INDEX              0x02
#define CODE_CONTEXT            0x03
#define CODE_NUL                0x04
#define CODE_USE                0x05
#define CODE_RETURN             0x06
#define CODE_BEEP               0x07
#define CODE_DEADKEY            0x08
//  0x09 = bkspace.-- we don't need to keep this separate though with UC_SENTINEL
#define CODE_EXTENDED           0x0A
//#define CODE_EXTENDEDEND  0x0B  deprecated
#define CODE_SWITCH             0x0C
#define CODE_KEY                0x0D
#define CODE_CLEARCONTEXT       0x0E
#define CODE_CALL               0x0F
// UC_SENTINEL_EXTENDEDEND  0x10
#define CODE_CONTEXTEX          0x11
#define CODE_NOTANY             0x12
#define CODE_KEYMAN70_LASTCODE  0x12

#define CODE_SETOPT             0x13
#define CODE_IFOPT              0x14
#define CODE_SAVEOPT            0x15
#define CODE_RESETOPT           0x16
#define CODE_KEYMAN80_LASTCODE  0x16

/* Keyman 9.0 codes */
#define CODE_IFSYSTEMSTORE      0x17
#define CODE_SETSYSTEMSTORE     0x18
#define CODE_LASTCODE           0x18

/* Keyman4Mac codes */
#define CODE_NULLCHAR           0x20


#define NOT_EQUAL   0
#define EQUAL       1

/* KVK file format definitions */

#define KVKK_BITMAP             0x01
#define KVKK_UNICODE            0x02

#define KVKS_NORMAL             0x00
#define KVKS_SHIFT              0x01
#define KVKS_CTRL               0x02
#define KVKS_ALT                0x04
#define KVKS_LCTRL              0x08
#define KVKS_RCTRL              0x10
#define KVKS_LALT               0x20
#define KVKS_RALT               0x40
//#define KVKS_META               0x80    // Either Meta-key flag (tentative).  Not usable in keyboard rules;
                                          // Used internally to ensure Meta-key shortcuts safely bypass rules


/*
PWSTR incxstr(PWSTR p) {
    if(*p == 0) return p;
    if(*p != UC_SENTINEL)
    {
        if(*p >= 0xD800 && *p <= 0xDBFF && *(p+1) >= 0xDC00 && *(p+1) <= 0xDFFF) return p+2;
        return p+1;
    }
    p+=2;
    switch(*(p-1))
    {
        case CODE_ANY:      return p+1;
        case CODE_NOTANY:   return p+1;
        case CODE_INDEX:        return p+2;
        case CODE_USE:          return p+1;
        case CODE_DEADKEY:      return p+1;
        case CODE_EXTENDED:     p += 2; while(*p != UC_SENTINEL_EXTENDEDEND) p++; return p+1;
        case CODE_CLEARCONTEXT: return p+1;
        case CODE_CALL:         return p+1;
        case CODE_CONTEXTEX:    return p+1;
        case CODE_IFOPT:    return p+3;
        case CODE_IFSYSTEMSTORE: return p+3;
        case CODE_SETOPT:   return p+2;
        case CODE_SETSYSTEMSTORE: return p+2;
        case CODE_RESETOPT: return p+1;
        case CODE_SAVEOPT:  return p+1;
        default:                return p;
    }
}*/

#endif /* KMBinaryFileFormat_h */
