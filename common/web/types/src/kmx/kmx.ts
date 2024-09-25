import * as r from 'restructure';
import { ModifierKeyConstants } from '../consts/modifier-key-constants.js';

/* Definitions from kmx_file.h. Must be kept in sync */

// TODO: split kmx-file from kmx in-memory, similar to what I've done for kvk (keep restructure decl + BUILDER_ interfaces together)

// In memory representations of KMX structures
// kmx-builder will transform these to the corresponding COMP_xxxx

export enum KMX_Version {
  VERSION_30 =  0x00000300,
  VERSION_31 =  0x00000301,
  VERSION_32 =  0x00000302,
  VERSION_40 =  0x00000400,
  VERSION_50 =  0x00000500,
  VERSION_501 = 0x00000501,
  VERSION_60 =  0x00000600,
  VERSION_70 =  0x00000700,
  VERSION_80 =  0x00000800,
  VERSION_90 =  0x00000900,
  VERSION_100 = 0x00000A00,
  VERSION_140 = 0x00000E00,
  VERSION_150 = 0x00000F00,
  VERSION_160 = 0x00001000,
  VERSION_170 = 0x00001100
};


export class KEYBOARD {
  fileVersion?: number;  // dwFileVersion (TSS_FILEVERSION)

  startGroup: {
    ansi: number;           // from COMP_KEYBOARD
    unicode: number;        // from COMP_KEYBOARD
    newContext: number;     // from TSS_BEGIN_NEWCONTEXT store
    postKeystroke: number;  // from TSS_BEGIN_POSTKEYSTROKE store
  } = {ansi:-1, unicode:-1, newContext:-1, postKeystroke:-1};

  flags?: number;
  hotkey?: number;

  //bitmap:
  groups: GROUP[] = [];
  stores: STORE[] = [];

  // Following values are extracted from stores[] but are
  // informative only

  keyboardVersion?: string;   // version (TSS_KEYBOARDVERSION)
  isMnemonic: boolean;        // TSS_MNEMONICLAYOUT store
  targets: string;            // TSS_TARGETS store ('desktop' if missing)
};

export class STORE {
  dwSystemID: number;
  dpName: string;
  dpString: string;
};

export class GROUP {
  dpName: string;
  keys: KEY[] = [];
  dpMatch: string;
  dpNoMatch: string;
  fUsingKeys: boolean;
};

export class KEY {
  Key: number;
  Line: number;
  ShiftFlags: number;
  dpOutput: string;
  dpContext: string;
};

// These type-checking structures are here to ensure that
// we match the structures from kmx.ts in the generator
//
// They are used internally when reading and writing .kmx files

export interface BUILDER_COMP_KEYBOARD {
  dwIdentifier: number;
  dwFileVersion: number;
  dwCheckSum: number;
  KeyboardID: number;
  IsRegistered: number;
  version: number;

  cxStoreArray: number;
  cxGroupArray: number;

  dpStoreArray: number;
  dpGroupArray: number;

  StartGroup_ANSI: number;
  StartGroup_Unicode: number;

  dwFlags: number;

  dwHotKey: number;

  dpBitmapOffset: number;
  dwBitmapSize: number;
};

export interface BUILDER_COMP_KEYBOARD_KMXPLUSINFO {
  dpKMXPlus: number;
  dwKMXPlusSize: number;
};

export interface BUILDER_COMP_STORE {
  dwSystemID: number;
  dpName: number;
  dpString: number;
};

export interface BUILDER_COMP_KEY {
  Key: number;
  _padding: number;
  Line: number;
  ShiftFlags: number;
  dpOutput: number;
  dpContext: number;
};

export interface BUILDER_COMP_GROUP {
  dpName: number;
  dpKeyArray: number;
  dpMatch: number;
  dpNoMatch: number;
  cxKeyArray: number;
  fUsingKeys: number;
};

export class KMXFile {

  /* KMX file structures */

  public readonly COMP_STORE: any;
  public readonly COMP_KEY: any;
  public readonly COMP_GROUP: any;
  public readonly COMP_KEYBOARD_KMXPLUSINFO: any;
  public readonly COMP_KEYBOARD: any;

  public static readonly FILEID_COMPILED	= 0x5354584B; // 'KXTS'

  //
  // File version identifiers (COMP_KEYBOARD.dwFileVersion)
  //

  public static readonly VERSION_30 =  KMX_Version.VERSION_30;
  public static readonly VERSION_31 =  KMX_Version.VERSION_31;
  public static readonly VERSION_32 =  KMX_Version.VERSION_32;
  public static readonly VERSION_40 =  KMX_Version.VERSION_40;
  public static readonly VERSION_50 =  KMX_Version.VERSION_50;
  public static readonly VERSION_501 = KMX_Version.VERSION_501;
  public static readonly VERSION_60 =  KMX_Version.VERSION_60;
  public static readonly VERSION_70 =  KMX_Version.VERSION_70;
  public static readonly VERSION_80 =  KMX_Version.VERSION_80;
  public static readonly VERSION_90 =  KMX_Version.VERSION_90;
  public static readonly VERSION_100 = KMX_Version.VERSION_100;
  public static readonly VERSION_140 = KMX_Version.VERSION_140;
  public static readonly VERSION_150 = KMX_Version.VERSION_150;
  public static readonly VERSION_160 = KMX_Version.VERSION_160;
  public static readonly VERSION_170 = KMX_Version.VERSION_170;

  public static readonly VERSION_MIN = this.VERSION_50;
  public static readonly VERSION_MAX = this.VERSION_170;

  //
  // Backspace types
  //

  public static readonly BK_DEFAULT =    0;
  public static readonly BK_DEADKEY =    1;

  // Different begin types (COMP_STORE.StartGroup_*)

  public static readonly BEGIN_ANSI =    0;
  public static readonly BEGIN_UNICODE = 1;

  //
  // System Store values (COMP_STORE.dwSystemID)
  //

  public static readonly TSS_NONE =                0;
  public static readonly TSS_BITMAP =              1;
  public static readonly TSS_COPYRIGHT =           2;
  public static readonly TSS_HOTKEY =              3;
  public static readonly TSS_LANGUAGE =            4;
  public static readonly TSS_LAYOUT =              5;
  public static readonly TSS_MESSAGE =             6;
  public static readonly TSS_NAME =                7;
  public static readonly TSS_VERSION =             8;
  public static readonly TSS_CAPSONONLY =          9;
  public static readonly TSS_CAPSALWAYSOFF =       10;
  public static readonly TSS_SHIFTFREESCAPS =      11;
  public static readonly TSS_LANGUAGENAME =        12;

  public static readonly TSS_CALLDEFINITION =      13;
  public static readonly TSS_CALLDEFINITION_LOADFAILED = 14;

  public static readonly TSS_ETHNOLOGUECODE =      15;

  public static readonly TSS_DEBUG_LINE =          16;

  public static readonly TSS_MNEMONIC =            17;

  public static readonly TSS_INCLUDECODES =        18;

  public static readonly TSS_OLDCHARPOSMATCHING =  19;

  public static readonly TSS_COMPILEDVERSION =     20;
  public static readonly TSS_KEYMANCOPYRIGHT =     21;

  public static readonly TSS_CUSTOMKEYMANEDITION =     22;
  public static readonly TSS_CUSTOMKEYMANEDITIONNAME = 23;

  /* Keyman 7.0 system stores */

  public static readonly TSS__KEYMAN_60_MAX =   23;

  public static readonly TSS_VISUALKEYBOARD =   24;
  public static readonly TSS_KMW_RTL =          25;
  public static readonly TSS_KMW_HELPFILE =     26;
  public static readonly TSS_KMW_HELPTEXT =     27;
  public static readonly TSS_KMW_EMBEDJS =      28;

  public static readonly TSS_WINDOWSLANGUAGES = 29;

  public static readonly TSS__KEYMAN_70_MAX =   29;

  /* Keyman 8.0 system stores */

  public static readonly TSS_COMPARISON =       30;

  public static readonly TSS__KEYMAN_80_MAX =   30;

  /* Keyman 9.0 system stores */

  public static readonly TSS_PLATFORM =    31;
  public static readonly TSS_BASELAYOUT =  32;
  public static readonly TSS_LAYER =       33;

  public static readonly TSS_PLATFORM_NOMATCH =  0x8001;  // Reserved for internal use - after platform statement is run, set to either TSS_PLATFORM_NOMATCH or TSS_PLATFORM_MATCH
  public static readonly TSS_PLATFORM_MATCH =    0x8002;  // Reserved for internal use - as the result will never change for the lifetime of the process.

  public static readonly TSS_VKDICTIONARY =     34;      // Dictionary of virtual key names for v9 dynamic layouts
  public static readonly TSS_LAYOUTFILE =       35;      // Keyman 9 layer-based JSON OSK
  public static readonly TSS_KEYBOARDVERSION =  36;      // &keyboardversion system store   // I4140
  public static readonly TSS_KMW_EMBEDCSS =     37;

  public static readonly TSS_TARGETS =          38;

  public static readonly TSS__KEYMAN_90_MAX =   38;

  /* Keyman 14.0 system stores */

  public static readonly TSS_CASEDKEYS =        39;

  public static readonly TSS__KEYMAN_140_MAX =  39;

  /* Keyman 15.0 system stores */

  public static readonly TSS_BEGIN_NEWCONTEXT =    40;
  public static readonly TSS_BEGIN_POSTKEYSTROKE = 41;
  public static readonly TSS_NEWLAYER =            42;
  public static readonly TSS_OLDLAYER =            43;

  public static readonly TSS__KEYMAN_150_MAX =     43;

  /* Keyman 17.0 system stores */

  public static readonly TSS_DISPLAYMAP =          44;

  public static readonly TSS__KEYMAN_170_MAX =     44;

  public static readonly TSS__MAX =                44;


  public static readonly UC_SENTINEL =       0xFFFF;
  public static readonly UC_SENTINEL_EXTENDEDEND = 0x10;

  public static readonly U_UC_SENTINEL = "\uFFFF";

  //
  // VK__MAX defines the highest virtual key code defined in the system = 0xFF.  Custom VK codes start at 256
  //

  public static readonly VK__MAX = 255;

  //
  // Extended String CODE_ values
  //

  public static readonly CODE_ANY =          0x01;
  public static readonly CODE_INDEX =        0x02;
  public static readonly CODE_CONTEXT =      0x03;
  public static readonly CODE_NUL =          0x04;
  public static readonly CODE_USE =          0x05;
  public static readonly CODE_RETURN =       0x06;
  public static readonly CODE_BEEP =         0x07;
  public static readonly CODE_DEADKEY =      0x08;
  //  0x09 = bkspace.-- we don't need to keep this separate though with UC_SENTINEL
  public static readonly CODE_EXTENDED =     0x0A;
  //public static readonly CODE_EXTENDEDEND =  0x0B;  deprecated
  public static readonly CODE_SWITCH =       0x0C;
  public static readonly CODE_KEY =          0x0D;
  public static readonly CODE_CLEARCONTEXT = 0x0E;
  public static readonly CODE_CALL =         0x0F;
  // UC_SENTINEL_EXTENDEDEND  0x10
  public static readonly CODE_CONTEXTEX =    0x11;

  public static readonly CODE_NOTANY =   0x12;

  public static readonly CODE_KEYMAN70_LASTCODE =    0x12;

  public static readonly CODE_SETOPT =    0x13;
  public static readonly CODE_IFOPT =     0x14;
  public static readonly CODE_SAVEOPT =   0x15;
  public static readonly CODE_RESETOPT =  0x16;

  public static readonly CODE_KEYMAN80_LASTCODE =  0x16;

  /* Keyman 9.0 codes */

  public static readonly CODE_IFSYSTEMSTORE =  0x17;
  public static readonly CODE_SETSYSTEMSTORE = 0x18;

  public static readonly CODE_LASTCODE =   0x18;


  public static readonly KF_SHIFTFREESCAPS =   0x0001;
  public static readonly KF_CAPSONONLY =       0x0002;
  public static readonly KF_CAPSALWAYSOFF =    0x0004;
  public static readonly KF_LOGICALLAYOUT =    0x0008;
  public static readonly KF_AUTOMATICVERSION = 0x0010;

  // 16.0: Support for LDML Keyboards in KMXPlus file format
  public static readonly KF_KMXPLUS =  0x0020;

  public static readonly HK_ALT =      0x00010000;
  public static readonly HK_CTRL =     0x00020000;
  public static readonly HK_SHIFT =    0x00040000;

  public static readonly LCTRLFLAG      = ModifierKeyConstants.LCTRLFLAG;      // Left Control flag
  public static readonly RCTRLFLAG      = ModifierKeyConstants.RCTRLFLAG;      // Right Control flag
  public static readonly LALTFLAG       = ModifierKeyConstants.LALTFLAG;       // Left Alt flag
  public static readonly RALTFLAG       = ModifierKeyConstants.RALTFLAG;       // Right Alt flag
  public static readonly K_SHIFTFLAG    = ModifierKeyConstants.K_SHIFTFLAG;    // Either shift flag
  public static readonly K_CTRLFLAG     = ModifierKeyConstants.K_CTRLFLAG;     // Either ctrl flag
  public static readonly K_ALTFLAG      = ModifierKeyConstants.K_ALTFLAG;      // Either alt flag
  //public static readonly K_METAFLAG =  0x0080;    // Either Meta-key flag (tentative).  Not usable in keyboard rules;
                                  // Used internally (currently, only by KMW) to ensure Meta-key
                                  // shortcuts safely bypass rules
                                  // Meta key = Command key on macOS, Windows key on Windows
  public static readonly CAPITALFLAG    = ModifierKeyConstants.CAPITALFLAG;    // Caps lock on
  public static readonly NOTCAPITALFLAG = ModifierKeyConstants.NOTCAPITALFLAG; // Caps lock NOT on
  public static readonly NUMLOCKFLAG    = ModifierKeyConstants.NUMLOCKFLAG;    // Num lock on
  public static readonly NOTNUMLOCKFLAG = ModifierKeyConstants.NOTNUMLOCKFLAG; // Num lock NOT on
  public static readonly SCROLLFLAG     = ModifierKeyConstants.SCROLLFLAG;     // Scroll lock on
  public static readonly NOTSCROLLFLAG  = ModifierKeyConstants.NOTSCROLLFLAG;  // Scroll lock NOT on
  public static readonly ISVIRTUALKEY   = ModifierKeyConstants.ISVIRTUALKEY;   // It is a Virtual Key Sequence
  public static readonly VIRTUALCHARKEY = ModifierKeyConstants.VIRTUALCHARKEY; // Keyman 6.0: Virtual Key Cap Sequence NOT YET

  // Note: OTHER_MODIFIER = 0x10000, used by KMX+ for the
  // other modifier flag in layers, > 16 bit so not available here.
  // See keys_mod_other in keyman_core_ldml.ts

  public static readonly MASK_MODIFIER_CHIRAL = KMXFile.LCTRLFLAG | KMXFile.RCTRLFLAG | KMXFile.LALTFLAG | KMXFile.RALTFLAG;
  public static readonly MASK_MODIFIER_SHIFT = KMXFile.K_SHIFTFLAG;
  public static readonly MASK_MODIFIER_NONCHIRAL = KMXFile.K_CTRLFLAG | KMXFile.K_ALTFLAG;

  public static readonly MASK_STATEKEY = KMXFile.CAPITALFLAG | KMXFile.NOTCAPITALFLAG |
                                         KMXFile.NUMLOCKFLAG | KMXFile.NOTNUMLOCKFLAG |
                                         KMXFile.SCROLLFLAG | KMXFile.NOTSCROLLFLAG;
  public static readonly MASK_KEYTYPE  = KMXFile.ISVIRTUALKEY | KMXFile.VIRTUALCHARKEY;

  public static readonly MASK_MODIFIER = KMXFile.MASK_MODIFIER_CHIRAL | KMXFile.MASK_MODIFIER_SHIFT | KMXFile.MASK_MODIFIER_NONCHIRAL;

  public static readonly MASK_KEYS = KMXFile.MASK_MODIFIER | KMXFile.MASK_STATEKEY;
  public static readonly KMX_MASK_VALID    = KMXFile.MASK_KEYS | KMXFile.MASK_KEYTYPE;


  public static readonly K_MODIFIERFLAG    = 0x007F;
  public static readonly K_NOTMODIFIERFLAG = 0xFF00;   // I4548

  public static readonly COMP_KEYBOARD_SIZE = 64;
  public static readonly COMP_KEYBOARD_KMXPLUSINFO_SIZE = 8;
  public static readonly COMP_STORE_SIZE  = 12;
  public static readonly COMP_GROUP_SIZE  = 24;
  public static readonly COMP_KEY_SIZE    = 20;


  public static readonly VERSION_MASK_MINOR = 0x00FF;
  public static readonly VERSION_MASK_MAJOR = 0xFF00;

  /* In-memory representation of the keyboard */

  public keyboard: KEYBOARD = new KEYBOARD();

  constructor() {

    // Binary-correct structures matching kmx_file.h

    this.COMP_STORE = new r.Struct({
      dwSystemID: r.uint32le,
      dpName: r.uint32le,
      dpString: r.uint32le
    });

    if(this.COMP_STORE.size() != KMXFile.COMP_STORE_SIZE) {
      throw "COMP_STORE size is "+this.COMP_STORE.size()+" but should be "+KMXFile.COMP_STORE_SIZE+" bytes";
    }

    this.COMP_KEY = new r.Struct({
      Key: r.uint16le,
      _padding: new r.Reserved(r.uint16le), // padding
      Line: r.uint32le,
      ShiftFlags: r.uint32le,
      dpOutput: r.uint32le,
      dpContext: r.uint32le
    });

    if(this.COMP_KEY.size() != KMXFile.COMP_KEY_SIZE) {
      throw "COMP_KEY size is "+this.COMP_KEY.size()+" but should be "+KMXFile.COMP_KEY_SIZE+" bytes";
    }

    this.COMP_GROUP = new r.Struct({
      dpName: r.uint32le,
      dpKeyArray: r.uint32le,   // [LPKEY] address of first item in key array
      dpMatch: r.uint32le,
      dpNoMatch: r.uint32le,
      cxKeyArray: r.uint32le,   // in array entries
      fUsingKeys: r.uint32le   // group(xx) [using keys] <-- specified or not
    });

    if(this.COMP_GROUP.size() != KMXFile.COMP_GROUP_SIZE) {
      throw "COMP_GROUP size is "+this.COMP_GROUP.size()+" but should be "+KMXFile.COMP_GROUP_SIZE+" bytes";
    }

    this.COMP_KEYBOARD_KMXPLUSINFO = new r.Struct({
      dpKMXPlus: r.uint32le,      // 0040 offset of KMXPlus data, <sect> header is first
      dwKMXPlusSize: r.uint32le  // 0044 size in bytes of entire KMXPlus data
    });

    if(this.COMP_KEYBOARD_KMXPLUSINFO.size() != KMXFile.COMP_KEYBOARD_KMXPLUSINFO_SIZE) {
      throw "COMP_KEYBOARD_KMXPLUSINFO size is "+this.COMP_KEYBOARD_KMXPLUSINFO.size()+" but should be "+KMXFile.COMP_KEYBOARD_KMXPLUSINFO_SIZE+" bytes";
    }

    this.COMP_KEYBOARD = new r.Struct({
      dwIdentifier: r.uint32le,   // 0000 Keyman compiled keyboard id

      dwFileVersion: r.uint32le,  // 0004 Version of the file - Keyman 4.0 is 0x0400

      dwCheckSum: r.uint32le,     // 0008 As stored in keyboard
      KeyboardID: r.uint32le,     // 000C as stored in HKEY_LOCAL_MACHINE//system//currentcontrolset//control//keyboard layouts
      IsRegistered: r.uint32le,   // 0010
      version: r.uint32le,        // 0014 keyboard version

      cxStoreArray: r.uint32le,   // 0018 in array entries
      cxGroupArray: r.uint32le,   // 001C in array entries

      dpStoreArray: r.uint32le,   // 0020 [LPSTORE] address of first item in store array
      dpGroupArray: r.uint32le,   // 0024 [LPGROUP] address of first item in group array

      StartGroup_ANSI: r.uint32le,     // 0028 index of starting ANSI group
      StartGroup_Unicode: r.uint32le,  // 0028 index of starting Unicode groups

      dwFlags: r.uint32le,        // 0030 Flags for the keyboard file

      dwHotKey: r.uint32le,       // 0034 standard windows hotkey (hiword=shift/ctrl/alt stuff, loword=vkey)

      dpBitmapOffset: r.uint32le, // 0038 offset of the bitmaps in the file
      dwBitmapSize: r.uint32le   // 003C size in bytes of the bitmaps
    });

    if(this.COMP_KEYBOARD.size() != KMXFile.COMP_KEYBOARD_SIZE) {
      throw "COMP_KEYBOARD size is "+this.COMP_KEYBOARD.size()+" but should be "+KMXFile.COMP_KEYBOARD_SIZE+" bytes";
    }
  }
}
