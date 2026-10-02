/*
 * Keyman is copyright (C) SIL Global. MIT License.
 *
 * Created by Dr Mark C. Sinclair on 2026-09-25
 *
 * KMC KMN Next Generation Semantic Model Builder
 *
 * KMX enums for the Semantic Model Builder (KmxBuilder)
 */

/* These have been placed in version numerical order for ease of access */

export enum DwFileVersion {
  VERSION_30  = 0x00000300,
  VERSION_31  = 0x00000301,
  VERSION_32  = 0x00000302,
  VERSION_40  = 0x00000400,
  VERSION_50  = 0x00000500,
  VERSION_501 = 0x00000501,
  VERSION_60  = 0x00000600,
  VERSION_70  = 0x00000700,
  VERSION_80  = 0x00000800,
  VERSION_90  = 0x00000900,
  VERSION_100 = 0x00000A00,
  VERSION_140 = 0x00000E00,
  VERSION_150 = 0x00000F00,
  VERSION_160 = 0x00001000,
  VERSION_170 = 0x00001100,
  VERSION_190 = 0x00001300,
}

export const DW_FILE_VERSION_MAP = new Map<Number, DwFileVersion>([
  [3.0,  DwFileVersion.VERSION_30],
  [3.1,  DwFileVersion.VERSION_31],
  [3.2,  DwFileVersion.VERSION_32],
  [4.0,  DwFileVersion.VERSION_40],
  [5.0,  DwFileVersion.VERSION_50],
  [5.1,  DwFileVersion.VERSION_501],
  [6.0,  DwFileVersion.VERSION_60],
  [7.0,  DwFileVersion.VERSION_70],
  [8.0,  DwFileVersion.VERSION_80],
  [9.0,  DwFileVersion.VERSION_90],
  [10.0, DwFileVersion.VERSION_100],
  [11.0, DwFileVersion.VERSION_100],
  [12.0, DwFileVersion.VERSION_100],
  [13.0, DwFileVersion.VERSION_100],
  [14.0, DwFileVersion.VERSION_140],
  [15.0, DwFileVersion.VERSION_150],
  [16.0, DwFileVersion.VERSION_160],
  [17.0, DwFileVersion.VERSION_170],
  [18.0, DwFileVersion.VERSION_170],
  [19.0, DwFileVersion.VERSION_190],
]);

export enum DwSystemID {
  TSS_NONE                      =  0,
  TSS_BITMAP                    =  1,
  TSS_COPYRIGHT                 =  2,
  TSS_HOTKEY                    =  3,
  TSS_LANGUAGE                  =  4, // deprecated
  TSS_LAYOUT                    =  5, // deprecated
  TSS_MESSAGE                   =  6,
  TSS_NAME                      =  7,
  TSS_VERSION                   =  8,
  TSS_CAPSONONLY                =  9,
  TSS_CAPSALWAYSOFF             = 10,
  TSS_SHIFTFREESCAPS            = 11,
  TSS_LANGUAGENAME              = 12, // deprecated
  TSS_CALLDEFINITION            = 13,
  TSS_CALLDEFINITION_LOADFAILED = 14, // deprecated
  TSS_ETHNOLOGUECODE            = 15, // deprecated
  TSS_DEBUG_LINE                = 16,
  TSS_MNEMONIC                  = 17,
  TSS_INCLUDECODES              = 18,
  TSS_OLDCHARPOSMATCHING        = 19, // deprecated
  TSS_COMPILEDVERSION           = 20,
  TSS_KEYMANCOPYRIGHT           = 21,
  TSS_CUSTOMKEYMANEDITION       = 22, // deprecated, always '0', always present
  TSS_CUSTOMKEYMANEDITIONNAME   = 23, // deprecated, always 'Keyman', always present
  TSS_VISUALKEYBOARD            = 24,
  TSS_KMW_RTL                   = 25,
  TSS_KMW_HELPFILE              = 26,
  TSS_KMW_HELPTEXT              = 27,
  TSS_KMW_EMBEDJS               = 28,
  TSS_WINDOWSLANGUAGES          = 29, // deprecated
  TSS_COMPARISON                = 30,
  TSS_PLATFORM                  = 31,
  TSS_BASELAYOUT                = 32,
  TSS_LAYER                     = 33,
  TSS_VKDICTIONARY              = 34,
  TSS_LAYOUTFILE                = 35,
  TSS_KEYBOARDVERSION           = 36,
  TSS_KMW_EMBEDCSS              = 37,
  TSS_TARGETS                   = 38,
  TSS_CASEDKEYS                 = 39,
  TSS_BEGIN_NEWCONTEXT          = 40,
  TSS_BEGIN_POSTKEYSTROKE       = 41,
  TSS_NEWLAYER                  = 42,
  TSS_OLDLAYER                  = 43,
  TSS_DISPLAYMAP                = 44
}
