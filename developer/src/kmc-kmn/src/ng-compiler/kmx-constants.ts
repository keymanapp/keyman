/*
 * Keyman is copyright (C) SIL Global. MIT License.
 *
 * Created by Dr Mark C. Sinclair on 2026-09-25
 *
 * KMC KMN Next Generation Semantic Model Builder
 *
 * KMX enums for the Semantic Model Builder (KmxBuilder)
 */

export const KMX_FILEID_COMPILED = 0x5354584B; // KXTS

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
};

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
};

export enum DwFlags {
  KF_SHIFTFREESCAPS   = 0x00000001,
  KF_CAPSONONLY       = 0x00000002,
  KF_CAPSALWAYSOFF    = 0x00000004,
  KF_LOGICALLAYOUT    = 0x00000008, // unused, do not set
  KF_AUTOMATICVERSION = 0x00000010,
  KF_KMXPLUS          = 0x00000020
};

export enum DwHotKey {
  HK_ALT   = 0x00010000,
  HK_CTRL  = 0x00020000,
  HK_SHIFT = 0x00040000,
};

export enum VKeyName {
  K_$00       = "K_$00",				// 0x0
  K_LBUTTON   = "K_LBUTTON",		// 0x1
  K_RBUTTON   = "K_RBUTTON",	  // 0x2
  K_CANCEL    = "K_CANCEL",		  // 0x3
  K_MBUTTON   = "K_MBUTTON",		// 0x4
  K_$05       = "K_$05",				// 0x5
  K_$06       = "K_$06",				// 0x6
  K_$07       = "K_$07",				// 0x7
  K_BKSP      = "K_BKSP",	    	// 0x8
  K_TAB       = "K_TAB",	    	// 0x9
  K_$0A       = "K_$0A",				// 0xA
  K_$0B       = "K_$0B",				// 0xB
  K_KP5       = "K_KP5",		    // 0xC
  K_ENTER     = "K_ENTER",			// 0xD
  K_$0E       = "K_$0E",				// 0xE
  K_$0F       = "K_$0F",				// 0xF
  K_SHIFT     = "K_SHIFT",			// 0x10
  K_CONTROL   = "K_CONTROL",		// 0x11
  K_ALT       = "K_ALT",				// 0x12
  K_PAUSE     = "K_PAUSE",			// 0x13
  K_CAPS      = "K_CAPS",				// 0x14
  K_KANJI$15  = "K_KANJI$15",		// 0x15
  K_KANJI$16  = "K_KANJI$16",		// 0x16
  K_KANJI$17  = "K_KANJI$17",		// 0x17
  K_KANJI$18  = "K_KANJI$18",		// 0x18
  K_KANJI$19  = "K_KANJI$19",		// 0x19
  K_$1A       = "K_$1A",				// 0x1A
  K_ESC       = "K_ESC",				// 0x1B
  K_KANJI$1C  = "K_KANJI$1C",		// 0x1C
  K_KANJI$1D  = "K_KANJI$1D",		// 0x1D
  K_KANJI$1E  = "K_KANJI$1E",		// 0x1E
  K_KANJI$1F  = "K_KANJI$1F",		// 0x1F
  K_SPACE     = "K_SPACE",			// 0x20
  K_PGUP      = "K_PGUP",				// 0x21
  K_PGDN      = "K_PGDN",				// 0x22
  K_END       = "K_END",				// 0x23
  K_HOME      = "K_HOME",				// 0x24
  K_LEFT      = "K_LEFT",				// 0x25
  K_UP        = "K_UP",				  // 0x26
  K_RIGHT     = "K_RIGHT",			// 0x27
  K_DOWN      = "K_DOWN",				// 0x28
  K_SEL       = "K_SEL",				// 0x29
  K_PRINT     = "K_PRINT",			// 0x2A
  K_EXEC      = "K_EXEC",				// 0x2B
  K_PRTSCN    = "K_PRTSCN",			// 0x2C
  K_INS       = "K_INS",				// 0x2D
  K_DEL       = "K_DEL",				// 0x2E
  K_HELP      = "K_HELP",				// 0x2F
  K_0         = "K_0",					// 0x30
  K_1         = "K_1",					// 0x31
  K_2         = "K_2",					// 0x32
  K_3         = "K_3",					// 0x33
  K_4         = "K_4",					// 0x34
  K_5         = "K_5",					// 0x35
  K_6         = "K_6",					// 0x36
  K_7         = "K_7",					// 0x37
  K_8         = "K_8",					// 0x38
  K_9         = "K_9",					// 0x39
  K_$3A       = "K_$3A",				// 0x3A
  K_$3B       = "K_$3B",				// 0x3B
  K_$3C       = "K_$3C",				// 0x3C
  K_$3D       = "K_$3D",				// 0x3D
  K_$3E       = "K_$3E",				// 0x3E
  K_$3F       = "K_$3F",				// 0x3F
  K_$40       = "K_$40",				// 0x40

  K_A         = "K_A",					// 0x41
  K_B         = "K_B",					// 0x42
  K_C         = "K_C",					// 0x43
  K_D         = "K_D",					// 0x44
  K_E         = "K_E",					// 0x45
  K_F         = "K_F",					// 0x46
  K_G         = "K_G",					// 0x47
  K_H         = "K_H",					// 0x48
  K_I         = "K_I",					// 0x49
  K_J         = "K_J",					// 0x4A
  K_K         = "K_K",					// 0x4B
  K_L         = "K_L",					// 0x4C
  K_M         = "K_M",					// 0x4D
  K_N         = "K_N",					// 0x4E
  K_O         = "K_O",					// 0x4F
  K_P         = "K_P",					// 0x50
  K_Q         = "K_Q",					// 0x51
  K_R         = "K_R",					// 0x52
  K_S         = "K_S",					// 0x53
  K_T         = "K_T",					// 0x54
  K_U         = "K_U",					// 0x55
  K_V         = "K_V",					// 0x56
  K_W         = "K_W",					// 0x57
  K_X         = "K_X",					// 0x58
  K_Y         = "K_Y",					// 0x59
  K_Z         = "K_Z",					// 0x5A
  K_$5B       = "K_$5B",				// 0x5B
  K_$5C       = "K_$5C",				// 0x5C
  K_$5D       = "K_$5D",				// 0x5D
  K_$5E       = "K_$5E",				// 0x5E
  K_$5F       = "K_$5F",				// 0x5F
  K_NP0       = "K_NP0",				// 0x60
  K_NP1       = "K_NP1",				// 0x61
  K_NP2       = "K_NP2",				// 0x62
  K_NP3       = "K_NP3",				// 0x63
  K_NP4       = "K_NP4",				// 0x64
  K_NP5       = "K_NP5",				// 0x65
  K_NP6       = "K_NP6",				// 0x66
  K_NP7       = "K_NP7",				// 0x67
  K_NP8       = "K_NP8",				// 0x68
  K_NP9       = "K_NP9",				// 0x69
  K_NPSTAR    = "K_NPSTAR",			// 0x6A
  K_NPPLUS    = "K_NPPLUS",			// 0x6B
  K_SEPARATOR = "K_SEPARATOR",	// 0x6C
  K_NPMINUS   = "K_NPMINUS",		// 0x6D
  K_NPDOT     = "K_NPDOT",			// 0x6E
  K_NPSLASH   = "K_NPSLASH",		// 0x6F
  K_F1        = "K_F1",				  // 0x70
  K_F2        = "K_F2",				  // 0x71
  K_F3        = "K_F3",				  // 0x72
  K_F4        = "K_F4",				  // 0x73
  K_F5        = "K_F5",				  // 0x74
  K_F6        = "K_F6",				  // 0x75
  K_F7        = "K_F7",				  // 0x76
  K_F8        = "K_F8",				  // 0x77
  K_F9        = "K_F9",				  // 0x78
  K_F10       = "K_F10",				// 0x79
  K_F11       = "K_F11",				// 0x7A
  K_F12       = "K_F12",				// 0x7B
  K_F13       = "K_F13",				// 0x7C
  K_F14       = "K_F14",				// 0x7D
  K_F15       = "K_F15",				// 0x7E
  K_F16       = "K_F16",				// 0x7F
  K_F17       = "K_F17",				// 0x80
  K_F18       = "K_F18",				// 0x81
  K_F19       = "K_F19",				// 0x82
  K_F20       = "K_F20",				// 0x83
  K_F21       = "K_F21",				// 0x84
  K_F22       = "K_F22",				// 0x85
  K_F23       = "K_F23",				// 0x86
  K_F24       = "K_F24",				// 0x87

  K_$88       = "K_$88",				// 0x88
  K_$89       = "K_$89",				// 0x89
  K_$8A       = "K_$8A",				// 0x8A
  K_$8B       = "K_$8B",				// 0x8B
  K_$8C       = "K_$8C",				// 0x8C
  K_$8D       = "K_$8D",				// 0x8D
  K_$8E       = "K_$8E",				// 0x8E
  K_$8F       = "K_$8F",				// 0x8F

  K_NUMLOCK   = "K_NUMLOCK",		// 0x90
  K_SCROLL    = "K_SCROLL",			// 0x91

  K_$92       = "K_$92",				// 0x92
  K_$93       = "K_$93",				// 0x93
  K_$94       = "K_$94",				// 0x94
  K_$95       = "K_$95",				// 0x95
  K_$96       = "K_$96",				// 0x96
  K_$97       = "K_$97",				// 0x97
  K_$98       = "K_$98",				// 0x98
  K_$99       = "K_$99",				// 0x99
  K_$9A       = "K_$9A",				// 0x9A
  K_$9B       = "K_$9B",				// 0x9B
  K_$9C       = "K_$9C",				// 0x9C
  K_$9D       = "K_$9D",				// 0x9D
  K_$9E       = "K_$9E",				// 0x9E
  K_$9F       = "K_$9F",				// 0x9F
  K_$A0       = "K_$A0",				// 0xA0
  K_$A1       = "K_$A1",				// 0xA1
  K_$A2       = "K_$A2",				// 0xA2
  K_$A3       = "K_$A3",				// 0xA3
  K_$A4       = "K_$A4",				// 0xA4
  K_$A5       = "K_$A5",				// 0xA5
  K_$A6       = "K_$A6",				// 0xA6
  K_$A7       = "K_$A7",				// 0xA7
  K_$A8       = "K_$A8",				// 0xA8
  K_$A9       = "K_$A9",				// 0xA9
  K_$AA       = "K_$AA",				// 0xAA
  K_$AB       = "K_$AB",				// 0xAB
  K_$AC       = "K_$AC",				// 0xAC
  K_$AD       = "K_$AD",				// 0xAD
  K_$AE       = "K_$AE",				// 0xAE
  K_$AF       = "K_$AF",				// 0xAF
  K_$B0       = "K_$B0",				// 0xB0
  K_$B1       = "K_$B1",				// 0xB1
  K_$B2       = "K_$B2",				// 0xB2
  K_$B3       = "K_$B3",				// 0xB3
  K_$B4       = "K_$B4",				// 0xB4
  K_$B5       = "K_$B5",				// 0xB5
  K_$B6       = "K_$B6",				// 0xB6
  K_$B7       = "K_$B7",				// 0xB7
  K_$B8       = "K_$B8",				// 0xB8
  K_$B9       = "K_$B9",				// 0xB9

  K_COLON     = "K_COLON",			// 0xBA
  K_EQUAL     = "K_EQUAL",			// 0xBB
  K_COMMA     = "K_COMMA",			// 0xBC
  K_HYPHEN    = "K_HYPHEN",			// 0xBD
  K_PERIOD    = "K_PERIOD",			// 0xBE
  K_SLASH     = "K_SLASH",			// 0xBF
  K_BKQUOTE   = "K_BKQUOTE",		// 0xC0

  K_$C1       = "K_$C1",				// 0xC1
  K_$C2       = "K_$C2",				// 0xC2
  K_$C3       = "K_$C3",				// 0xC3
  K_$C4       = "K_$C4",				// 0xC4
  K_$C5       = "K_$C5",				// 0xC5
  K_$C6       = "K_$C6",				// 0xC6
  K_$C7       = "K_$C7",				// 0xC7
  K_$C8       = "K_$C8",				// 0xC8
  K_$C9       = "K_$C9",				// 0xC9
  K_$CA       = "K_$CA",				// 0xCA
  K_$CB       = "K_$CB",				// 0xCB
  K_$CC       = "K_$CC",				// 0xCC
  K_$CD       = "K_$CD",				// 0xCD
  K_$CE       = "K_$CE",				// 0xCE
  K_$CF       = "K_$CF",				// 0xCF
  K_$D0       = "K_$D0",				// 0xD0
  K_$D1       = "K_$D1",				// 0xD1
  K_$D2       = "K_$D2",				// 0xD2
  K_$D3       = "K_$D3",				// 0xD3
  K_$D4       = "K_$D4",				// 0xD4
  K_$D5       = "K_$D5",				// 0xD5
  K_$D6       = "K_$D6",				// 0xD6
  K_$D7       = "K_$D7",				// 0xD7
  K_$D8       = "K_$D8",				// 0xD8
  K_$D9       = "K_$D9",				// 0xD9
  K_$DA       = "K_$DA",				// 0xDA

  K_LBRKT     = "K_LBRKT",			// 0xDB
  K_BKSLASH   = "K_BKSLASH",		// 0xDC
  K_RBRKT     = "K_RBRKT",			// 0xDD
  K_QUOTE     = "K_QUOTE",			// 0xDE
  K_oDF       = "K_oDF",				// 0xDF
  K_oE0       = "K_oE0",				// 0xE0
  K_oE1       = "K_oE1",				// 0xE1
  K_oE2       = "K_oE2",				// 0xE2
  K_oE3       = "K_oE3",				// 0xE3
  K_oE4       = "K_oE4",				// 0xE4

  K_$E5       = "K_$E5",				// 0xE5

  K_oE6       = "K_oE6",				// 0xE6

  K_$E7       = "K_$E7",				// 0xE7
  K_$E8       = "K_$E8",				// 0xE8

  K_oE9       = "K_oE9",				// 0xE9
  K_oEA       = "K_oEA",				// 0xEA
  K_oEB       = "K_oEB",				// 0xEB
  K_oEC       = "K_oEC",				// 0xEC
  K_oED       = "K_oED",				// 0xED
  K_oEE       = "K_oEE",				// 0xEE
  K_oEF       = "K_oEF",				// 0xEF
  K_oF0       = "K_oF0",				// 0xF0
  K_oF1       = "K_oF1",				// 0xF1
  K_oF2       = "K_oF2",				// 0xF2
  K_oF3       = "K_oF3",				// 0xF3
  K_oF4       = "K_oF4",				// 0xF4
  K_oF5       = "K_oF5",				// 0xF5

  K_$F6       = "K_$F6",				// 0xF6
  K_$F7       = "K_$F7",				// 0xF7
  K_$F8       = "K_$F8",				// 0xF8
  K_$F9       = "K_$F9",				// 0xF9
  K_$FA       = "K_$FA",				// 0xFA
  K_$FB       = "K_$FB",				// 0xFB
  K_$FC       = "K_$FC",				// 0xFC
  K_$FD       = "K_$FD",				// 0xFD
  K_$FE       = "K_$FE",				// 0xFE
  K_$FF       = "K_$FF",				// 0xFF
}
