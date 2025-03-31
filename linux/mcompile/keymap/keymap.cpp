/*
 * Keyman is copyright (C) 2004 - 2024 SIL International. MIT License.
 *
 * Mnemonic layout support for Linux
 *
 * Throughout mcompile we use the following naming conventions:
 *  KEYCODE:     (name on Linux, Mac):The physical position of a key on a keyboard e.g. Keycode for 'Z' on US: 6 on Mac |  52 on Linux/x11 |  44 on Windows
 *  SCANCODE     (name on Windows):  The physical position of a key on a keyboard e.g. Keycode for 'Z' on US: 44 on Windows
 *  VIRTUAL KEY: The value of a character on a key e.g. 'A' = 65; 'a' = 97 - not neccessarily the same as ACSII- exists on a Windows keyboard only
 *  KEYVAL(UE):  The value of a character on a key e.g. 'A' = 65; 'a' = 97 - not neccessarily the same as ACSII
 */

#include "keymap.h"
#include "kmx_file.h"
#include "/usr/include/xcb/xproto.h"
#include <xkbcommon/xkbcommon.h>

const KMX_DWORD INVALID_NAME = 0;
const gint keycode_max  = 94;
const KMX_DWORD deadkey_min  = 0xfe50;  // X11's keysymdef.h defines deadkeys between 0xfe50-0xfe93
const KMX_DWORD deadkey_max  = 0xfe93;  // https://fossies.org/linux/tk/xlib/X11/keysymdef.h

/**
 * @brief  map of all US English virtual key codes that we can translate
 */
const KMX_DWORD KMX_VKMap[] = {
  'A', 'B', 'C', 'D', 'E', 'F', 'G', 'H', 'I', 'J', 'K', 'L', 'M', 'N', 'O', 'P', 'Q', 'R', 'S', 'T', 'U', 'V', 'W', 'X', 'Y', 'Z',
  '0', '1', '2', '3', '4', '5', '6', '7', '8', '9',

  VK_SPACE, /*   32 */

  VK_ACCENT, /*   192 VK_OEM_3   K_BKQUOTE  */
  VK_HYPHEN, /* - 189 VK_OEM_MINUS */
  VK_EQUAL,  /* = 187 VK_OEM_PLUS */

  VK_LBRKT,   /* [ 219 VK_OEM_4 */
  VK_RBRKT,   /* ] 221 VK_OEM_6 */
  VK_BKSLASH, /* \ 220 VK_OEM_5 */

  VK_COLON, /* ; 186 VK_OEM_1  */
  VK_QUOTE, /* ' 222 VK_OEM_7  */

  VK_COMMA,  /* , 188 VK_OEM_COMMA */
  VK_PERIOD, /* . 190 VK_OEM_PERIOD */
  VK_SLASH,  /* / 191 VK_OEM_2 */

  VK_xDF,     /* ß (?) 223*/
  VK_OEM_102, /* < > | 226 */
  0};

/**
 * @brief  array of USVirtualKey-ScanCode-pairs
 *         we use the same type of array as throughout Keyman even though we have lots of unused fields
 */
const KMX_DWORD USVirtualKeyToScanCode[256] = {
    0x00,   // L"K_?00",				// &H0
    0x00,   // L"K_LBUTTON",		// &H1
    0x00,   // L"K_RBUTTON",		// &H2
    0x46,   // L"K_CANCEL",	   	// &H3
    0x00,   // L"K_MBUTTON",		// &H4
    0x00,   // L"K_?05",				// &H5
    0x00,   // L"K_?06",				// &H6
    0x00,   // L"K_?07",				// &H7
    0x0E,   // L"K_BKSP",	    	// &H8
    0x0F,   // L"K_TAB",	   		// &H9
    0x00,   // L"K_?0A",				// &HA
    0x00,   // L"K_?0B",				// &HB
    0x4C,   // L"K_KP5",		   	// &HC
    0x1C,   // L"K_ENTER",			// &HD
    0x00,   // L"K_?0E",				// &HE
    0x00,   // L"K_?0F",				// &HF
    0x2A,   // L"K_SHIFT",			// &H10
    0x1D,   // L"K_CONTROL",	  // &H11
    0x38,   // L"K_ALT",				// &H12
    0x00,   // L"K_PAUSE",			// &H13
    0x3A,   // L"K_CAPS",				// &H14
    0x00,   // L"K_KANJI?15",		// &H15
    0x00,   // L"K_KANJI?16",		// &H16
    0x00,   // L"K_KANJI?17",		// &H17
    0x00,   // L"K_KANJI?18",		// &H18
    0x00,   // L"K_KANJI?19",		// &H19
    0x00,   // L"K_?1A",				// &H1A
    0x01,   // L"K_ESC",				// &H1B
    0x00,   // L"K_KANJI?1C",		// &H1C
    0x00,   // L"K_KANJI?1D",		// &H1D
    0x00,   // L"K_KANJI?1E",		// &H1E
    0x00,   // L"K_KANJI?1F",		// &H1F
    0x39,   // L"K_SPACE",			// &H20
    0x49,   // L"K_PGUP",				// &H21
    0x51,   // L"K_PGDN",				// &H22
    0x4F,   // L"K_END",				// &H23
    0x47,   // L"K_HOME",				// &H24
    0x4B,   // L"K_LEFT",				// &H25
    0x48,   // L"K_UP",					// &H26
    0x4D,   // L"K_RIGHT",			// &H27
    0x50,   // L"K_DOWN",				// &H28
    0x00,   // L"K_SEL",				// &H29
    0x00,   // L"K_PRINT",			// &H2A
    0x00,   // L"K_EXEC",				// &H2B
    0x54,   // L"K_PRTSCN",			// &H2C
    0x52,   // L"K_INS",				// &H2D
    0x53,   // L"K_DEL",				// &H2E
    0x63,   // L"K_HELP",				// &H2F
    0x0B,   // L"K_0",					// &H30
    0x02,   // L"K_1",					// &H31
    0x03,   // L"K_2",					// &H32
    0x04,   // L"K_3",					// &H33
    0x05,   // L"K_4",					// &H34
    0x06,   // L"K_5",					// &H35
    0x07,   // L"K_6",					// &H36
    0x08,   // L"K_7",					// &H37
    0x09,   // L"K_8",					// &H38
    0x0A,   // L"K_9",					// &H39
    0x00,   // L"K_?3A",				// &H3A
    0x00,   // L"K_?3B",				// &H3B
    0x00,   // L"K_?3C",				// &H3C
    0x00,   // L"K_?3D",				// &H3D
    0x00,   // L"K_?3E",				// &H3E
    0x00,   // L"K_?3F",				// &H3F
    0x00,   // L"K_?40",				// &H40
    0x1E,   // L"K_A",					// &H41
    0x30,   // L"K_B",					// &H42
    0x2E,   // L"K_C",					// &H43
    0x20,   // L"K_D",					// &H44
    0x12,   // L"K_E",					// &H45
    0x21,   // L"K_F",					// &H46
    0x22,   // L"K_G",					// &H47
    0x23,   // L"K_H",					// &H48
    0x17,   // L"K_I",					// &H49
    0x24,   // L"K_J",					// &H4A
    0x25,   // L"K_K",					// &H4B
    0x26,   // L"K_L",					// &H4C
    0x32,   // L"K_M",					// &H4D
    0x31,   // L"K_N",					// &H4E
    0x18,   // L"K_O",					// &H4F
    0x19,   // L"K_P",					// &H50
    0x10,   // L"K_Q",					// &H51
    0x13,   // L"K_R",					// &H52
    0x1F,   // L"K_S",					// &H53
    0x14,   // L"K_T",					// &H54
    0x16,   // L"K_U",					// &H55
    0x2F,   // L"K_V",					// &H56
    0x11,   // L"K_W",					// &H57
    0x2D,   // L"K_X",					// &H58
    0x15,   // L"K_Y",					// &H59
    0x2C,   // L"K_Z",					// &H5A
    0x5B,   // L"K_?5B",				// &H5B
    0x5C,   // L"K_?5C",				// &H5C
    0x5D,   // L"K_?5D",				// &H5D
    0x00,   // L"K_?5E",				// &H5E
    0x5F,   // L"K_?5F",				// &H5F
    0x52,   // L"K_NP0",				// &H60
    0x4F,   // L"K_NP1",				// &H61
    0x50,   // L"K_NP2",				// &H62
    0x51,   // L"K_NP3",				// &H63
    0x4B,   // L"K_NP4",				// &H64
    0x4C,   // L"K_NP5",				// &H65
    0x4D,   // L"K_NP6",				// &H66
    0x47,   // L"K_NP7",				// &H67
    0x48,   // L"K_NP8",				// &H68
    0x49,   // L"K_NP9",				// &H69
    0x37,   // L"K_NPSTAR",			// &H6A
    0x4E,   // L"K_NPPLUS",			// &H6B
    0x7E,   // L"K_SEPARATOR",	// &H6C		// MCD 01-11-02: Brazilian Fix, 00 -> 7E
    0x4A,   // L"K_NPMINUS",		// &H6D
    0x53,   // L"K_NPDOT",			// &H6E
    0x135,  // L"K_NPSLASH",		// &H6F
    0x3B,   // L"K_F1",					// &H70
    0x3C,   // L"K_F2",					// &H71
    0x3D,   // L"K_F3",					// &H72
    0x3E,   // L"K_F4",					// &H73
    0x3F,   // L"K_F5",					// &H74
    0x40,   // L"K_F6",					// &H75
    0x41,   // L"K_F7",					// &H76
    0x42,   // L"K_F8",					// &H77
    0x43,   // L"K_F9",					// &H78
    0x44,   // L"K_F10",				// &H79
    0x57,   // L"K_F11",				// &H7A
    0x58,   // L"K_F12",				// &H7B
    0x64,   // L"K_F13",				// &H7C
    0x65,   // L"K_F14",				// &H7D
    0x66,   // L"K_F15",				// &H7E
    0x67,   // L"K_F16",				// &H7F
    0x68,   // L"K_F17",				// &H80
    0x69,   // L"K_F18",				// &H81
    0x6A,   // L"K_F19",				// &H82
    0x6B,   // L"K_F20",				// &H83
    0x6C,   // L"K_F21",				// &H84
    0x6D,   // L"K_F22",				// &H85
    0x6E,   // L"K_F23",				// &H86
    0x76,   // L"K_F24",				// &H87

    0x00,  // L"K_?88",				// &H88
    0x00,  // L"K_?89",				// &H89
    0x00,  // L"K_?8A",				// &H8A
    0x00,  // L"K_?8B",				// &H8B
    0x00,  // L"K_?8C",				// &H8C
    0x00,  // L"K_?8D",				// &H8D
    0x00,  // L"K_?8E",				// &H8E
    0x00,  // L"K_?8F",				// &H8F

    0x45,  // L"K_NUMLOCK",		// &H90
    0x46,  // L"K_SCROL",    	// &H91

    0x00,  // L"K_?92",				// &H92
    0x00,  // L"K_?93",				// &H93
    0x00,  // L"K_?94",				// &H94
    0x00,  // L"K_?95",				// &H95
    0x00,  // L"K_?96",				// &H96
    0x00,  // L"K_?97",				// &H97
    0x00,  // L"K_?98",				// &H98
    0x00,  // L"K_?99",				// &H99
    0x00,  // L"K_?9A",				// &H9A
    0x00,  // L"K_?9B",				// &H9B
    0x00,  // L"K_?9C",				// &H9C
    0x00,  // L"K_?9D",				// &H9D
    0x00,  // L"K_?9E",				// &H9E
    0x00,  // L"K_?9F",				// &H9F
    0x2A,  // L"K_?A0",				// &HA0
    0x36,  // L"K_?A1",				// &HA1
    0x1D,  // L"K_?A2",				// &HA2
    0x1D,  // L"K_?A3",				// &HA3
    0x38,  // L"K_?A4",				// &HA4
    0x38,  // L"K_?A5",				// &HA5
    0x6A,  // L"K_?A6",				// &HA6
    0x69,  // L"K_?A7",				// &HA7
    0x67,  // L"K_?A8",				// &HA8
    0x68,  // L"K_?A9",				// &HA9
    0x65,  // L"K_?AA",				// &HAA
    0x66,  // L"K_?AB",				// &HAB
    0x32,  // L"K_?AC",				// &HAC
    0x20,  // L"K_?AD",				// &HAD
    0x2E,  // L"K_?AE",				// &HAE
    0x30,  // L"K_?AF",				// &HAF
    0x19,  // L"K_?B0",				// &HB0
    0x10,  // L"K_?B1",				// &HB1
    0x24,  // L"K_?B2",				// &HB2
    0x22,  // L"K_?B3",				// &HB3
    0x6C,  // L"K_?B4",				// &HB4
    0x6D,  // L"K_?B5",				// &HB5
    0x6B,  // L"K_?B6",				// &HB6
    0x21,  // L"K_?B7",				// &HB7
    0x00,  // L"K_?B8",				// &HB8
    0x00,  // L"K_?B9",				// &HB9
    0x27,  // L"K_COLON",			// &HBA
    0x0D,  // L"K_EQUAL",			// &HBB
    0x33,  // L"K_COMMA",			// &HBC
    0x0C,  // L"K_HYPHEN",			// &HBD
    0x34,  // L"K_PERIOD",			// &HBE
    0x35,  // L"K_SLASH",			// &HBF
    0x29,  // L"K_BKQUOTE",		// &HC0

    0x73,  // L"K_?C1",				// &HC1
    0x7E,  // L"K_?C2",				// &HC2
    0x00,  // L"K_?C3",				// &HC3
    0x00,  // L"K_?C4",				// &HC4
    0x00,  // L"K_?C5",				// &HC5
    0x00,  // L"K_?C6",				// &HC6
    0x00,  // L"K_?C7",				// &HC7
    0x00,  // L"K_?C8",				// &HC8
    0x00,  // L"K_?C9",				// &HC9
    0x00,  // L"K_?CA",				// &HCA
    0x00,  // L"K_?CB",				// &HCB
    0x00,  // L"K_?CC",				// &HCC
    0x00,  // L"K_?CD",				// &HCD
    0x00,  // L"K_?CE",				// &HCE
    0x00,  // L"K_?CF",				// &HCF
    0x00,  // L"K_?D0",				// &HD0
    0x00,  // L"K_?D1",				// &HD1
    0x00,  // L"K_?D2",				// &HD2
    0x00,  // L"K_?D3",				// &HD3
    0x00,  // L"K_?D4",				// &HD4
    0x00,  // L"K_?D5",				// &HD5
    0x00,  // L"K_?D6",				// &HD6
    0x00,  // L"K_?D7",				// &HD7
    0x00,  // L"K_?D8",				// &HD8
    0x00,  // L"K_?D9",				// &HD9
    0x00,  // L"K_?DA",				// &HDA
    0x1A,  // L"K_LBRKT",			// &HDB
    0x2B,  // L"K_BKSLASH",		// &HDC
    0x1B,  // L"K_RBRKT",			// &HDD
    0x28,  // L"K_QUOTE",			// &HDE
    0x73,  // L"K_oDF",				// &HDF			// MCD 01-11-02: Brazilian fix: 00 -> 73
    0x00,  // L"K_oE0",				// &HE0
    0x00,  // L"K_oE1",				// &HE1
    0x56,  // L"K_oE2",				// &HE2
    0x00,  // L"K_oE3",				// &HE3
    0x00,  // L"K_oE4",				// &HE4

    0x00,  // L"K_?E5",				// &HE5

    0x00,  // L"K_oE6",				// &HE6

    0x00,  // L"K_?E7",				// &HE7
    0x00,  // L"K_?E8",				// &HE8

    0x71,  // L"K_oE9",				// &HE9
    0x5C,  // L"K_oEA",				// &HEA
    0x7B,  // L"K_oEB",				// &HEB
    0x00,  // L"K_oEC",				// &HEC
    0x6F,  // L"K_oED",				// &HED
    0x5A,  // L"K_oEE",				// &HEE
    0x00,  // L"K_oEF",				// &HEF
    0x00,  // L"K_oF0",				// &HF0
    0x5B,  // L"K_oF1",				// &HF1
    0x00,  // L"K_oF2",				// &HF2
    0x5F,  // L"K_oF3",				// &HF3
    0x00,  // L"K_oF4",				// &HF4
    0x5E,  // L"K_oF5",				// &HF5

    0x00,  // L"K_?F6",				// &HF6
    0x00,  // L"K_?F7",				// &HF7
    0x00,  // L"K_?F8",				// &HF8
    0x5D,  // L"K_?F9",				// &HF9
    0x00,  // L"K_?FA",				// &HFA
    0x62,  // L"K_?FB",				// &HFB
    0x00,  // L"K_?FC",				// &HFC
    0x00,  // L"K_?FD",				// &HFD
    0x00,  // L"K_?FE",				// &HFE
    0x00   // L"K_?FF"				  // &HFF
};

/**
 * @brief  array of ScanCode-USVirtualKey-pairs
 *         we use the same type of array as throughout Keyman even though we have lots of unused fields
 */
const KMX_DWORD ScanCodeToUSVirtualKey[128] = {
    0x01,  // 0x00 => K_LBUTTON
    0x1b,  // 0x01 => K_ESC
    0x31,  // 0x02 => K_1
    0x32,  // 0x03 => K_2
    0x33,  // 0x04 => K_3
    0x34,  // 0x05 => K_4
    0x35,  // 0x06 => K_5
    0x36,  // 0x07 => K_6
    0x37,  // 0x08 => K_7
    0x38,  // 0x09 => K_8
    0x39,  // 0x0a => K_9
    0x30,  // 0x0b => K_0
    0xbd,  // 0x0c => K_HYPHEN
    0xbb,  // 0x0d => K_EQUAL
    0x08,  // 0x0e => K_BKSP
    0x09,  // 0x0f => K_TAB
    0x51,  // 0x10 => K_Q
    0x57,  // 0x11 => K_W
    0x45,  // 0x12 => K_E
    0x52,  // 0x13 => K_R
    0x54,  // 0x14 => K_T
    0x59,  // 0x15 => K_Y
    0x55,  // 0x16 => K_U
    0x49,  // 0x17 => K_I
    0x4f,  // 0x18 => K_O
    0x50,  // 0x19 => K_P
    0xdb,  // 0x1a => K_LBRKT
    0xdd,  // 0x1b => K_RBRKT
    0x0d,  // 0x1c => K_ENTER
    0x11,  // 0x1d => K_CONTROL
    0x41,  // 0x1e => K_A
    0x53,  // 0x1f => K_S
    0x44,  // 0x20 => K_D
    0x46,  // 0x21 => K_F
    0x47,  // 0x22 => K_G
    0x48,  // 0x23 => K_H
    0x4a,  // 0x24 => K_J
    0x4b,  // 0x25 => K_K
    0x4c,  // 0x26 => K_L
    0xba,  // 0x27 => K_COLON
    0xde,  // 0x28 => K_QUOTE
    0xc0,  // 0x29 => K_BKQUOTE
    0x10,  // 0x2a => K_SHIFT
    0xdc,  // 0x2b => K_BKSLASH
    0x5a,  // 0x2c => K_Z
    0x58,  // 0x2d => K_X
    0x43,  // 0x2e => K_C
    0x56,  // 0x2f => K_V
    0x42,  // 0x30 => K_B
    0x4e,  // 0x31 => K_N
    0x4d,  // 0x32 => K_M
    0xbc,  // 0x33 => K_COMMA
    0xbe,  // 0x34 => K_PERIOD
    0xbf,  // 0x35 => K_SLASH
    0xa1,  // 0x36 => K_?A1
    0x6a,  // 0x37 => K_NPSTAR
    0x12,  // 0x38 => K_ALT
    0x20,  // 0x39 => K_SPACE
    0x14,  // 0x3a => K_CAPS
    0x70,  // 0x3b => K_F1
    0x71,  // 0x3c => K_F2
    0x72,  // 0x3d => K_F3
    0x73,  // 0x3e => K_F4
    0x74,  // 0x3f => K_F5
    0x75,  // 0x40 => K_F6
    0x76,  // 0x41 => K_F7
    0x77,  // 0x42 => K_F8
    0x78,  // 0x43 => K_F9
    0x79,  // 0x44 => K_F10
    0x90,  // 0x45 => K_NUMLOCK
    0x03,  // 0x46 => K_CANCEL
    0x24,  // 0x47 => K_HOME
    0x26,  // 0x48 => K_UP
    0x21,  // 0x49 => K_PGUP
    0x6d,  // 0x4a => K_NPMINUS
    0x25,  // 0x4b => K_LEFT
    0x0c,  // 0x4c => K_KP5
    0x27,  // 0x4d => K_RIGHT
    0x6b,  // 0x4e => K_NPPLUS
    0x23,  // 0x4f => K_END
    0x28,  // 0x50 => K_DOWN
    0x22,  // 0x51 => K_PGDN
    0x2d,  // 0x52 => K_INS
    0x2e,  // 0x53 => K_DEL
    0x2c,  // 0x54 => K_PRTSCN
    0x00,  // 0x55 => No match
    0xe2,  // 0x56 => K_oE2
    0x7a,  // 0x57 => K_F11
    0x7b,  // 0x58 => K_F12
    0x00,  // 0x59 => No match
    0xee,  // 0x5a => K_oEE
    0x5b,  // 0x5b => K_?5B
    0x5c,  // 0x5c => K_?5C
    0x5d,  // 0x5d => K_?5D
    0xf5,  // 0x5e => K_oF5
    0x5f,  // 0x5f => K_?5F
    0x00,  // 0x60 => No match
    0x00,  // 0x61 => No match
    0xfb,  // 0x62 => K_?FB
    0x2f,  // 0x63 => K_HELP
    0x7c,  // 0x64 => K_F13
    0x7d,  // 0x65 => K_F14
    0x7e,  // 0x66 => K_F15
    0x7f,  // 0x67 => K_F16
    0x80,  // 0x68 => K_F17
    0x81,  // 0x69 => K_F18
    0x82,  // 0x6a => K_F19
    0x83,  // 0x6b => K_F20
    0x84,  // 0x6c => K_F21
    0x85,  // 0x6d => K_F22
    0x86,  // 0x6e => K_F23
    0xed,  // 0x6f => K_oED
    0x00,  // 0x70 => No match
    0xe9,  // 0x71 => K_oE9
    0x00,  // 0x72 => No match
    0xc1,  // 0x73 => K_?C1
    0x00,  // 0x74 => No match
    0x00,  // 0x75 => No match
    0x87,  // 0x76 => K_F24
    0x00,  // 0x77 => No match
    0x00,  // 0x78 => No match
    0x00,  // 0x79 => No match
    0x00,  // 0x7a => No match
    0xeb,  // 0x7b => K_oEB
    0x00,  // 0x7c => No match
    0x00,  // 0x7d => No match
    0x6c,  // 0x7e => K_SEPARATOR
    0x00   // 0x7f => No match
};

/**
 * @brief  map a shiftstate used on Windows to a shiftstate suitable for gdk_keymap_translate_keyboard_state() on Linux
 *            Windows: (Base: 00000000 (0); Shift 00010000 (16); AltGr 00001001 (9); Shift+AltGr 00011001 (25))
 *            Linux:   (Base: 0;            Shift 1;             ALTGr 2;            Shift+ALTGr 3            )
 * @param  shiftState shiftstate used on Windows
 * @return a shiftstate usable for gdk_keymap_translate_keyboard_state() on linux if available
 *         if shiftState is a windows ShiftState: convert the windows ShiftState (0,16,9,25) to a Linux ShiftState (0,1,2,3) that is then used as "Level" in gdk
 *         if shiftState is NOT a windows ShiftState (then in_ShiftState is already a Linux shiftstate): return the entered shiftstate
 */
int convert_Shiftstate_to_LinuxShiftstate(int shiftState) {
  if (shiftState == 0)                                           return 0;                                           // Win ss 0  -> Lin ss 0
  else if (shiftState == K_SHIFTFLAG)                            return XCB_MOD_MASK_SHIFT;                          // Win ss 16 -> Lin ss 1
  else if (shiftState == (LCTRLFLAG | RALTFLAG))                 return XCB_MOD_MASK_LOCK;                           // Win ss 9  -> Lin ss 2
  else if (shiftState == (K_SHIFTFLAG | LCTRLFLAG | RALTFLAG))   return (XCB_MOD_MASK_SHIFT | XCB_MOD_MASK_LOCK);    // Win ss 25 -> Lin ss 3
  else return shiftState;                                                                                            // Lin ss x  -> Lin ss x
}

/**
 * @brief  map a shiftstate used for rgkey to a shiftstate suitable for gdk_keymap_translate_keyboard_state() on Linux
 *            rgkey:   (Base: 0;            Shift1 ;             AltGr 6;            Shift+AltGr 7)
 *            Linux:   (Base: 0;            Shift 1;             ALTGr 2;            Shift+ALTGr 3            )
 * @param  shiftState shiftstate used for rgkey
 * @return a shiftstate usable for gdk_keymap_translate_keyboard_state() on linux if available
 *         if shiftState is a rgkey ShiftState: convert the rgkey ShiftState (0,1,6,7) to a Linux ShiftState (0,1,2,3) that is then used as "Level" in gdk
 *         if shiftState is NOT a rgkey ShiftState (then in_ShiftState is already a Linux shiftstate): return the entered shiftstate
 */
int convert_rgkey_Shiftstate_to_LinuxShiftstate(ShiftState shiftState) {
  if (shiftState == Base)                return 0;                                           // rgkey ss 0  -> Lin ss 0
  else if (shiftState == Shft)           return XCB_MOD_MASK_SHIFT;                          // rgkey ss 1  -> Lin ss 1
  else if (shiftState == MenuCtrl)       return XCB_MOD_MASK_LOCK;                           // rgkey ss 6  -> Lin ss 2
  else if (shiftState == ShftMenuCtrl)   return (XCB_MOD_MASK_SHIFT | XCB_MOD_MASK_LOCK);    // rgkey ss 7  -> Lin ss 3
  else return shiftState;                                                                    // Lin   ss x  -> Lin ss x
}

/**
 * @brief  check for correct input parameter that will later be used in gdk_keymap_translate_keyboard_state()
 * @param  shiftstate the currently used shiftstate
 * @param  keycode the code of the key in question
 * @return true if all parameters are OK;
 * 				 false if not
 */
bool ensureValidInputForKeyboardTranslation(int shiftstate, gint keycode) {

  // We're dealing with shiftstates 0,1,2,3
  if (shiftstate < 0 || shiftstate > 3)
    return false;

  // For K_Space (keycode = 65) only Base and Shift are allowed
  if (keycode == 65 && shiftstate > 1)
    return false;

  if (keycode > keycode_max)
    return false;

  return true;
}

/**
 * @brief  convert names of keys stated in a symbol file to a keyvalue
 * @param  tok_str the name stated in symbol file
 * @return the keyvalue
 */
KMX_DWORD convertNamesTo_DWORD_Value(std::string tok_str) {
  // more on https://manpages.ubuntu.com/manpages/jammy/man3/keysyms.3tk.html
  std::map<std::string, KMX_DWORD> key_values;

  key_values["ampersand"]    = 38;
  key_values["apostrophe"]   = 39;
  key_values["asciicircum"]  = 136;
  key_values["asciitilde"]   = 126;
  key_values["asterisk"]     = 42;
  key_values["at"]           = 64;
  key_values["backslash"]    = 92;
  key_values["BackSpace"]    = 65288;
  key_values["bar"]          = 124;
  key_values["braceleft"]    = 123;
  key_values["braceright"]   = 125;
  key_values["bracketleft"]  = 91;
  key_values["bracketright"] = 93;
  key_values["colon"]        = 58;
  key_values["comma"]        = 44;
  key_values["diaeresis"]    = 168;
  key_values["dollar"]       = 36;
  key_values["equal"]        = 61;
  key_values["exclam"]       = 33;
  key_values["grave"]        = 96;
  key_values["greater"]      = 62;
  key_values["less"]         = 60;
  key_values["minus"]        = 45;
  key_values["numbersign"]   = 35;
  key_values["parenleft"]    = 40;
  key_values["parenright"]   = 41;
  key_values["percent"]      = 37;
  key_values["period"]       = 46;
  key_values["plus"]         = 43;
  key_values["question"]     = 63;
  key_values["quotedbl"]     = 34;
  key_values["semicolon"]    = 59;
  key_values["slash"]        = 47;
  key_values["space"]        = 32;
  key_values["ssharp"]       = 223;
  key_values["underscore"]   = 95;

  key_values["nobreakspace"]   = 160;
  key_values["exclamdown"]     = 161;
  key_values["cent"]           = 162;
  key_values["sterling"]       = 163;
  key_values["currency"]       = 164;
  key_values["yen"]            = 165;
  key_values["brokenbar"]      = 166;
  key_values["section"]        = 167;
  key_values["copyright"]      = 169;
  key_values["ordfeminine"]    = 170;
  key_values["guillemotleft"]  = 171;
  key_values["notsign"]        = 172;
  key_values["hyphen"]         = 173;
  key_values["registered"]     = 174;
  key_values["macron"]         = 175;
  key_values["degree"]         = 176;
  key_values["plusminus"]      = 177;
  key_values["twosuperior"]    = 178;
  key_values["threesuperior"]  = 179;
  key_values["acute"]          = 180;
  key_values["mu"]             = 181;
  key_values["paragraph"]      = 182;
  key_values["periodcentered"] = 183;
  key_values["cedilla"]        = 184;
  key_values["onesuperior"]    = 185;
  key_values["masculine"]      = 186;
  key_values["guillemotright"] = 187;
  key_values["onequarter"]     = 188;
  key_values["onehalf"]        = 189;
  key_values["threequarters"]  = 190;
  key_values["questiondown"]   = 191;
  key_values["Agrave"]         = 192;
  key_values["Aacute"]         = 193;
  key_values["Acircumflex"]    = 194;
  key_values["Atilde"]         = 195;
  key_values["Adiaeresis"]     = 196;
  key_values["Aring"]          = 197;
  key_values["AE"]             = 198;
  key_values["Ccedilla"]       = 199;
  key_values["Egrave"]         = 200;
  key_values["Eacute"]         = 201;
  key_values["Ecircumflex"]    = 202;
  key_values["Ediaeresis"]     = 203;
  key_values["Igrave"]         = 204;
  key_values["Iacute"]         = 205;
  key_values["Icircumflex"]    = 206;
  key_values["Idiaeresis"]     = 207;
  key_values["ETH"]            = 208;
  key_values["Ntilde"]         = 209;
  key_values["Ograve"]         = 210;
  key_values["Oacute"]         = 211;
  key_values["Ocircumflex"]    = 212;
  key_values["Otilde"]         = 213;
  key_values["Odiaeresis"]     = 214;
  key_values["multiply"]       = 215;
  key_values["Oslash"]         = 216;
  key_values["Ugrave"]         = 217;
  key_values["Uacute"]         = 218;
  key_values["Ucircumflex"]    = 219;
  key_values["Udiaeresis"]     = 220;
  key_values["Yacute"]         = 221;
  key_values["THORN"]          = 222;
  key_values["agrave"]         = 224;
  key_values["aacute"]         = 225;
  key_values["acircumflex"]    = 226;
  key_values["atilde"]         = 227;
  key_values["adiaeresis"]     = 228;
  key_values["aring"]          = 229;
  key_values["ae"]             = 230;
  key_values["ccedilla"]       = 231;
  key_values["egrave"]         = 232;
  key_values["eacute"]         = 233;
  key_values["ecircumflex"]    = 234;
  key_values["ediaeresis"]     = 235;
  key_values["igrave"]         = 236;
  key_values["iacute"]         = 237;
  key_values["icircumflex"]    = 238;
  key_values["idiaeresis"]     = 239;
  key_values["eth"]            = 240;
  key_values["ntilde"]         = 241;
  key_values["ograve"]         = 242;
  key_values["oacute"]         = 243;
  key_values["ocircumflex"]    = 244;
  key_values["otilde"]         = 245;
  key_values["odiaeresis"]     = 246;
  key_values["division"]       = 247;
  key_values["oslash"]         = 248;
  key_values["ugrave"]         = 249;
  key_values["uacute"]         = 250;
  key_values["ucircumflex"]    = 251;
  key_values["udiaeresis"]     = 252;
  key_values["yacute"]         = 253;
  key_values["thorn"]          = 254;
  key_values["ydiaeresis"]     = 255;
  key_values["Aogonek"]        = 417;
  key_values["breve"]          = 418;
  key_values["Lstroke"]        = 419;
  key_values["Lcaron"]         = 421;
  key_values["Sacute"]         = 422;
  key_values["Scaron"]         = 425;
  key_values["Scedilla"]       = 426;
  key_values["Tcaron"]         = 427;
  key_values["Zacute"]         = 428;
  key_values["Zcaron"]         = 430;
  key_values["Zabovedot"]      = 431;
  key_values["aogonek"]        = 433;
  key_values["ogonek"]         = 434;
  key_values["lstroke"]        = 435;
  key_values["lcaron"]         = 437;
  key_values["sacute"]         = 438;
  key_values["caron"]          = 439;
  key_values["scaron"]         = 441;
  key_values["scedilla"]       = 442;
  key_values["tcaron"]         = 443;
  key_values["zacute"]         = 444;
  key_values["doubleacute"]    = 445;
  key_values["zcaron"]         = 446;
  key_values["zabovedot"]      = 447;
  key_values["Racute"]         = 448;
  key_values["Abreve"]         = 451;
  key_values["Lacute"]         = 453;
  key_values["Cacute"]         = 454;
  key_values["Ccaron"]         = 456;
  key_values["Eogonek"]        = 458;
  key_values["Ecaron"]         = 460;
  key_values["Dcaron"]         = 463;
  key_values["Dstroke"]        = 464;
  key_values["Nacute"]         = 465;
  key_values["Ncaron"]         = 466;
  key_values["Odoubleacute"]   = 469;
  key_values["Rcaron"]         = 472;
  key_values["Uring"]          = 473;
  key_values["Udoubleacute"]   = 475;
  key_values["Tcedilla"]       = 478;
  key_values["racute"]         = 480;
  key_values["abreve"]         = 483;
  key_values["lacute"]         = 485;
  key_values["cacute"]         = 486;
  key_values["ccaron"]         = 488;
  key_values["eogonek"]        = 490;
  key_values["ecaron"]         = 492;
  key_values["dcaron"]         = 495;
  key_values["dstroke"]        = 496;
  key_values["nacute"]         = 497;
  key_values["ncaron"]         = 498;
  key_values["odoubleacute"]   = 501;
  key_values["rcaron"]         = 504;
  key_values["uring"]          = 505;
  key_values["udoubleacute"]   = 507;
  key_values["tcedilla"]       = 510;
  key_values["abovedot"]       = 511;
  key_values["Hstroke"]        = 673;
  key_values["Hcircumflex"]    = 678;
  key_values["Iabovedot"]      = 681;
  key_values["Gbreve"]         = 683;
  key_values["Jcircumflex"]    = 684;
  key_values["hstroke"]        = 689;
  key_values["hcircumflex"]    = 694;
  key_values["idotless"]       = 697;
  key_values["gbreve"]         = 699;
  key_values["jcircumflex"]    = 700;
  key_values["Cabovedot"]      = 709;
  key_values["Ccircumflex"]    = 710;
  key_values["Gabovedot"]      = 725;
  key_values["Gcircumflex"]    = 728;
  key_values["Ubreve"]         = 733;
  key_values["Scircumflex"]    = 734;
  key_values["cabovedot"]      = 741;
  key_values["ccircumflex"]    = 742;
  key_values["gabovedot"]      = 757;
  key_values["gcircumflex"]    = 760;
  key_values["ubreve"]         = 765;
  key_values["scircumflex"]    = 766;
  key_values["kra"]            = 930;
  key_values["Rcedilla"]       = 931;
  key_values["Itilde"]         = 933;
  key_values["Lcedilla"]       = 934;
  key_values["Emacron"]        = 938;
  key_values["Gcedilla"]       = 939;
  key_values["Tslash"]         = 940;
  key_values["rcedilla"]       = 947;
  key_values["itilde"]         = 949;
  key_values["lcedilla"]       = 950;
  key_values["emacron"]        = 954;
  key_values["gcedilla"]       = 955;
  key_values["tslash"]         = 956;
  key_values["ENG"]            = 957;
  key_values["eng"]            = 959;
  key_values["Amacron"]        = 960;
  key_values["Iogonek"]        = 967;
  key_values["Eabovedot"]      = 972;
  key_values["Imacron"]        = 975;
  key_values["Ncedilla"]       = 977;
  key_values["Omacron"]        = 978;
  key_values["Kcedilla"]       = 979;
  key_values["Uogonek"]        = 985;
  key_values["Utilde"]         = 989;
  key_values["Umacron"]        = 990;
  key_values["amacron"]        = 992;
  key_values["iogonek"]        = 999;
  key_values["eabovedot"]      = 1004;
  key_values["imacron"]        = 1007;
  key_values["ncedilla"]       = 1009;
  key_values["omacron"]        = 1010;
  key_values["kcedilla"]       = 1011;
  key_values["uogonek"]        = 1017;
  key_values["utilde"]         = 1021;
  key_values["umacron"]        = 1022;
  key_values["overline"]       = 1150;

  key_values["dead_abovedot"]    = 729;
  key_values["dead_abovering"]   = 730;
  key_values["dead_acute"]       = 180;
  key_values["dead_breve"]       = 728;
  key_values["dead_caron"]       = 711;
  key_values["dead_cedilla"]     = 184;
  key_values["dead_circumflex"]  = 94;
  key_values["dead_diaeresis"]   = 168;
  key_values["dead_doubleacute"] = 733;
  key_values["dead_grave"]       = 96;
  key_values["dead_ogonek"]      = 731;
  key_values["dead_perispomeni"] = 126;
  key_values["dead_tilde"]       = 126;

  key_values["acute accent"] = 0xB4;

  if (tok_str.size() == 1) {
    return (KMX_DWORD)(*tok_str.c_str());
  } else {
    std::map<std::string, KMX_DWORD>::iterator it;
    for (it = key_values.begin(); it != key_values.end(); ++it) {
      if (it->first == tok_str)
        return it->second;
    }
  }
  return INVALID_NAME;
}

/**
 * @brief  create a 3D-Vector containing data of the US keyboard and the currently used (underlying) keyboard
 *          all_vector [ US_Keyboard  ]
 *                           [KeyCode_US        ]
 *                           [Keyval unshifted  ]
 *                           [Keyval shifted    ]
 *                     [Underlying Kbd]
 *                           [KeyCode_underlying]
 *                           [Keyval unshifted  ]
 *                           [Keyval shifted    ]
 * @param[in,out] all_vector Vector that holds the data of the US keyboard as well as the currently used (underlying) keyboard
 * @param         keymap     pointer to currently used (underlying) keyboard layout
 * @return 0 on success;
 * 				 1 if data of US keyboard was not written;
 * 				 2 if data of underlying keyboard was not written
*/
int createOneVectorFromBothKeyboards(vec_dword_3D& all_vector, GdkKeymap* keymap) {
  // store contents of the English (US) keyboard in all_vector
  if (write_US_ToVector(all_vector)) {
    printf("ERROR: can't write full US to Vector \n");
    return 1;
  }

  // add contents of underlying keyboard to all_vector
  if (append_underlying_ToVector(all_vector, keymap)) {
    printf("ERROR: can't append underlying ToVector \n");
    return 2;
  }
  return 0;
}

/**
 * @brief  write data of the US keyboard into a 3D-Vector which later will contain
 *         data of the US keyboard and the currently used (underlying) keyboard
 * @param[in,out] vec_us Vector that holds the data of the US keyboard
 * @return 0 on success;
 * 				 1 if data of US keyboard was not written;
*/
int write_US_ToVector(vec_dword_3D& vec_us) {
  // create 1D-vector of the complete line
  vec_string_1D vector_completeUS;
  if (createCompleteVector_US(vector_completeUS)) {
    printf("ERROR: can't create complete row US \n");
    return 1;
  }

  // split contents of 1D Vector to 3D vector
  if (split_US_To_3D_Vector(vec_us, vector_completeUS)) {
    return 1;
  }

  if (vector_completeUS.size() < 2) {
    printf("ERROR: several keys of the US keyboard are not processed \n");
    return 1;
  }

  if (vector_completeUS.size() != 48) {
    printf("WARNING: the wrong keyboard input might have been chosen.\n");
    return 0;
  }

  return 0;
}

/**
 * @brief  create a 1D-Vector containing all relevant entries of the symbol file us basic
 * @param[in,out] complete_List the 1D-Vector
 * @return FALSE on success;
 *         TRUE if file could not be opened
*/
bool createCompleteVector_US(vec_string_1D& complete_List) {
  // in the Configuration file we find the appopriate paragraph between "xkb_symbol <text>" and the next xkb_symbol
  // then copy all rows starting with "key <" to a 1D-Vector

  bool create_row = false;
  const char* key = "key <";
  std::string line;
  std::string str_us_kbd_name("xkb_symbols \"basic\"");
  std::string xbk_mark = "xkb_symbol";
  std::ifstream inputFile("/usr/share/X11/xkb/symbols/us");

  if (!inputFile.is_open()) {
    printf("ERROR: could not open file!\n");
    return TRUE;
  }

  else {
    while (getline(inputFile, line)) {
      // stop when finding the mark xkb_symbol
      if (line.find(xbk_mark) != std::string::npos)
        create_row = false;

      // start when finding the mark xkb_symbol + correct layout
      if (line.find(str_us_kbd_name) != std::string::npos)
        create_row = true;

      // as long as we are in the same xkb_symbol layout block and find "key <" we push the whole line into a 1D-vector
      else if (create_row && (line.find(key) != std::string::npos)) {
        complete_List.push_back(line);
      }
    }
  }
  complete_List.push_back("    key <SPCE>  { [ space,        space] };");

  inputFile.close();
  return FALSE;
}

/**
 * @brief  convert the key name obtained from symbol file to the matching keycode
 *         e.g. name of Key <AD06>) --> Keycode 15
 * @param  key_name as stated in the symbol file
 * @return the equivalent keycode
*/
int get_keycode_from_keyname(std::string key_name) {
  int out = INVALID_NAME;

  if (key_name == "key<TLDE>")
    out = 49; /* VK_ BKQUOTE         */
  else if (key_name == "key<AE01>")
    out = 10; /* VK_1                */
  else if (key_name == "key<AE02>")
    out = 11; /* VK_2                */
  else if (key_name == "key<AE03>")
    out = 12; /* VK_3                */
  else if (key_name == "key<AE04>")
    out = 13; /* VK_4                */
  else if (key_name == "key<AE05>")
    out = 14; /* VK_5                */
  else if (key_name == "key<AE06>")
    out = 15; /* VK_6                */
  else if (key_name == "key<AE07>")
    out = 16; /* VK_7                */
  else if (key_name == "key<AE08>")
    out = 17; /* VK_8                */
  else if (key_name == "key<AE09>")
    out = 18; /* VK_9                */
  else if (key_name == "key<AE10>")
    out = 19; /* VK_0                */
  else if (key_name == "key<AE11>")
    out = 20; /* VK_MINUS K_HYPHEN   */
  else if (key_name == "key<AE12>")
    out = 21; /* VK_EQUAL            */

  else if (key_name == "key<AD01>")
    out = 24; /* VK_Q                */
  else if (key_name == "key<AD02>")
    out = 25; /* VK_W                */
  else if (key_name == "key<AD03>")
    out = 26; /* VK_E                */
  else if (key_name == "key<AD04>")
    out = 27; /* VK_R                */
  else if (key_name == "key<AD05>")
    out = 28; /* VK_T                */
  else if (key_name == "key<AD06>")
    out = 29; /* VK_Y                */
  else if (key_name == "key<AD07>")
    out = 30; /* VK_U                */
  else if (key_name == "key<AD08>")
    out = 31; /* VK_I                */
  else if (key_name == "key<AD09>")
    out = 32; /* VK_O                */
  else if (key_name == "key<AD10>")
    out = 33; /* VK_P                */
  else if (key_name == "key<AD11>")
    out = 34; /* VK_LEFTBRACE        */
  else if (key_name == "key<AD12>")
    out = 35; /* VK_RIGHTBRACE       */

  else if (key_name == "key<AC01>")
    out = 38; /* VK_A                */
  else if (key_name == "key<AC02>")
    out = 39; /* VK_S                */
  else if (key_name == "key<AC03>")
    out = 40; /* VK_D                */
  else if (key_name == "key<AC04>")
    out = 41; /* VK_F                */
  else if (key_name == "key<AC05>")
    out = 42; /* VK_G                */
  else if (key_name == "key<AC06>")
    out = 43; /* VK_H                */
  else if (key_name == "key<AC07>")
    out = 44; /* VK_J                */
  else if (key_name == "key<AC08>")
    out = 45; /* VK_K                */
  else if (key_name == "key<AC09>")
    out = 46; /* VK_L                */
  else if (key_name == "key<AC10>")
    out = 47; /* VK_SEMICOLON        */
  else if (key_name == "key<AC11>")
    out = 48; /* VK_APOSTROPHE       */

  else if (key_name == "key<AB01>")
    out = 52; /* VK_Z                */
  else if (key_name == "key<AB02>")
    out = 53; /* VK_X                */
  else if (key_name == "key<AB03>")
    out = 54; /* VK_C                */
  else if (key_name == "key<AB04>")
    out = 55; /* VK_V                */
  else if (key_name == "key<AB05>")
    out = 56; /* VK_B                */
  else if (key_name == "key<AB06>")
    out = 57; /* VK_N                */
  else if (key_name == "key<AB07>")
    out = 58; /* VK_M                */
  else if (key_name == "key<AB08>")
    out = 59; /* VK_ COMMA           */
  else if (key_name == "key<AB09>")
    out = 60; /* VK_DOT              */
  else if (key_name == "key<AB10>")
    out = 61; /* VK_SLASH            */
  else if (key_name == "key<BKSL>")
    out = 51; /* VK_BKSLASH          */
  else if (key_name == "key<LSGT>")
    out = 63; /* VK_RIGHTSHIFT       */
  else if (key_name == "key<SPCE>")
    out = 65; /* VK_SPACE            */

  return out;
}

/**
 * @brief  process each element of a 1D-Vector, split and write to a 3D-Vector
 * @param[in,out] all_US        a 3D_Vector containing all keyvalues of the US keyboard
 * @param         completeList  a 1D-Vector containing all relevant entries copied from the symbol file us basic
 * @return 0 on success if entry can be split
*/
int split_US_To_3D_Vector(vec_dword_3D& all_US, vec_string_1D completeList) {
  // 1: take the whole line of the 1D-Vector and remove unwanted characters.
  // 2: seperate the name e.g. key<AD06> from the shiftstates
  // 3: convert to KMX_DWORD
  // 4: push Names/Shiftstates to shift_states and then shift_states to All_US, our 3D-Vector holding all Elements

  std::vector<char> delim{' ', '[', ']', '}', ';', '\t', '\n'};
  int keyCode;
  vec_string_1D tokens;
  vec_dword_1D tokens_dw;
  vec_dword_2D shift_states;

  // loop through the whole vector
  for (int k = 0; k < (int)completeList.size(); k++) {
    // remove all unwanted char
    for (int i = 0; i < (int)delim.size(); i++) {
      completeList[k].erase(remove(completeList[k].begin(), completeList[k].end(), delim[i]), completeList[k].end());
    }

    // only lines with ("key<.. are of interest
    if (completeList[k].find("key<") != std::string::npos) {
      // split off the key names
      std::istringstream split_Keyname(completeList[k]);
      for (std::string each; std::getline(split_Keyname, each, '{'); tokens.push_back(each)) {
        // empty
      }

      // replace keys names with Keycode (<AD06> with 21,...)
      keyCode   = get_keycode_from_keyname(tokens[0]);
      tokens[0] = std::to_string(keyCode);

      // seperate rest of the vector to its elements and push to 'tokens'
      std::istringstream split_Characters(tokens[1]);
      tokens.pop_back();

      for (std::string each; std::getline(split_Characters, each, ',');
        tokens.push_back(each));

      // now convert all to KMX_DWORD and fill tokens
      tokens_dw.push_back((KMX_DWORD)keyCode);

      for (int i = 1; i < (int)tokens.size(); i++) {
        // replace a name with a single character ( a -> a  ; equal -> = )
        KMX_DWORD tokens_int = convertNamesTo_DWORD_Value(tokens[i]);
        tokens_dw.push_back(tokens_int);
      }

      shift_states.push_back(tokens_dw);
      tokens_dw.clear();
      tokens.clear();
    }
  }
  all_US.push_back(shift_states);

  if (all_US.size() == 0) {
    printf("ERROR: Can't split US to 3D-Vector\n");
    return 1;
  }
  return 0;
}

/**
 * @brief  create an 2D-Vector with all fields initialized to INVALID_NAME
 * @param  dim_rows number of rows in vector
 * @param  dim_ss   number of columns in vector
 * @return the 2D-Vector
*/
vec_dword_2D create_empty_2D_Vector(int dim_rows, int dim_ss) {
  vec_dword_1D shifts;
  vec_dword_2D vector_2D;

  for (int j = 0; j < dim_ss; j++) {
    shifts.push_back(INVALID_NAME);
  }

  for (int i = 0; i < dim_rows; i++) {
    vector_2D.push_back(shifts);
  }
  return vector_2D;
}

/**
 * @brief  append a 2D-vector containing data of the currently used (underlying) keyboard to the 3D-vector
 * @param[in,out] all_vector 3D-vector that holds the data of the US keyboard and the currently used (underlying) keyboard
 * @param         keymap     pointer to currently used (underlying) keybord layout
 * @return 0 on success;
 *         1 if the initialization of the underlying vector fails;
 *         2 if data of less than 2 keyboards is contained in all_vector
*/
int append_underlying_ToVector(vec_dword_3D& all_vector, GdkKeymap* keymap) {
  if (all_vector.size() != 1) {
    printf("ERROR: data for US keyboard not correct\n");
    return 1;
  }

  // create a 2D vector all filled with " " and push to 3D-Vector
  vec_dword_2D underlying_Vector2D = create_empty_2D_Vector(all_vector[0].size(), all_vector[0][0].size());

  if (underlying_Vector2D.size() == 0) {
    printf("ERROR: can't create empty 2D-Vector\n");
    return 1;
  }

  all_vector.push_back(underlying_Vector2D);
  if (all_vector.size() < 2) {
    printf("ERROR: creation of 3D-Vector failed\n");
    return 2;
  }

  for (int i = 0; i < (int)all_vector[1].size(); i++) {
    // get key name US stored in [0][i][0] and copy to name in "underlying"-block[1][i][0]
    all_vector[1][i][0] = all_vector[0][i][0];

    // get Keyvals of this key and copy to unshifted/shifted in "underlying"-block[1][i][1] / block[1][i][2]
    all_vector[1][i][0 + 1] = KMX_get_KeyValUnderlying_From_KeyCodeUnderlying(keymap, all_vector[0][i][0], convert_rgkey_Shiftstate_to_LinuxShiftstate(ShiftState::Base));  // shift state: unshifted:0
    all_vector[1][i][1 + 1] = KMX_get_KeyValUnderlying_From_KeyCodeUnderlying(keymap, all_vector[0][i][0], convert_rgkey_Shiftstate_to_LinuxShiftstate(ShiftState::Shft));  // shift state: shifted:1
  }

  return 0;
}

/**
 * @brief  initializes GDK and return the current keymap for later use
 * @param  keymap [out] currently used (underlying) keyboard layout
 * @return FALSE on success;
 *         TRUE if the display or keymap is not found
*/
bool InitializeGDK(GdkKeymap** keymap, int argc, gchar* argv[]) {
  // get keymap of underlying keyboard

  gdk_init(&argc, &argv);
  GdkDisplay* display = gdk_display_get_default();
  if (!display) {
    printf("ERROR: can't get display\n");
    return TRUE;
  }

  *keymap = gdk_keymap_get_for_display(display);
  if (!keymap) {
    printf("ERROR: Can't get keymap\n");
    gdk_display_close(display);
    return TRUE;
  }
  // intentionally leaking `display` in order to still be able to access `keymap`
  return FALSE;
}

/**
 * @brief  check if keyval correponds to a character we use in mcompile
 * @param  kv the keyval to be checked
 * @return true if keyval is used in mcompile;
 *         false if not
*/
bool IsKeymanUsedChar(int kv) {
  //         32            A-Z                      a-z
  if ((kv == 0x20) || (kv >= 65 && kv <= 90) || (kv >= 97 && kv <= 122))
    return true;
  else
    return false;
}

/**
 * @brief  convert a deadkey-value to a u16string if it is in the range of deadkeys used for mcompile.
 *         deadkeys used for mcompile e.g. 65106 -> '^'
 * @param  in value to be converted
 * @return on success a u16string holding the converted value;
 *         else u"\0"
*/
std::u16string convert_DeadkeyValues_To_U16str(KMX_DWORD in) {
  if (in == 0)
    return u"\0";

  if ((int)in < (int)deadkey_min) {                    // no deadkey; no Unicode
    return std::u16string(1, in);
  }

  std::string long_name((const char*)gdk_keyval_name(in));  // e.g. "dead_circumflex",  "U+017F",  "t"

  if (long_name.substr(0, 2) == "U+")             // U+... Unicode value
    return CodePointToU16String(in - 0x1000000);  // GDK's gdk_keymap_translate_keyboard_state() returns (Keyvalue | 0x01000000)
                                                  // since we never have a carry-over we can just subtract 0x01000000

  KMX_DWORD lname = convertNamesTo_DWORD_Value(long_name);  // 65106 => "dead_circumflex" => 94 => "^"

  if (lname != INVALID_NAME) {
    return std::u16string(1, lname);
  } else
    return u"\0";
}

/**
 * @brief  return the keyvalue for a given Keycode, shiftstate and caps
 *         currently used (underlying) keyboard layout
 *         "What character will be produced for a keypress of a key and modifier?"
 * @param  keymap  pointer to the currently used (underlying) keyboard layout
 * @param  keycode a key of the currently used keyboard layout
 * @param  ss      a (windows-)shiftstate of the currently used keyboard layout
 * @param  caps    state of the caps key of the currently used keyboard layout
 * @return the keyval obtained from keycode, shiftstate and caps
*/
KMX_DWORD KMX_get_KeyVal_From_KeyCode(GdkKeymap* keymap, guint keycode, ShiftState ss, int caps) {
  GdkModifierType consumed;
  GdkKeymapKey* maps;
  guint* keyvals;
  gint count;
  if (!gdk_keymap_get_entries_for_keycode(keymap, keycode, &maps, &keyvals, &count))
    return 0;

  if (!(ensureValidInputForKeyboardTranslation(convert_rgkey_Shiftstate_to_LinuxShiftstate(ss), keycode))) {
    g_free(keyvals);
    g_free(maps);
    return 0;
  }

  // BASE (shiftstate: 0)
  if ((ss == Base) && (caps == 0)) {
    GdkModifierType MOD_base = (GdkModifierType)(~GDK_MODIFIER_MASK);
    gdk_keymap_translate_keyboard_state(keymap, keycode, MOD_base, 0, keyvals, NULL, NULL, &consumed);
  }

  // BASE + CAPS (shiftstate: 0)
  else if ((ss == Base) && (caps == 1)) {
    GdkModifierType MOD_Caps = (GdkModifierType)(GDK_LOCK_MASK);
    gdk_keymap_translate_keyboard_state(keymap, keycode, MOD_Caps, 0, keyvals, NULL, NULL, &consumed);
  }

  // SHIFT (shiftstate: 1)
  else if ((ss == Shft) && (caps == 0)) {
    GdkModifierType MOD_Shift = (GdkModifierType)(GDK_SHIFT_MASK);
    gdk_keymap_translate_keyboard_state(keymap, keycode, MOD_Shift, 0, keyvals, NULL, NULL, &consumed);
  }

  // SHIFT + CAPS (shiftstate: 1)
  else if ((ss == Shft) && (caps == 1)) {
    GdkModifierType MOD_ShiftCaps = (GdkModifierType)((GDK_SHIFT_MASK | GDK_LOCK_MASK));
    gdk_keymap_translate_keyboard_state(keymap, keycode, MOD_ShiftCaps, 0, keyvals, NULL, NULL, &consumed);
  }

  // Ctrl (shiftstate: 2)
  else if ((ss == Ctrl) && (caps == 0)) {
    GdkModifierType MOD_Ctrl = (GdkModifierType)(GDK_MOD5_MASK);
    gdk_keymap_translate_keyboard_state(keymap, keycode, MOD_Ctrl, 0, keyvals, NULL, NULL, &consumed);
  }

  // Ctrl + CAPS  (shiftstate: 2)
  else if ((ss == Ctrl) && (caps == 1)) {
    GdkModifierType MOD_CtrlCaps = (GdkModifierType)(GDK_MOD5_MASK | GDK_LOCK_MASK);
    gdk_keymap_translate_keyboard_state(keymap, keycode, MOD_CtrlCaps, 0, keyvals, NULL, NULL, &consumed);
  }

  // SHIFT+Ctrl (shiftstate: 3)
  else if ((ss == ShftCtrl) && (caps == 0)) {
    GdkModifierType MOD_Ctrl = (GdkModifierType)(GDK_SHIFT_MASK | GDK_MOD5_MASK);
    gdk_keymap_translate_keyboard_state(keymap, keycode, MOD_Ctrl, 0, keyvals, NULL, NULL, &consumed);
  }

  // SHIFT+Ctrl + CAPS  (shiftstate: 3)
  else if ((ss == ShftCtrl) && (caps == 1)) {
    GdkModifierType MOD_CtrlCaps = (GdkModifierType)(GDK_SHIFT_MASK | GDK_MOD5_MASK | GDK_LOCK_MASK);
    gdk_keymap_translate_keyboard_state(keymap, keycode, MOD_CtrlCaps, 0, keyvals, NULL, NULL, &consumed);
  }

  // ALT-GR (shiftstate: 6)
  else if ((ss == MenuCtrl) && (caps == 0)) {
    GdkModifierType MOD_AltGr = (GdkModifierType)(GDK_MOD2_MASK | GDK_MOD5_MASK);
    gdk_keymap_translate_keyboard_state(keymap, keycode, MOD_AltGr, 0, keyvals, NULL, NULL, &consumed);
  }

  // ALT-GR + CAPS (shiftstate: 6)
  else if ((ss == MenuCtrl) && (caps == 1)) {
    GdkModifierType MOD_AltGr = (GdkModifierType)(GDK_MOD2_MASK | GDK_MOD5_MASK | GDK_LOCK_MASK);
    gdk_keymap_translate_keyboard_state(keymap, keycode, MOD_AltGr, 0, keyvals, NULL, NULL, &consumed);
  }

  // ALT-GR (shiftstate: 7)
  else if ((ss == ShftMenuCtrl) && (caps == 0)) {
    GdkModifierType MOD_AltGr = (GdkModifierType)((GDK_SHIFT_MASK | GDK_MOD2_MASK | GDK_MOD5_MASK));
    gdk_keymap_translate_keyboard_state(keymap, keycode, MOD_AltGr, 0, keyvals, NULL, NULL, &consumed);
  }

  // ALT-GR +CAPS (shiftstate: 7)
  else if ((ss == ShftMenuCtrl) && (caps == 1)) {
    GdkModifierType MOD_AltGr = (GdkModifierType)((GDK_SHIFT_MASK | GDK_MOD2_MASK | GDK_MOD5_MASK | GDK_LOCK_MASK));
    gdk_keymap_translate_keyboard_state(keymap, keycode, MOD_AltGr, 0, keyvals, NULL, NULL, &consumed);
  } else
    return 0;

  return (int)*keyvals;
}

/**
 * @brief  return the keyvalue for a given Keycode and shiftstate of the currently used (underlying) keyboard layout.
 *         "What character will be produced for a keypress of a key and modifiers on the underlying keyboard?"
 * @param  keymap          a pointer to the currently used (underlying) keyboard layout
 * @param  keycode         a key of the currently used keyboard
 * @param  shiftState a shiftstate of the currently used keyboard layout
 * @return the keyval obtained from Keycode and shiftstate;
 */
KMX_DWORD KMX_get_KeyValUnderlying_From_KeyCodeUnderlying(GdkKeymap* keymap, guint keycode, int shiftState) {
  GdkKeymapKey* maps;
  guint* keyvals;
  gint count;
  KMX_DWORD kVal;

  if (!gdk_keymap_get_entries_for_keycode(keymap, keycode, &maps, &keyvals, &count))
    return 0;


  if (!(ensureValidInputForKeyboardTranslation(shiftState, keycode))) {
    g_free(keyvals);
    g_free(maps);
    return 0;
  }

  kVal = KMX_get_KeyVal_From_KeyCode(keymap, keycode, (ShiftState)shiftState, 0);

  g_free(keyvals);
  g_free(maps);

  return kVal;
}

/**
 * @brief  return the keyvalue for a given Keycode and shiftstate of the currently used (underlying) keyboard layout.
 *         "What character will be produced for a keypress of a key and modifiers on the underlying keyboard?"
 *         If a deadkey was found return 0xFFFF and copy the deadkey into deadKey
 *         This function is similar to KMX_DWORD KMX_get_KeyValUnderlying_From_KeyCodeUnderlying(GdkKeymap* keymap, guint keycode, int shiftState)
 *         but processes deadkeys
 * @param  keymap     a pointer to the currently used (underlying) keyboard layout
 * @param  keycode    a key of the currently used keyboard
 * @param  shiftState a shiftstate of the currently used keyboard layout
 * @param  deadKey*   pointer to keyvalue if a deadkey was found; if not NULL
 * @return 0xFFFF in case a deadkey was found, then the deadkey is stored in deadKey
 *         0xFFFE in case a deadkey is out of range
 *         the keyval obtained from Keycode and shiftstate and caps;
*/
KMX_DWORD KMX_get_KeyValUnderlying_From_KeyCodeUnderlying(GdkKeymap* keymap, guint keycode, KMX_DWORD shiftState, PKMX_WCHAR deadkey) {
  GdkKeymapKey* maps;
  guint* keyvals;
  gint count;
  PKMX_WCHAR dky = NULL;

  if (!gdk_keymap_get_entries_for_keycode(keymap, keycode, &maps, &keyvals, &count))
    return 0;

 if (!(ensureValidInputForKeyboardTranslation(convert_Shiftstate_to_LinuxShiftstate(shiftState), keycode))) {
    g_free(keyvals);
    g_free(maps);
    return 0;
  }

  KMX_DWORD keyV = KMX_get_KeyVal_From_KeyCode(keymap, keycode, ShiftState(convert_Shiftstate_to_LinuxShiftstate(shiftState)), 0);

  g_free(keyvals);
  g_free(maps);

  if ((keyV >= deadkey_min) && (keyV <= deadkey_max)) {                          // deadkey

    std::u16string keyVS = convert_DeadkeyValues_To_U16str(keyV);
    dky = (PKMX_WCHAR)keyVS.c_str();

    *deadkey = *dky;
    return 0xFFFF;
  } else if ((keyV > deadkey_max) || ((keyV < deadkey_min) && (keyV > 0xFF)))   // out of range
    return 0xFFFE;
  else                                                                          // usable char
    return keyV;
}


/**
 * @brief  return the keyvalue of a key of the the currently used (underlying) keyboard for a given keyvalue of the US keyboard
 *         "What character is on the same position/shiftstats/caps on the currently used (underlying) keyboard as on the US keyboard?"
 * @param  all_vector 3D-vector that holds the data of the US keyboard and the currently used (underlying) keyboard
 * @param  kv_us      a keyvalue on the US keyboard
 * @return keyval of the underlying keyboard if available;
 * 				 else the keyval of the US keyboard
*/
KMX_DWORD KMX_get_KeyValUnderlying_From_KeyValUS(vec_dword_3D& all_vector, KMX_DWORD kv_us) {
  // look for kv_us for any shiftstate of US keyboard
  for (int i = 0; i < (int)all_vector[0].size() - 1; i++) {
    for (int j = 1; j < (int)all_vector[0][0].size(); j++) {
      if (all_vector[0][i][j] == kv_us)
        return all_vector[1][i][j];
    }
  }
  return kv_us;
}

/**
 * @brief  return the keycode of the currently used (underlying) keyboard for a given keycode of the US keyboard
 *         "Where on an underlying keyboard do we find a character that is on a certain key on a US keyboard?"
 * @param  keymap     the currently used (underlying) keyboard layout
 * @param  all_vector 3D-vector that holds the data of the US keyboard and the currently used (underlying) keyboard
 * @param  kc_us      a key of the US keyboard
 * @param  ss         a windows-type shiftstate
 * @param  caps       state of the caps key
 * @return the keycode of the underlying keyboard if found;
 * 				 else the keycode of the US keyboard
*/
KMX_DWORD KMX_get_KeyCodeUnderlying_From_KeyCodeUS(GdkKeymap* keymap, vec_dword_3D& all_vector, KMX_DWORD kc_us, ShiftState ss, int caps) {
  std::u16string u16str = convert_DeadkeyValues_To_U16str(KMX_get_KeyVal_From_KeyCode(keymap, kc_us, ss, caps));

  for (int i = 0; i < (int)all_vector[1].size() - 1; i++) {
    for (int j = 1; j < (int)all_vector[1][0].size(); j++) {
      if ((all_vector[1][i][j] == (KMX_DWORD)*u16str.c_str()))
        return all_vector[1][i][0];
    }
  }
  return kc_us;
}

/**
 * @brief  return the keycode of the currently used (underlying) keyboard for a given virtual key of the US keyboard
 *         "Where on an underlying keyboard do we find a character of a US keyboard?"
 * @param  virtualKeyUS a virtual key of the US keyboard
 * @return the keycode of the currently used (underlying) keyboard
*/
KMX_DWORD KMX_get_KeyCodeUnderlying_From_VKUS(KMX_DWORD virtualKeyUS) {
  // Linux virtualKeys are always 8 different to Windows virtualKeys
  return (KMX_DWORD)(8 + USVirtualKeyToScanCode[virtualKeyUS]);
}

/**
 * @brief  return a virtual key of the US keyboard for a given keycode of the currently used (underlying) keyboard
 *         "Which key of a underlying keyboard will be mapped to a virtual key of a US keyboard?"
 * @param  keycode a keycode of the currently used (underlying) keyboard
 * @return the virtual key of the US keyboard or
 *  	     0 if the key is not used
*/
KMX_DWORD KMX_get_VKUS_From_KeyCodeUnderlying(KMX_DWORD keycode) {
  // Linux virtualKeys are always 8 different to Windows virtualKeys
  if (keycode > 7)
    return (KMX_DWORD)ScanCodeToUSVirtualKey[keycode - 8];

  return 0;
}

/**
 * @brief  convert a codepoint to a u16string
 * @param  codepoint to be converted
 * @return a u16string holding the converted value;
*/
std::u16string CodePointToU16String(unsigned int codepoint) {
  std::u16string str;

  if (codepoint <= 0xFFFF) {
    str = static_cast<char16_t>(codepoint);
  } else {
    assert(codepoint < 0x10FFFF);
    assert(isLittleEndianSystem());

    codepoint -= 0x10000;
    str.resize(2);
    str[0] = static_cast<char16_t>(0xDC00 + (codepoint & 0x3FF));
    str[1] = static_cast<char16_t>(0xD800 + ((codepoint >> 10) & 0x3FF));
  }
  return str;
}
