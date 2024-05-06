
#pragma once
#ifndef KEYMAP_H
#define KEYMAP_H

// compile with.    gcc -framework Carbon -o Bla.exe HW.cpp
// #import <Carbon/Carbon.h>

#include <iostream>
#include <Carbon/Carbon.h>
#include <string>
#include <vector>

#include "u16.h"

std::u16string mac_get_character_From_Keycode(std::vector<int> keyval, int shiftstate,const UCKeyboardLayout* keyboard_layout);

KMX_DWORD  mac_get_keyval_From_Keycode_new(int charVal,const UCKeyboardLayout* keyboard_layout , KMX_DWORD shiftstate);

//################################################################################################################################################
//################################# Code beyond these lines needs to be included in mcompile #####################################################
//################################################################################################################################################

// In ths program we use a 3D-Vector  Vector[language][Keys][Shiftstates]
/*

#include <X11/XKBlib.h>
#include <X11/Xlib.h>
#include <gdk/gdk.h>

#include <map>
#include <algorithm>
#include <fstream>
#include <iomanip>
#include <iostream>
#include <string>
#include <vector>
*/

enum ShiftState {
    Base = 0,                           // 0
    Shft = 1,                           // 1
    Ctrl = 2,                           // 2
    ShftCtrl = Shft | Ctrl,             // 3
    Menu = 4,                           // 4 -- NOT USED
    ShftMenu = Shft | Menu,             // 5 -- NOT USED
    MenuCtrl = Menu | Ctrl,             // 6
    ShftMenuCtrl = Shft | Menu | Ctrl,  // 7
    Xxxx = 8,                           // 8
    ShftXxxx = Shft | Xxxx,             // 9
};

// Map of all US English virtual key codes that we can translate
const KMX_DWORD KMX_VKMap[] = {
  'A', 'B', 'C', 'D', 'E', 'F', 'G', 'H', 'I', 'J', 'K', 'L', 'M', 'N', 'O', 'P', 'Q', 'R', 'S', 'T', 'U', 'V', 'W', 'X', 'Y', 'Z',
  '0', '1', '2', '3', '4', '5', '6', '7', '8', '9',

  VK_SPACE,     //   32 //

  VK_ACCENT,    //   192 VK_OEM_3   K_BKQUOTE  // 
  VK_HYPHEN,    // - 189 VK_OEM_MINUS //
  VK_EQUAL,     // = 187 VK_OEM_PLUS //

  VK_LBRKT,     // [ 219 VK_OEM_4 //
  VK_RBRKT,     // ] 221 VK_OEM_6 //
  VK_BKSLASH,   // \ 220 VK_OEM_5 //

  VK_COLON,     // ; 186 VK_OEM_1  //
  VK_QUOTE,     // ' 222 VK_OEM_7  //

  VK_COMMA,     // , 188 VK_OEM_COMMA //
  VK_PERIOD,    // . 190 VK_OEM_PERIOD //
  VK_SLASH,     // / 191 VK_OEM_2 //ˍ

  VK_xDF,       // ß (?) 223//
  VK_OEM_102,   // < > | 226 //
  0
};

typedef std::vector<std::string> v_str_1D;

typedef std::vector<KMX_DWORD> v_dw_1D;
typedef std::vector<std::vector<KMX_DWORD> > v_dw_2D;
typedef std::vector<std::vector<std::vector<KMX_DWORD> > > v_dw_3D;

static KMX_DWORD returnIfCharInvalid = 0;
static KMX_DWORD max_shiftstate = 2;		// _S2 only base+Shift
static KMX_DWORD keycode_max = 94;			// _S2 needed??
static KMX_DWORD deadkey_min = 0xfe50;	// _S2 needed??
static KMX_DWORD deadkey_max = 0xfe93;	// _S2 needed??
//static KMX_DWORD deadkey_max = 0xfe52;  // _S2 TOP_6 TODO This has to go! my test: to only return 3 dk

// map Shiftstate to modifier (e.g. 0->0; )
int mac_map_VKShiftState_to_MacModifier(int VKShiftState);

// take a std::string (=contents of line symbols-file ) and returns the (int) value of the character
KMX_DWORD mac_convertNamesTo_DWORD_Value(std::string tok_str);

// create a Vector with all entries of both keymaps
int mac_createOneVectorFromBothKeyboards(v_dw_3D &All_Vector, const UCKeyboardLayout * keykeyboard_layout);

// read configuration file, split and write to 3D-Vector (Data for US on Vector[0][ ][ ]  )
int mac_write_US_ToVector(v_dw_3D &vec);

// 1. step: read complete Row of Configuration file US
bool mac_createCompleteRow_US(v_str_1D &complete_List, FILE *fpp, const char *text, std::string language);

// replace Name of Key (e.g. <AD06>)  wih Keycode ( e.g. 15 )
int mac_replace_KeyName_with_Keycode(std::string  in);

// create an empty 2D vector containing 0 in all fields
v_dw_2D mac_create_empty_2D_Vector(int dim_rows, int dim_shifts);

// append characters to 3D-Vector using GDK (Data for underlying Language on Vector[1][ ][ ]  )
int mac_append_underlying_ToVector(v_dw_3D &All_Vector, const UCKeyboardLayout * keykeyboard_layout);

// initialize UCHR
bool mac_InitializeUCHR(const UCKeyboardLayout **keyboard_layout);
//------------------------------

const UINT USVirtualKeyToScanCode[256] = {
	0x00, // L"K_?00",				// &H0
	0x00, // L"K_LBUTTON",		// &H1
	0x00, // L"K_RBUTTON",		// &H2
	0x46, // L"K_CANCEL",	   	// &H3
	0x00, // L"K_MBUTTON",		// &H4
	0x00, // L"K_?05",				// &H5
	0x00, // L"K_?06",				// &H6
	0x00, // L"K_?07",				// &H7
	0x0E, // L"K_BKSP",	    	// &H8
	0x0F, // L"K_TAB",	   		// &H9
	0x00, // L"K_?0A",				// &HA
	0x00, // L"K_?0B",				// &HB
	0x4C, // L"K_KP5",		   	// &HC
	0x1C, // L"K_ENTER",			// &HD
	0x00, // L"K_?0E",				// &HE
	0x00, // L"K_?0F",				// &HF
	0x2A, // L"K_SHIFT",			// &H10
	0x1D, // L"K_CONTRO0x00,	// L",	// &H11
	0x38, // L"K_ALT",				// &H12
	0x00, // L"K_PAUSE",			// &H13
	0x3A, // L"K_CAPS",				// &H14
	0x00, // L"K_KANJI?15",		// &H15
	0x00, // L"K_KANJI?16",		// &H16
	0x00, // L"K_KANJI?17",		// &H17
	0x00, // L"K_KANJI?18",		// &H18
	0x00, // L"K_KANJI?19",		// &H19
	0x00, // L"K_?1A",				// &H1A
	0x01, // L"K_ESC",				// &H1B
	0x00, // L"K_KANJI?1C",		// &H1C
	0x00, // L"K_KANJI?1D",		// &H1D
	0x00, // L"K_KANJI?1E",		// &H1E
	0x00, // L"K_KANJI?1F",		// &H1F
	0x39, // L"K_SPACE",			// &H20
	0x49, // L"K_PGUP",				// &H21
	0x51, // L"K_PGDN",				// &H22
	0x4F, // L"K_END",				// &H23
	0x47, // L"K_HOME",				// &H24
	0x4B, // L"K_LEFT",				// &H25
	0x48, // L"K_UP",					// &H26
	0x4D, // L"K_RIGHT",			// &H27
	0x50, // L"K_DOWN",				// &H28
	0x00, // L"K_SEL",				// &H29
	0x00, // L"K_PRINT",			// &H2A
	0x00, // L"K_EXEC",				// &H2B
	0x54, // L"K_PRTSCN",			// &H2C
	0x52, // L"K_INS",				// &H2D
	0x53, // L"K_DEL",				// &H2E
	0x63, // L"K_HELP",				// &H2F
	0x0B, // L"K_0",					// &H30
	0x02, // L"K_1",					// &H31
	0x03, // L"K_2",					// &H32
	0x04, // L"K_3",					// &H33
	0x05, // L"K_4",					// &H34
	0x06, // L"K_5",					// &H35
	0x07, // L"K_6",					// &H36
	0x08, // L"K_7",					// &H37
	0x09, // L"K_8",					// &H38
	0x0A, // L"K_9",					// &H39
	0x00, // L"K_?3A",				// &H3A
	0x00, // L"K_?3B",				// &H3B
	0x00, // L"K_?3C",				// &H3C
	0x00, // L"K_?3D",				// &H3D
	0x00, // L"K_?3E",				// &H3E
	0x00, // L"K_?3F",				// &H3F
	0x00, // L"K_?40",				// &H40
	0x1E, // L"K_A",					// &H41
	0x30, // L"K_B",					// &H42
	0x2E, // L"K_C",					// &H43
	0x20, // L"K_D",					// &H44
	0x12, // L"K_E",					// &H45
	0x21, // L"K_F",					// &H46
	0x22, // L"K_G",					// &H47
	0x23, // L"K_H",					// &H48
	0x17, // L"K_I",					// &H49
	0x24, // L"K_J",					// &H4A
	0x25, // L"K_K",					// &H4B
	0x26, // L"K_L",					// &H4C
	0x32, // L"K_M",					// &H4D
	0x31, // L"K_N",					// &H4E
	0x18, // L"K_O",					// &H4F
	0x19, // L"K_P",					// &H50
	0x10, // L"K_Q",					// &H51
	0x13, // L"K_R",					// &H52
	0x1F, // L"K_S",					// &H53
	0x14, // L"K_T",					// &H54
	0x16, // L"K_U",					// &H55
	0x2F, // L"K_V",					// &H56
	0x11, // L"K_W",					// &H57
	0x2D, // L"K_X",					// &H58
	0x15, // L"K_Y",					// &H59
	0x2C, // L"K_Z",					// &H5A
	0x5B, // L"K_?5B",				// &H5B
	0x5C, // L"K_?5C",				// &H5C
	0x5D, // L"K_?5D",				// &H5D
	0x00, // L"K_?5E",				// &H5E
	0x5F, // L"K_?5F",				// &H5F
	0x52, // L"K_NP0",				// &H60
	0x4F, // L"K_NP1",				// &H61
	0x50, // L"K_NP2",				// &H62
	0x51, // L"K_NP3",				// &H63
	0x4B, // L"K_NP4",				// &H64
	0x4C, // L"K_NP5",				// &H65
	0x4D, // L"K_NP6",				// &H66
	0x47, // L"K_NP7",				// &H67
	0x48, // L"K_NP8",				// &H68
	0x49, // L"K_NP9",				// &H69
	0x37, // L"K_NPSTAR",			// &H6A
	0x4E, // L"K_NPPLUS",			// &H6B
	0x7E, // L"K_SEPARATOR",	// &H6C		// MCD 01-11-02: Brazilian Fix, 00 -> 7E
	0x4A, // L"K_NPMINUS",		// &H6D
	0x53, // L"K_NPDOT",			// &H6E
	0x135, // L"K_NPSLASH",		// &H6F
	0x3B, // L"K_F1",					// &H70
	0x3C, // L"K_F2",					// &H71
	0x3D, // L"K_F3",					// &H72
	0x3E, // L"K_F4",					// &H73
	0x3F, // L"K_F5",					// &H74
	0x40, // L"K_F6",					// &H75
	0x41, // L"K_F7",					// &H76
	0x42, // L"K_F8",					// &H77
	0x43, // L"K_F9",					// &H78
	0x44, // L"K_F10",				// &H79
	0x57, // L"K_F11",				// &H7A
	0x58, // L"K_F12",				// &H7B
	0x64, // L"K_F13",				// &H7C
	0x65, // L"K_F14",				// &H7D
	0x66, // L"K_F15",				// &H7E
	0x67, // L"K_F16",				// &H7F
	0x68, // L"K_F17",				// &H80
	0x69, // L"K_F18",				// &H81
	0x6A, // L"K_F19",				// &H82
	0x6B, // L"K_F20",				// &H83
	0x6C, // L"K_F21",				// &H84
	0x6D, // L"K_F22",				// &H85
	0x6E, // L"K_F23",				// &H86
	0x76, // L"K_F24",				// &H87

	0x00, // L"K_?88",				// &H88
	0x00, // L"K_?89",				// &H89
	0x00, // L"K_?8A",				// &H8A
	0x00, // L"K_?8B",				// &H8B
	0x00, // L"K_?8C",				// &H8C
	0x00, // L"K_?8D",				// &H8D
	0x00, // L"K_?8E",				// &H8E
	0x00, // L"K_?8F",				// &H8F

	0x45, // L"K_NUMLOCK",		// &H90
	0x46, // L"K_SCROL0x00, 	// &H91

	0x00, // L"K_?92",				// &H92
	0x00, // L"K_?93",				// &H93
	0x00, // L"K_?94",				// &H94
	0x00, // L"K_?95",				// &H95
	0x00, // L"K_?96",				// &H96
	0x00, // L"K_?97",				// &H97
	0x00, // L"K_?98",				// &H98
	0x00, // L"K_?99",				// &H99
	0x00, // L"K_?9A",				// &H9A
	0x00, // L"K_?9B",				// &H9B
	0x00, // L"K_?9C",				// &H9C
	0x00, // L"K_?9D",				// &H9D
	0x00, // L"K_?9E",				// &H9E
	0x00, // L"K_?9F",				// &H9F
	0x2A, // L"K_?A0",				// &HA0
	0x36, // L"K_?A1",				// &HA1
	0x1D, // L"K_?A2",				// &HA2
	0x1D, // L"K_?A3",				// &HA3
	0x38, // L"K_?A4",				// &HA4
	0x38, // L"K_?A5",				// &HA5
	0x6A, // L"K_?A6",				// &HA6
	0x69, // L"K_?A7",				// &HA7
	0x67, // L"K_?A8",				// &HA8
	0x68, // L"K_?A9",				// &HA9
	0x65, // L"K_?AA",				// &HAA
	0x66, // L"K_?AB",				// &HAB
	0x32, // L"K_?AC",				// &HAC
	0x20, // L"K_?AD",				// &HAD
	0x2E, // L"K_?AE",				// &HAE
	0x30, // L"K_?AF",				// &HAF
	0x19, // L"K_?B0",				// &HB0
	0x10, // L"K_?B1",				// &HB1
	0x24, // L"K_?B2",				// &HB2
	0x22, // L"K_?B3",				// &HB3
	0x6C, // L"K_?B4",				// &HB4
	0x6D, // L"K_?B5",				// &HB5
	0x6B, // L"K_?B6",				// &HB6
	0x21, // L"K_?B7",				// &HB7
	0x00, // L"K_?B8",				// &HB8
	0x00, // L"K_?B9",				// &HB9
	0x27, // L"K_COLON",			// &HBA
	0x0D, // L"K_EQUA0x00, // L",	// &HBB
	0x33, // L"K_COMMA",			// &HBC
	0x0C, // L"K_HYPHEN",			// &HBD
	0x34, // L"K_PERIOD",			// &HBE
	0x35, // L"K_SLASH",			// &HBF
	0x29, // L"K_BKQUOTE",			// &HC0

	0x73, // L"K_?C1",				// &HC1
	0x7E, // L"K_?C2",				// &HC2
	0x00, // L"K_?C3",				// &HC3
	0x00, // L"K_?C4",				// &HC4
	0x00, // L"K_?C5",				// &HC5
	0x00, // L"K_?C6",				// &HC6
	0x00, // L"K_?C7",				// &HC7
	0x00, // L"K_?C8",				// &HC8
	0x00, // L"K_?C9",				// &HC9
	0x00, // L"K_?CA",				// &HCA
	0x00, // L"K_?CB",				// &HCB
	0x00, // L"K_?CC",				// &HCC
	0x00, // L"K_?CD",				// &HCD
	0x00, // L"K_?CE",				// &HCE
	0x00, // L"K_?CF",				// &HCF
	0x00, // L"K_?D0",				// &HD0
	0x00, // L"K_?D1",				// &HD1
	0x00, // L"K_?D2",				// &HD2
	0x00, // L"K_?D3",				// &HD3
	0x00, // L"K_?D4",				// &HD4
	0x00, // L"K_?D5",				// &HD5
	0x00, // L"K_?D6",				// &HD6
	0x00, // L"K_?D7",				// &HD7
	0x00, // L"K_?D8",				// &HD8
	0x00, // L"K_?D9",				// &HD9
	0x00, // L"K_?DA",				// &HDA
	0x1A, // L"K_LBRKT",			// &HDB
	0x2B, // L"K_BKSLASH",			// &HDC
	0x1B, // L"K_RBRKT",			// &HDD
	0x28, // L"K_QUOTE",			// &HDE
	0x73, // L"K_oDF",				// &HDF			// MCD 01-11-02: Brazilian fix: 00 -> 73
	0x00, // L"K_oE0",				// &HE0
	0x00, // L"K_oE1",				// &HE1
	0x56, // L"K_oE2",				// &HE2
	0x00, // L"K_oE3",				// &HE3
	0x00, // L"K_oE4",				// &HE4

	0x00, // L"K_?E5",				// &HE5

	0x00, // L"K_oE6",				// &HE6

	0x00, // L"K_?E7",				// &HE7
	0x00, // L"K_?E8",				// &HE8

	0x71, // L"K_oE9",				// &HE9
	0x5C, // L"K_oEA",				// &HEA
	0x7B, // L"K_oEB",				// &HEB
	0x00, // L"K_oEC",				// &HEC
	0x6F, // L"K_oED",				// &HED
	0x5A, // L"K_oEE",				// &HEE
	0x00, // L"K_oEF",				// &HEF
	0x00, // L"K_oF0",				// &HF0
	0x5B, // L"K_oF1",				// &HF1
	0x00, // L"K_oF2",				// &HF2
	0x5F, // L"K_oF3",				// &HF3
	0x00, // L"K_oF4",				// &HF4
	0x5E, // L"K_oF5",				// &HF5

	0x00, // L"K_?F6",				// &HF6
	0x00, // L"K_?F7",				// &HF7
	0x00, // L"K_?F8",				// &HF8
	0x5D, // L"K_?F9",				// &HF9
	0x00, // L"K_?FA",				// &HFA
	0x62, // L"K_?FB",				// &HFB
	0x00, // L"K_?FC",				// &HFC
	0x00, // L"K_?FD",				// &HFD
	0x00, // L"K_?FE",				// &HFE
	0x00  // L"K_?FF"				// &HFF
};

const UINT ScanCodeToUSVirtualKey[128] = {
  0x01, // 0x00 => K_LBUTTON
  0x1b, // 0x01 => K_ESC
  0x31, // 0x02 => K_1
  0x32, // 0x03 => K_2
  0x33, // 0x04 => K_3
  0x34, // 0x05 => K_4
  0x35, // 0x06 => K_5
  0x36, // 0x07 => K_6
  0x37, // 0x08 => K_7
  0x38, // 0x09 => K_8
  0x39, // 0x0a => K_9
  0x30, // 0x0b => K_0
  0xbd, // 0x0c => K_HYPHEN
  0xbb, // 0x0d => K_EQUAL
  0x08, // 0x0e => K_BKSP
  0x09, // 0x0f => K_TAB
  0x51, // 0x10 => K_Q
  0x57, // 0x11 => K_W
  0x45, // 0x12 => K_E
  0x52, // 0x13 => K_R
  0x54, // 0x14 => K_T				20
  0x59, // 0x15 => K_Y
  0x55, // 0x16 => K_U
  0x49, // 0x17 => K_I
  0x4f, // 0x18 => K_O
  0x50, // 0x19 => K_P
  0xdb, // 0x1a => K_LBRKT
  0xdd, // 0x1b => K_RBRKT
  0x0d, // 0x1c => K_ENTER
  0x11, // 0x1d => K_CONTROL
  0x41, // 0x1e => K_A
  0x53, // 0x1f => K_S
  0x44, // 0x20 => K_D
  0x46, // 0x21 => K_F
  0x47, // 0x22 => K_G
  0x48, // 0x23 => K_H
  0x4a, // 0x24 => K_J
  0x4b, // 0x25 => K_K
  0x4c, // 0x26 => K_L
  0xba, // 0x27 => K_COLON    		39
  0xde, // 0x28 => K_QUOTE
  0xc0, // 0x29 => K_BKQUOTE
  0x10, // 0x2a => K_SHIFT
  0xdc, // 0x2b => K_BKSLASH
  0x5a, // 0x2c => K_Z
  0x58, // 0x2d => K_X
  0x43, // 0x2e => K_C
  0x56, // 0x2f => K_V
  0x42, // 0x30 => K_B
  0x4e, // 0x31 => K_N
  0x4d, // 0x32 => K_M
  0xbc, // 0x33 => K_COMMA
  0xbe, // 0x34 => K_PERIOD
  0xbf, // 0x35 => K_SLASH
  0xa1, // 0x36 => K_?A1
  0x6a, // 0x37 => K_NPSTAR
  0x12, // 0x38 => K_ALT
  0x20, // 0x39 => K_SPACE
  0x14, // 0x3a => K_CAPS
  0x70, // 0x3b => K_F1 			59
  0x71, // 0x3c => K_F2
  0x72, // 0x3d => K_F3
  0x73, // 0x3e => K_F4
  0x74, // 0x3f => K_F5
  0x75, // 0x40 => K_F6
  0x76, // 0x41 => K_F7
  0x77, // 0x42 => K_F8
  0x78, // 0x43 => K_F9
  0x79, // 0x44 => K_F10
  0x90, // 0x45 => K_NUMLOCK
  0x03, // 0x46 => K_CANCEL
  0x24, // 0x47 => K_HOME
  0x26, // 0x48 => K_UP
  0x21, // 0x49 => K_PGUP
  0x6d, // 0x4a => K_NPMINUS
  0x25, // 0x4b => K_LEFT
  0x0c, // 0x4c => K_KP5
  0x27, // 0x4d => K_RIGHT
  0x6b, // 0x4e => K_NPPLUS
  0x23, // 0x4f => K_END
  0x28, // 0x50 => K_DOWN
  0x22, // 0x51 => K_PGDN
  0x2d, // 0x52 => K_INS
  0x2e, // 0x53 => K_DEL
  0x2c, // 0x54 => K_PRTSCN
  0x00, // 0x55 => No match
  0xe2, // 0x56 => K_oE2
  0x7a, // 0x57 => K_F11
  0x7b, // 0x58 => K_F12
  0x00, // 0x59 => No match
  0xee, // 0x5a => K_oEE
  0x5b, // 0x5b => K_?5B
  0x5c, // 0x5c => K_?5C
  0x5d, // 0x5d => K_?5D
  0xf5, // 0x5e => K_oF5
  0x5f, // 0x5f => K_?5F
  0x00, // 0x60 => No match
  0x00, // 0x61 => No match
  0xfb, // 0x62 => K_?FB
  0x2f, // 0x63 => K_HELP
  0x7c, // 0x64 => K_F13
  0x7d, // 0x65 => K_F14
  0x7e, // 0x66 => K_F15
  0x7f, // 0x67 => K_F16
  0x80, // 0x68 => K_F17
  0x81, // 0x69 => K_F18
  0x82, // 0x6a => K_F19
  0x83, // 0x6b => K_F20
  0x84, // 0x6c => K_F21
  0x85, // 0x6d => K_F22
  0x86, // 0x6e => K_F23
  0xed, // 0x6f => K_oED
  0x00, // 0x70 => No match
  0xe9, // 0x71 => K_oE9
  0x00, // 0x72 => No match
  0xc1, // 0x73 => K_?C1
  0x00, // 0x74 => No match
  0x00, // 0x75 => No match
  0x87, // 0x76 => K_F24
  0x00, // 0x77 => No match
  0x00, // 0x78 => No match
  0x00, // 0x79 => No match
  0x00, // 0x7a => No match
  0xeb, // 0x7b => K_oEB
  0x00, // 0x7c => No match
  0x00, // 0x7d => No match
  0x6c, // 0x7e => K_SEPARATOR
  0x00  // 0x7f => No match
};

// _S2 many keys still need to be defined !!  
//( on x. place there is key e.g. 
// on (US) position 24 we find key xBB ( = + ) 
// on (US) position  0 we find key x41 ( A a )
const UINT mac_ScanCodeToUSVirtualKey[128] = {
	0x41	,//	L"K_A",	//	&H41
	0x53	,//	L"K_S",	//	&H53
	0x44	,//	L"K_D",	//	&H44
	0x46	,//	L"K_F",	//	&H46
	0x48	,//	L"K_H",	//	&H48
	0x47	,//	L"K_G",	//	&H47
	0x5A	,//	L"K_Z",	//	&H5A
	0x58	,//	L"K_X",	//	&H58
	0x43	,//	L"K_C",	//	&H43
	0x56	,//	L"K_V",	//	&H56
0xC0,			//  ^° 									// 10
	0x42	,//	L"K_B",	//	&H42
	0x51	,//	L"K_Q",	//	&H51
	0x57	,//	L"K_W",	//	&H57
	0x45	,//	L"K_E",	//	&H45
	0x52	,//	L"K_R",	//	&H52
	0x59	,//	L"K_Y",	//	&H59
	0x54	,//	L"K_T",	//	&H54
	0x31	,//	L"K_1",	//	&H31
	0x32	,//	L"K_2",	//	&H32
0x33	,//	L"K_3",	//	&H33				//  20
	0x34	,//	L"K_4",	//	&H34
	0x36	,//	L"K_6",	//	&H36
	0x35	,//	L"K_5",	//	&H35
	0xBB  ,//	L"K_?00",//	&HB8
	0x39	,//	L"K_9",	//	&H39
	0x37	,//	L"K_7",	//	&H37
	0xBD	,//	L"K_?00",//	&HBA
	0x38	,//	L"K_8",	//	&H38
	0x30	,//	L"K_0",	//	&H30
0xDD	,//	0	//	&HD9								//  30
	0x4F	,//	L"K_O",	//	&H4F
	0x55	,//	L"K_U",	//	&H55
	0xDB	,  //	0	//	&HD7
	0x49	,//	L"K_I",	//	&H49
	0x50	,//	L"K_P",	//	&H50
	0x00,
	0x4C	,//	L"K_L",	//	&H4C
	0x4A	,//	L"K_J",	//	&H4A
	0xDE,	//222	,//	0	//	&HDA
0x4B	,//	L"K_K",	//	&H4B				//  40
	186	,//	L"K_?00",	//	&HB7
	220	,//	0	//	&HD8
	188	,//	L"K_?00",	//	&HB9
	191	,//	L"K_?00",	//	&HBC
	0x4E	,//	L"K_N",	//	&H4E
	0x4D	,//	L"K_M",	//	&H4D
	190	,//	L"K_?00",	//	L",
	0	,//	L"K_?00",	//	&H0   // 0x77 => No match
	32	,//	L"K_?00",	//	&H1
0xE2	,//	L"K_?00",	//	&H2				//  50																// NEU    xxx
	3	,//	L"K_?00",	//	&H3
	4	,//	L"K_?00",	//	&H4
	5	,//	L"K_?00",	//	&H5
	6	,//	L"K_?00",	//	&H6
	7	,//	L"K_?00",	//	&H7
	8	,//	L"K_?00",	//	&H8
	9	,//	L"K_?00",	//	&H9
	10	,//	L"K_?00",	//	&HA
	11	,//	L"K_?00",	//	&HB
12	,//	L"K_?00",	//	&HC				  //  60
	13	,//	L"K_?00",	//	&HD
	14	,//	L"K_?00",	//	&HE
	15	,//	L"K_?00",	//	&HF
	16	,//	L"K_?00",	//	&H10
	17	,//	L"K_?00",	//	L",
	18	,//	L"K_?00",	//	&H12
	19	,//	L"K_?00",	//	&H13
	20	,//	L"K_?00",	//	&H14
	21	,//	L"K_?00",	//	&H15
	22	,//	L"K_?00",	//	&H16
	23	,//	L"K_?00",	//	&H17
	24	,//	L"K_?00",	//	&H18
	25	,//	L"K_?00",	//	&H19
	26	,//	L"K_?00",	//	&H1A
	27	,//	L"K_?00",	//	&H1B
	28	,//	L"K_?00",	//	&H1C
	29	,//	L"K_?00",	//	&H1D
	30	,//	L"K_?00",	//	&H1E
	31	,//	L"K_?00",	//	&H1F
	32	,//	L"K_?00",	//	&H20
	33	,//	L"K_?00",	//	&H21
	34	,//	L"K_?00",	//	&H22
	35	,//	L"K_?00",	//	&H23
	36	,//	L"K_?00",	//	&H24
	37	,//	L"K_?00",	//	&H25
	38	,//	L"K_?00",	//	&H26
	39	,//	L"K_?00",	//	&H27
	40	,//	L"K_?00",	//	&H28
	41	,//	L"K_?00",	//	&H29
	42	,//	L"K_?00",	//	&H2A
	43	,//	L"K_?00",	//	&H2B
	44	,//	L"K_?00",	//	&H2C
	45	,//	L"K_?00",	//	&H2D
	46	,//	L"K_?00",	//	&H2E
	47	,//	L"K_?00",	//	&H2F
	58	,//	L"K_?00",	//	&H3A
	59	,//	L"K_?00",	//	&H3B
	60	,//	L"K_?00",	//	&H3C
	61	,//	L"K_?00",	//	&H3D
	62	,//	L"K_?00",	//	&H3E
	63	,//	L"K_?00",	//	&H3F
	64	,//	L"K_?00",	//	&H40
	91	,//	L"K_?00",	//	&H5B
	92	,//	L"K_?00",	//	&H5C
	93	,//	L"K_?00",	//	&H5D
	94	,//	L"K_?00",	//	&H5E
	95	,//	L"K_?00",	//	&H5F
	96	,//	L"K_?00",	//	&H60
	97	,//	L"K_?00",	//	&H61
	98	,//	L"K_?00",	//	&H62
	99	,//	L"K_?00",	//	&H63
	100	,//	L"K_?00",	//	&H64
	101	,//	L"K_?00",	//	&H65
	102	,//	L"K_?00",	//	&H66
	103	,//	L"K_?00",	//	&H67
	104	,//	L"K_?00",	//	&H68
	105	,//	L"K_?00",	//	&H69
	106	,//	L"K_?00",	//	&H6A
	107	,//	L"K_?00",	//	&H6B
	108	,//	L"K_?00",	//	&H6C
	109	,//	L"K_?00",	//	&H6D
	110	,//	L"K_?00",	//	&H6E
	111	,//	L"K_?00",	//	&H6F
	112	,//	L"K_?00",	//	&H70
	113	,//	L"K_?00",	//	&H71
	114	,//	L"K_?00",	//	&H72
	115	//	L"K_?00",	//	&H73
};

// _S2 what instead x999?
//( on character-.st index we find the keycode e.g.
// for character A  (65) we look at pos  65 and find keycode 0
// for character X  (87) we look at pos  87 and find keycode 7
// for character + (187) we look at pos 187 and find keycode 24

const UINT mac_USVirtualKeyToScanCode[256] = {
		0x999, // L"K_?00",				// &H0 ................................................... 0 ...........................................
		0x999, // L"K_LBUTTON",			// &H1
		0x999, // L"K_RBUTTON",			// &H2
		0x999, //										0x46, // L"K_CANCEL",		   	// &H3
		0x999, // L"K_MBUTTON",			// &H4
		0x999, // L"K_?05",				// &H5
		0x999, // L"K_?06",				// &H6
		0x999, // L"K_?07",				// &H7
		0x999, //										0x0E, // L"K_BKSP",	    		// &H8
		0x999, //										0x0F, // L"K_TAB",	    		// &H9
	0x999, // L"K_?0A",				// &HA
		0x999, // L"K_?0B",				// &HB................................................... 11 ...........................................
		0x999, //										0x4C, // L"K_KP5",		    	// &HC
		0x999, //										0x1C, // L"K_ENTER",				// &HD
		0x999, // L"K_?0E",				// &HE
		0x999, // L"K_?0F",				// &HF
		0x999, //										0x2A, // L"K_SHIFT",				// &H10
		0x999, //										0x1D, // L"K_CONTRO   0x999, // L",			// &H11
		0x999, //										0x38, // L"K_ALT",				// &H12
		0x999, // L"K_PAUSE",				// &H13
	0x999, //										0x3A, // L"K_CAPS",				// &H14
		0x999, // L"K_KANJI?15",			// &H15
		0x999, // L"K_KANJI?16",			// &H16
		0x999, // L"K_KANJI?17",			// &H17
		0x999, // L"K_KANJI?18",			// &H18
		0x999, // L"K_KANJI?19",			// &H19
		0x999, // L"K_?1A",				// &H1A
		0x999, // L"K_ESC",				// &H1B
		0x999, // L"K_KANJI?1C",			// &H1C
		0x999, // L"K_KANJI?1D",			// &H1D
0x999, // L"K_KANJI?1E",			// &H1E
		0x999, // L"K_KANJI?1F",			// &H1F................................................... 31 ...........................................
		0x31, // L"K_SPACE",				// &H20
		0x999, //										0x49, // L"K_PGUP",				// &H21
		0x999, //										0x51, // L"K_PGDN",				// &H22
		0x999, //										0x4F, // L"K_END",				// &H23
		0x999, //										0x47, // L"K_HOME",				// &H24
		0x999, //										0x4B, // L"K_LEFT",				// &H25
		0x999, //										0x48, // L"K_UP",				// &H26
		0x999, //										0x4D, // L"K_RIGHT",				// &H27
0x999, //										0x50, // L"K_DOWN",				// &H28
		0x999, // L"K_SEL",				// &H29
		0x999, // L"K_PRINT",				// &H2A
		0x999, // L"K_EXEC",				// &H2B
		0x999, //										0x54, // L"K_PRTSCN",			// &H2C
		0x999, //										0x52, // L"K_INS",				// &H2D
		0x999, //										0x53, // L"K_DEL",				// &H2E
		0x999, //										0x63, // L"K_HELP",				// &H2F
	0x1D, // L"K_0",					// &H30
	0x12, // L"K_1",					// &H31
0x13, // L"K_2",					// &H32
	0x14, // L"K_3",					// &H33................................................... 51 ...........................................
	0x15, // L"K_4",					// &H34
	0x17, // L"K_5",					// &H35
	0x16, // L"K_6",					// &H36
	0x1A, // L"K_7",					// &H37
	0x1C, // L"K_8",					// &H38
	0x19, // L"K_9",					// &H39
		0x999, // L"K_?3A",				// &H3A
		0x999, // L"K_?3B",				// &H3B
0x999, // L"K_?3C",				// &H3C
		0x999, // L"K_?3D",				// &H3D
		0x999, // L"K_?3E",				// &H3E
		0x999, // L"K_?3F",				// &H3F
		0x999, // L"K_?40",				// &H40
	0x00, // L"K_A",					// &H41
	0x0B, // L"K_B",					// &H42
	0x08, // L"K_C",					// &H43
	0x02, // L"K_D",					// &H44
	0x0E, // L"K_E",					// &H45
0x03, // L"K_F",					// &H46
	0x05, // L"K_G",					// &H47................................................... 71 ...........................................
	0x04, // L"K_H",					// &H48
	0x22, // L"K_I",					// &H49
	0x26, // L"K_J",					// &H4A
	0x28, // L"K_K",					// &H4B
	0x25, // L"K_L",					// &H4C
	0x2E, // L"K_M",					// &H4D
	0x2D, // L"K_N",					// &H4E
	0x1F, // L"K_O",					// &H4F
0x23, // L"K_P",					// &H50
	0x0C, // L"K_Q",					// &H51
	0x0F, // L"K_R",					// &H52
	0x01, // L"K_S",					// &H53
	0x11, // L"K_T",					// &H54
	0x20, // L"K_U",					// &H55
	0x09, // L"K_V",					// &H56
	0x0D, // L"K_W",					// &H57
	0x07, // L"K_X",					// &H58
	0x10, // L"K_Y",					// &H59................................................... 89 ...........................................
0x06, // L"K_Z",					// &H5A
		0x999,      //0x5B, // L"K_?5B",				// &H5B
		0x999,      //0x5C, // L"K_?5C",				// &H5C
		0x999,      //0x5D, // L"K_?5D",				// &H5D
		0x999,      //0x00, // L"K_?5E",				// &H5E
		0x999,      //0x5F, // L"K_?5F",				// &H5F
		0x999,      //0x52, // L"K_NP0",				// &H60
		0x999,      //0x4F, // L"K_NP1",				// &H61
		0x999,      //0x50, // L"K_NP2",				// &H62
		0x999,      //0x51, // L"K_NP3",				// &H63
	0x999,      //0x4B, // L"K_NP4",				// &H64
		0x999,      //0x4C, // L"K_NP5",				// &H65
		0x999,      //0x4D, // L"K_NP6",				// &H66
		0x999,      //0x47, // L"K_NP7",				// &H67
		0x999,      //0x48, // L"K_NP8",				// &H68
		0x999,      //0x49, // L"K_NP9",				// &H69
		0x999,      //0x37, // L"K_NPSTAR",			// &H6A
		0x999,      //0x4E, // L"K_NPPLUS",			// &H6B
		0x999,      //0x7E, // L"K_SEPARATOR",			// &H6C		// MCD 01-11-02: Brazilian Fix, 00 -> 7E
		0x999,      //0x4A, // L"K_NPMINUS",			// &H6D
	0x999,      //0x53, // L"K_NPDOT",				// &H6E		// ................................................. 110 ...........
		0x999,      //0x135, // L"K_NPSLASH",			// &H6F
		0x999,      //0x3B, // L"K_F1",				// &H70
		0x999,      //0x3C, // L"K_F2",				// &H71
		0x999,      //0x3D, // L"K_F3",				// &H72
		0x999,      //0x3E, // L"K_F4",				// &H73
		0x999,      //0x3F, // L"K_F5",				// &H74
		0x999,      //0x40, // L"K_F6",				// &H75
		0x999,      //0x41, // L"K_F7",				// &H76
		0x999,      //0x42, // L"K_F8",				// &H77
	0x999,      //0x43, // L"K_F9",				// &H78
		0x999,      //0x44, // L"K_F10",				// &H79
		0x999,      //0x57, // L"K_F11",				// &H7A
		0x999,      //0x58, // L"K_F12",				// &H7B
		0x999,      //0x64, // L"K_F13",				// &H7C
		0x999,      //0x65, // L"K_F14",				// &H7D
		0x999,      //0x66, // L"K_F15",				// &H7E
		0x999,      //0x67, // L"K_F16",				// &H7F
		0x999,      //0x68, // L"K_F17",				// &H80.....
		0x999,      //0x69, // L"K_F18",				// &H81
	0x999,      //0x6A, // L"K_F19",				// &H82.................................................. 130 .......................................
		0x999,      //0x6B, // L"K_F20",				// &H83
		0x999,      //0x6C, // L"K_F21",				// &H84
		0x999,      //0x6D, // L"K_F22",				// &H85
		0x999,      //0x6E, // L"K_F23",				// &H86
		0x999,      //0x76, // L"K_F24",				// &H87  -----
		0x999,      //0x00, // L"K_?88",				// &H88
		0x999,      //0x00, // L"K_?89",				// &H89
		0x999,      //0x00, // L"K_?8A",				// &H8A
		0x999,      //0x00, // L"K_?8B",				// &H8B
	0x999,      //0x00, // L"K_?8C",				// &H8C
		0x999,      //0x00, // L"K_?8D",				// &H8D
		0x999,      //0x00, // L"K_?8E",				// &H8E
		0x999,      //0x00, // L"K_?8F",				// &H8F -----
		0x999,      //0x45, // L"K_NUMLOCK",			// &H90
		0x999,      //0x46, // L"K_SCROL	0x999,      //0x00, // L",			// &H91 -----
		0x999,      //0x00, // L"K_?92",				// &H92
		0x999,      //0x00, // L"K_?93",				// &H93
		0x999,      //0x00, // L"K_?94",				// &H94.................
		0x999,      //0x00, // L"K_?95",				// &H95
	0x999,      //0x00, // L"K_?96",				// &H96............................................. 150 ................................
		0x999,      //0x00, // L"K_?97",				// &H97
		0x999,      //0x00, // L"K_?98",				// &H98
		0x999,      //0x00, // L"K_?99",				// &H99
		0x999,      //0x00, // L"K_?9A",				// &H9A
		0x999,      //0x00, // L"K_?9B",				// &H9B
		0x999,      //0x00, // L"K_?9C",				// &H9C
		0x999,      //0x00, // L"K_?9D",				// &H9D
		0x999,      //0x00, // L"K_?9E",				// &H9E
		0x999,      //0x00, // L"K_?9F",				// &H9F
	0x999,      //0x2A, // L"K_?A0",				// &HA0
		0x999,      //0x36, // L"K_?A1",				// &HA1
		0x999,      //0x1D, // L"K_?A2",				// &HA2
		0x999,      //0x1D, // L"K_?A3",				// &HA3
		0x999,      //0x38, // L"K_?A4",				// &HA4
		0x999,      //0x38, // L"K_?A5",				// &HA5
		0x999,      //0x6A, // L"K_?A6",				// &HA6
		0x999,      //0x69, // L"K_?A7",				// &HA7
		0x999,      //0x67, // L"K_?A8",				// &HA8......................
		0x999,      //0x68, // L"K_?A9",				// &HA9
	0x999,      //0x65, // L"K_?AA",				// &HAA....................................... 170 .................................
		0x999,      //0x66, // L"K_?AB",				// &HAB
		0x999,      //0x32, // L"K_?AC",				// &HAC
		0x999,      //0x20, // L"K_?AD",				// &HAD
		0x999,      //0x2E, // L"K_?AE",				// &HAE
		0x999,      //0x30, // L"K_?AF",				// &HAF
		0x999,      //0x19, // L"K_?B0",				// &HB0
		0x999,      //0x10, // L"K_?B1",				// &HB1
		0x999,      //0x24, // L"K_?B2",				// &HB2
		0x999,      //0x22, // L"K_?B3",				// &HB3
	0x999,      //0x6C, // L"K_?B4",				// &HB4
		0x999,      //0x6D, // L"K_?B5",				// &HB5
		0x999,      //0x6B, // L"K_?B6",				// &HB6
		0x999,      //0x21, // L"K_?B7",				// &HB7
		0x999,      //0x00, // L"K_?B8",				// &HB8
		0x999,      //0x00, // L"K_?B9",				// &HB9 -----
	0x29, // L"K_COLON",				// &HBA
		24, // 0x0D, // L"K_EQUA0x00, // L",				// &HBB						187
	0x2B, // L"K_COMMA",				// &HBC ........
	0x1B, // L"K_HYPHEN",			// &HBD
0x2F, // L"K_PERIOD",			// &HBE................................................ 190 ......................................
	0x2C, // L"K_SLASH",				// &HBF
	 0x0A,      //0x77, // L"K_BKQUOTE",			// &HC0 -----
		0x999,      //0x73, // L"K_?C1",				// &HC1
		0x999,      //0x7E, // L"K_?C2",				// &HC2
		0x999,      //0x00, // L"K_?C3",				// &HC3
		0x999,      //0x00, // L"K_?C4",				// &HC4		196
		0x999,      //0x00, // L"K_?C5",				// &HC5
		0x999,      //0x00, // L"K_?C6",				// &HC6
		0x999,      //0x00, // L"K_?C7",				// &HC7
	0x999,      //0x00, // L"K_?C8",				// &HC8
		0x999,      //0x00, // L"K_?C9",				// &HC9
		0x999,      //0x00, // L"K_?CA",				// &HCA
		0x999,      //0x00, // L"K_?CB",				// &HCB
		0x999,      //0x00, // L"K_?CC",				// &HCC
		0x999,      //0x00, // L"K_?CD",				// &HCD
		0x999,      //0x00, // L"K_?CE",				// &HCE
		0x999,      //0x00, // L"K_?CF",				// &HCF
		0x999,      //0x00, // L"K_?D0",				// &HD0...............
		0x999,      //0x00, // L"K_?D1",				// &HD1
	0x999,      //0x00, // L"K_?D2",				// &HD2............................................... 210 ................................
		0x999,      //0x00, // L"K_?D3",				// &HD3
		0x999,      //0x00, // L"K_?D4",				// &HD4
		0x999,      //0x00, // L"K_?D5",				// &HD5
		0x999,      //0x00, // L"K_?D6",				// &HD6
		0x999,      //0x00, // L"K_?D7",				// &HD7
		0x999,      //0x00, // L"K_?D8",				// &HD8
		0x999,      //0x00, // L"K_?D9",				// &HD9
		0x999,      //0x00, // L"K_?DA",				// &HDA
		0x21, // L"K_LBRKT",				// &HDB
	0x2A, // L"K_BKSLASH",			// &HDC
		0x1E, // L"K_RBRKT",				// &HDD
		0x27, // L"K_QUOTE",				// &HDE
		0x32, // L"K_oDF",				// &HDF			// MCD 01-11-02: Brazilian fix: 00 -> 73
		0x999,      //0x00, // L"K_oE0",				// &HE0
		0x999,      //0x00, // L"K_oE1",				// &HE1
		50,      //0x56, // L"K_oE2",				// &HE2
		0x999,      //0x00, // L"K_oE3",				// &HE3
		0x999,      //0x00, // L"K_oE4",				// &HE4..... 
		0x999,      //0x00, // L"K_?E5",				// &HE5
0x999,      //0x00, // L"K_oE6",				// &HE6.................................................. 230 .......................................
		0x999,      //0x00, // L"K_?E7",				// &HE7
		0x999,      //0x00, // L"K_?E8",				// &HE8		222
		0x999,      //0x71, // L"K_oE9",				// &HE9
		0x999,      //0x5C, // L"K_oEA",				// &HEA
		0x999,      //0x7B, // L"K_oEB",				// &HEB
		0x999,      //0x00, // L"K_oEC",				// &HEC  226
		0x999,      //0x6F, // L"K_oED",				// &HED
		0x999,      //0x5A, // L"K_oEE",				// &HEE
		0x999,      //0x00, // L"K_oEF",				// &HEF
	0x999,      //0x00, // L"K_oF0",				// &HF0
		0x999,      //0x5B, // L"K_oF1",				// &HF1
		0x999,      //0x00, // L"K_oF2",				// &HF2
		0x999,      //0x5F, // L"K_oF3",				// &HF3
		0x999,      //0x00, // L"K_oF4",				// &HF4
		0x999,      //0x5E, // L"K_oF5",				// &HF5
		0x999,      //0x00, // L"K_?F6",				// &HF6
		0x999,      //0x00, // L"K_?F7",				// &HF7
		0x999,      //0x00, // L"K_?F8",				// &HF8
		0x999,      //0x5D, // L"K_?F9",				// &HF9
	0x999,      //0x00, // L"K_?FA",				// &HFA
		0x999,      //0x62, // L"K_?FB",				// &HFB
		0x999,      //0x00, // L"K_?FC",				// &HFC
		0x999,      //0x00, // L"K_?FD",				// &HFD
		0x999,      //0x00, // L"K_?FE",				// &HFE
		0x999,      //0x00  // L"K_?FF"				// &HFF
};

bool mac_IsKeymanUsedChar(int KV);
//------------------------------

// take deadkey-value (e.g.65106) and return u16string (e.g. '^' )
std::u16string mac_convert_DeadkeyValues_To_U16str(int in);

// use gdk_keymap_translate_keyboard_state to get keyval - base function to get keyvals
// _S2 can I use mac_KMX_get_KeyVal_From_KeyCode_dk only?
int mac_KMX_get_KeyVal_From_KeyCode(const UCKeyboardLayout * keyboard_layout, int keycode, int ss, int caps);
int mac_KMX_get_KeyVal_From_KeyCode_dk(const UCKeyboardLayout * keyboard_layout, int keycode, int shiftstate, int caps, UInt32 &deadkeystate_ret);

// use mac_KMX_get_KeyVal_From_KeyCode and prevent use of certain keycodes
KMX_DWORD mac_KMX_get_KeyValUnderlying_From_KeyCodeUnderlying(const UCKeyboardLayout * keyboard_layout, int keycode, int shift_state_pos);

// fill Deadkey with dk and return CATEGORY
KMX_DWORD mac_KMX_get_KeyValUnderlying_From_KeyCodeUnderlying(const UCKeyboardLayout * keyboard_layout, UINT VKShiftState, UINT KC_underlying, PKMX_WCHAR DeadKey);

// use Vector to return the Keyval of underlying Keyboard
KMX_WCHAR mac_KMX_get_KeyValUnderlying_From_KeyValUS(v_dw_3D &All_Vector,KMX_DWORD VK_underlying);

// use Vector to return the Scancode of underlying Keyboard
KMX_WCHAR mac_KMX_get_KeyCodeUnderlying_From_KeyValUnderlying(v_dw_3D & All_Vector, KMX_DWORD KV_Underlying);

// use Vector to return the Keycode of the underlying Keyboard for given VK_US using GDK
KMX_DWORD mac_KMX_get_KeyCodeUnderlying_From_KeyCodeUS(const UCKeyboardLayout * keyboard_layout, v_dw_3D &All_Vector,KMX_DWORD KC_US, ShiftState ss, int caps);

// return the Keycode of the underlying Keyboard for given VK_US
UINT mac_KMX_get_KeyCodeUnderlying_From_VKUS(KMX_DWORD VirtualKeyUS);

// return the VirtualKey of the US Keyboard for a given Keyode using GDK
KMX_DWORD mac_KMX_get_VKUS_From_KeyCodeUnderlying(KMX_DWORD keycode);

// convert codePoint to u16string
std::u16string mac_CodePointToU16String(unsigned int codepoint);

//################################################################################################################################################
//################################################################################################################################################

// _S2 need to go

void printoutKeyboards(v_dw_3D &All_Vector);
KMX_DWORD  X_playWithDK(int shiftstate,const UCKeyboardLayout* keyboard_layout , KMX_DWORD charVal) ;
KMX_DWORD  X_playWithDK_one(int shiftstate,const UCKeyboardLayout* keyboard_layout , KMX_DWORD charVal);
KMX_DWORD X_compare_Shiftstates(int shiftstate,const UCKeyboardLayout* keyboard_layout , KMX_DWORD charVal=0);
KMX_DWORD X_find_Shiftstates(int shiftstate,const UCKeyboardLayout* keyboard_layout , KMX_DWORD charVal=0);
KMX_DWORD  printout_dk(const UCKeyboardLayout* keyboard_layout);/**/
#endif  // KEYMAP_H