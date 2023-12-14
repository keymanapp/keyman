
#include "kmx_processevent.h"

namespace km {
namespace core {
namespace kmx {

const struct char_to_vkey s_char_to_vkey[] = {
  {KM_CORE_VKEY_SPACE, 0, 0},     //
  {'1', 1, 0},       // !
  {KM_CORE_VKEY_QUOTE, 1, 0},  // "
  {'3', 1, 0},       // #
  {'4', 1, 0},       // $
  {'5', 1, 0},       // %
  {'7', 1, 0},       // &
  {KM_CORE_VKEY_QUOTE, 0, 0},     // '
  {'9', 1, 0},       // (
  {'0', 1, 0},       // )
  {'8', 1, 0},       // *
  {KM_CORE_VKEY_EQUAL, 1, 0},  // +
  {KM_CORE_VKEY_COMMA, 0, 0},     // ,
  {KM_CORE_VKEY_HYPHEN, 0, 0},    // -
  {KM_CORE_VKEY_PERIOD, 0, 0},    // .
  {KM_CORE_VKEY_SLASH, 0, 0},     // /
  {'0', 0, 0},
  {'1', 0, 0},
  {'2', 0, 0},
  {'3', 0, 0},
  {'4', 0, 0},
  {'5', 0, 0},
  {'6', 0, 0},
  {'7', 0, 0},
  {'8', 0, 0},
  {'9', 0, 0},
  {KM_CORE_VKEY_COLON, 1, 0},  // :
  {KM_CORE_VKEY_COLON, 0, 0},     // ;
  {KM_CORE_VKEY_COMMA, 1, 0},  // <
  {KM_CORE_VKEY_EQUAL, 0, 0},     // =
  {KM_CORE_VKEY_PERIOD, 1, 0}, // >
  {KM_CORE_VKEY_SLASH, 1, 0},  // ?
  {'2', 1, 0},       // @
  {'A', 1, 1},
  {'B', 1, 1},
  {'C', 1, 1},
  {'D', 1, 1},
  {'E', 1, 1},
  {'F', 1, 1},
  {'G', 1, 1},
  {'H', 1, 1},
  {'I', 1, 1},
  {'J', 1, 1},
  {'K', 1, 1},
  {'L', 1, 1},
  {'M', 1, 1},
  {'N', 1, 1},
  {'O', 1, 1},
  {'P', 1, 1},
  {'Q', 1, 1},
  {'R', 1, 1},
  {'S', 1, 1},
  {'T', 1, 1},
  {'U', 1, 1},
  {'V', 1, 1},
  {'W', 1, 1},
  {'X', 1, 1},
  {'Y', 1, 1},
  {'Z', 1, 1},
  {KM_CORE_VKEY_LBRKT, 0, 0},
  {KM_CORE_VKEY_BKSLASH, 0, 0},
  {KM_CORE_VKEY_RBRKT, 0, 0},
  {'6', 1, 0},
  {KM_CORE_VKEY_HYPHEN, 1, 0},
  {KM_CORE_VKEY_BKQUOTE, 0, 0},
  {'A', 0, 1},
  {'B', 0, 1},
  {'C', 0, 1},
  {'D', 0, 1},
  {'E', 0, 1},
  {'F', 0, 1},
  {'G', 0, 1},
  {'H', 0, 1},
  {'I', 0, 1},
  {'J', 0, 1},
  {'K', 0, 1},
  {'L', 0, 1},
  {'M', 0, 1},
  {'N', 0, 1},
  {'O', 0, 1},
  {'P', 0, 1},
  {'Q', 0, 1},
  {'R', 0, 1},
  {'S', 0, 1},
  {'T', 0, 1},
  {'U', 0, 1},
  {'V', 0, 1},
  {'W', 0, 1},
  {'X', 0, 1},
  {'Y', 0, 1},
  {'Z', 0, 1},
  {KM_CORE_VKEY_LBRKT, 1, 0},
  {KM_CORE_VKEY_BKSLASH, 1, 0},
  {KM_CORE_VKEY_RBRKT, 1, 0},
  {KM_CORE_VKEY_BKQUOTE, 1, 0},
  {0, 0, 0}
};

const bool vkey_to_contextreset[256] = {
	true,   //L"K_?00",				// &H0
	true,   //L"K_LBUTTON",			// &H1
	true,   //L"K_RBUTTON",			// &H2
	true,   //L"K_CANCEL",		   	// &H3
	true,   //L"K_MBUTTON",			// &H4
	true,   //L"K_?05",				// &H5
	true,   //L"K_?06",				// &H6
	true,   //L"K_?07",				// &H7
	true,   //L"K_BKSP",	    		// &H8
	true,   //L"K_TAB",	    		// &H9
	true,   //L"K_?0A",				// &HA
	true,   //L"K_?0B",				// &HB
	true,   //L"K_KP5",		    	// &HC
	true,   //L"K_ENTER",				// &HD
	true,   //L"K_?0E",				// &HE
	true,   //L"K_?0F",				// &HF
	false,   //L"K_SHIFT",				// &H10
	false,   //L"K_CONTROL",			// &H11
	false,   //L"K_ALT",				// &H12
	true,   //L"K_PAUSE",				// &H13
	false,   //L"K_CAPS",				// &H14
	true,   //L"K_KANJI?15",			// &H15
	true,   //L"K_KANJI?16",			// &H16
	true,   //L"K_KANJI?17",			// &H17
	true,   //L"K_KANJI?18",			// &H18
	true,   //L"K_KANJI?19",			// &H19
	true,   //L"K_?1A",				// &H1A
	true,   //L"K_ESC",				// &H1B
	true,   //L"K_KANJI?1C",			// &H1C
	true,   //L"K_KANJI?1D",			// &H1D
	true,   //L"K_KANJI?1E",			// &H1E
	true,   //L"K_KANJI?1F",			// &H1F
	false,   //L"K_SPACE",				// &H20
	true,   //L"K_PGUP",				// &H21
	true,   //L"K_PGDN",				// &H22
	true,   //L"K_END",				// &H23
	true,   //L"K_HOME",				// &H24
	true,   //L"K_LEFT",				// &H25
	true,   //L"K_UP",				// &H26
	true,   //L"K_RIGHT",				// &H27
	true,   //L"K_DOWN",				// &H28
	true,   //L"K_SEL",				// &H29
	true,   //L"K_PRINT",				// &H2A
	true,   //L"K_EXEC",				// &H2B
	true,   //L"K_PRTSCN",			// &H2C
	false,   //L"K_INS",				// &H2D
	true,   //L"K_DEL",				// &H2E
	true,   //L"K_HELP",				// &H2F
	false,   //L"K_0",					// &H30
	false,   //L"K_1",					// &H31
	false,   //L"K_2",					// &H32
	false,   //L"K_3",					// &H33
	false,   //L"K_4",					// &H34
	false,   //L"K_5",					// &H35
	false,   //L"K_6",					// &H36
	false,   //L"K_7",					// &H37
	false,   //L"K_8",					// &H38
	false,   //L"K_9",					// &H39
	false,   //L"K_?3A",				// &H3A
	false,   //L"K_?3B",				// &H3B
	false,   //L"K_?3C",				// &H3C
	false,   //L"K_?3D",				// &H3D
	false,   //L"K_?3E",				// &H3E
	false,   //L"K_?3F",				// &H3F
	false,   //L"K_?40",				// &H40

	false,   //L"K_A",					// &H41
	false,   //L"K_B",					// &H42
	false,   //L"K_C",					// &H43
	false,   //L"K_D",					// &H44
	false,   //L"K_E",					// &H45
	false,   //L"K_F",					// &H46
	false,   //L"K_G",					// &H47
	false,   //L"K_H",					// &H48
	false,   //L"K_I",					// &H49
	false,   //L"K_J",					// &H4A
	false,   //L"K_K",					// &H4B
	false,   //L"K_L",					// &H4C
	false,   //L"K_M",					// &H4D
	false,   //L"K_N",					// &H4E
	false,   //L"K_O",					// &H4F
	false,   //L"K_P",					// &H50
	false,   //L"K_Q",					// &H51
	false,   //L"K_R",					// &H52
	false,   //L"K_S",					// &H53
	false,   //L"K_T",					// &H54
	false,   //L"K_U",					// &H55
	false,   //L"K_V",					// &H56
	false,   //L"K_W",					// &H57
	false,   //L"K_X",					// &H58
	false,   //L"K_Y",					// &H59
	false,   //L"K_Z",					// &H5A
	false,   //L"K_?5B",				// &H5B
	false,   //L"K_?5C",				// &H5C
	false,   //L"K_?5D",				// &H5D
	false,   //L"K_?5E",				// &H5E
	false,   //L"K_?5F",				// &H5F
	false,   //L"K_NP0",				// &H60
	false,   //L"K_NP1",				// &H61
	false,   //L"K_NP2",				// &H62
	false,   //L"K_NP3",				// &H63
	false,   //L"K_NP4",				// &H64
	false,   //L"K_NP5",				// &H65
	false,   //L"K_NP6",				// &H66
	false,   //L"K_NP7",				// &H67
	false,   //L"K_NP8",				// &H68
	false,   //L"K_NP9",				// &H69
	false,   //L"K_NPSTAR",			// &H6A
	false,   //L"K_NPPLUS",			// &H6B
	false,   //L"K_SEPARATOR",			// &H6C
	false,   //L"K_NPMINUS",			// &H6D
	false,   //L"K_NPDOT",				// &H6E
	false,   //L"K_NPSLASH",			// &H6F
	true,   //L"K_F1",				// &H70
	true,   //L"K_F2",				// &H71
	true,   //L"K_F3",				// &H72
	true,   //L"K_F4",				// &H73
	true,   //L"K_F5",				// &H74
	true,   //L"K_F6",				// &H75
	true,   //L"K_F7",				// &H76
	true,   //L"K_F8",				// &H77
	true,   //L"K_F9",				// &H78
	true,   //L"K_F10",				// &H79
	true,   //L"K_F11",				// &H7A
	true,   //L"K_F12",				// &H7B
	true,   //L"K_F13",				// &H7C
	true,   //L"K_F14",				// &H7D
	true,   //L"K_F15",				// &H7E
	true,   //L"K_F16",				// &H7F
	true,   //L"K_F17",				// &H80
	true,   //L"K_F18",				// &H81
	true,   //L"K_F19",				// &H82
	true,   //L"K_F20",				// &H83
	true,   //L"K_F21",				// &H84
	true,   //L"K_F22",				// &H85
	true,   //L"K_F23",				// &H86
	true,   //L"K_F24",				// &H87

	false,   //L"K_?88",				// &H88
	false,   //L"K_?89",				// &H89
	false,   //L"K_?8A",				// &H8A
	false,   //L"K_?8B",				// &H8B
	false,   //L"K_?8C",				// &H8C
	false,   //L"K_?8D",				// &H8D
	false,   //L"K_?8E",				// &H8E
	false,   //L"K_?8F",				// &H8F

	false,   //L"K_NUMLOCK",			// &H90
	false,   //L"K_SCROLL",			// &H91

	false,   //L"K_?92",				// &H92
	false,   //L"K_?93",				// &H93
	false,   //L"K_?94",				// &H94
	false,   //L"K_?95",				// &H95
	false,   //L"K_?96",				// &H96
	false,   //L"K_?97",				// &H97
	false,   //L"K_?98",				// &H98
	false,   //L"K_?99",				// &H99
	false,   //L"K_?9A",				// &H9A
	false,   //L"K_?9B",				// &H9B
	false,   //L"K_?9C",				// &H9C
	false,   //L"K_?9D",				// &H9D
	false,   //L"K_?9E",				// &H9E
	false,   //L"K_?9F",				// &H9F
	false,   //L"K_?A0",				// &HA0
	false,   //L"K_?A1",				// &HA1
	false,   //L"K_?A2",				// &HA2
	false,   //L"K_?A3",				// &HA3
	false,   //L"K_?A4",				// &HA4
	false,   //L"K_?A5",				// &HA5
	false,   //L"K_?A6",				// &HA6
	false,   //L"K_?A7",				// &HA7
	false,   //L"K_?A8",				// &HA8
	false,   //L"K_?A9",				// &HA9
	false,   //L"K_?AA",				// &HAA
	false,   //L"K_?AB",				// &HAB
	false,   //L"K_?AC",				// &HAC
	false,   //L"K_?AD",				// &HAD
	false,   //L"K_?AE",				// &HAE
	false,   //L"K_?AF",				// &HAF
	false,   //L"K_?B0",				// &HB0
	false,   //L"K_?B1",				// &HB1
	false,   //L"K_?B2",				// &HB2
	false,   //L"K_?B3",				// &HB3
	false,   //L"K_?B4",				// &HB4
	false,   //L"K_?B5",				// &HB5
	false,   //L"K_?B6",				// &HB6
	false,   //L"K_?B7",				// &HB7
	false,   //L"K_?B8",				// &HB8
	false,   //L"K_?B9",				// &HB9

	false,   //L"K_COLON",				// &HBA
	false,   //L"K_EQUAL",				// &HBB
	false,   //L"K_COMMA",				// &HBC
	false,   //L"K_HYPHEN",			// &HBD
	false,   //L"K_PERIOD",			// &HBE
	false,   //L"K_SLASH",				// &HBF
	false,   //L"K_BKQUOTE",			// &HC0

	false,   //L"K_?C1",				// &HC1
	false,   //L"K_?C2",				// &HC2
	false,   //L"K_?C3",				// &HC3
	false,   //L"K_?C4",				// &HC4
	false,   //L"K_?C5",				// &HC5
	false,   //L"K_?C6",				// &HC6
	false,   //L"K_?C7",				// &HC7
	false,   //L"K_?C8",				// &HC8
	false,   //L"K_?C9",				// &HC9
	false,   //L"K_?CA",				// &HCA
	false,   //L"K_?CB",				// &HCB
	false,   //L"K_?CC",				// &HCC
	false,   //L"K_?CD",				// &HCD
	false,   //L"K_?CE",				// &HCE
	false,   //L"K_?CF",				// &HCF
	false,   //L"K_?D0",				// &HD0
	false,   //L"K_?D1",				// &HD1
	false,   //L"K_?D2",				// &HD2
	false,   //L"K_?D3",				// &HD3
	false,   //L"K_?D4",				// &HD4
	false,   //L"K_?D5",				// &HD5
	false,   //L"K_?D6",				// &HD6
	false,   //L"K_?D7",				// &HD7
	false,   //L"K_?D8",				// &HD8
	false,   //L"K_?D9",				// &HD9
	false,   //L"K_?DA",				// &HDA

	false,   //L"K_LBRKT",				// &HDB
	false,   //L"K_BKSLASH",			// &HDC
	false,   //L"K_RBRKT",				// &HDD
	false,   //L"K_QUOTE",				// &HDE
	false,   //L"K_oDF",				// &HDF
	false,   //L"K_oE0",				// &HE0
	false,   //L"K_oE1",				// &HE1
	false,   //L"K_oE2",				// &HE2
	false,   //L"K_oE3",				// &HE3
	false,   //L"K_oE4",				// &HE4

	false,   //L"K_?E5",				// &HE5

	false,   //L"K_oE6",				// &HE6

	false,   //L"K_?E7",				// &HE7
	false,   //L"K_?E8",				// &HE8

	false,   //L"K_oE9",				// &HE9
	false,   //L"K_oEA",				// &HEA
	false,   //L"K_oEB",				// &HEB
	false,   //L"K_oEC",				// &HEC
	false,   //L"K_oED",				// &HED
	false,   //L"K_oEE",				// &HEE
	false,   //L"K_oEF",				// &HEF
	false,   //L"K_oF0",				// &HF0
	false,   //L"K_oF1",				// &HF1
	false,   //L"K_oF2",				// &HF2
	false,   //L"K_oF3",				// &HF3
	false,   //L"K_oF4",				// &HF4
	false,   //L"K_oF5",				// &HF5

	false,   //L"K_?F6",				// &HF6
	false,   //L"K_?F7",				// &HF7
	false,   //L"K_?F8",				// &HF8
	false,   //L"K_?F9",				// &HF9
	false,   //L"K_?FA",				// &HFA
	false,   //L"K_?FB",				// &HFB
	false,   //L"K_?FC",				// &HFC
	false,   //L"K_?FD",				// &HFD
	false,   //L"K_?FE",				// &HFE
	false,   //L"K_?FF"				// &HFF
};


} // namespace kmx
} // namespace core
} // namespace km
