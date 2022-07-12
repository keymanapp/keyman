#include "pch.h"

LPCSTR KeyNames[256] = {
// Key Codes
	"?00",					// &H0
	"?Left Mouse Button",	// &H1
	"?Right Mouse Button",	// &H2
	"?Ctrl+Break",		   	// &H3
	"?Middle Mouse Button",	// &H4
	"?05",					// &H5
	"?06",					// &H6
	"?07",					// &H7
	"Backspace",	   		// &H8
	"Tab",	   		 		// &H9
	"?0A",					// &HA
	"?0B",					// &HB
	"Key Pad 5",	    	// &HC
	"Enter",				// &HD
	"?0E",					// &HE
	"?0F",					// &HF
	"?Shift",				// &H10
	"?Control",				// &H11
	"?Alt",					// &H12
	"Pause",				// &H13
	"?Caps Lock",			// &H14
	"?KANJI?15",			// &H15
	"?KANJI?16",			// &H16
	"?KANJI?17",			// &H17
	"?KANJI?18",			// &H18
	"?KANJI?19",			// &H19
	"?1A",				// &H1A
	"Escape",				// &H1B
	"?KANJI?1C",			// &H1C
	"?KANJI?1D",			// &H1D
	"?KANJI?1E",			// &H1E
	"?KANJI?1F",			// &H1F
	"Spacebar",				// &H20
	"Page Up",				// &H21
	"Page Down",				// &H22
	"End",				// &H23
	"Home",				// &H24
	"Left Arrow",				// &H25
	"Up Arrow",					// &H26
	"Right Arrow",				// &H27
	"Down Arrow",				// &H28
	"?Select",				// &H29
	"?PRINT",				// &H2A
	"?Execute",				// &H2B
	"Print Screen",				// &H2C
	"Insert",				// &H2D
	"Delete",				// &H2E
	"?Help",				// &H2F
	"0",					// &H30
	"1",					// &H31
	"2",					// &H32
	"3",					// &H33
	"4",					// &H34
	"5",					// &H35
	"6",					// &H36
	"7",					// &H37
	"8",					// &H38
	"9",					// &H39
	"?3A",				// &H3A
	"?3B",				// &H3B
	"?3C",				// &H3C
	"?3D",				// &H3D
	"?3E",				// &H3E
	"?3F",				// &H3F
	"?40",				// &H40

	"A",					// &H41
	"B",					// &H42
	"C",					// &H43
	"D",					// &H44
	"E",					// &H45
	"F",					// &H46
	"G",					// &H47
	"H",					// &H48
	"I",					// &H49
	"J",					// &H4A
	"K",					// &H4B
	"L",					// &H4C
	"M",					// &H4D
	"N",					// &H4E
	"O",					// &H4F
	"P",					// &H50
	"Q",					// &H51
	"R",					// &H52
	"S",					// &H53
	"T",					// &H54
	"U",					// &H55
	"V",					// &H56
	"W",					// &h57
	"X",					// &H58
	"Y",					// &H59
	"Z",					// &H5A
	"?5B",				// &H5B
	"?5C",				// &H5C
	"?5D",				// &H5D
	"?5E",				// &H5E
	"?5F",				// &H5F
	"Number Pad 0",				// &H60
	"Number Pad 1",				// &H61
	"Number Pad 2",				// &H62
	"Number Pad 3",				// &H63
	"Number Pad 4",				// &H64
	"Number Pad 5",				// &H65
	"Number Pad 6",				// &H66
	"Number Pad 7",				// &H67
	"Number Pad 8",				// &H68
	"Number Pad 9",				// &H69
	"Number Pad *",				// &H6A
	"Number Pad +",				// &H6B
	"?Separator",			// &H6C
	"Number Pad -",			// &H6D
	"Number Pad .",				// &H6E
	"Number Pad /",			// &H6F
	"F1",					// &H70
	"F2",					// &H71
	"F3",					// &H72
	"F4",					// &H73
	"F5",					// &H74
	"F6",					// &H75
	"F7",					// &H76
	"F8",					// &H77
	"F9",					// &H78
	"F10",				// &H79
	"F11",				// &H7A
	"F12",				// &H7B
	"F13",				// &H7C
	"F14",				// &H7D
	"F15",				// &H7E
	"F16",				// &H7F
	"F17",				// &H80
	"F18",				// &H81
	"F19",				// &H82
	"F20",				// &H83
	"F21",				// &H84
	"F22",				// &H85
	"F23",				// &H86
	"F24",				// &H87

	"?88",				// &H88
	"?89",				// &H89
	"?8A",				// &H8A
	"?8B",				// &H8B
	"?8C",				// &H8C
	"?8D",				// &H8D
	"?8E",				// &H8E
	"?8F",				// &H8F

	"?Num Lock",			// &H90
	"?Scroll Lock",				// &H91

	"?92",				// &H92
	"?93",				// &H93
	"?94",				// &H94
	"?95",				// &H95
	"?96",				// &H96
	"?97",				// &H97
	"?98",				// &H98
	"?99",				// &H99
	"?9A",				// &H9A
	"?9B",				// &H9B
	"?9C",				// &H9C
	"?9D",				// &H9D
	"?9E",				// &H9E
	"?9F",				// &H9F

	"?LShift",				// &HA0
	"?RShift",				// &HA1
	"?LControl",				// &HA2
	"?RControl",				// &HA3
	"?LAlt",				// &HA4
	"?RAlt",				// &HA5

	"?A6",				// &HA6
	"?A7",				// &HA7
	"?A8",				// &HA8
	"?A9",				// &HA9
	"?AA",				// &HAA
	"?AB",				// &HAB
	"?AC",				// &HAC
	"?AD",				// &HAD
	"?AE",				// &HAE
	"?AF",				// &HAF
	"?B0",				// &HB0
	"?B1",				// &HB1
	"?B2",				// &HB2
	"?B3",				// &HB3
	"?B4",				// &HB4
	"?B5",				// &HB5
	"?B6",				// &HB6
	"?B7",				// &HB7
	"?B8",				// &HB8
	"?B9",				// &HB9

	";",				// &HBA
	"=",				// &HBB
	",",				// &HBC
	"-",				// &HBD
	".",				// &HBE
	"/",				// &HBF
	"`",			// &HC0

	"?C1",				// &HC1
	"?C2",				// &HC2
	"?C3",				// &HC3
	"?C4",				// &HC4
	"?C5",				// &HC5
	"?C6",				// &HC6
	"?C7",				// &HC7
	"?C8",				// &HC8
	"?C9",				// &HC9
	"?CA",				// &HCA
	"?CB",				// &HCB
	"?CC",				// &HCC
	"?CD",				// &HCD
	"?CE",				// &HCE
	"?CF",				// &HCF
	"?D0",				// &HD0
	"?D1",				// &HD1
	"?D2",				// &HD2
	"?D3",				// &HD3
	"?D4",				// &HD4
	"?D5",				// &HD5
	"?D6",				// &HD6
	"?D7",				// &HD7
	"?D8",				// &HD8
	"?D9",				// &HD9
	"?DA",				// &HDA

	"[",				// &HDB
	"\\",				// &HDC
	"]",				// &HDD
	"'",				// &HDE
	"?DF",				// &HDF
	"?E0",				// &HE0
	"?E1",				// &HE1
	"?E2",				// &HE2
	"?E3",				// &HE3
	"?E4",				// &HE4

	"?E5",				// &HE5

	"?E6",				// &HE6

	"VK_PACKET",				// &HE7
	"?E8",				// &HE8

	"?E9",				// &HE9
	"?EA",				// &HEA
	"?EB",				// &HEB
	"?EC",				// &HEC
	"?ED",				// &HED
	"?EE",				// &HEE
	"?EF",				// &HEF
	"?F0",				// &HF0
	"?F1",				// &HF1
	"?F2",				// &HF2
	"?F3",				// &HF3
	"?F4",				// &HF4
	"?F5",				// &HF5

	"?F6",				// &HF6
	"?F7",				// &HF7
	"?F8",				// &HF8
	"?F9",				// &HF9
	"?FA",				// &HFA
	"?FB",				// &HFB
	"?FC",				// &HFC
	"?FD",				// &HFD
	"?FE",				// &HFE
	"?FF"					// &HFF
	};

