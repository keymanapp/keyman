(*
  Name:             KeymanWebKeyCodes
  Copyright:        Copyright (C) SIL International.
  Documentation:    
  Description:      
  Create Date:      20 Jun 2006

  Modified Date:    20 Jun 2006
  Authors:          mcdurdin
  Related Files:    
  Dependencies:     

  Bugs:             
  Todo:             
  Notes:            
  History:          20 Jun 2006 - mcdurdin - Initial version
*)
unit KeymanWebKeyCodes;

interface

const
  CKeymanWebKeyCodes: array[0..255] of Integer = (
    $FF, // L"K_?00",				// &H0
    $FF, // L"K_LBUTTON",			// &H1
    $FF, // L"K_RBUTTON",			// &H2
    $FF, // L"K_CANCEL",		   	// &H3
    $FF, // L"K_MBUTTON",			// &H4
    $FF, // L"K_?05",				// &H5
    $FF, // L"K_?06",				// &H6
    $FF, // L"K_?07",				// &H7
    $FF, // L"K_BKSP",	    		// &H8
    $FF, // L"K_TAB",	    		// &H9
    $FF, // L"K_?0A",				// &HA
    $FF, // L"K_?0B",				// &HB
    $FF, // L"K_KP5",		    	// &HC
    $FF, // L"K_ENTER",				// &HD
    $FF, // L"K_?0E",				// &HE
    $FF, // L"K_?0F",				// &HF
    $FF, // L"K_SHIFT",				// &H10
    $FF, // L"K_CONTRO$00, // L",			// &H11
    $FF, // L"K_ALT",				// &H12
    $FF, // L"K_PAUSE",				// &H13
    $FF, // L"K_CAPS",				// &H14
    $FF, // L"K_KANJI?15",			// &H15
    $FF, // L"K_KANJI?16",			// &H16
    $FF, // L"K_KANJI?17",			// &H17
    $FF, // L"K_KANJI?18",			// &H18
    $FF, // L"K_KANJI?19",			// &H19
    $FF, // L"K_?1A",				// &H1A
    $FF, // L"K_ESC",				// &H1B
    $FF, // L"K_KANJI?1C",			// &H1C
    $FF, // L"K_KANJI?1D",			// &H1D
    $FF, // L"K_KANJI?1E",			// &H1E
    $FF, // L"K_KANJI?1F",			// &H1F
    $40, // L"K_SPACE",				// &H20
    $FF, // L"K_PGUP",				// &H21
    $FF, // L"K_PGDN",				// &H22
    $FF, // L"K_END",				// &H23
    $FF, // L"K_HOME",				// &H24
    $FF, // L"K_LEFT",				// &H25
    $FF, // L"K_UP",				// &H26
    $FF, // L"K_RIGHT",				// &H27
    $FF, // L"K_DOWN",				// &H28
    $FF, // L"K_SEL",				// &H29
    $FF, // L"K_PRINT",				// &H2A
    $FF, // L"K_EXEC",				// &H2B
    $FF, // L"K_PRTSCN",			// &H2C
    $FF, // L"K_INS",				// &H2D
    $FF, // L"K_DEL",				// &H2E
    $FF, // L"K_HELP",				// &H2F
    $0A, // L"K_0",					// &H30
    $01, // L"K_1",					// &H31
    $02, // L"K_2",					// &H32
    $03, // L"K_3",					// &H33
    $04, // L"K_4",					// &H34
    $05, // L"K_5",					// &H35
    $06, // L"K_6",					// &H36
    $07, // L"K_7",					// &H37
    $08, // L"K_8",					// &H38
    $09, // L"K_9",					// &H39
    $FF, // L"K_?3A",				// &H3A
    $FF, // L"K_?3B",				// &H3B
    $FF, // L"K_?3C",				// &H3C
    $FF, // L"K_?3D",				// &H3D
    $FF, // L"K_?3E",				// &H3E
    $FF, // L"K_?3F",				// &H3F
    $FF, // L"K_?40",				// &H40

    $20, // L"K_A",					// &H41
    $35, // L"K_B",					// &H42
    $33, // L"K_C",					// &H43
    $22, // L"K_D",					// &H44
    $12, // L"K_E",					// &H45
    $23, // L"K_F",					// &H46
    $24, // L"K_G",					// &H47
    $25, // L"K_H",					// &H48
    $17, // L"K_I",					// &H49
    $26, // L"K_J",					// &H4A
    $27, // L"K_K",					// &H4B
    $28, // L"K_L",					// &H4C
    $37, // L"K_M",					// &H4D
    $36, // L"K_N",					// &H4E
    $18, // L"K_O",					// &H4F
    $19, // L"K_P",					// &H50
    $10, // L"K_Q",					// &H51
    $13, // L"K_R",					// &H52
    $21, // L"K_S",					// &H53
    $14, // L"K_T",					// &H54
    $16, // L"K_U",					// &H55
    $34, // L"K_V",					// &H56
    $11, // L"K_W",					// &H57
    $32, // L"K_X",					// &H58
    $15, // L"K_Y",					// &H59
    $31, // L"K_Z",					// &H5A
    $FF, // L"K_?5B",				// &H5B
    $FF, // L"K_?5C",				// &H5C
    $FF, // L"K_?5D",				// &H5D
    $FF, // L"K_?5E",				// &H5E
    $FF, // L"K_?5F",				// &H5F
    $FF, // L"K_NP0",				// &H60
    $FF, // L"K_NP1",				// &H61
    $FF, // L"K_NP2",				// &H62
    $FF, // L"K_NP3",				// &H63
    $FF, // L"K_NP4",				// &H64
    $FF, // L"K_NP5",				// &H65
    $FF, // L"K_NP6",				// &H66
    $FF, // L"K_NP7",				// &H67
    $FF, // L"K_NP8",				// &H68
    $FF, // L"K_NP9",				// &H69
    $FF, // L"K_NPSTAR",			// &H6A
    $FF, // L"K_NPPLUS",			// &H6B
    $FF, // L"K_SEPARATOR",			// &H6C
    $FF, // L"K_NPMINUS",			// &H6D
    $FF, // L"K_NPDOT",				// &H6E
    $FF, // L"K_NPSLASH",			// &H6F
    $FF, // L"K_F1",				// &H70
    $FF, // L"K_F2",				// &H71
    $FF, // L"K_F3",				// &H72
    $FF, // L"K_F4",				// &H73
    $FF, // L"K_F5",				// &H74
    $FF, // L"K_F6",				// &H75
    $FF, // L"K_F7",				// &H76
    $FF, // L"K_F8",				// &H77
    $FF, // L"K_F9",				// &H78
    $FF, // L"K_F10",				// &H79
    $FF, // L"K_F11",				// &H7A
    $FF, // L"K_F12",				// &H7B
    $FF, // L"K_F13",				// &H7C
    $FF, // L"K_F14",				// &H7D
    $FF, // L"K_F15",				// &H7E
    $FF, // L"K_F16",				// &H7F
    $FF, // L"K_F17",				// &H80
    $FF, // L"K_F18",				// &H81
    $FF, // L"K_F19",				// &H82
    $FF, // L"K_F20",				// &H83
    $FF, // L"K_F21",				// &H84
    $FF, // L"K_F22",				// &H85
    $FF, // L"K_F23",				// &H86
    $FF, // L"K_F24",				// &H87

    $FF, // L"K_?88",				// &H88
    $FF, // L"K_?89",				// &H89
    $FF, // L"K_?8A",				// &H8A
    $FF, // L"K_?8B",				// &H8B
    $FF, // L"K_?8C",				// &H8C
    $FF, // L"K_?8D",				// &H8D
    $FF, // L"K_?8E",				// &H8E
    $FF, // L"K_?8F",				// &H8F

    $FF, // L"K_NUMLOCK",			// &H90
    $FF, // L"K_SCROL$00, // L",			// &H91

    $FF, // L"K_?92",				// &H92
    $FF, // L"K_?93",				// &H93
    $FF, // L"K_?94",				// &H94
    $FF, // L"K_?95",				// &H95
    $FF, // L"K_?96",				// &H96
    $FF, // L"K_?97",				// &H97
    $FF, // L"K_?98",				// &H98
    $FF, // L"K_?99",				// &H99
    $FF, // L"K_?9A",				// &H9A
    $FF, // L"K_?9B",				// &H9B
    $FF, // L"K_?9C",				// &H9C
    $FF, // L"K_?9D",				// &H9D
    $FF, // L"K_?9E",				// &H9E
    $FF, // L"K_?9F",				// &H9F
    $FF, // L"K_?A0",				// &HA0
    $FF, // L"K_?A1",				// &HA1
    $FF, // L"K_?A2",				// &HA2
    $FF, // L"K_?A3",				// &HA3
    $FF, // L"K_?A4",				// &HA4
    $FF, // L"K_?A5",				// &HA5
    $FF, // L"K_?A6",				// &HA6
    $FF, // L"K_?A7",				// &HA7
    $FF, // L"K_?A8",				// &HA8
    $FF, // L"K_?A9",				// &HA9
    $FF, // L"K_?AA",				// &HAA
    $FF, // L"K_?AB",				// &HAB
    $FF, // L"K_?AC",				// &HAC
    $FF, // L"K_?AD",				// &HAD
    $FF, // L"K_?AE",				// &HAE
    $FF, // L"K_?AF",				// &HAF
    $FF, // L"K_?B0",				// &HB0
    $FF, // L"K_?B1",				// &HB1
    $FF, // L"K_?B2",				// &HB2
    $FF, // L"K_?B3",				// &HB3
    $FF, // L"K_?B4",				// &HB4
    $FF, // L"K_?B5",				// &HB5
    $FF, // L"K_?B6",				// &HB6
    $FF, // L"K_?B7",				// &HB7
    $FF, // L"K_?B8",				// &HB8
    $FF, // L"K_?B9",				// &HB9

    $29, // L"K_COLON",				// &HBA
    $0C, // L"K_EQUA$00, // L",				// &HBB
    $38, // L"K_COMMA",				// &HBC
    $0B, // L"K_HYPHEN",			// &HBD
    $39, // L"K_PERIOD",			// &HBE
    $3A, // L"K_SLASH",				// &HBF
    $00, // L"K_BKQUOTE",			// &HC0

    $00, // L"K_?C1",				// &HC1
    $00, // L"K_?C2",				// &HC2
    $00, // L"K_?C3",				// &HC3
    $00, // L"K_?C4",				// &HC4
    $00, // L"K_?C5",				// &HC5
    $00, // L"K_?C6",				// &HC6
    $00, // L"K_?C7",				// &HC7
    $00, // L"K_?C8",				// &HC8
    $00, // L"K_?C9",				// &HC9
    $00, // L"K_?CA",				// &HCA
    $00, // L"K_?CB",				// &HCB
    $00, // L"K_?CC",				// &HCC
    $00, // L"K_?CD",				// &HCD
    $00, // L"K_?CE",				// &HCE
    $00, // L"K_?CF",				// &HCF
    $00, // L"K_?D0",				// &HD0
    $00, // L"K_?D1",				// &HD1
    $00, // L"K_?D2",				// &HD2
    $00, // L"K_?D3",				// &HD3
    $00, // L"K_?D4",				// &HD4
    $00, // L"K_?D5",				// &HD5
    $00, // L"K_?D6",				// &HD6
    $00, // L"K_?D7",				// &HD7
    $00, // L"K_?D8",				// &HD8
    $00, // L"K_?D9",				// &HD9
    $00, // L"K_?DA",				// &HDA

    $1A, // L"K_LBRKT",				// &HDB
    $1C, // L"K_BKSLASH",			// &HDC
    $1B, // L"K_RBRKT",				// &HDD
    $2A, // L"K_QUOTE",				// &HDE
    $00, // L"K_oDF",				// &HDF
    $00, // L"K_oE0",				// &HE0
    $00, // L"K_oE1",				// &HE1
    $30, // L"K_oE2",				// &HE2
    $00, // L"K_oE3",				// &HE3
    $00, // L"K_oE4",				// &HE4

    $00, // L"K_?E5",				// &HE5

    $00, // L"K_oE6",				// &HE6

    $00, // L"K_?E7",				// &HE7
    $00, // L"K_?E8",				// &HE8

    $00, // L"K_oE9",				// &HE9
    $00, // L"K_oEA",				// &HEA
    $00, // L"K_oEB",				// &HEB
    $00, // L"K_oEC",				// &HEC
    $00, // L"K_oED",				// &HED
    $00, // L"K_oEE",				// &HEE
    $00, // L"K_oEF",				// &HEF
    $00, // L"K_oF0",				// &HF0
    $00, // L"K_oF1",				// &HF1
    $00, // L"K_oF2",				// &HF2
    $00, // L"K_oF3",				// &HF3
    $00, // L"K_oF4",				// &HF4
    $00, // L"K_oF5",				// &HF5

    $00, // L"K_?F6",				// &HF6
    $00, // L"K_?F7",				// &HF7
    $00, // L"K_?F8",				// &HF8
    $00, // L"K_?F9",				// &HF9
    $00, // L"K_?FA",				// &HFA
    $00, // L"K_?FB",				// &HFB
    $00, // L"K_?FC",				// &HFC
    $00, // L"K_?FD",				// &HFD
    $00, // L"K_?FE",				// &HFE
    $00  // L"K_?FF"				// &HFF
  );

implementation

end.

