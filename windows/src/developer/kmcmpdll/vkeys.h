/*
  Name:             vkeys
  Copyright:        Copyright (C) SIL International.
  Documentation:    
  Description:      
  Create Date:      22 Mar 2010

  Modified Date:    22 Mar 2010
  Authors:          mcdurdin
  Related Files:    
  Dependencies:     

  Bugs:             
  Todo:             
  Notes:            
  History:          22 Mar 2010 - mcdurdin - Compiler fixup - tidy up
*/

#define lpuch(x) L ## x

static PWSTR VKeyNames[256] = {
// Key Codes
	lpuch("K_?00"),				// &H0
	lpuch("K_LBUTTON"),			// &H1
	lpuch("K_RBUTTON"),			// &H2
	lpuch("K_CANCEL"),		   	// &H3
	lpuch("K_MBUTTON"),			// &H4
	lpuch("K_?05"),				// &H5
	lpuch("K_?06"),				// &H6
	lpuch("K_?07"),				// &H7
	lpuch("K_BKSP"),	    		// &H8
	lpuch("K_TAB"),	    		// &H9
	lpuch("K_?0A"),				// &HA
	lpuch("K_?0B"),				// &HB
	lpuch("K_KP5"),		    	// &HC
	lpuch("K_ENTER"),				// &HD
	lpuch("K_?0E"),				// &HE
	lpuch("K_?0F"),				// &HF
	lpuch("K_SHIFT"),				// &H10
	lpuch("K_CONTROL"),			// &H11
	lpuch("K_ALT"),				// &H12
	lpuch("K_PAUSE"),				// &H13
	lpuch("K_CAPS"),				// &H14
	lpuch("K_KANJI?15"),			// &H15
	lpuch("K_KANJI?16"),			// &H16
	lpuch("K_KANJI?17"),			// &H17
	lpuch("K_KANJI?18"),			// &H18
	lpuch("K_KANJI?19"),			// &H19
	lpuch("K_?1A"),				// &H1A
	lpuch("K_ESC"),				// &H1B
	lpuch("K_KANJI?1C"),			// &H1C
	lpuch("K_KANJI?1D"),			// &H1D
	lpuch("K_KANJI?1E"),			// &H1E
	lpuch("K_KANJI?1F"),			// &H1F
	lpuch("K_SPACE"),				// &H20
	lpuch("K_PGUP"),				// &H21
	lpuch("K_PGDN"),				// &H22
	lpuch("K_END"),				// &H23
	lpuch("K_HOME"),				// &H24
	lpuch("K_LEFT"),				// &H25
	lpuch("K_UP"),				// &H26
	lpuch("K_RIGHT"),				// &H27
	lpuch("K_DOWN"),				// &H28
	lpuch("K_SEL"),				// &H29
	lpuch("K_PRINT"),				// &H2A
	lpuch("K_EXEC"),				// &H2B
	lpuch("K_PRTSCN"),			// &H2C
	lpuch("K_INS"),				// &H2D
	lpuch("K_DEL"),				// &H2E
	lpuch("K_HELP"),				// &H2F
	lpuch("K_0"),					// &H30
	lpuch("K_1"),					// &H31
	lpuch("K_2"),					// &H32
	lpuch("K_3"),					// &H33
	lpuch("K_4"),					// &H34
	lpuch("K_5"),					// &H35
	lpuch("K_6"),					// &H36
	lpuch("K_7"),					// &H37
	lpuch("K_8"),					// &H38
	lpuch("K_9"),					// &H39
	lpuch("K_?3A"),				// &H3A
	lpuch("K_?3B"),				// &H3B
	lpuch("K_?3C"),				// &H3C
	lpuch("K_?3D"),				// &H3D
	lpuch("K_?3E"),				// &H3E
	lpuch("K_?3F"),				// &H3F
	lpuch("K_?40"),				// &H40

	lpuch("K_A"),					// &H41
	lpuch("K_B"),					// &H42
	lpuch("K_C"),					// &H43
	lpuch("K_D"),					// &H44
	lpuch("K_E"),					// &H45
	lpuch("K_F"),					// &H46
	lpuch("K_G"),					// &H47
	lpuch("K_H"),					// &H48
	lpuch("K_I"),					// &H49
	lpuch("K_J"),					// &H4A
	lpuch("K_K"),					// &H4B
	lpuch("K_L"),					// &H4C
	lpuch("K_M"),					// &H4D
	lpuch("K_N"),					// &H4E
	lpuch("K_O"),					// &H4F
	lpuch("K_P"),					// &H50
	lpuch("K_Q"),					// &H51
	lpuch("K_R"),					// &H52
	lpuch("K_S"),					// &H53
	lpuch("K_T"),					// &H54
	lpuch("K_U"),					// &H55
	lpuch("K_V"),					// &H56
	lpuch("K_W"),					// &H57
	lpuch("K_X"),					// &H58
	lpuch("K_Y"),					// &H59
	lpuch("K_Z"),					// &H5A
	lpuch("K_?5B"),				// &H5B
	lpuch("K_?5C"),				// &H5C
	lpuch("K_?5D"),				// &H5D
	lpuch("K_?5E"),				// &H5E
	lpuch("K_?5F"),				// &H5F
	lpuch("K_NP0"),				// &H60
	lpuch("K_NP1"),				// &H61
	lpuch("K_NP2"),				// &H62
	lpuch("K_NP3"),				// &H63
	lpuch("K_NP4"),				// &H64
	lpuch("K_NP5"),				// &H65
	lpuch("K_NP6"),				// &H66
	lpuch("K_NP7"),				// &H67
	lpuch("K_NP8"),				// &H68
	lpuch("K_NP9"),				// &H69
	lpuch("K_NPSTAR"),			// &H6A
	lpuch("K_NPPLUS"),			// &H6B
	lpuch("K_SEPARATOR"),			// &H6C
	lpuch("K_NPMINUS"),			// &H6D
	lpuch("K_NPDOT"),				// &H6E
	lpuch("K_NPSLASH"),			// &H6F
	lpuch("K_F1"),				// &H70
	lpuch("K_F2"),				// &H71
	lpuch("K_F3"),				// &H72
	lpuch("K_F4"),				// &H73
	lpuch("K_F5"),				// &H74
	lpuch("K_F6"),				// &H75
	lpuch("K_F7"),				// &H76
	lpuch("K_F8"),				// &H77
	lpuch("K_F9"),				// &H78
	lpuch("K_F10"),				// &H79
	lpuch("K_F11"),				// &H7A
	lpuch("K_F12"),				// &H7B
	lpuch("K_F13"),				// &H7C
	lpuch("K_F14"),				// &H7D
	lpuch("K_F15"),				// &H7E
	lpuch("K_F16"),				// &H7F
	lpuch("K_F17"),				// &H80
	lpuch("K_F18"),				// &H81
	lpuch("K_F19"),				// &H82
	lpuch("K_F20"),				// &H83
	lpuch("K_F21"),				// &H84
	lpuch("K_F22"),				// &H85
	lpuch("K_F23"),				// &H86
	lpuch("K_F24"),				// &H87

	lpuch("K_?88"),				// &H88
	lpuch("K_?89"),				// &H89
	lpuch("K_?8A"),				// &H8A
	lpuch("K_?8B"),				// &H8B
	lpuch("K_?8C"),				// &H8C
	lpuch("K_?8D"),				// &H8D
	lpuch("K_?8E"),				// &H8E
	lpuch("K_?8F"),				// &H8F

	lpuch("K_NUMLOCK"),			// &H90
	lpuch("K_SCROLL"),			// &H91

	lpuch("K_?92"),				// &H92
	lpuch("K_?93"),				// &H93
	lpuch("K_?94"),				// &H94
	lpuch("K_?95"),				// &H95
	lpuch("K_?96"),				// &H96
	lpuch("K_?97"),				// &H97
	lpuch("K_?98"),				// &H98
	lpuch("K_?99"),				// &H99
	lpuch("K_?9A"),				// &H9A
	lpuch("K_?9B"),				// &H9B
	lpuch("K_?9C"),				// &H9C
	lpuch("K_?9D"),				// &H9D
	lpuch("K_?9E"),				// &H9E
	lpuch("K_?9F"),				// &H9F
	lpuch("K_?A0"),				// &HA0
	lpuch("K_?A1"),				// &HA1
	lpuch("K_?A2"),				// &HA2
	lpuch("K_?A3"),				// &HA3
	lpuch("K_?A4"),				// &HA4
	lpuch("K_?A5"),				// &HA5
	lpuch("K_?A6"),				// &HA6
	lpuch("K_?A7"),				// &HA7
	lpuch("K_?A8"),				// &HA8
	lpuch("K_?A9"),				// &HA9
	lpuch("K_?AA"),				// &HAA
	lpuch("K_?AB"),				// &HAB
	lpuch("K_?AC"),				// &HAC
	lpuch("K_?AD"),				// &HAD
	lpuch("K_?AE"),				// &HAE
	lpuch("K_?AF"),				// &HAF
	lpuch("K_?B0"),				// &HB0
	lpuch("K_?B1"),				// &HB1
	lpuch("K_?B2"),				// &HB2
	lpuch("K_?B3"),				// &HB3
	lpuch("K_?B4"),				// &HB4
	lpuch("K_?B5"),				// &HB5
	lpuch("K_?B6"),				// &HB6
	lpuch("K_?B7"),				// &HB7
	lpuch("K_?B8"),				// &HB8
	lpuch("K_?B9"),				// &HB9

	lpuch("K_COLON"),				// &HBA
	lpuch("K_EQUAL"),				// &HBB
	lpuch("K_COMMA"),				// &HBC
	lpuch("K_HYPHEN"),			// &HBD
	lpuch("K_PERIOD"),			// &HBE
	lpuch("K_SLASH"),				// &HBF
	lpuch("K_BKQUOTE"),			// &HC0

	lpuch("K_?C1"),				// &HC1
	lpuch("K_?C2"),				// &HC2
	lpuch("K_?C3"),				// &HC3
	lpuch("K_?C4"),				// &HC4
	lpuch("K_?C5"),				// &HC5
	lpuch("K_?C6"),				// &HC6
	lpuch("K_?C7"),				// &HC7
	lpuch("K_?C8"),				// &HC8
	lpuch("K_?C9"),				// &HC9
	lpuch("K_?CA"),				// &HCA
	lpuch("K_?CB"),				// &HCB
	lpuch("K_?CC"),				// &HCC
	lpuch("K_?CD"),				// &HCD
	lpuch("K_?CE"),				// &HCE
	lpuch("K_?CF"),				// &HCF
	lpuch("K_?D0"),				// &HD0
	lpuch("K_?D1"),				// &HD1
	lpuch("K_?D2"),				// &HD2
	lpuch("K_?D3"),				// &HD3
	lpuch("K_?D4"),				// &HD4
	lpuch("K_?D5"),				// &HD5
	lpuch("K_?D6"),				// &HD6
	lpuch("K_?D7"),				// &HD7
	lpuch("K_?D8"),				// &HD8
	lpuch("K_?D9"),				// &HD9
	lpuch("K_?DA"),				// &HDA

	lpuch("K_LBRKT"),				// &HDB
	lpuch("K_BKSLASH"),			// &HDC
	lpuch("K_RBRKT"),				// &HDD
	lpuch("K_QUOTE"),				// &HDE
	lpuch("K_oDF"),				// &HDF
	lpuch("K_oE0"),				// &HE0
	lpuch("K_oE1"),				// &HE1
	lpuch("K_oE2"),				// &HE2
	lpuch("K_oE3"),				// &HE3
	lpuch("K_oE4"),				// &HE4

	lpuch("K_?E5"),				// &HE5

	lpuch("K_oE6"),				// &HE6

	lpuch("K_?E7"),				// &HE7
	lpuch("K_?E8"),				// &HE8

	lpuch("K_oE9"),				// &HE9
	lpuch("K_oEA"),				// &HEA
	lpuch("K_oEB"),				// &HEB
	lpuch("K_oEC"),				// &HEC
	lpuch("K_oED"),				// &HED
	lpuch("K_oEE"),				// &HEE
	lpuch("K_oEF"),				// &HEF
	lpuch("K_oF0"),				// &HF0
	lpuch("K_oF1"),				// &HF1
	lpuch("K_oF2"),				// &HF2
	lpuch("K_oF3"),				// &HF3
	lpuch("K_oF4"),				// &HF4
	lpuch("K_oF5"),				// &HF5

	lpuch("K_?F6"),				// &HF6
	lpuch("K_?F7"),				// &HF7
	lpuch("K_?F8"),				// &HF8
	lpuch("K_?F9"),				// &HF9
	lpuch("K_?FA"),				// &HFA
	lpuch("K_?FB"),				// &HFB
	lpuch("K_?FC"),				// &HFC
	lpuch("K_?FD"),				// &HFD
	lpuch("K_?FE"),				// &HFE
	lpuch("K_?FF")				// &HFF
	};
	
