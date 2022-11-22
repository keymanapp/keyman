(*
  Name:             VKeys
  Copyright:        Copyright (C) SIL International.
  Documentation:    
  Description:      
  Create Date:      19 Mar 2007

  Modified Date:    19 Mar 2014
  Authors:          mcdurdin
  Related Files:    
  Dependencies:     

  Bugs:             
  Todo:             
  Notes:            
  History:          19 Mar 2007 - mcdurdin - I698 - Fix non matching of K_oE2 (due to case mismatch)
                    19 Mar 2014 - mcdurdin - I4141 - V9.0 - Warn when unusable key ids are used
                    19 Mar 2014 - mcdurdin - I4142 - V9.0 - Validate key ids are in an acceptable format
                    
*)
unit VKeys;

interface

function FindVKeyName(const txt: string): Word;

const VKeyNames: array[0..255] of string = (
	'K_?00',				// &H0
	'K_LBUTTON',			// &H1
	'K_RBUTTON',			// &H2
	'K_CANCEL',		   	// &H3
	'K_MBUTTON',			// &H4
	'K_?05',				// &H5
	'K_?06',				// &H6
	'K_?07',				// &H7
	'K_BKSP',	    		// &H8
	'K_TAB',	    		// &H9
	'K_?0A',				// &HA
	'K_?0B',				// &HB
	'K_KP5',		    	// &HC
	'K_ENTER',				// &HD
	'K_?0E',				// &HE
	'K_?0F',				// &HF
	'K_SHIFT',				// &H10
	'K_CONTROL',			// &H11
	'K_ALT',				// &H12
	'K_PAUSE',				// &H13
	'K_CAPS',				// &H14
	'K_KANJI?15',			// &H15
	'K_KANJI?16',			// &H16
	'K_KANJI?17',			// &H17
	'K_KANJI?18',			// &H18
	'K_KANJI?19',			// &H19
	'K_?1A',				// &H1A
	'K_ESC',				// &H1B
	'K_KANJI?1C',			// &H1C
	'K_KANJI?1D',			// &H1D
	'K_KANJI?1E',			// &H1E
	'K_KANJI?1F',			// &H1F
	'K_SPACE',				// &H20
	'K_PGUP',				// &H21
	'K_PGDN',				// &H22
	'K_END',				// &H23
	'K_HOME',				// &H24
	'K_LEFT',				// &H25
	'K_UP',				// &H26
	'K_RIGHT',				// &H27
	'K_DOWN',				// &H28
	'K_SEL',				// &H29
	'K_PRINT',				// &H2A
	'K_EXEC',				// &H2B
	'K_PRTSCN',			// &H2C
	'K_INS',				// &H2D
	'K_DEL',				// &H2E
	'K_HELP',				// &H2F
	'K_0',					// &H30
	'K_1',					// &H31
	'K_2',					// &H32
	'K_3',					// &H33
	'K_4',					// &H34
	'K_5',					// &H35
	'K_6',					// &H36
	'K_7',					// &H37
	'K_8',					// &H38
	'K_9',					// &H39
	'K_?3A',				// &H3A
	'K_?3B',				// &H3B
	'K_?3C',				// &H3C
	'K_?3D',				// &H3D
	'K_?3E',				// &H3E
	'K_?3F',				// &H3F
	'K_?40',				// &H40

	'K_A',					// &H41
	'K_B',					// &H42
	'K_C',					// &H43
	'K_D',					// &H44
	'K_E',					// &H45
	'K_F',					// &H46
	'K_G',					// &H47
	'K_H',					// &H48
	'K_I',					// &H49
	'K_J',					// &H4A
	'K_K',					// &H4B
	'K_L',					// &H4C
	'K_M',					// &H4D
	'K_N',					// &H4E
	'K_O',					// &H4F
	'K_P',					// &H50
	'K_Q',					// &H51
	'K_R',					// &H52
	'K_S',					// &H53
	'K_T',					// &H54
	'K_U',					// &H55
	'K_V',					// &H56
	'K_W',					// &H57
	'K_X',					// &H58
	'K_Y',					// &H59
	'K_Z',					// &H5A
	'K_?5B',				// &H5B
	'K_?5C',				// &H5C
	'K_?5D',				// &H5D
	'K_?5E',				// &H5E
	'K_?5F',				// &H5F
	'K_NP0',				// &H60
	'K_NP1',				// &H61
	'K_NP2',				// &H62
	'K_NP3',				// &H63
	'K_NP4',				// &H64
	'K_NP5',				// &H65
	'K_NP6',				// &H66
	'K_NP7',				// &H67
	'K_NP8',				// &H68
	'K_NP9',				// &H69
	'K_NPSTAR',			// &H6A
	'K_NPPLUS',			// &H6B
	'K_SEPARATOR',			// &H6C
	'K_NPMINUS',			// &H6D
	'K_NPDOT',				// &H6E
	'K_NPSLASH',			// &H6F
	'K_F1',				// &H70
	'K_F2',				// &H71
	'K_F3',				// &H72
	'K_F4',				// &H73
	'K_F5',				// &H74
	'K_F6',				// &H75
	'K_F7',				// &H76
	'K_F8',				// &H77
	'K_F9',				// &H78
	'K_F10',				// &H79
	'K_F11',				// &H7A
	'K_F12',				// &H7B
	'K_F13',				// &H7C
	'K_F14',				// &H7D
	'K_F15',				// &H7E
	'K_F16',				// &H7F
	'K_F17',				// &H80
	'K_F18',				// &H81
	'K_F19',				// &H82
	'K_F20',				// &H83
	'K_F21',				// &H84
	'K_F22',				// &H85
	'K_F23',				// &H86
	'K_F24',				// &H87

	'K_?88',				// &H88
	'K_?89',				// &H89
	'K_?8A',				// &H8A
	'K_?8B',				// &H8B
	'K_?8C',				// &H8C
	'K_?8D',				// &H8D
	'K_?8E',				// &H8E
	'K_?8F',				// &H8F

	'K_NUMLOCK',			// &H90
	'K_SCROLL',			// &H91

	'K_?92',				// &H92
	'K_?93',				// &H93
	'K_?94',				// &H94
	'K_?95',				// &H95
	'K_?96',				// &H96
	'K_?97',				// &H97
	'K_?98',				// &H98
	'K_?99',				// &H99
	'K_?9A',				// &H9A
	'K_?9B',				// &H9B
	'K_?9C',				// &H9C
	'K_?9D',				// &H9D
	'K_?9E',				// &H9E
	'K_?9F',				// &H9F
	'K_?A0',				// &HA0
	'K_?A1',				// &HA1
	'K_?A2',				// &HA2
	'K_?A3',				// &HA3
	'K_?A4',				// &HA4
	'K_?A5',				// &HA5
	'K_?A6',				// &HA6
	'K_?A7',				// &HA7
	'K_?A8',				// &HA8
	'K_?A9',				// &HA9
	'K_?AA',				// &HAA
	'K_?AB',				// &HAB
	'K_?AC',				// &HAC
	'K_?AD',				// &HAD
	'K_?AE',				// &HAE
	'K_?AF',				// &HAF
	'K_?B0',				// &HB0
	'K_?B1',				// &HB1
	'K_?B2',				// &HB2
	'K_?B3',				// &HB3
	'K_?B4',				// &HB4
	'K_?B5',				// &HB5
	'K_?B6',				// &HB6
	'K_?B7',				// &HB7
	'K_?B8',				// &HB8
	'K_?B9',				// &HB9

	'K_COLON',				// &HBA
	'K_EQUAL',				// &HBB
	'K_COMMA',				// &HBC
	'K_HYPHEN',			// &HBD
	'K_PERIOD',			// &HBE
	'K_SLASH',				// &HBF
	'K_BKQUOTE',			// &HC0

	'K_?C1',				// &HC1
	'K_?C2',				// &HC2
	'K_?C3',				// &HC3
	'K_?C4',				// &HC4
	'K_?C5',				// &HC5
	'K_?C6',				// &HC6
	'K_?C7',				// &HC7
	'K_?C8',				// &HC8
	'K_?C9',				// &HC9
	'K_?CA',				// &HCA
	'K_?CB',				// &HCB
	'K_?CC',				// &HCC
	'K_?CD',				// &HCD
	'K_?CE',				// &HCE
	'K_?CF',				// &HCF
	'K_?D0',				// &HD0
	'K_?D1',				// &HD1
	'K_?D2',				// &HD2
	'K_?D3',				// &HD3
	'K_?D4',				// &HD4
	'K_?D5',				// &HD5
	'K_?D6',				// &HD6
	'K_?D7',				// &HD7
	'K_?D8',				// &HD8
	'K_?D9',				// &HD9
	'K_?DA',				// &HDA

	'K_LBRKT',				// &HDB
	'K_BKSLASH',			// &HDC
	'K_RBRKT',				// &HDD
	'K_QUOTE',				// &HDE
	'K_oDF',				// &HDF
	'K_oE0',				// &HE0
	'K_oE1',				// &HE1
	'K_oE2',				// &HE2
	'K_oE3',				// &HE3
	'K_oE4',				// &HE4

	'K_?E5',				// &HE5

	'K_oE6',				// &HE6

	'K_?E7',				// &HE7
	'K_?E8',				// &HE8

	'K_oE9',				// &HE9
	'K_oEA',				// &HEA
	'K_oEB',				// &HEB
	'K_oEC',				// &HEC
	'K_oED',				// &HED
	'K_oEE',				// &HEE
	'K_oEF',				// &HEF
	'K_oF0',				// &HF0
	'K_oF1',				// &HF1
	'K_oF2',				// &HF2
	'K_oF3',				// &HF3
	'K_oF4',				// &HF4
	'K_oF5',				// &HF5

	'K_?F6',				// &HF6
	'K_?F7',				// &HF7
	'K_?F8',				// &HF8
	'K_?F9',				// &HF9
	'K_?FA',				// &HFA
	'K_?FB',				// &HFB
	'K_?FC',				// &HFC
	'K_?FD',				// &HFD
	'K_?FE',				// &HFE
	'K_?FF');				// &HFF

///
/// Virtual key names used in KeymanWeb. Blanks indicate a
/// virtual key that is not recognised by KeymanWeb.
///
const KMWVKeyNames: array[0..255] of string = (
  '',
  '',
  '',
  '',
  '',
  '',
  '',
  '',
  'K_BKSP',
  'K_TAB',
  '',
  '',
  '',
  'K_ENTER',
  '',
  '',
  'K_SHIFT',
  'K_CONTROL',
  'K_ALT',
  'K_PAUSE',
  'K_CAPS',
  '',
  '',
  '',
  '',
  '',
  '',
  'K_ESC',
  '',
  '',
  '',
  '',
  'K_SPACE',
  'K_PGUP',
  'K_PGDN',
  'K_END',
  'K_HOME',
  'K_LEFT',
  'K_UP',
  'K_RIGHT',
  'K_DOWN',
  'K_SEL',
  'K_PRINT',
  'K_EXEC',
  '',
  'K_INS',
  'K_DEL',
  'K_HELP',
  'K_0',
  'K_1',
  'K_2',
  'K_3',
  'K_4',
  'K_5',
  'K_6',
  'K_7',
  'K_8',
  'K_9',
  '',
  '',
  '',
  '',
  '',
  '',
  '',
  'K_A',
  'K_B',
  'K_C',
  'K_D',
  'K_E',
  'K_F',
  'K_G',
  'K_H',
  'K_I',
  'K_J',
  'K_K',
  'K_L',
  'K_M',
  'K_N',
  'K_O',
  'K_P',
  'K_Q',
  'K_R',
  'K_S',
  'K_T',
  'K_U',
  'K_V',
  'K_W',
  'K_X',
  'K_Y',
  'K_Z',
  '',
  '',
  '',
  '',
  '',
  'K_NP0',
  'K_NP1',
  'K_NP2',
  'K_NP3',
  'K_NP4',
  'K_NP5',
  'K_NP6',
  'K_NP7',
  'K_NP8',
  'K_NP9',
  'K_NPSTAR',
  'K_NPPLUS',
  'K_SEPARATOR',
  'K_NPMINUS',
  'K_NPDOT',
  'K_NPSLASH',
  'K_F1',
  'K_F2',
  'K_F3',
  'K_F4',
  'K_F5',
  'K_F6',
  'K_F7',
  'K_F8',
  'K_F9',
  'K_F10',
  'K_F11',
  'K_F12',
  '',
  '',
  '',
  '',
  '',
  '',
  '',
  '',
  '',
  '',
  '',
  '',
  '',
  '',
  '',
  '',
  '',
  '',
  '',
  '',
  'K_NUMLOCK',
  'K_SCROLL',
  '',
  '',
  '',
  '',
  '',
  '',
  '',
  '',
  '',
  '',
  '',
  '',
  '',
  '',
  'K_LSHIFT',
  'K_RSHIFT',
  'K_LCONTROL',
  'K_RCONTROL',
  'K_LALT',
  'K_RALT',
  '',
  '',
  '',
  '',
  '',
  '',
  '',
  '',
  '',
  '',
  '',
  '',
  '',
  '',
  '',
  '',
  '',
  '',
  '',
  '',
  'K_COLON',
  'K_EQUAL',
  'K_COMMA',
  'K_HYPHEN',
  'K_PERIOD',
  'K_SLASH',
  'K_BKQUOTE',
  '',
  '',
  '',
  '',
  '',
  '',
  '',
  '',
  '',
  '',
  '',
  '',
  '',
  '',
  '',
  '',
  '',
  '',
  '',
  '',
  '',
  '',
  '',
  '',
  '',
  '',
  'K_LBRKT',
  'K_BKSLASH',
  'K_RBRKT',
  'K_QUOTE',
  '',
  '',
  '',
  'K_oE2',
  '',
  '',
  '',
  '',
  '',
  '',
  '',
  '',
  '',
  '',
  '',
  '',
  '',
  '',
  '',
  '',
  '',
  '',
  '',
  '',
  '',
  '',
  '',
  '',
  '',
  '',
  '',
  '',
  ''
);

const VKeyISO9995Names: array[0..255] of string = (
    // Key Codes
    '', // 'K_?00',				// &H0
    '', // 'K_LBUTTON',			// &H1
    '', // 'K_RBUTTON',			// &H2
    '', // 'K_CANCEL',		   	// &H3
    '', // 'K_MBUTTON',			// &H4
    '', // 'K_?05',				// &H5
    '', // 'K_?06',				// &H6
    '', // 'K_?07',				// &H7
    'E14', // 'K_BKSP',	    		// &H8
    'D00', // 'K_TAB',	    		// &H9
    '', // 'K_?0A',				// &HA
    '', // 'K_?0B',				// &HB
    '', // 'K_KP5',		    	// &HC
    'C13', // 'K_ENTER',				// &HD
    '', // 'K_?0E',				// &HE
    '', // 'K_?0F',				// &HF
    'B99', // 'K_SHIFT',				// &H10
    'A99', // 'K_CONTROL',			// &H11
    'A02', // 'K_ALT',				// &H12
    '', // 'K_PAUSE',				// &H13
    'C00', // 'K_CAPS',				// &H14
    '', // 'K_KANJI?15',			// &H15
    '', // 'K_KANJI?16',			// &H16
    '', // 'K_KANJI?17',			// &H17
    '', // 'K_KANJI?18',			// &H18
    '', // 'K_KANJI?19',			// &H19
    '', // 'K_?1A',				// &H1A
    '', // 'K_ESC',				// &H1B
    '', // 'K_KANJI?1C',			// &H1C
    '', // 'K_KANJI?1D',			// &H1D
    '', // 'K_KANJI?1E',			// &H1E
    '', // 'K_KANJI?1F',			// &H1F
    'A03', // 'K_SPACE',				// &H20
    '', // 'K_PGUP',				// &H21
    '', // 'K_PGDN',				// &H22
    '', // 'K_END',				// &H23
    '', // 'K_HOME',				// &H24
    '', // 'K_LEFT',				// &H25
    '', // 'K_UP',				// &H26
    '', // 'K_RIGHT',				// &H27
    '', // 'K_DOWN',				// &H28
    '', // 'K_SEL',				// &H29
    '', // 'K_PRINT',				// &H2A
    '', // 'K_EXEC',				// &H2B
    '', // 'K_PRTSCN',			// &H2C
    '', // 'K_INS',				// &H2D
    '', // 'K_DEL',				// &H2E
    '', // 'K_HELP',				// &H2F
    'E10', // 'K_0',					// &H30
    'E01', // 'K_1',					// &H31
    'E02', // 'K_2',					// &H32
    'E03', // 'K_3',					// &H33
    'E04', // 'K_4',					// &H34
    'E05', // 'K_5',					// &H35
    'E06', // 'K_6',					// &H36
    'E07', // 'K_7',					// &H37
    'E08', // 'K_8',					// &H38
    'E09', // 'K_9',					// &H39
    '', // 'K_?3A',				// &H3A
    '', // 'K_?3B',				// &H3B
    '', // 'K_?3C',				// &H3C
    '', // 'K_?3D',				// &H3D
    '', // 'K_?3E',				// &H3E
    '', // 'K_?3F',				// &H3F
    '', // 'K_?40',				// &H40

    'C01', // 'K_A',					// &H41
    'B05', // 'K_B',					// &H42
    'B03', // 'K_C',					// &H43
    'C03', // 'K_D',					// &H44
    'D03', // 'K_E',					// &H45
    'C04', // 'K_F',					// &H46
    'C05', // 'K_G',					// &H47
    'C06', // 'K_H',					// &H48
    'D08', // 'K_I',					// &H49
    'C07', // 'K_J',					// &H4A
    'C08', // 'K_K',					// &H4B
    'C09', // 'K_L',					// &H4C
    'B07', // 'K_M',					// &H4D
    'B06', // 'K_N',					// &H4E
    'D09', // 'K_O',					// &H4F
    'D10', // 'K_P',					// &H50
    'D01', // 'K_Q',					// &H51
    'D04', // 'K_R',					// &H52
    'C02', // 'K_S',					// &H53
    'D05', // 'K_T',					// &H54
    'D07', // 'K_U',					// &H55
    'B04', // 'K_V',					// &H56
    'D02', // 'K_W',					// &H57
    'B02', // 'K_X',					// &H58
    'D06', // 'K_Y',					// &H59
    'B01', // 'K_Z',					// &H5A
    '', // 'K_?5B',				// &H5B
    '', // 'K_?5C',				// &H5C
    '', // 'K_?5D',				// &H5D
    '', // 'K_?5E',				// &H5E
    '', // 'K_?5F',				// &H5F
    '', // 'K_NP0',				// &H60
    '', // 'K_NP1',				// &H61
    '', // 'K_NP2',				// &H62
    '', // 'K_NP3',				// &H63
    '', // 'K_NP4',				// &H64
    '', // 'K_NP5',				// &H65
    '', // 'K_NP6',				// &H66
    '', // 'K_NP7',				// &H67
    '', // 'K_NP8',				// &H68
    '', // 'K_NP9',				// &H69
    '', // 'K_NPSTAR',			// &H6A
    '', // 'K_NPPLUS',			// &H6B
    '', // 'K_SEPARATOR',			// &H6C
    '', // 'K_NPMINUS',			// &H6D
    '', // 'K_NPDOT',				// &H6E
    '', // 'K_NPSLASH',			// &H6F
    '', // 'K_F1',				// &H70
    '', // 'K_F2',				// &H71
    '', // 'K_F3',				// &H72
    '', // 'K_F4',				// &H73
    '', // 'K_F5',				// &H74
    '', // 'K_F6',				// &H75
    '', // 'K_F7',				// &H76
    '', // 'K_F8',				// &H77
    '', // 'K_F9',				// &H78
    '', // 'K_F10',				// &H79
    '', // 'K_F11',				// &H7A
    '', // 'K_F12',				// &H7B
    '', // 'K_F13',				// &H7C
    '', // 'K_F14',				// &H7D
    '', // 'K_F15',				// &H7E
    '', // 'K_F16',				// &H7F
    '', // 'K_F17',				// &H80
    '', // 'K_F18',				// &H81
    '', // 'K_F19',				// &H82
    '', // 'K_F20',				// &H83
    '', // 'K_F21',				// &H84
    '', // 'K_F22',				// &H85
    '', // 'K_F23',				// &H86
    '', // 'K_F24',				// &H87

    '', // 'K_?88',				// &H88
    '', // 'K_?89',				// &H89
    '', // 'K_?8A',				// &H8A
    '', // 'K_?8B',				// &H8B
    '', // 'K_?8C',				// &H8C
    '', // 'K_?8D',				// &H8D
    '', // 'K_?8E',				// &H8E
    '', // 'K_?8F',				// &H8F

    '', // 'K_NUMLOCK',			// &H90
    '', // 'K_SCROLL',			// &H91

    '', // 'K_?92',				// &H92
    '', // 'K_?93',				// &H93
    '', // 'K_?94',				// &H94
    '', // 'K_?95',				// &H95
    '', // 'K_?96',				// &H96
    '', // 'K_?97',				// &H97
    '', // 'K_?98',				// &H98
    '', // 'K_?99',				// &H99
    '', // 'K_?9A',				// &H9A
    '', // 'K_?9B',				// &H9B
    '', // 'K_?9C',				// &H9C
    '', // 'K_?9D',				// &H9D
    '', // 'K_?9E',				// &H9E
    '', // 'K_?9F',				// &H9F
    '', // 'K_?A0',				// &HA0
    '', // 'K_?A1',				// &HA1
    '', // 'K_?A2',				// &HA2
    '', // 'K_?A3',				// &HA3
    '', // 'K_?A4',				// &HA4
    '', // 'K_?A5',				// &HA5
    '', // 'K_?A6',				// &HA6
    '', // 'K_?A7',				// &HA7
    '', // 'K_?A8',				// &HA8
    '', // 'K_?A9',				// &HA9
    '', // 'K_?AA',				// &HAA
    '', // 'K_?AB',				// &HAB
    '', // 'K_?AC',				// &HAC
    '', // 'K_?AD',				// &HAD
    '', // 'K_?AE',				// &HAE
    '', // 'K_?AF',				// &HAF
    '', // 'K_?B0',				// &HB0
    '', // 'K_?B1',				// &HB1
    '', // 'K_?B2',				// &HB2
    '', // 'K_?B3',				// &HB3
    '', // 'K_?B4',				// &HB4
    '', // 'K_?B5',				// &HB5
    '', // 'K_?B6',				// &HB6
    '', // 'K_?B7',				// &HB7
    '', // 'K_?B8',				// &HB8
    '', // 'K_?B9',				// &HB9

    'C10', // 'K_COLON',				// &HBA
    'E12', // 'K_EQUAL',				// &HBB
    'B08', // 'K_COMMA',				// &HBC
    'E11', // 'K_HYPHEN',			// &HBD
    'B09', // 'K_PERIOD',			// &HBE
    'B10', // 'K_SLASH',				// &HBF
    'E00', // 'K_BKQUOTE',			// &HC0

    '', // 'K_?C1',				// &HC1
    '', // 'K_?C2',				// &HC2
    '', // 'K_?C3',				// &HC3
    '', // 'K_?C4',				// &HC4
    '', // 'K_?C5',				// &HC5
    '', // 'K_?C6',				// &HC6
    '', // 'K_?C7',				// &HC7
    '', // 'K_?C8',				// &HC8
    '', // 'K_?C9',				// &HC9
    '', // 'K_?CA',				// &HCA
    '', // 'K_?CB',				// &HCB
    '', // 'K_?CC',				// &HCC
    '', // 'K_?CD',				// &HCD
    '', // 'K_?CE',				// &HCE
    '', // 'K_?CF',				// &HCF
    '', // 'K_?D0',				// &HD0
    '', // 'K_?D1',				// &HD1
    '', // 'K_?D2',				// &HD2
    '', // 'K_?D3',				// &HD3
    '', // 'K_?D4',				// &HD4
    '', // 'K_?D5',				// &HD5
    '', // 'K_?D6',				// &HD6
    '', // 'K_?D7',				// &HD7
    '', // 'K_?D8',				// &HD8
    '', // 'K_?D9',				// &HD9
    '', // 'K_?DA',				// &HDA

    'D11', // 'K_LBRKT',				// &HDB
    'D13', // 'K_BKSLASH',			// &HDC
    'D12', // 'K_RBRKT',				// &HDD
    'C11', // 'K_QUOTE',				// &HDE
    '', // 'K_oDF',				// &HDF
    '', // 'K_oE0',				// &HE0
    '', // 'K_oE1',				// &HE1
    'B00', // 'K_oE2',				// &HE2
    '', // 'K_oE3',				// &HE3
    '', // 'K_oE4',				// &HE4

    '', // 'K_?E5',				// &HE5

    '', // 'K_oE6',				// &HE6

    '', // 'K_?E7',				// &HE7
    '', // 'K_?E8',				// &HE8

    '', // 'K_oE9',				// &HE9
    '', // 'K_oEA',				// &HEA
    '', // 'K_oEB',				// &HEB
    '', // 'K_oEC',				// &HEC
    '', // 'K_oED',				// &HED
    '', // 'K_oEE',				// &HEE
    '', // 'K_oEF',				// &HEF
    '', // 'K_oF0',				// &HF0
    '', // 'K_oF1',				// &HF1
    '', // 'K_oF2',				// &HF2
    '', // 'K_oF3',				// &HF3
    '', // 'K_oF4',				// &HF4
    '', // 'K_oF5',				// &HF5

    '', // 'K_?F6',				// &HF6
    '', // 'K_?F7',				// &HF7
    '', // 'K_?F8',				// &HF8
    '', // 'K_?F9',				// &HF9
    '', // 'K_?FA',				// &HFA
    '', // 'K_?FB',				// &HFB
    '', // 'K_?FC',				// &HFC
    '', // 'K_?FD',				// &HFD
    '', // 'K_?FE',				// &HFE
    ''  // 'K_?FF'				// &HFF
);


type
  TKeyNameRecord = record   // I4141   // I4142
    Name: string;
    Code: Integer;
  end;

const
  { Used only by KMW }
  AdditionalKeyNames: array[0..3] of TKeyNameRecord = (
    (Name: 'K_LCONTROL'; Code: $A2),
    (Name: 'K_RCONTROL'; Code: $A3),
    (Name: 'K_LALT'; Code: $A4),
    (Name: 'K_RALT'; Code: $A5)
  );

type
  TKeymanWebTouchStandardKey = (
    K_LOPT = 50001,
    K_ROPT = 50002,
    K_NUMERALS = 50003,
    K_SYMBOLS = 50004,
    K_CURRENCIES = 50005,
    K_UPPER = 50006,
    K_LOWER = 50007,
    K_ALPHA = 50008,
    K_SHIFTED = 50009,
    K_ALTGR = 50010,
    K_TABBACK = 50011,
    K_TABFWD = 50012);

const
  KeymanWebTouchStandardKeys: array[TKeymanWebTouchStandardKey] of string = (
    'K_LOPT',
    'K_ROPT',
    'K_NUMERALS',
    'K_SYMBOLS',
    'K_CURRENCIES',
    'K_UPPER',
    'K_LOWER',
    'K_ALPHA',
    'K_SHIFTED',
    'K_ALTGR',
    'K_TABBACK',
    'K_TABFWD');

implementation

uses
  System.SysUtils,
  System.TypInfo;

function FindVKeyName(const txt: string): Word;
var
  i: Integer;
  t: TKeymanWebTouchStandardKey;
begin
  if txt = '' then
    Exit($FFFF);

  for i := Low(VKeyNames) to High(VKeyNames) do
    if SameText(txt, VKeyNames[i]) or SameText(txt, VKeyISO9995Names[i]) then
      Exit(i);

  for t := Low(TKeymanWebTouchStandardKey) to High(TKeymanWebTouchStandardKey) do
    if SameText(txt, KeymanWebTouchStandardKeys[t]) then
      Exit(Ord(t));

  for i := 0 to High(AdditionalKeyNames) do
    if SameText(txt, AdditionalKeyNames[i].Name) then
      Exit(AdditionalKeyNames[i].Code);

  Result := $FFFF;
end;

end.
