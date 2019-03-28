module.exports = function(modifier, key) {
  const VKeyNames = [ // from vkeys.h
    // Key Codes
    "K_?00",				// &H0
    "K_LBUTTON",			// &H1
    "K_RBUTTON",			// &H2
    "K_CANCEL",		   	// &H3
    "K_MBUTTON",			// &H4
    "K_?05",				// &H5
    "K_?06",				// &H6
    "K_?07",				// &H7
    "K_BKSP",	    		// &H8
    "K_TAB",	    		// &H9
    "K_?0A",				// &HA
    "K_?0B",				// &HB
    "K_KP5",		    	// &HC
    "K_ENTER",				// &HD
    "K_?0E",				// &HE
    "K_?0F",				// &HF
    "K_SHIFT",				// &H10
    "K_CONTROL",			// &H11
    "K_ALT",				// &H12
    "K_PAUSE",				// &H13
    "K_CAPS",				// &H14
    "K_KANJI?15",			// &H15
    "K_KANJI?16",			// &H16
    "K_KANJI?17",			// &H17
    "K_KANJI?18",			// &H18
    "K_KANJI?19",			// &H19
    "K_?1A",				// &H1A
    "K_ESC",				// &H1B
    "K_KANJI?1C",			// &H1C
    "K_KANJI?1D",			// &H1D
    "K_KANJI?1E",			// &H1E
    "K_KANJI?1F",			// &H1F
    "K_SPACE",				// &H20
    "K_PGUP",				// &H21
    "K_PGDN",				// &H22
    "K_END",				// &H23
    "K_HOME",				// &H24
    "K_LEFT",				// &H25
    "K_UP",				// &H26
    "K_RIGHT",				// &H27
    "K_DOWN",				// &H28
    "K_SEL",				// &H29
    "K_PRINT",				// &H2A
    "K_EXEC",				// &H2B
    "K_PRTSCN",			// &H2C
    "K_INS",				// &H2D
    "K_DEL",				// &H2E
    "K_HELP",				// &H2F
    "K_0",					// &H30
    "K_1",					// &H31
    "K_2",					// &H32
    "K_3",					// &H33
    "K_4",					// &H34
    "K_5",					// &H35
    "K_6",					// &H36
    "K_7",					// &H37
    "K_8",					// &H38
    "K_9",					// &H39
    "K_?3A",				// &H3A
    "K_?3B",				// &H3B
    "K_?3C",				// &H3C
    "K_?3D",				// &H3D
    "K_?3E",				// &H3E
    "K_?3F",				// &H3F
    "K_?40",				// &H40
  
    "K_A",					// &H41
    "K_B",					// &H42
    "K_C",					// &H43
    "K_D",					// &H44
    "K_E",					// &H45
    "K_F",					// &H46
    "K_G",					// &H47
    "K_H",					// &H48
    "K_I",					// &H49
    "K_J",					// &H4A
    "K_K",					// &H4B
    "K_L",					// &H4C
    "K_M",					// &H4D
    "K_N",					// &H4E
    "K_O",					// &H4F
    "K_P",					// &H50
    "K_Q",					// &H51
    "K_R",					// &H52
    "K_S",					// &H53
    "K_T",					// &H54
    "K_U",					// &H55
    "K_V",					// &H56
    "K_W",					// &H57
    "K_X",					// &H58
    "K_Y",					// &H59
    "K_Z",					// &H5A
    "K_?5B",				// &H5B
    "K_?5C",				// &H5C
    "K_?5D",				// &H5D
    "K_?5E",				// &H5E
    "K_?5F",				// &H5F
    "K_NP0",				// &H60
    "K_NP1",				// &H61
    "K_NP2",				// &H62
    "K_NP3",				// &H63
    "K_NP4",				// &H64
    "K_NP5",				// &H65
    "K_NP6",				// &H66
    "K_NP7",				// &H67
    "K_NP8",				// &H68
    "K_NP9",				// &H69
    "K_NPSTAR",			// &H6A
    "K_NPPLUS",			// &H6B
    "K_SEPARATOR",			// &H6C
    "K_NPMINUS",			// &H6D
    "K_NPDOT",				// &H6E
    "K_NPSLASH",			// &H6F
    "K_F1",				// &H70
    "K_F2",				// &H71
    "K_F3",				// &H72
    "K_F4",				// &H73
    "K_F5",				// &H74
    "K_F6",				// &H75
    "K_F7",				// &H76
    "K_F8",				// &H77
    "K_F9",				// &H78
    "K_F10",				// &H79
    "K_F11",				// &H7A
    "K_F12",				// &H7B
    "K_F13",				// &H7C
    "K_F14",				// &H7D
    "K_F15",				// &H7E
    "K_F16",				// &H7F
    "K_F17",				// &H80
    "K_F18",				// &H81
    "K_F19",				// &H82
    "K_F20",				// &H83
    "K_F21",				// &H84
    "K_F22",				// &H85
    "K_F23",				// &H86
    "K_F24",				// &H87
  
    "K_?88",				// &H88
    "K_?89",				// &H89
    "K_?8A",				// &H8A
    "K_?8B",				// &H8B
    "K_?8C",				// &H8C
    "K_?8D",				// &H8D
    "K_?8E",				// &H8E
    "K_?8F",				// &H8F
  
    "K_NUMLOCK",			// &H90
    "K_SCROLL",			// &H91
  
    "K_?92",				// &H92
    "K_?93",				// &H93
    "K_?94",				// &H94
    "K_?95",				// &H95
    "K_?96",				// &H96
    "K_?97",				// &H97
    "K_?98",				// &H98
    "K_?99",				// &H99
    "K_?9A",				// &H9A
    "K_?9B",				// &H9B
    "K_?9C",				// &H9C
    "K_?9D",				// &H9D
    "K_?9E",				// &H9E
    "K_?9F",				// &H9F
    "K_?A0",				// &HA0
    "K_?A1",				// &HA1
    "K_?A2",				// &HA2
    "K_?A3",				// &HA3
    "K_?A4",				// &HA4
    "K_?A5",				// &HA5
    "K_?A6",				// &HA6
    "K_?A7",				// &HA7
    "K_?A8",				// &HA8
    "K_?A9",				// &HA9
    "K_?AA",				// &HAA
    "K_?AB",				// &HAB
    "K_?AC",				// &HAC
    "K_?AD",				// &HAD
    "K_?AE",				// &HAE
    "K_?AF",				// &HAF
    "K_?B0",				// &HB0
    "K_?B1",				// &HB1
    "K_?B2",				// &HB2
    "K_?B3",				// &HB3
    "K_?B4",				// &HB4
    "K_?B5",				// &HB5
    "K_?B6",				// &HB6
    "K_?B7",				// &HB7
    "K_?B8",				// &HB8
    "K_?B9",				// &HB9
  
    "K_COLON",				// &HBA
    "K_EQUAL",				// &HBB
    "K_COMMA",				// &HBC
    "K_HYPHEN",			// &HBD
    "K_PERIOD",			// &HBE
    "K_SLASH",				// &HBF
    "K_BKQUOTE",			// &HC0
  
    "K_?C1",				// &HC1
    "K_?C2",				// &HC2
    "K_?C3",				// &HC3
    "K_?C4",				// &HC4
    "K_?C5",				// &HC5
    "K_?C6",				// &HC6
    "K_?C7",				// &HC7
    "K_?C8",				// &HC8
    "K_?C9",				// &HC9
    "K_?CA",				// &HCA
    "K_?CB",				// &HCB
    "K_?CC",				// &HCC
    "K_?CD",				// &HCD
    "K_?CE",				// &HCE
    "K_?CF",				// &HCF
    "K_?D0",				// &HD0
    "K_?D1",				// &HD1
    "K_?D2",				// &HD2
    "K_?D3",				// &HD3
    "K_?D4",				// &HD4
    "K_?D5",				// &HD5
    "K_?D6",				// &HD6
    "K_?D7",				// &HD7
    "K_?D8",				// &HD8
    "K_?D9",				// &HD9
    "K_?DA",				// &HDA
  
    "K_LBRKT",				// &HDB
    "K_BKSLASH",			// &HDC
    "K_RBRKT",				// &HDD
    "K_QUOTE",				// &HDE
    "K_oDF",				// &HDF
    "K_oE0",				// &HE0
    "K_oE1",				// &HE1
    "K_oE2",				// &HE2
    "K_oE3",				// &HE3
    "K_oE4",				// &HE4
  
    "K_?E5",				// &HE5
  
    "K_oE6",				// &HE6
  
    "K_?E7",				// &HE7
    "K_?E8",				// &HE8
  
    "K_oE9",				// &HE9
    "K_oEA",				// &HEA
    "K_oEB",				// &HEB
    "K_oEC",				// &HEC
    "K_oED",				// &HED
    "K_oEE",				// &HEE
    "K_oEF",				// &HEF
    "K_oF0",				// &HF0
    "K_oF1",				// &HF1
    "K_oF2",				// &HF2
    "K_oF3",				// &HF3
    "K_oF4",				// &HF4
    "K_oF5",				// &HF5
  
    "K_?F6",				// &HF6
    "K_?F7",				// &HF7
    "K_?F8",				// &HF8
    "K_?F9",				// &HF9
    "K_?FA",				// &HFA
    "K_?FB",				// &HFB
    "K_?FC",				// &HFC
    "K_?FD",				// &HFD
    "K_?FE",				// &HFE
    "K_?FF"				// &HFF
  ];
  
  const modifierNames = [ // from kmanalyze.cpp
    { name: "LCTRL", id: 0x0001 },
    { name: "RCTRL", id: 0x0002 },
    { name: "LALT", id: 0x0004 },
    { name: "RALT", id: 0x0008 },
    { name: "SHIFT", id: 0x0010 },
    { name: "CTRL", id: 0x0020 },
    { name: "ALT", id: 0x0040 },
    { name: "CAPS", id: 0x0100 },
    { name: "NO_CAPS", id: 0x0200 },
    { name: "NUM_LOCK", id: 0x0400 },
    { name: "NO_NUM_LOCK", id: 0x0800 },
    { name: "SCROLL_LOCK", id: 0x1000 },
    { name: "NO_SCROLL_LOCK", id: 0x2000 },
    { name: "VIRTUAL_KEY", id: 0x4000 }
  ];
  
  modifier &= ~0x8000;
  let result = '[';

  for(let i = 0; i < modifierNames.length; i++) {
    if(modifier & modifierNames[i].id) {
      result += modifierNames[i].name + ' ';
    }
  }

  if(key < VKeyNames.length) {
    result += VKeyNames[key];
  } else {
    result += key.toString();
  }

  return result + ']';
};