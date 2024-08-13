$(function() {

  this.standardKeyNames = [
    'K_?00',      // &H0
    'K_LBUTTON',    // &H1
    'K_RBUTTON',    // &H2
    'K_CANCEL',       // &H3
    'K_MBUTTON',    // &H4
    'K_?05',      // &H5
    'K_?06',      // &H6
    'K_?07',      // &H7
    'K_BKSP',         // &H8
    'K_TAB',        // &H9
    'K_?0A',      // &HA
    'K_?0B',      // &HB
    'K_KP5',        // &HC
    'K_ENTER',      // &HD
    'K_?0E',      // &HE
    'K_?0F',      // &HF
    'K_SHIFT',      // &H10
    'K_CONTROL',    // &H11
    'K_ALT',      // &H12
    'K_PAUSE',      // &H13
    'K_CAPS',       // &H14
    'K_KANJI?15',     // &H15
    'K_KANJI?16',     // &H16
    'K_KANJI?17',     // &H17
    'K_KANJI?18',     // &H18
    'K_KANJI?19',     // &H19
    'K_?1A',      // &H1A
    'K_ESC',      // &H1B
    'K_KANJI?1C',     // &H1C
    'K_KANJI?1D',     // &H1D
    'K_KANJI?1E',     // &H1E
    'K_KANJI?1F',     // &H1F
    'K_SPACE',      // &H20
    'K_PGUP',       // &H21
    'K_PGDN',       // &H22
    'K_END',      // &H23
    'K_HOME',       // &H24
    'K_LEFT',       // &H25
    'K_UP',       // &H26
    'K_RIGHT',      // &H27
    'K_DOWN',       // &H28
    'K_SEL',      // &H29
    'K_PRINT',      // &H2A
    'K_EXEC',       // &H2B
    'K_PRTSCN',     // &H2C
    'K_INS',      // &H2D
    'K_DEL',      // &H2E
    'K_HELP',       // &H2F
    'K_0',        // &H30
    'K_1',        // &H31
    'K_2',        // &H32
    'K_3',        // &H33
    'K_4',        // &H34
    'K_5',        // &H35
    'K_6',        // &H36
    'K_7',        // &H37
    'K_8',        // &H38
    'K_9',        // &H39
    'K_?3A',      // &H3A
    'K_?3B',      // &H3B
    'K_?3C',      // &H3C
    'K_?3D',      // &H3D
    'K_?3E',      // &H3E
    'K_?3F',      // &H3F
    'K_?40',      // &H40

    'K_A',        // &H41
    'K_B',        // &H42
    'K_C',        // &H43
    'K_D',        // &H44
    'K_E',        // &H45
    'K_F',        // &H46
    'K_G',        // &H47
    'K_H',        // &H48
    'K_I',        // &H49
    'K_J',        // &H4A
    'K_K',        // &H4B
    'K_L',        // &H4C
    'K_M',        // &H4D
    'K_N',        // &H4E
    'K_O',        // &H4F
    'K_P',        // &H50
    'K_Q',        // &H51
    'K_R',        // &H52
    'K_S',        // &H53
    'K_T',        // &H54
    'K_U',        // &H55
    'K_V',        // &H56
    'K_W',        // &H57
    'K_X',        // &H58
    'K_Y',        // &H59
    'K_Z',        // &H5A
    'K_?5B',      // &H5B
    'K_?5C',      // &H5C
    'K_?5D',      // &H5D
    'K_?5E',      // &H5E
    'K_?5F',      // &H5F
    'K_NP0',      // &H60
    'K_NP1',      // &H61
    'K_NP2',      // &H62
    'K_NP3',      // &H63
    'K_NP4',      // &H64
    'K_NP5',      // &H65
    'K_NP6',      // &H66
    'K_NP7',      // &H67
    'K_NP8',      // &H68
    'K_NP9',      // &H69
    'K_NPSTAR',     // &H6A
    'K_NPPLUS',     // &H6B
    'K_SEPARATOR',    // &H6C
    'K_NPMINUS',    // &H6D
    'K_NPDOT',      // &H6E
    'K_NPSLASH',    // &H6F
    'K_F1',       // &H70
    'K_F2',       // &H71
    'K_F3',       // &H72
    'K_F4',       // &H73
    'K_F5',       // &H74
    'K_F6',       // &H75
    'K_F7',       // &H76
    'K_F8',       // &H77
    'K_F9',       // &H78
    'K_F10',      // &H79
    'K_F11',      // &H7A
    'K_F12',      // &H7B
    'K_F13',      // &H7C
    'K_F14',      // &H7D
    'K_F15',      // &H7E
    'K_F16',      // &H7F
    'K_F17',      // &H80
    'K_F18',      // &H81
    'K_F19',      // &H82
    'K_F20',      // &H83
    'K_F21',      // &H84
    'K_F22',      // &H85
    'K_F23',      // &H86
    'K_F24',      // &H87

    'K_?88',      // &H88
    'K_?89',      // &H89
    'K_?8A',      // &H8A
    'K_?8B',      // &H8B
    'K_?8C',      // &H8C
    'K_?8D',      // &H8D
    'K_?8E',      // &H8E
    'K_?8F',      // &H8F

    'K_NUMLOCK',    // &H90
    'K_SCROLL',     // &H91

    'K_?92',      // &H92
    'K_?93',      // &H93
    'K_?94',      // &H94
    'K_?95',      // &H95
    'K_?96',      // &H96
    'K_?97',      // &H97
    'K_?98',      // &H98
    'K_?99',      // &H99
    'K_?9A',      // &H9A
    'K_?9B',      // &H9B
    'K_?9C',      // &H9C
    'K_?9D',      // &H9D
    'K_?9E',      // &H9E
    'K_?9F',      // &H9F
    'K_?A0',      // &HA0
    'K_?A1',      // &HA1
    'K_?A2',      // &HA2
    'K_?A3',      // &HA3
    'K_?A4',      // &HA4
    'K_?A5',      // &HA5
    'K_?A6',      // &HA6
    'K_?A7',      // &HA7
    'K_?A8',      // &HA8
    'K_?A9',      // &HA9
    'K_?AA',      // &HAA
    'K_?AB',      // &HAB
    'K_?AC',      // &HAC
    'K_?AD',      // &HAD
    'K_?AE',      // &HAE
    'K_?AF',      // &HAF
    'K_?B0',      // &HB0
    'K_?B1',      // &HB1
    'K_?B2',      // &HB2
    'K_?B3',      // &HB3
    'K_?B4',      // &HB4
    'K_?B5',      // &HB5
    'K_?B6',      // &HB6
    'K_?B7',      // &HB7
    'K_?B8',      // &HB8
    'K_?B9',      // &HB9

    'K_COLON',      // &HBA
    'K_EQUAL',      // &HBB
    'K_COMMA',      // &HBC
    'K_HYPHEN',     // &HBD
    'K_PERIOD',     // &HBE
    'K_SLASH',      // &HBF
    'K_BKQUOTE',    // &HC0

    'K_?C1',      // &HC1
    'K_?C2',      // &HC2
    'K_?C3',      // &HC3
    'K_?C4',      // &HC4
    'K_?C5',      // &HC5
    'K_?C6',      // &HC6
    'K_?C7',      // &HC7
    'K_?C8',      // &HC8
    'K_?C9',      // &HC9
    'K_?CA',      // &HCA
    'K_?CB',      // &HCB
    'K_?CC',      // &HCC
    'K_?CD',      // &HCD
    'K_?CE',      // &HCE
    'K_?CF',      // &HCF
    'K_?D0',      // &HD0
    'K_?D1',      // &HD1
    'K_?D2',      // &HD2
    'K_?D3',      // &HD3
    'K_?D4',      // &HD4
    'K_?D5',      // &HD5
    'K_?D6',      // &HD6
    'K_?D7',      // &HD7
    'K_?D8',      // &HD8
    'K_?D9',      // &HD9
    'K_?DA',      // &HDA

    'K_LBRKT',      // &HDB
    'K_BKSLASH',    // &HDC
    'K_RBRKT',      // &HDD
    'K_QUOTE',      // &HDE
    'K_oDF',      // &HDF
    'K_oE0',      // &HE0
    'K_oE1',      // &HE1
    'K_oE2',      // &HE2
    'K_oE3',      // &HE3
    'K_oE4',      // &HE4

    'K_?E5',      // &HE5

    'K_oE6',      // &HE6

    'K_?E7',      // &HE7
    'K_?E8',      // &HE8

    'K_oE9',      // &HE9
    'K_oEA',      // &HEA
    'K_oEB',      // &HEB
    'K_oEC',      // &HEC
    'K_oED',      // &HED
    'K_oEE',      // &HEE
    'K_oEF',      // &HEF
    'K_oF0',      // &HF0
    'K_oF1',      // &HF1
    'K_oF2',      // &HF2
    'K_oF3',      // &HF3
    'K_oF4',      // &HF4
    'K_oF5',      // &HF5

    'K_?F6',      // &HF6
    'K_?F7',      // &HF7
    'K_?F8',      // &HF8
    'K_?F9',      // &HF9
    'K_?FA',      // &HFA
    'K_?FB',      // &HFB
    'K_?FC',      // &HFC
    'K_?FD',      // &HFD
    'K_?FE',      // &HFE
    'K_?FF'         // &HFF
  ];

  this.standardKeyCaps = [
    ['',''], // 'K_?00',      // &H0
    ['',''], // 'K_LBUTTON',    // &H1
    ['',''], // 'K_RBUTTON',    // &H2
    ['',''], // 'K_CANCEL',       // &H3
    ['',''], // 'K_MBUTTON',    // &H4
    ['',''], // 'K_?05',      // &H5
    ['',''], // 'K_?06',      // &H6
    ['',''], // 'K_?07',      // &H7
    ['',''], // 'K_BKSP',         // &H8
    ['',''], // 'K_TAB',        // &H9
    ['',''], // 'K_?0A',      // &HA
    ['',''], // 'K_?0B',      // &HB
    ['',''], // 'K_KP5',        // &HC
    ['',''], // 'K_ENTER',      // &HD
    ['',''], // 'K_?0E',      // &HE
    ['',''], // 'K_?0F',      // &HF
    ['',''], // 'K_SHIFT',      // &H10
    ['',''], // 'K_CONTROL',    // &H11
    ['',''], // 'K_ALT',      // &H12
    ['',''], // 'K_PAUSE',      // &H13
    ['',''], // 'K_CAPS',       // &H14
    ['',''], // 'K_KANJI?15',     // &H15
    ['',''], // 'K_KANJI?16',     // &H16
    ['',''], // 'K_KANJI?17',     // &H17
    ['',''], // 'K_KANJI?18',     // &H18
    ['',''], // 'K_KANJI?19',     // &H19
    ['',''], // 'K_?1A',      // &H1A
    ['',''], // 'K_ESC',      // &H1B
    ['',''], // 'K_KANJI?1C',     // &H1C
    ['',''], // 'K_KANJI?1D',     // &H1D
    ['',''], // 'K_KANJI?1E',     // &H1E
    ['',''], // 'K_KANJI?1F',     // &H1F
    [' ',' '], // 'K_SPACE',      // &H20
    ['',''], // 'K_PGUP',       // &H21
    ['',''], // 'K_PGDN',       // &H22
    ['',''], // 'K_END',      // &H23
    ['',''], // 'K_HOME',       // &H24
    ['',''], // 'K_LEFT',       // &H25
    ['',''], // 'K_UP',       // &H26
    ['',''], // 'K_RIGHT',      // &H27
    ['',''], // 'K_DOWN',       // &H28
    ['',''], // 'K_SEL',      // &H29
    ['',''], // 'K_PRINT',      // &H2A
    ['',''], // 'K_EXEC',       // &H2B
    ['',''], // 'K_PRTSCN',     // &H2C
    ['',''], // 'K_INS',      // &H2D
    ['',''], // 'K_DEL',      // &H2E
    ['',''], // 'K_HELP',       // &H2F
    ['0',')'], // 'K_0',        // &H30
    ['1','!'], // 'K_1',        // &H31
    ['2','@'], // 'K_2',        // &H32
    ['3','#'], // 'K_3',        // &H33
    ['4','$'], // 'K_4',        // &H34
    ['5','%'], // 'K_5',        // &H35
    ['6','^'], // 'K_6',        // &H36
    ['7','&'], // 'K_7',        // &H37
    ['8','*'], // 'K_8',        // &H38
    ['9','('], // 'K_9',        // &H39
    ['',''], // 'K_?3A',      // &H3A
    ['',''], // 'K_?3B',      // &H3B
    ['',''], // 'K_?3C',      // &H3C
    ['',''], // 'K_?3D',      // &H3D
    ['',''], // 'K_?3E',      // &H3E
    ['',''], // 'K_?3F',      // &H3F
    ['',''], // 'K_?40',      // &H40

    ['a','A'], // 'K_A',        // &H41
    ['b','B'], // 'K_B',        // &H42
    ['c','C'], // 'K_C',        // &H43
    ['d','D'], // 'K_D',        // &H44
    ['e','E'], // 'K_E',        // &H45
    ['f','F'], // 'K_F',        // &H46
    ['g','G'], // 'K_G',        // &H47
    ['h','H'], // 'K_H',        // &H48
    ['i','I'], // 'K_I',        // &H49
    ['j','J'], // 'K_J',        // &H4A
    ['k','K'], // 'K_K',        // &H4B
    ['l','L'], // 'K_L',        // &H4C
    ['m','M'], // 'K_M',        // &H4D
    ['n','N'], // 'K_N',        // &H4E
    ['o','O'], // 'K_O',        // &H4F
    ['p','P'], // 'K_P',        // &H50
    ['q','Q'], // 'K_Q',        // &H51
    ['r','R'], // 'K_R',        // &H52
    ['s','S'], // 'K_S',        // &H53
    ['t','T'], // 'K_T',        // &H54
    ['u','U'], // 'K_U',        // &H55
    ['v','V'], // 'K_V',        // &H56
    ['w','W'], // 'K_W',        // &H57
    ['x','X'], // 'K_X',        // &H58
    ['y','Y'], // 'K_Y',        // &H59
    ['z','Z'], // 'K_Z',        // &H5A
    ['',''], // 'K_?5B',      // &H5B
    ['',''], // 'K_?5C',      // &H5C
    ['',''], // 'K_?5D',      // &H5D
    ['',''], // 'K_?5E',      // &H5E
    ['',''], // 'K_?5F',      // &H5F
    ['',''], // 'K_NP0',      // &H60
    ['',''], // 'K_NP1',      // &H61
    ['',''], // 'K_NP2',      // &H62
    ['',''], // 'K_NP3',      // &H63
    ['',''], // 'K_NP4',      // &H64
    ['',''], // 'K_NP5',      // &H65
    ['',''], // 'K_NP6',      // &H66
    ['',''], // 'K_NP7',      // &H67
    ['',''], // 'K_NP8',      // &H68
    ['',''], // 'K_NP9',      // &H69
    ['',''], // 'K_NPSTAR',     // &H6A
    ['',''], // 'K_NPPLUS',     // &H6B
    ['',''], // 'K_SEPARATOR',    // &H6C
    ['',''], // 'K_NPMINUS',    // &H6D
    ['',''], // 'K_NPDOT',      // &H6E
    ['',''], // 'K_NPSLASH',    // &H6F
    ['',''], // 'K_F1',       // &H70
    ['',''], // 'K_F2',       // &H71
    ['',''], // 'K_F3',       // &H72
    ['',''], // 'K_F4',       // &H73
    ['',''], // 'K_F5',       // &H74
    ['',''], // 'K_F6',       // &H75
    ['',''], // 'K_F7',       // &H76
    ['',''], // 'K_F8',       // &H77
    ['',''], // 'K_F9',       // &H78
    ['',''], // 'K_F10',      // &H79
    ['',''], // 'K_F11',      // &H7A
    ['',''], // 'K_F12',      // &H7B
    ['',''], // 'K_F13',      // &H7C
    ['',''], // 'K_F14',      // &H7D
    ['',''], // 'K_F15',      // &H7E
    ['',''], // 'K_F16',      // &H7F
    ['',''], // 'K_F17',      // &H80
    ['',''], // 'K_F18',      // &H81
    ['',''], // 'K_F19',      // &H82
    ['',''], // 'K_F20',      // &H83
    ['',''], // 'K_F21',      // &H84
    ['',''], // 'K_F22',      // &H85
    ['',''], // 'K_F23',      // &H86
    ['',''], // 'K_F24',      // &H87

    ['',''], // 'K_?88',      // &H88
    ['',''], // 'K_?89',      // &H89
    ['',''], // 'K_?8A',      // &H8A
    ['',''], // 'K_?8B',      // &H8B
    ['',''], // 'K_?8C',      // &H8C
    ['',''], // 'K_?8D',      // &H8D
    ['',''], // 'K_?8E',      // &H8E
    ['',''], // 'K_?8F',      // &H8F

    ['',''], // 'K_NUMLOCK',    // &H90
    ['',''], // 'K_SCROLL',     // &H91

    ['',''], // 'K_?92',      // &H92
    ['',''], // 'K_?93',      // &H93
    ['',''], // 'K_?94',      // &H94
    ['',''], // 'K_?95',      // &H95
    ['',''], // 'K_?96',      // &H96
    ['',''], // 'K_?97',      // &H97
    ['',''], // 'K_?98',      // &H98
    ['',''], // 'K_?99',      // &H99
    ['',''], // 'K_?9A',      // &H9A
    ['',''], // 'K_?9B',      // &H9B
    ['',''], // 'K_?9C',      // &H9C
    ['',''], // 'K_?9D',      // &H9D
    ['',''], // 'K_?9E',      // &H9E
    ['',''], // 'K_?9F',      // &H9F
    ['',''], // 'K_?A0',      // &HA0
    ['',''], // 'K_?A1',      // &HA1
    ['',''], // 'K_?A2',      // &HA2
    ['',''], // 'K_?A3',      // &HA3
    ['',''], // 'K_?A4',      // &HA4
    ['',''], // 'K_?A5',      // &HA5
    ['',''], // 'K_?A6',      // &HA6
    ['',''], // 'K_?A7',      // &HA7
    ['',''], // 'K_?A8',      // &HA8
    ['',''], // 'K_?A9',      // &HA9
    ['',''], // 'K_?AA',      // &HAA
    ['',''], // 'K_?AB',      // &HAB
    ['',''], // 'K_?AC',      // &HAC
    ['',''], // 'K_?AD',      // &HAD
    ['',''], // 'K_?AE',      // &HAE
    ['',''], // 'K_?AF',      // &HAF
    ['',''], // 'K_?B0',      // &HB0
    ['',''], // 'K_?B1',      // &HB1
    ['',''], // 'K_?B2',      // &HB2
    ['',''], // 'K_?B3',      // &HB3
    ['',''], // 'K_?B4',      // &HB4
    ['',''], // 'K_?B5',      // &HB5
    ['',''], // 'K_?B6',      // &HB6
    ['',''], // 'K_?B7',      // &HB7
    ['',''], // 'K_?B8',      // &HB8
    ['',''], // 'K_?B9',      // &HB9

    [';',':'], // 'K_COLON',      // &HBA
    ['=','+'], // 'K_EQUAL',      // &HBB
    [',','<'], // 'K_COMMA',      // &HBC
    ['-','_'], // 'K_HYPHEN',     // &HBD
    ['.','>'], // 'K_PERIOD',     // &HBE
    ['/','?'], // 'K_SLASH',      // &HBF
    ['`','~'], // 'K_BKQUOTE',    // &HC0

    ['',''], // 'K_?C1',      // &HC1
    ['',''], // 'K_?C2',      // &HC2
    ['',''], // 'K_?C3',      // &HC3
    ['',''], // 'K_?C4',      // &HC4
    ['',''], // 'K_?C5',      // &HC5
    ['',''], // 'K_?C6',      // &HC6
    ['',''], // 'K_?C7',      // &HC7
    ['',''], // 'K_?C8',      // &HC8
    ['',''], // 'K_?C9',      // &HC9
    ['',''], // 'K_?CA',      // &HCA
    ['',''], // 'K_?CB',      // &HCB
    ['',''], // 'K_?CC',      // &HCC
    ['',''], // 'K_?CD',      // &HCD
    ['',''], // 'K_?CE',      // &HCE
    ['',''], // 'K_?CF',      // &HCF
    ['',''], // 'K_?D0',      // &HD0
    ['',''], // 'K_?D1',      // &HD1
    ['',''], // 'K_?D2',      // &HD2
    ['',''], // 'K_?D3',      // &HD3
    ['',''], // 'K_?D4',      // &HD4
    ['',''], // 'K_?D5',      // &HD5
    ['',''], // 'K_?D6',      // &HD6
    ['',''], // 'K_?D7',      // &HD7
    ['',''], // 'K_?D8',      // &HD8
    ['',''], // 'K_?D9',      // &HD9
    ['',''], // 'K_?DA',      // &HDA

    ['[','{'], // 'K_LBRKT',      // &HDB
    ['\\','|'], // 'K_BKSLASH',    // &HDC
    [']','}'], // 'K_RBRKT',      // &HDD
    ['\'','"'], // 'K_QUOTE',      // &HDE
    ['',''], // 'K_oDF',      // &HDF
    ['',''], // 'K_oE0',      // &HE0
    ['',''], // 'K_oE1',      // &HE1
    ['\\','|'], // 'K_oE2',      // &HE2
    ['',''], // 'K_oE3',      // &HE3
    ['',''], // 'K_oE4',      // &HE4

    ['',''], // 'K_?E5',      // &HE5

    ['',''], // 'K_oE6',      // &HE6

    ['',''], // 'K_?E7',      // &HE7
    ['',''], // 'K_?E8',      // &HE8

    ['',''], // 'K_oE9',      // &HE9
    ['',''], // 'K_oEA',      // &HEA
    ['',''], // 'K_oEB',      // &HEB
    ['',''], // 'K_oEC',      // &HEC
    ['',''], // 'K_oED',      // &HED
    ['',''], // 'K_oEE',      // &HEE
    ['',''], // 'K_oEF',      // &HEF
    ['',''], // 'K_oF0',      // &HF0
    ['',''], // 'K_oF1',      // &HF1
    ['',''], // 'K_oF2',      // &HF2
    ['',''], // 'K_oF3',      // &HF3
    ['',''], // 'K_oF4',      // &HF4
    ['',''], // 'K_oF5',      // &HF5

    ['',''], // 'K_?F6',      // &HF6
    ['',''], // 'K_?F7',      // &HF7
    ['',''], // 'K_?F8',      // &HF8
    ['',''], // 'K_?F9',      // &HF9
    ['',''], // 'K_?FA',      // &HFA
    ['',''], // 'K_?FB',      // &HFB
    ['',''], // 'K_?FC',      // &HFC
    ['',''], // 'K_?FD',      // &HFD
    ['',''], // 'K_?FE',      // &HFE
    ['',''], // 'K_?FF'       // &HFF
  ];

  this.lookupKeyNames = [];

  for (var i = 0; i < this.standardKeyNames.length; i++) {
    if (this.standardKeyNames[i].indexOf('?') < 0) {
      this.lookupKeyNames.push(this.standardKeyNames[i]);
    }
  }
  this.lookupKeyNames.sort();

  // Defines the PUA code mapping for the various 'special' modifier/control/non-printing keys on keyboards.
  // This is lifted directly from specialCharacters.ts and must be kept in sync. See also CompileKeymanWeb.pas: CSpecialText10
  this.specialCharacters = {
    '*Shift*':    8,
    '*Enter*':    5,
    '*Tab*':      6,
    '*BkSp*':     4,
    '*Menu*':     11,
    '*Hide*':     10,
    '*Alt*':      25,
    '*Ctrl*':     1,
    '*Caps*':     3,
    '*ABC*':      16,
    '*abc*':      17,
    '*123*':      19,
    '*Symbol*':   21,
    '*Currency*': 20,
    '*Shifted*':  9,
    '*AltGr*':    2,
    '*TabLeft*':  7,
    '*LAlt*':     0x56,
    '*RAlt*':     0x57,
    '*LCtrl*':    0x58,
    '*RCtrl*':    0x59,
    '*LAltCtrl*':       0x60,
    '*RAltCtrl*':       0x61,
    '*LAltCtrlShift*':  0x62,
    '*RAltCtrlShift*':  0x63,
    '*AltShift*':       0x64,
    '*CtrlShift*':      0x65,
    '*AltCtrlShift*':   0x66,
    '*LAltShift*':      0x67,
    '*RAltShift*':      0x68,
    '*LCtrlShift*':     0x69,
    '*RCtrlShift*':     0x70,
    // Added in Keyman 14.0
    '*LTREnter*':       0x05, // Default alias of '*Enter*'.
    '*LTRBkSp*':        0x04, // Default alias of '*BkSp*'.
    '*RTLEnter*':       0x71,
    '*RTLBkSp*':        0x72,
    '*ShiftLock*':      0x73,
    '*ShiftedLock*':    0x74,
    '*ZWNJ*':           0x75, // * If this one is specified, auto-detection will kick in.
    '*ZWNJiOS*':        0x75, //   The iOS version will be used by default, but the
    '*ZWNJAndroid*':    0x76, //   Android platform has its own default glyph.
    // Added in Keyman 17.0.
    // Reference: https://github.com/silnrsi/font-symchar/blob/v4.000/documentation/encoding.md
    '*ZWNJGeneric*':    0x79, // Generic version of ZWNJ (no override)
    '*Sp*':             0x80, // Space
    '*NBSp*':           0x82, // No-break Space
    '*NarNBSp*':        0x83, // Narrow No-break Space
    '*EnQ*':            0x84, // En Quad
    '*EmQ*':            0x85, // Em Quad
    '*EnSp*':           0x86, // En Space
    '*EmSp*':           0x87, // Em Space
    // TODO: Skipping #-per-em-space
    '*PunctSp*':        0x8c, // Punctuation Space
    '*ThSp*':           0x8d, // Thin Space
    '*HSp*':            0x8e, // Hair Space
    '*ZWSp*':           0x81, // Zero Width Space
    '*ZWJ*':            0x77, // Zero Width Joiner
    '*WJ*':             0x78, // Word Joiner
    '*CGJ*':            0x7a, // Combining Grapheme Joiner
    '*LTRM*':           0x90, // Left-to-right Mark
    '*RTLM*':           0x91, // Right-to-left Mark
    '*SH*':             0xa1, // Soft Hyphen
    '*HTab*':           0xa2, // Horizontal Tabulation
    // TODO: Skipping size references
  };

  this.specialKeyNames = Object.entries(this.specialCharacters).map(ch => ch[0]);

  this.presentations = {
    "tablet-ipad-landscape": { "x": 829, "y": 299, "name": "iPad (landscape)" }, // 829x622 = iPad tablet box size; (97,101)-(926,723)
    "tablet-ipad-portrait": { "x": 605, "y": 300, "name": "iPad (portrait)" }, // 605x806 = iPad tablet box size; (98,94)-(703,900)
    "phone-iphone5-landscape": { "x": 731, "y": 196, "name": "iPhone 5 (landscape)" }, // 731x412 = iPhone box size; (144,39)-(875,451)
    "phone-iphone5-portrait": { "x": 526, "y": 266, "name": "iPhone 5 (portrait)"}, // 528x936 = iPhone box size; (90,204)-(618,1040)
    "desktop": { "x": 640, "y": 300, "name": "Desktop" },
  };

  this.keyMargin = 15;

  // from kmwosk.js:
  this.modifierCodes = {
    "LCTRL":0x0001,
    "RCTRL":0x0002,
    "LALT":0x0004,
    "RALT":0x0008,
    "SHIFT":0x0010,
    "CTRL":0x0020,
    "ALT":0x0040,
    "CAPS":0x0100,
    "NO_CAPS":0x0200
    /*"NUM_LOCK":0x0400, We don't support these in the designer
    "NO_NUM_LOCK":0x0800,
    "SCROLL_LOCK":0x1000,
    "NO_SCROLL_LOCK":0x2000,
    "VIRTUAL_KEY":0x4000*/
  };

  // Lists the combinations that we allow users to use. Some are mutually exclusive,
  // such as Left+Right modifiers, or chiral and non-chiral modifiers (excl. Shift)
  this.validModifierCombinations = [
    0,

    this.modifierCodes.LCTRL,
    this.modifierCodes.RCTRL,

    this.modifierCodes.LALT,
    this.modifierCodes.LCTRL | this.modifierCodes.LALT,

    this.modifierCodes.RALT,
    this.modifierCodes.RCTRL | this.modifierCodes.RALT,

    this.modifierCodes.SHIFT,

    this.modifierCodes.LCTRL | this.modifierCodes.SHIFT,
    this.modifierCodes.LALT | this.modifierCodes.SHIFT,
    this.modifierCodes.LCTRL | this.modifierCodes.LALT | this.modifierCodes.SHIFT,

    this.modifierCodes.RCTRL | this.modifierCodes.SHIFT,
    this.modifierCodes.RALT | this.modifierCodes.SHIFT,
    this.modifierCodes.RCTRL | this.modifierCodes.RALT | this.modifierCodes.SHIFT,

    this.modifierCodes.CTRL,
    this.modifierCodes.CTRL | this.modifierCodes.SHIFT,

    this.modifierCodes.ALT,
    this.modifierCodes.CTRL | this.modifierCodes.ALT,
    this.modifierCodes.SHIFT | this.modifierCodes.ALT,
    this.modifierCodes.SHIFT | this.modifierCodes.CTRL | this.modifierCodes.ALT,
    this.modifierCodes.CAPS
  ];

  this.minimalModifierCombinations = [
    0,
    this.modifierCodes.RALT,
    this.modifierCodes.SHIFT,
    this.modifierCodes.RALT | this.modifierCodes.SHIFT,
    this.modifierCodes.CAPS
  ];

  this.showAllModifierCombinations = false;

  // Add CAPS variants for all of the above
  (function addCapsCombinations(validCombinations, CAPS) {
    const n = validCombinations.length;
    for(let i = 0; i < n; i++) {
      const newCombination = validCombinations[i] | CAPS;
      if(!validCombinations.includes(newCombination)) {
        validCombinations.push(newCombination);
      }
    }
  })(this.validModifierCombinations, this.modifierCodes.CAPS);

  this.modifierNames = [
    'leftctrl',   // 0x001
    'rightctrl',  // 0x002
    'leftalt',    // 0x004
    'rightalt',   // 0x008
    'shift',      // 0x010
    'ctrl',       // 0x020
    'alt',        // 0x040
    '',           // 0x080 - reserved
    'caps',       // 0x100
    'nocaps'      // 0x200
  ];

  /**
  *  Get kebab-cased full name of a modifier combination given a bitmask
  *
  *  @param  {Number}  c
  *  @return {string}
  **/
  this.getModifierCombinationName = function(c) {
    var r = '';
    if(c == 0) return 'default';
    for(var i = 0; i < this.modifierNames.length; i++) {
      if(c & (1<<i)) r += (r == '' ? '' : '-') + this.modifierNames[i];
    }
    return r;
  };

}.bind(builder));
