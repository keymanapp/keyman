unit HotkeyUtils;

interface

uses Windows;

type
  THotkeySafe = (hksSafe, hksNeedsShifting, hksNeverSafe);

const FHotkeySafe: array[0..255] of THotkeySafe = (
	hksNeverSafe,				  //'?00',					// &H0// &H0
	hksNeverSafe,     //'?Left Mouse Button',	// &H1// &H1
	hksNeverSafe,     //'?Right Mouse Button',	// &H2//
	hksNeverSafe,     //'?Ctrl+Break',		   	// &H3//
	hksNeverSafe,     //'?Middle Mouse Button',	// &H4//
	hksNeverSafe,  			  //'?05',					// &H5// &H5
	hksNeverSafe, 				  //'?06',					// &H6// &H6
	hksNeverSafe, 				  //'?07',					// &H7// &H7
	hksNeverSafe,		  //'Backspace',	   		// &H8// &H8
	hksNeverSafe,		  //'Tab',	   		 		// &H9// &H9
	hksNeverSafe,				  //'?0A',					// &HA// &HA
	hksNeverSafe,				  //'?0B',					// &HB// &HB
	hksNeverSafe,	    	  //'Key Pad 5',	    	// &HC// &HC
	hksNeverSafe,		  //'Enter',				// &HD// &HD
	hksNeverSafe,			   	//'?0E',					// &HE// &HE
	hksNeverSafe,			   	//'?0F',					// &HF// &HF
	hksNeverSafe,     //'?Shift',				// &H10
	hksNeverSafe,     //'?Control',				// &H11
	hksNeverSafe,     //'?Alt',					// &H12
	hksNeverSafe,          //'Pause',				// &H13
	hksNeverSafe,     //'?Caps Lock',			// &H14
	hksNeverSafe,          //'?KANJI?15',			// &H15
	hksNeverSafe,          //'?KANJI?16',			// &H16
	hksNeverSafe,          //'?KANJI?17',			// &H17
	hksNeverSafe,          //'?KANJI?18',			// &H18
	hksNeverSafe,          //'?KANJI?19',			// &H19
	hksNeverSafe,          //'?1A',				// &H1A
	hksNeverSafe,          //'Escape',				// &H1B
	hksNeverSafe,          //'?KANJI?1C',			// &H1C
	hksNeverSafe,          //'?KANJI?1D',			// &H1D
	hksNeverSafe,          //'?KANJI?1E',			// &H1E
	hksNeverSafe,          //'?KANJI?1F',			// &H1F
	hksNeverSafe,          //'Spacebar',				// &H20
	hksNeverSafe,          //'Page Up',				// &H21
	hksNeverSafe,          //'Page Down',				// &H22
	hksNeverSafe,          //'End',				// &H23
	hksNeverSafe,          //'Home',				// &H24
	hksNeverSafe,          //'Left Arrow',				// &H25
	hksNeverSafe,          //'Up Arrow',					// &H26
	hksNeverSafe,          //'Right Arrow',				// &H27
	hksNeverSafe,          //'Down Arrow',				// &H28
	hksNeverSafe,          //'?Select',				// &H29
	hksNeverSafe,          //'?PRINT',				// &H2A
	hksNeverSafe,          //'?Execute',				// &H2B
	hksNeverSafe,          //'Print Screen',				// &H2C
	hksNeverSafe,          //'Insert',				// &H2D
	hksNeverSafe,          //'Delete',				// &H2E
	hksNeverSafe,          //'?Help',				// &H2F
	hksNeedsShifting,          //'0',					// &H30
	hksNeedsShifting,          //'1',					// &H31
	hksNeedsShifting,          //'2',					// &H32
	hksNeedsShifting,          //'3',					// &H33
	hksNeedsShifting,          //'4',					// &H34
	hksNeedsShifting,          //'5',					// &H35
	hksNeedsShifting,          //'6',					// &H36
	hksNeedsShifting,          //'7',					// &H37
	hksNeedsShifting,          //'8',					// &H38
	hksNeedsShifting,          //'9',					// &H39
	hksNeverSafe,          //'?3A',				// &H3A
	hksNeverSafe,          //'?3B',				// &H3B
	hksNeverSafe,          //'?3C',				// &H3C
	hksNeverSafe,          //'?3D',				// &H3D
	hksNeverSafe,          //'?3E',				// &H3E
	hksNeverSafe,          //'?3F',				// &H3F
	hksNeverSafe,          //'?40',				// &H40

	hksNeedsShifting,          //'A',					// &H41
	hksNeedsShifting,          //'B',					// &H42
	hksNeedsShifting,          //'C',					// &H43
	hksNeedsShifting,          //'D',					// &H44
	hksNeedsShifting,          //'E',					// &H45
	hksNeedsShifting,          //'F',					// &H46
	hksNeedsShifting,          //'G',					// &H47
	hksNeedsShifting,          //'H',					// &H48
	hksNeedsShifting,          //'I',					// &H49
	hksNeedsShifting,          //'J',					// &H4A
	hksNeedsShifting,          //'K',					// &H4B
	hksNeedsShifting,          //'L',					// &H4C
	hksNeedsShifting,          //'M',					// &H4D
	hksNeedsShifting,          //'N',					// &H4E
	hksNeedsShifting,          //'O',					// &H4F
	hksNeedsShifting,          //'P',					// &H50
	hksNeedsShifting,          //'Q',					// &H51
	hksNeedsShifting,          //'R',					// &H52
	hksNeedsShifting,          //'S',					// &H53
	hksNeedsShifting,          //'T',					// &H54
	hksNeedsShifting,          //'U',					// &H55
	hksNeedsShifting,          //'V',					// &H56
	hksNeedsShifting,          //'W',					// &h57
	hksNeedsShifting,          //'X',					// &H58
	hksNeedsShifting,          //'Y',					// &H59
	hksNeedsShifting,          //'Z',					// &H5A
	hksNeverSafe,          //'?5B',				// &H5B
	hksNeverSafe,          //'?5C',				// &H5C
	hksNeverSafe,          //'?5D',				// &H5D
	hksNeverSafe,          //'?5E',				// &H5E
	hksNeverSafe,          //'?5F',				// &H5F
	hksSafe,          //'Number Pad 0',				// &H60
	hksSafe,          //'Number Pad 1',				// &H61
	hksSafe,          //'Number Pad 2',				// &H62
	hksSafe,          //'Number Pad 3',				// &H63
	hksSafe,          //'Number Pad 4',				// &H64
	hksSafe,          //'Number Pad 5',				// &H65
	hksSafe,          //'Number Pad 6',				// &H66
	hksSafe,          //'Number Pad 7',				// &H67
	hksSafe,          //'Number Pad 8',				// &H68
	hksSafe,          //'Number Pad 9',				// &H69
	hksSafe,          //'Number Pad *',				// &H6A
	hksSafe,        			// &H6B
	hksSafe,          //'?Separator',			// &H6C
	hksSafe,      //'Number Pad -',			// &H6D
	hksSafe,     //'Number Pad .',				// &H6E
	hksSafe,      //'Number Pad /',			// &H6F
	hksSafe,          //'F1',					// &H70
	hksSafe,          //'F2',					// &H71
	hksSafe,          //'F3',					// &H72
	hksSafe,          //'F4',					// &H73
	hksSafe,          //'F5',					// &H74
	hksSafe,          //'F6',					// &H75
	hksSafe,          //'F7',					// &H76
	hksSafe,          //'F8',					// &H77
	hksSafe,          //'F9',					// &H78
	hksSafe,          //'F10',				// &H79
	hksSafe,          //'F11',				// &H7A
	hksSafe,          //'F12',				// &H7B
	hksSafe,          //'?F13',				// &H7C
	hksSafe,          //'?F14',				// &H7D
	hksSafe,          //'?F15',				// &H7E
	hksSafe,          //'?F16',				// &H7F
	hksSafe,          //'?F17',				// &H80
	hksSafe,          //'?F18',				// &H81
	hksSafe,          //'?F19',				// &H82
	hksSafe,          //'?F20',				// &H83
	hksSafe,          //'?F21',				// &H84
	hksSafe,          //'?F22',				// &H85
	hksSafe,          //'?F23',				// &H86
	hksSafe,          //'?F24',				// &H87

	hksNeverSafe,          //'?88',				// &H88
	hksNeverSafe,          //'?89',				// &H89
	hksNeverSafe,          //'?8A',				// &H8A
	hksNeverSafe,          //'?8B',				// &H8B
	hksNeverSafe,          //'?8C',				// &H8C
	hksNeverSafe,          //'?8D',				// &H8D
	hksNeverSafe,          //'?8E',				// &H8E
	hksNeverSafe,          //'?8F',				// &H8F

	hksNeverSafe,          //'?Num Lock',			// &H90
	hksNeverSafe,          //'?Scroll Lock',				// &H91

	hksNeverSafe,          //'?92',				// &H92
	hksNeverSafe,          //'?93',				// &H93
	hksNeverSafe,          //'?94',				// &H94
	hksNeverSafe,          //'?95',				// &H95
	hksNeverSafe,          //'?96',				// &H96
	hksNeverSafe,          //'?97',				// &H97
	hksNeverSafe,          //'?98',				// &H98
	hksNeverSafe,          //'?99',				// &H99
	hksNeverSafe,          //'?9A',				// &H9A
	hksNeverSafe,          //'?9B',				// &H9B
	hksNeverSafe,          //'?9C',				// &H9C
	hksNeverSafe,          //'?9D',				// &H9D
	hksNeverSafe,          //'?9E',				// &H9E
	hksNeverSafe,          //'?9F',				// &H9F
	hksNeverSafe,          //'?A0',				// &HA0
	hksNeverSafe,          //'?A1',				// &HA1
	hksNeverSafe,          //'?A2',				// &HA2
	hksNeverSafe,          //'?A3',				// &HA3
	hksNeverSafe,          //'?A4',				// &HA4
	hksNeverSafe,          //'?A5',				// &HA5
	hksNeverSafe,          //'?A6',				// &HA6
	hksNeverSafe,          //'?A7',				// &HA7
	hksNeverSafe,          //'?A8',				// &HA8
	hksNeverSafe,          //'?A9',				// &HA9
	hksNeverSafe,          //'?AA',				// &HAA
	hksNeverSafe,          //'?AB',				// &HAB
	hksNeverSafe,          //'?AC',				// &HAC
	hksNeverSafe,          //'?AD',				// &HAD
	hksNeverSafe,          //'?AE',				// &HAE
	hksNeverSafe,          //'?AF',				// &HAF
	hksNeverSafe,          //'?B0',				// &HB0
	hksNeverSafe,          //'?B1',				// &HB1
	hksNeverSafe,          //'?B2',				// &HB2
	hksNeverSafe,          //'?B3',				// &HB3
	hksNeverSafe,          //'?B4',				// &HB4
	hksNeverSafe,          //'?B5',				// &HB5
	hksNeverSafe,          //'?B6',				// &HB6
	hksNeverSafe,          //'?B7',				// &HB7
	hksNeverSafe,          //'?B8',				// &HB8
	hksNeverSafe,          //'?B9',				// &HB9

	hksNeedsShifting,      //';',				// &HBA
	hksNeedsShifting,      //'=',				// &HBB
	hksNeedsShifting,      //',',				// &HBC
	hksNeedsShifting,      //'-',				// &HBD
	hksNeedsShifting,      //'.',				// &HBE
	hksNeedsShifting,      //'/',				// &HBF
	hksNeedsShifting,      //'`',			// &HC0

	hksNeverSafe,          //'?C1',				// &HC1
	hksNeverSafe,          //'?C2',				// &HC2
	hksNeverSafe,          //'?C3',				// &HC3
	hksNeverSafe,          //'?C4',				// &HC4
	hksNeverSafe,          //'?C5',				// &HC5
	hksNeverSafe,          //'?C6',				// &HC6
	hksNeverSafe,          //'?C7',				// &HC7
	hksNeverSafe,          //'?C8',				// &HC8
	hksNeverSafe,          //'?C9',				// &HC9
	hksNeverSafe,          //'?CA',				// &HCA
	hksNeverSafe,          //'?CB',				// &HCB
	hksNeverSafe,          //'?CC',				// &HCC
	hksNeverSafe,          //'?CD',				// &HCD
	hksNeverSafe,          //'?CE',				// &HCE
	hksNeverSafe,          //'?CF',				// &HCF
	hksNeverSafe,          //'?D0',				// &HD0
	hksNeverSafe,          //'?D1',				// &HD1
	hksNeverSafe,          //'?D2',				// &HD2
	hksNeverSafe,          //'?D3',				// &HD3
	hksNeverSafe,          //'?D4',				// &HD4
	hksNeverSafe,          //'?D5',				// &HD5
	hksNeverSafe,          //'?D6',				// &HD6
	hksNeverSafe,          //'?D7',				// &HD7
	hksNeverSafe,          //'?D8',				// &HD8
	hksNeverSafe,          //'?D9',				// &HD9
	hksNeverSafe,          //'?DA',				// &HDA

	hksNeedsShifting,      //'[',				// &HDB
	hksNeedsShifting,       //'\',				// &HDC
	hksNeedsShifting,       //']',				// &HDD
	hksNeedsShifting,       //'''',				// &HDE
	hksNeedsShifting,       //'?DF',				// &HDF
	hksNeedsShifting,       //'?E0',				// &HE0
	hksNeedsShifting,      //'?E1',				// &HE1
	hksNeedsShifting,      //'?E2',				// &HE2
	hksNeedsShifting,      //'?E3',				// &HE3
	hksNeedsShifting,      //'?E4',				// &HE4

	hksNeverSafe,          //'?E5',				// &HE5

	hksNeedsShifting,      //'?E6',				// &HE6

	hksNeverSafe,          //'?E7',				// &HE7
	hksNeverSafe,          //'?E8',				// &HE8

	hksNeedsShifting,      //'?E9',				// &HE9
	hksNeedsShifting,      //'?EA',				// &HEA
	hksNeedsShifting,      //'?EB',				// &HEB
	hksNeedsShifting,      //'?EC',				// &HEC
	hksNeedsShifting,      //'?ED',				// &HED
	hksNeedsShifting,      //'?EE',				// &HEE
	hksNeedsShifting,      //'?EF',				// &HEF
	hksNeedsShifting,      //'?F0',				// &HF0
	hksNeedsShifting,      //'?F1',				// &HF1
	hksNeedsShifting,      //'?F2',				// &HF2
	hksNeedsShifting,      //'?F3',				// &HF3
	hksNeedsShifting,      //'?F4',				// &HF4
	hksNeedsShifting,      //'?F5',				// &HF5

	hksNeverSafe,          //'?F6',				// &HF6
	hksNeverSafe,          //'?F7',				// &HF7
	hksNeverSafe,          //'?F8',				// &HF8
	hksNeverSafe,          //'?F9',				// &HF9
	hksNeverSafe,          //'?FA',				// &HFA
	hksNeverSafe,          //'?FB',				// &HFB
	hksNeverSafe,          //'?FC',				// &HFC
	hksNeverSafe,          //'?FD',				// &HFD
	hksNeverSafe,          //'?FE',				// &HFE
	hksNeverSafe           //'?FF'					// &HFF
	);

function IsHotkeySafe(FHotkey: Integer): THotkeySafe;

implementation

uses keymanapi_TLB;

function IsHotkeySafe(FHotkey: Integer): THotkeySafe;
begin
  if FHotkey = 0 then
    Result := hksSafe
  else
    case FHotkeySafe[LOBYTE(FHOtkey)] of
      hksNeverSafe: Result := hksNeverSafe;
      hksNeedsShifting: if (FHotkey and (HK_ALT or HK_CTRL)) = 0 then Result := hksNeedsShifting else Result := hksSafe;
    else Result := hksSafe;
    end
end;

end.
