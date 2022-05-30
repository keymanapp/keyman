unit ScanCodeMap;

interface

uses Windows;

function VKToScanCodeToVK(VKey: UINT; hkl: HKL): UINT;
function MapScanCodeToUSVK(scancode: UINT): UINT;

const
  USVirtualKeyToScanCode: array[0..255] of UINT = (
    $00, // L"K_?00",				// &H0
    $00, // L"K_LBUTTON",			// &H1
    $00, // L"K_RBUTTON",			// &H2
    $46, // L"K_CANCE$00, // L",		   	// &H3
    $00, // L"K_MBUTTON",			// &H4
    $00, // L"K_?05",				// &H5
    $00, // L"K_?06",				// &H6
    $00, // L"K_?07",				// &H7
    $0E, // L"K_BKSP",	    		// &H8
    $0F, // L"K_TAB",	    		// &H9
    $00, // L"K_?0A",				// &HA
    $00, // L"K_?0B",				// &HB
    $4C, // L"K_KP5",		    	// &HC
    $1C, // L"K_ENTER",				// &HD
    $00, // L"K_?0E",				// &HE
    $00, // L"K_?0F",				// &HF
    $2A, // L"K_SHIFT",				// &H10
    $1D, // L"K_CONTROL",			// &H11
    $38, // L"K_ALT",				// &H12
    $00, // L"K_PAUSE",				// &H13
    $3A, // L"K_CAPS",				// &H14
    $00, // L"K_KANJI?15",			// &H15
    $00, // L"K_KANJI?16",			// &H16
    $00, // L"K_KANJI?17",			// &H17
    $00, // L"K_KANJI?18",			// &H18
    $00, // L"K_KANJI?19",			// &H19
    $00, // L"K_?1A",				// &H1A
    $01, // L"K_ESC",				// &H1B
    $00, // L"K_KANJI?1C",			// &H1C
    $00, // L"K_KANJI?1D",			// &H1D
    $00, // L"K_KANJI?1E",			// &H1E
    $00, // L"K_KANJI?1F",			// &H1F
    $39, // L"K_SPACE",				// &H20
    $49, // L"K_PGUP",				// &H21
    $51, // L"K_PGDN",				// &H22
    $4F, // L"K_END",				// &H23
    $47, // L"K_HOME",				// &H24
    $4B, // L"K_LEFT",				// &H25
    $48, // L"K_UP",				// &H26
    $4D, // L"K_RIGHT",				// &H27
    $50, // L"K_DOWN",				// &H28
    $00, // L"K_SEL",				// &H29
    $00, // L"K_PRINT",				// &H2A
    $00, // L"K_EXEC",				// &H2B
    $54, // L"K_PRTSCN",			// &H2C
    $52, // L"K_INS",				// &H2D
    $53, // L"K_DEL",				// &H2E
    $63, // L"K_HELP",				// &H2F
    $0B, // L"K_0",					// &H30
    $02, // L"K_1",					// &H31
    $03, // L"K_2",					// &H32
    $04, // L"K_3",					// &H33
    $05, // L"K_4",					// &H34
    $06, // L"K_5",					// &H35
    $07, // L"K_6",					// &H36
    $08, // L"K_7",					// &H37
    $09, // L"K_8",					// &H38
    $0A, // L"K_9",					// &H39
    $00, // L"K_?3A",				// &H3A
    $00, // L"K_?3B",				// &H3B
    $00, // L"K_?3C",				// &H3C
    $00, // L"K_?3D",				// &H3D
    $00, // L"K_?3E",				// &H3E
    $00, // L"K_?3F",				// &H3F
    $00, // L"K_?40",				// &H40

    $1E, // L"K_A",					// &H41
    $30, // L"K_B",					// &H42
    $2E, // L"K_C",					// &H43
    $20, // L"K_D",					// &H44
    $12, // L"K_E",					// &H45
    $21, // L"K_F",					// &H46
    $22, // L"K_G",					// &H47
    $23, // L"K_H",					// &H48
    $17, // L"K_I",					// &H49
    $24, // L"K_J",					// &H4A
    $25, // L"K_K",					// &H4B
    $26, // L"K_L",					// &H4C
    $32, // L"K_M",					// &H4D
    $31, // L"K_N",					// &H4E
    $18, // L"K_O",					// &H4F
    $19, // L"K_P",					// &H50
    $10, // L"K_Q",					// &H51
    $13, // L"K_R",					// &H52
    $1F, // L"K_S",					// &H53
    $14, // L"K_T",					// &H54
    $16, // L"K_U",					// &H55
    $2F, // L"K_V",					// &H56
    $11, // L"K_W",					// &H57
    $2D, // L"K_X",					// &H58
    $15, // L"K_Y",					// &H59
    $2C, // L"K_Z",					// &H5A
    $5B, // L"K_?5B",				// &H5B
    $5C, // L"K_?5C",				// &H5C
    $5D, // L"K_?5D",				// &H5D
    $00, // L"K_?5E",				// &H5E
    $5F, // L"K_?5F",				// &H5F
    $52, // L"K_NP0",				// &H60
    $4F, // L"K_NP1",				// &H61
    $50, // L"K_NP2",				// &H62
    $51, // L"K_NP3",				// &H63
    $4B, // L"K_NP4",				// &H64
    $4C, // L"K_NP5",				// &H65
    $4D, // L"K_NP6",				// &H66
    $47, // L"K_NP7",				// &H67
    $48, // L"K_NP8",				// &H68
    $49, // L"K_NP9",				// &H69
    $37, // L"K_NPSTAR",			// &H6A
    $4E, // L"K_NPPLUS",			// &H6B
    $00, // L"K_SEPARATOR",			// &H6C
    $4A, // L"K_NPMINUS",			// &H6D
    $53, // L"K_NPDOT",				// &H6E
    $135, // L"K_NPSLASH",			// &H6F
    $3B, // L"K_F1",				// &H70
    $3C, // L"K_F2",				// &H71
    $3D, // L"K_F3",				// &H72
    $3E, // L"K_F4",				// &H73
    $3F, // L"K_F5",				// &H74
    $40, // L"K_F6",				// &H75
    $41, // L"K_F7",				// &H76
    $42, // L"K_F8",				// &H77
    $43, // L"K_F9",				// &H78
    $44, // L"K_F10",				// &H79
    $57, // L"K_F11",				// &H7A
    $58, // L"K_F12",				// &H7B
    $64, // L"K_F13",				// &H7C
    $65, // L"K_F14",				// &H7D
    $66, // L"K_F15",				// &H7E
    $67, // L"K_F16",				// &H7F
    $68, // L"K_F17",				// &H80
    $69, // L"K_F18",				// &H81
    $6A, // L"K_F19",				// &H82
    $6B, // L"K_F20",				// &H83
    $6C, // L"K_F21",				// &H84
    $6D, // L"K_F22",				// &H85
    $6E, // L"K_F23",				// &H86
    $76, // L"K_F24",				// &H87  

    $00, // L"K_?88",				// &H88
    $00, // L"K_?89",				// &H89
    $00, // L"K_?8A",				// &H8A
    $00, // L"K_?8B",				// &H8B
    $00, // L"K_?8C",				// &H8C
    $00, // L"K_?8D",				// &H8D
    $00, // L"K_?8E",				// &H8E
    $00, // L"K_?8F",				// &H8F 

    $45, // L"K_NUMLOCK",			// &H90
    $46, // L"K_SCROL$00, // L",			// &H91 

    $00, // L"K_?92",				// &H92
    $00, // L"K_?93",				// &H93
    $00, // L"K_?94",				// &H94
    $00, // L"K_?95",				// &H95
    $00, // L"K_?96",				// &H96
    $00, // L"K_?97",				// &H97
    $00, // L"K_?98",				// &H98
    $00, // L"K_?99",				// &H99
    $00, // L"K_?9A",				// &H9A
    $00, // L"K_?9B",				// &H9B
    $00, // L"K_?9C",				// &H9C
    $00, // L"K_?9D",				// &H9D
    $00, // L"K_?9E",				// &H9E
    $00, // L"K_?9F",				// &H9F
    $2A, // L"K_?A0",				// &HA0
    $36, // L"K_?A1",				// &HA1
    $1D, // L"K_?A2",				// &HA2
    $1D, // L"K_?A3",				// &HA3
    $38, // L"K_?A4",				// &HA4
    $38, // L"K_?A5",				// &HA5
    $6A, // L"K_?A6",				// &HA6
    $69, // L"K_?A7",				// &HA7
    $67, // L"K_?A8",				// &HA8
    $68, // L"K_?A9",				// &HA9
    $65, // L"K_?AA",				// &HAA
    $66, // L"K_?AB",				// &HAB
    $32, // L"K_?AC",				// &HAC
    $20, // L"K_?AD",				// &HAD
    $2E, // L"K_?AE",				// &HAE
    $30, // L"K_?AF",				// &HAF
    $19, // L"K_?B0",				// &HB0
    $10, // L"K_?B1",				// &HB1
    $24, // L"K_?B2",				// &HB2
    $22, // L"K_?B3",				// &HB3
    $6C, // L"K_?B4",				// &HB4
    $6D, // L"K_?B5",				// &HB5
    $6B, // L"K_?B6",				// &HB6
    $21, // L"K_?B7",				// &HB7
    $00, // L"K_?B8",				// &HB8
    $00, // L"K_?B9",				// &HB9 

    $27, // L"K_COLON",				// &HBA
    $0D, // L"K_EQUA$00, // L",				// &HBB
    $33, // L"K_COMMA",				// &HBC
    $0C, // L"K_HYPHEN",			// &HBD
    $34, // L"K_PERIOD",			// &HBE
    $35, // L"K_SLASH",				// &HBF
    $29, // L"K_BKQUOTE",			// &HC0 

    $73, // L"K_?C1",				// &HC1
    $7E, // L"K_?C2",				// &HC2
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
    $2B, // L"K_BKSLASH",			// &HDC
    $1B, // L"K_RBRKT",				// &HDD
    $28, // L"K_QUOTE",				// &HDE
    $00, // L"K_oDF",				// &HDF
    $00, // L"K_oE0",				// &HE0
    $00, // L"K_oE1",				// &HE1
    $56, // L"K_oE2",				// &HE2
    $00, // L"K_oE3",				// &HE3
    $00, // L"K_oE4",				// &HE4

    $00, // L"K_?E5",				// &HE5 

    $00, // L"K_oE6",				// &HE6 

    $00, // L"K_?E7",				// &HE7
    $00, // L"K_?E8",				// &HE8 

    $71, // L"K_oE9",				// &HE9
    $5C, // L"K_oEA",				// &HEA
    $7B, // L"K_oEB",				// &HEB
    $00, // L"K_oEC",				// &HEC
    $6F, // L"K_oED",				// &HED
    $5A, // L"K_oEE",				// &HEE
    $00, // L"K_oEF",				// &HEF
    $00, // L"K_oF0",				// &HF0
    $5B, // L"K_oF1",				// &HF1
    $00, // L"K_oF2",				// &HF2
    $5F, // L"K_oF3",				// &HF3
    $00, // L"K_oF4",				// &HF4
    $5E, // L"K_oF5",				// &HF5

    $00, // L"K_?F6",				// &HF6
    $00, // L"K_?F7",				// &HF7
    $00, // L"K_?F8",				// &HF8
    $5D, // L"K_?F9",				// &HF9
    $00, // L"K_?FA",				// &HFA
    $62, // L"K_?FB",				// &HFB
    $00, // L"K_?FC",				// &HFC
    $00, // L"K_?FD",				// &HFD
    $00, // L"K_?FE",				// &HFE
    $00  // L"K_?FF"				// &HFF
  );

implementation

var
  VkToVkByScanCode: array[0..255] of UINT;
  ActiveHKL: HKL = 0;

function VKToScanCodeToVK(VKey: UINT; hkl: HKL): UINT;
var
  scancode: UINT;
  i: Integer;
begin
	{ Find the appropriate keyboard tables }

  if hkl <> ActiveHKL then
  begin
    for i := 0 to 255 do VkToVkByScanCode[i] := 0;
    ActiveHKL := hkl;
  end;

	{ Look in the buffer }

	if VkToVkByScanCode[VKey] <> 0 then
	begin
		Result := VkToVkByScanCode[VKey];
    Exit;
	end;

	{ Map the virtual key to the US English character }

	scancode := MapVirtualKeyEx(VKey, 0, hkl);
	if scancode = 0 then
  begin
    VkToVkByScanCode[VKey] := VKey;
    Result := VKey;
    Exit;
  end;

  { Translate through the US scan code table }

	for i := 0 to High(USVirtualKeyToScanCode) do
		if USVirtualKeyToScanCode[i] = scancode then
    begin
      VkToVkByScanCode[VKey] := i;
			Result := i;
      Exit;
    end;

  VkToVkByScanCode[VKey] := VKey;
	Result := VKey;
end;

function MapScanCodeToUSVK(scancode: UINT): UINT;
var
  i: Integer;
begin
  for i := 0 to High(USVirtualKeyToScanCode) do
		if USVirtualKeyToScanCode[i] = scancode then
    begin
      Result := i;
      Exit;
    end;
  Result := 0;
end;


end.
