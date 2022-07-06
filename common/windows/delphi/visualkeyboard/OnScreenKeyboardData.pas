unit OnScreenKeyboardData;

interface

type
  TOnScreenKeyboardKeyType = (kktNormal, kktTab, kktCaps, kktShiftLeft, kktCtrlLeft, kktAltLeft,
    kktBackSpace, kktEnter, kktShiftRight, kktCtrlRight, kktAltRight);

  TOnScreenKeyboardKeyData = record
    KeyType: TOnScreenKeyboardKeyType;
    X, Y, Width, ScanCode: Integer;
    Row: Boolean;
  end;

const
  OnScreenKeyboardKeyTypeCaption: array[TOnScreenKeyboardKeyType] of WideString =
    ('', 'Tab', 'Caps', 'Shift', 'Ctrl', 'Alt', 'BkSp', 'Enter', 'Shift', 'Ctrl', 'Alt');

const
  KeyData: array[0..58] of TOnScreenKeyboardKeyData = (
    (KeyType: kktNormal; X: 0;   Y: 0; Width: 33; ScanCode: $29; Row: True),	//`
    (KeyType: kktNormal; X: 34;  Y: 0; Width: 33; ScanCode: $02),	//1
    (KeyType: kktNormal; X: 68;  Y: 0; Width: 33; ScanCode: $03),	//2
    (KeyType: kktNormal; X: 102; Y: 0; Width: 33; ScanCode: $04),	//3
    (KeyType: kktNormal; X: 136; Y: 0; Width: 33; ScanCode: $05),	//4
    (KeyType: kktNormal; X: 170; Y: 0; Width: 33; ScanCode: $06),	//5
    (KeyType: kktNormal; X: 204; Y: 0; Width: 33; ScanCode: $07),	//6
    (KeyType: kktNormal; X: 238; Y: 0; Width: 33; ScanCode: $08),	//7
    (KeyType: kktNormal; X: 272; Y: 0; Width: 33; ScanCode: $09),	//8
    (KeyType: kktNormal; X: 306; Y: 0; Width: 33; ScanCode: $0A),	//9
    (KeyType: kktNormal; X: 340; Y: 0; Width: 33; ScanCode: $0B),	//0
    (KeyType: kktNormal; X: 374; Y: 0; Width: 33; ScanCode: $0C),	//-
    (KeyType: kktNormal; X: 408; Y: 0; Width: 33; ScanCode: $0D),	//=
    (KeyType: kktBackspace; X: 442; Y: 0; Width: 58; ScanCode: $0E),	//bksp

    (KeyType: kktTab;    X: 0;   Y: 34; Width: 49; ScanCode: $0F; Row: True),	//tab
    (KeyType: kktNormal; X: 50;  Y: 34; Width: 33; ScanCode: $10),	//Q
    (KeyType: kktNormal; X: 84;  Y: 34; Width: 33; ScanCode: $11),	//W
    (KeyType: kktNormal; X: 118; Y: 34; Width: 33; ScanCode: $12),	//E
    (KeyType: kktNormal; X: 152; Y: 34; Width: 33; ScanCode: $13),	//R
    (KeyType: kktNormal; X: 186; Y: 34; Width: 33; ScanCode: $14),	//T
    (KeyType: kktNormal; X: 220; Y: 34; Width: 33; ScanCode: $15),	//Y
    (KeyType: kktNormal; X: 254; Y: 34; Width: 33; ScanCode: $16),	//U
    (KeyType: kktNormal; X: 288; Y: 34; Width: 33; ScanCode: $17),	//I
    (KeyType: kktNormal; X: 322; Y: 34; Width: 33; ScanCode: $18),	//O
    (KeyType: kktNormal; X: 356; Y: 34; Width: 33; ScanCode: $19),	//P
    (KeyType: kktNormal; X: 390; Y: 34; Width: 33; ScanCode: $1A),	//[
    (KeyType: kktNormal; X: 424; Y: 34; Width: 33; ScanCode: $1B),	//]
    (KeyType: kktNormal; X: 458; Y: 34; Width: 42; ScanCode: $2B),	//\

    (KeyType: kktCaps;   X: 0;   Y: 68; Width: 57; ScanCode: $3A; Row: True),	//CAPS
    (KeyType: kktNormal; X: 58;  Y: 68; Width: 33; ScanCode: $1E),	//A
    (KeyType: kktNormal; X: 92;  Y: 68; Width: 33; ScanCode: $1F),	//S
    (KeyType: kktNormal; X: 126; Y: 68; Width: 33; ScanCode: $20),	//D
    (KeyType: kktNormal; X: 160; Y: 68; Width: 33; ScanCode: $21),	//F
    (KeyType: kktNormal; X: 194; Y: 68; Width: 33; ScanCode: $22),	//G
    (KeyType: kktNormal; X: 228; Y: 68; Width: 33; ScanCode: $23),	//H
    (KeyType: kktNormal; X: 262; Y: 68; Width: 33; ScanCode: $24),	//J
    (KeyType: kktNormal; X: 296; Y: 68; Width: 33; ScanCode: $25),	//K
    (KeyType: kktNormal; X: 330; Y: 68; Width: 33; ScanCode: $26),	//L
    (KeyType: kktNormal; X: 364; Y: 68; Width: 33; ScanCode: $27),	//;
    (KeyType: kktNormal; X: 398; Y: 68; Width: 33; ScanCode: $28),	//'
    (KeyType: kktEnter; X: 432; Y: 68; Width: 68; ScanCode: $1C),	//ENTER

    (KeyType: kktShiftLeft; X: 0; Y: 102; Width: 41; ScanCode: $2A; Row: True),	//SHIFT
    (KeyType: kktNormal; X: 42;  Y: 102; Width: 33; ScanCode: $56),	//102
    (KeyType: kktNormal; X: 76;  Y: 102; Width: 33; ScanCode: $2C),	//Z
    (KeyType: kktNormal; X: 110; Y: 102; Width: 33; ScanCode: $2D),	//X
    (KeyType: kktNormal; X: 144; Y: 102; Width: 33; ScanCode: $2E),	//C
    (KeyType: kktNormal; X: 178; Y: 102; Width: 33; ScanCode: $2F),	//V
    (KeyType: kktNormal; X: 212; Y: 102; Width: 33; ScanCode: $30),	//B
    (KeyType: kktNormal; X: 246; Y: 102; Width: 33; ScanCode: $31),	//N
    (KeyType: kktNormal; X: 280; Y: 102; Width: 33; ScanCode: $32),	//M
    (KeyType: kktNormal; X: 314; Y: 102; Width: 33; ScanCode: $33),	//,
    (KeyType: kktNormal; X: 348; Y: 102; Width: 33; ScanCode: $34),	//.
    (KeyType: kktNormal; X: 382; Y: 102; Width: 33; ScanCode: $35),	// /
    (KeyType: kktShiftRight; X: 416; Y: 102; Width: 84; ScanCode: $2A),	//SHIFT

    (KeyType: kktCtrlLeft; X: 0; Y: 136; Width: 45; ScanCode: $1D; Row: True),	//CTRL
    (KeyType: kktAltLeft; X: 78; Y: 136; Width: 45; ScanCode: $38),	//ALT
    (KeyType: kktNormal; X: 124; Y: 136; Width: 234; ScanCode: $39),	//SPACE
    (KeyType: kktAltRight; X: 359; Y: 136; Width: 45; ScanCode: $38),	//ALT
    (KeyType: kktCtrlRight; X: 455; Y: 136; Width: 45; ScanCode: $1D));	//CTRL

implementation

end.
