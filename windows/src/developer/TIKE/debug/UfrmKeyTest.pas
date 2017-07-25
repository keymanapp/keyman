(*
  Name:             UfrmKeyTest
  Copyright:        Copyright (C) SIL International.
  Documentation:    
  Description:      
  Create Date:      1 Aug 2006

  Modified Date:    22 Jan 2007
  Authors:          mcdurdin
  Related Files:    
  Dependencies:     

  Bugs:             
  Todo:             
  Notes:            
  History:          23 Aug 2006 - mcdurdin - Use TApplicationEvents instead of hooking all messages
                    23 Aug 2006 - mcdurdin - Insert with new CharMapDropTool
                    22 Jan 2007 - mcdurdin - Add K_NPENTER special case
*)
unit UfrmKeyTest;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, ExtCtrls, AppEvnts, UfrmTike;

const
  KT_SHIFT    = $0001000;
  KT_CTRL     = $0002000;
  KT_ALT      = $0004000;
  KT_LCTRL    = $0008000;
  KT_LALT     = $0010000;
  KT_RCTRL    = $0020000;
  KT_RALT     = $0040000;
  
type
  TfrmKeyTest = class(TTIKEForm)
    lblTitle: TLabel;
    panKeyCode: TPanel;
    lblKeyCodeCaption: TLabel;
    lblKeymanNameCaption: TLabel;
    lblKeyCode: TLabel;
    lblKeymanName: TLabel;
    cmdClose: TButton;
    lblClose: TLabel;
    cmdInsert: TButton;
    chkLRDistinguish: TCheckBox;
    lblDistinguish: TLabel;
    AppEvent: TApplicationEvents;
    procedure cmdCloseClick(Sender: TObject);
    procedure cmdInsertClick(Sender: TObject);
    procedure AppEventMessage(var Msg: tagMSG; var Handled: Boolean);
  private
    FShift, FLeftCtrl, FRightCtrl, FLeftAlt, FRightAlt: Boolean;
    FLastKey: Word;
    FLastShift: DWord;
    FLastActiveControl: TWinControl;
    procedure WMKey(var Msg: TMsg);
  protected
  public
    function ShowModal: Integer; override;
  end;

function FormatKeyName(Shift, Key: Integer):  string;

implementation

uses UfrmMain, KeyNames, UfrmMDIChild, CharMapInsertMode, CharMapDropTool;

{$R *.DFM}

const
 VKeyNames: array[0..255] of string = (
// Key Codes
	'K_?00',				// &H0
	'K_LBUTTON',			// &H1
	'K_RBUTTON',			// &H2
	'K_CANCEL',		   	    // &H3
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
	'K_UP',				    // &H26
	'K_RIGHT',				// &H27
	'K_DOWN',				// &H28
	'K_SEL',				// &H29
	'K_PRINT',				// &H2A
	'K_EXEC',				// &H2B
	'K_PRTSCN',			    // &H2C
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
	'K_NPSTAR',			    // &H6A
	'K_NPPLUS',			    // &H6B
	'K_SEPARATOR',			// &H6C
	'K_NPMINUS',			// &H6D
	'K_NPDOT',				// &H6E
	'K_NPSLASH',			// &H6F
	'K_F1',				    // &H70
	'K_F2',				    // &H71
	'K_F3',				    // &H72
	'K_F4',				    // &H73
	'K_F5',				    // &H74
	'K_F6',				    // &H75
	'K_F7',				    // &H76
	'K_F8',				    // &H77
	'K_F9',				    // &H78
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
	'K_SCROLL',			    // &H91

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
	'K_HYPHEN',			    // &HBD
	'K_PERIOD',			    // &HBE
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

procedure TfrmKeyTest.AppEventMessage(var Msg: tagMSG; var Handled: Boolean);
begin
  if (Msg.message >= WM_KEYFIRST) and (Msg.message <= WM_KEYLAST) then
  begin
    WMKey(Msg);
    Handled := True;
    AppEvent.CancelDispatch;
  end
  else
    Handled := False;
end;

procedure TfrmKeyTest.cmdCloseClick(Sender: TObject);
begin
  Close;
end;

procedure TfrmKeyTest.cmdInsertClick(Sender: TObject);
begin
  Close;
  if Assigned(FLastActiveControl) then
    GetCharMapDropTool.InsertToControl(FLastActiveControl, FormatKeyName(FLastShift, FLastKey), cmimCode);
end;

function TfrmKeyTest.ShowModal: Integer;
begin
  FLastActiveControl := Screen.ActiveControl;
  Result := inherited ShowModal;
end;

function FormatKeyName(Shift, Key: Integer):  string;
begin
  Result := '[';
  if (Shift and KT_SHIFT) = KT_SHIFT then Result := Result + 'SHIFT ';
  if (Shift and KT_CTRL) = KT_CTRL   then Result := Result + 'CTRL ';
  if (Shift and KT_ALT) = KT_ALT     then Result := Result + 'ALT ';
  if (Shift and KT_LCTRL) = KT_LCTRL then Result := Result + 'LCTRL ';
  if (Shift and KT_LALT) = KT_LALT   then Result := Result + 'LALT ';
  if (Shift and KT_RCTRL) = KT_RCTRL then Result := Result + 'RCTRL ';
  if (Shift and KT_RALT) = KT_RALT   then Result := Result + 'RALT ';

  if LoWord(Key) = 5
    then Result := Result + 'K_NPENTER]'
    else Result := Result + VKeyNames[LoWord(Key)] + ']';
end;

procedure TfrmKeyTest.WMKey(var Msg: TMsg);
var
  Key: Integer;
  Shift: TShiftState;
  s: string;
  FDown: Boolean;
  n: Integer;
begin
  case Msg.message of
    WM_KEYDOWN, WM_SYSKEYDOWN:
      FDown := True;
    WM_KEYUP, WM_SYSKEYUP:
      FDown := False;
    else Exit;
  end;

  Key := Msg.wParam;

  if Key = VK_SHIFT then FShift := FDown
  else if (Msg.LParam and (1 shl 24)) <> 0 then
  begin
    // Right ctrl/alt
    if Key = VK_CONTROL then FRightCtrl := FDown
    else if Key = VK_MENU then FRightAlt := FDown;
  end
  else if Key = VK_CONTROL then FLeftCtrl := FDown
  else if Key = VK_MENU then FLeftAlt := FDown;

  if not FDown then Exit;
  if Key in [VK_SHIFT, VK_CONTROL, VK_MENU] then Exit;

  Shift := [];
  if GetKeyState(VK_SHIFT) < 0 then Include(Shift, ssShift);
  if GetKeyState(VK_CONTROL) < 0 then Include(Shift, ssCtrl);
  if GetKeyState(VK_MENU) < 0 then Include(Shift, ssAlt);

  if (Key = VK_ESCAPE) and (Shift = [ssShift]) then
  begin
    cmdCloseClick(cmdClose);
    Exit;
  end;

  if (Key = VK_RETURN) and (Shift = [ssShift]) then
  begin
    cmdInsertClick(cmdInsert);
    Exit;
  end;

  if (Key = Ord('D')) and (Shift = [ssAlt]) and FLeftAlt then // not Right-Alt
    chkLRDistinguish.Checked := not chkLRDistinguish.Checked;

  s := '';
  n := 0;
  if chkLRDistinguish.Checked then
  begin
    if FShift     then begin s := s + 'shift ';      n := n or KT_SHIFT; end;
    if FLeftCtrl  then begin s := s + 'left-ctrl ';  n := n or KT_LCTRL; end;
    if FLeftAlt   then begin s := s + 'left-alt ';   n := n or KT_LALT;  end;
    if FRightCtrl then begin s := s + 'right-ctrl '; n := n or KT_RCTRL; end;
    if FRightAlt  then begin s := s + 'right-alt ';  n := n or KT_RALT;  end;
  end
  else
  begin
    if FShift                  then begin s := s + 'shift ';      n := n or KT_SHIFT; end;
    if FLeftCtrl or FRightCtrl then begin s := s + 'ctrl ';       n := n or KT_CTRL;  end;
    if FLeftAlt or FRightAlt   then begin s := s + 'alt ';        n := n or KT_ALT;   end;
  end;

  if (Key = VK_RETURN) and ((msg.lParam and (1 shl 24)) <> 0) then
  begin
    lblKeyCode.Caption := s + 'Number Pad Enter ('+IntToStr(Key)+')';
    lblKeymanName.Caption := FormatKeyName(n, 5);
    FLastKey := 5;
  end
  else
  begin
    lblKeyCode.Caption := s + SKeyNames[LoWord(Key)] + ' ('+IntToStr(Key)+')';
    lblKeymanName.Caption := FormatKeyName(n, Key);
    FLastKey := Key;
  end;

  FLastShift := n;
end;

end.

