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
    lblScanCode: TLabel;
    lblScanCodeCaption: TLabel;
    lblISOPosition: TLabel;
    lblISOPositionCaption: TLabel;
    lblActiveLayout: TLabel;
    lblActiveLayoutCaption: TLabel;
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
    function GetHelpTopic: string; override;
  public
    function ShowModal: Integer; override;
  end;

function FormatKeyName(Shift, Key: Integer):  string;

implementation

uses
  System.Win.Registry,
  RegistryKeys,
  Keyman.Developer.System.HelpTopics,
  UfrmMain,
  KeyNames,
  VKeys,
  UfrmMDIChild,
  CharMapInsertMode,
  CharMapDropTool;

{$R *.DFM}

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
  if Assigned(FLastActiveControl) then
    GetCharMapDropTool.InsertToControl(FLastActiveControl, FormatKeyName(FLastShift, FLastKey), cmimCode);
end;

function TfrmKeyTest.GetHelpTopic: string;
begin
  Result := SHelpTopic_Context_KeyTest;
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
  name: array[0..KL_NAMELENGTH] of Char;
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

  GetKeyboardLayoutName(name);
  with TRegistry.Create do
  try
    RootKey := HKEY_LOCAL_MACHINE;
    if OpenKeyReadOnly(SRegKey_KeyboardLayouts_LM + '\' + name) and
        ValueExists(SRegValue_KeyboardLayoutText) then
      lblActiveLayout.Caption := ReadString(SRegValue_KeyboardLayoutText)
    else
      lblActiveLayout.Caption := name;
  finally
    Free;
  end;

  lblISOPosition.Caption := VKeyISO9995Names[LoWord(Key)];
  lblScanCode.Caption := IntToHex((msg.lParam and $FF0000) shr 16, 2);

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

