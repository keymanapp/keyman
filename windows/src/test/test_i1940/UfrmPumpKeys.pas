(*
  Name:             UfrmPumpKeys
  Copyright:        Copyright (C) SIL International.
  Documentation:    
  Description:      
  Create Date:      23 Apr 2009

  Modified Date:    23 Apr 2009
  Authors:          mcdurdin
  Related Files:    
  Dependencies:     

  Bugs:             
  Todo:             
  Notes:            
  History:          23 Apr 2009 - mcdurdin - I1940 - Test I1940
*)
unit UfrmPumpKeys;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, TntStdCtrls;

type
  TForm1 = class(TForm)
    Memo1: TTntMemo;
    Button1: TButton;
    Button2: TButton;
    Button3: TButton;
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure Button3Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

uses menus, utilexecute;

{$R *.dfm}

type
  TShortcutSetup = record
    Key: Word;
    Shift: TShiftState;
  end;

const
  Hotkeys: array[0..5] of TShortcutSetup = (
    (Key: Ord('G'); Shift: [ssCtrl, ssAlt]),
    (Key: Ord('B'); Shift: [ssAlt]),
    (Key: Ord('G'); Shift: [ssAlt, ssShift]),
    (Key: Ord('H'); Shift: [ssAlt, ssShift]),
    (Key: Ord('T'); Shift: [ssCtrl, ssAlt]),
    (Key: Ord('A'); Shift: [ssCtrl, ssAlt])
  );
  
procedure PressHotKey(k: TShortcutSetup);
begin
  if ssShift in k.Shift then keybd_event(VK_SHIFT, $2a, 0, 0);
  if ssCtrl in k.Shift then  keybd_event(VK_CONTROL, $1d, 0, 0);
  if ssAlt in k.Shift then   keybd_event(VK_MENU, $38, 0, 0);
  keybd_event(k.Key, 0, 0, 0);
  keybd_event(k.Key, 0, KEYEVENTF_KEYUP, 0);
  if ssAlt in k.Shift then   keybd_event(VK_MENU, $38, KEYEVENTF_KEYUP, 0);
  if ssCtrl in k.Shift then  keybd_event(VK_CONTROL, $1d, KEYEVENTF_KEYUP, 0);
  if ssShift in k.Shift then keybd_event(VK_SHIFT, $2a, KEYEVENTF_KEYUP, 0);
end;

procedure TForm1.Button1Click(Sender: TObject);
var
  n, ln, k: Integer;
begin
  memo1.SetFocus;

  n := 0;
  ln := 0;

  while GetAsyncKeyState(VK_ESCAPE) >= 0 do
  begin
    if Random(100) > 95 then
    begin
      PressHotKey(Hotkeys[Random(Length(Hotkeys))]);
    end
    else
    begin
      k := Random(26) + Ord('A');
      keybd_event(k, 0, 0, 0);
      keybd_event(k, 0, KEYEVENTF_KEYUP, 0);
    end;
    Application.ProcessMessages;
    Inc(n);
    if n > 60 then
    begin
      Inc(ln);
      if ln > 100 then
      begin
        memo1.Text := '';
        ln := 0;
      end
      else
      begin
        keybd_event(VK_RETURN, 0, 0, 0);
        keybd_event(VK_RETURN, 0, KEYEVENTF_KEYUP, 0);
      end;
      n := 0;
    end;
  end;
end;

procedure TForm1.Button2Click(Sender: TObject);
var
  n: Integer;
begin
  n := Random(Length(Hotkeys));
  memo1.Lines.Add('Pressed hotkey '+IntToStr(n)+': '+  ShortCutToText(ShortCut(Hotkeys[n].Key, Hotkeys[n].Shift)));
  PressHotKey(Hotkeys[n]);
end;

procedure TForm1.Button3Click(Sender: TObject);
var
  hwnd: THandle;
begin
  while GetAsyncKeyState(VK_ESCAPE) >= 0 do
  begin
    TUtilExecute.Shell(Handle, 'c:\program files\common files\tavultesoft\Keyman Engine 8.0\keyman.exe', '', '-kmc start 1');
    repeat
      Sleep(500);
      hwnd := FindWindow('TfrmKeyman7Main', 'frmKeyman7Main');
      if GetAsyncKeyState(VK_ESCAPE) < 0 then Exit;
    until hwnd <> 0;
    PostMessage(hwnd, WM_CLOSE, 0, 0);
    Sleep(500);
  end;
end;

end.
