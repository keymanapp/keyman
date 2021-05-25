(*
  Name:             test_i4793
  Copyright:        Copyright (C) 2003-2017 SIL International.
  Documentation:    
  Description:      
  Create Date:      24 Jul 2015

  Modified Date:    24 Jul 2015
  Authors:          mcdurdin
  Related Files:    
  Dependencies:     

  Bugs:             
  Todo:             
  Notes:            
  History:          24 Jul 2015 - mcdurdin - I4793 - Race condition with preserved keys and modifiers
*)
unit test_i4793;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.ExtCtrls;

type
  TfrmI4793 = class(TForm)
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Timer1: TTimer;
    Label4: TLabel;
    procedure FormDblClick(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
  private
    ntime: Integer;
  protected
    procedure WMKeyDown(var Message: TWMKeyDown); message WM_KEYDOWN;
    procedure WMKeyUp(var Message: TWMKeyUp); message WM_KEYUP;
    procedure WMChar(var Message: TWMChar); message WM_CHAR;

    { Private declarations }

  public
    { Public declarations }
  end;

var
  frmI4793: TfrmI4793;

implementation

{$R *.dfm}

{ TForm3 }

procedure TfrmI4793.FormDblClick(Sender: TObject);
begin
  timer1.enabled := not timer1.enabled;
  ntime := 5;
end;

procedure TfrmI4793.Timer1Timer(Sender: TObject);
var
  x: array[0..7] of TInput;
begin
  if ntime > 0 then
  begin
    Dec(ntime);
    Caption := 'Form3 - '+IntToStr(ntime);
  end
  else if ntime = 0 then
  begin
    timer1.Enabled := False;
    FillChar(x, sizeof(x), 0);
    x[0].Itype := INPUT_KEYBOARD;
    x[0].ki.wVk := VK_SHIFT;

    x[1].Itype := INPUT_KEYBOARD;
    x[1].ki.wVk := Ord('A');
    x[2].Itype := INPUT_KEYBOARD;
    x[2].ki.wVk := Ord('A');
    x[2].ki.dwFlags := KEYEVENTF_KEYUP;

    x[3].Itype := INPUT_KEYBOARD;
    x[3].ki.wVk := Ord('B');
    x[4].Itype := INPUT_KEYBOARD;
    x[4].ki.wVk := Ord('B');
    x[4].ki.dwFlags := KEYEVENTF_KEYUP;

    x[5].Itype := INPUT_KEYBOARD;
    x[5].ki.wVk := Ord('C');
    x[6].Itype := INPUT_KEYBOARD;
    x[6].ki.wVk := Ord('C');
    x[6].ki.dwFlags := KEYEVENTF_KEYUP;

    x[7].Itype := INPUT_KEYBOARD;
    x[7].ki.wVk := VK_SHIFT;
    x[7].ki.dwFlags := KEYEVENTF_KEYUP;

    SendInput(8, x[0], sizeof(TInput));
  end;
end;

procedure TfrmI4793.WMChar(var Message: TWMChar);
begin
  if Message.CharCode = VK_BACK then
    label3.Caption := Copy(label3.Caption, 1, Length(label3.Caption)-1)

  else
    label3.Caption := label3.Caption + Char(Message.CharCode);

end;

procedure TfrmI4793.WMKeyDown(var Message: TWMKeyDown);
begin
  if Message.CharCode = Ord('A') then
    Sleep(500);

  if (Message.CharCode >= Ord('A')) and (Message.CharCode <= Ord('Z')) then
    label1.Caption := label1.Caption + Char(Message.CharCode)

  else if Message.CharCode = VK_BACK then
    label1.Caption := Copy(label1.Caption, 1, Length(label1.Caption)-1)

  else if (Message.CharCode = VK_SHIFT) and ((Message.KeyData and $40000000)=0) then
  begin
    sleep(500);
    label1.Caption := label1.Caption + '^';
  end;
end;

procedure TfrmI4793.WMKeyUp(var Message: TWMKeyUp);
begin
  if (Message.CharCode >= Ord('A')) and (Message.CharCode <= Ord('Z')) then
    label2.Caption := label2.Caption + Char(Message.CharCode)

  else if Message.CharCode = VK_BACK then
    label2.Caption := Copy(label2.Caption, 1, Length(label2.Caption)-1)

  else if Message.CharCode = VK_SHIFT then
    label2.Caption := label2.Caption + '^';
end;

end.
