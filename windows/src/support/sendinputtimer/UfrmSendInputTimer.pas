(*
  Name:             UfrmSendInputTimer
  Copyright:        Copyright (C) 2003-2017 SIL International.
  Documentation:    
  Description:      
  Create Date:      29 Mar 2015

  Modified Date:    29 Mar 2015
  Authors:          mcdurdin
  Related Files:    
  Dependencies:     

  Bugs:             
  Todo:             
  Notes:            
  History:          29 Mar 2015 - mcdurdin - I4642 - V9.0 - In Logos, generated backspace receives a scan of 0x00 instead of 0xFF
*)
unit UfrmSendInputTimer;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.ExtCtrls;

type
  TForm3 = class(TForm)
    Timer1: TTimer;
    Button1: TButton;
    procedure Button1Click(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
  private
    Count: Integer;
    procedure Fire;
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form3: TForm3;

implementation

{$R *.dfm}

procedure TForm3.Button1Click(Sender: TObject);
begin
  if not Timer1.Enabled then
    Timer1.Enabled := True;
  Count := 5;
end;

procedure TForm3.Timer1Timer(Sender: TObject);
begin
  if Count > 0 then
  begin
    Dec(Count);
    Button1.Caption := 'Backspace in '+InttoStr(count)+' secs';
  end
  else
  begin
    Fire;
    timer1.Enabled := False;
    Button1.Caption := 'Backspace in 5 secs';
  end;
end;

procedure TForm3.Fire;
var
  input: array[0..1] of TInput;
begin
  FillChar(input, sizeof(TInput)*2, 0);
  input[0].Itype := INPUT_KEYBOARD;
  input[0].ki.wVk := VK_BACK;
  input[0].ki.wScan := $FF;
  input[1].Itype := INPUT_KEYBOARD;
  input[1].ki.wVk := VK_BACK;
  input[1].ki.wScan := $FF;
  input[1].ki.dwFlags := KEYEVENTF_KEYUP;
  SendInput(2, input[0], sizeof(TInput));
end;

end.
