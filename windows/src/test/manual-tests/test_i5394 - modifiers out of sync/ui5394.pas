unit ui5394;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.ExtCtrls;

type
  TForm1 = class(TForm)
    Memo1: TMemo;
    Panel1: TPanel;
    chkShiftDelay: TCheckBox;
    procedure FormKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
  private
    c: Integer;
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

procedure TForm1.FormKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
var
  i: array[0..1] of TInput;
  n: Integer;

  procedure Log(s: string);
  begin
    memo1.Text := memo1.Text + s;
    memo1.Update;
  end;
  procedure LogKeyState;
  begin
    Log(Format('n:%d VK_SHIFT GKS=%x GAKS=%x'#13#10,
      [n, GetKeyState(VK_SHIFT), GetAsyncKeyState(VK_SHIFT)]));
  end;
begin
  if chkShiftDelay.Checked and (Key = VK_SHIFT) then
  begin
    Log(#13#10);
    for n := 0 to 4 do
    begin
      LogKeyState;
      Sleep(500);
    end;
    FillChar(i, sizeof(TInput)*2, 0);
    i[0].Itype := INPUT_KEYBOARD;
    i[0].ki.wVk := Ord('A')+c;
    i[0].ki.wScan := $1E;
    i[0].ki.dwFlags := 0;
    i[1].Itype := INPUT_KEYBOARD;
    i[1].ki.wVk := Ord('A')+c;
    i[1].ki.wScan := $1E;
    i[1].ki.dwFlags := KEYEVENTF_KEYUP;;
    SendInput(2, i[0], sizeof(TInput));
    Log('['+Chr(Ord('A')+c)+']'#13#10);
    Inc(c);
    for n := 5 to 9 do
    begin
      LogKeyState;
      Sleep(500);
    end;
  end
  else Sleep(100);
end;

procedure TForm1.FormKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
//  Sleep(100);
end;

end.
