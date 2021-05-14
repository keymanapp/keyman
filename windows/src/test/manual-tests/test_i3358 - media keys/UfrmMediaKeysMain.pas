unit UfrmMediaKeysMain;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls;

type
  TForm1 = class(TForm)
    Memo1: TMemo;
    Button1: TButton;
    procedure Button1Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

procedure TForm1.Button1Click(Sender: TObject);
var
  input: TINPUT;
begin
  Memo1.SetFocus;
  input.Itype := INPUT_KEYBOARD;
  input.ki.wVk := $af;  // volume up
  input.ki.wScan := 0; //0100 0001
  input.ki.dwFlags := KEYEVENTF_EXTENDEDKEY;
  input.ki.dwExtraInfo := 0;
  SendInput(1, input, sizeof(input));
end;

end.
