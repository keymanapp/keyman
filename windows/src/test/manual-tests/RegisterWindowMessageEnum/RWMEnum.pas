unit RWMEnum;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls;

type
  TForm1 = class(TForm)
    Memo1: TMemo;
    procedure FormCreate(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

procedure TForm1.FormCreate(Sender: TObject);
var
  cstrRWMName: array [0 .. 1024] of char;
  i: Integer;
  s: string;
  lenRWM: Integer;
begin
  s := '//RWM Atom Table ****************************'#13#10;
  for i := $C000 to $FFFF do
  begin
    lenRWM := GetClipboardFormatName(i, cstrRWMName, 1024);
    if lenRWM > 0 then
    begin
      s := s + Format('%X=%s'#13#10, [i, Copy(cstrRWMName, 1, lenRWM)]);
    end;
  end;

  memo1.Text := s;
end;

end.
