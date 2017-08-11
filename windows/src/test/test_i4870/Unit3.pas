unit Unit3;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, PlusMemo, PMSupport, ExtHilit, OOPHilit,
  Vcl.StdCtrls;

type
  TForm3 = class(TForm)
    PlusMemo1: TPlusMemo;
    procedure PlusMemo1Change(Sender: TObject);
  private
  public
  end;

var
  Form3: TForm3;

implementation

{$R *.dfm}

procedure TForm3.PlusMemo1Change(Sender: TObject);
begin
  Caption := Caption + '-';
end;

end.
