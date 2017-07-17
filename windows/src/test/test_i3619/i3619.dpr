program i3619;

uses
  Vcl.Forms,
  test_i3619 in 'test_i3619.pas' {Form1},
  msctf in '..\..\global\delphi\winapi\msctf.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
