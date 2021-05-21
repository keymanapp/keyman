program subst;

uses
  Vcl.Forms,
  subst_main in 'subst_main.pas' {Form1},
  msctf in '..\..\global\delphi\winapi\msctf.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
