program test_i2896;

uses
  Forms,
  test_i2896_unit in 'test_i2896_unit.pas' {Form1},
  certificate_check in '..\..\global\delphi\general\certificate_check.pas',
  Crypt32 in '..\..\global\delphi\general\Crypt32.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
