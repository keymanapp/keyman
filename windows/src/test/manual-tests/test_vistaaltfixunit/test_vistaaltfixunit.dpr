program test_vistaaltfixunit;

uses
  Forms,
  testvafu in 'testvafu.pas' {Form1},
  VistaAltFixUnit2 in '..\..\global\delphi\comp\VistaAltFixUnit2.pas',
  vafuForm2 in 'vafuForm2.pas' {Form2};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
