program kmkb0045;

uses
  Forms,
  kbdpatchmain in 'kbdpatchmain.pas' {Form1},
  GetOsVersion in '..\..\global\delphi\general\GetOsVersion.pas',
  RegistryKeys in '..\..\global\delphi\general\RegistryKeys.pas';

{$R *.res}
{$R manifest.res}

begin
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
