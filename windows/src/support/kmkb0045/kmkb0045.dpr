program kmkb0045;

uses
  Forms,
  kbdpatchmain in 'kbdpatchmain.pas' {Form1},
  GetOsVersion in '..\..\..\..\common\windows\delphi\general\GetOsVersion.pas',
  RegistryKeys in '..\..\..\..\common\windows\delphi\general\RegistryKeys.pas';

{$R *.res}
{$R manifest.res}

begin
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
