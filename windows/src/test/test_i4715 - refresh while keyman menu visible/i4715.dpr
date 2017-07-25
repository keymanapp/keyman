program i4715;

uses
  Vcl.Forms,
  UfrmTestI4715 in 'UfrmTestI4715.pas' {Form3},
  LangManager in '..\..\support\unload_keyboards\LangManager.pas',
  msctf in '..\..\global\delphi\winapi\msctf.pas',
  Glossary in '..\..\global\delphi\general\Glossary.pas',
  ErrorControlledRegistry in '..\..\global\delphi\vcl\ErrorControlledRegistry.pas',
  RegistryKeys in '..\..\global\delphi\general\RegistryKeys.pas',
  KeymanVersion in '..\..\global\delphi\general\KeymanVersion.pas',
  GetOsVersion in '..\..\global\delphi\general\GetOsVersion.pas',
  LoadIndirectStringUnit in '..\..\engine\keyman\langswitch\LoadIndirectStringUnit.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TForm3, Form3);
  Application.Run;
end.
