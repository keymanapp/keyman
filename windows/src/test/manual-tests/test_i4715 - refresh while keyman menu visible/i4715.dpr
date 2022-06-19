program i4715;

uses
  Vcl.Forms,
  UfrmTestI4715 in 'UfrmTestI4715.pas' {Form3},
  LangManager in '..\..\support\unload_keyboards\LangManager.pas',
  msctf in '..\..\global\delphi\winapi\msctf.pas',
  Glossary in '..\..\..\..\common\windows\delphi\general\Glossary.pas',
  ErrorControlledRegistry in '..\..\..\..\common\windows\delphi\vcl\ErrorControlledRegistry.pas',
  RegistryKeys in '..\..\..\..\common\windows\delphi\general\RegistryKeys.pas',
  KeymanVersion in '..\..\..\..\common\windows\delphi\general\KeymanVersion.pas',
  GetOsVersion in '..\..\..\..\common\windows\delphi\general\GetOsVersion.pas',
  LoadIndirectStringUnit in '..\..\engine\keyman\langswitch\LoadIndirectStringUnit.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TForm3, Form3);
  Application.Run;
end.
