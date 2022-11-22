program unloadkeyboards;

uses
  Vcl.Forms,
  UfrmUnloadKeyboards in 'UfrmUnloadKeyboards.pas' {Form1},
  Glossary in '..\..\..\..\common\windows\delphi\general\Glossary.pas',
  ErrorControlledRegistry in '..\..\..\..\common\windows\delphi\vcl\ErrorControlledRegistry.pas',
  RegistryKeys in '..\..\..\..\common\windows\delphi\general\RegistryKeys.pas',
  KeymanVersion in '..\..\..\..\common\windows\delphi\general\KeymanVersion.pas',
  GetOsVersion in '..\..\..\..\common\windows\delphi\general\GetOsVersion.pas',
  msctf in '..\..\global\delphi\winapi\msctf.pas',
  LoadIndirectStringUnit in '..\..\engine\keyman\langswitch\LoadIndirectStringUnit.pas',
  LangManager in 'LangManager.pas',
  input_installlayoutortip in '..\..\global\delphi\winapi\input_installlayoutortip.pas',
  utiltsf in '..\..\global\delphi\general\utiltsf.pas',
  keyman_msctf in '..\..\global\delphi\winapi\keyman_msctf.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
