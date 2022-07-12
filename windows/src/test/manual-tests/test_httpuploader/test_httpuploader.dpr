program test_httpuploader;

uses
  Forms,
  SysUtils,
  ExcMagicGUI in '..\..\global\delphi\excmagic\unit\ExcMagicGUI.pas',
  ExcMagic in '..\..\global\delphi\excmagic\unit\ExcMagic.pas',
  ExcMagicUtils in '..\..\global\delphi\excmagic\unit\ExcMagicUtils.pas',
  ExcMemMap in '..\..\global\delphi\excmagic\unit\ExcMemMap.pas',
  ExcUnmangle in '..\..\global\delphi\excmagic\unit\ExcUnmangle.pas',
  ExcMagicDlg in '..\..\global\delphi\excmagic\unit\ExcMagicDlg.pas',
  ExcMagicPatch in '..\..\global\delphi\excmagic\unit\ExcMagicPatch.pas',
  ExcStackTracer in '..\..\global\delphi\excmagic\unit\ExcStackTracer.pas',
  VersionInfo in '..\..\..\..\common\windows\delphi\general\VersionInfo.pas',
  ExcMagicDefaultFilter in '..\..\global\delphi\excmagic\unit\ExcMagicDefaultFilter.pas',
  main in 'main.pas' {Form2},
  httpuploader in 'httpuploader.pas',
  Upload_Settings in '..\..\..\..\common\windows\delphi\general\Upload_Settings.pas',
  urlutil in '..\..\global\delphi\general\urlutil.pas',
  RegistryKeys in '..\..\..\..\common\windows\delphi\general\RegistryKeys.pas',
  DebugPaths in '..\..\..\..\common\windows\delphi\general\DebugPaths.pas',
  wininet5 in '..\..\..\..\common\windows\delphi\general\wininet5.pas',
  GlobalProxySettings in '..\..\..\..\common\windows\delphi\general\GlobalProxySettings.pas',
  DCPbase64 in '..\..\global\delphi\crypt\DCPbase64.pas',
  DCPconst in '..\..\global\delphi\crypt\DCPconst.pas',
  DCPcrypt2 in '..\..\global\delphi\crypt\DCPcrypt2.pas',
  DCPrc4 in '..\..\global\delphi\crypt\DCPrc4.pas',
  UfrmOnlineUpdateSetup in '..\..\global\delphi\online\UfrmOnlineUpdateSetup.pas' {frmOnlineUpdateSetup};

{$R *.res}

var
  DefaultExcMagicFilter: TDefaultExcMagicFilter;
begin
  DefaultExcMagicFilter := TDefaultExcMagicFilter.Create;
  ExceptionHook.RegisterExceptionFilter(Exception, DefaultExcMagicFilter, False);
  Application.Initialize;
  Application.CreateForm(TForm2, Form2);
  Application.Run;
end.
