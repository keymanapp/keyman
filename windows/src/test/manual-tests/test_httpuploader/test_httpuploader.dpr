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
  VersionInfo in '..\..\global\delphi\general\VersionInfo.pas',
  ExcMagicDefaultFilter in '..\..\global\delphi\excmagic\unit\ExcMagicDefaultFilter.pas',
  main in 'main.pas' {Form2},
  httpuploader in 'httpuploader.pas',
  Upload_Settings in '..\..\global\delphi\general\Upload_Settings.pas',
  urlutil in '..\..\global\delphi\general\urlutil.pas',
  RegistryKeys in '..\..\global\delphi\general\RegistryKeys.pas',
  DebugPaths in '..\..\global\delphi\general\DebugPaths.pas',
  wininet5 in '..\..\global\delphi\general\wininet5.pas',
  GlobalProxySettings in '..\..\global\delphi\general\GlobalProxySettings.pas',
  DCPbase64 in '..\..\global\delphi\crypt\DCPbase64.pas',
  DCPblockciphers in '..\..\global\delphi\crypt\DCPblockciphers.pas',
  DCPconst in '..\..\global\delphi\crypt\DCPconst.pas',
  DCPcrypt2 in '..\..\global\delphi\crypt\DCPcrypt2.pas',
  DCPrc4 in '..\..\global\delphi\crypt\Ciphers\DCPrc4.pas',
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
