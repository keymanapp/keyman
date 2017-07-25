program regressiontest;

uses
  System.Win.ComObj,
  Winapi.ActiveX,
  Forms,
  UfrmRegressionTestsWaitForIdle in 'UfrmRegressionTestsWaitForIdle.pas' {frmRegressionTestsWaitForIdle},
  UfrmRegressionTests in 'UfrmRegressionTests.pas' {frmRegressionTests},
  KeyNames in '..\..\global\delphi\general\KeyNames.pas',
  RegistryKeys in '..\..\global\delphi\general\RegistryKeys.pas',
  Registry in '..\..\global\delphi\vcl\Registry.pas',
  ResourceStrings in '..\..\global\delphi\general\ResourceStrings.pas',
  keymanstrings in 'keymanstrings.pas',
  VKeys in '..\..\global\delphi\general\VKeys.pas',
  GetOsVersion in '..\..\global\delphi\general\GetOsVersion.pas',
  kmxfileconsts in '..\..\global\delphi\general\kmxfileconsts.pas',
  KeymanVersion in '..\..\global\delphi\general\KeymanVersion.pas',
  RedistFiles in '..\..\developer\TIKE\main\RedistFiles.pas',
  ErrorControlledRegistry in '..\..\global\delphi\vcl\ErrorControlledRegistry.pas',
  DebugPaths in '..\..\global\delphi\general\DebugPaths.pas',
  utilexception in '..\..\global\delphi\general\utilexception.pas',
  utilexecute in '..\..\global\delphi\general\utilexecute.pas',
  Unicode in '..\..\global\delphi\general\Unicode.pas',
  VersionInfo in '..\..\global\delphi\general\VersionInfo.pas',
  klog in '..\..\global\delphi\general\klog.pas',
  keyman32_int in '..\..\global\delphi\general\keyman32_int.pas',
  compile in '..\..\global\delphi\general\compile.pas',
  SystemDebugPath in '..\..\global\delphi\general\SystemDebugPath.pas',
  msctf in '..\..\global\delphi\winapi\msctf.pas',
  DebugUtils in 'DebugUtils.pas',
  kmcomapi_TLB in '..\..\engine\kmcomapi\kmcomapi_TLB.pas',
  UserMessages in '..\..\global\delphi\general\UserMessages.pas',
  utiltsf in '..\..\global\delphi\general\utiltsf.pas';

{$R *.RES}

begin
  CoinitFlags := COINIT_APARTMENTTHREADED;
  Application.Initialize;
  Application.CreateForm(TfrmRegressionTests, frmRegressionTests);
  Application.Run;
end.
