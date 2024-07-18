program regressiontest;

uses
  System.Win.ComObj,
  Winapi.ActiveX,
  Forms,
  UfrmRegressionTestsWaitForIdle in 'UfrmRegressionTestsWaitForIdle.pas' {frmRegressionTestsWaitForIdle},
  UfrmRegressionTests in 'UfrmRegressionTests.pas' {frmRegressionTests},
  KeyNames in '..\..\..\..\common\windows\delphi\general\KeyNames.pas',
  RegistryKeys in '..\..\..\..\common\windows\delphi\general\RegistryKeys.pas',
  Registry in '..\..\global\delphi\vcl\Registry.pas',
  ResourceStrings in '..\..\global\delphi\general\ResourceStrings.pas',
  keymanstrings in 'keymanstrings.pas',
  VKeys in '..\..\..\..\common\windows\delphi\general\VKeys.pas',
  GetOsVersion in '..\..\..\..\common\windows\delphi\general\GetOsVersion.pas',
  kmxfileconsts in '..\..\..\..\common\windows\delphi\keyboards\kmxfileconsts.pas',
  KeymanVersion in '..\..\..\..\common\windows\delphi\general\KeymanVersion.pas',
  RedistFiles in '..\..\developer\TIKE\main\RedistFiles.pas',
  ErrorControlledRegistry in '..\..\..\..\common\windows\delphi\vcl\ErrorControlledRegistry.pas',
  DebugPaths in '..\..\..\..\common\windows\delphi\general\DebugPaths.pas',
  utilexception in '..\..\global\delphi\general\utilexception.pas',
  utilexecute in '..\..\..\..\common\windows\delphi\general\utilexecute.pas',
  Unicode in '..\..\..\..\common\windows\delphi\general\Unicode.pas',
  VersionInfo in '..\..\..\..\common\windows\delphi\general\VersionInfo.pas',
  klog in '..\..\..\..\common\windows\delphi\general\klog.pas',
  keyman32_int in '..\..\global\delphi\general\keyman32_int.pas',
  SystemDebugPath in '..\..\global\delphi\general\SystemDebugPath.pas',
  msctf in '..\..\global\delphi\winapi\msctf.pas',
  DebugUtils in 'DebugUtils.pas',
  kmcomapi_TLB in '..\..\engine\kmcomapi\kmcomapi_TLB.pas',
  UserMessages in '..\..\..\..\common\windows\delphi\general\UserMessages.pas',
  utiltsf in '..\..\global\delphi\general\utiltsf.pas';

{$R *.RES}

begin
  CoinitFlags := COINIT_APARTMENTTHREADED;
  Application.Initialize;
  Application.CreateForm(TfrmRegressionTests, frmRegressionTests);
  Application.Run;
end.
