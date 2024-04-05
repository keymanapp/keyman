program setup;

uses
  bootstrapmain in 'bootstrapmain.pas',
  UfrmDownloadProgress in 'UfrmDownloadProgress.pas',
  UfrmRun in 'UfrmRun.pas',
  CommonControls in 'CommonControls.pas',
  VersionInfo in '..\..\..\common\windows\delphi\general\VersionInfo.pas',
  httpuploader in '..\..\..\common\windows\delphi\general\httpuploader.pas',
  RegistryKeys in '..\..\..\common\windows\delphi\general\RegistryKeys.pas',
  klog in '..\..\..\common\windows\delphi\general\klog.pas',
  Upload_Settings in '..\..\..\common\windows\delphi\general\Upload_Settings.pas',
  DebugPaths in '..\..\..\common\windows\delphi\general\DebugPaths.pas',
  GetOsVersion in '..\..\..\common\windows\delphi\general\GetOsVersion.pas',
  TntDialogHelp in 'TntDialogHelp.pas',
  httpuploader_messageprocessor_windows in 'httpuploader_messageprocessor_windows.pas',
  SetupForm in 'SetupForm.pas',
  resource in 'resource.pas',
  SetupStrings in 'SetupStrings.pas',
  utilsystem in '..\..\..\common\windows\delphi\general\utilsystem.pas',
  wininet5 in '..\..\..\common\windows\delphi\general\wininet5.pas',
  GlobalProxySettings in '..\..\..\common\windows\delphi\general\GlobalProxySettings.pas',
  ErrorControlledRegistry in '..\..\..\common\windows\delphi\vcl\ErrorControlledRegistry.pas',
  Unicode in '..\..\..\common\windows\delphi\general\Unicode.pas',
  utilexecute in '..\..\..\common\windows\delphi\general\utilexecute.pas',
  KeymanVersion in '..\..\..\common\windows\delphi\general\KeymanVersion.pas',
  SFX in '..\..\..\common\windows\delphi\setup\SFX.pas',
  Keyman.System.UpdateCheckResponse in '..\..\..\common\windows\delphi\general\Keyman.System.UpdateCheckResponse.pas',
  utilhttp in '..\..\..\common\windows\delphi\general\utilhttp.pas';

{$R icons.res}
{$R version.res}
{$R manifest.res}

begin
  //try
    Run;
  {except
    ExceptionHook.LogException;
  end;}
end.
