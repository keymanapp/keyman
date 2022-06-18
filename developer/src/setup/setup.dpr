program setup;

uses
  bootstrapmain in 'bootstrapmain.pas',
  UfrmDownloadProgress in 'UfrmDownloadProgress.pas',
  UfrmRun in 'UfrmRun.pas',
  CommonControls in 'CommonControls.pas',
  VersionInfo in '..\..\..\windows\src\global\delphi\general\VersionInfo.pas',
  httpuploader in '..\..\..\windows\src\global\delphi\general\httpuploader.pas',
  RegistryKeys in '..\..\..\windows\src\global\delphi\general\RegistryKeys.pas',
  klog in '..\..\..\windows\src\global\delphi\general\klog.pas',
  Upload_Settings in '..\..\..\windows\src\global\delphi\general\Upload_Settings.pas',
  DebugPaths in '..\..\..\windows\src\global\delphi\general\DebugPaths.pas',
  GetOsVersion in '..\..\..\common\windows\delphi\general\GetOsVersion.pas',
  TntDialogHelp in 'TntDialogHelp.pas',
  httpuploader_messageprocessor_windows in 'httpuploader_messageprocessor_windows.pas',
  SetupForm in 'SetupForm.pas',
  resource in 'resource.pas',
  SetupStrings in 'SetupStrings.pas',
  utilsystem in '..\..\..\windows\src\global\delphi\general\utilsystem.pas',
  wininet5 in '..\..\..\windows\src\global\delphi\general\wininet5.pas',
  GlobalProxySettings in '..\..\..\windows\src\global\delphi\general\GlobalProxySettings.pas',
  ErrorControlledRegistry in '..\..\..\windows\src\global\delphi\vcl\ErrorControlledRegistry.pas',
  Unicode in '..\..\..\windows\src\global\delphi\general\Unicode.pas',
  utilexecute in '..\..\..\windows\src\global\delphi\general\utilexecute.pas',
  KeymanVersion in '..\..\..\windows\src\global\delphi\general\KeymanVersion.pas',
  SFX in '..\..\..\common\windows\delphi\setup\SFX.pas',
  Keyman.System.UpdateCheckResponse in '..\..\..\windows\src\global\delphi\general\Keyman.System.UpdateCheckResponse.pas';

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
