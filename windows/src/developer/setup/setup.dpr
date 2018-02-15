program setup;

uses
  bootstrapmain in 'bootstrapmain.pas',
  UfrmDownloadProgress in 'UfrmDownloadProgress.pas',
  UfrmRun in 'UfrmRun.pas',
  CommonControls in 'CommonControls.pas',
  VersionInfo in '..\..\global\delphi\general\VersionInfo.pas',
  httpuploader in '..\..\global\delphi\general\httpuploader.pas',
  RegistryKeys in '..\..\global\delphi\general\RegistryKeys.pas',
  klog in '..\..\global\delphi\general\klog.pas',
  Upload_Settings in '..\..\global\delphi\general\Upload_Settings.pas',
  DebugPaths in '..\..\global\delphi\general\DebugPaths.pas',
  GetOsVersion in '..\..\global\delphi\general\GetOsVersion.pas',
  SystemDebugPath in '..\..\global\delphi\general\SystemDebugPath.pas',
  TntDialogHelp in 'TntDialogHelp.pas',
  httpuploader_messageprocessor_windows in 'httpuploader_messageprocessor_windows.pas',
  SetupForm in 'SetupForm.pas',
  resource in 'resource.pas',
  SetupStrings in 'SetupStrings.pas',
  utilsystem in '..\..\global\delphi\general\utilsystem.pas',
  wininet5 in '..\..\global\delphi\general\wininet5.pas',
  GlobalProxySettings in '..\..\global\delphi\general\GlobalProxySettings.pas',
  OnlineConstants in '..\..\global\delphi\productactivation\OnlineConstants.pas',
  ErrorControlledRegistry in '..\..\global\delphi\vcl\ErrorControlledRegistry.pas',
  Unicode in '..\..\global\delphi\general\Unicode.pas',
  utilexecute in '..\..\global\delphi\general\utilexecute.pas',
  KeymanVersion in '..\..\global\delphi\general\KeymanVersion.pas',
  SFX in '..\..\global\delphi\setup\SFX.pas',
  Keyman.System.UpdateCheckResponse in '..\..\global\delphi\general\Keyman.System.UpdateCheckResponse.pas';

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
