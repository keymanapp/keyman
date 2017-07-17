program setup;

uses
  bootstrapmain in 'bootstrapmain.pas',
  UfrmDownloadProgress in 'UfrmDownloadProgress.pas',
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
  MsiPackageCode in 'MsiPackageCode.pas',
  wininet5 in '..\..\global\delphi\general\wininet5.pas',
  GlobalProxySettings in '..\..\global\delphi\general\GlobalProxySettings.pas',
  OnlineConstants in '..\..\global\delphi\productactivation\OnlineConstants.pas',
  UCreateProcessAsShellUser in '..\..\global\delphi\general\UCreateProcessAsShellUser.pas',
  UfrmRunDesktop in 'UfrmRunDesktop.pas' {frmRunDesktop},
  UfrmInstallOptions in 'UfrmInstallOptions.pas' {frmInstallOptions},
  RunTools in 'RunTools.pas',
  UfrmHTML in 'UfrmHTML.pas' {frmHTML},
  KeymanEmbeddedWB in '..\..\global\delphi\comp\KeymanEmbeddedWB.pas',
  ErrLogPath in '..\..\global\delphi\general\ErrLogPath.pas',
  ShellUserRegistry in '..\..\global\delphi\general\ShellUserRegistry.pas',
  RegistryHelpers in 'RegistryHelpers.pas',
  ErrorControlledRegistry in '..\..\global\delphi\vcl\ErrorControlledRegistry.pas',
  utilexecute in '..\..\global\delphi\general\utilexecute.pas',
  Unicode in '..\..\global\delphi\general\Unicode.pas',
  KeymanVersion in '..\..\global\delphi\general\KeymanVersion.pas',
  KeymanPaths in '..\..\global\delphi\general\KeymanPaths.pas',
  SFX in '..\..\global\delphi\setup\SFX.pas';

{$R icons.res}
{$R version.res}
{$R manifest.res}

begin
  try
    Run;
  except
    //TODO: Handle exceptions with JCL
  end;
end.
