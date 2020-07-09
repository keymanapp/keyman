program setup;

uses
  bootstrapmain in 'bootstrapmain.pas',
  UfrmDownloadProgress in 'UfrmDownloadProgress.pas',
  CommonControls in 'CommonControls.pas',
  VersionInfo in '..\..\global\delphi\general\VersionInfo.pas',
  RegistryKeys in '..\..\global\delphi\general\RegistryKeys.pas',
  Upload_Settings in '..\..\global\delphi\general\Upload_Settings.pas',
  DebugPaths in '..\..\global\delphi\general\DebugPaths.pas',
  GetOsVersion in '..\..\global\delphi\general\GetOsVersion.pas',
  TntDialogHelp in 'TntDialogHelp.pas',
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
  ShellUserRegistry in '..\..\global\delphi\general\ShellUserRegistry.pas',
  RegistryHelpers in 'RegistryHelpers.pas',
  ErrorControlledRegistry in '..\..\global\delphi\vcl\ErrorControlledRegistry.pas',
  utilexecute in '..\..\global\delphi\general\utilexecute.pas',
  Unicode in '..\..\global\delphi\general\Unicode.pas',
  KeymanVersion in '..\..\global\delphi\general\KeymanVersion.pas',
  KeymanPaths in '..\..\global\delphi\general\KeymanPaths.pas',
  SFX in '..\..\global\delphi\setup\SFX.pas',
  Keyman.System.UpdateCheckResponse in '..\..\global\delphi\general\Keyman.System.UpdateCheckResponse.pas',
  Keyman.System.UpgradeRegistryKeys in '..\..\global\delphi\general\Keyman.System.UpgradeRegistryKeys.pas',
  Keyman.Setup.System.InstallInfo in 'Keyman.Setup.System.InstallInfo.pas',
  Keyman.Setup.System.OnlineResourceCheck in 'Keyman.Setup.System.OnlineResourceCheck.pas',
  PackageInfo in '..\..\global\delphi\general\PackageInfo.pas',
  utilstr in '..\..\global\delphi\general\utilstr.pas',
  utilfiletypes in '..\..\global\delphi\general\utilfiletypes.pas',
  StockFileNames in '..\..\global\delphi\cust\StockFileNames.pas',
  utildir in '..\..\global\delphi\general\utildir.pas',
  JsonUtil in '..\..\global\delphi\general\JsonUtil.pas',
  kmpinffile in '..\..\global\delphi\general\kmpinffile.pas',
  PackageFileFormats in '..\..\global\delphi\general\PackageFileFormats.pas',
  Keyman.Setup.System.ResourceDownloader in 'Keyman.Setup.System.ResourceDownloader.pas';

{$R icons.res}
{$R version.res}
{$R manifest.res}

begin
  Run;
end.
