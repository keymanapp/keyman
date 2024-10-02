program setup;

uses
  bootstrapmain in 'bootstrapmain.pas',
  UfrmDownloadProgress in 'UfrmDownloadProgress.pas' {frmDownloadProgress},
  CommonControls in 'CommonControls.pas',
  VersionInfo in '..\..\..\..\common\windows\delphi\general\VersionInfo.pas',
  RegistryKeys in '..\..\..\..\common\windows\delphi\general\RegistryKeys.pas',
  Upload_Settings in '..\..\..\..\common\windows\delphi\general\Upload_Settings.pas',
  DebugPaths in '..\..\..\..\common\windows\delphi\general\DebugPaths.pas',
  GetOsVersion in '..\..\..\..\common\windows\delphi\general\GetOsVersion.pas',
  TntDialogHelp in 'TntDialogHelp.pas',
  SetupForm in 'SetupForm.pas',
  resource in 'resource.pas',
  SetupStrings in 'SetupStrings.pas',
  utilsystem in '..\..\..\..\common\windows\delphi\general\utilsystem.pas',
  Keyman.Setup.System.MsiUtils in 'Keyman.Setup.System.MsiUtils.pas',
  wininet5 in '..\..\..\..\common\windows\delphi\general\wininet5.pas',
  GlobalProxySettings in '..\..\..\..\common\windows\delphi\general\GlobalProxySettings.pas',
  UfrmRunDesktop in 'UfrmRunDesktop.pas' {frmRunDesktop},
  UfrmInstallOptions in 'UfrmInstallOptions.pas' {frmInstallOptions},
  RunTools in 'RunTools.pas',
  ShellUserRegistry in '..\..\global\delphi\general\ShellUserRegistry.pas',
  RegistryHelpers in 'RegistryHelpers.pas',
  ErrorControlledRegistry in '..\..\..\..\common\windows\delphi\vcl\ErrorControlledRegistry.pas',
  utilexecute in '..\..\..\..\common\windows\delphi\general\utilexecute.pas',
  Unicode in '..\..\..\..\common\windows\delphi\general\Unicode.pas',
  KeymanVersion in '..\..\..\..\common\windows\delphi\general\KeymanVersion.pas',
  KeymanPaths in '..\..\..\..\common\windows\delphi\general\KeymanPaths.pas',
  SFX in '..\..\..\..\common\windows\delphi\setup\SFX.pas',
  Keyman.System.UpdateCheckResponse in '..\..\..\..\common\windows\delphi\general\Keyman.System.UpdateCheckResponse.pas',
  Keyman.System.UpgradeRegistryKeys in '..\..\global\delphi\general\Keyman.System.UpgradeRegistryKeys.pas',
  Keyman.Setup.System.InstallInfo in 'Keyman.Setup.System.InstallInfo.pas',
  Keyman.Setup.System.OnlineResourceCheck in 'Keyman.Setup.System.OnlineResourceCheck.pas',
  PackageInfo in '..\..\..\..\common\windows\delphi\packages\PackageInfo.pas',
  utilstr in '..\..\..\..\common\windows\delphi\general\utilstr.pas',
  utilfiletypes in '..\..\..\..\common\windows\delphi\general\utilfiletypes.pas',
  StockFileNames in '..\..\..\..\common\windows\delphi\general\StockFileNames.pas',
  utildir in '..\..\..\..\common\windows\delphi\general\utildir.pas',
  JsonUtil in '..\..\..\..\common\windows\delphi\general\JsonUtil.pas',
  kmpinffile in '..\..\..\..\common\windows\delphi\packages\kmpinffile.pas',
  PackageFileFormats in '..\..\..\..\common\windows\delphi\packages\PackageFileFormats.pas',
  Keyman.Setup.System.ResourceDownloader in 'Keyman.Setup.System.ResourceDownloader.pas',
  httpuploader in '..\..\..\..\common\windows\delphi\general\httpuploader.pas',
  Keyman.System.MITLicense in '..\..\global\delphi\general\Keyman.System.MITLicense.pas',
  Keyman.Setup.System.Locales in 'Keyman.Setup.System.Locales.pas',
  Keyman.System.UILanguageManager in '..\..\global\delphi\general\Keyman.System.UILanguageManager.pas',
  Keyman.Setup.System.SetupUILanguageManager in 'Keyman.Setup.System.SetupUILanguageManager.pas',
  utilhttp in '..\..\..\..\common\windows\delphi\general\utilhttp.pas';

{$R icons.res}
{$R version.res}
{$R manifest.res}

begin
  Run;
end.
