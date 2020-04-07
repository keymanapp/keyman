program kmconvert;

{$APPTYPE CONSOLE}

uses
  System.SysUtils,
  Keyman.Developer.System.KeymanConvertMain in 'Keyman.Developer.System.KeymanConvertMain.pas',
  Keyman.Developer.System.ImportWindowsKeyboard in 'Keyman.Developer.System.ImportWindowsKeyboard.pas',
  RegistryKeys in '..\..\global\delphi\general\RegistryKeys.pas',
  KeymanVersion in '..\..\global\delphi\general\KeymanVersion.pas',
  utilexecute in '..\..\global\delphi\general\utilexecute.pas',
  Keyman.Developer.System.KeyboardProjectTemplate in 'Keyman.Developer.System.KeyboardProjectTemplate.pas',
  UKeymanTargets in '..\..\global\delphi\general\UKeymanTargets.pas',
  utilstr in '..\..\global\delphi\general\utilstr.pas',
  Unicode in '..\..\global\delphi\general\Unicode.pas',
  StockFileNames in '..\..\global\delphi\cust\StockFileNames.pas',
  utilfiletypes in '..\..\global\delphi\general\utilfiletypes.pas',
  KeyboardParser in '..\TIKE\main\KeyboardParser.pas',
  kmxfile in '..\..\global\delphi\general\kmxfile.pas',
  kmxfileconsts in '..\..\global\delphi\general\kmxfileconsts.pas',
  OnlineConstants in '..\..\global\delphi\productactivation\OnlineConstants.pas',
  ExtShiftState in '..\..\global\delphi\comp\ExtShiftState.pas',
  utilsystem in '..\..\global\delphi\general\utilsystem.pas',
  VKeys in '..\..\global\delphi\general\VKeys.pas',
  WindowsLanguages in '..\..\global\delphi\general\WindowsLanguages.pas',
  GetOsVersion in '..\..\global\delphi\general\GetOsVersion.pas',
  CRC32 in '..\..\global\delphi\general\CRC32.pas',
  KeyNames in '..\..\global\delphi\general\KeyNames.pas',
  utildir in '..\..\global\delphi\general\utildir.pas',
  TextFileFormat in '..\TIKE\main\TextFileFormat.pas',
  kpsfile in '..\..\global\delphi\general\kpsfile.pas',
  PackageInfo in '..\..\global\delphi\general\PackageInfo.pas',
  JsonUtil in '..\..\global\delphi\general\JsonUtil.pas',
  VersionInfo in '..\..\global\delphi\general\VersionInfo.pas',
  PackageFileFormats in '..\..\global\delphi\general\PackageFileFormats.pas',
  Keyman.Developer.System.Project.ProjectLog in '..\TIKE\project\Keyman.Developer.System.Project.ProjectLog.pas',
  BCP47Tag in '..\..\global\delphi\general\BCP47Tag.pas',
  Keyman.System.CanonicalLanguageCodeUtils in '..\..\global\delphi\general\Keyman.System.CanonicalLanguageCodeUtils.pas',
  Keyman.System.LanguageCodeUtils in '..\..\global\delphi\general\Keyman.System.LanguageCodeUtils.pas',
  Keyman.System.KeyboardJSInfo in '..\..\global\delphi\keyboards\Keyman.System.KeyboardJSInfo.pas',
  Keyman.System.KeyboardUtils in '..\..\global\delphi\keyboards\Keyman.System.KeyboardUtils.pas',
  Keyman.System.KMXFileLanguages in '..\..\global\delphi\keyboards\Keyman.System.KMXFileLanguages.pas',
  Keyman.System.Standards.BCP47SubtagRegistry in '..\..\global\delphi\standards\Keyman.System.Standards.BCP47SubtagRegistry.pas',
  Keyman.System.Standards.BCP47SuppressScriptRegistry in '..\..\global\delphi\standards\Keyman.System.Standards.BCP47SuppressScriptRegistry.pas',
  Keyman.System.Standards.ISO6393ToBCP47Registry in '..\..\global\delphi\standards\Keyman.System.Standards.ISO6393ToBCP47Registry.pas',
  Keyman.System.Standards.LCIDToBCP47Registry in '..\..\global\delphi\standards\Keyman.System.Standards.LCIDToBCP47Registry.pas',
  Keyman.System.Standards.NRSIAllTagsRegistry in '..\..\global\delphi\standards\Keyman.System.Standards.NRSIAllTagsRegistry.pas',
  Keyman.System.RegExGroupHelperRSP19902 in '..\..\global\delphi\general\Keyman.System.RegExGroupHelperRSP19902.pas',
  Keyman.System.PackageInfoRefreshKeyboards in '..\..\global\delphi\packages\Keyman.System.PackageInfoRefreshKeyboards.pas',
  Keyman.Developer.System.Project.ProjectFile in '..\TIKE\project\Keyman.Developer.System.Project.ProjectFile.pas',
  Keyman.Developer.System.Project.ProjectFiles in '..\TIKE\project\Keyman.Developer.System.Project.ProjectFiles.pas',
  Keyman.Developer.System.Project.ProjectSaver in '..\TIKE\project\Keyman.Developer.System.Project.ProjectSaver.pas',
  Keyman.Developer.System.Project.ProjectFileType in '..\TIKE\project\Keyman.Developer.System.Project.ProjectFileType.pas',
  mrulist in '..\TIKE\main\mrulist.pas',
  ErrorControlledRegistry in '..\..\global\delphi\vcl\ErrorControlledRegistry.pas',
  TempFileManager in '..\..\global\delphi\general\TempFileManager.pas',
  klog in '..\..\global\delphi\general\klog.pas',
  RedistFiles in '..\TIKE\main\RedistFiles.pas',
  DebugPaths in '..\..\global\delphi\general\DebugPaths.pas',
  Upload_Settings in '..\..\global\delphi\general\Upload_Settings.pas',
  Keyman.Developer.System.Project.Project in '..\TIKE\project\Keyman.Developer.System.Project.Project.pas',
  Keyman.Developer.System.Project.ProjectLoader in '..\TIKE\project\Keyman.Developer.System.Project.ProjectLoader.pas',
  Keyman.Developer.System.Project.kmnProjectFile in '..\TIKE\project\Keyman.Developer.System.Project.kmnProjectFile.pas',
  Keyman.Developer.System.Project.kpsProjectFile in '..\TIKE\project\Keyman.Developer.System.Project.kpsProjectFile.pas',
  VisualKeyboard in '..\..\global\delphi\visualkeyboard\VisualKeyboard.pas',
  VKeyChars in '..\..\global\delphi\general\VKeyChars.pas',
  VisualKeyboardLoaderBinary in '..\..\global\delphi\visualkeyboard\VisualKeyboardLoaderBinary.pas',
  VisualKeyboardLoaderXML in '..\..\global\delphi\visualkeyboard\VisualKeyboardLoaderXML.pas',
  VisualKeyboardSaverBinary in '..\..\global\delphi\visualkeyboard\VisualKeyboardSaverBinary.pas',
  VisualKeyboardSaverXML in '..\..\global\delphi\visualkeyboard\VisualKeyboardSaverXML.pas',
  UMD5Hash in '..\..\global\delphi\general\UMD5Hash.pas',
  Keyman.Developer.System.KMConvertParameters in 'Keyman.Developer.System.KMConvertParameters.pas',
  Keyman.Developer.System.ImportKeyboardDLL in 'Keyman.Developer.System.ImportKeyboardDLL.pas',
  ScanCodeMap in '..\..\global\delphi\general\ScanCodeMap.pas',
  Keyman.Developer.System.TouchLayoutToVisualKeyboardConverter in 'Keyman.Developer.System.TouchLayoutToVisualKeyboardConverter.pas',
  OnScreenKeyboardData in '..\..\global\delphi\visualkeyboard\OnScreenKeyboardData.pas',
  TouchLayout in '..\TIKE\oskbuilder\TouchLayout.pas',
  TouchLayoutDefinitions in '..\TIKE\oskbuilder\TouchLayoutDefinitions.pas',
  TouchLayoutUtils in '..\TIKE\oskbuilder\TouchLayoutUtils.pas',
  KeyboardFonts in '..\..\global\delphi\general\KeyboardFonts.pas',
  Keyman.System.Util.RenderLanguageIcon in '..\..\global\delphi\ui\Keyman.System.Util.RenderLanguageIcon.pas',
  utilicon in '..\..\global\delphi\general\utilicon.pas',
  CompileErrorCodes in '..\..\global\delphi\general\CompileErrorCodes.pas',
  Keyman.Developer.System.ModelProjectTemplate in 'Keyman.Developer.System.ModelProjectTemplate.pas',
  Keyman.Developer.System.Project.modelTsProjectFile in '..\TIKE\project\Keyman.Developer.System.Project.modelTsProjectFile.pas',
  Keyman.Developer.System.Project.wordlistTsvProjectFile in '..\TIKE\project\Keyman.Developer.System.Project.wordlistTsvProjectFile.pas',
  Sentry.Client in '..\..\ext\sentry\Sentry.Client.pas',
  Sentry.Client.Console in '..\..\ext\sentry\Sentry.Client.Console.pas',
  sentry in '..\..\ext\sentry\sentry.pas',
  Keyman.System.KeymanSentryClient in '..\..\global\delphi\general\Keyman.System.KeymanSentryClient.pas',
  KeymanPaths in '..\..\global\delphi\general\KeymanPaths.pas';

{$R icons.RES}
{$R version.res}
{$R manifest.res}

const
  LOGGER_DEVELOPER_TOOLS_KMCONVERT = TKeymanSentryClient.LOGGER_DEVELOPER_TOOLS + '.kmconvert';
begin
  TKeymanSentryClient.Start(TSentryClientConsole, kscpDeveloper, LOGGER_DEVELOPER_TOOLS_KMCONVERT);
  try
    try
      TKeymanSentryClient.Validate;
      Run;
    except
      on E: Exception do
        if not SentryHandleException(E) then raise;
    end;
  finally
    TKeymanSentryClient.Stop;
  end;
end.
