program kmcomp;

{$APPTYPE CONSOLE}

uses
  System.SysUtils,
  main in 'main.pas',
  compile in '..\common\delphi\compiler\compile.pas',
  VersionInfo in '..\..\..\common\windows\delphi\general\VersionInfo.pas',
  RegistryKeys in '..\..\..\common\windows\delphi\general\RegistryKeys.pas',
  kccompilepackage in 'kccompilepackage.pas',
  CompilePackage in '..\common\delphi\compiler\CompilePackage.pas',
  kpsfile in '..\common\delphi\packages\kpsfile.pas',
  PackageInfo in '..\..\..\common\windows\delphi\packages\PackageInfo.pas',
  PackageFileFormats in '..\..\..\common\windows\delphi\packages\PackageFileFormats.pas',
  kmpinffile in '..\..\..\common\windows\delphi\packages\kmpinffile.pas',
  RedistFiles in '..\tike\main\RedistFiles.pas',
  DebugPaths in '..\..\..\common\windows\delphi\general\DebugPaths.pas',
  httpuploader in '..\..\..\common\windows\delphi\general\httpuploader.pas',
  httpuploader_messageprocessor_forms in '..\..\..\common\windows\delphi\general\httpuploader_messageprocessor_forms.pas',
  utilfiletypes in '..\..\..\common\windows\delphi\general\utilfiletypes.pas',
  klog in '..\..\..\common\windows\delphi\general\klog.pas',
  KeyNames in '..\..\..\common\windows\delphi\general\KeyNames.pas',
  StockFileNames in '..\..\..\common\windows\delphi\general\StockFileNames.pas',
  KeymanDeveloperOptions in '..\tike\main\KeymanDeveloperOptions.pas',
  Upload_Settings in '..\..\..\common\windows\delphi\general\Upload_Settings.pas',
  utilstr in '..\..\..\common\windows\delphi\general\utilstr.pas',
  utilsystem in '..\..\..\common\windows\delphi\general\utilsystem.pas',
  utildir in '..\..\..\common\windows\delphi\general\utildir.pas',
  utilkeyboard in '..\..\..\common\windows\delphi\keyboards\utilkeyboard.pas',
  Unicode in '..\..\..\common\windows\delphi\general\Unicode.pas',
  utilhttp in '..\..\..\common\windows\delphi\general\utilhttp.pas',
  GetOsVersion in '..\..\..\common\windows\delphi\general\GetOsVersion.pas',
  UfrmTike in '..\tike\main\UfrmTike.pas' {TikeForm: TTntForm},
  CompilePackageInstaller in '..\common\delphi\compiler\CompilePackageInstaller.pas',
  UTikeDebugMode in '..\tike\main\UTikeDebugMode.pas',
  VisualKeyboard in '..\..\..\common\windows\delphi\visualkeyboard\VisualKeyboard.pas',
  KeymanWebKeyCodes in '..\tike\compile\KeymanWebKeyCodes.pas',
  ExtShiftState in '..\..\..\common\windows\delphi\visualkeyboard\ExtShiftState.pas',
  kmxfileconsts in '..\..\..\common\windows\delphi\keyboards\kmxfileconsts.pas',
  wininet5 in '..\..\..\common\windows\delphi\general\wininet5.pas',
  kmxfileutils in '..\..\..\common\windows\delphi\keyboards\kmxfileutils.pas',
  GlobalProxySettings in '..\..\..\common\windows\delphi\general\GlobalProxySettings.pas',
  VKeyChars in '..\..\..\common\windows\delphi\general\VKeyChars.pas',
  ErrorControlledRegistry in '..\..\..\common\windows\delphi\vcl\ErrorControlledRegistry.pas',
  utilexecute in '..\..\..\common\windows\delphi\general\utilexecute.pas',
  KeymanVersion in '..\..\..\common\windows\delphi\general\KeymanVersion.pas',
  kmxfile in '..\..\..\common\windows\delphi\keyboards\kmxfile.pas',
  Glossary in '..\..\..\common\windows\delphi\general\Glossary.pas',
  VKeys in '..\..\..\common\windows\delphi\general\VKeys.pas',
  CompileErrorCodes in '..\common\delphi\compiler\CompileErrorCodes.pas',
  TouchLayout in '..\tike\oskbuilder\TouchLayout.pas',
  TouchLayoutDefinitions in '..\tike\oskbuilder\TouchLayoutDefinitions.pas',
  KeyboardFonts in '..\common\delphi\general\KeyboardFonts.pas',
  KeyboardParser in '..\tike\main\KeyboardParser.pas',
  WindowsLanguages in '..\common\delphi\general\WindowsLanguages.pas',
  TempFileManager in '..\..\..\common\windows\delphi\general\TempFileManager.pas',
  JsonUtil in '..\..\..\common\windows\delphi\general\JsonUtil.pas',
  TikeUnicodeData in '..\tike\main\TikeUnicodeData.pas',
  UnicodeData in '..\..\..\common\windows\delphi\charmap\UnicodeData.pas',
  ttinfo in '..\..\..\common\windows\delphi\general\ttinfo.pas',
  ADODB_TLB in '..\..\..\common\windows\delphi\tlb\ADODB_TLB.pas',
  ADOX_TLB in '..\..\..\common\windows\delphi\tlb\ADOX_TLB.pas',
  UKeymanTargets in '..\common\delphi\general\UKeymanTargets.pas',
  kccompileproject in 'kccompileproject.pas',
  Keyman.Developer.System.Project.ProjectFile in '..\tike\project\Keyman.Developer.System.Project.ProjectFile.pas',
  mrulist in '..\tike\main\mrulist.pas',
  Keyman.Developer.System.Project.Project in '..\tike\project\Keyman.Developer.System.Project.Project.pas',
  Keyman.Developer.System.Project.ProjectFiles in '..\tike\project\Keyman.Developer.System.Project.ProjectFiles.pas',
  Keyman.Developer.System.Project.ProjectFileType in '..\tike\project\Keyman.Developer.System.Project.ProjectFileType.pas',
  Keyman.Developer.System.Project.ProjectLoader in '..\tike\project\Keyman.Developer.System.Project.ProjectLoader.pas',
  Keyman.Developer.System.Project.ProjectSaver in '..\tike\project\Keyman.Developer.System.Project.ProjectSaver.pas',
  Keyman.Developer.System.Project.kmnProjectFile in '..\tike\project\Keyman.Developer.System.Project.kmnProjectFile.pas',
  Keyman.Developer.System.Project.kmxProjectFile in '..\tike\project\Keyman.Developer.System.Project.kmxProjectFile.pas',
  Keyman.Developer.System.Project.kpsProjectFile in '..\tike\project\Keyman.Developer.System.Project.kpsProjectFile.pas',
  Keyman.Developer.System.Project.kvkProjectFile in '..\tike\project\Keyman.Developer.System.Project.kvkProjectFile.pas',
  Keyman.Developer.System.Project.ProjectLog in '..\tike\project\Keyman.Developer.System.Project.ProjectLog.pas',
  UserMessages in '..\..\..\common\windows\delphi\general\UserMessages.pas',
  VisualKeyboardLoaderBinary in '..\..\..\common\windows\delphi\visualkeyboard\VisualKeyboardLoaderBinary.pas',
  VisualKeyboardLoaderXML in '..\..\..\common\windows\delphi\visualkeyboard\VisualKeyboardLoaderXML.pas',
  VisualKeyboardSaverBinary in '..\..\..\common\windows\delphi\visualkeyboard\VisualKeyboardSaverBinary.pas',
  VisualKeyboardSaverXML in '..\..\..\common\windows\delphi\visualkeyboard\VisualKeyboardSaverXML.pas',
  kccompilekvk in 'kccompilekvk.pas',
  MergeKeyboardInfo in '..\tike\compile\MergeKeyboardInfo.pas',
  JsonExtractKeyboardInfo in '..\tike\compile\JsonExtractKeyboardInfo.pas',
  Keyman.System.PackageInfoRefreshKeyboards in '..\common\delphi\packages\Keyman.System.PackageInfoRefreshKeyboards.pas',
  Keyman.System.KeyboardJSInfo in '..\common\delphi\keyboards\Keyman.System.KeyboardJSInfo.pas',
  Keyman.System.KeyboardUtils in '..\common\delphi\keyboards\Keyman.System.KeyboardUtils.pas',
  Keyman.System.KMXFileLanguages in '..\common\delphi\keyboards\Keyman.System.KMXFileLanguages.pas',
  Keyman.System.Standards.ISO6393ToBCP47Registry in '..\..\..\common\windows\delphi\standards\Keyman.System.Standards.ISO6393ToBCP47Registry.pas',
  Keyman.System.Standards.LCIDToBCP47Registry in '..\..\..\common\windows\delphi\standards\Keyman.System.Standards.LCIDToBCP47Registry.pas',
  Keyman.System.KeyboardInfoFile in '..\common\delphi\keyboards\Keyman.System.KeyboardInfoFile.pas',
  BCP47Tag in '..\..\..\common\windows\delphi\general\BCP47Tag.pas',
  Keyman.System.LanguageCodeUtils in '..\..\..\common\windows\delphi\general\Keyman.System.LanguageCodeUtils.pas',
  Keyman.System.RegExGroupHelperRSP19902 in '..\..\..\common\windows\delphi\vcl\Keyman.System.RegExGroupHelperRSP19902.pas',
  Keyman.System.Standards.BCP47SubtagRegistry in '..\..\..\common\windows\delphi\standards\Keyman.System.Standards.BCP47SubtagRegistry.pas',
  Keyman.System.Standards.BCP47SuppressScriptRegistry in '..\..\..\common\windows\delphi\standards\Keyman.System.Standards.BCP47SuppressScriptRegistry.pas',
  TextFileFormat in '..\common\delphi\general\TextFileFormat.pas',
  Keyman.Developer.System.Project.kmnProjectFileAction in '..\tike\project\Keyman.Developer.System.Project.kmnProjectFileAction.pas',
  Keyman.Developer.System.Project.kpsProjectFileAction in '..\tike\project\Keyman.Developer.System.Project.kpsProjectFileAction.pas',
  Keyman.System.PackageInfoRefreshLexicalModels in '..\common\delphi\packages\Keyman.System.PackageInfoRefreshLexicalModels.pas',
  Keyman.Developer.System.Project.modelTsProjectFile in '..\tike\project\Keyman.Developer.System.Project.modelTsProjectFile.pas',
  Keyman.Developer.System.Project.modelTsProjectFileAction in '..\tike\project\Keyman.Developer.System.Project.modelTsProjectFileAction.pas',
  Keyman.Developer.System.LexicalModelCompile in '..\common\delphi\lexicalmodels\Keyman.Developer.System.LexicalModelCompile.pas',
  Keyman.System.LexicalModelUtils in '..\common\delphi\lexicalmodels\Keyman.System.LexicalModelUtils.pas',
  Keyman.Developer.System.Project.ProjectLogConsole in 'Keyman.Developer.System.Project.ProjectLogConsole.pas',
  Sentry.Client in '..\..\..\common\windows\delphi\ext\sentry\Sentry.Client.pas',
  Sentry.Client.Console in '..\..\..\common\windows\delphi\ext\sentry\Sentry.Client.Console.pas',
  sentry in '..\..\..\common\windows\delphi\ext\sentry\sentry.pas',
  Keyman.System.KeymanSentryClient in '..\..\..\common\windows\delphi\general\Keyman.System.KeymanSentryClient.pas',
  KeymanPaths in '..\..\..\common\windows\delphi\general\KeymanPaths.pas',
  Keyman.System.CanonicalLanguageCodeUtils in '..\..\..\common\windows\delphi\general\Keyman.System.CanonicalLanguageCodeUtils.pas',
  Keyman.System.Standards.LangTagsRegistry in '..\..\..\common\windows\delphi\standards\Keyman.System.Standards.LangTagsRegistry.pas',
  Keyman.Developer.System.Project.UrlRenderer in '..\tike\project\Keyman.Developer.System.Project.UrlRenderer.pas',
  Keyman.Developer.System.ValidateRepoChanges in 'Keyman.Developer.System.ValidateRepoChanges.pas',
  Keyman.Developer.System.KeymanDeveloperPaths in '..\tike\main\Keyman.Developer.System.KeymanDeveloperPaths.pas',
  Keyman.Developer.System.ValidateKpsFile in '..\common\delphi\compiler\Keyman.Developer.System.ValidateKpsFile.pas',
  Keyman.Developer.System.KmcWrapper in '..\tike\compile\Keyman.Developer.System.KmcWrapper.pas';

{$R icons.RES}
{$R version.res}
{$R manifest.res}

const
{$IFDEF WIN64}
  LOGGER_DEVELOPER_TOOLS_KMCOMP = TKeymanSentryClient.LOGGER_DEVELOPER_TOOLS + '.kmcomp.x64';
{$ELSE}
  LOGGER_DEVELOPER_TOOLS_KMCOMP = TKeymanSentryClient.LOGGER_DEVELOPER_TOOLS + '.kmcomp';
{$ENDIF}
begin
  TKeymanSentryClient.Start(TSentryClientConsole, kscpDeveloper, LOGGER_DEVELOPER_TOOLS_KMCOMP);
  try
    try
      TKeymanSentryClient.Validate;
      Run;
    except
      on E: Exception do
        if not SentryHandleException(E) then
        begin
          writeln(E.Message);
          ExitCode := 99;
        end;
    end;
  finally
    TKeymanSentryClient.Stop;
  end;
end.
