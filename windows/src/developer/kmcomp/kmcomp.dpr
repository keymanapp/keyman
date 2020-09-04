program kmcomp;

{$APPTYPE CONSOLE}

uses
  System.SysUtils,
  main in 'main.pas',
  compile in '..\..\global\delphi\general\compile.pas',
  VersionInfo in '..\..\global\delphi\general\VersionInfo.pas',
  RegistryKeys in '..\..\global\delphi\general\RegistryKeys.pas',
  kccompilepackage in 'kccompilepackage.pas',
  CompilePackage in '..\..\global\delphi\general\CompilePackage.pas',
  CRC32 in '..\..\global\delphi\general\CRC32.pas',
  kpsfile in '..\..\global\delphi\general\kpsfile.pas',
  PackageInfo in '..\..\global\delphi\general\PackageInfo.pas',
  PackageFileFormats in '..\..\global\delphi\general\PackageFileFormats.pas',
  kmpinffile in '..\..\global\delphi\general\kmpinffile.pas',
  RedistFiles in '..\TIKE\main\RedistFiles.pas',
  DebugPaths in '..\..\global\delphi\general\DebugPaths.pas',
  httpuploader in '..\..\global\delphi\general\httpuploader.pas',
  httpuploader_messageprocessor_forms in '..\..\global\delphi\general\httpuploader_messageprocessor_forms.pas',
  utilfiletypes in '..\..\global\delphi\general\utilfiletypes.pas',
  CustomisationStorage in '..\..\global\delphi\cust\CustomisationStorage.pas',
  klog in '..\..\global\delphi\general\klog.pas',
  KeyNames in '..\..\global\delphi\general\KeyNames.pas',
  StockFileNames in '..\..\global\delphi\cust\StockFileNames.pas',
  OnlineConstants in '..\..\global\delphi\productactivation\OnlineConstants.pas',
  KeymanDeveloperOptions in '..\TIKE\main\KeymanDeveloperOptions.pas',
  Upload_Settings in '..\..\global\delphi\general\Upload_Settings.pas',
  utilstr in '..\..\global\delphi\general\utilstr.pas',
  utilsystem in '..\..\global\delphi\general\utilsystem.pas',
  utildir in '..\..\global\delphi\general\utildir.pas',
  utilkeyboard in '..\..\global\delphi\general\utilkeyboard.pas',
  unicode in '..\..\global\delphi\general\unicode.pas',
  utilhttp in '..\..\global\delphi\general\utilhttp.pas',
  GetOsVersion in '..\..\global\delphi\general\GetOsVersion.pas',
  UfrmTike in '..\TIKE\main\UfrmTike.pas' {TikeForm: TTntForm},
  CompilePackageInstaller in '..\..\global\delphi\general\CompilePackageInstaller.pas',
  UTikeDebugMode in '..\TIKE\main\UTikeDebugMode.pas',
  CompileKeymanWeb in '..\TIKE\compile\CompileKeymanWeb.pas',
  VisualKeyboard in '..\..\global\delphi\visualkeyboard\VisualKeyboard.pas',
  KeymanWebKeyCodes in '..\TIKE\compile\KeymanWebKeyCodes.pas',
  ExtShiftState in '..\..\global\delphi\comp\ExtShiftState.pas',
  kmxfileconsts in '..\..\global\delphi\general\kmxfileconsts.pas',
  wininet5 in '..\..\global\delphi\general\wininet5.pas',
  kmxfileutils in '..\..\global\delphi\general\kmxfileutils.pas',
  GlobalProxySettings in '..\..\global\delphi\general\GlobalProxySettings.pas',
  VKeyChars in '..\..\global\delphi\general\VKeyChars.pas',
  utilmsxml in '..\..\global\delphi\general\utilmsxml.pas',
  ErrorControlledRegistry in '..\..\global\delphi\vcl\ErrorControlledRegistry.pas',
  utilexecute in '..\..\global\delphi\general\utilexecute.pas',
  KeymanVersion in '..\..\global\delphi\general\KeymanVersion.pas',
  kmxfile in '..\..\global\delphi\general\kmxfile.pas',
  Glossary in '..\..\global\delphi\general\Glossary.pas',
  VKeys in '..\..\global\delphi\general\VKeys.pas',
  CompileErrorCodes in '..\..\global\delphi\general\CompileErrorCodes.pas',
  TouchLayout in '..\TIKE\oskbuilder\TouchLayout.pas',
  TouchLayoutDefinitions in '..\TIKE\oskbuilder\TouchLayoutDefinitions.pas',
  TouchLayoutUtils in '..\TIKE\oskbuilder\TouchLayoutUtils.pas',
  KeyboardFonts in '..\..\global\delphi\general\KeyboardFonts.pas',
  KeyboardParser in '..\TIKE\main\KeyboardParser.pas',
  WindowsLanguages in '..\..\global\delphi\general\WindowsLanguages.pas',
  TempFileManager in '..\..\global\delphi\general\TempFileManager.pas',
  JsonUtil in '..\..\global\delphi\general\JsonUtil.pas',
  TikeUnicodeData in '..\TIKE\main\TikeUnicodeData.pas',
  UnicodeData in '..\..\global\delphi\charmap\UnicodeData.pas',
  ttinfo in '..\..\global\delphi\general\ttinfo.pas',
  ADODB_TLB in '..\..\global\delphi\tlb\ADODB_TLB.pas',
  ADOX_TLB in '..\..\global\delphi\tlb\ADOX_TLB.pas',
  UKeymanTargets in '..\..\global\delphi\general\UKeymanTargets.pas',
  kccompileproject in 'kccompileproject.pas',
  Keyman.Developer.System.Project.ProjectFile in '..\TIKE\project\Keyman.Developer.System.Project.ProjectFile.pas',
  mrulist in '..\TIKE\main\mrulist.pas',
  Keyman.Developer.System.Project.Project in '..\TIKE\project\Keyman.Developer.System.Project.Project.pas',
  Keyman.Developer.System.Project.ProjectFiles in '..\TIKE\project\Keyman.Developer.System.Project.ProjectFiles.pas',
  Keyman.Developer.System.Project.ProjectFileType in '..\TIKE\project\Keyman.Developer.System.Project.ProjectFileType.pas',
  Keyman.Developer.System.Project.ProjectLoader in '..\TIKE\project\Keyman.Developer.System.Project.ProjectLoader.pas',
  Keyman.Developer.System.Project.ProjectSaver in '..\TIKE\project\Keyman.Developer.System.Project.ProjectSaver.pas',
  Keyman.Developer.System.Project.kmnProjectFile in '..\TIKE\project\Keyman.Developer.System.Project.kmnProjectFile.pas',
  Keyman.Developer.System.Project.kmxProjectFile in '..\TIKE\project\Keyman.Developer.System.Project.kmxProjectFile.pas',
  Keyman.Developer.System.Project.kpsProjectFile in '..\TIKE\project\Keyman.Developer.System.Project.kpsProjectFile.pas',
  Keyman.Developer.System.Project.kvkProjectFile in '..\TIKE\project\Keyman.Developer.System.Project.kvkProjectFile.pas',
  Keyman.Developer.System.Project.ProjectLog in '..\TIKE\project\Keyman.Developer.System.Project.ProjectLog.pas',
  UMD5Hash in '..\..\global\delphi\general\UMD5Hash.pas',
  UserMessages in '..\..\global\delphi\general\UserMessages.pas',
  VisualKeyboardLoaderBinary in '..\..\global\delphi\visualkeyboard\VisualKeyboardLoaderBinary.pas',
  VisualKeyboardLoaderXML in '..\..\global\delphi\visualkeyboard\VisualKeyboardLoaderXML.pas',
  VisualKeyboardSaverBinary in '..\..\global\delphi\visualkeyboard\VisualKeyboardSaverBinary.pas',
  VisualKeyboardSaverXML in '..\..\global\delphi\visualkeyboard\VisualKeyboardSaverXML.pas',
  kccompilekvk in 'kccompilekvk.pas',
  ValidateKeyboardInfo in '..\TIKE\compile\ValidateKeyboardInfo.pas',
  MergeKeyboardInfo in '..\TIKE\compile\MergeKeyboardInfo.pas',
  JsonExtractKeyboardInfo in '..\TIKE\compile\JsonExtractKeyboardInfo.pas',
  Keyman.System.PackageInfoRefreshKeyboards in '..\..\global\delphi\packages\Keyman.System.PackageInfoRefreshKeyboards.pas',
  Keyman.System.KeyboardJSInfo in '..\..\global\delphi\keyboards\Keyman.System.KeyboardJSInfo.pas',
  Keyman.System.KeyboardUtils in '..\..\global\delphi\keyboards\Keyman.System.KeyboardUtils.pas',
  Keyman.System.KMXFileLanguages in '..\..\global\delphi\keyboards\Keyman.System.KMXFileLanguages.pas',
  Keyman.System.Standards.ISO6393ToBCP47Registry in '..\..\global\delphi\standards\Keyman.System.Standards.ISO6393ToBCP47Registry.pas',
  Keyman.System.Standards.LCIDToBCP47Registry in '..\..\global\delphi\standards\Keyman.System.Standards.LCIDToBCP47Registry.pas',
  Keyman.System.KeyboardInfoFile in '..\..\global\delphi\keyboards\Keyman.System.KeyboardInfoFile.pas',
  BCP47Tag in '..\..\global\delphi\general\BCP47Tag.pas',
  Keyman.System.LanguageCodeUtils in '..\..\global\delphi\general\Keyman.System.LanguageCodeUtils.pas',
  Keyman.System.RegExGroupHelperRSP19902 in '..\..\global\delphi\general\Keyman.System.RegExGroupHelperRSP19902.pas',
  Keyman.System.Standards.BCP47SubtagRegistry in '..\..\global\delphi\standards\Keyman.System.Standards.BCP47SubtagRegistry.pas',
  Keyman.System.Standards.BCP47SuppressScriptRegistry in '..\..\global\delphi\standards\Keyman.System.Standards.BCP47SuppressScriptRegistry.pas',
  Keyman.System.Standards.NRSIAllTagsRegistry in '..\..\global\delphi\standards\Keyman.System.Standards.NRSIAllTagsRegistry.pas',
  TextFileFormat in '..\TIKE\main\TextFileFormat.pas',
  Keyman.Developer.System.Project.kmnProjectFileAction in '..\TIKE\project\Keyman.Developer.System.Project.kmnProjectFileAction.pas',
  Keyman.Developer.System.Project.kpsProjectFileAction in '..\TIKE\project\Keyman.Developer.System.Project.kpsProjectFileAction.pas',
  Keyman.System.PackageInfoRefreshLexicalModels in '..\..\global\delphi\packages\Keyman.System.PackageInfoRefreshLexicalModels.pas',
  Keyman.Developer.System.Project.modelTsProjectFile in '..\TIKE\project\Keyman.Developer.System.Project.modelTsProjectFile.pas',
  Keyman.Developer.System.Project.modelTsProjectFileAction in '..\TIKE\project\Keyman.Developer.System.Project.modelTsProjectFileAction.pas',
  Keyman.Developer.System.LexicalModelCompile in '..\..\global\delphi\lexicalmodels\Keyman.Developer.System.LexicalModelCompile.pas',
  Keyman.System.LexicalModelUtils in '..\..\global\delphi\lexicalmodels\Keyman.System.LexicalModelUtils.pas',
  Keyman.Developer.System.Project.ProjectLogConsole in 'Keyman.Developer.System.Project.ProjectLogConsole.pas' {$R icons.RES},
  Sentry.Client in '..\..\ext\sentry\Sentry.Client.pas',
  Sentry.Client.Console in '..\..\ext\sentry\Sentry.Client.Console.pas',
  sentry in '..\..\ext\sentry\sentry.pas',
  Keyman.System.KeymanSentryClient in '..\..\global\delphi\general\Keyman.System.KeymanSentryClient.pas',
  KeymanPaths in '..\..\global\delphi\general\KeymanPaths.pas',
  Keyman.System.CanonicalLanguageCodeUtils in '..\..\global\delphi\general\Keyman.System.CanonicalLanguageCodeUtils.pas',
  Keyman.System.Standards.LangTagsRegistry in '..\..\global\delphi\standards\Keyman.System.Standards.LangTagsRegistry.pas';

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
      if (ParamStr(1) = '-sentry-client-test-exception') and (ParamStr(2) = 'dll') then
      begin
        compile.Compiler_Diagnostic_Console(0);
        Exit;
      end;
      TKeymanSentryClient.Validate;
      Run;
    except
      on E: Exception do
        SentryHandleException(E);
    end;
  finally
    TKeymanSentryClient.Stop;
  end;
end.
