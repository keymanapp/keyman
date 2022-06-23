program kmcomp;

{$APPTYPE CONSOLE}

uses
  System.SysUtils,
  main in 'main.pas',
  compile in '..\..\..\windows\src\global\delphi\general\compile.pas',
  VersionInfo in '..\..\..\windows\src\global\delphi\general\VersionInfo.pas',
  RegistryKeys in '..\..\..\windows\src\global\delphi\general\RegistryKeys.pas',
  kccompilepackage in 'kccompilepackage.pas',
  CompilePackage in '..\..\..\windows\src\global\delphi\general\CompilePackage.pas',
  CRC32 in '..\..\..\windows\src\global\delphi\general\CRC32.pas',
  kpsfile in '..\..\..\windows\src\global\delphi\general\kpsfile.pas',
  PackageInfo in '..\..\..\windows\src\global\delphi\general\PackageInfo.pas',
  PackageFileFormats in '..\..\..\windows\src\global\delphi\general\PackageFileFormats.pas',
  kmpinffile in '..\..\..\windows\src\global\delphi\general\kmpinffile.pas',
  RedistFiles in '..\tike\main\RedistFiles.pas',
  DebugPaths in '..\..\..\windows\src\global\delphi\general\DebugPaths.pas',
  httpuploader in '..\..\..\windows\src\global\delphi\general\httpuploader.pas',
  httpuploader_messageprocessor_forms in '..\..\..\windows\src\global\delphi\general\httpuploader_messageprocessor_forms.pas',
  utilfiletypes in '..\..\..\windows\src\global\delphi\general\utilfiletypes.pas',
  CustomisationStorage in '..\..\..\windows\src\global\delphi\cust\CustomisationStorage.pas',
  klog in '..\..\..\windows\src\global\delphi\general\klog.pas',
  KeyNames in '..\..\..\windows\src\global\delphi\general\KeyNames.pas',
  StockFileNames in '..\..\..\windows\src\global\delphi\cust\StockFileNames.pas',
  KeymanDeveloperOptions in '..\tike\main\KeymanDeveloperOptions.pas',
  Upload_Settings in '..\..\..\windows\src\global\delphi\general\Upload_Settings.pas',
  utilstr in '..\..\..\windows\src\global\delphi\general\utilstr.pas',
  utilsystem in '..\..\..\windows\src\global\delphi\general\utilsystem.pas',
  utildir in '..\..\..\windows\src\global\delphi\general\utildir.pas',
  utilkeyboard in '..\..\..\windows\src\global\delphi\general\utilkeyboard.pas',
  unicode in '..\..\..\windows\src\global\delphi\general\unicode.pas',
  utilhttp in '..\..\..\windows\src\global\delphi\general\utilhttp.pas',
  GetOsVersion in '..\..\..\windows\src\global\delphi\general\GetOsVersion.pas',
  UfrmTike in '..\tike\main\UfrmTike.pas' {TikeForm: TTntForm},
  CompilePackageInstaller in '..\..\..\windows\src\global\delphi\general\CompilePackageInstaller.pas',
  UTikeDebugMode in '..\tike\main\UTikeDebugMode.pas',
  CompileKeymanWeb in '..\tike\compile\CompileKeymanWeb.pas',
  VisualKeyboard in '..\..\..\common\windows\delphi\visualkeyboard\VisualKeyboard.pas',
  KeymanWebKeyCodes in '..\tike\compile\KeymanWebKeyCodes.pas',
  ExtShiftState in '..\..\..\windows\src\global\delphi\comp\ExtShiftState.pas',
  kmxfileconsts in '..\..\..\windows\src\global\delphi\general\kmxfileconsts.pas',
  wininet5 in '..\..\..\windows\src\global\delphi\general\wininet5.pas',
  kmxfileutils in '..\..\..\windows\src\global\delphi\general\kmxfileutils.pas',
  GlobalProxySettings in '..\..\..\windows\src\global\delphi\general\GlobalProxySettings.pas',
  VKeyChars in '..\..\..\windows\src\global\delphi\general\VKeyChars.pas',
  utilmsxml in '..\..\..\windows\src\global\delphi\general\utilmsxml.pas',
  ErrorControlledRegistry in '..\..\..\windows\src\global\delphi\vcl\ErrorControlledRegistry.pas',
  utilexecute in '..\..\..\windows\src\global\delphi\general\utilexecute.pas',
  KeymanVersion in '..\..\..\windows\src\global\delphi\general\KeymanVersion.pas',
  kmxfile in '..\..\..\windows\src\global\delphi\general\kmxfile.pas',
  Glossary in '..\..\..\windows\src\global\delphi\general\Glossary.pas',
  VKeys in '..\..\..\windows\src\global\delphi\general\VKeys.pas',
  CompileErrorCodes in '..\..\..\windows\src\global\delphi\general\CompileErrorCodes.pas',
  TouchLayout in '..\tike\oskbuilder\TouchLayout.pas',
  TouchLayoutDefinitions in '..\tike\oskbuilder\TouchLayoutDefinitions.pas',
  TouchLayoutUtils in '..\tike\oskbuilder\TouchLayoutUtils.pas',
  KeyboardFonts in '..\..\..\windows\src\global\delphi\general\KeyboardFonts.pas',
  KeyboardParser in '..\tike\main\KeyboardParser.pas',
  WindowsLanguages in '..\..\..\windows\src\global\delphi\general\WindowsLanguages.pas',
  TempFileManager in '..\..\..\windows\src\global\delphi\general\TempFileManager.pas',
  JsonUtil in '..\..\..\windows\src\global\delphi\general\JsonUtil.pas',
  TikeUnicodeData in '..\tike\main\TikeUnicodeData.pas',
  UnicodeData in '..\..\..\common\windows\delphi\charmap\UnicodeData.pas',
  ttinfo in '..\..\..\windows\src\global\delphi\general\ttinfo.pas',
  ADODB_TLB in '..\..\..\common\windows\delphi\tlb\ADODB_TLB.pas',
  ADOX_TLB in '..\..\..\common\windows\delphi\tlb\ADOX_TLB.pas',
  UKeymanTargets in '..\..\..\windows\src\global\delphi\general\UKeymanTargets.pas',
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
  UserMessages in '..\..\..\windows\src\global\delphi\general\UserMessages.pas',
  VisualKeyboardLoaderBinary in '..\..\..\common\windows\delphi\visualkeyboard\VisualKeyboardLoaderBinary.pas',
  VisualKeyboardLoaderXML in '..\..\..\common\windows\delphi\visualkeyboard\VisualKeyboardLoaderXML.pas',
  VisualKeyboardSaverBinary in '..\..\..\common\windows\delphi\visualkeyboard\VisualKeyboardSaverBinary.pas',
  VisualKeyboardSaverXML in '..\..\..\common\windows\delphi\visualkeyboard\VisualKeyboardSaverXML.pas',
  kccompilekvk in 'kccompilekvk.pas',
  ValidateKeyboardInfo in '..\tike\compile\ValidateKeyboardInfo.pas',
  MergeKeyboardInfo in '..\tike\compile\MergeKeyboardInfo.pas',
  JsonExtractKeyboardInfo in '..\tike\compile\JsonExtractKeyboardInfo.pas',
  Keyman.System.PackageInfoRefreshKeyboards in '..\common\delphi\packages\Keyman.System.PackageInfoRefreshKeyboards.pas',
  Keyman.System.KeyboardJSInfo in '..\common\delphi\keyboards\Keyman.System.KeyboardJSInfo.pas',
  Keyman.System.KeyboardUtils in '..\common\delphi\keyboards\Keyman.System.KeyboardUtils.pas',
  Keyman.System.KMXFileLanguages in '..\common\delphi\keyboards\Keyman.System.KMXFileLanguages.pas',
  Keyman.System.Standards.ISO6393ToBCP47Registry in '..\..\..\common\windows\delphi\standards\Keyman.System.Standards.ISO6393ToBCP47Registry.pas',
  Keyman.System.Standards.LCIDToBCP47Registry in '..\..\..\common\windows\delphi\standards\Keyman.System.Standards.LCIDToBCP47Registry.pas',
  Keyman.System.KeyboardInfoFile in '..\common\delphi\keyboards\Keyman.System.KeyboardInfoFile.pas',
  BCP47Tag in '..\..\..\windows\src\global\delphi\general\BCP47Tag.pas',
  Keyman.System.LanguageCodeUtils in '..\..\..\windows\src\global\delphi\general\Keyman.System.LanguageCodeUtils.pas',
  Keyman.System.RegExGroupHelperRSP19902 in '..\..\..\windows\src\global\delphi\general\Keyman.System.RegExGroupHelperRSP19902.pas',
  Keyman.System.Standards.BCP47SubtagRegistry in '..\..\..\common\windows\delphi\standards\Keyman.System.Standards.BCP47SubtagRegistry.pas',
  Keyman.System.Standards.BCP47SuppressScriptRegistry in '..\..\..\common\windows\delphi\standards\Keyman.System.Standards.BCP47SuppressScriptRegistry.pas',
  TextFileFormat in '..\tike\main\TextFileFormat.pas',
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
  Keyman.System.KeymanSentryClient in '..\..\..\windows\src\global\delphi\general\Keyman.System.KeymanSentryClient.pas',
  KeymanPaths in '..\..\..\windows\src\global\delphi\general\KeymanPaths.pas',
  Keyman.System.CanonicalLanguageCodeUtils in '..\..\..\windows\src\global\delphi\general\Keyman.System.CanonicalLanguageCodeUtils.pas',
  Keyman.System.Standards.LangTagsRegistry in '..\..\..\common\windows\delphi\standards\Keyman.System.Standards.LangTagsRegistry.pas',
  Keyman.Developer.System.Project.UrlRenderer in '..\tike\project\Keyman.Developer.System.Project.UrlRenderer.pas',
  Keyman.Developer.System.ValidateRepoChanges in 'Keyman.Developer.System.ValidateRepoChanges.pas',
  Keyman.Developer.System.KeymanDeveloperPaths in '..\tike\main\Keyman.Developer.System.KeymanDeveloperPaths.pas',
  Keyman.Developer.System.ValidateKpsFile in '..\..\..\windows\src\global\delphi\general\Keyman.Developer.System.ValidateKpsFile.pas';

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
