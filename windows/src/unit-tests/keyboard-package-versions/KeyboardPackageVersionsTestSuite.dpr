program KeyboardPackageVersionsTestSuite;

{$IFNDEF TESTINSIGHT}
{$APPTYPE CONSOLE}
{$ENDIF}{$STRONGLINKTYPES ON}
uses
  System.SysUtils,
  {$IFDEF TESTINSIGHT}
  TestInsight.DUnitX,
  {$ENDIF }
  DUnitX.Loggers.Console,
  DUnitX.Loggers.Xml.NUnit,
  DUnitX.TestFramework,
  Keyman.Test.System.CompilePackageVersioningTest in 'Keyman.Test.System.CompilePackageVersioningTest.pas',
  CompilePackage in '..\..\global\delphi\general\CompilePackage.pas',
  OnlineConstants in '..\..\global\delphi\productactivation\OnlineConstants.pas',
  VisualKeyboard in '..\..\global\delphi\visualkeyboard\VisualKeyboard.pas',
  kmpinffile in '..\..\global\delphi\general\kmpinffile.pas',
  kpsfile in '..\..\global\delphi\general\kpsfile.pas',
  PackageFileFormats in '..\..\global\delphi\general\PackageFileFormats.pas',
  PackageInfo in '..\..\global\delphi\general\PackageInfo.pas',
  utilfiletypes in '..\..\global\delphi\general\utilfiletypes.pas',
  StockFileNames in '..\..\global\delphi\cust\StockFileNames.pas',
  utilstr in '..\..\global\delphi\general\utilstr.pas',
  Unicode in '..\..\global\delphi\general\Unicode.pas',
  JsonUtil in '..\..\global\delphi\general\JsonUtil.pas',
  KeymanVersion in '..\..\global\delphi\general\KeymanVersion.pas',
  utildir in '..\..\global\delphi\general\utildir.pas',
  utilsystem in '..\..\global\delphi\general\utilsystem.pas',
  RegistryKeys in '..\..\global\delphi\general\RegistryKeys.pas',
  utilexecute in '..\..\global\delphi\general\utilexecute.pas',
  GetOsVersion in '..\..\global\delphi\general\GetOsVersion.pas',
  VersionInfo in '..\..\global\delphi\general\VersionInfo.pas',
  ExtShiftState in '..\..\global\delphi\comp\ExtShiftState.pas',
  VisualKeyboardLoaderBinary in '..\..\global\delphi\visualkeyboard\VisualKeyboardLoaderBinary.pas',
  VisualKeyboardLoaderXML in '..\..\global\delphi\visualkeyboard\VisualKeyboardLoaderXML.pas',
  VisualKeyboardSaverBinary in '..\..\global\delphi\visualkeyboard\VisualKeyboardSaverBinary.pas',
  VisualKeyboardSaverXML in '..\..\global\delphi\visualkeyboard\VisualKeyboardSaverXML.pas',
  VKeyChars in '..\..\global\delphi\general\VKeyChars.pas',
  VKeys in '..\..\global\delphi\general\VKeys.pas',
  ErrorControlledRegistry in '..\..\global\delphi\vcl\ErrorControlledRegistry.pas',
  kmxfile in '..\..\global\delphi\general\kmxfile.pas',
  CRC32 in '..\..\global\delphi\general\CRC32.pas',
  KeyNames in '..\..\global\delphi\general\KeyNames.pas',
  KeymanDeveloperOptions in '..\..\developer\TIKE\main\KeymanDeveloperOptions.pas',
  Keyman.System.PackageInfoRefreshKeyboards in '..\..\global\delphi\packages\Keyman.System.PackageInfoRefreshKeyboards.pas',
  Keyman.System.KeyboardJSInfo in '..\..\global\delphi\keyboards\Keyman.System.KeyboardJSInfo.pas',
  Keyman.System.KeyboardUtils in '..\..\global\delphi\keyboards\Keyman.System.KeyboardUtils.pas',
  Keyman.System.KMXFileLanguages in '..\..\global\delphi\keyboards\Keyman.System.KMXFileLanguages.pas',
  Keyman.System.Standards.ISO6393ToBCP47Registry in '..\..\global\delphi\standards\Keyman.System.Standards.ISO6393ToBCP47Registry.pas',
  Keyman.System.Standards.LCIDToBCP47Registry in '..\..\global\delphi\standards\Keyman.System.Standards.LCIDToBCP47Registry.pas',
  TempFileManager in '..\..\global\delphi\general\TempFileManager.pas',
  RedistFiles in '..\..\developer\TIKE\main\RedistFiles.pas',
  DebugPaths in '..\..\global\delphi\general\DebugPaths.pas',
  Upload_Settings in '..\..\global\delphi\general\Upload_Settings.pas',
  klog in '..\..\global\delphi\general\klog.pas',
  compile in '..\..\global\delphi\general\compile.pas',
  Keyman.Developer.System.Project.kmnProjectFile in '..\..\developer\TIKE\project\Keyman.Developer.System.Project.kmnProjectFile.pas',
  Keyman.Developer.System.Project.kmnProjectFileAction in '..\..\developer\TIKE\project\Keyman.Developer.System.Project.kmnProjectFileAction.pas',
  Keyman.Developer.System.Project.Project in '..\..\developer\TIKE\project\Keyman.Developer.System.Project.Project.pas',
  Keyman.Developer.System.Project.ProjectFile in '..\..\developer\TIKE\project\Keyman.Developer.System.Project.ProjectFile.pas',
  Keyman.Developer.System.Project.ProjectFiles in '..\..\developer\TIKE\project\Keyman.Developer.System.Project.ProjectFiles.pas',
  Keyman.Developer.System.Project.ProjectFileType in '..\..\developer\TIKE\project\Keyman.Developer.System.Project.ProjectFileType.pas',
  mrulist in '..\..\developer\TIKE\main\mrulist.pas',
  kmxfileconsts in '..\..\global\delphi\general\kmxfileconsts.pas',
  Keyman.Developer.System.Project.ProjectLoader in '..\..\developer\TIKE\project\Keyman.Developer.System.Project.ProjectLoader.pas',
  Keyman.Developer.System.Project.ProjectLog in '..\..\developer\TIKE\project\Keyman.Developer.System.Project.ProjectLog.pas',
  Keyman.Developer.System.Project.ProjectSaver in '..\..\developer\TIKE\project\Keyman.Developer.System.Project.ProjectSaver.pas',
  UKeymanTargets in '..\..\global\delphi\general\UKeymanTargets.pas',
  CompileKeymanWeb in '..\..\developer\TIKE\compile\CompileKeymanWeb.pas',
  CompileErrorCodes in '..\..\global\delphi\general\CompileErrorCodes.pas',
  KeyboardParser in '..\..\developer\TIKE\main\KeyboardParser.pas',
  WindowsLanguages in '..\..\global\delphi\general\WindowsLanguages.pas',
  KeymanWebKeyCodes in '..\..\developer\TIKE\compile\KeymanWebKeyCodes.pas',
  kmxfileutils in '..\..\global\delphi\general\kmxfileutils.pas',
  TikeUnicodeData in '..\..\developer\TIKE\main\TikeUnicodeData.pas',
  UnicodeData in '..\..\global\delphi\charmap\UnicodeData.pas',
  ADODB_TLB in '..\..\global\delphi\tlb\ADODB_TLB.pas',
  ADOX_TLB in '..\..\global\delphi\tlb\ADOX_TLB.pas',
  ttinfo in '..\..\global\delphi\general\ttinfo.pas',
  TouchLayoutDefinitions in '..\..\developer\TIKE\oskbuilder\TouchLayoutDefinitions.pas',
  TouchLayout in '..\..\developer\TIKE\oskbuilder\TouchLayout.pas',
  TouchLayoutUtils in '..\..\developer\TIKE\oskbuilder\TouchLayoutUtils.pas',
  KeyboardFonts in '..\..\global\delphi\general\KeyboardFonts.pas',
  Keyman.Developer.System.Project.kpsProjectFile in '..\..\developer\TIKE\project\Keyman.Developer.System.Project.kpsProjectFile.pas',
  Keyman.Developer.System.Project.kpsProjectFileAction in '..\..\developer\TIKE\project\Keyman.Developer.System.Project.kpsProjectFileAction.pas',
  CompilePackageInstaller in '..\..\global\delphi\general\CompilePackageInstaller.pas',
  utilhttp in '..\..\global\delphi\general\utilhttp.pas',
  UMD5Hash in '..\..\global\delphi\general\UMD5Hash.pas',
  Keyman.System.LanguageCodeUtils in '..\..\global\delphi\general\Keyman.System.LanguageCodeUtils.pas',
  Keyman.System.RegExGroupHelperRSP19902 in '..\..\global\delphi\general\Keyman.System.RegExGroupHelperRSP19902.pas',
  DUnitX.Loggers.TeamCity in '..\..\global\delphi\general\DUnitX.Loggers.TeamCity.pas',
  BCP47Tag in '..\..\global\delphi\general\BCP47Tag.pas',
  Keyman.System.Standards.BCP47SubtagRegistry in '..\..\global\delphi\standards\Keyman.System.Standards.BCP47SubtagRegistry.pas',
  Keyman.System.Standards.BCP47SuppressScriptRegistry in '..\..\global\delphi\standards\Keyman.System.Standards.BCP47SuppressScriptRegistry.pas',
  Keyman.System.Standards.NRSIAllTagsRegistry in '..\..\global\delphi\standards\Keyman.System.Standards.NRSIAllTagsRegistry.pas',
  Keyman.System.CanonicalLanguageCodeUtils in '..\..\global\delphi\general\Keyman.System.CanonicalLanguageCodeUtils.pas',
  TextFileFormat in '..\..\developer\TIKE\main\TextFileFormat.pas',
  Keyman.System.LexicalModelUtils in '..\..\global\delphi\lexicalmodels\Keyman.System.LexicalModelUtils.pas',
  Keyman.System.PackageInfoRefreshLexicalModels in '..\..\global\delphi\packages\Keyman.System.PackageInfoRefreshLexicalModels.pas',
  Keyman.System.Standards.LangTagsRegistry in '..\..\global\delphi\standards\Keyman.System.Standards.LangTagsRegistry.pas';

var
  runner : ITestRunner;
  results : IRunResults;
  logger : ITestLogger;
  nunitLogger : ITestLogger;
begin
{$IFDEF TESTINSIGHT}
  TestInsight.DUnitX.RunRegisteredTests;
  exit;
{$ENDIF}
  try
    //Check command line options, will exit if invalid
    TDUnitX.CheckCommandLine;
    //Create the test runner
    runner := TDUnitX.CreateRunner;
    //Tell the runner to use RTTI to find Fixtures
    runner.UseRTTI := True;
    //tell the runner how we will log things
    //Log to the console window
    logger := TDUnitXConsoleLogger.Create(true);
    runner.AddLogger(logger);
    //Generate an NUnit compatible XML File
    nunitLogger := TDUnitXXMLNUnitFileLogger.Create(TDUnitX.Options.XMLOutputFile);
    runner.AddLogger(nunitLogger);
    runner.FailsOnNoAsserts := False; //When true, Assertions must be made during tests;

    //Run tests
    results := runner.Execute;
    if not results.AllPassed then
      System.ExitCode := EXIT_ERRORS;

    {$IFNDEF CI}
    //We don't want this happening when running under CI.
    if TDUnitX.Options.ExitBehavior = TDUnitXExitBehavior.Pause then
    begin
      System.Write('Done.. press <Enter> key to quit.');
      System.Readln;
    end;
    {$ENDIF}

    DUnitX.Loggers.TeamCity.ReportToTeamCity;
  except
    on E: Exception do
      System.Writeln(E.ClassName, ': ', E.Message);
  end;
end.
