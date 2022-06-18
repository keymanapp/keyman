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
  CompilePackage in '..\..\..\..\..\windows\src\global\delphi\general\CompilePackage.pas',
  VisualKeyboard in '..\..\..\..\..\common\windows\delphi\visualkeyboard\VisualKeyboard.pas',
  kmpinffile in '..\..\..\..\..\windows\src\global\delphi\general\kmpinffile.pas',
  kpsfile in '..\..\..\..\..\windows\src\global\delphi\general\kpsfile.pas',
  PackageFileFormats in '..\..\..\..\..\windows\src\global\delphi\general\PackageFileFormats.pas',
  PackageInfo in '..\..\..\..\..\windows\src\global\delphi\general\PackageInfo.pas',
  utilfiletypes in '..\..\..\..\..\windows\src\global\delphi\general\utilfiletypes.pas',
  StockFileNames in '..\..\..\..\..\windows\src\global\delphi\cust\StockFileNames.pas',
  utilstr in '..\..\..\..\..\windows\src\global\delphi\general\utilstr.pas',
  Unicode in '..\..\..\..\..\windows\src\global\delphi\general\Unicode.pas',
  JsonUtil in '..\..\..\..\..\windows\src\global\delphi\general\JsonUtil.pas',
  KeymanVersion in '..\..\..\..\..\windows\src\global\delphi\general\KeymanVersion.pas',
  utildir in '..\..\..\..\..\windows\src\global\delphi\general\utildir.pas',
  utilsystem in '..\..\..\..\..\windows\src\global\delphi\general\utilsystem.pas',
  RegistryKeys in '..\..\..\..\..\windows\src\global\delphi\general\RegistryKeys.pas',
  utilexecute in '..\..\..\..\..\windows\src\global\delphi\general\utilexecute.pas',
  GetOsVersion in '..\..\..\..\..\windows\src\global\delphi\general\GetOsVersion.pas',
  VersionInfo in '..\..\..\..\..\windows\src\global\delphi\general\VersionInfo.pas',
  ExtShiftState in '..\..\..\..\..\common\windows\delphi\visualkeyboard\ExtShiftState.pas',
  VisualKeyboardLoaderBinary in '..\..\..\..\..\common\windows\delphi\visualkeyboard\VisualKeyboardLoaderBinary.pas',
  VisualKeyboardLoaderXML in '..\..\..\..\..\common\windows\delphi\visualkeyboard\VisualKeyboardLoaderXML.pas',
  VisualKeyboardSaverBinary in '..\..\..\..\..\common\windows\delphi\visualkeyboard\VisualKeyboardSaverBinary.pas',
  VisualKeyboardSaverXML in '..\..\..\..\..\common\windows\delphi\visualkeyboard\VisualKeyboardSaverXML.pas',
  VKeyChars in '..\..\..\..\..\windows\src\global\delphi\general\VKeyChars.pas',
  VKeys in '..\..\..\..\..\windows\src\global\delphi\general\VKeys.pas',
  ErrorControlledRegistry in '..\..\..\..\..\windows\src\global\delphi\vcl\ErrorControlledRegistry.pas',
  kmxfile in '..\..\..\..\..\windows\src\global\delphi\general\kmxfile.pas',
  CRC32 in '..\..\..\..\..\windows\src\global\delphi\general\CRC32.pas',
  KeyNames in '..\..\..\..\..\windows\src\global\delphi\general\KeyNames.pas',
  KeymanDeveloperOptions in '..\..\..\tike\main\KeymanDeveloperOptions.pas',
  Keyman.System.PackageInfoRefreshKeyboards in '..\..\..\common\delphi\packages\Keyman.System.PackageInfoRefreshKeyboards.pas',
  Keyman.System.KeyboardJSInfo in '..\..\..\common\delphi\keyboards\Keyman.System.KeyboardJSInfo.pas',
  Keyman.System.KeyboardUtils in '..\..\..\common\delphi\keyboards\Keyman.System.KeyboardUtils.pas',
  Keyman.System.KMXFileLanguages in '..\..\..\common\delphi\keyboards\Keyman.System.KMXFileLanguages.pas',
  Keyman.System.Standards.ISO6393ToBCP47Registry in '..\..\..\..\..\common\windows\delphi\standards\Keyman.System.Standards.ISO6393ToBCP47Registry.pas',
  Keyman.System.Standards.LCIDToBCP47Registry in '..\..\..\..\..\common\windows\delphi\standards\Keyman.System.Standards.LCIDToBCP47Registry.pas',
  TempFileManager in '..\..\..\..\..\windows\src\global\delphi\general\TempFileManager.pas',
  RedistFiles in '..\..\..\tike\main\RedistFiles.pas',
  DebugPaths in '..\..\..\..\..\windows\src\global\delphi\general\DebugPaths.pas',
  Upload_Settings in '..\..\..\..\..\windows\src\global\delphi\general\Upload_Settings.pas',
  klog in '..\..\..\..\..\windows\src\global\delphi\general\klog.pas',
  compile in '..\..\..\..\..\windows\src\global\delphi\general\compile.pas',
  Keyman.Developer.System.Project.kmnProjectFile in '..\..\..\tike\project\Keyman.Developer.System.Project.kmnProjectFile.pas',
  Keyman.Developer.System.Project.kmnProjectFileAction in '..\..\..\tike\project\Keyman.Developer.System.Project.kmnProjectFileAction.pas',
  Keyman.Developer.System.Project.Project in '..\..\..\tike\project\Keyman.Developer.System.Project.Project.pas',
  Keyman.Developer.System.Project.ProjectFile in '..\..\..\tike\project\Keyman.Developer.System.Project.ProjectFile.pas',
  Keyman.Developer.System.Project.ProjectFiles in '..\..\..\tike\project\Keyman.Developer.System.Project.ProjectFiles.pas',
  Keyman.Developer.System.Project.ProjectFileType in '..\..\..\tike\project\Keyman.Developer.System.Project.ProjectFileType.pas',
  mrulist in '..\..\..\tike\main\mrulist.pas',
  kmxfileconsts in '..\..\..\..\..\windows\src\global\delphi\general\kmxfileconsts.pas',
  Keyman.Developer.System.Project.ProjectLoader in '..\..\..\tike\project\Keyman.Developer.System.Project.ProjectLoader.pas',
  Keyman.Developer.System.Project.ProjectLog in '..\..\..\tike\project\Keyman.Developer.System.Project.ProjectLog.pas',
  Keyman.Developer.System.Project.ProjectSaver in '..\..\..\tike\project\Keyman.Developer.System.Project.ProjectSaver.pas',
  UKeymanTargets in '..\..\..\..\..\windows\src\global\delphi\general\UKeymanTargets.pas',
  CompileKeymanWeb in '..\..\..\tike\compile\CompileKeymanWeb.pas',
  CompileErrorCodes in '..\..\..\..\..\windows\src\global\delphi\general\CompileErrorCodes.pas',
  KeyboardParser in '..\..\..\tike\main\KeyboardParser.pas',
  WindowsLanguages in '..\..\..\..\..\windows\src\global\delphi\general\WindowsLanguages.pas',
  KeymanWebKeyCodes in '..\..\..\tike\compile\KeymanWebKeyCodes.pas',
  kmxfileutils in '..\..\..\..\..\windows\src\global\delphi\general\kmxfileutils.pas',
  TikeUnicodeData in '..\..\..\tike\main\TikeUnicodeData.pas',
  UnicodeData in '..\..\..\..\..\common\windows\delphi\charmap\UnicodeData.pas',
  ADODB_TLB in '..\..\..\..\..\common\windows\delphi\tlb\ADODB_TLB.pas',
  ADOX_TLB in '..\..\..\..\..\common\windows\delphi\tlb\ADOX_TLB.pas',
  ttinfo in '..\..\..\..\..\windows\src\global\delphi\general\ttinfo.pas',
  TouchLayoutDefinitions in '..\..\..\tike\oskbuilder\TouchLayoutDefinitions.pas',
  TouchLayout in '..\..\..\tike\oskbuilder\TouchLayout.pas',
  TouchLayoutUtils in '..\..\..\tike\oskbuilder\TouchLayoutUtils.pas',
  KeyboardFonts in '..\..\..\..\..\windows\src\global\delphi\general\KeyboardFonts.pas',
  Keyman.Developer.System.Project.kpsProjectFile in '..\..\..\tike\project\Keyman.Developer.System.Project.kpsProjectFile.pas',
  Keyman.Developer.System.Project.kpsProjectFileAction in '..\..\..\tike\project\Keyman.Developer.System.Project.kpsProjectFileAction.pas',
  CompilePackageInstaller in '..\..\..\..\..\windows\src\global\delphi\general\CompilePackageInstaller.pas',
  utilhttp in '..\..\..\..\..\windows\src\global\delphi\general\utilhttp.pas',
  Keyman.System.LanguageCodeUtils in '..\..\..\..\..\windows\src\global\delphi\general\Keyman.System.LanguageCodeUtils.pas',
  Keyman.System.RegExGroupHelperRSP19902 in '..\..\..\..\..\windows\src\global\delphi\general\Keyman.System.RegExGroupHelperRSP19902.pas',
  DUnitX.Loggers.TeamCity in '..\..\..\..\..\windows\src\global\delphi\general\DUnitX.Loggers.TeamCity.pas',
  BCP47Tag in '..\..\..\..\..\windows\src\global\delphi\general\BCP47Tag.pas',
  Keyman.System.Standards.BCP47SubtagRegistry in '..\..\..\..\..\common\windows\delphi\standards\Keyman.System.Standards.BCP47SubtagRegistry.pas',
  Keyman.System.Standards.BCP47SuppressScriptRegistry in '..\..\..\..\..\common\windows\delphi\standards\Keyman.System.Standards.BCP47SuppressScriptRegistry.pas',
  Keyman.System.CanonicalLanguageCodeUtils in '..\..\..\..\..\windows\src\global\delphi\general\Keyman.System.CanonicalLanguageCodeUtils.pas',
  TextFileFormat in '..\..\..\common\delphi\general\TextFileFormat.pas',
  Keyman.System.LexicalModelUtils in '..\..\..\common\delphi\lexicalmodels\Keyman.System.LexicalModelUtils.pas',
  Keyman.System.PackageInfoRefreshLexicalModels in '..\..\..\common\delphi\packages\Keyman.System.PackageInfoRefreshLexicalModels.pas',
  Keyman.System.Standards.LangTagsRegistry in '..\..\..\..\..\common\windows\delphi\standards\Keyman.System.Standards.LangTagsRegistry.pas',
  Keyman.Developer.System.Project.UrlRenderer in '..\..\..\tike\project\Keyman.Developer.System.Project.UrlRenderer.pas',
  KeymanPaths in '..\..\..\..\..\windows\src\global\delphi\general\KeymanPaths.pas',
  Keyman.Developer.System.ValidateKpsFile in '..\..\..\..\..\windows\src\global\delphi\general\Keyman.Developer.System.ValidateKpsFile.pas',
  Keyman.Developer.System.KeymanDeveloperPaths in '..\..\..\tike\main\Keyman.Developer.System.KeymanDeveloperPaths.pas';

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
