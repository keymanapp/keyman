program WindowsSetupTest;

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
  Keyman.System.Test.InstallInfoTest in 'Keyman.System.Test.InstallInfoTest.pas',
  Keyman.Setup.System.InstallInfo in '..\..\..\desktop\setup\Keyman.Setup.System.InstallInfo.pas',
  PackageInfo in '..\..\..\..\..\common\windows\delphi\packages\PackageInfo.pas',
  utilfiletypes in '..\..\..\..\..\common\windows\delphi\general\utilfiletypes.pas',
  StockFileNames in '..\..\..\..\..\common\windows\delphi\general\StockFileNames.pas',
  utilstr in '..\..\..\..\..\common\windows\delphi\general\utilstr.pas',
  Unicode in '..\..\..\..\..\common\windows\delphi\general\Unicode.pas',
  JsonUtil in '..\..\..\..\..\common\windows\delphi\general\JsonUtil.pas',
  KeymanVersion in '..\..\..\..\..\common\windows\delphi\general\KeymanVersion.pas',
  utildir in '..\..\..\..\..\common\windows\delphi\general\utildir.pas',
  utilsystem in '..\..\..\..\..\common\windows\delphi\general\utilsystem.pas',
  RegistryKeys in '..\..\..\..\..\common\windows\delphi\general\RegistryKeys.pas',
  utilexecute in '..\..\..\..\..\common\windows\delphi\general\utilexecute.pas',
  GetOsVersion in '..\..\..\..\..\common\windows\delphi\general\GetOsVersion.pas',
  VersionInfo in '..\..\..\..\..\common\windows\delphi\general\VersionInfo.pas',
  SetupStrings in '..\..\..\desktop\setup\SetupStrings.pas',
  kmpinffile in '..\..\..\..\..\common\windows\delphi\packages\kmpinffile.pas',
  PackageFileFormats in '..\..\..\..\..\common\windows\delphi\packages\PackageFileFormats.pas',
  Keyman.Setup.System.SetupUILanguageManager in '..\..\..\desktop\setup\Keyman.Setup.System.SetupUILanguageManager.pas',
  Keyman.System.UILanguageManager in '..\..\..\global\delphi\general\Keyman.System.UILanguageManager.pas',
  Keyman.Setup.System.MsiUtils in '..\..\..\desktop\setup\Keyman.Setup.System.MsiUtils.pas';

var
  runner : ITestRunner;
  results : IRunResults;
  logger : ITestLogger;
  nunitLogger : ITestLogger;
begin

{$R *.res}

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
  except
    on E: Exception do
      System.Writeln(E.ClassName, ': ', E.Message);
  end;
end.
