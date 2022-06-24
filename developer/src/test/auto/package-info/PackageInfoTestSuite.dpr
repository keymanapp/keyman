program PackageInfoTestSuite;

{$IFNDEF TESTINSIGHT}
{$APPTYPE CONSOLE}
{$ENDIF}{$STRONGLINKTYPES ON}
uses
  System.SysUtils,
  {$IFDEF TESTINSIGHT}
  TestInsight.DUnitX,
  {$ENDIF }
  PackageInfo in '..\..\..\..\..\common\windows\delphi\packages\PackageInfo.pas',
  utilfiletypes in '..\..\..\..\..\common\windows\delphi\general\utilfiletypes.pas',
  utilstr in '..\..\..\..\..\common\windows\delphi\general\utilstr.pas',
  StockFileNames in '..\..\..\..\..\common\windows\delphi\general\StockFileNames.pas',
  Unicode in '..\..\..\..\..\common\windows\delphi\general\Unicode.pas',
  KeymanVersion in '..\..\..\..\..\common\windows\delphi\general\KeymanVersion.pas',
  VersionInfo in '..\..\..\..\..\common\windows\delphi\general\VersionInfo.pas',
  utilsystem in '..\..\..\..\..\common\windows\delphi\general\utilsystem.pas',
  utilexecute in '..\..\..\..\..\common\windows\delphi\general\utilexecute.pas',
  utildir in '..\..\..\..\..\common\windows\delphi\general\utildir.pas',
  RegistryKeys in '..\..\..\..\..\common\windows\delphi\general\RegistryKeys.pas',
  GetOsVersion in '..\..\..\..\..\common\windows\delphi\general\GetOsVersion.pas',
  kpsfile in '..\..\..\..\..\developer\src\common\delphi\packages\kpsfile.pas',
  PackageFileFormats in '..\..\..\..\..\common\windows\delphi\packages\PackageFileFormats.pas',
  DUnitX.Loggers.Console,
  DUnitX.Loggers.Xml.NUnit,
  DUnitX.TestFramework,
  PackageInfoTest in 'PackageInfoTest.pas',
  JsonUtil in '..\..\..\..\..\common\windows\delphi\general\JsonUtil.pas',
  DUnitX.Loggers.TeamCity in '..\..\..\..\..\common\windows\delphi\general\DUnitX.Loggers.TeamCity.pas';

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
