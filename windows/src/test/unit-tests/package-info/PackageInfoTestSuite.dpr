program PackageInfoTestSuite;

{$IFNDEF TESTINSIGHT}
{$APPTYPE CONSOLE}
{$ENDIF}{$STRONGLINKTYPES ON}
uses
  System.SysUtils,
  {$IFDEF TESTINSIGHT}
  TestInsight.DUnitX,
  {$ENDIF }
  PackageInfo in '..\..\..\global\delphi\general\PackageInfo.pas',
  utilfiletypes in '..\..\..\global\delphi\general\utilfiletypes.pas',
  utilstr in '..\..\..\global\delphi\general\utilstr.pas',
  StockFileNames in '..\..\..\global\delphi\cust\StockFileNames.pas',
  Unicode in '..\..\..\global\delphi\general\Unicode.pas',
  KeymanVersion in '..\..\..\global\delphi\general\KeymanVersion.pas',
  VersionInfo in '..\..\..\global\delphi\general\VersionInfo.pas',
  utilsystem in '..\..\..\global\delphi\general\utilsystem.pas',
  utilexecute in '..\..\..\global\delphi\general\utilexecute.pas',
  utildir in '..\..\..\global\delphi\general\utildir.pas',
  RegistryKeys in '..\..\..\global\delphi\general\RegistryKeys.pas',
  GetOsVersion in '..\..\..\global\delphi\general\GetOsVersion.pas',
  kpsfile in '..\..\..\global\delphi\general\kpsfile.pas',
  PackageFileFormats in '..\..\..\global\delphi\general\PackageFileFormats.pas',
  DUnitX.Loggers.Console,
  DUnitX.Loggers.Xml.NUnit,
  DUnitX.TestFramework,
  PackageInfoTest in 'PackageInfoTest.pas',
  JsonUtil in '..\..\..\global\delphi\general\JsonUtil.pas',
  DUnitX.Loggers.TeamCity in '..\..\..\global\delphi\general\DUnitX.Loggers.TeamCity.pas';

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
