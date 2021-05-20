program KMXFileLanguagesTestSuite;

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
  Keyman.System.KMXFileLanguages in '..\..\..\global\delphi\keyboards\Keyman.System.KMXFileLanguages.pas',
  kmxfile in '..\..\..\global\delphi\general\kmxfile.pas',
  CRC32 in '..\..\..\global\delphi\general\CRC32.pas',
  KeyNames in '..\..\..\global\delphi\general\KeyNames.pas',
  utildir in '..\..\..\global\delphi\general\utildir.pas',
  utilfiletypes in '..\..\..\global\delphi\general\utilfiletypes.pas',
  OnlineConstants in '..\..\..\global\delphi\productactivation\OnlineConstants.pas',
  KeymanVersion in '..\..\..\global\delphi\general\KeymanVersion.pas',
  StockFileNames in '..\..\..\global\delphi\cust\StockFileNames.pas',
  utilstr in '..\..\..\global\delphi\general\utilstr.pas',
  Unicode in '..\..\..\global\delphi\general\Unicode.pas',
  Keyman.System.Standards.ISO6393ToBCP47Registry in '..\..\..\global\delphi\standards\Keyman.System.Standards.ISO6393ToBCP47Registry.pas',
  Keyman.Test.System.KMXFileLanguagesTest in 'Keyman.Test.System.KMXFileLanguagesTest.pas',
  Keyman.System.Standards.LCIDToBCP47Registry in '..\..\..\global\delphi\standards\Keyman.System.Standards.LCIDToBCP47Registry.pas',
  Keyman.System.LanguageCodeUtils in '..\..\..\global\delphi\general\Keyman.System.LanguageCodeUtils.pas',
  DUnitX.Loggers.TeamCity in '..\..\..\global\delphi\general\DUnitX.Loggers.TeamCity.pas',
  kmxfileconsts in '..\..\..\global\delphi\general\kmxfileconsts.pas',
  Keyman.System.Standards.BCP47SubtagRegistry in '..\..\..\global\delphi\standards\Keyman.System.Standards.BCP47SubtagRegistry.pas',
  Keyman.System.Standards.BCP47SuppressScriptRegistry in '..\..\..\global\delphi\standards\Keyman.System.Standards.BCP47SuppressScriptRegistry.pas';

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
