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
  Keyman.System.KMXFileLanguages in '..\..\..\common\delphi\keyboards\Keyman.System.KMXFileLanguages.pas',
  kmxfile in '..\..\..\..\..\common\windows\delphi\keyboards\kmxfile.pas',
  KeyNames in '..\..\..\..\..\common\windows\delphi\general\KeyNames.pas',
  utildir in '..\..\..\..\..\common\windows\delphi\general\utildir.pas',
  utilfiletypes in '..\..\..\..\..\common\windows\delphi\general\utilfiletypes.pas',
  KeymanVersion in '..\..\..\..\..\common\windows\delphi\general\KeymanVersion.pas',
  StockFileNames in '..\..\..\..\..\common\windows\delphi\general\StockFileNames.pas',
  utilstr in '..\..\..\..\..\common\windows\delphi\general\utilstr.pas',
  Unicode in '..\..\..\..\..\common\windows\delphi\general\Unicode.pas',
  Keyman.System.Standards.ISO6393ToBCP47Registry in '..\..\..\..\..\common\windows\delphi\standards\Keyman.System.Standards.ISO6393ToBCP47Registry.pas',
  Keyman.Test.System.KMXFileLanguagesTest in 'Keyman.Test.System.KMXFileLanguagesTest.pas',
  Keyman.System.Standards.LCIDToBCP47Registry in '..\..\..\..\..\common\windows\delphi\standards\Keyman.System.Standards.LCIDToBCP47Registry.pas',
  Keyman.System.LanguageCodeUtils in '..\..\..\..\..\common\windows\delphi\general\Keyman.System.LanguageCodeUtils.pas',
  DUnitX.Loggers.TeamCity in '..\..\..\..\..\common\windows\delphi\general\DUnitX.Loggers.TeamCity.pas',
  kmxfileconsts in '..\..\..\..\..\common\windows\delphi\keyboards\kmxfileconsts.pas',
  Keyman.System.Standards.BCP47SubtagRegistry in '..\..\..\..\..\common\windows\delphi\standards\Keyman.System.Standards.BCP47SubtagRegistry.pas',
  Keyman.System.Standards.BCP47SuppressScriptRegistry in '..\..\..\..\..\common\windows\delphi\standards\Keyman.System.Standards.BCP47SuppressScriptRegistry.pas';

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
