program standardsdata;

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
  Keyman.Test.System.Standards.LangTagsRegistryTest in 'Keyman.Test.System.Standards.LangTagsRegistryTest.pas',
  Keyman.Test.System.CanonicalLanguageCodeUtilsTest in 'Keyman.Test.System.CanonicalLanguageCodeUtilsTest.pas',
  Keyman.System.CanonicalLanguageCodeUtils in '..\..\..\global\delphi\general\Keyman.System.CanonicalLanguageCodeUtils.pas',
  BCP47Tag in '..\..\..\global\delphi\general\BCP47Tag.pas',
  Keyman.System.LanguageCodeUtils in '..\..\..\global\delphi\general\Keyman.System.LanguageCodeUtils.pas',
  Keyman.System.Standards.BCP47SubtagRegistry in '..\..\..\global\delphi\standards\Keyman.System.Standards.BCP47SubtagRegistry.pas',
  Keyman.System.Standards.BCP47SuppressScriptRegistry in '..\..\..\global\delphi\standards\Keyman.System.Standards.BCP47SuppressScriptRegistry.pas',
  Keyman.System.Standards.ISO6393ToBCP47Registry in '..\..\..\global\delphi\standards\Keyman.System.Standards.ISO6393ToBCP47Registry.pas',
  Keyman.System.Standards.LangTagsRegistry in '..\..\..\global\delphi\standards\Keyman.System.Standards.LangTagsRegistry.pas',
  Keyman.System.Standards.LCIDToBCP47Registry in '..\..\..\global\delphi\standards\Keyman.System.Standards.LCIDToBCP47Registry.pas',
  utilstr in '..\..\..\global\delphi\general\utilstr.pas',
  Unicode in '..\..\..\global\delphi\general\Unicode.pas';

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
  except
    on E: Exception do
      System.Writeln(E.ClassName, ': ', E.Message);
  end;
end.
