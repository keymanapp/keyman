program kmconverttest;

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
  Keyman.Developer.System.Test.KMConvertParametersTest in 'Keyman.Developer.System.Test.KMConvertParametersTest.pas',
  Keyman.Developer.System.KMConvertParameters in '..\..\kmconvert\Keyman.Developer.System.KMConvertParameters.pas',
  UKeymanTargets in '..\..\common\delphi\general\UKeymanTargets.pas',
  BCP47Tag in '..\..\..\..\common\windows\delphi\general\BCP47Tag.pas',
  Keyman.System.CanonicalLanguageCodeUtils in '..\..\..\..\common\windows\delphi\general\Keyman.System.CanonicalLanguageCodeUtils.pas',
  Keyman.System.LanguageCodeUtils in '..\..\..\..\common\windows\delphi\general\Keyman.System.LanguageCodeUtils.pas',
  Keyman.System.Standards.LangTagsRegistry in '..\..\..\..\common\windows\delphi\standards\Keyman.System.Standards.LangTagsRegistry.pas',
  Keyman.System.KeyboardUtils in '..\..\common\delphi\keyboards\Keyman.System.KeyboardUtils.pas',
  Keyman.System.LexicalModelUtils in '..\..\common\delphi\lexicalmodels\Keyman.System.LexicalModelUtils.pas',
  utilstr in '..\..\..\..\common\windows\delphi\general\utilstr.pas',
  Unicode in '..\..\..\..\common\windows\delphi\general\Unicode.pas',
  GetOsVersion in '..\..\..\..\common\windows\delphi\general\GetOsVersion.pas',
  Keyman.System.Standards.BCP47SubtagRegistry in '..\..\..\..\common\windows\delphi\standards\Keyman.System.Standards.BCP47SubtagRegistry.pas',
  Keyman.System.Standards.BCP47SuppressScriptRegistry in '..\..\..\..\common\windows\delphi\standards\Keyman.System.Standards.BCP47SuppressScriptRegistry.pas',
  Keyman.System.Standards.ISO6393ToBCP47Registry in '..\..\..\..\common\windows\delphi\standards\Keyman.System.Standards.ISO6393ToBCP47Registry.pas',
  Keyman.System.Standards.LCIDToBCP47Registry in '..\..\..\..\common\windows\delphi\standards\Keyman.System.Standards.LCIDToBCP47Registry.pas',
  utilfiletypes in '..\..\..\..\common\windows\delphi\general\utilfiletypes.pas',
  StockFileNames in '..\..\..\..\common\windows\delphi\general\StockFileNames.pas';

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
