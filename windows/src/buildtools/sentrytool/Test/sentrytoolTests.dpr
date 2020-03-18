program sentrytoolTests;
{

  Delphi DUnit Test Project
  -------------------------
  This project contains the DUnit test framework and the GUI/Console test runners.
  Add "CONSOLE_TESTRUNNER" to the conditional defines entry in the project options
  to use the console test runner.  Otherwise the GUI test runner will be used by
  default.

}

{$IFDEF CONSOLE_TESTRUNNER}
{$APPTYPE CONSOLE}
{$ENDIF}

uses
  DUnitTestRunner,
  Keyman.Test.System.SentryTool.DelphiSearchFile in 'Keyman.Test.System.SentryTool.DelphiSearchFile.pas',
  Keyman.System.SentryTool.DelphiSearchFile in '..\Keyman.System.SentryTool.DelphiSearchFile.pas';

{$R *.RES}

begin
  DUnitTestRunner.RunRegisteredTests;
end.

