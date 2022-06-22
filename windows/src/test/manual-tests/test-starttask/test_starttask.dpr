program test_starttask;

{$APPTYPE CONSOLE}

{$R *.res}

uses
  System.SysUtils,
  Keyman.System.KeymanStartTask in '..\..\desktop\kmshell\util\Keyman.System.KeymanStartTask.pas',
  DebugPaths in '..\..\..\..\common\windows\delphi\general\DebugPaths.pas',
  RegistryKeys in '..\..\..\..\common\windows\delphi\general\RegistryKeys.pas',
  TaskScheduler_TLB in '..\..\global\delphi\winapi\TaskScheduler_TLB.pas',
  ErrorControlledRegistry in '..\..\..\..\common\windows\delphi\vcl\ErrorControlledRegistry.pas',
  KeymanVersion in '..\..\..\..\common\windows\delphi\general\KeymanVersion.pas',
  Keyman.System.Test.KeymanStartTaskTest in 'Keyman.System.Test.KeymanStartTaskTest.pas',
  KeymanPaths in '..\..\..\..\common\windows\delphi\general\KeymanPaths.pas';

begin
  try
    Run;
    { TODO -oUser -cConsole Main : Insert code here }
  except
    on E: Exception do
      Writeln(E.ClassName, ': ', E.Message);
  end;
end.
