program test_klog;

{$APPTYPE CONSOLE}

uses
  SysUtils,
  klog in '..\..\..\..\..\common\windows\delphi\general\klog.pas',
  VersionInfo in '..\..\..\..\..\common\windows\delphi\general\VersionInfo.pas',
  ErrorControlledRegistry in '..\..\..\..\..\common\windows\delphi\vcl\ErrorControlledRegistry.pas',
  Unicode in '..\..\..\..\..\common\windows\delphi\general\Unicode.pas',
  DebugPaths in '..\..\..\..\..\common\windows\delphi\general\DebugPaths.pas',
  RegistryKeys in '..\..\..\..\..\common\windows\delphi\general\RegistryKeys.pas',
  KeymanVersion in '..\..\..\..\..\common\windows\delphi\general\KeymanVersion.pas',
  KeymanPaths in '..\..\..\..\..\common\windows\delphi\general\KeymanPaths.pas';

begin
  if KLEnabled then
  begin
    writeln('KLog is enabled - disable KLogging before release!');
    ExitCode := 0;
  end
  else
  begin
    writeln('KLog is disabled');
    ExitCode := 0;
  end;
end.
