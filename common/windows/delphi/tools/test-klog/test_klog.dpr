program test_klog;

{$APPTYPE CONSOLE}

uses
  SysUtils,
  klog in '..\..\..\..\..\common\windows\delphi\general\klog.pas',
  VersionInfo in '..\..\..\..\..\common\windows\delphi\general\VersionInfo.pas',
  ErrorControlledRegistry in '..\..\..\..\..\common\windows\delphi\vcl\ErrorControlledRegistry.pas',
  Unicode in '..\..\..\..\..\common\windows\delphi\general\Unicode.pas';

begin
  if KLEnabled then
  begin
    writeln('KLog is enabled - disable KLogging before release!');
    ExitCode := 1;
  end
  else
  begin
    writeln('KLog is disabled');
    ExitCode := 0;
  end;
end.
