program test_klog;

{$APPTYPE CONSOLE}

uses
  SysUtils,
  klog in '..\..\global\delphi\general\klog.pas',
  VersionInfo in '..\..\global\delphi\general\VersionInfo.pas',
  ErrorControlledRegistry in '..\..\global\delphi\vcl\ErrorControlledRegistry.pas',
  Unicode in '..\..\global\delphi\general\Unicode.pas';

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
