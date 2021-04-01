program sentrytool;

{$APPTYPE CONSOLE}

{$R *.res}

uses
  System.SysUtils,
  Keyman.System.SentryTool.SentryToolMain in 'Keyman.System.SentryTool.SentryToolMain.pas',
  Keyman.System.MapToSym in 'Keyman.System.MapToSym.pas',
  Keyman.System.AddPdbToPe in 'Keyman.System.AddPdbToPe.pas',
  Keyman.System.SentryTool.DelphiSearchFile in 'Keyman.System.SentryTool.DelphiSearchFile.pas';

begin
  try
    ExitCode := main;
  except
    on E: Exception do
    begin
      Writeln(E.ClassName, ': ', E.Message);
      ExitCode := 1;
    end;
  end;
end.
