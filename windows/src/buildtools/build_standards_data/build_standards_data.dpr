program build_standards_data;

{$APPTYPE CONSOLE}

{$R *.res}

uses
  System.SysUtils,
  Keyman.System.BuildLanguageSubtagRegistry in 'Keyman.System.BuildLanguageSubtagRegistry.pas',
  Keyman.System.Standards.ISO6393ToBCP47Registry in '..\..\global\delphi\standards\Keyman.System.Standards.ISO6393ToBCP47Registry.pas',
  Keyman.System.BuildISO6393Registry in 'Keyman.System.BuildISO6393Registry.pas',
  Keyman.Console.BuildStandardsData in 'Keyman.Console.BuildStandardsData.pas';

begin
  try
    Run;
  except
    on E: Exception do
    begin
      Writeln(E.ClassName, ': ', E.Message);
      ExitCode := 1;
    end;
  end;
end.
