program build_standards_data;

{$APPTYPE CONSOLE}

{$R *.res}

uses
  System.SysUtils,
  Keyman.System.BuildLanguageSubtagRegistry in 'Keyman.System.BuildLanguageSubtagRegistry.pas',
  Keyman.System.BuildISO6393Registry in 'Keyman.System.BuildISO6393Registry.pas',
  Keyman.Console.BuildStandardsData in 'Keyman.Console.BuildStandardsData.pas',
  Keyman.System.BuildLCIDToBCP47Registry in 'Keyman.System.BuildLCIDToBCP47Registry.pas',
  Keyman.System.BuildLangTags in 'Keyman.System.BuildLangTags.pas',
  JsonUtil in '..\..\global\delphi\general\JsonUtil.pas';

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
