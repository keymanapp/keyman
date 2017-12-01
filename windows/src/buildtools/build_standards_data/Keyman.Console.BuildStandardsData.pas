unit Keyman.Console.BuildStandardsData;

interface

procedure Run;

implementation

uses
  Keyman.System.BuildISO6393Registry,
  Keyman.System.BuildLanguageSubtagRegistry;

procedure Run;
begin
  if ParamCount < 4 then
  begin
    writeln('Usage: build_standards_data <iso6393.txt> <subtag-registry.txt>');
    writeln('   <Keyman.System.Standards.ISO6393ToBCP47Registry.pas-path>');
    writeln('   <Keyman.System.Standards.BCP47SuppressScriptRegistry.pas-path>');
    Halt(2);
  end;

  writeln('Building '+ParamStr(3));
  TBuildISO6393Registry.Build(ParamStr(1), ParamStr(3));

  writeln('Building '+ParamStr(4));
  with TBuildLanguageSubtagRegistry.Create(ParamStr(2)) do
  try
    BuildSuppressScriptRegistry(ParamStr(4));
  finally
    Free;
  end;

  ExitCode := 0;
end;

end.
