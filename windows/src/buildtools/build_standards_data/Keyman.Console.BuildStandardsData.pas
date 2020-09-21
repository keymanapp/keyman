unit Keyman.Console.BuildStandardsData;

interface

procedure Run;

implementation

uses
  Keyman.System.BuildISO6393Registry,
  Keyman.System.BuildLanguageSubtagRegistry,
  Keyman.System.BuildLCIDToBCP47Registry,
  Keyman.System.BuildNRSIAllTags,
  Keyman.System.BuildLangTags;

procedure Run;
begin
  if ParamCount < 3 then
  begin
    writeln('Usage: build_standards_data <mode> <infile> <outfile>');
    writeln('  Mode: iso6393|subtag|lcid');
    Halt(2);
  end;

  if ParamStr(1) = 'iso6393' then
  begin
    writeln('Building '+ParamStr(3));
    TBuildISO6393Registry.Build(ParamStr(2), ParamStr(3));
  end
  else if ParamStr(1) = 'subtag' then
  begin
    writeln('Building '+ParamStr(3));
    with TBuildLanguageSubtagRegistry.Create(ParamStr(2)) do
    try
      BuildSubtagRegistry(ParamStr(3));
    finally
      Free;
    end;
  end
  else if ParamStr(1) = 'suppress' then
  begin
    writeln('Building '+ParamStr(3));
    with TBuildLanguageSubtagRegistry.Create(ParamStr(2)) do
    try
      BuildSuppressScriptRegistry(ParamStr(3));
    finally
      Free;
    end;
  end
  else if ParamStr(1) = 'lcid' then
  begin
    writeln('Building '+ParamStr(3));
    TBuildLCIDToBCP47Registry.Build(ParamStr(2), ParamStr(3));
  end
  else if ParamStr(1) = 'alltags' then
  begin
    writeln('Building '+ParamStr(3));
    TBuildNRSIAllTags.Build(ParamStr(2), ParamStr(3));
  end
  else if ParamStr(1) = 'langtags' then
  begin
    writeln('Building '+ParamStr(3));
    TBuildLangTags.Build(ParamStr(2), ParamStr(3));
  end
  else
  begin
    writeln('Invalid parameter');
    Halt(3);
  end;
  ExitCode := 0;
end;

end.
