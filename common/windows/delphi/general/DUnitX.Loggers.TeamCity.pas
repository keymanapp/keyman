unit DUnitX.Loggers.TeamCity;

interface

procedure ReportToTeamCity;

implementation

uses
  System.SysUtils;

procedure ReportToTeamCity;
var
  KeymanRoot: string;
  ReportPath: string;
begin
  if GetEnvironmentVariable('TEAMCITY_VERSION') <> '' then
  begin
    KeymanRoot := ExcludeTrailingPathDelimiter(GetEnvironmentVariable('KEYMAN_ROOT'));
    ReportPath := ExtractRelativePath(KeymanRoot, ExtractFilePath(ParamStr(0)) + 'dunitx-results.xml');
    writeln('##teamcity[importData type=''nunit'' path='''+ReportPath+''']');
  end;
end;

end.
