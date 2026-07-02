unit DUnitX.Loggers.TeamCity;

interface

procedure ReportToTeamCity;

implementation

uses
  System.SysUtils;

procedure ReportToTeamCity;
var
  ReportPath: string;
begin
  if GetEnvironmentVariable('TEAMCITY_GIT_PATH') <> '' then
  begin
    ReportPath := ExtractFilePath(ParamStr(0)) + 'dunitx-results.xml';
    writeln('##teamcity[importData type=''nunit'' path='''+ReportPath+''']');
  end;
end;

end.
