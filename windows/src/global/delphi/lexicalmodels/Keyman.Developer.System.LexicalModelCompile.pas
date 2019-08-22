unit Keyman.Developer.System.LexicalModelCompile;

interface

uses
  Keyman.Developer.System.Project.ProjectFile,
  Keyman.Developer.System.Project.ProjectLog;

function CompileModelFile(ProjectFile: TProjectFile; infile, outfile: string; debug: Boolean): Boolean;

implementation

uses
  System.Classes,
  System.SysUtils,
  utilexecute;

function CompileModelFile(ProjectFile: TProjectFile; infile, outfile: string; debug: Boolean): Boolean;
var
  s: TStringList;
  logtext, cmdline: string;
  ec: Integer;
  line: string;
begin
  ec := 0;
  logtext := '';
  cmdline := Format('"%skmlmc.cmd" "%s" -o "%s"', [ExtractFilePath(ParamStr(0)), infile, outfile]);
  Result := TUtilExecute.Console(cmdline, ExtractFileDir(infile), logtext, ec) or (ec <> 0);

  s := TStringList.Create;
  try
    s.Text := logtext;
    for line in s do
    begin
      // TODO: for errors and warnings
      ProjectFile.Project.Log(plsInfo, infile, line);
    end;
  finally
    s.Free;
  end;
end;

end.
