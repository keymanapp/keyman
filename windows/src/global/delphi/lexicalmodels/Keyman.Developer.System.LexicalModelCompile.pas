unit Keyman.Developer.System.LexicalModelCompile;

interface

uses
  Keyman.Developer.System.Project.ProjectFile,
  Keyman.Developer.System.Project.ProjectLog,
  Keyman.Developer.System.KeymanDeveloperPaths;

function CompileModelFile(ProjectFile: TProjectFile; infile, outfile: string; debug: Boolean): Boolean;

implementation

uses
  System.Classes,
  System.SysUtils,
  compile,
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

  cmdline := Format('"%s" "%s" -o "%s"', [TKeymanDeveloperPaths.LexicalModelCompilerPath, infile, outfile]);
  Result := TUtilExecute.Console(cmdline, ExtractFileDir(infile), logtext, ec);

  logtext := UTF8ToString(AnsiString(logtext));

  if not Result then
  begin
    ProjectFile.Project.Log(plsError, infile,
      Format('Compiler failed to start with error %d: %s', [GetLastError, SysErrorMessage(GetLastError)]), CERR_ERROR, 0);
  end;

  Result := Result and (ec = 0);

  s := TStringList.Create;
  try
    s.Text := logtext;
    for line in s do
    begin
      // TODO: for errors and warnings
      ProjectFile.Project.Log(plsInfo, infile, line, 0, 0);
    end;
  finally
    s.Free;
  end;
end;

end.
