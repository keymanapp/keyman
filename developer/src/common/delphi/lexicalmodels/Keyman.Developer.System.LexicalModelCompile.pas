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
  System.RegularExpressions,
  System.SysUtils,
  compile,
  utilexecute;

function CompileModelFile(ProjectFile: TProjectFile; infile, outfile: string; debug: Boolean): Boolean;
var
  s: TStringList;
  logtext, cmdline: string;
  ec: Integer;
  line: string;
  messageLine: TRegEx;
  m: TMatch;
  state: TProjectLogState;
  msgFilename: string;
  msgText: string;
  msgCode: Integer;
  msgLine: Integer;
  msgType: string;
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

  // Format of messages emitted from kmlmc, see errors.ts:printLogs:
  // '<file> (<line>): <type>: <errornum> <message>'
  messageLine := TRegEx.Create('^(?:(.+) \((\d+)\): )?(Hint|Warning|Error|Fatal Error): ([a-fA-F0-9]+) (.+)$');
  s := TStringList.Create;
  try
    s.Text := logtext;
    for line in s do
    begin
      m := messageLine.Match(line);
      if m.Success then
      begin
        msgFilename := m.Groups[1].Value;
        msgLine := StrToIntDef(m.Groups[2].Value, 0);
        msgType := m.Groups[3].Value;
        msgCode := StrToInt('$'+m.Groups[4].Value);
        msgText := m.Groups[5].Value;
        if msgType = 'Hint' then
          state := plsHint
        else if msgType = 'Warning' then
        begin
          state := plsWarning;
          if ProjectFile.Project.Options.CompilerWarningsAsErrors then
            Result := False;
        end
        else if msgType = 'Error' then
        begin
          state := plsError;
          Result := False;
        end
        else if msgType = 'Fatal Error' then
        begin
          state := plsFatal;
          Result := False;
        end
        else
          state := plsInfo;
        ProjectFile.Project.Log(state, msgFilename, msgText, msgCode, msgLine);
      end
      else
        ProjectFile.Project.Log(plsInfo, infile, line, 0, 0);
    end;
  finally
    s.Free;
  end;
end;

end.
