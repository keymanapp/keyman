unit Keyman.Developer.System.KmcWrapper;

interface

uses
  Keyman.Developer.System.Project.ProjectFile;

type
  TKmcWrapper = class
  public
    function Compile(ProjectFile: TProjectFile; const infile, outfile: string; debug: Boolean): Boolean;
  end;

implementation

uses
  System.Classes,
  System.SysUtils,

  Keyman.Developer.System.Project.ProjectLog,
  Keyman.Developer.System.KeymanDeveloperPaths,
  compile,
  utilexecute;

{ TKmcWrapper }

function TKmcWrapper.Compile(
  ProjectFile: TProjectFile;
  const infile: string;
  const outfile: string;
  debug: Boolean
): Boolean;
var
  s: TStringList;
  logtext, cmdline: string;
  ec: Integer;
  line: string;
  messageLine: TArray<string>;
  state: TProjectLogState;
  msgFilename: string;
  msgText: string;
  msgCode: Integer;
  msgLine: Integer;
  msgType: string;
begin
  ec := 0;
  logtext := '';

  // TODO: log-level as opt?
  cmdline := Format('"%s" build --log-format tsv --log-level info "%s"', [TKeymanDeveloperPaths.KmcPath, infile]);
  if outfile <> '' then
    cmdline := cmdline + Format(' --out-file "%s"', [outfile]);
  if ProjectFile.Project.Options.CompilerWarningsAsErrors then
    cmdline := cmdline + ' --compiler-warnings-as-errors'
  else
    cmdline := cmdline + ' --no-compiler-warnings-as-errors';
  if not ProjectFile.Project.Options.WarnDeprecatedCode then
    cmdline := cmdline + ' --no-warn-deprecated-code';

  Result := TUtilExecute.Console(cmdline, ExtractFileDir(infile), logtext, ec);

  logtext := UTF8ToString(AnsiString(logtext));

  if not Result then
  begin
    ProjectFile.Project.Log(plsError, infile,
      Format('Compiler failed to start with error %d: %s', [GetLastError, SysErrorMessage(GetLastError)]), CERR_ERROR, 0);
  end;

  Result := Result and (ec = 0);

  // TSV Format of messages emitted from kmc, see NodeCompilerCallbacks.ts:printTsvMessage:
  // file line severity code message
  // (tab separated)
  s := TStringList.Create;
  try
    s.Text := logtext;
    for line in s do
    begin
      if line.Trim = '' then
        Continue;
      messageLine := line.Split([#9]);
      if Length(messageLine) = 5 then
      begin
        msgFilename := messageLine[0];
        msgLine := StrToIntDef(messageLine[1], 0);
        msgType := messageLine[2];
        msgCode := StrToInt('$'+messageLine[3].Substring(2)); // KM12345
        msgText := messageLine[4];

        if msgType = 'hint' then
          state := plsHint
        else if msgType = 'warn' then
        begin
          state := plsWarning;
          if ProjectFile.Project.Options.CompilerWarningsAsErrors then
            Result := False;
        end
        else if msgType = 'error' then
        begin
          state := plsError;
          Result := False;
        end
        else if msgType = 'fatal' then
        begin
          state := plsFatal;
          Result := False;
        end
        else // assume msgType = 'info'
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
