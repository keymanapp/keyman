(*
 * Keyman is copyright (C) SIL Global. MIT License.
 *
 * Wrapper functions for the kmc command line tool
 *)
unit Keyman.Developer.System.KmcWrapper;

interface

uses
  Keyman.Developer.System.Project.ProjectFile,
  utilexecute;

type
  TKmcWrapper = class
  private
    function Run(const parameters: TArray<string>; const path, logFilename: string; Callback: TUtilExecuteCallbackEvent): Boolean;
  public
    function Compile(ProjectFile: TProjectFile; const infile, outfile: string; debug: Boolean; Callback: TUtilExecuteCallbackEvent = nil): Boolean;
    function Copy(const source, dest, cwd: string; relocateExternal: Boolean; Callback: TUtilExecuteCallbackEvent = nil): Boolean;
  end;

const
  // TODO: these are still used for messages raised from the IDE, perhaps need to be factored out
  CERR_ERROR   = $00004000;
  CERR_WARNING = $00002000;
  CERR_INFO    = $00000000;

implementation

uses
  System.Classes,
  System.SysUtils,

  Keyman.Developer.System.Project.Project,
  Keyman.Developer.System.Project.ProjectLog,
  Keyman.Developer.UI.Project.ProjectFileUI,
  Keyman.Developer.System.KeymanDeveloperPaths;

{ TKmcWrapper }

function TKmcWrapper.Compile(
  ProjectFile: TProjectFile;
  const infile: string;
  const outfile: string;
  debug: Boolean;
  Callback: TUtilExecuteCallbackEvent
): Boolean;
var
  cmdline: TArray<string>;
begin
  cmdline := [
    'build',
    '--log-format', 'tsv',
    '--log-level', 'info',
    infile
  ];

  if outfile <> '' then
    cmdline := cmdline + ['--out-file', outfile];

  if Assigned(FGlobalProject) and (FGlobalProject.Options.CompilerWarningsAsErrors) then
    cmdline := cmdline + ['--compiler-warnings-as-errors']
  else
    cmdline := cmdline + ['--no-compiler-warnings-as-errors'];

  if FGlobalProject.Options.WarnDeprecatedCode then
    cmdline := cmdline + ['--no-warn-deprecated-code'];

  if debug then
    cmdline := cmdline + ['--debug'];

  Result := Run(cmdline, ExtractFileDir(infile), infile, Callback);
end;

function TKmcWrapper.Copy(const source, dest, cwd: string;
  relocateExternal: Boolean;
  Callback: TUtilExecuteCallbackEvent): Boolean;
var
  destFilename, destPath: string;
  cmdline: TArray<string>;
begin
  if not dest.EndsWith('.kpj') then
    raise Exception.Create('Invalid destination "'+dest+'" for TKmcWrapper.copy: must be a filename ending in .kpj');
  destFilename := ChangeFileExt(ExtractFileName(dest), '');
  destPath := ExtractFileDir(dest);
  if ExtractFileName(destPath) <> destFilename then
    raise Exception.Create('Invalid destination "'+dest+'" for TKmcWrapper.copy: filename must match target folder name');

  cmdline := [
    'copy',
    '--out-path', destPath,
    '--log-format', 'tsv',
    '--log-level', 'info',
    source
  ];

  if relocateExternal then
    cmdline := cmdline + ['--relocate-external'];

  Result := Run(cmdline, cwd, dest, Callback);
end;

function TKmcWrapper.Run(
  const parameters: TArray<string>;
  const path, logFilename: string;
  Callback: TUtilExecuteCallbackEvent
): Boolean;
var
  param: string;
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
  // TODO: embed in server for better performance
  cmdline := '"'+TKeymanDeveloperPaths.KmcPath+'"';
  for param in parameters do
  begin
    cmdline := cmdline + ' "'+param+'"';
  end;

  // Log the precise command line used
  LogMessage(plsInfo, logFilename, cmdline, CERR_INFO, 0);

  Result := TUtilExecute.Console(cmdline, path, logtext, ec, Callback);

  logtext := UTF8ToString(AnsiString(logtext));

  if not Result then
  begin
    LogMessage(plsError, logFilename,
       Format('kmc failed to start with error %d: %s', [GetLastError, SysErrorMessage(GetLastError)]), CERR_ERROR, 0);
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
          if Assigned(FGlobalProject) and FGlobalProject.Options.CompilerWarningsAsErrors then
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
        else // assume msgType = 'info' (fine also for 'verbose', 'debug')
          state := plsInfo;
        LogMessage(state, msgFilename, msgText, msgCode, msgLine);
      end
      else
        LogMessage(plsInfo, logFilename, line, 0, 0);
    end;
  finally
    s.Free;
  end;
end;

end.
