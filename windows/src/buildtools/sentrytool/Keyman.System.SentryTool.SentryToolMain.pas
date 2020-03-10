unit Keyman.System.SentryTool.SentryToolMain;

interface

function main: Integer;

implementation

uses
  System.Classes,
  System.SysUtils,
  Winapi.Windows,

  Keyman.System.AddPdbToPe,
  Keyman.System.MapToSym;

var
  FDebug: Boolean = FAlse;
  FInfile, FOutfile: string;

function ParseParams: Boolean;
var
  n: Integer;
begin
  n := 1;
  FDebug := ParamStr(n) = '-v';
  if FDebug then Inc(n);
  if ParamStr(n) <> 'delphiprep' then Exit(False);
  Inc(n);
  FInfile := ParamStr(n);
  Inc(n);
  FOutfile := ParamStr(n);
  if FOutfile = '' then
    FOutfile := FInfile;
  Result := True;
end;

procedure WriteUsage;
begin
  writeln('Usage: sentrytool [-v] delphiprep input.exe [output.exe]');
  writeln;
  writeln('delphiprep:');
  writeln('Adds a debug segment to Delphi compiled executable');
  writeln('input.exe, and generates an input.sym from the');
  writeln('corresponding input.map.');
  writeln;
  writeln('If the optional output.exe is specified, writes to');
  writeln('output.exe, output.sym respectively.');
  writeln;
  writeln(' -v   verbose (debug) output');
  writeln;
  writeln('NOTE: If file.exe contains debug symbols, this program');
  writeln('will fail: ensure that tdstrip has been called or ');
  writeln('debug symbols have written to a separate .tds at build');
  writeln('time.');
end;

function main: Integer;
var
  ms: TMemoryStream;
  guid: TGUID;
  codeId: string;
begin
  if not ParseParams then
  begin
    WriteUsage;
    Exit(2);
  end;

  AddPdbToPe_Debug := FDebug;

  ms := TMemoryStream.Create;
  try
    ms.LoadFromFile(FInfile);

    if not Succeeded(CreateGuid(guid)) then
    begin
      writeln('ERROR: Failed to create Guid');
      Exit(1);
    end;

    if not AddDebugId(ms, guid, codeId) then
      Exit(1);

    ms.SaveToFile(FOutfile);
  finally
    ms.Free;
  end;

  if not CreateSymFromMap(
      FOutfile,
      ChangeFileExt(FInfile, '.map'),
      ChangeFileExt(FOutfile, '.sym'),
      codeId, guid, 1) then
    Exit(1);

  writeln(Format('DebugID: %s', [GuidToDebugId(guid, 1)]));
  writeln(Format('CodeID: %s', [codeId]));

  Result := 0;
end;

end.
