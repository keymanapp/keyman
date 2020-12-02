unit Keyman.System.SentryTool.SentryToolMain;

interface

function main: Integer;

implementation

uses
  System.Classes,
  System.SysUtils,
  Winapi.Windows,

  Keyman.System.AddPdbToPe,
  Keyman.System.MapToSym,
  Keyman.System.SentryTool.DelphiSearchFile;

var
  FDebug: Boolean = False;
  FInfile, FOutfile, FProjectFile, FRootPath, FIncludePath: string;
  FDelphiSearchFile: TDelphiSearchFile = nil;

function ParseParams: Boolean;
var
  n: Integer;
begin
  n := 1;
  FDebug := ParamStr(n) = '-v';
  if FDebug then Inc(n);
  if ParamStr(n) <> 'delphiprep' then Exit(False);
  Inc(n);

  while n <= ParamCount do
  begin
    if ParamStr(n) = '-dpr' then
      FProjectFile := ParamStr(n+1)
    else if ParamStr(n) = '-i' then
      FIncludePath := ParamStr(n+1)
    else if ParamStr(n) = '-o' then
      FOutFile := ParamStr(n+1)
    else if ParamStr(n) = '-r' then
      FRootPath := ParamStr(n+1)
    else
    begin
      if FInfile = '' then
      begin
        FInfile := ParamStr(n);
        Inc(n);
        Continue;
      end;
      Exit(False);
    end;
    Inc(n, 2);

  end;

  if FOutfile = '' then
    FOutfile := FInfile;

  Result := True;
end;

procedure WriteUsage;
begin
  writeln('Usage: sentrytool [-v] delphiprep input.exe [-r root] [-dpr input.dpr] [-i includepaths] [-o output.exe]');
  writeln;
  writeln('delphiprep:');
  writeln('  Adds a debug segment to Delphi compiled executable');
  writeln('  input.exe, and generates an input.sym from the');
  writeln('  corresponding input.map.');
  writeln;
  writeln('-o output.exe   If the optional output.exe is specified, writes to');
  writeln('                output.exe, output.sym respectively.');
  writeln('-dpr input.dpr  Path to corresponding project source in order to');
  writeln('                locate source file paths');
  writeln('-i includepaths List of search paths to check for source files');
  writeln('-r root         Relative root for output files');
  writeln;
  writeln('General parameters:');
  writeln;
  writeln(' -v   verbose (debug) output');
  writeln;
  writeln('NOTE: If file.exe contains debug symbols, this program');
  writeln('will fail: ensure that tdstrip has been called or ');
  writeln('debug symbols have written to a separate .tds at build');
  writeln('time.');
end;

function FindFilePath(const UnitName: string): string;
begin
  if SameText(UnitName + '.dpr', ExtractFileName(FProjectFile))
    then Result := ExpandFileName(FProjectFile)
    else Result := FDelphiSearchFile.FindFile(UnitName);

  if Result <> '' then
    Result := ExpandFileName(Result); // ExtractRelativePath(FRootPath, Result);
end;

function main: Integer;
var
  ms: TMemoryStream;
  guid: TGUID;
  codeId: string;
  isAmd64: Boolean;
begin
  if not ParseParams then
  begin
    WriteUsage;
    Exit(2);
  end;

  AddPdbToPe_Debug := FDebug;
  FRootPath := IncludeTrailingPathDelimiter(FRootPath);

  ms := TMemoryStream.Create;
  try
    ms.LoadFromFile(FInfile);

    if not Succeeded(CreateGuid(guid)) then
    begin
      writeln('ERROR: Failed to create Guid');
      Exit(1);
    end;

    if not AddDebugId(ms, guid, codeId, isAmd64) then
      Exit(1);

    ms.SaveToFile(FOutfile);
  finally
    ms.Free;
  end;

  FDelphiSearchFile := TDelphiSearchFile.Create(FProjectFile, FIncludePath);
  try
    if not CreateSymFromMap(
        FOutfile,
        ChangeFileExt(FInfile, '.map'),
        ChangeFileExt(FOutfile, '.sym'),
        codeId, isAmd64, guid, 1,
        FindFilePath) then
      Exit(1);
  finally
    FDelphiSearchFile.Free;
  end;

  writeln(Format('DebugID: %s', [GuidToDebugId(guid, 1)]));
  writeln(Format('CodeID: %s', [codeId]));

  Result := 0;
end;

end.
