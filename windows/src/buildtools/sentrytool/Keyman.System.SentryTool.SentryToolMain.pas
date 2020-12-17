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

  CmdParams: array of string;

// Note: this is lifted from System.pas, which doesn't
// make GetParamStr a public symbol, sadly.
function GetParamStr(P: PChar; var Param: string): PChar;
var
  i, Len: Integer;
  Start, S: PChar;
begin
  // U-OK
  while True do
  begin
    while (P[0] <> #0) and (P[0] <= ' ') do
      Inc(P);
    if (P[0] = '"') and (P[1] = '"') then Inc(P, 2) else Break;
  end;
  Len := 0;
  Start := P;
  while P[0] > ' ' do
  begin
    if P[0] = '"' then
    begin
      Inc(P);
      while (P[0] <> #0) and (P[0] <> '"') do
      begin
        Inc(Len);
        Inc(P);
      end;
      if P[0] <> #0 then
        Inc(P);
    end
    else
    begin
      Inc(Len);
      Inc(P);
    end;
  end;

  SetLength(Param, Len);

  P := Start;
  S := Pointer(Param);
  i := 0;
  while P[0] > ' ' do
  begin
    if P[0] = '"' then
    begin
      Inc(P);
      while (P[0] <> #0) and (P[0] <> '"') do
      begin
        S[i] := P^;
        Inc(P);
        Inc(i);
      end;
      if P[0] <> #0 then Inc(P);
    end
    else
    begin
      S[i] := P^;
      Inc(P);
      Inc(i);
    end;
  end;

  Result := P;
end;

function LoadParamsFromFile(const Filename: string): Boolean;
var
  p: PChar;
  cmd, s: string;
  str: TStringList;
begin
  cmd := '';
  str := TStringList.Create;
  try
    str.LoadFromFile(Filename);
    for s in str do
      cmd := cmd + Trim(s) + ' ';
  finally
    str.Free;
  end;
  p := PChar(cmd);
  while p <> #0 do
  begin
    p := GetParamStr(p, s);
    if s <> '' then
    begin
      SetLength(CmdParams, Length(CmdParams)+1);
      CmdParams[High(CmdParams)] := s;
    end;
  end;
  Result := True;
end;

function LoadParamsFromCmdline: Boolean;
var
  i: Integer;
begin
  SetLength(CmdParams, ParamCount);
  for i := 0 to ParamCount - 1 do
    CmdParams[i] := ParamStr(i+1);
  Result := True;
end;

function CmdParamStr(n: Integer): string;
begin
  Assert(n >= 0);
  if n = 0 then
    Result := ParamStr(0)
  else if n > Length(CmdParams)
    then Result := ''
    else Result := CmdParams[n-1];
end;

function CmdParamCount: Integer;
begin
  Result := Length(CmdParams);
end;

function ParseParams: Boolean;
var
  n: Integer;
begin
  if Copy(ParamStr(1), 1, 1) = '@' then
  begin
    if not LoadParamsFromFile(Copy(ParamStr(1), 2, MaxInt)) then Exit(False);
  end
  else
    if not LoadParamsFromCmdline then Exit(False);

  n := 1;
  FDebug := CmdParamStr(n) = '-v';
  if FDebug then Inc(n);
  if CmdParamStr(n) <> 'delphiprep' then Exit(False);
  Inc(n);

  while n <= CmdParamCount do
  begin
    if CmdParamStr(n) = '-dpr' then
      FProjectFile := CmdParamStr(n+1)
    else if CmdParamStr(n) = '-i' then
      FIncludePath := CmdParamStr(n+1)
    else if CmdParamStr(n) = '-o' then
      FOutFile := CmdParamStr(n+1)
    else if CmdParamStr(n) = '-r' then
      FRootPath := CmdParamStr(n+1)
    else
    begin
      if FInfile = '' then
      begin
        FInfile := CmdParamStr(n);
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
