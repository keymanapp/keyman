unit Keyman.System.MapToSym;

interface

uses
  System.Classes,
  System.StrUtils,
  System.SysUtils,
  Winapi.Windows,

  JclDebug;

function CreateSymFromMap(const exeFile, mapFile, symFile, codeId: string; guid: TGUID; age: Integer): Boolean;
function GuidToDebugId(guid: TGUID; age: Integer): string;

implementation

type
  TMapToSym = class(TJclMapScanner)
    function Write(const exeFile, symFile, codeId: string; guid: TGUID; age: Integer): Boolean;
  private
    function UnitIndexFromVA(va: DWORD): Integer;
  end;

  // Crack access to TJclMapScanner private symbols
  TJclMapScannerCracker = class(TJclAbstractMapParser)
  private
    FSegmentClasses: array of TJclMapSegmentClass;
    FLineNumbers: array of TJclMapLineNumber;
    FProcNames: array of TJclMapProcName;
    FSegments: array of TJclMapSegment;
  end;

{ TMapToSym }

function GuidToDebugId(guid: TGUID; age: Integer): string;
begin
  Result := GuidToString(guid);
  Delete(Result, 1, 1);
  Delete(Result, 9, 1);
  Delete(Result, 13, 1);
  Delete(Result, 17, 1);
  Delete(Result, 21, 1);
  Delete(Result, 33, 1);
  Result := Result + IntToStr(age);
end;

function TMapToSym.UnitIndexFromVA(va: DWORD): Integer;
var
  s: TJclMapScannerCracker;
  i: Integer;
begin
  s := TJclMapScannerCracker(Self);
  for i := 0 to Length(s.FSegments) div 2 - 1 do
  begin
    if (s.FSegments[i].StartVA <= va) and (s.FSegments[i].EndVA > va) then Exit(i);
  end;
  Result := -1;
end;

function TMapToSym.Write(const exeFile, symFile, codeId: string; guid: TGUID; age: Integer): Boolean;
var
  s: TJclMapScannerCracker;
  r: TStringList;
  i: Integer;
  groupName, unitName: string;
  procCount, lineCount: Integer;
  procLen, lineIndex, lineLen: Integer;
  procName: string;
begin
  r := TStringList.Create;
  try
    s := TJclMapScannerCracker(Self); // gain access to private members
                                      // (these should have been protected, really)
    Self.Parse;

    // The funcs are listed twice; we only need the first reference
    procCount := Length(s.FProcNames) div 2 - 1;
    lineCount := Length(s.FLineNumbers) div 2 - 1;

    // Header information
    r.Add(Format('MODULE windows x86 %s %s', [GuidToDebugId(guid, age), ExtractFileName(exeFile)]));
    r.Add(Format('INFO CODE_ID %s %s', [codeId, ExtractFileName(exeFile)]));

    // Find unit names;
    for i := 0 to High(s.FSegments) do
    begin
      groupName := MapStringCacheToStr(s.FSegmentClasses[s.FSegments[i].Segment].GroupName);
      unitName := MapStringCacheToStr(s.FSegments[i].UnitName);
      if groupName = 'ICODE' then
        r.Add(Format('FILE %d %s.pas', [i+1, unitName])); // Note: occasional units may be .inc, .cpp
    end;

    // Write out functions and line numbers
    lineIndex := 0;
    for i := 0 to procCount - 1 do
    begin
      if (i > 0) and (s.FProcNames[i].VA < s.FProcNames[i-1].VA) then
        Break;

      if i < procCount - 1
        then procLen := s.FProcNames[i+1].VA - s.FProcNames[i].VA
        else procLen := 0;

      procName := MapStringCacheToStr(s.FProcNames[i].ProcName);
      r.Add(Format('FUNC %x %x 0 %s', [s.FProcNames[i].VA + $1000 {constant entry point}, procLen, procName]));

      while (lineIndex < lineCount) and (s.FLineNumbers[lineIndex].VA < s.FProcNames[i].VA + Cardinal(procLen)) do
      begin
        if lineIndex < lineCount - 1
          then lineLen := s.FLineNumbers[lineIndex+1].VA - s.FLineNumbers[lineIndex].VA
          else lineLen := 0;

        r.Add(Format('%x %x %d %d', [s.FLineNumbers[lineIndex].VA + $1000 {constant entry point},
          lineLen, s.FLineNumbers[lineIndex].LineNumber, UnitIndexFromVA(s.FLineNumbers[lineIndex].VA)+1]));
        Inc(lineIndex);
      end;
    end;

    r.SaveToFile(symFile);
  finally
    r.Free;
  end;

  Result := True;
end;

function CreateSymFromMap(const exeFile, mapFile, symFile, codeId: string; guid: TGUID; age: Integer): Boolean;
var
  p: TMapToSym;
begin
  if not FileExists(mapFile) then
  begin
    writeln('ERROR: file '+mapFile+' does not exist.');
    Exit(False);
  end;

  p := TMapToSym.Create(mapFile, 0);
  try
    Result := p.Write(exeFile, symFile, codeId, guid, age);
  finally
    p.Free;
  end;
end;

end.
