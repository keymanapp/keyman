unit Keyman.System.MapToSym;

interface

uses
  System.Classes,
  System.StrUtils,
  System.SysUtils,
  Winapi.Windows,

  JclDebug;

type
  TMapToSymFindFileProc = function(const UnitName: string): string;

function CreateSymFromMap(const exeFile, mapFile, symFile, codeId: string;
  isAmd64: Boolean; guid: TGUID; age: Integer; findFileProc: TMapToSymFindFileProc): Boolean;
function GuidToDebugId(guid: TGUID; age: Integer): string;

implementation

type
  TMapToSym = class(TJclMapScanner)
    function Write(const exeFile, symFile, codeId: string; isAmd64: Boolean; guid: TGUID; age: Integer): Boolean;
  private
    FFindFileProc: TMapToSymFindFileProc;
    function UnitIndexFromVA(va: DWORD): Integer;
    property FindFileProc: TMapToSymFindFileProc read FFindFileProc write FFindFileProc;
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

function TMapToSym.Write(const exeFile, symFile, codeId: string; isAmd64: Boolean; guid: TGUID; age: Integer): Boolean;
var
  s: TJclMapScannerCracker;
  r: TStringList;
  i: Integer;
  fileName, groupName, unitName: string;
  procCount, lineCount: Integer;
  procLen, lineIndex, lineLen: Integer;
  procName: string;
  FTextSegment, FITextSegment: Word;
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
    r.Add(Format('MODULE windows %s %s %s', [IfThen(isAmd64, 'x86_64', 'x86'), GuidToDebugId(guid, age), ExtractFileName(exeFile)]));
    r.Add(Format('INFO CODE_ID %s %s', [codeId, ExtractFileName(exeFile)]));

    FTextSegment := $FFFF;
    FITextSegment := $FFFF; // only for x86
    for i := 0 to High(s.FSegmentClasses) do
    begin
      if MapStringCacheToStr(s.FSegmentClasses[i].GroupName) = 'CODE' then
        FTextSegment := s.FSegmentClasses[i].Segment
      else if MapStringCacheToStr(s.FSegmentClasses[i].GroupName) = 'ICODE' then
        FITextSegment := s.FSegmentClasses[i].Segment;
    end;

    // Find unit names;
    for i := 0 to High(s.FSegments) do
    begin
      if s.FSegments[i].Segment = FTextSegment then
//        (s.FSegments[i].FProcNames[i].Segment <> FITextSegment) then Continue;

//      groupName := MapStringCacheToStr(s.FSegmentClasses[s.FSegments[i].Segment].GroupName);
//      if groupName = 'CODE' then
      begin
        unitName := MapStringCacheToStr(s.FSegments[i].UnitName);

        if Assigned(FFindFileProc)
          then fileName := FFindFileProc(unitName)
          else fileName := '';

        if fileName = '' then
          fileName := unitName + '.pas';

        r.Add(Format('FILE %d %s', [i+1, fileName])); // Note: occasional units may be .inc, .cpp
      end;
    end;

    // Write out functions and line numbers
    lineIndex := 0;
    for i := 0 to procCount - 1 do
    begin
      if (i > 0) and (s.FProcNames[i].VA < s.FProcNames[i-1].VA) then
        Break;

      if i < procCount - 1
        then procLen := s.FProcNames[i+1].VA - s.FProcNames[i].VA
        else procLen := 0; // TODO: look at module size for maximum here?

      if (s.FProcNames[i].Segment <> FTextSegment) and
        (s.FProcNames[i].Segment <> FITextSegment) then Continue;

      procName := MapStringCacheToStr(s.FProcNames[i].ProcName);
      r.Add(Format('FUNC %x %x 0 %s', [s.FProcNames[i].VA + $1000 {constant entry point}, procLen, procName]));

      if (lineIndex >= lineCount) or (s.FLineNumbers[lineIndex].VA >= s.FProcNames[i].VA + Cardinal(procLen)) then
      begin
        // No line numbers, let's add an artificial line, otherwise Sentry does
        // not use it for symbolication
        r.Add(Format('%x %x %d %d', [s.FProcNames[i].VA + $1000, procLen, 1, UnitIndexFromVA(s.FProcNames[i].VA)+1]));
      end;

      while (lineIndex < lineCount) and (s.FLineNumbers[lineIndex].VA < s.FProcNames[i].VA + Cardinal(procLen)) do
      begin
        if lineIndex < lineCount - 1
          then lineLen := s.FLineNumbers[lineIndex+1].VA - s.FLineNumbers[lineIndex].VA
          else lineLen := 0;  // TODO: look at module size for maximum here?

        // We never want the range to go outside the bounds of the function -- if there are no
        // line numbers for the next function, then we ensure we end at the function boundary
        if s.FLineNumbers[lineIndex].VA + Cardinal(lineLen) > s.FProcNames[i].VA + Cardinal(procLen) then
          lineLen := s.FProcNames[i].VA + Cardinal(procLen) - s.FLineNumbers[lineIndex].VA;

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

function CreateSymFromMap(const exeFile, mapFile, symFile, codeId: string; isAmd64: Boolean; guid: TGUID; age: Integer; findFileProc: TMapToSymFindFileProc): Boolean;
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
    p.FindFileProc := findFileProc;
    Result := p.Write(exeFile, symFile, codeId, isAmd64, guid, age);
  finally
    p.Free;
  end;
end;

end.
