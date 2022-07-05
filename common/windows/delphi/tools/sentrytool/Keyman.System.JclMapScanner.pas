//
// This is a lightly modified version of TJclMapScanner, removing all reference
// to loaded modules, as we don't use those for our map file conversion. All
// unrelated code is excluded so we do not need to depend on the full JCL.
//
{**************************************************************************************************}
{                                                                                                  }
{ Project JEDI Code Library (JCL)                                                                  }
{                                                                                                  }
{ The contents of this file are subject to the Mozilla Public License Version 1.1 (the "License"); }
{ you may not use this file except in compliance with the License. You may obtain a copy of the    }
{ License at http://www.mozilla.org/MPL/                                                           }
{                                                                                                  }
{ Software distributed under the License is distributed on an "AS IS" basis, WITHOUT WARRANTY OF   }
{ ANY KIND, either express or implied. See the License for the specific language governing rights  }
{ and limitations under the License.                                                               }
{                                                                                                  }
{ The Original Code is JclDebug.pas.                                                               }
{                                                                                                  }
{ The Initial Developers of the Original Code are Petr Vones and Marcel van Brakel.                }
{ Portions created by these individuals are Copyright (C) of these individuals.                    }
{ All Rights Reserved.                                                                             }
{                                                                                                  }
{ Contributor(s):                                                                                  }
{   Marcel van Brakel                                                                              }
{   Flier Lu (flier)                                                                               }
{   Florent Ouchet (outchy)                                                                        }
{   Robert Marquardt (marquardt)                                                                   }
{   Robert Rossmair (rrossmair)                                                                    }
{   Andreas Hausladen (ahuser)                                                                     }
{   Petr Vones (pvones)                                                                            }
{   Soeren Muehlbauer                                                                              }
{   Uwe Schuster (uschuster)                                                                       }
{                                                                                                  }
{**************************************************************************************************}
{                                                                                                  }
{ Various debugging support routines and classes. This includes: Diagnostics routines, Trace       }
{ routines, Stack tracing and Source Locations a la the C/C++ __FILE__ and __LINE__ macros.        }
{                                                                                                  }
{**************************************************************************************************}
{                                                                                                  }
{ Last modified: $Date::                                                                         $ }
{ Revision:      $Rev::                                                                          $ }
{ Author:        $Author::                                                                       $ }
{                                                                                                  }
{**************************************************************************************************}

unit Keyman.System.JclMapScanner;

interface

uses
  System.Classes,
  System.SysUtils,
  Winapi.Windows;

type
  TJclAddr32 = Cardinal;
  TJclAddr64 = UInt64;

  {$IFDEF _WIN64}
  TJclAddr = TJclAddr64;
  {$ELSE}
  TJclAddr = TJclAddr32;
  {$ENDIF}

  PJclMapAddress = ^TJclMapAddress;
  TJclMapAddress = packed record
    Segment: Word;
    Offset: TJclAddr;
  end;

  PJclMapString = PAnsiChar;

  TJclFileMappingStream = class(TCustomMemoryStream)
  private
    FFileHandle: THandle;
    FMapping: THandle;
  protected
    procedure Close;
  public
    constructor Create(const FileName: string; FileMode: Word = fmOpenRead or fmShareDenyWrite);
    destructor Destroy; override;
    function Write(const Buffer; Count: Longint): Longint; override;
  end;

  TJclAbstractMapParser = class(TObject)
  private
    FLinkerBug: Boolean;
    FLinkerBugUnitName: PJclMapString;
    FStream: TJclFileMappingStream;
    function GetLinkerBugUnitName: string;
  protected
(*    FModule: HMODULE;*)
    FLastUnitName: PJclMapString;
    FLastUnitFileName: PJclMapString;
    procedure ClassTableItem(const Address: TJclMapAddress; Len: Integer; SectionName, GroupName: PJclMapString); virtual; abstract;
    procedure SegmentItem(const Address: TJclMapAddress; Len: Integer; GroupName, UnitName: PJclMapString); virtual; abstract;
    procedure PublicsByNameItem(const Address: TJclMapAddress; Name: PJclMapString); virtual; abstract;
    procedure PublicsByValueItem(const Address: TJclMapAddress; Name: PJclMapString); virtual; abstract;
    procedure LineNumberUnitItem(UnitName, UnitFileName: PJclMapString); virtual; abstract;
    procedure LineNumbersItem(LineNumber: Integer; const Address: TJclMapAddress); virtual; abstract;
  public
    constructor Create(const MapFileName: TFileName; Module: HMODULE); overload; virtual;
    constructor Create(const MapFileName: TFileName); overload;
    destructor Destroy; override;
    procedure Parse;
    class function MapStringToFileName(MapString: PJclMapString): string;
    class function MapStringToModuleName(MapString: PJclMapString): string;
    class function MapStringToStr(MapString: PJclMapString; IgnoreSpaces: Boolean = False): string;
    property LinkerBug: Boolean read FLinkerBug;
    property LinkerBugUnitName: string read GetLinkerBugUnitName;
    property Stream: TJclFileMappingStream read FStream;
  end;

  TJclMapStringCache = record
    CachedValue: string;
    RawValue: PJclMapString;
  end;

  // MAP file scanner
  PJclMapSegmentClass = ^TJclMapSegmentClass;
  TJclMapSegmentClass = record
    Segment: Word; // segment ID
    Start: DWORD;  // start as in the map file
    Addr: DWORD;   // start as in process memory
    VA: DWORD;     // position relative to module base adress
    Len: DWORD;    // segment length
    SectionName: TJclMapStringCache;
    GroupName: TJclMapStringCache;
  end;

  PJclMapSegment = ^TJclMapSegment;
  TJclMapSegment = record
    Segment: Word;
    StartVA: DWORD; // VA relative to (module base address + $10000)
    EndVA: DWORD;
    UnitName: TJclMapStringCache;
  end;

  PJclMapProcName = ^TJclMapProcName;
  TJclMapProcName = record
    Segment: Word;
    VA: DWORD; // VA relative to (module base address + $10000)
    ProcName: TJclMapStringCache;
  end;

  PJclMapLineNumber = ^TJclMapLineNumber;
  TJclMapLineNumber = record
    Segment: Word;
    VA: DWORD; // VA relative to (module base address + $10000)
    LineNumber: Integer;
  end;

  TJclMapScanner = class(TJclAbstractMapParser)
  private
    FSegmentClasses: array of TJclMapSegmentClass;
    FLineNumbers: array of TJclMapLineNumber;
    FProcNames: array of TJclMapProcName;
    FSegments: array of TJclMapSegment;
    FSourceNames: array of TJclMapProcName;
    FLineNumbersCnt: Integer;
    FLineNumberErrors: Integer;
    FNewUnitFileName: PJclMapString;
    FProcNamesCnt: Integer;
    FSegmentCnt: Integer;
    FLastAccessedSegementIndex: Integer;
    function IndexOfSegment(Addr: DWORD): Integer;
  protected
    function MAPAddrToVA(const Addr: DWORD): DWORD;
    procedure ClassTableItem(const Address: TJclMapAddress; Len: Integer; SectionName, GroupName: PJclMapString); override;
    procedure SegmentItem(const Address: TJclMapAddress; Len: Integer; GroupName, UnitName: PJclMapString); override;
    procedure PublicsByNameItem(const Address: TJclMapAddress; Name: PJclMapString); override;
    procedure PublicsByValueItem(const Address: TJclMapAddress; Name: PJclMapString); override;
    procedure LineNumbersItem(LineNumber: Integer; const Address: TJclMapAddress); override;
    procedure LineNumberUnitItem(UnitName, UnitFileName: PJclMapString); override;
    procedure Scan;
  public
    constructor Create(const MapFileName: TFileName; Module: HMODULE); override;

    class function MapStringCacheToFileName(var MapString: TJclMapStringCache): string;
    class function MapStringCacheToModuleName(var MapString: TJclMapStringCache): string;
    class function MapStringCacheToStr(var MapString: TJclMapStringCache; IgnoreSpaces: Boolean = False): string;

    // Addr are virtual addresses relative to (module base address + $10000)
    function LineNumberFromAddr(Addr: DWORD): Integer; overload;
    function LineNumberFromAddr(Addr: DWORD; out Offset: Integer): Integer; overload;
    function ModuleNameFromAddr(Addr: DWORD): string;
    function ModuleStartFromAddr(Addr: DWORD): DWORD;
    function ProcNameFromAddr(Addr: DWORD): string; overload;
    function ProcNameFromAddr(Addr: DWORD; out Offset: Integer): string; overload;
    function SourceNameFromAddr(Addr: DWORD): string;
    property LineNumberErrors: Integer read FLineNumberErrors;
  end;

implementation

uses
  System.AnsiStrings,
  System.Character;

//=== { TJclFileMappingStream } ==============================================

constructor TJclFileMappingStream.Create(const FileName: string; FileMode: Word);
var
  Protect, Access, Size: DWORD;
  BaseAddress: Pointer;
begin
  inherited Create;
  FFileHandle := THandle(FileOpen(FileName, FileMode));
  if FFileHandle = INVALID_HANDLE_VALUE then
    RaiseLastOSError;
  if (FileMode and $0F) = fmOpenReadWrite then
  begin
    Protect := PAGE_WRITECOPY;
    Access := FILE_MAP_COPY;
  end
  else
  begin
    Protect := PAGE_READONLY;
    Access := FILE_MAP_READ;
  end;
  FMapping := CreateFileMapping(FFileHandle, nil, Protect, 0, 0, nil);
  if FMapping = 0 then
  begin
    Close;
    RaiseLastOSError;
  end;
  BaseAddress := MapViewOfFile(FMapping, Access, 0, 0, 0);
  if BaseAddress = nil then
  begin
    Close;
    RaiseLastOSError;
  end;
  Size := GetFileSize(FFileHandle, nil);
  if Size = DWORD(-1) then
  begin
    UnMapViewOfFile(BaseAddress);
    Close;
    RaiseLastOSError;
  end;
  SetPointer(BaseAddress, Size);
end;

destructor TJclFileMappingStream.Destroy;
begin
  Close;
  inherited Destroy;
end;

procedure TJclFileMappingStream.Close;
begin
  if Memory <> nil then
  begin
    UnMapViewOfFile(Memory);
    SetPointer(nil, 0);
  end;
  if FMapping <> 0 then
  begin
    CloseHandle(FMapping);
    FMapping := 0;
  end;
  if FFileHandle <> INVALID_HANDLE_VALUE then
  begin
    FileClose(FFileHandle);
    FFileHandle := INVALID_HANDLE_VALUE;
  end;
end;

function TJclFileMappingStream.Write(const Buffer; Count: Integer): Longint;
begin
  Result := 0;
  if (Size - Position) >= Count then
  begin
    System.Move(Buffer, Pointer(TJclAddr(Memory) + TJclAddr(Position))^, Count);
    Position := Position + Count;
    Result := Count;
  end;
end;

//=== { TJclAbstractMapParser } ==============================================

constructor TJclAbstractMapParser.Create(const MapFileName: TFileName; Module: HMODULE);
begin
  inherited Create;
(*  FModule := Module;*)
  if FileExists(MapFileName) then
    FStream := TJclFileMappingStream.Create(MapFileName, fmOpenRead or fmShareDenyWrite);
end;

constructor TJclAbstractMapParser.Create(const MapFileName: TFileName);
begin
  Create(MapFileName, 0);
end;

destructor TJclAbstractMapParser.Destroy;
begin
  FreeAndNil(FStream);
  inherited Destroy;
end;

function TJclAbstractMapParser.GetLinkerBugUnitName: string;
begin
  Result := MapStringToStr(FLinkerBugUnitName);
end;

class function TJclAbstractMapParser.MapStringToFileName(MapString: PJclMapString): string;
var
  PEnd: PJclMapString;
begin
  if MapString = nil then
  begin
    Result := '';
    Exit;
  end;
  PEnd := MapString;
  while (PEnd^ <> #0) and not (PEnd^ in ['=', #10, #13]) do
    Inc(PEnd);
  if (PEnd^ = '=') then
  begin
    while (PEnd >= MapString) and (PEnd^ <> ' ') do
      Dec(PEnd);
    while (PEnd >= MapString) and ((PEnd-1)^ = ' ') do
      Dec(PEnd);
  end;
  SetString(Result, MapString, PEnd - MapString);
end;

class function TJclAbstractMapParser.MapStringToModuleName(MapString: PJclMapString): string;
var
  PStart, PEnd, PExtension: PJclMapString;
begin
  if MapString = nil then
  begin
    Result := '';
    Exit;
  end;
  PEnd := MapString;
  while (PEnd^ <> #0) and not (PEnd^ in ['=', #10, #13]) do
    Inc(PEnd);
  if (PEnd^ = '=') then
  begin
    while (PEnd >= MapString) and (PEnd^ <> ' ') do
      Dec(PEnd);
    while (PEnd >= MapString) and ((PEnd-1)^ = ' ') do
      Dec(PEnd);
  end;
  PExtension := PEnd;
  while (PExtension >= MapString) and (PExtension^ <> '.') and (PExtension^ <> '|') do
    Dec(PExtension);
  if (System.AnsiStrings.StrIComp(PExtension, '.pas ') = 0) or
     (System.AnsiStrings.StrIComp(PExtension, '.obj ') = 0) then
    PEnd := PExtension;
  PExtension := PEnd;
  while (PExtension >= MapString) and (PExtension^ <> '|') and (PExtension^ <> '\') do
    Dec(PExtension);
  if PExtension >= MapString then
    PStart := PExtension + 1
  else
    PStart := MapString;
  SetString(Result, PStart, PEnd - PStart);
end;

class function TJclAbstractMapParser.MapStringToStr(MapString: PJclMapString;
  IgnoreSpaces: Boolean): string;
var
  P: PJclMapString;
begin
  if MapString = nil then
  begin
    Result := '';
    Exit;
  end;
  if MapString^ = '(' then
  begin
    Inc(MapString);
    P := MapString;
    while (P^ <> #0) and not (P^ in [')', #10, #13]) do
      Inc(P);
  end
  else
  begin
    P := MapString;
    if IgnoreSpaces then
      while (P^ <> #0) and not (P^ in ['(', #10, #13]) do
        Inc(P)
    else
      while (P^ <> #0) and (P^ <> '(') and (P^ > ' ') do
        Inc(P);
  end;
  SetString(Result, MapString, P - MapString);
end;

const
  NativeLineFeed       = Char(#10);
  NativeCarriageReturn = Char(#13);

function CharIsReturn(const C: Char): Boolean;
begin
  Result := (C = NativeLineFeed) or (C = NativeCarriageReturn);
end;

function CharIsDigit(const C: Char): Boolean;
begin
  Result := C.IsDigit;
end;

procedure TJclAbstractMapParser.Parse;
const
  TableHeader          : array [0..3] of string = ('Start', 'Length', 'Name', 'Class');
  SegmentsHeader       : array [0..3] of string = ('Detailed', 'map', 'of', 'segments');
  PublicsByNameHeader  : array [0..3] of string = ('Address', 'Publics', 'by', 'Name');
  PublicsByValueHeader : array [0..3] of string = ('Address', 'Publics', 'by', 'Value');
  LineNumbersPrefix    : string = 'Line numbers for';
var
  CurrPos, EndPos: PJclMapString;
{$IFNDEF COMPILER9_UP}
  PreviousA,
{$ENDIF COMPILER9_UP}
  A: TJclMapAddress;
  L: Integer;
  P1, P2: PJclMapString;

  function Eof: Boolean;
  begin
    Result := CurrPos >= EndPos;
  end;

  procedure SkipWhiteSpace;
  var
    LCurrPos, LEndPos: PJclMapString;
  begin
    LCurrPos := CurrPos;
    LEndPos := EndPos;
    while (LCurrPos < LEndPos) and (LCurrPos^ <= ' ') do
      Inc(LCurrPos);
    CurrPos := LCurrPos;
  end;

  procedure SkipEndLine;
  begin
    while not Eof and not CharIsReturn(Char(CurrPos^)) do
      Inc(CurrPos);
    SkipWhiteSpace;
  end;

  function IsDecDigit: Boolean;
  begin
    Result := CharIsDigit(Char(CurrPos^));
  end;

  function ReadTextLine: string;
  var
    P: PJclMapString;
  begin
    P := CurrPos;
    while (P^ <> #0) and not (P^ in [#10, #13]) do
      Inc(P);
    SetString(Result, CurrPos, P - CurrPos);
    CurrPos := P;
  end;


  function ReadDecValue: Integer;
  var
    P: PJclMapString;
  begin
    P := CurrPos;
    Result := 0;
    while P^ in ['0'..'9'] do
    begin
      Result := Result * 10 + (Ord(P^) - Ord('0'));
      Inc(P);
    end;
    CurrPos := P;
  end;

  function ReadHexValue: DWORD;
  var
    C: AnsiChar;
  begin
    Result := 0;
    repeat
      C := CurrPos^;
      case C of
        '0'..'9':
          Result := (Result shl 4) or DWORD(Ord(C) - Ord('0'));
        'A'..'F':
          Result := (Result shl 4) or DWORD(Ord(C) - Ord('A') + 10);
        'a'..'f':
          Result := (Result shl 4) or DWORD(Ord(C) - Ord('a') + 10);
        'H', 'h':
          begin
            Inc(CurrPos);
            Break;
          end;
      else
        Break;
      end;
      Inc(CurrPos);
    until False;
  end;

  function ReadAddress: TJclMapAddress;
  begin
    Result.Segment := ReadHexValue;
    if CurrPos^ = ':' then
    begin
      Inc(CurrPos);
      Result.Offset := ReadHexValue;
    end
    else
      Result.Offset := 0;
  end;

  function ReadString: PJclMapString;
  begin
    SkipWhiteSpace;
    Result := CurrPos;
    while {(CurrPos^ <> #0) and} (CurrPos^ > ' ') do
      Inc(CurrPos);
  end;

  procedure FindParam(Param: AnsiChar);
  begin
    while not ((CurrPos^ = Param) and ((CurrPos + 1)^ = '=')) do
      Inc(CurrPos);
    Inc(CurrPos, 2);
  end;

  function SyncToHeader(const Header: array of string): Boolean;
  var
    S: string;
    TokenIndex, OldPosition, CurrentPosition: Integer;
  begin
    Result := False;
    while not Eof do
    begin
      S := Trim(ReadTextLine);
      TokenIndex := Low(Header);
      CurrentPosition := 0;
      OldPosition := 0;
      while (TokenIndex <= High(Header)) do
      begin
        CurrentPosition := Pos(Header[TokenIndex],S);
        if (CurrentPosition <= OldPosition) then
        begin
          CurrentPosition := 0;
          Break;
        end;
        OldPosition := CurrentPosition;
        Inc(TokenIndex);
      end;
      Result := CurrentPosition <> 0;
      if Result then
        Break;
      SkipEndLine;
    end;
    if not Eof then
      SkipWhiteSpace;
  end;

  function SyncToPrefix(const Prefix: string): Boolean;
  var
    I: Integer;
    P: PJclMapString;
    S: string;
  begin
    if Eof then
    begin
      Result := False;
      Exit;
    end;
    SkipWhiteSpace;
    I := Length(Prefix);
    P := CurrPos;
    while not Eof and (P^ <> #13) and (P^ <> #0) and (I > 0) do
    begin
      Inc(P);
      Dec(I);
    end;
    SetString(S, CurrPos, Length(Prefix));
    Result := (S = Prefix);
    if Result then
      CurrPos := P;
    SkipWhiteSpace;
  end;

begin
  if FStream <> nil then
  begin
    FLinkerBug := False;
{$IFNDEF COMPILER9_UP}
    PreviousA.Segment := 0;
    PreviousA.Offset := 0;
{$ENDIF COMPILER9_UP}
    CurrPos := FStream.Memory;
    EndPos := CurrPos + FStream.Size;
    if SyncToHeader(TableHeader) then
      while IsDecDigit do
      begin
        A := ReadAddress;
        SkipWhiteSpace;
        L := ReadHexValue;
        P1 := ReadString;
        P2 := ReadString;
        SkipEndLine;
        ClassTableItem(A, L, P1, P2);
      end;
    if SyncToHeader(SegmentsHeader) then
      while IsDecDigit do
      begin
        A := ReadAddress;
        SkipWhiteSpace;
        L := ReadHexValue;
        FindParam('C');
        P1 := ReadString;
        FindParam('M');
        P2 := ReadString;
        SkipEndLine;
        SegmentItem(A, L, P1, P2);
      end;
    if SyncToHeader(PublicsByNameHeader) then
      while IsDecDigit do
      begin
        A := ReadAddress;
        P1 := ReadString;
        SkipEndLine; // compatibility with C++Builder MAP files
        PublicsByNameItem(A, P1);
      end;
    if SyncToHeader(PublicsByValueHeader) then
      while not Eof and IsDecDigit do
      begin
        A := ReadAddress;
        P1 := ReadString;
        SkipEndLine; // compatibility with C++Builder MAP files
        PublicsByValueItem(A, P1);
      end;
    while SyncToPrefix(LineNumbersPrefix) do
    begin
      FLastUnitName := CurrPos;
      FLastUnitFileName := CurrPos;
      while FLastUnitFileName^ <> '(' do
        Inc(FLastUnitFileName);
      SkipEndLine;
      LineNumberUnitItem(FLastUnitName, FLastUnitFileName);
      repeat
        SkipWhiteSpace;
        L := ReadDecValue;
        SkipWhiteSpace;
        A := ReadAddress;
        SkipWhiteSpace;
        LineNumbersItem(L, A);
{$IFNDEF COMPILER9_UP}
        if (not FLinkerBug) and (A.Offset < PreviousA.Offset) then
        begin
          FLinkerBugUnitName := FLastUnitName;
          FLinkerBug := True;
        end;
        PreviousA := A;
{$ENDIF COMPILER9_UP}
      until not IsDecDigit;
    end;
  end;
end;

//=== { TJclMapScanner } =====================================================

constructor TJclMapScanner.Create(const MapFileName: TFileName; Module: HMODULE);
begin
  inherited Create(MapFileName, Module);
  Scan;
end;

function TJclMapScanner.MAPAddrToVA(const Addr: DWORD): DWORD;
begin
  // MAP file format was changed in Delphi 2005
  // before Delphi 2005: segments started at offset 0
  //                     only one segment of code
  // after Delphi 2005: segments started at code base address (module base address + $10000)
  //                    2 segments of code
  if (Length(FSegmentClasses) > 0) and (FSegmentClasses[0].Start > 0) and (Addr >= FSegmentClasses[0].Start) then
    // Delphi 2005 and later
    // The first segment should be code starting at module base address + $10000
    Result := Addr - FSegmentClasses[0].Start
  else
    // before Delphi 2005
    Result := Addr;
end;

class function TJclMapScanner.MapStringCacheToFileName(
  var MapString: TJclMapStringCache): string;
begin
  Result := MapString.CachedValue;
  if Result = '' then
  begin
    Result := MapStringToFileName(MapString.RawValue);
    MapString.CachedValue := Result;
  end;
end;

class function TJclMapScanner.MapStringCacheToModuleName(
  var MapString: TJclMapStringCache): string;
begin
  Result := MapString.CachedValue;
  if Result = '' then
  begin
    Result := MapStringToModuleName(MapString.RawValue);
    MapString.CachedValue := Result;
  end;
end;

class function TJclMapScanner.MapStringCacheToStr(var MapString: TJclMapStringCache;
  IgnoreSpaces: Boolean): string;
begin
  Result := MapString.CachedValue;
  if Result = '' then
  begin
    Result := MapStringToStr(MapString.RawValue, IgnoreSpaces);
    MapString.CachedValue := Result;
  end;
end;

procedure TJclMapScanner.ClassTableItem(const Address: TJclMapAddress; Len: Integer;
  SectionName, GroupName: PJclMapString);
var
  C: Integer;
(*  SectionHeader: PImageSectionHeader; *)
begin
  C := Length(FSegmentClasses);
  SetLength(FSegmentClasses, C + 1);
  FSegmentClasses[C].Segment := Address.Segment;
  FSegmentClasses[C].Start := Address.Offset;
  FSegmentClasses[C].Addr := Address.Offset; // will be fixed below while considering module mapped address
  // test GroupName because SectionName = '.tls' in Delphi and '_tls' in BCB
  if System.AnsiStrings.StrIComp(GroupName, 'TLS') = 0 then
    FSegmentClasses[C].VA := FSegmentClasses[C].Start
  else
    FSegmentClasses[C].VA := MAPAddrToVA(FSegmentClasses[C].Start);
  FSegmentClasses[C].Len := Len;
  FSegmentClasses[C].SectionName.RawValue := SectionName;
  FSegmentClasses[C].GroupName.RawValue := GroupName;
(*
  if FModule <> 0 then
  begin
    { Fix the section addresses }
    SectionHeader := PeMapImgFindSectionFromModule(Pointer(FModule), MapStringToStr(SectionName));
    if SectionHeader = nil then
      { before Delphi 2005 the class names where used for the section names }
      SectionHeader := PeMapImgFindSectionFromModule(Pointer(FModule), MapStringToStr(GroupName));

    if SectionHeader <> nil then
    begin
      FSegmentClasses[C].Addr := TJclAddr(FModule) + SectionHeader.VirtualAddress;
      FSegmentClasses[C].VA := SectionHeader.VirtualAddress;
    end;
  end;
*)
end;

function TJclMapScanner.LineNumberFromAddr(Addr: DWORD): Integer;
var
  Dummy: Integer;
begin
  Result := LineNumberFromAddr(Addr, Dummy);
end;

function Search_MapLineNumber(Item1, Item2: Pointer): Integer;
begin
  Result := Integer(PJclMapLineNumber(Item1)^.VA) - PInteger(Item2)^;
end;

// Dynamic array sort and search routines
type
  TDynArraySortCompare = function (Item1, Item2: Pointer): Integer;
  SizeInt = Integer;
  PSizeInt = ^SizeInt;

function SearchDynArray(const ArrayPtr: Pointer; ElementSize: Cardinal; SortFunc: TDynArraySortCompare;
  ValuePtr: Pointer; Nearest: Boolean): SizeInt;
var
  L, H, I, C: SizeInt;
  B: Boolean;
begin
  Result := -1;
  if ArrayPtr <> nil then
  begin
    L := 0;
    H := PSizeInt(TJclAddr(ArrayPtr) - SizeOf(SizeInt))^ - 1;
    B := False;
    while L <= H do
    begin
      I := (L + H) shr 1;
      C := SortFunc(Pointer(TJclAddr(ArrayPtr) + TJclAddr(I * SizeInt(ElementSize))), ValuePtr);
      if C < 0 then
        L := I + 1
      else
      begin
        H := I - 1;
        if C = 0 then
        begin
          B := True;
          L := I;
        end;
      end;
    end;
    if B then
      Result := L
    else
    if Nearest and (H >= 0) then
      Result := H;
  end;
end;

function TJclMapScanner.LineNumberFromAddr(Addr: DWORD; out Offset: Integer): Integer;
var
  I: Integer;
  ModuleStartAddr: DWORD;
begin
  ModuleStartAddr := ModuleStartFromAddr(Addr);
  Result := 0;
  Offset := 0;
  I := SearchDynArray(FLineNumbers, SizeOf(FLineNumbers[0]), Search_MapLineNumber, @Addr, True);
  if (I <> -1) and (FLineNumbers[I].VA >= ModuleStartAddr) then
  begin
    Result := FLineNumbers[I].LineNumber;
    Offset := Addr - FLineNumbers[I].VA;
  end;
end;

procedure TJclMapScanner.LineNumbersItem(LineNumber: Integer; const Address: TJclMapAddress);
var
  SegIndex, C: Integer;
  VA: DWORD;
  Added: Boolean;
begin
  Added := False;
  for SegIndex := Low(FSegmentClasses) to High(FSegmentClasses) do
    if (FSegmentClasses[SegIndex].Segment = Address.Segment)
      and (DWORD(Address.Offset) < FSegmentClasses[SegIndex].Len) then
  begin
    if System.AnsiStrings.StrIComp(FSegmentClasses[SegIndex].GroupName.RawValue, 'TLS') = 0 then
      Va := Address.Offset
    else
      VA := MAPAddrToVA(Address.Offset + FSegmentClasses[SegIndex].Start);
    { Starting with Delphi 2005, "empty" units are listes with the last line and
      the VA 0001:00000000. When we would accept 0 VAs here, System.pas functions
      could be mapped to other units and line numbers. Discaring such items should
      have no impact on the correct information, because there can't be a function
      that starts at VA 0. }
    if VA = 0 then
      Continue;
    if FLineNumbersCnt = Length(FLineNumbers)  then
    begin
      if FLineNumbersCnt < 512 then
        SetLength(FLineNumbers, FLineNumbersCnt + 512)
      else
        SetLength(FLineNumbers, FLineNumbersCnt * 2);
    end;
    FLineNumbers[FLineNumbersCnt].Segment := FSegmentClasses[SegIndex].Segment;
    FLineNumbers[FLineNumbersCnt].VA := VA;
    FLineNumbers[FLineNumbersCnt].LineNumber := LineNumber;
    Inc(FLineNumbersCnt);
    Added := True;
    if FNewUnitFileName <> nil then
    begin
      C := Length(FSourceNames);
      SetLength(FSourceNames, C + 1);
      FSourceNames[C].Segment := FSegmentClasses[SegIndex].Segment;
      FSourceNames[C].VA := VA;
      FSourceNames[C].ProcName.RawValue := FNewUnitFileName;
      FNewUnitFileName := nil;
    end;
    Break;
  end;
  if not Added then
    Inc(FLineNumberErrors);
end;

procedure TJclMapScanner.LineNumberUnitItem(UnitName, UnitFileName: PJclMapString);
begin
  FNewUnitFileName := UnitFileName;
end;

function TJclMapScanner.IndexOfSegment(Addr: DWORD): Integer;
var
  L, R: Integer;
  S: PJclMapSegment;
begin
  R := Length(FSegments) - 1;
  Result := FLastAccessedSegementIndex;
  if Result <= R then
  begin
    S := @FSegments[Result];
    if (S.StartVA <= Addr) and (Addr < S.EndVA) then
      Exit;
  end;

  // binary search
  L := 0;
  while L <= R do
  begin
    Result := L + (R - L) div 2;
    S := @FSegments[Result];
    if Addr >= S.EndVA then
      L := Result + 1
    else
    begin
      R := Result - 1;
      if (S.StartVA <= Addr) and (Addr < S.EndVA) then
      begin
        FLastAccessedSegementIndex := Result;
        Exit;
      end;
    end;
  end;
  Result := -1;
end;

function TJclMapScanner.ModuleNameFromAddr(Addr: DWORD): string;
var
  I: Integer;
begin
  I := IndexOfSegment(Addr);
  if I <> -1 then
    Result := MapStringCacheToModuleName(FSegments[I].UnitName)
  else
    Result := '';
end;

function TJclMapScanner.ModuleStartFromAddr(Addr: DWORD): DWORD;
var
  I: Integer;
begin
  I := IndexOfSegment(Addr);
  Result := DWORD(-1);
  if I <> -1 then
    Result := FSegments[I].StartVA;
end;

function TJclMapScanner.ProcNameFromAddr(Addr: DWORD): string;
var
  Dummy: Integer;
begin
  Result := ProcNameFromAddr(Addr, Dummy);
end;

function Search_MapProcName(Item1, Item2: Pointer): Integer;
begin
  Result := Integer(PJclMapProcName(Item1)^.VA) - PInteger(Item2)^;
end;

function TJclMapScanner.ProcNameFromAddr(Addr: DWORD; out Offset: Integer): string;
var
  I: Integer;
  ModuleStartAddr: DWORD;
begin
  ModuleStartAddr := ModuleStartFromAddr(Addr);
  Result := '';
  Offset := 0;
  I := SearchDynArray(FProcNames, SizeOf(FProcNames[0]), Search_MapProcName, @Addr, True);
  if (I <> -1) and (FProcNames[I].VA >= ModuleStartAddr) then
  begin
    Result := MapStringCacheToStr(FProcNames[I].ProcName, True);
    Offset := Addr - FProcNames[I].VA;
  end;
end;

procedure TJclMapScanner.PublicsByNameItem(const Address: TJclMapAddress;  Name: PJclMapString);
begin
  { TODO : What to do? }
end;

procedure TJclMapScanner.PublicsByValueItem(const Address: TJclMapAddress; Name: PJclMapString);
var
  SegIndex: Integer;
begin
  for SegIndex := Low(FSegmentClasses) to High(FSegmentClasses) do
    if (FSegmentClasses[SegIndex].Segment = Address.Segment)
      and (DWORD(Address.Offset) < FSegmentClasses[SegIndex].Len) then
  begin
    if FProcNamesCnt = Length(FProcNames)  then
    begin
      if FProcNamesCnt < 512 then
        SetLength(FProcNames, FProcNamesCnt + 512)
      else
        SetLength(FProcNames, FProcNamesCnt * 2);
    end;
    FProcNames[FProcNamesCnt].Segment := FSegmentClasses[SegIndex].Segment;
    if System.AnsiStrings.StrIComp(FSegmentClasses[SegIndex].GroupName.RawValue, 'TLS') = 0 then
      FProcNames[FProcNamesCnt].VA := Address.Offset
    else
      FProcNames[FProcNamesCnt].VA := MAPAddrToVA(Address.Offset + FSegmentClasses[SegIndex].Start);
    FProcNames[FProcNamesCnt].ProcName.RawValue := Name;
    Inc(FProcNamesCnt);
    Break;
  end;
end;

function Sort_MapLineNumber(Item1, Item2: Pointer): Integer;
begin
  Result := Integer(PJclMapLineNumber(Item1)^.VA) - Integer(PJclMapLineNumber(Item2)^.VA);
end;

function Sort_MapProcName(Item1, Item2: Pointer): Integer;
begin
  Result := Integer(PJclMapProcName(Item1)^.VA) - Integer(PJclMapProcName(Item2)^.VA);
end;

function Sort_MapSegment(Item1, Item2: Pointer): Integer;
begin
  Result := Integer(PJclMapSegment(Item1)^.StartVA) - Integer(PJclMapSegment(Item2)^.StartVA);
end;

type
  TDynByteArray          = array of Byte;

procedure SortDynArray(const ArrayPtr: Pointer; ElementSize: Cardinal; SortFunc: TDynArraySortCompare);
var
  TempBuf: TDynByteArray;

  function ArrayItemPointer(Item: SizeInt): Pointer;
  begin
    Assert(Item >= 0);
    Result := Pointer(TJclAddr(ArrayPtr) + TJclAddr(Item * SizeInt(ElementSize)));
  end;

  procedure QuickSort(L, R: SizeInt);
  var
    I, J, T: SizeInt;
    P, IPtr, JPtr: Pointer;
    ElSize: Integer;
  begin
    ElSize := ElementSize;
    repeat
      I := L;
      J := R;
      P := ArrayItemPointer((L + R) shr 1);
      repeat
        IPtr := ArrayItemPointer(I);
        JPtr := ArrayItemPointer(J);
        while SortFunc(IPtr, P) < 0 do
        begin
          Inc(I);
          Inc(PByte(IPtr), ElSize);
        end;
        while SortFunc(JPtr, P) > 0 do
        begin
          Dec(J);
          Dec(PByte(JPtr), ElSize);
        end;
        if I <= J then
        begin
          if I <> J then
          begin
            case ElementSize of
              SizeOf(Byte):
                begin
                  T := PByte(IPtr)^;
                  PByte(IPtr)^ := PByte(JPtr)^;
                  PByte(JPtr)^ := T;
                end;
              SizeOf(Word):
                begin
                  T := PWord(IPtr)^;
                  PWord(IPtr)^ := PWord(JPtr)^;
                  PWord(JPtr)^ := T;
                end;
              SizeOf(Integer):
                begin
                  T := PInteger(IPtr)^;
                  PInteger(IPtr)^ := PInteger(JPtr)^;
                  PInteger(JPtr)^ := T;
                end;
            else
              Move(IPtr^, TempBuf[0], ElementSize);
              Move(JPtr^, IPtr^, ElementSize);
              Move(TempBuf[0], JPtr^, ElementSize);
            end;
          end;
          if P = IPtr then
            P := JPtr
          else
          if P = JPtr then
            P := IPtr;
          Inc(I);
          Dec(J);
        end;
      until I > J;
      if L < J then
        QuickSort(L, J);
      L := I;
    until I >= R;
  end;

begin
  if ArrayPtr <> nil then
  begin
    SetLength(TempBuf, ElementSize);
    QuickSort(0, PSizeInt(TJclAddr(ArrayPtr) - SizeOf(SizeInt))^ - 1);
  end;
end;

procedure TJclMapScanner.Scan;
begin
  FLineNumberErrors := 0;
  FSegmentCnt := 0;
  FProcNamesCnt := 0;
  FLastAccessedSegementIndex := 0;
  Parse;
  SetLength(FLineNumbers, FLineNumbersCnt);
  SetLength(FProcNames, FProcNamesCnt);
  SetLength(FSegments, FSegmentCnt);
  SortDynArray(FLineNumbers, SizeOf(FLineNumbers[0]), Sort_MapLineNumber);
  SortDynArray(FProcNames, SizeOf(FProcNames[0]), Sort_MapProcName);
  SortDynArray(FSegments, SizeOf(FSegments[0]), Sort_MapSegment);
  SortDynArray(FSourceNames, SizeOf(FSourceNames[0]), Sort_MapProcName);
end;

procedure TJclMapScanner.SegmentItem(const Address: TJclMapAddress; Len: Integer;
  GroupName, UnitName: PJclMapString);
var
  SegIndex: Integer;
  VA: DWORD;
begin
  for SegIndex := Low(FSegmentClasses) to High(FSegmentClasses) do
    if (FSegmentClasses[SegIndex].Segment = Address.Segment)
      and (DWORD(Address.Offset) < FSegmentClasses[SegIndex].Len) then
  begin
    if System.AnsiStrings.StrIComp(FSegmentClasses[SegIndex].GroupName.RawValue, 'TLS') = 0 then
      VA := Address.Offset
    else
      VA := MAPAddrToVA(Address.Offset + FSegmentClasses[SegIndex].Start);
    if FSegmentCnt mod 16 = 0 then
      SetLength(FSegments, FSegmentCnt + 16);
    FSegments[FSegmentCnt].Segment := FSegmentClasses[SegIndex].Segment;
    FSegments[FSegmentCnt].StartVA := VA;
    FSegments[FSegmentCnt].EndVA := VA + DWORD(Len);
    FSegments[FSegmentCnt].UnitName.RawValue := UnitName;
    Inc(FSegmentCnt);
    Break;
  end;
end;

function TJclMapScanner.SourceNameFromAddr(Addr: DWORD): string;
var
  I: Integer;
  ModuleStartVA: DWORD;
begin
  // try with line numbers first (Delphi compliance)
  ModuleStartVA := ModuleStartFromAddr(Addr);
  Result := '';
  I := SearchDynArray(FSourceNames, SizeOf(FSourceNames[0]), Search_MapProcName, @Addr, True);
  if (I <> -1) and (FSourceNames[I].VA >= ModuleStartVA) then
    Result := MapStringCacheToStr(FSourceNames[I].ProcName);
  if Result = '' then
  begin
    // try with module names (C++Builder compliance)
    I := IndexOfSegment(Addr);
    if I <> -1 then
      Result := MapStringCacheToFileName(FSegments[I].UnitName);
  end;
end;

end.

