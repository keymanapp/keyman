//
// Kept because of the expanded search path algorithm which may be useful for adding to the C++ version of tds2dbg in ext, in the future
//

(*
  Name:             DbgFile
  Copyright:        Copyright (C) 2003-2017 SIL International.
  Documentation:
  Description:
  Create Date:      27 May 2009

  Modified Date:    8 Jun 2012
  Authors:          mcdurdin
  Related Files:
  Dependencies:

  Bugs:
  Todo:
  Notes:
  History:          27 May 2009 - mcdurdin - I3213 - Review error management and improve [Audit4] <Modification>
                    18 Feb 2010 - mcdurdin - I5203 - tds2dbg should use full path for source files [DevTools] <Modification>
                    17 May 2012 - mcdurdin - I3310 - V9.0 - Unicode in Delphi fixes
*)
unit DbgFile;   // I3213

interface

uses
  Classes,
  Contnrs,
  ImageHlp,
  System.SysUtils,
  Windows,
  WinNT,
  Registry,
  cvexefmt,
  cvinfo;

//============================================================================
// TDbgFile -- for creating a .DBG file from scratch
// methods TDebugFile(fnexe,fndbg), AddSymbol(seg,off,name), End()
// They return a 'bool' for success or failure. The text string 'err'
// reports what that error was.
// End is automatically called by the destructor. But you might want
// to call it yourself, beforehand, for manual error checking.
// If file is non-null then it means we have succesfully set things up.
//============================================================================
// File format is as follows:
// In each column the offsets are relative to the start of that column.
// Thus, szHeader is relative to the file as a whole; cvoSstModule is relative to the
// start of the SstModule; gpoSym is relative to the start of GlobalPub module.
//
// @0. IMAGE_SEPARATE_DEBUG_HEADER -- header of the file. [WriteDBGHeader]
// @.  numsecs * IMAGE_SECTION_HEADER -- executable's section table. [WriteSectionTable]
// @.  1 * IMAGE_DEBUG_DIRECTORY -- only one cv-type debug directory. [WriteDbgDirectory]
// @szHeader. <cv-data> -- this is the raw data. of size szCv
//   @0. OMFSignature -- 'NB09'+omfdir. [in WriteCv]
//   @8. OMFDirHeader -- subsection directory header. [in WriteCv]
//   @.  3 * OMFDirEntry -- 3 directory entries: sstModule, sstGlobalPub, sstSegMap. [in WriteCv]
//   @cvoSstModule. <sst-module>, of length SstModuleSize. [WriteSstModule]
//     @0. OMFModule
//     @.  numsecs * OMFSegDest
//     @.  modname, of size fnsize.
//   @cvoGlobalPub. <global-pub>, of length GlobalPubSize.
//     @0. OMFSymHash -- [WriteGlobalPubHeader]
//     @.  nSymbols * var. Variable-sized sympols. [WriteSymbol]
//     @gpoSym. always points to the next symbol to write, is relative to the start of global-pub
//   @cvoSegMap. <seg-map>, of length SetMapSize. [WriteSegMap]
//     @0. OMFSegMap
//     @.  nsec * OMFSegMapDesc
//
// Start
//   * numsec deduced from the executable-image.
//   * szHeader is easy
//   * cvoSstModule, szSstModule are constant. szModName is easy.
//   * cvoGlobalPub just comes after, gpoSym initialized to after the OMFSymHash
//     [don't write anything yet]
// AddEntry
//   * increases gpoSym. [WriteSymbol]
// Finish
//   * cvoSegMap = cvoGlobalPub + gpoSym.
//     [WriteDBGHeader, WriteSectionTable, WriteDbgDirectory, WriteCv...]
//     [... WriteSstModule, WriteGlobalPubHeader, WriteSegMap]
//

type
  TOMFSourceFileDelphi = packed record
    cSeg: Word;           // number of segments from source file = 1
    reserved: Word;       // reserved
    baseSrcLn:  DWord;                 // base of OMFSourceLine tables
    _start: DWord;                      // this array is followed by array
    _end: Dword;                       // of segment start/end pairs followed by
                                       // an array of linker indices
                                       // for each segment in the file
    cFName: Byte;                      // length of source file name
    Name: array[0..0] of ansichar;         // name of file padded to long boundary  // I3310
  end;

  POMFSourceFileDelphi = ^TOMFSourceFileDelphi;

type
  EDbgFile = class(Exception);

  TDbgFileSymbol = class
  private
    FSize: Integer;
    FSym: PPUBSYM32;
    Buffer: array[0..511] of Byte;
  public
    constructor Create(seg: Word; offset: DWord; symbol: ansistring);  // I3310
    property Sym: PPUBSYM32 read FSym;
    property Size: Integer read FSize;
  end;

  TDbgFileSymbols = class(TObjectList)
  private
    function GetItem(Index: Integer): TDbgFileSymbol;
    procedure SetItem(Index: Integer; const Value: TDbgFileSymbol);
  public
    property Items[Index: Integer]: TDbgFileSymbol read GetItem write SetItem; default;
  end;

  TDbgFileSourceLine = class
  private
    FAddress: DWord;
    FLine: Integer;
  public
    constructor Create(AAddress: DWord; ALine: Integer);
    property Line: Integer read FLine;
    property Address: DWord read FAddress;
  end;

  TDbgFileSourceLines = class(TObjectList)
  private
    function GetItem(Index: Integer): TDbgFileSourceLine;
    procedure SetItem(Index: Integer; const Value: TDbgFileSourceLine);
  public
    property Items[Index: Integer]: TDbgFileSourceLine read GetItem write SetItem; default;
  end;

  TDbgFileSourceFile = class
  private
    FLines: TDbgFileSourceLines;
    FName: string;
    buffer: array[0..512] of byte;
    FSrcFile: POMFSourceFileDelphi;
    function GetSize: Integer;
    function GetLineSize: Integer;
    function LocateFileInSourcePath(ADprPaths, APaths: TStrings; AName, ARoot: string): string;   // I8274
  public
    constructor Create(ADprPaths, APaths: TStrings; const AName, ARoot: string);   // I5203   // I8274
    destructor Destroy; override;
    function AddSourceLine(offset: DWord; line: Integer): TDbgFileSourceLine;
    property Lines: TDbgFileSourceLines read FLines;
    property Name: string read FName;
    property SrcFile: POMFSourceFileDelphi read FSrcFile;
    property Size: Integer read GetSize;
    property LineSize: Integer read GetLineSize;
  end;

  TDbgFileSourceFiles = class(TObjectList)
  private
    function GetItem(Index: Integer): TDbgFileSourceFile;
    procedure SetItem(Index: Integer; const Value: TDbgFileSourceFile);
  public
    property Items[Index: Integer]: TDbgFileSourceFile read GetItem write SetItem; default;
  end;

  TDbgFile = class
  private
    FPaths: TStrings;   // I8274
    FFile: TFileStream;
    FIsMapped: Boolean;
    FExeFileName, FDbgFileName: AnsiString;  // I3310
    FError: string;
    FModuleName: ansistring;  // I3310
    FModuleNameLength: Cardinal;
    FSourceFiles: TDbgFileSourceFiles;
    FSymbols: TDbgFileSymbols;

    image: LOADED_IMAGE;
    FDprPaths: TStringList;   // I8274

    function Check(pos: DWord; const ErrorMessage: string): Boolean;
    function EnsureStarted: Boolean;
    function WriteGlobalPub(var omfdirentry: TOMFDirEntry;
      var p: Pointer): Boolean;
    function WriteModule(var omfdirentry: TOMFDirEntry;
      var p: Pointer): Boolean;
    function WriteSegMap(var omfdirentry: TOMFDirEntry;
      var p: Pointer): Boolean;
    function WriteSrcModule(var omfdirentry: TOMFDirEntry;
      var p: Pointer): Boolean;
    procedure LoadSourcePaths;   // I8274
    procedure LoadDprPaths(FDprFileName: string);   // I8274
  public
    constructor Create(const AExeFileName, ADbgFileName: string);
    destructor Destroy; override;
    function EndWrite: Boolean;
    function AddSymbol(seg: Word; offset: DWord; symbol: ansistring): Boolean;  // I3310
    function AddSourceFile(name, root: string): TDbgFileSourceFile;   // I5203
  end;

implementation

uses
  Winapi.ActiveX,
  Winapi.ShlObj,
  AnsiStrings,
  RegExpr,
  Unicode;

{ TDbgFile }

const
  SRegKey_Delphi = 'Software\Embarcadero\BDS\18.0';
  SRegKey_DelphiLibraryWin32 = 'Software\Embarcadero\BDS\18.0\Library\Win32';
  SRegValue_BrowsingPath = 'Browsing Path';
  SRegValue_SearchPath = 'Search Path';
  SRegValue_RootDir = 'RootDir';
  Path_Source = 'source';
  Path_Lib = 'lib';
  Path_UserDir = 'Embarcadero\Studio\18.0';

constructor TDbgFile.Create(const AExeFileName, ADbgFileName: string);
var
  FDprFileName: string;
begin
  inherited Create;
  FPaths := TStringList.Create;   // I8274
  FDprPaths := TStringList.Create;   // I8274
  FSourceFiles := TDbgFileSourceFiles.Create;
  FSymbols := TDbgFileSymbols.Create;
  FExeFileName := String_UtoA(AExeFileName);  // I3310
  FDbgFileName := String_UtoA(ADbgFileName);  // I3310
  FDprFileName := ChangeFileExt(AExeFileName, '.dpr');   // I8274  // I3310
  if FileExists(FDprFileName) then LoadDprPaths(FDprFileName);   // I8274
  LoadSourcePaths;   // I8274
end;

destructor TDbgFile.Destroy;
begin
	EndWrite;
	if FIsMapped then
	  UnMapAndLoad(@image);
	FIsMapped := False;
	FreeAndNil(FFile);
  FSourceFiles.Free;
  FSymbols.Free;
  FreeAndNil(FPaths);   // I8274
  FreeAndNil(FDprPaths);   // I8274
  inherited Destroy;
end;

procedure TDbgFile.LoadDprPaths(FDprFileName: string);   // I8274
var
  i: Integer;
  re: TRegExpr;
begin
  re := TRegExpr.Create;
  try
    re.Expression := '^\s*[a-zA-Z0-9_]+ in ''(.+)''';
    with TStringList.Create do
    try
      LoadFromFile(FDprFileName);
      for i := 0 to Count - 1 do
      begin
        if re.Exec(Strings[i]) then
        begin
          FDprPaths.Add(ExtractFileName(re.Match[1])+'='+re.Match[1]);
        end;
      end;
    finally
      Free;
    end;
  finally
    re.Free;
  end;
end;

procedure TDbgFile.LoadSourcePaths;   // I8274
var
  FRootDir: string;

    procedure AddSearchPaths(path: string);
    var
      f: TSearchRec;
    begin
      if FPaths.IndexOf(path) < 0 then
        FPaths.Add(path);
      if FindFirst(path + '*', faDirectory, f) = 0 then
      begin
        repeat
          if (f.Name <> '.') and (f.Name <> '..') and ((f.Attr and faDirectory) = faDirectory) then
            AddSearchPaths(path + f.Name + '\');
        until FindNext(f) <> 0;
        System.SysUtils.FindClose(f);
      end;
    end;

  function GetFolderPath(csidl: Integer): string;
  var
    buf: array[0..260] of Char;
    idl: PItemIDList;
    mm: IMalloc;
  begin
    Result := '';
    if SHGetMalloc(mm) = NOERROR then
    begin
      if SHGetSpecialFolderLocation(0, csidl, idl) = NOERROR then
      begin
        if SHGetPathFromIDList(idl, buf) then
        begin
          Result := Buf;
        end;
        mm.Free(idl);
      end;
      mm._Release;
    end;

    if Result <> '' then
      if Result[Length(Result)] <> '\' then Result := Result + '\';
  end;


  function ExpandVariables(Path: string): string;
  begin
    Path := StringReplace(Path, '$(BDS)', ExcludeTrailingPathDelimiter(FRootDir), [rfReplaceAll, rfIgnoreCase]);
    Path := StringReplace(Path, '$(BDSLIB)', IncludeTrailingPathDelimiter(FRootDir) + Path_Lib, [rfReplaceAll, rfIgnoreCase]);
    Path := StringReplace(Path, '$(PLATFORM)', 'Win32', [rfReplaceAll, rfIgnoreCase]);
    Path := StringReplace(Path, '$(BDSUSERDIR)', GetFolderPath(CSIDL_MYDOCUMENTS) + Path_UserDir, [rfReplaceAll, rfIgnoreCase]);
    Path := StringReplace(Path, '$(BDSCOMMONDIR)', GetFolderPath(CSIDL_COMMON_DOCUMENTS) + Path_UserDir, [rfReplaceAll, rfIgnoreCase]);
    Result := Path;
  end;

  procedure AddPaths(RegValue: string);
  var
    i: Integer;
    FNewPaths: TStringList;
    s: string;
  begin
    FNewPaths := TStringList.Create;
    with TRegistry.Create do
    try
      if OpenKeyReadOnly(SRegKey_DelphiLibraryWin32) and ValueExists(RegValue) then
      begin
        FNewPaths.StrictDelimiter := True;
        FNewPaths.Delimiter := ';';
        FNewPaths.QuoteChar := #0;
        FNewPaths.DelimitedText := ReadString(RegValue);
        for i := 0 to FNewPaths.Count-1 do
        begin
          s := FNewPaths[i];
          s := ExpandVariables(s);
          s := IncludeTrailingPathDelimiter(s);
          if FPaths.IndexOf(s) < 0 then
            FPaths.Add(s);
        end;
      end;
    finally
      Free;
      FNewPaths.Free;
    end;
  end;

begin
  with TRegistry.Create do
  try
    if OpenKeyReadOnly(SRegKey_Delphi) and ValueExists(SRegValue_RootDir) then
      FRootDir := ReadString(SRegValue_RootDir);
  finally
    Free;
  end;

  // We'll use the Embarcadero source path?
  AddPaths(SRegValue_BrowsingPath);
  AddPaths(SRegValue_SearchPath);
  AddSearchPaths(IncludeTrailingPathDelimiter(FRootDir)+Path_Source+'\');
end;

function TDbgFile.Check(pos: DWord; const ErrorMessage: string): Boolean;
begin
  if (pos <> FFile.Position) and (FError <> '') then
  begin
    FError := ErrorMessage;
    Result := False;
  end
  else
    Result := True;
end;

function offsetof(p, q: Pointer): DWord;
begin
  Result := DWord(q) - DWord(p);
end;

function TDbgFile.EnsureStarted: Boolean; // this routine is called automatically by AddSymbol and End
begin
  Result := True;
  if Assigned(FFile) then Exit;

  if not MapAndLoad(PAnsiChar(FExeFileName), nil, @image, False, True) then  // I3310  // I3310
    raise EDbgFile.Create('Failed to load executable "'+String_AtoU(FExeFileName)+'"');  // I3310

  FIsMapped := True;

  FModuleName        := AnsiStrings.ChangeFileExt(AnsiStrings.ExtractFileName(FExeFileName), '');  // I3310  // I3310
  FModuleNameLength  := ((Length(FModuleName) + 1)+3) and (not 3); // round it up

  FFile := TFileStream.Create(String_AtoU(FDbgFileName), fmCreate);  // I3310
end;

function TDbgFile.AddSymbol(seg: Word; offset: DWord; symbol: ansistring): Boolean;  // I3310
begin
  FSymbols.Add(TDbgFileSymbol.Create(seg, offset, symbol));
  Result := True;
end;

function TDbgFile.WriteModule(var omfdirentry: TOMFDirEntry; var p: Pointer): Boolean;
var
  omfmodule: TOMFModule;

  pomfmodule: cvexefmt.POMFModule;
  pomfsegdesc: cvexefmt.POMFSegDesc;

  s: PImageSectionHeader;
  i: Integer;
begin
  omfdirentry.SubSection := sstModule;
  omfdirentry.iMod := 1;
  omfdirentry.cb := offsetof(@OMFModule,@OMFModule.SegInfo) + image.NumberOfSections*sizeof(TOMFSegDesc) + FModuleNameLength;

  p := AllocMem(omfdirentry.cb);
  pomfmodule := cvexefmt.POMFModule(p);

  pomfmodule.ovlNumber := 0;
  pomfmodule.iLib := 0;
  pomfmodule.cSeg := Word(image.NumberOfSections);
  pomfmodule.Style[0] := 'C';
  pomfmodule.Style[1] := 'V';

  pomfsegdesc := @pomfmodule.SegInfo[0];

  s := image.Sections;
  for i := 0 to image.NumberOfSections - 1 do
  begin
	  pomfsegdesc.Seg := Word(i+1);
  	pomfsegdesc.pad := 0;
	  pomfsegdesc.Off := 0;
  	pomfsegdesc.cbSeg := s.Misc.VirtualSize;
    Inc(s);
    Inc(pomfsegdesc);
  end;

  AnsiStrings.StrPCopy(PAnsiChar(pomfsegdesc), FModuleName); // Will be zero padded according to FModuleNameLength  // I3310

  Result := True;
end;

function TDbgFile.WriteSrcModule(var omfdirentry: TOMFDirEntry; var p: Pointer): Boolean;
var
  numsecs: Integer;
  pomfSrcModule: cvexefmt.POMFSourceModule;
  pomfSrcFile: POMFSourceFileDelphi;
  pomfSrcLine: POMFSourceLine;
  poffset, pd: PDWord;
  pw: PWord;
  pline: PWord;
  pp: PAnsiChar;  // I3310
  i, j: Integer;
  szHeader, szFiles, szLines: DWord;
begin
  if FSourceFiles.Count = 0 then
    Exit(False);

  numsecs := 1; // delphi only writes code in section 1

  szHeader := SizeOf(TOMFSourceModule) - SizeOf(DWord) +    // cFile: Word, cSeg: Word (subtract initial baseSrcFile)
    FSourceFiles.Count * SizeOf(DWord) +                    // baseSrcFile: array[cFile] of DWORD
    numsecs * SizeOf(Dword) * 2 +                           // startEnd: array[cSeg] of DWORD,DWORD
    numsecs * SizeOf(Word);                                 // seg: array[cSeg] of WORD
  if (numsecs mod 2) = 1 then Inc(szHeader, 2);             // Padding

  szFiles := 0;
  szLines := 0;
  for i := 0 to FSourceFiles.Count - 1 do
  begin
    szFiles := szFiles + DWord(FSourceFiles[i].Size);
    szLines := szLines + DWord(FSourceFiles[i].LineSize);
    if (FSourceFiles[i].Lines.Count mod 2) = 1 then Inc(szLines, 2); // pad
  end;

  //if (szFiles mod 4) <> 0 then Inc(szFiles, 4-(szFiles mod 4));
  //if (szLines mod 4) <> 0 then Inc(szLines, 4-(szLines mod 4));

  omfdirentry.SubSection := sstSrcModule;
  omfdirentry.iMod := 1;
  omfdirentry.cb := szHeader + szFiles + szLines;
  if (omfdirentry.cb mod 4) <> 0 then Inc(omfdirentry.cb, 4 - (omfdirentry.cb mod 4));

  p := AllocMem(omfdirentry.cb);

  pomfSrcModule := cvexefmt.POMFSourceModule(p);
  pomfSrcModule.cFile := FSourceFiles.Count;
  pomfSrcModule.cSeg := 1;

  pp := p; Inc(pp, SizeOf(TOMFSourceModule) - SizeOf(DWord) +    // cFile: Word, cSeg: Word (subtract initial baseSrcFile)
    FSourceFiles.Count * SizeOf(DWord));                   // baseSrcFile: array[cFile] of DWORD
  pd := PDWord(pp);
  pd^ := FSourceFiles[0].Lines[0].Address;
  Inc(pd);
  pd^ := FSourceFiles[FSourceFiles.Count-1].Lines[FSourceFiles[FSourceFiles.Count-1].Lines.Count-1].Address;
  Inc(pd);
  pw := PWord(pd);
  pw^ := 1;

  pd := @pomfSrcModule.baseSrcFile[0];
  pp := p; Inc(pp, szHeader); pomfSrcFile := POMFSourceFileDelphi(pp);

  for i := 0 to FSourceFiles.Count - 1 do
  begin
    pd^ := DWord(pomfSrcFile) - DWord(p);
    Move(FSourceFiles[i].SrcFile^, pomfSrcFile^, FSourceFiles[i].Size);

    pomfSrcLine := POMFSourceLine(DWord(pomfSrcFile) + DWord(FSourceFiles[i].Size));
    pomfSrcFile.baseSrcLn := DWord(pomfSrcLine) - DWord(p);
    pomfSrcFile._start := FSourceFiles[i].Lines[0].Address;
    pomfSrcFile._end := FSourceFiles[i].Lines[FSourceFiles[i].Lines.Count-1].Address;
    pomfSrcLine.Seg := 1;
    pomfSrcLine.cLnOff := FSourceFiles[i].Lines.Count;

    poffset := @pomfSrcLine.offset[0];
    for j := 0 to FSourceFiles[i].Lines.Count - 1 do
    begin
      poffset^ := FSourceFiles[i].Lines[j].FAddress;
      Inc(poffset);
    end;
    pline := PWord(poffset);
    for j := 0 to FSourceFiles[i].Lines.Count - 1 do
    begin
      pline^ := FSourceFiles[i].Lines[j].line;
      Inc(pline);
    end;

    // Move to next record
    Inc(pd);

    if (FSourceFiles[i].Lines.Count mod 2) = 1 then Inc(pline); // pad
    pomfSrcFile := POMFSourcefileDelphi(pline);

    //pp := PChar(pomfSrcFile);
    //Inc(pp, FSourceFiles[i].Size);
    //pomfSrcFile := POMFSourceFileDelphi(pp);

    //pomfsrcLine := cvexefmt.POMFSourceLine(pline);
  end;

  Result := true;
end;


function TDbgFile.WriteGlobalPub(var omfdirentry: TOMFDirEntry; var p: Pointer): Boolean;
var
  pomfSymHash: cvexefmt.POMFSymHash;
  sz: Dword;
  i: Integer;
  pp: PAnsiChar;  // I3310
begin
  sz := 0;
  for i := 0 to FSymbols.Count - 1 do
    Inc(sz, FSymbols[i].Size);

  if (sz mod 4) <> 0 then Inc(sz, 4 - (sz mod 4));

  omfdirentry.SubSection := sstGlobalPub;
  omfdirentry.iMod := $FFFF;
  omfdirentry.cb := SizeOf(TOMFSymHash) + sz;

  p := AllocMem(omfdirentry.cb);
  pomfsymhash := cvexefmt.POMFSymHash(p);
  pomfSymHash.cbSymbol := sz;
  pomfSymHash.symhash := 0; // No symbol or address hash tables...
  pomfSymHash.addrhash := 0;
  pomfSymHash.cbHSym := 0;
  pomfSymHash.cbHAddr := 0;
  Inc(pomfSymHash);
  pp := PAnsiChar(pomfSymHash);  // I3310

  for i := 0 to FSymbols.Count - 1 do
  begin
    Move(FSymbols[i].Sym^, pp^, FSymbols[i].Size);
    Inc(pp, FSymbols[i].Size);
  end;
  Result := True;
end;

function TDbgFile.WriteSegMap(var omfdirentry: TOMFDirEntry; var p: Pointer): Boolean;
var
  pomfSegMap: cvexefmt.POMFSegMap;
  pomfSegMapDesc: cvexefmt.POMFSegMapDesc;
  s: PImageSectionHeader;
  i: Integer;
begin
  omfdirentry.SubSection := sstSegMap;
  omfdirentry.iMod := $FFFF;
  omfdirentry.cb := SizeOf(TOMFSegMap) + (image.NumberOfSections-1)*sizeof(TOMFSegMapDesc);

  p := AllocMem(omfdirentry.cb);

  pomfSegMap := cvexefmt.POMFSegMap(p);
  pomfSegMap.cSeg := image.NumberOfSections;
  pomfSegMap.cSegLog := image.NumberOfSections;

  pomfSegMapDesc := @pomfSegMap.rgDesc[0];
  s := image.Sections;
  for i := 1 to image.NumberOfSections do
  begin
    pomfSegMapDesc.flags := 0;
    pomfSegMapDesc.ovl := 0;
    pomfSegMapDesc.group := 0;
    pomfSegMapDesc.frame := Word(i);
    pomfSegMapDesc.iSegName := $FFFF;
    pomfSegMapDesc.iClassName := $FFFF;
    pomfSegMapDesc.offset := 0;
    pomfSegMapDesc.cbSeg := s.Misc.VirtualSize;
    Inc(s);
    inc(pomfSegMapDesc);
  end;

  Result := True;
end;

function TDbgFile.EndWrite: Boolean;
var
  numsecs: Integer;
  isdh: IMAGE_SEPARATE_DEBUG_HEADER;
  idd: IMAGE_DEBUG_DIRECTORY;
  omfsig: TOMFSignature;
  omfdirhdr: TOMFDirHeader;

  omfdirentry: array[0..3] of TOMFDirEntry;
  pdata: array[0..3] of Pointer;
  p: Pointer;

  i: Integer;
begin
  EnsureStarted;
  
  numsecs := image.NumberOfSections;

  if numsecs >= $FFFF then
  begin
    Result := False; // OMFSegDesc only uses 'unsigned short'
    Exit;
  end;

  // CodeView Signature
  omfsig.Signature[0] := 'N';
  omfsig.Signature[1] := 'B';
  omfsig.Signature[2] := '0';
  omfsig.Signature[3] := '9';
  omfsig.FilePos := SizeOf(omfsig);

  // CodeView Directory Header
  omfdirhdr.cbDirHeader := sizeof(omfdirhdr);
  omfdirhdr.cbDirEntry := sizeof(TOMFDirEntry);
  omfdirhdr.cDir := Length(omfdirentry);
  omfdirhdr.lfoNextDir := 0;
  omfdirhdr.flags := 0;

  // Prepare all data sections
  WriteModule(omfdirentry[0], pData[0]);
  if not WriteSrcModule(omfdirentry[1], pData[1]) then Exit(False);
  WriteGlobalPub(omfdirentry[2], pData[2]);
  WriteSegMap(omfdirentry[3], pData[3]);

  // Calculate directory offsets
  omfdirentry[0].lfo := sizeof(TOMFSignature) + sizeof(TOMFDirHeader) + omfdirhdr.cDir*sizeof(TOMFDirEntry);
  for i := 1 to omfdirhdr.cDir - 1 do
    omfdirentry[i].lfo := DWord(omfdirentry[i-1].lfo) + omfdirentry[i-1].cb;

  //
  // Debug Header
  isdh.Signature := IMAGE_SEPARATE_DEBUG_SIGNATURE;
  isdh.Flags := 0;
  isdh.Machine            := image.FileHeader.FileHeader.Machine;
  isdh.Characteristics    := image.FileHeader.FileHeader.Characteristics;
  isdh.TimeDateStamp      := image.FileHeader.FileHeader.TimeDateStamp;
  isdh.CheckSum           := image.FileHeader.OptionalHeader.CheckSum;
  isdh.ImageBase          := image.FileHeader.OptionalHeader.ImageBase;
  isdh.SizeOfImage        := image.FileHeader.OptionalHeader.SizeOfImage;
  isdh.NumberOfSections   := numsecs;
  isdh.ExportedNamesSize  := 0;
  isdh.DebugDirectorySize := 1*sizeof(IMAGE_DEBUG_DIRECTORY);
  isdh.SectionAlignment   := image.FileHeader.OptionalHeader.SectionAlignment;

  //
  // Debug Directory
  idd.Characteristics := 0;
  idd.TimeDateStamp := image.FileHeader.FileHeader.TimeDateStamp;
  idd.MajorVersion := 0;
  idd.MinorVersion := 0;
  idd._Type := IMAGE_DEBUG_TYPE_CODEVIEW;
  idd.SizeOfData := DWord(omfdirentry[omfdirhdr.cDir - 1].lfo) + omfdirentry[omfdirhdr.cDir - 1].cb;
  idd.AddressOfRawData := 0;
  idd.PointerToRawData := sizeof(IMAGE_SEPARATE_DEBUG_HEADER) + image.NumberOfSections*sizeof(IMAGE_SECTION_HEADER) + 1*sizeof(IMAGE_DEBUG_DIRECTORY);

  EnsureStarted;
  FFile.Seek(0, soFromBeginning);

  // Debug Header, Sections and Directory
  FFile.WriteBuffer(isdh, SizeOf(isdh));                                        Check(sizeof(IMAGE_SEPARATE_DEBUG_HEADER), 'Section table');
  FFile.WriteBuffer(image.Sections^, sizeof(IMAGE_SECTION_HEADER) * numsecs);   Check(sizeof(IMAGE_SEPARATE_DEBUG_HEADER) + numsecs*sizeof(IMAGE_SECTION_HEADER), 'Debug directory');
  FFile.WriteBuffer(idd, SizeOf(idd));

  // CodeView
  check(sizeof(IMAGE_SEPARATE_DEBUG_HEADER) + image.NumberOfSections*sizeof(IMAGE_SECTION_HEADER) + 1*sizeof(IMAGE_DEBUG_DIRECTORY), 'CV data');
  FFile.WriteBuffer(omfsig, SizeOf(omfsig));
  FFile.WriteBuffer(omfdirhdr, SizeOf(omfdirhdr));
  FFile.WriteBuffer(omfdirentry, SizeOf(TOMFDirEntry) * omfdirhdr.cDir);

  for i := 0 to omfdirhdr.cDir - 1 do
  begin
    p := PData[i];
    FFile.WriteBuffer(p^, omfdirentry[i].cb);
    FreeMem(p);
  end;

  Result := True;
end;

function TDbgFile.AddSourceFile(name, root: string): TDbgFileSourceFile;   // I5203
begin
  Result := TDbgFileSourceFile.Create(FDprPaths, FPaths, name, root);   // I5203   // I8274
  FSourceFiles.Add(Result);
end;

{ TDbgFileSourceLines }

function TDbgFileSourceLines.GetItem(Index: Integer): TDbgFileSourceLine;
begin
  Result := inherited GetItem(Index) as TDbgFileSourceLine;
end;

procedure TDbgFileSourceLines.SetItem(Index: Integer; const Value: TDbgFileSourceLine);
begin
  inherited SetItem(Index, Value);
end;

{ TDbgFileSourceFiles }

function TDbgFileSourceFiles.GetItem(Index: Integer): TDbgFileSourceFile;
begin
  Result := inherited GetItem(Index) as TDbgFileSourceFile;
end;

procedure TDbgFileSourceFiles.SetItem(Index: Integer; const Value: TDbgFileSourceFile);
begin
  inherited SetItem(Index, Value);
end;

{ TDbgFileSourceFile }

function TDbgFileSourceFile.AddSourceLine(offset: DWord;
  line: Integer): TDbgFileSourceLine;
begin
  Result := TDbgFileSourceLine.Create(offset, line);
  FLines.Add(Result);
end;

function ExpandFileNameEx(const AName, ARoot: string): string;   // I5203
begin
  if Copy(AName, 2, 1) = ':' then Result := AName // 'C:\...
  else if Copy(AName, 1, 1) = '\' then
    Result := Copy(ARoot, 1, 2) + AName
  else
    Result := ARoot + AName;
  Result := ExpandFileName(Result);
end;

function TDbgFileSourceFile.LocateFileInSourcePath(ADprPaths, APaths: TStrings; AName, ARoot: string): string;   // I8274
var
  i: Integer;
begin
  AName := ExtractFileName(AName);

  i := ADprPaths.IndexOfName(AName);
  if i >= 0 then
  begin
    Result := ExpandFileNameEx(ADprPaths.ValueFromIndex[i], ARoot);
    Exit;
  end;

  for i := 0 to APaths.Count-1 do
  begin
    if FileExists(APaths[i] + AName) then
    begin
      Result := APaths[i] + AName;
      Exit;
    end;
  end;
  Result := AName;
end;

constructor TDbgFileSourceFile.Create(ADprPaths, APaths: TStrings; const AName, ARoot: string);   // I8274
var
  FFullName: string;
begin
  inherited Create;
  FSrcFile := POMFSourceFileDelphi(@Buffer[0]);
  FName := AName;
  FFullName := ExpandFileNameEx(AName, ARoot);   // I5203

  if not FileExists(FFullName) then   // I8274
    FFullName := LocateFileInSourcePath(ADprPaths, APaths, AName, ARoot);

  FSrcFile.cSeg := 1;
  FSrcFile.reserved := 0;
  FSrcfile.baseSrcLn := 0;
  FSrcFile._start := 0;
  FSrcFile._end := 0;
  FSrcFile.cFName := Length(FFullName) + 1;   // I5203
  if (FSrcFile.cFName mod 4) <> 0 then Inc(FSrcFile.cFName, 4 - (FSrcFile.cFName mod 4));
  Dec(FSrcFile.cFName);
  AnsiStrings.StrPCopy(FSrcFile.Name, String_UtoA(FFullName)); // TO DO: Paths   // I5203  // I3310

  FLines := TDbgFileSourceLines.Create;
end;

destructor TDbgFileSourceFile.Destroy;
begin
  FLines.Free;
  inherited Destroy;
end;

function TDbgFileSourceFile.GetLineSize: Integer;
begin
  Result :=
    SizeOf(TOMFSourceLine) - SizeOf(DWord) - SizeOf(Word) +    // Header - initial entry
    Lines.Count * (SizeOf(DWord) + SizeOf(Word)); // Each line
end;

function TDbgFileSourceFile.GetSize: Integer;
begin
  Result := SizeOf(TOMFSourceFileDelphi) + FSrcFile.cFName - 1;
end;

{ TDbgFileSourceLine }

constructor TDbgFileSourceLine.Create(AAddress: DWord; ALine: Integer);
begin
  inherited Create;
  FAddress := AAddress;
  FLine := ALine;
end;

{ TDbgFileSymbol }

constructor TDbgFileSymbol.Create(seg: Word; offset: DWord; symbol: ansistring);  // I3310
var
  cbSymbol: DWord;
begin
  if Length(symbol) > 255 then symbol := Copy(symbol, 1, 255);

  cbSymbol := Length(symbol);
  if (cbSymbol mod 4) <> 0 then Inc(cbSymbol, 4 - (cbSymbol mod 4));
  Inc(cbSymbol, 2); // pad to 4

  FSize := SizeOf(PUBSYM32) + cbSymbol;

  FSym := PPUBSYM32(@Buffer[0]);
  FSym.reclen   := Word(FSize - 2);
  FSym.rectyp   := S_PUB32;
  FSym.off      := offset;
  FSym.seg      := seg;
  FSym.typind   := 0;
  FSym.name[0]  := AnsiChar(cbSymbol);  // I3310
  AnsiStrings.StrPCopy(@FSym.name[1], symbol);
end;

{ TDbgFileSymbols }

function TDbgFileSymbols.GetItem(Index: Integer): TDbgFileSymbol;
begin
  Result := inherited GetItem(Index) as TDbgFileSymbol;
end;

procedure TDbgFileSymbols.SetItem(Index: Integer; const Value: TDbgFileSymbol);
begin
  inherited SetItem(Index, Value);
end;

end.
