(*
  Name:             udumpfile
  Copyright:        Copyright (C) SIL International.
  Documentation:    km4.1
  Description:      dump parts of a PE file - searching for hook functions
  Create Date:      13 May 2005

  Modified Date:    8 Jun 2012
  Authors:          mcdurdin
  Related Files:    
  Dependencies:     

  Bugs:             
  Todo:             
  Notes:            
  History:          13 May 2005 - mcdurdin - Integrated into kmshell from tsysinfo
                    08 Jun 2012 - mcdurdin - I3310 - V9.0 - Unicode in Delphi fixes
*)
unit udumpfile;

interface

uses
  Windows,
  System.Classes,
  SysUtils,
  Contnrs;

type
  EDumpFile = class(Exception);

  TDumpFileImport = class
  private
    FModuleName: string;
    FTimeDateStamp,
    FForwarderChain,
    FOrdinal: Dword;
    FImportName: string;
  public
    constructor Create(const AModuleName: string; ATimeDateStamp: DWord; AForwarderChain: DWord; AOrdinal: DWord; const AImportName: string = '');
    property ModuleName: string read FModuleName;
    property ImportName: string read FImportName;
  end;

  TDumpFileImports = class(TObjectList)
  private
    function GetItem(Index: Integer): TDumpFileImport;
    procedure SetItem(Index: Integer; const Value: TDumpFileImport);
  public
    function IndexOf(const ModuleName, ImportName: string): Integer;
    property Items[Index: Integer]: TDumpFileImport read GetItem write SetItem; default;
  end;

  TDumpFile = class
  private
    FRaw, FFileName: string;
    FImports: TDumpFileImports;
    procedure DumpImportsSection(base: DWord; pNTHeader: PImageNTHeaders);
    //function GetSectionHeader(name: string;
    //  pNTHeader: PImageNtHeaders): PImageSectionHeader;
    procedure DumpExeFile(dosHeader: PImageDosHeader);
    function GetPtrFromRva(rva: DWord; pNTHeader: PImageNtHeaders;
      imageBase: PByte; var delta: Integer): Pointer;
  public
    constructor Create(const AFileName: string);
    destructor Destroy; override;
    procedure Dump;
    property FileName: string read FFileName;
    property Imports: TDumpFileImports read FImports;
  end;

implementation

procedure TDumpFile.Dump;
var
  hFile: THandle;
  hFileMapping: THandle;
  lpFileBase: Pointer;
  dosHeader: PImageDosHeader;
begin
  hFile := CreateFile(PChar(FFileName), GENERIC_READ, FILE_SHARE_READ, nil, OPEN_EXISTING, FILE_ATTRIBUTE_NORMAL, 0);
  if hFile = INVALID_HANDLE_VALUE then
    raise EDumpFile.Create('Could not open file with CreateFile()');

  hFileMapping := CreateFileMapping(hFile, nil, PAGE_READONLY, 0, 0, nil);
  if hFileMapping = 0 then
  begin
    CloseHandle(hFile);
    raise EDumpFile.Create('Couldn''t open file mapping with CreateFileMapping()');
  end;

  lpFileBase := MapViewOfFile(hFileMapping, FILE_MAP_READ, 0, 0, 0);
  if lpFileBase = nil then
  begin
    CloseHandle(hFileMapping);
    CloseHandle(hFile);
    raise EDumpFile.Create('Couldn''t map view of file with MapViewOfFile()');
  end;

  dosHeader := PImageDosHeader(lpFileBase);
  if dosHeader.e_magic = IMAGE_DOS_SIGNATURE then
    DumpExeFile(dosHeader)
  else
    raise EDumpFile.Create('unrecognized file format');
  UnmapViewOfFile(lpFileBase);
  CloseHandle(hFileMapping);
  CloseHandle(hFile);
end;

//
// Given a section name, look it up in the section table and return a
// pointer to its IMAGE_SECTION_HEADER
//
(*function TDumpFile.GetSectionHeader(name: string; pNTHeader: PImageNtHeaders): PImageSectionHeader;
var
  section: PImageSectionHeader;
  pp: PImageNtHeaders;
  i: Integer;
  s: string;
begin
  pp := pNTHeader; Inc(pp);
  section := PImageSectionHeader(pp);

  for i := 0 to pNTHeader.FileHeader.NumberOfSections - 1 do
  begin
    s := PChar(@section.Name[0]);
    if AnsiCompareText(s, name) = 0 then
    begin
      Result := section;
      Exit;
    end;
    Inc(section);
  end;

  Result := nil;
end;*)

(*
//
// Dump the exports table (the .edata section) of a PE file
//
  void DumpExportsSection(DWORD base, PIMAGE_NT_HEADERS pNTHeader)
  {
  PIMAGE_EXPORT_DIRECTORY exportDir;
  PIMAGE_SECTION_HEADER header;
  INT delta;
  PSTR filename;
  DWORD i;
  PDWORD functions;
  PWORD ordinals;
  PSTR *name;

  header = GetSectionHeader(".edata", pNTHeader);
  if ( !header )
    return;
  exportDir = MakePtr(PIMAGE_EXPORT_DIRECTORY, base,
             header->PointerToRawData);
  delta = (INT)(header->VirtualAddress - header->PointerToRawData);

  filename = (PSTR)(exportDir->Name - delta + base);

  printf("exports table:\n\n");
  printf("  Name:            %s\n", filename);
  printf("  Characteristics: %08X\n", exportDir->Characteristics);
  printf("  TimeDateStamp:   %08X\n", exportDir->TimeDateStamp);
  printf("  Version:         %u.%02u\n", exportDir->MajorVersion,
      exportDir->MinorVersion);
  printf("  Ordinal base:    %08X\n", exportDir->Base);
  printf("  # of functions:  %08X\n", exportDir->NumberOfFunctions);
  printf("  # of Names:      %08X\n", exportDir->NumberOfNames);

  functions = (PDWORD)((DWORD)exportDir->AddressOfFunctions - delta + base);
  ordinals = (PWORD)((DWORD)exportDir->AddressOfNameOrdinals - delta + base);
  name = (PSTR * )((DWORD)exportDir->AddressOfNames - delta + base);

  printf("\n  Entry Pt  Ordn  Name\n");
  for ( i=0; i < exportDir->NumberOfNames; i++ )
  {
    printf("  %08X  %4u  %s\n", *functions,
        *ordinals + exportDir->Base,
        ( *name - delta + base));
    name++;			// Bump each pointer to the next array element
    ordinals++;
    functions++;
  }
  }
  *)

type
  PImageImportDescriptor = ^TImageImportDescriptor;

  TImageImportDescriptor = record
    Characteristics_OriginalFirstThunk: DWORD;

          // 0 for terminating null import descriptor
          // RVA to original unbound IAT (PIMAGE_THUNK_DATA)

    TimeDateStamp: DWORD;                  // 0 if not bound,
                                            // -1 if bound, and real date\time stamp
                                            //     in IMAGE_DIRECTORY_ENTRY_BOUND_IMPORT (new BIND)
                                            // O.W. date/time stamp of DLL bound to (Old BIND)

    ForwarderChain: DWord;                 // -1 if no forwarders
    Name: DWORD;
    FirstThunk: DWORD;                     // RVA to IAT (if bound this IAT has actual addresses)
  end;

  PImageThunkData = ^TImageThunkData;
  TImageThunkData = record
    case Integer of
      1: (ForwarderString: DWord);      // PBYTE
      2: (AFunction: DWord);            // PDWORD
      3: (Ordinal: DWord);
      4: (AddressOfData: DWord);        // PImageImportByName
  end;

  PImageImportByName = ^TImageImportByName;
  TImageImportByName = record
    Hint: Word;
    Name: array[0..0] of Byte;
  end;

const
  IMAGE_ORDINAL_FLAG: DWord = $80000000;

//
// Dump the imports table (the .idata section) of a PE file
//

//================================================================================
//
// Given an RVA, look up the section header that encloses it and return a
// pointer to its IMAGE_SECTION_HEADER
//
function GetEnclosingSectionHeader(rva: DWord; pNTHeader: PImageNtHeaders): PImageSectionHeader;
var
  section: PImageSectionHeader;
  size: DWord;
  i: Integer;
  pp: PImageNtHeaders;
begin
  pp := pNTHeader; Inc(pp);
  section := PImageSectionHeader(pp);
  for i := 0 to pNTHeader.FileHeader.NumberOfSections-1 do
  begin
    // This 3 line idiocy is because Watcom's linker actually sets the
    // Misc.VirtualSize field to 0.  (!!! - Retards....!!!)
    if section.Misc.VirtualSize = 0
      then size := section.SizeOfRawData
      else size := section.Misc.VirtualSize;

    // Is the RVA within this section?
    if (rva >= section.VirtualAddress) and (rva < section.VirtualAddress + size) then
    begin
      Result := section;
      Exit;
    end;
    Inc(section);
  end;

  Result := nil;
end;

function TDumpFile.GetPtrFromRva(rva: DWord; pNTHeader: PImageNtHeaders; imageBase: PByte; var delta: Integer): Pointer;
var
  pSectionHdr: PImageSectionHeader;
begin
  Result := nil;
  pSectionHdr := GetEnclosingSectionHeader(rva, pNTHeader);
  if not Assigned(pSectionHdr) then Exit;
  delta := Integer(pSectionHdr.VirtualAddress-pSectionHdr.PointerToRawData);
  Result := Pointer(Integer(imageBase) + Integer(rva) - delta);
end;

procedure TDumpFile.DumpImportsSection(base: DWord; pNTHeader: PImageNTHeaders);
var
  importDesc: PImageImportDescriptor;
  //header: PImageSectionHeader;
  thunk: PImageThunkData;
  pOrdinalName: PImageImportByName;
  //exportsStartRVA, exportsEndRVA: DWord;
  delta: Integer;
  pModuleName: PAnsiChar;
  {importSize,} importRva: DWord;
begin
  importRva := pNTHeader.OptionalHeader.DataDirectory[IMAGE_DIRECTORY_ENTRY_IMPORT].VirtualAddress;
//  importSize := pNTHeader.OptionalHeader.DataDirectory[IMAGE_DIRECTORY_ENTRY_IMPORT].Size;
//  header := GetSectionHeader('.idata', pNTHeader);
//  if not Assigned(header) then Exit;

  if importRva = 0 then Exit;

  importDesc := PImageImportDescriptor(GetPtrFromRva(importRva,pNTHeader,PByte(base), delta));
//  delta := Integer(header.VirtualAddress - header.PointerToRawData);

//  thunk := nil;

  repeat
    // See if we've reached an empty IMAGE_IMPORT_DESCRIPTOR
    if (importDesc.TimeDateStamp=0) and (importDesc.Name=0) then Break;
    pModuleName := PAnsiChar(Integer(importDesc.Name) - delta + Integer(base));

    if importDesc.Characteristics_OriginalFirstThunk = 0 then
      if importDesc.FirstThunk = 0 then Exit else thunk := PImageThunkData(importDesc.FirstThunk)
    else thunk := PImageThunkData(importDesc.Characteristics_OriginalFirstThunk);

    thunk := PImageThunkData(Integer(thunk) - delta + Integer(base));

    // If the pointer that thunk points to is outside of the .idata
    // section, it looks like this file is "pre-fixed up" with regards
    // to the thunk table.  In this situation, we'll need to fall back
    // to the hint-name (aka, the "Characteristics") table.
{    exportsStartRVA := importRva; //header.VirtualAddress;
    exportsEndRVA := exportsStartRVA + importSize;// header.SizeOfRawData;
    if (PDWORD(thunk)^ <= exportsStartRVA) or
       (PDWORD(thunk)^ >= exportsEndRVA) then
    begin
      if importDesc.Characteristics_OriginalFirstThunk = 0 then Exit; // Borland doesn't have this table!!!

      thunk := PImageThunkData(importDesc.Characteristics_OriginalFirstThunk);
      if (DWORD(thunk) <= exportsStartRVA) or
         (DWORD(thunk) >= exportsEndRVA) then Exit;

      thunk := PImageThunkData(DWord(thunk) - delta + base);
    end;
}
    repeat  // Loop forever (or until we break out)
      if thunk.AddressOfData = 0 then Break;

      if (thunk.Ordinal and IMAGE_ORDINAL_FLAG) <> 0 then
      begin
        FImports.Add(TDumpFileImport.Create(string(pModuleName), importDesc.TimeDateStamp, importDesc.ForwarderChain,  // I3310
          thunk.Ordinal and $FFFF));
      end
      else
      begin
        pOrdinalName := PImageImportByName(thunk.AddressOfData);
        pOrdinalName := PImageImportByName(Integer(pOrdinalName) - delta + Integer(base));
        FImports.Add(TDumpFileImport.Create(string(pModuleName), importDesc.TimeDateStamp, importDesc.ForwarderChain,  // I3310
          pOrdinalName.Hint, PChar(string(ansistring(PAnsiChar(@pOrdinalName.Name[0]))))));
      end;

      Inc(thunk);
    until 1 = 0;

    Inc(importDesc);
  until 1 = 0;
end;

constructor TDumpFile.Create(const AFileName: string);
begin
  FFileName := AFileName;
  fRaw := ExtractFileName(AFileName);
  FImports := TDumpFileImports.Create;
end;

destructor TDumpFile.Destroy;
begin
  FImports.Free;
  inherited Destroy;
end;

procedure TDumpfile.DumpExeFile(dosHeader: PImageDosHeader);
var
  pNTHeader: PImageNtHeaders;
  base: DWord;
begin
  base := DWord(dosHeader);

  pNtHeader := PImageNtHeaders(dosHeader._lfanew + Integer(base));

  // First, verify that the e_lfanew field gave us a reasonable
  // pointer, then verify the PE signature.
  if IsBadReadPtr(pNTHeader, sizeof(IMAGE_NT_HEADERS)) or
      (pNTHeader.Signature <> IMAGE_NT_SIGNATURE) then
    raise EDumpFile.Create('Unhandled EXE type, or invalid .EXE');

  DumpImportsSection(base, pNTHeader);
end;


{ TDumpFileImport }

constructor TDumpFileImport.Create(const AModuleName: string; ATimeDateStamp: DWord; AForwarderChain: DWord; AOrdinal: DWord; const AImportName: string = '');
begin
  inherited Create;
  FModuleName := AModuleName;
  FTimeDateStamp := ATimeDateStamp;
  FForwarderChain := AForwarderChain;
  FOrdinal := AOrdinal;
  FImportName := AImportName;
end;

{ TDumpFileImports }

function TDumpFileImports.GetItem(Index: Integer): TDumpFileImport;
begin
  Result := inherited GetItem(Index) as TDumpFileImport;
end;

function TDumpFileImports.IndexOf(const ModuleName, ImportName: string): Integer;
var
  i: Integer;
begin
  for i := 0 to Count - 1 do
    if (AnsiCompareText(Items[i].ModuleName, ModuleName) = 0) and
      (AnsiCompareText(Items[i].ImportName, ImportName) = 0) then
    begin
      Result := i;
      Exit;
    end;
  Result := -1;
end;

procedure TDumpFileImports.SetItem(Index: Integer;
  const Value: TDumpFileImport);
begin
  inherited SetItem(Index, Value);
end;

end.

