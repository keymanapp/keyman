unit Keyman.System.AddPdbToPe;

interface

uses
  System.AnsiStrings,
  System.Classes,
  System.SysUtils,
  Winapi.ImageHlp,
  Winapi.Windows;

function AddDebugID(s: TMemoryStream; guid: TGUID; var codeId: string): Boolean;

var
  AddPdbToPe_Debug: Boolean = False;

implementation

function RVA2Offset(nt: PImageNtHeaders; rva: DWORD): DWORD;
var
  section: PImageSectionHeader;
  sectionIndex: Integer;
begin
  section := PImageSectionHeader(PByte(nt) + sizeof(IMAGE_NT_HEADERS));

  for sectionIndex := 0 to nt.FileHeader.NumberOfSections - 1 do
  begin
    {
        Check if the RVA is within the virtual addressing space of the section
        Make sure the RVA is less than  the VirtualAddress plus its raw data size
        IMAGE_HEADER_SECTION.VirtualAddress = The address of the first byte of
            the section when loaded into memory, relative to the image base. For
            object files, this is the address of the first byte before
            relocation is applied.
        Our ImageBase is 0, since we aren't loaded into actual memory
    }
    if (rva >= section.VirtualAddress) and (rva < section.VirtualAddress + section.SizeOfRawData) then
    begin
      Exit(section.PointerToRawData + rva - section.VirtualAddress);
    end;
    Inc(section);
  end;

  Result := 0;
end;

type
  _CV_HEADER = record
    Signature: DWORD;
    Offset: DWORD;
  end;

  TCVHeader = _CV_HEADER;
  PCVHeader = ^TCVHeader;

  _CV_INFO_PDB70 = record
    CvSignature: array[0..3] of ansichar;
    Signature: TGUID;
    Age: DWORD;
    PdbFileName: array[0..1] of AnsiChar;
  end;

  TCVInfoPdb70 = _CV_INFO_PDB70;
  PCVInfoPdb70 = ^TCVInfoPdb70;

procedure debug_writeln(s: string);
begin
  if AddPdbToPe_Debug then
    writeln('DEBUG: '+s);
end;

procedure error_writeln(s: string);
begin
  writeln('ERROR: '+s);
end;

function AddDebugID(s: TMemoryStream; guid: TGUID; var codeId: string): Boolean;
var
  dh: PImageDosHeader;
  nh: PImageNtHeaders;
  i: Integer;
  a: DWord;
  pdd: PImageDebugDirectory;
  debugDirectoryEntryCount: DWORD;
  pc: PCVInfoPdb70;
  section: PImageSectionHeader;
  sectionIndex: Integer;
  FirstSection: PImageSectionHeader;
  SectionOffset: DWORD;
  LastSection: PImageSectionHeader;
  SectionAddress: DWORD;
  SectionRawSize: Cardinal;
  SectionVirtualSize: Cardinal;
begin
  dh := PImageDosHeader(s.Memory);
  if dh.e_magic <> IMAGE_DOS_SIGNATURE then
  begin
    error_writeln('File is not a valid image file.');
    Exit(False);
  end;

  debug_writeln('Offset to PE header: '+IntToStr(dh._lfanew));

  nh := PImageNtHeaders(PByte(s.Memory) + dh._lfanew);
  if nh.Signature <> IMAGE_NT_SIGNATURE then
  begin
    error_writeln('File is not a valid PE image file.');
    Exit(False);
  end;

  debug_writeln('PE header machine type: '+IntToStr(nh.FileHeader.Machine));
//  Exit;

  if AddPdbToPe_Debug then
  begin
    debug_writeln('Number of directory entries: '+IntToStr(nh.OptionalHeader.NumberOfRvaAndSizes));
    for i := 0 to nh.OptionalHeader.NumberOfRvaAndSizes - 1 do
    begin
      debug_writeln(Format('Directory[%02.2d] rva=%08.8x address=%08.8x size=%08.8x', [i, nh.OptionalHeader.DataDirectory[i].VirtualAddress, RVA2Offset(nh, nh.OptionalHeader.DataDirectory[i].VirtualAddress),
        nh.OptionalHeader.DataDirectory[i].Size]));
    end;

    debug_writeln('Number of sections: '+IntToStr(nh.FileHeader.NumberOfSections));
  end;

  section := PImageSectionHeader(PByte(nh) + sizeof(IMAGE_NT_HEADERS));
  LastSection := section;
  FirstSection := section;
  for sectionIndex := 0 to nh.FileHeader.NumberOfSections - 1 do
  begin
    if (section.PointerToRawData > 0) and ((section.PointerToRawData < FirstSection.PointerToRawData) or (FirstSection.PointerToRawData = 0)) then
      FirstSection := section;
    if section.PointerToRawData > LastSection.PointerToRawData then
      LastSection := section;
    debug_writeln(Format('Section[%02.2d] name=%-8.8s vs=%08.8x rva=%08.8x address=%08.8x size=%08.8x char=%x', [sectionIndex, PAnsiChar(@section.Name[0]), section.Misc.VirtualSize, section.VirtualAddress, section.PointerToRawData, section.SizeOfRawData, section.Characteristics]));
    Inc(section);
  end;

  debug_writeln(Format('End of section table is at %8.8x', [Integer(section) - Integer(s.Memory)]));
  debug_writeln(Format('One more section takes us to %8.8x', [Integer(section) - Integer(s.Memory) + sizeof(_IMAGE_SECTION_HEADER)]));

  if nh.OptionalHeader.DataDirectory[IMAGE_DIRECTORY_ENTRY_DEBUG].VirtualAddress <> 0 then
  begin
    // Let's look up the debug data for our own sanity
    error_writeln('This executable has a debug section. Not able to update this file.');
    if AddPdbToPe_Debug then
    begin
      a := RVA2Offset(nh, nh.OptionalHeader.DataDirectory[IMAGE_DIRECTORY_ENTRY_DEBUG].VirtualAddress);
      debug_writeln(Format('RVA = %x addr = %x', [nh.OptionalHeader.DataDirectory[IMAGE_DIRECTORY_ENTRY_DEBUG].VirtualAddress, a]));

      if nh.OptionalHeader.DataDirectory[IMAGE_DIRECTORY_ENTRY_DEBUG].Size < sizeof(TImageDebugDirectory) then
        // Delphi compiler bug! should be size in bytes not entries
        debugDirectoryEntryCount := nh.OptionalHeader.DataDirectory[IMAGE_DIRECTORY_ENTRY_DEBUG].Size
      else
        debugDirectoryEntryCount := nh.OptionalHeader.DataDirectory[IMAGE_DIRECTORY_ENTRY_DEBUG].Size div sizeof(TImageDebugDirectory);
      pdd := Pointer(PByte(s.Memory) + a);

      for i := 1 to debugDirectoryEntryCount do
      begin
        debug_writeln(Format('pdd = %x %x ver = %x minorver = %x type = %x size = %d addr = %x ptr = %x', [
          pdd.Characteristics, pdd.TimeDateStamp, pdd.MajorVersion, pdd.MinorVersion,
          pdd._Type, pdd.SizeOfData, pdd.AddressOfRawData, pdd.PointerToRawData]));
    //      FindDataDirectory(ms, nh, IMAGE_DIRECTORY_ENTRY_DEBUG);
        if pdd._Type = IMAGE_DEBUG_TYPE_CODEVIEW then
        begin
          // Let's process the codeview section
          pc := Pointer(PByte(s.Memory) + pdd.PointerToRawData);
          debug_writeln('Signature: '+pc.CvSignature);
          if pc.CvSignature = 'RSDS' then
          begin
            debug_writeln(Format('File Debug ID = %s-%d', [GuidToString(pc.Signature), pc.Age]));
            debug_writeln(Format('File PDB = %s', [PAnsiChar(@pc.PdbFileName[0])]));
          end;
        end;
        Inc(pdd);
      end;
    end;
    Exit(False);
  end;

  // Ensure we have space for an additional section header
  debug_writeln(Format('offset=%x', [DWORD(section) - DWORD(s.Memory)+sizeof(_IMAGE_SECTION_HEADER)]));
  debug_writeln(Format('first=%x', [FirstSection.PointerToRawData]));
  if DWORD(section) - DWORD(s.Memory) + sizeof(_IMAGE_SECTION_HEADER) > FirstSection.PointerToRawData then
  begin
    error_writeln('We don''t have space in the file header for a new section. Not going to try to relocate.');
    Exit(False);
  end;

  // Calculate some new constants
  SectionRawSize := nh.OptionalHeader.FileAlignment;
  SectionVirtualSize := nh.OptionalHeader.SectionAlignment;
  SectionOffset := s.Size;
  if (SectionOffset mod SectionRawSize) <> 0 then
    Inc(SectionOffset, SectionRawSize - (SectionOffset mod SectionRawSize));

  SectionAddress := LastSection.VirtualAddress + LastSection.Misc.VirtualSize;
  if (SectionAddress mod SectionVirtualSize) <> 0 then
    Inc(SectionAddress, SectionVirtualSize - (SectionAddress mod SectionVirtualSize));

  // Allocate a new section for the debug directory
  s.SetSize(SectionOffset + SectionRawSize);
  FillChar((PByte(s.Memory) + SectionOffset)^, SectionRawSize, 0);

  // Increment the number of sections
  Inc(nh.FileHeader.NumberOfSections);

  // Update the file headers
  Inc(nh.OptionalHeader.SizeOfImage, SectionVirtualSize);
  Inc(nh.OptionalHeader.SizeOfInitializedData, SectionVirtualSize);

  // Setup the debug directory
  nh.OptionalHeader.DataDirectory[IMAGE_DIRECTORY_ENTRY_DEBUG].VirtualAddress := SectionAddress;
  nh.OptionalHeader.DataDirectory[IMAGE_DIRECTORY_ENTRY_DEBUG].Size := sizeof(TImageDebugDirectory);

  // Fill in the section header
  System.AnsiStrings.StrCopy(PAnsiChar(@section.Name[0]), '.debug');
  section.Misc.VirtualSize := SectionVirtualSize;
  section.VirtualAddress := SectionAddress;
  section.SizeOfRawData := SectionRawSize;
  section.PointerToRawData := SectionOffset;
  section.PointerToRelocations := 0;
  section.PointerToLinenumbers := 0;
  section.NumberOfRelocations := 0;
  section.NumberOfLinenumbers := 0;
  section.Characteristics := IMAGE_SCN_MEM_READ or IMAGE_SCN_CNT_INITIALIZED_DATA; //$40000040;

  // Fill in the debug directory entry
  pdd := Pointer(PByte(s.Memory) + SectionOffset);
  pdd.Characteristics := 0;
  pdd.TimeDateStamp := nh.FileHeader.TimeDateStamp;
  pdd.MajorVersion := 0;
  pdd.MinorVersion := 0;
  pdd._Type := 2; // CODEVIEW
  pdd.SizeOfData := sizeof(_CV_INFO_PDB70) - 2 + Length('null.pdb') + 1; // todo consider pdb filename?
  pdd.AddressOfRawData := SectionAddress + sizeof(TImageDebugDirectory);
  pdd.PointerToRawData := SectionOffset + sizeof(TImageDebugDirectory);

  // Fill in the codeview header
  pc := Pointer(PByte(s.Memory) + pdd.PointerToRawData);
  pc.CvSignature := 'RSDS';
  pc.Signature := guid;
  pc.Age := 1;
  System.AnsiStrings.StrCopy(PAnsiChar(@pc.PdbFileName[0]), 'null.pdb');

  codeId := IntToHex(nh.FileHeader.TimeDateStamp, 8) + IntToHex(nh.OptionalHeader.SizeOfImage, 1);

  debug_writeln(Format('Success. Added GUID %s, file age 1 to file', [GuidToString(guid)]));

  Result := True;
end;

end.
