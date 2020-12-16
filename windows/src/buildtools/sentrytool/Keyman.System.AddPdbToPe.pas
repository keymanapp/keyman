unit Keyman.System.AddPdbToPe;

interface

uses
  System.AnsiStrings,
  System.Classes,
  System.SysUtils,
  Winapi.ImageHlp,
  Winapi.Windows;

function AddDebugID(s: TMemoryStream; guid: TGUID; var codeId: string; var isAmd64: Boolean): Boolean;

var
  AddPdbToPe_Debug: Boolean = False;

implementation

procedure debug_writeln(s: string);
begin
  if AddPdbToPe_Debug then
    writeln('DEBUG: '+s);
end;

procedure error_writeln(s: string);
begin
  writeln('ERROR: '+s);
end;

function RVA2Offset(nt: PImageNtHeaders; header_size: Integer; rva: DWORD): DWORD;
var
  section: PImageSectionHeader;
  sectionIndex: Integer;
begin
  if rva = 0 then
    Exit(0);

  section := PImageSectionHeader(PByte(nt) + header_size);

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

  error_writeln(Format('unexpected RVA out of bounds: %x', [rva]));
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

  TImageDataDirectoryArray = packed array[0..IMAGE_NUMBEROF_DIRECTORY_ENTRIES-1] of TImageDataDirectory;
  PImageDataDirectoryArray = ^TImageDataDirectoryArray;

function AddDebugID(s: TMemoryStream; guid: TGUID; var codeId: string; var isAmd64: Boolean): Boolean;
var
  dh: PImageDosHeader;
  nh32: PImageNtHeaders32;
  nh64: PImageNtHeaders64;
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
  NumberOfRvaAndSizes: DWORD;
  DataDirectory: PImageDataDirectoryArray;
  ImageHeaderSize: Integer;
begin
  //
  // Parse the .exe file to find the appropriate sections
  //
  dh := PImageDosHeader(s.Memory);
  if dh.e_magic <> IMAGE_DOS_SIGNATURE then
  begin
    error_writeln('File is not a valid image file.');
    Exit(False);
  end;

  debug_writeln('Offset to PE header: '+IntToStr(dh._lfanew));

  nh32 := PImageNtHeaders(PByte(s.Memory) + dh._lfanew);
  if nh32.Signature <> IMAGE_NT_SIGNATURE then
  begin
    error_writeln('File is not a valid PE image file.');
    Exit(False);
  end;

  debug_writeln('PE header machine type: 0x'+IntToHex(nh32.FileHeader.Machine, 4));
  if nh32.FileHeader.Machine = IMAGE_FILE_MACHINE_AMD64 then
    isAmd64 := True
  else if nh32.FileHeader.Machine = IMAGE_FILE_MACHINE_I386 then
    isAmd64 := False
  else
  begin
    error_writeln('File is not a x86 or x64 image (0x'+IntToHex(nh32.FileHeader.Machine, 4)+')');
    Exit(False);
  end;

  if nh32.OptionalHeader.Magic = IMAGE_NT_OPTIONAL_HDR32_MAGIC then
  begin
    nh64 := nil;
    NumberOfRvaAndSizes := nh32.OptionalHeader.NumberOfRvaAndSizes;
    DataDirectory := @nh32.OptionalHeader.DataDirectory;
    ImageHeaderSize := sizeof(TImageNtHeaders32);
  end
  else if nh32.OptionalHeader.Magic = IMAGE_NT_OPTIONAL_HDR64_MAGIC then
  begin
    nh64 := PImageNtHeaders64(nh32);
    NumberOfRvaAndSizes := nh64.OptionalHeader.NumberOfRvaAndSizes;
    DataDirectory := @nh64.OptionalHeader.DataDirectory;
    ImageHeaderSize := sizeof(TImageNtHeaders64);
  end
  else
  begin
    error_writeln('File is neither a PE32 or a PE32+ (x64) file');
    Exit(False);
  end;

  if AddPdbToPe_Debug then
  begin
    debug_writeln('Number of directory entries: '+IntToStr(NumberOfRvaAndSizes));
    for i := 0 to NumberOfRvaAndSizes - 1 do
    begin
      debug_writeln(Format('Directory[%02.2d] rva=%08.8x address=%08.8x size=%08.8x', [i, DataDirectory[i].VirtualAddress, RVA2Offset(nh32, ImageHeaderSize, DataDirectory[i].VirtualAddress),
        DataDirectory[i].Size]));
    end;

    debug_writeln('Number of sections: '+IntToStr(nh32.FileHeader.NumberOfSections));
  end;

  // Check section list to find address of last section
  section := PImageSectionHeader(PByte(nh32) + ImageHeaderSize);
  LastSection := section;
  FirstSection := section;
  for sectionIndex := 0 to nh32.FileHeader.NumberOfSections - 1 do
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

  //
  // Check that we don't already have a debug directory entry
  //

  if DataDirectory[IMAGE_DIRECTORY_ENTRY_DEBUG].VirtualAddress <> 0 then
  begin
    // Let's look up the debug data for our own sanity
    error_writeln('This executable has a debug section. Not able to update this file.');
    if AddPdbToPe_Debug then
    begin
      a := RVA2Offset(nh32, ImageHeaderSize, DataDirectory[IMAGE_DIRECTORY_ENTRY_DEBUG].VirtualAddress);
      debug_writeln(Format('RVA = %x addr = %x', [DataDirectory[IMAGE_DIRECTORY_ENTRY_DEBUG].VirtualAddress, a]));

      if DataDirectory[IMAGE_DIRECTORY_ENTRY_DEBUG].Size < sizeof(TImageDebugDirectory) then
        // Delphi compiler bug! should be size in bytes not entries
        debugDirectoryEntryCount := DataDirectory[IMAGE_DIRECTORY_ENTRY_DEBUG].Size
      else
        debugDirectoryEntryCount := DataDirectory[IMAGE_DIRECTORY_ENTRY_DEBUG].Size div sizeof(TImageDebugDirectory);
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

  //
  // Ensure we have space for an additional section header
  //

  debug_writeln(Format('offset=%x', [DWORD(section) - DWORD(s.Memory)+sizeof(_IMAGE_SECTION_HEADER)]));
  debug_writeln(Format('first=%x', [FirstSection.PointerToRawData]));
  if DWORD(section) - DWORD(s.Memory) + sizeof(_IMAGE_SECTION_HEADER) > FirstSection.PointerToRawData then
  begin
    error_writeln('We don''t have space in the file header for a new section. Not going to try to relocate.');
    Exit(False);
  end;

  //
  // Okay, everything looks good, so let's figure out where to put the new data.
  // We want to write a section at the end of the file and then update the debug
  // data directory entry to point to this new section.
  //

  // Calculate some new constants: SectionRawSize is never going to be smaller
  // than the size of the debug data, which is:
  // `sizeof(_CV_INFO_PDB70) - 2 + Length('null.pdb') + 1`, that is, 35 bytes.
  // The minimum `FileAlignment` is:
  // [512 bytes](https://docs.microsoft.com/en-us/windows/win32/debug/pe-format#file-headers).

  if nh64 = nil then
  begin
    SectionRawSize := nh32.OptionalHeader.FileAlignment;
    SectionVirtualSize := nh32.OptionalHeader.SectionAlignment;
    SectionOffset := s.Size;
    if (SectionOffset mod SectionRawSize) <> 0 then
      Inc(SectionOffset, SectionRawSize - (SectionOffset mod SectionRawSize));

    SectionAddress := LastSection.VirtualAddress + LastSection.Misc.VirtualSize;
    if (SectionAddress mod SectionVirtualSize) <> 0 then
      Inc(SectionAddress, SectionVirtualSize - (SectionAddress mod SectionVirtualSize));

    // Allocate a new section for the debug directory
    s.SetSize(SectionOffset + SectionRawSize);
    FillChar((PByte(s.Memory) + SectionOffset)^, SectionRawSize, 0);

    nh32 := PImageNtHeaders(NativeUInt(nh32)-NativeUInt(dh)+NativeUInt(s.Memory));
    section := PImageSectionHeader(NativeUInt(section)-NativeUInt(dh)+NativeUInt(s.Memory));

    // Increment the number of sections
    Inc(nh32.FileHeader.NumberOfSections);

    // Update the file headers
    Inc(nh32.OptionalHeader.SizeOfImage, SectionVirtualSize);
    Inc(nh32.OptionalHeader.SizeOfInitializedData, SectionVirtualSize);

    // Setup the debug directory
    nh32.OptionalHeader.DataDirectory[IMAGE_DIRECTORY_ENTRY_DEBUG].VirtualAddress := SectionAddress;
    nh32.OptionalHeader.DataDirectory[IMAGE_DIRECTORY_ENTRY_DEBUG].Size := sizeof(TImageDebugDirectory);

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
    pdd.TimeDateStamp := nh32.FileHeader.TimeDateStamp;
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

    codeId := IntToHex(nh32.FileHeader.TimeDateStamp, 8) + IntToHex(nh32.OptionalHeader.SizeOfImage, 1);
  end
  else
  begin
    SectionRawSize := nh64.OptionalHeader.FileAlignment;
    SectionVirtualSize := nh64.OptionalHeader.SectionAlignment;
    SectionOffset := s.Size;
    if (SectionOffset mod SectionRawSize) <> 0 then
      Inc(SectionOffset, SectionRawSize - (SectionOffset mod SectionRawSize));

    SectionAddress := LastSection.VirtualAddress + LastSection.Misc.VirtualSize;
    if (SectionAddress mod SectionVirtualSize) <> 0 then
      Inc(SectionAddress, SectionVirtualSize - (SectionAddress mod SectionVirtualSize));

    // Allocate a new section for the debug directory
    s.SetSize(SectionOffset + SectionRawSize);
    FillChar((PByte(s.Memory) + SectionOffset)^, SectionRawSize, 0);

    nh64 := PImageNtHeaders64(NativeUInt(nh64)-NativeUInt(dh)+NativeUInt(s.Memory));
    section := PImageSectionHeader(NativeUInt(section)-NativeUInt(dh)+NativeUInt(s.Memory));

    // Increment the number of sections
    Inc(nh64.FileHeader.NumberOfSections);

    // Update the file headers
    Inc(nh64.OptionalHeader.SizeOfImage, SectionVirtualSize);
    Inc(nh64.OptionalHeader.SizeOfInitializedData, SectionVirtualSize);

    // Setup the debug directory
    nh64.OptionalHeader.DataDirectory[IMAGE_DIRECTORY_ENTRY_DEBUG].VirtualAddress := SectionAddress;
    nh64.OptionalHeader.DataDirectory[IMAGE_DIRECTORY_ENTRY_DEBUG].Size := sizeof(TImageDebugDirectory);

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
    pdd.TimeDateStamp := nh64.FileHeader.TimeDateStamp;
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

    codeId := IntToHex(nh64.FileHeader.TimeDateStamp, 8) + IntToHex(nh64.OptionalHeader.SizeOfImage, 1);
  end;

  debug_writeln(Format('Success. Added GUID %s, file age 1 to file', [GuidToString(guid)]));

  Result := True;
end;

end.
