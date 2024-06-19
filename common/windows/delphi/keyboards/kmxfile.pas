(*
  Name:             kmxfile
  Copyright:        Copyright (C) SIL International.
  Documentation:
  Description:
  Create Date:      1 Aug 2006

  Modified Date:    4 Nov 2014
  Authors:          mcdurdin
  Related Files:
  Dependencies:

  Bugs:
  Todo:
  Notes:
  History:          01 Aug 2006 - mcdurdin - Fix packing of TKeyboardFileKey
                    23 Aug 2006 - mcdurdin - Add TSS_VISUALKEYBOARD, TSS_KMW_RTL, TSS_KMWHELPFILE, TSS_KMW_HELPTEXT, TSS_KMWEMBEDJS
                    23 Aug 2006 - mcdurdin - Add TSS_MNEMONIC, TSS_INCLUDECODES, TSS_OLDCHARPOSMATCHING, TSS_KEYMANCOPYRIGHT
                    30 Aug 2006 - mcdurdin - Check for version 7 of keyboard files in debugger
                    19 Mar 2007 - mcdurdin - Add KMX_VIRTUALCHARKEY constant
                    27 Mar 2008 - mcdurdin - I1358 - Add TSS_WINDOWSLANGUAGES
                    14 Jun 2008 - mcdurdin - Add ssWindowsLanguages
                    26 Jul 2010 - mcdurdin - I2467 - 8.0 renumber
                    04 May 2012 - mcdurdin - I3310 - V9.0 - Unicode in Delphi fixes
                    17 Aug 2012 - mcdurdin - I3377 - KM9 - Update code references from 8.0 to 9.0
                    27 Aug 2012 - mcdurdin - I3430 - V9.0 - Add support for if(&platform) and if(&baselayout) to compilers
                    27 Aug 2012 - mcdurdin - I3437 - V9.0 - Add support for set(&layer) and layer()
                    27 Aug 2012 - mcdurdin - I3438 - V9.0 - Add support for custom virtual keys
                    24 Oct 2012 - mcdurdin - I3483 - V9.0 - Add support for compiling in the layout file
                    17 Jan 2013 - mcdurdin - I3763 - V9.0 - GetLanguageName refactor
                    19 Mar 2014 - mcdurdin - I4140 - V9.0 - Add keyboard version information to keyboards
                    28 May 2014 - mcdurdin - I3757 - V9.0 - Merge kmxfile in engine/kmcomapi/util and global/delphi/general
                    12 Aug 2014 - mcdurdin - I4368 - V9.0 - Add custom stylesheet to kmw compile
                    04 Nov 2014 - mcdurdin - I4504 - V9.0 - Consolidate the compile action into single command
*)
unit kmxfile;   // I3757

interface

uses
  System.Classes,
  System.SysUtils,
  Vcl.Graphics,
  Winapi.Windows,
  kmxfileconsts;

type
  EKMXError = class(Exception)
  private
    FErrorCode: Integer;
  public
    constructor Create(AErrorCode: Integer; const Message: string);
    constructor CreateFmt(AErrorCode: Integer; const Message: string; const Args: array of const);
    property ErrorCode: Integer read FErrorCode;
  end;

const
  EKMX_OldKeyboardFile = 1;
  EKMX_NewKeyboardFile = 2;
  EKMX_InvalidKeyboardFile = 3;

type
  TKIEncodings = set of (keANSI, keUnicode);

  TKeyboardInfo = record
    KeyboardID: Dword;    // Windows language
    FileVersion: Dword;
    KeyboardName: WideString;
    CopyrightString, MessageString: WideString;
    DefaultHotkey: Dword;
    //LanguageID: DWord;
    LogicalLayout: Boolean;
    Encodings: TKIEncodings;
    IsRegistered: Boolean;
    Bitmap: Vcl.Graphics.TBitmap;
    Icon: TIcon;
    MemoryDump: TMemoryStream;
    MnemonicLayout: Boolean;
    KMW_RTL: Boolean;
    WindowsLanguages: WideString;
    ISO6393Languages: WideString;
    KeyboardVersion: WideString;   // I4136
    Targets: WideString;
    function FileVersionAsString: string;
  end;

  PKeyboardInfo = ^TKeyboardInfo;


function SystemStoreFromName(Name: WideString): TSystemStore;

function GetSystemStore(Memory: PByte; SystemID: DWord; var Buffer: WideString): Boolean;  // I3310
procedure GetKeyboardInfo(const FileName: string; FReturnDump: Boolean; var ki: TKeyboardInfo; FReturnBitmap: Boolean = False);

function GetLanguageCodesFromKeyboard(ki: TKeyboardInfo): TArray<Integer>;

function PRIMARYLANGID(n: DWORD): WORD;
function SUBLANGID(n: DWORD): WORD;

{
  These sanity checks help ensure we don't
  break on-disk struct sizes when we cross
  compilers, bitness and platforms. They must
  correspond to the equivalent constants in
  $(KEYMAN_ROOT)\common\windows\cpp\include\legacy_kmx_file.h
}
const
  KEYBOARDFILEHEADER_SIZE = 64;
  KEYBOARDFILESTORE_SIZE = 12;
  KEYBOARDFILEGROUP_SIZE = 24;
  KEYBOARDFILEKEY_SIZE = 20;

type
  TKeyboardFileHeader = packed record
    dwIdentifier: Dword;    // Keyman compiled keyboard id
    dwFileVersion: Dword;   // 0x0400 or 0x0500
    dwCheckSum: Dword;       // As stored in keyboard. DEPRECATED as of 16.0
    KeyboardID: DWord;       // as stored in HKEY_LOCAL_MACHINE//system//currentcontrolset//control//keyboard layouts
    IsRegistered: DWord;        // was layout id;
    version: DWord;         // keyboard file version with VERSION keyword
    cxStoreArray, cxGroupArray: Integer;
    dpStoreArray, dpGroupArray: DWord;
    StartGroupANSI, StartGroupUnicode: Integer; //Dword;   // -1 if not relevant type
    dwFlags: Dword;          // Flags for the keyboard file
    Hotkey: Dword;          // standard windows hotkey (hiword=shift/ctrl/alt stuff, loword=vkey)
    //dpName: Dword;
    //dpLanguageName: Dword;
    //dpCopyright: Dword;
    //dpMessage: Dword;
  	dwBitmapOffset, dwBitmapSize: DWord;
  end;

  PKeyboardFileHeader = ^TKeyboardFileHeader;

{$IF SizeOf(TKeyboardFileHeader) <> KEYBOARDFILEHEADER_SIZE}
  {$MESSAGE ERROR 'SizeOf(TKeyboardFileHeader) must be KEYBOARDFILEHEADER_SIZE bytes'}
{$ENDIF}

  TKeyboardFileStore = packed record
    dwSystemID: DWORD;
	  dpName: DWORD;
	  dpString: DWORD;
  end;

  PKeyboardFileStore = ^TKeyboardFileStore;

{$IF SizeOf(TKeyboardFileStore) <> KEYBOARDFILESTORE_SIZE}
  {$MESSAGE ERROR 'SizeOf(TKeyboardFileStore) must be KEYBOARDFILESTORE_SIZE bytes'}
{$ENDIF}

  TKeyboardFileGroup = packed record
	  dpName: DWORD;
    dpKeyArray: DWORD;
    dpMatch, dpNoMatch: DWORD;
    cxKeyArray: DWORD;
    fUsingKeys: BOOL;
  end;

  PKeyboardFileGroup = ^TKeyboardFileGroup;

{$IF SizeOf(TKeyboardFileGroup) <> KEYBOARDFILEGROUP_SIZE}
  {$MESSAGE ERROR 'SizeOf(TKeyboardFileGroup) must be KEYBOARDFILEGROUP_SIZE bytes'}
{$ENDIF}

  TKeyboardFileKey = packed record
    Key: WORD; packing: WORD;
    Line: DWORD;
    ShiftFlags: DWORD;
    dpOutput: DWORD;
    dpContext: DWORD;
  end;

  PKeyboardFileKey = ^TKeyboardFileKey;

{$IF SizeOf(TKeyboardFileKey) <> KEYBOARDFILEKEY_SIZE}
  {$MESSAGE ERROR 'SizeOf(TKeyboardFileKey) must be KEYBOARDFILEKEY_SIZE bytes'}
{$ENDIF}

function EncodingsAsString(ke: TKIEncodings): string;
function HotkeyAsString(hotkey: DWord): string;

implementation

uses
  KeyNames,
  utildir,
  utilfiletypes;

const
    FILEID_COMPILED	= $5354584B;

const
    KF_SHIFTFREESCAPS = $0001;
    KF_CAPSONONLY =     $0002;
    KF_CAPSALWAYSOFF =  $0004;
    KF_LOGICALLAYOUT =  $0008;
    KF_AUTOMATICVERSION = $0010;

    // 16.0: Support for LDML Keyboards in KMXPlus file format
    KF_KMXPLUS =        $0020;

function GetSystemStore(Memory: PByte; SystemID: DWord; var Buffer: WideString): Boolean;  // I3310
var
  pch: PByte;
  kfs: PKeyboardFileStore;
  kfh: PKeyboardFileHeader;
  i: Integer;
begin
  kfh := PKeyboardFileHeader(Memory);
  pch := Memory;
  Inc(pch, kfh.dpStoreArray);
  for i := 0 to kfh.cxStoreArray - 1 do
  begin
    kfs := PKeyboardFileStore(pch);
    if kfs.dwSystemID = SystemID then
    begin
      pch := PByte(Memory);  // I3310
      Inc(pch, kfs.dpString);
      Buffer := PWideChar(pch);
      Result := True;
      Exit;
    end;
    Inc(pch, SizeOf(TKeyboardFileStore));
  end;
  Buffer := '';
  Result := False;
end;

procedure GetKeyboardInfo(const FileName: string; FReturnDump: Boolean; var ki: TKeyboardInfo; FReturnBitmap: Boolean = False);
var
  kfh: TKeyboardFileHeader;
  mem: TMemoryStream;
  ver: WideString;
  shver: string;
  srtl, smnemonic: WideString;
  signature: Integer;
  bmpsig: array[0..2] of ansichar;  // I3310
begin
  if not IsKeyboardFile(FileName) then
    raise EKMXError.CreateFmt(EKMX_InvalidKeyboardFile, 'The keyboard file %0:s is invalid', [ExtractFileName(FileName)]);

  mem := TMemoryStream.Create;
  with mem do
  try
    LoadFromFile(FileName);

    Read(signature, 4);
    if signature <> FILEID_COMPILED then
      raise EKMXError.CreateFmt(EKMX_InvalidKeyboardFile, 'The keyboard file %0:s is invalid', [ExtractFileName(FileName)]);

    Position := 0;
    Read(kfh, SizeOf(TKeyboardFileHeader));

    if kfh.dwIdentifier <> FILEID_COMPILED then
      raise EKMXError.CreateFmt(EKMX_InvalidKeyboardFile, 'The keyboard file %0:s is invalid', [ExtractFileName(FileName)]);

    if (kfh.dwFileVersion >= VERSION_50) and (kfh.dwFileVersion <= VERSION_MAX) then  // I3377
    begin
      ki.FileVersion := kfh.dwFileVersion;
      GetSystemStore(Memory, TSS_NAME, ki.KeyboardName);
      GetSystemStore(Memory, TSS_COPYRIGHT, ki.CopyrightString);
      GetSystemStore(Memory, TSS_MESSAGE, ki.MessageString);

      GetSystemStore(Memory, TSS_WINDOWSLANGUAGES, ki.WindowsLanguages);
      GetSystemStore(Memory, TSS_ETHNOLOGUECODE, ki.ISO6393Languages);
      GetSystemStore(Memory, TSS_TARGETS, ki.Targets);

      if not GetSystemStore(Memory, TSS_KEYBOARDVERSION, ki.KeyboardVersion) then   // I4136
        ki.KeyboardVersion := '1.0';

      ki.MnemonicLayout := GetSystemStore(Memory, TSS_MNEMONIC, smnemonic) and (smnemonic <> '0');

      ki.KMW_RTL := GetSystemStore(Memory, TSS_KMW_RTL, srtl) and (srtl <> '0');

      ki.KeyboardID := kfh.KeyboardID;
      ki.DefaultHotKey := kfh.HotKey;
      ki.LogicalLayout := (kfh.dwFlags and KF_LOGICALLAYOUT) = KF_LOGICALLAYOUT;
      ki.Encodings := [];
      if (kfh.StartGroupANSI >= 0) and (kfh.StartGroupANSI < kfh.cxGroupArray) then
        Include(ki.Encodings, keANSI);
      if (kfh.StartGroupUnicode >= 0) and (kfh.StartGroupUnicode < kfh.cxGroupArray) then
        Include(ki.Encodings, keUnicode);

      ki.IsRegistered := kfh.IsRegistered = 1;

      if (ki.KeyboardID <> 0) and (Pos('x'+IntToHex(ki.KeyboardID, 4), string(ki.WindowsLanguages)) = 0) then
        ki.WindowsLanguages := Trim('x'+IntToHex(ki.KeyboardID, 4) + ' ' + ki.WindowsLanguages);

      ki.Icon := nil;
      ki.Bitmap := nil;
      if FReturnBitmap then
      begin
        if (kfh.dwBitmapOffset > 0) and (kfh.dwBitmapSize > 0) then
        begin
          mem.Position := kfh.dwBitmapOffset;
          mem.Read(bmpsig[0], 2);
          bmpsig[2] := #0;
          mem.Position := kfh.dwBitmapOffset;
          if bmpsig = 'BM' then
          begin
            ki.Bitmap := Vcl.Graphics.TBitmap.Create;
            ki.Bitmap.LoadFromStream(mem);
            if ki.Bitmap.Width = 0 then FreeAndNil(ki.Bitmap);
          end
          else
          begin
            ki.Icon := TIcon.Create;
            ki.Icon.LoadFromStream(mem);
          end;
        end;
      end;
    end
    else if kfh.dwFileVersion = VERSION_40 then
      raise EKMXError.Create(EKMX_OldKeyboardFile,
        'The keyboard file is from version 4.0 and is not supported in this version of Keyman.')
    else if (kfh.dwFileVersion > VERSION_60) and (HIWORD(kfh.dwFileVersion) = 0) then
    begin
      if GetSystemStore(Memory, TSS_COMPILEDVERSION, ver) then
      begin
        shver := ver;
        raise EKMXError.CreateFmt(EKMX_NewKeyboardFile,
          'The keyboard file was created with version %s of Keyman Developer '+
          'and is not supported in this version of Keyman.', [shver]);
      end
      else
        raise EKMXError.CreateFmt(EKMX_NewKeyboardFile,
          'The keyboard file was created with version %x of Keyman Developer '+
          'and is not supported in this version of Keyman.', [kfh.dwFileVersion]);
    end
    else
      raise EKMXError.CreateFmt(EKMX_InvalidKeyboardFile, 'The keyboard file %0:s is invalid',
        [ExtractFileName(FileName)]);

    if FReturnDump then
    begin
      ki.MemoryDump := TMemoryStream.Create;
      mem.Position := 0;
      mem.SaveToStream(ki.MemoryDump);
    end;
  finally
    Free;
  end;
end;

function PRIMARYLANGID(n: DWORD): WORD;
begin
  Result := WORD(n AND $3FF);
end;

function SUBLANGID(n: DWORD): WORD;
begin
  Result := WORD(n shr 10);
end;

function EncodingsAsString(ke: TKIEncodings): string;
var
  s: string;
begin
  if keANSI in ke then
  begin
    s := 'Code Page/Custom';
    if keUnicode in ke then s := s + ', Unicode';
  end
  else if keUnicode in ke then
    s := 'Unicode'
  else
    s := '';
  Result := s;
end;

function HotkeyAsString(hotkey: DWord): string;
var
  s: string;
begin
  s := '';
  if LoByte(hotkey) <> 0 then
  begin
    if (hotkey and HK_SHIFT) = HK_SHIFT then s := s + 'Shift+';
    if (hotkey and HK_CTRL) = HK_CTRL   then s := s + 'Ctrl+';
    if (hotkey and HK_ALT) = HK_ALT     then s := s + 'Alt+';
    s := s + SKeyNames[LoByte(hotkey)];
  end;

  Result := s;
end;

{ EKMXError }

constructor EKMXError.Create(AErrorCode: Integer; const Message: string);
begin
  FErrorCode := AErrorCode;
  inherited Create(Message);
end;

constructor EKMXError.CreateFmt(AErrorCode: Integer; const Message: string;
  const Args: array of const);
begin
  FErrorCode := AErrorCode;
  inherited CreateFmt(Message, Args);
end;

function SystemStoreFromName(Name: WideString): TSystemStore;
var
  i: TSystemStore;
begin
  if Copy(Name,1,1) = '&' then Delete(Name, 1, 1);
    for i := Low(i) to High(i) do
      if WideSameText(SystemStoreNames[i], Name) then
      begin
        Result := i;
        Exit;
      end;
  Result := ssNone;
end;


function GetLanguageCodesFromKeyboard(ki: TKeyboardInfo): TArray<Integer>;
  procedure AddLanguage(FLanguageID: Integer);
  var
    i: Integer;
  begin
    for i := 0 to High(Result) do
      if Result[i] = FLanguageID then
        Exit;

    SetLength(Result, Length(Result)+1);
    Result[High(Result)] := FLanguageID;
  end;

var
  FAllLanguagesW: WideString;
  FAllLanguages: TArray<string>;
  FLanguage: string;
  FLanguageID: Integer;
begin
  //
  // Use keyboard-defined default profile first   // I4607
  //
  if ki.KeyboardID <> 0 then
  begin
    AddLanguage(ki.KeyboardID);
  end;

  //
  // Then enumerate all defined languages to try next   // I4607
  //
  GetSystemStore(ki.MemoryDump.Memory, TSS_WINDOWSLANGUAGES, FAllLanguagesW);

  FAllLanguages := string(FAllLanguagesW).Split([' ']);
  for FLanguage in FAllLanguages do
  begin
    // Each language code starts with `x` so ignore that
    if TryStrToInt('$'+FLanguage.Substring(1), FLanguageID) then
    begin
      AddLanguage(FLanguageID);
    end;
  end;
end;

{ TKeyboardInfo }

function TKeyboardInfo.FileVersionAsString: string;
begin
  Result := Format('%d.%d', [(FileVersion and VERSION_MASK_MAJOR) shr 8, FileVersion and VERSION_MASK_MINOR]);
end;

end.

