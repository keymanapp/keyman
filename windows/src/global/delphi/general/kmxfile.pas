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
  Winapi.Windows;

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
    ProductID: Integer;
    MnemonicLayout: Boolean;
    WindowsLanguages: WideString;
    BCP47Languages: WideString;
    KeyboardVersion: WideString;   // I4136
  end;

  PKeyboardInfo = ^TKeyboardInfo;

const
    HK_ALT   = $00010000;
    HK_CTRL	 = $00020000;
    HK_SHIFT = $00040000;

const
  KMX_LCTRLFLAG      = $0001;	// Left Control flag
  KMX_RCTRLFLAG      = $0002;	// Right Control flag
  KMX_LALTFLAG       = $0004;	// Left Alt flag
  KMX_RALTFLAG       = $0008;	// Right Alt flag
  KMX_SHIFTFLAG      = $0010;	// Either shift flag
  KMX_CTRLFLAG       = $0020;	// Either ctrl flag
  KMX_ALTFLAG        = $0040;	// Either alt flag
  KMX_CAPITALFLAG    = $0100;	// Caps lock on
  KMX_NOTCAPITALFLAG = $0200;	// Caps lock NOT on
  KMX_NUMLOCKFLAG    = $0400;	// Num lock on
  KMX_NOTNUMLOCKFLAG = $0800;	// Num lock NOT on
  KMX_SCROLLFLAG     = $1000;	// Scroll lock on
  KMX_NOTSCROLLFLAG  = $2000;	// Scroll lock NOT on
  KMX_ISVIRTUALKEY   = $4000;	// It is a Virtual Key Sequence
  KMX_VIRTUALCHARKEY = $8000; // It is a virtual character key sequence - mnemonic layouts 

  // Combinations of key masks
  KMX_MASK_MODIFIER_CHIRAL = KMX_LCTRLFLAG or KMX_RCTRLFLAG or KMX_LALTFLAG or KMX_RALTFLAG;
  KMX_MASK_MODIFIER_SHIFT = KMX_SHIFTFLAG;
  KMX_MASK_MODIFIER_NONCHIRAL = KMX_CTRLFLAG or KMX_ALTFLAG;
  KMX_MASK_STATEKEY = KMX_CAPITALFLAG or KMX_NOTCAPITALFLAG or
                      KMX_NUMLOCKFLAG or KMX_NOTNUMLOCKFLAG or
                      KMX_SCROLLFLAG or KMX_NOTSCROLLFLAG;
  KMX_MASK_KEYTYPE  = KMX_ISVIRTUALKEY or KMX_VIRTUALCHARKEY;

  KMX_MASK_MODIFIER = KMX_MASK_MODIFIER_CHIRAL or KMX_MASK_MODIFIER_SHIFT or KMX_MASK_MODIFIER_NONCHIRAL;
  KMX_MASK_KEYS     = KMX_MASK_MODIFIER or KMX_MASK_STATEKEY;
  KMX_MASK_VALID    = KMX_MASK_KEYS or KMX_MASK_KEYTYPE;

const
  TSS_NONE                = 0;
  TSS_BITMAP              = 1;
  TSS_COPYRIGHT           = 2;
  TSS_HOTKEY              = 3;
  TSS_LANGUAGE            = 4;
  TSS_LAYOUT              = 5;
  TSS_MESSAGE             = 6;
  TSS_NAME                = 7;
  TSS_VERSION             = 8;
  TSS_CAPSONONLY          = 9;
  TSS_CAPSALWAYSOFF       = 10;
  TSS_SHIFTFREESCAPS      = 11;
  TSS_LANGUAGENAME        = 12;

  TSS_CALLDEFINITION	    = 13;
  TSS_CALLDEFINITION_LOADFAILED = 14;

  TSS_ETHNOLOGUECODE      = 15;

  TSS_DEBUG_LINE          = 16;

  TSS_MNEMONIC            = 17;

  TSS_INCLUDECODES        = 18;

  TSS_OLDCHARPOSMATCHING  = 19;

  TSS_COMPILEDVERSION     = 20;

  TSS_KEYMANCOPYRIGHT     = 21;

  TSS_CUSTOMKEYMANEDITION = 22;
  TSS_CUSTOMKEYMANEDITIONNAME = 23;

  { V7.0+ }
  TSS_VISUALKEYBOARD      = 24;
  TSS_KMW_RTL = 25;
  TSS_KMW_HELPFILE = 26;
  TSS_KMW_HELPTEXT = 27;
  TSS_KMW_EMBEDJS = 28;

  { V7.0.244.0+ }
  TSS_WINDOWSLANGUAGES    = 29;

  { V8.0 }

  TSS_COMPARISON = 30;

  { V9.0 }

  TSS_PLATFORM     = 31;  // I3430
  TSS_BASELAYOUT   = 32;  // I3430
  TSS_LAYER        = 33;  // I3437
  TSS_VKDICTIONARY = 34;  // I3438    // Dictionary of virtual key names for v9 dynamic layouts
  TSS_LAYOUTFILE   = 35;  // I3483
  TSS_KEYBOARDVERSION = 36;   // I4140
  TSS_KMW_EMBEDCSS = 37;   // I4368
  TSS_TARGETS = 38;   // I4504

type
  TSystemStore = (ssNone = 0, ssBitmap = 1, ssCopyright = 2, ssHotkey = 3, ssLanguage = 4, ssLayout = 5, ssMessage = 6,
    ssName = 7, ssVersion = 8, ssCapsOnOnly = 9, ssCapsAlwaysOff = 10, ssShiftFreesCaps = 11, ssLanguageName = 12,
    ssCallDefinition = 13, ssCallDefinition_LoadFailed = 14, ssEthnologueCode= 15, ssDebugLine = 16,
    ssMnemonicLayout = 17, ssIncludeCodes = 18, ssOldCharPosMatching = 19,
    ssCompiledVersion = 20, ssKeymanCopyright = 21, ssCustomKeymanEdition = 22, ssCustomKeymanEditionName = 23,
    ssVisualKeyboard = 24, ssKMW_RTL = 25, ssKMW_HelpFile = 26, ssKMW_HelpText = 27, ssKMW_EmbedJS = 28,
    ssWindowsLanguages = 29,
    ssComparison = 30,
    ssPlatform = 31, ssBaseLayout = 32, ssLayer = 33, ssVKDictionary = 34, ssLayoutFile = 35,  // I3438 // I3483
    ssKeyboardVersion = 36, ssKMW_EmbedCSS = 37, ssTargets = 38);   // I4140   // I4368   // I4504

const
  SystemStoreNames: array[TSystemStore] of WideString = (
    '', 'BITMAP', 'COPYRIGHT', 'HOTKEY', 'LANGUAGE', 'LAYOUT', 'MESSAGE',
    'NAME', 'VERSION', 'CAPSONONLY', 'CAPSALWAYSOFF', 'SHIFTFREESCAPS', 'LANGUAGENAME',
    '', '', 'ETHNOLOGUECODE', '',
    'MNEMONICLAYOUT', 'INCLUDECODES', 'OLDCHARPOSMATCHING',
    '', '', '', '',
    'VISUALKEYBOARD', 'KMW_RTL', 'KMW_HELPFILE', 'KMW_HELPTEXT', 'KMW_EMBEDJS',
    'WINDOWSLANGUAGES',
    '', //8.0
    '', '', '', '', 'LAYOUTFILE', 'KEYBOARDVERSION', 'KMW_EMBEDCSS',
    'TARGETS'); //9.0  // I3483   // I4140   // I4368   // I4504

function SystemStoreFromName(Name: WideString): TSystemStore;

function GetSystemStore(Memory: PByte; SystemID: DWord; var Buffer: WideString): Boolean;  // I3310
procedure GetKeyboardInfo(const FileName: string; FReturnDump: Boolean; var ki: TKeyboardInfo; FReturnBitmap: Boolean = False);

function PRIMARYLANGID(n: DWORD): WORD;
function SUBLANGID(n: DWORD): WORD;

type
  TKeyboardFileHeader = packed record
    dwIdentifier: Dword;    // Keyman compiled keyboard id
    dwFileVersion: Dword;   // 0x0400 or 0x0500
    dwCheckSum: Dword;
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

  TKeyboardFileStore = packed record
    dwSystemID: DWORD;
	  dpName: DWORD;
	  dpString: DWORD;
  end;

  PKeyboardFileStore = ^TKeyboardFileStore;

  TKeyboardFileGroup = packed record
	  dpName: DWORD;
    dpKeyArray: DWORD;
    dpMatch, dpNoMatch: DWORD;
    cxKeyArray: DWORD;
    fUsingKeys: BOOL;
  end;

  PKeyboardFileGroup = ^TKeyboardFileGroup;

  TKeyboardFileKey = packed record
    Key: WORD; packing: WORD;
    Line: DWORD;
    ShiftFlags: DWORD;
    dpOutput: DWORD;
    dpContext: DWORD;
  end;

  PKeyboardFileKey = ^TKeyboardFileKey;

function EncodingsAsString(ke: TKIEncodings): string;
function HotkeyAsString(hotkey: DWord): string;

implementation

uses
  crc32,
  KeyNames,
  OnlineConstants,
  utildir,
  utilfiletypes;

const
    FILEID_COMPILED	= $5354584B;

const
    KF_SHIFTFREESCAPS = $0001;
    KF_CAPSONONLY =     $0002;
    KF_CAPSALWAYSOFF =  $0004;
    KF_LOGICALLAYOUT =  $0008;



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
  cs: DWord;
  mem: TMemoryStream;
  ver: WideString;
  shver: string;
  smnemonic: WideString;
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

    ki.ProductID := OnlineProductID_KeymanDesktop_100;  // I3377

    Position := 0;
    Read(kfh, SizeOf(TKeyboardFileHeader));

    cs := kfh.dwCheckSum;
    kfh.dwCheckSum := 0;

    Position := 0;
    Write(kfh, SizeOf(TKeyboardFileHeader));

    if CalculateBufferCRC(Size, Memory) <> cs then
      raise EKMXError.CreateFmt(EKMX_InvalidKeyboardFile, 'The keyboard file %0:s is invalid', [ExtractFileName(FileName)]);;

    if kfh.dwIdentifier <> FILEID_COMPILED then
      raise EKMXError.CreateFmt(EKMX_InvalidKeyboardFile, 'The keyboard file %0:s is invalid', [ExtractFileName(FileName)]);;
    if (kfh.dwFileVersion = $0500) or (kfh.dwFileVersion = $0501) or (kfh.dwFileVersion = $0600) or (kfh.dwFileVersion = $0700) or (kfh.dwFileVersion = $0800) or  // I3377
      (kfh.dwFileVersion = $0900) then  // I3377
    begin
      GetSystemStore(Memory, TSS_NAME, ki.KeyboardName);
      GetSystemStore(Memory, TSS_COPYRIGHT, ki.CopyrightString);
      GetSystemStore(Memory, TSS_MESSAGE, ki.MessageString);

      GetSystemStore(Memory, TSS_WINDOWSLANGUAGES, ki.WindowsLanguages);
      GetSystemStore(Memory, TSS_ETHNOLOGUECODE, ki.BCP47Languages);

      if not GetSystemStore(Memory, TSS_KEYBOARDVERSION, ki.KeyboardVersion) then   // I4136
        ki.KeyboardVersion := '1.0';

      ki.MnemonicLayout := GetSystemStore(Memory, TSS_MNEMONIC, smnemonic) and (smnemonic <> '0');

      {GetSystemStore(Memory, TSS_LANGUAGE, lang);
      if kfh.dwFileVersion = $0600
        then ki.LanguageID := StrToIntDef(lang, 0)
        else ki.LanguageID := 0;}
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
//        KL.Log('Keyboard bitmap for %s is at %x', [ki.KeyboardName, kfh.dwBitmapOffset]);
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
    else if kfh.dwFileVersion = $0400 then
      raise EKMXError.Create(EKMX_OldKeyboardFile,
        'The keyboard file is from version 4.0 and is not supported in this version of Keyman.')
    else if kfh.dwFileVersion > $0600 then
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

end.

