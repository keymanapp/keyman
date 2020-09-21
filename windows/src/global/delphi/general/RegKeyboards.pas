(*
  Name:             RegKeyboards
  Copyright:        Copyright (C) SIL International.
  Documentation:    
  Description:      
  Create Date:      14 Sep 2006

  Modified Date:    28 May 2014
  Authors:          mcdurdin
  Related Files:    
  Dependencies:     

  Bugs:             
  Todo:             
  Notes:            
  History:          14 Sep 2006 - mcdurdin - Add Icon property, remove Bitmap property
                    04 Dec 2006 - mcdurdin - Add menmonic layout
                    12 Dec 2006 - mcdurdin - Fix keyboard not activating when installed with a language
                    15 Jan 2007 - mcdurdin - Load 16x16 kbd_noicon instead of 32x32
                    12 Oct 2007 - mcdurdin - I1105 - Handle invalid keyboard file, just mark keyboard as corrupt or missing
                    27 Mar 2008 - mcdurdin - I1374 - Add support for font helper
                    27 Mar 2008 - mcdurdin - I1358 - Add multiple language information
                    27 Mar 2008 - mcdurdin - I1345 - Get User Default Keyboard instead of default language keyboard
                    14 Jun 2008 - mcdurdin - Remove DefaultLanguageID, add KeyboardLanguageID
                    14 Jun 2008 - mcdurdin - I1400 - Move language info to the system config item check
                    16 Jan 2009 - mcdurdin - I1675 - Fix crash when KMX file is corrupt
                    26 Jul 2010 - mcdurdin - I2467 - 8.0 renumber
                    03 May 2011 - mcdurdin - I2890 - Record diagnostic data when encountering registry errors
                    18 May 2012 - mcdurdin - I3306 - V9.0 - Remove TntControls + Win9x support
                    17 Aug 2012 - mcdurdin - I3377 - KM9 - Update code references from 8.0 to 9.0
                    20 Nov 2012 - mcdurdin - I3581 - V9.0 - KMTip needs to pass activated profile guid through to Keyman32 to switch keyboards
                    28 Nov 2012 - mcdurdin - I3599 - V9.0 - Refactor GetKeyboardIconFileName
                    01 Dec 2012 - mcdurdin - I3613 - V9.0 - System shadow keyboards obsolete, strip out remaining code
                    19 Mar 2014 - mcdurdin - I4136 - V9.0 - Add keyboard version information to Keyman Configuration
                    28 May 2014 - mcdurdin - I4220 - V9.0 - Remove references to LoadKeyboardLayout, Preload, Substitutes, etc. and use only TSF
*)
unit RegKeyboards;  // I3306

interface

uses
  Classes,
  Graphics,
  Contnrs,
  kmxfile,
  kmxfileconsts,
  kmxfileusedscripts,
  kmxfileutils,
  Unicode,
  UnicodeBlocks,
  utilkeyman,
  SysUtils,
  HotkeyUtils;

{$R kbd_noicon.res}

{$R-}

type
  TRegKeyboard = class
  private
    { Installed Keyboards settings }
    FDefaultHotkey: Integer;
    FKeymanFile: string;
    Legacy_FKeyboardID: Integer;   // I3613
    FPackageName: string;
    FPackageDescription: string;

    { Active Keyboards settings }
    FEnabled: Boolean;
    FKeymanID: Integer;
    FName: string;

    { Properties from the keyboard files }
    FKeyboardName: WideString;
    FEncodings: TKIEncodings;
    FIsRegistered: Boolean;
    FVisualKeyboardInstalled: Boolean;
    FCopyright: WideString;
    FMessage: WideString;
    FVisualKeyboardFileName: string;
    FProductID: Integer;
    FIcon: TIcon;
    FMnemonicLayout: Boolean;
    //FNewlyInstalled: Boolean;
    FWindowsLanguages: WideString;
    FKeyboardLanguageID: Cardinal;
    FIconFileName: string;   // I3581
    FKeyboardVersion: string;
    FISO6393Languages: WideString;   // I4136

//    procedure InstallLanguage;

  public
    constructor Create(const AName: string);
    destructor Destroy; override;

    procedure Load(FLoadKeyboard: Boolean);
    procedure Save;

    function GetUsedChars: WideString;
    function GetUsedScripts: TUnicodeBlockDataArray;

    property Name: string read FName;

    { Installed Keyboards settings }
    property DefaultHotkey: Integer read FDefaultHotkey;
    property KeymanFile: string read FKeymanFile;
    property Legacy_KeyboardID: Integer read Legacy_FKeyboardID;   // I3613
    property PackageName: string read FPackageName;
    property PackageDescription: string read FPackageDescription;
    property VisualKeyboardInstalled: Boolean read FVisualKeyboardInstalled;
    property VisualKeyboardFileName: string read FVisualKeyboardFileName;
    property IconFileName: string read FIconFileName;   // I3581

    { Active Keyboards settings }
    property KeymanID: Integer read FKeymanID;

    { Properties from keyboard }
    property KeyboardName: WideString read FKeyboardName;
    property Encodings: TKIEncodings read FEncodings;
    property IsRegistered: Boolean read FIsRegistered;
    property Copyright: WideString read FCopyright;
    property Message: WideString read FMessage;
    property Icon: TIcon read FIcon;
    property ProductID: Integer read FProductID;
    property MnemonicLayout: Boolean read FMnemonicLayout;
    property WindowsLanguages: WideString read FWindowsLanguages;
    property ISO6393Languages: WideString read FISO6393Languages;
    property KeyboardLanguageID: Cardinal read FKeyboardLanguageID;
    property KeyboardVersion: string read FKeyboardVersion;   // I4136

    { Modifiable settings -- Active Keyboards }
    property Enabled: Boolean read FEnabled;
  end;

  TRegKeyboardList = class(TObjectList)
  protected
    function Get(Index: Integer): TRegKeyboard;
    procedure Put(Index: Integer; Item: TRegKeyboard);
  public
    procedure Sort;
    function IndexOfName(const Name: string): Integer;
    procedure Load(FLoadKeyboards: Boolean);
    procedure Save; // Updates Enabled only
    property Items[Index: Integer]: TRegKeyboard read Get write Put; default;
    function Add(Item: TRegKeyboard): Integer;
  end;

implementation

uses
  GetOSVersion,
  Glossary,
  isadmin,
  KLog,
  kmxfileusedchars,
  OnlineConstants,
  ErrorControlledRegistry,
  RegistryKeys,
  utilolepicture,
  Windows;

{-------------------------------------------------------------------------------
 - TRegKeyboardList                                                            -
 ------------------------------------------------------------------------------}

procedure RemoveActiveKeyboard(FName: string);
begin
  { Find whether Keyman installed a language with the keyboard }   // I4220
  with TRegistryErrorControlled.Create do  // I2890
  try
    { Remove all Active Keyboards details of the keyboard }
    DeleteKey(SRegKey_ActiveKeyboards_CU + '\' + FName);
  finally
    Free;
  end;
end;

function TRegKeyboardList.Add(Item: TRegKeyboard): Integer;   begin Result := inherited Add(Item); end;
function TRegKeyboardList.Get(Index: Integer): TRegKeyboard;  begin Result := TRegKeyboard(inherited Get(Index)); end;
procedure TRegKeyboardList.Put(Index: Integer; Item: TRegKeyboard); begin inherited Put(Index, Item); end;

procedure TRegKeyboardList.Load(FLoadKeyboards: Boolean);
var
  str: TStringList;
  rk: TRegKeyboard;
  i: Integer;
begin
  str := TStringList.Create;
  with TRegistryErrorControlled.Create do  // I2890
  try
    { Load all installed keyboards }

    RootKey := HKEY_LOCAL_MACHINE;
    if OpenKeyReadOnly(SRegKey_InstalledKeyboards_LM) then
    begin
      GetKeyNames(str);
      for i := 0 to str.Count - 1 do
      begin
        rk := TRegKeyboard.Create(str[i]);
        rk.Load(FLoadKeyboards);
        Add(rk);
      end;
      CloseKey;
    end;

    RootKey := HKEY_CURRENT_USER;

    { Find all "Active Keyboards" that have been uninstalled }
    if OpenKeyReadOnly('\'+SRegKey_ActiveKeyboards_CU) then
    begin
      GetKeyNames(str);
      for i := 0 to str.Count - 1 do
        if IndexOfName(str[i]) < 0 then
          RemoveActiveKeyboard(str[i]);
    end;

    Sort; // Order by name
  finally
    str.Free;
    Free;
  end;
end;

procedure TRegKeyboardList.Save;
var
  i, n: Integer;
  str: TStringList;
begin
  { Renumber KeymanIDs for all keyboards before saving }
  n := 0;

  for i := 0 to Count - 1 do
  begin
    if Items[i].Enabled then
    begin
      Items[i].FKeymanID := n;
      Inc(n);
    end
    else
      Items[i].FKeymanID := -1;
  end;

  { Save all KeymanIDs }
  for i := 0 to Count - 1 do
    Items[i].Save;

  { Rewrite the KeymanIDs in HKCU\Software\...\Keyman\5.0\Active Keyboards }
  str := TStringList.Create;
  with TRegistryErrorControlled.Create do  // I2890
  try
    RootKey := HKEY_CURRENT_USER;
    if OpenKey(SRegKey_ActiveKeyboards_CU, True) then
    begin
      GetValueNames(str);
      for i := 0 to str.Count - 1 do DeleteValue(str[i]);
      for i := 0 to Count - 1 do
        if Items[i].Enabled then
          WriteString(IntToStr(Items[i].KeymanID), Items[i].Name);
    end;
  finally
    Free;
    str.Free;
  end;
end;

function TRegKeyboardList.IndexOfName(const Name: string): Integer;
var
  i: Integer;
begin
  for i := 0 to Count - 1 do
    if LowerCase(Items[i].Name) = LowerCase(Name) then
    begin
      Result := i;
      Exit;
    end;
  Result := -1;
end;

function SortRegKeyboardList(Item1, Item2: Pointer): Integer;
var
  rk1, rk2: TRegKeyboard;
begin
  rk1 := TRegKeyboard(Item1);
  rk2 := TRegKeyboard(Item2);
  if LowerCase(rk1.Name) < LowerCase(rk2.Name)  then Result := -1
  else if LowerCase(rk1.Name) > LowerCase(rk2.Name)  then Result := 1
  else Result := 0;
end;

procedure TRegKeyboardList.Sort;
begin
  inherited Sort(SortRegKeyboardList);
end;

{-------------------------------------------------------------------------------
 - TRegKeyboard                                                                -
 ------------------------------------------------------------------------------}

constructor TRegKeyboard.Create(const AName: string);
begin
  FName := AName;
end;

procedure TRegKeyboard.Load(FLoadKeyboard: Boolean);
var
  ki: TKeyboardInfo;
begin
  FDefaultHotkey := 0;
  FKeymanFile := '';
  Legacy_FKeyboardID := 0;   // I3613
  FPackageName := '';
  FPackageDescription := '';
  FKeymanID := -1;
  FEnabled := False;
  FKeyboardName := '';
  FEncodings := [];
  FIsRegistered := False;
  FVisualKeyboardInstalled := False;
  FMessage := '';
  FCopyright := '';
  FKeyboardVersion := '';   // I4136
  FProductID := OnlineProductID_KeymanDesktop_100;  // I3377

  with TRegistryErrorControlled.Create do  // I2890
  try
    RootKey := HKEY_LOCAL_MACHINE;

    if OpenKeyReadOnly(GetRegistryKeyboardInstallKey_LM(FName)) then
    begin
      FKeymanFile := ReadString(SRegValue_KeymanFile);
      Legacy_FKeyboardID := StrToIntDef('$'+ReadString(SRegValue_Legacy_KeymanKeyboardID), 0);   // I3613
      FPackageName := ReadString(SRegValue_PackageName);
      FVisualKeyboardInstalled := ValueExists(SRegValue_VisualKeyboard);
      if FVisualKeyboardInstalled
        then FVisualKeyboardFileName := ReadString(SRegValue_VisualKeyboard)
        else FVisualKeyboardFileName := '';

      if FPackageName <> '' then
        with TRegistryErrorControlled.Create do  // I2890
        try
          RootKey := HKEY_LOCAL_MACHINE;
          if OpenKeyReadOnly(GetRegistryPackageInstallKey_LM(FPackageName)) and ValueExists(SRegValue_PackageDescription)
            then FPackageDescription := ReadString(SRegValue_PackageDescription)
            else FPackageDescription := FPackageName;
        finally
          Free;
        end;

      CloseKey;
    end;

    RootKey := HKEY_CURRENT_USER;
    if OpenKeyReadOnly(GetRegistryKeyboardActiveKey_CU(FName)) then
    begin
      { Currently installed keyboard }
      if ValueExists(SRegValue_KeymanID) then
      begin
        FKeymanID := StrToIntDef(ReadString(SRegValue_KeymanID), -1);
        FEnabled := FKeymanID >= 0;
      end;
    end
    else
    begin
      { Newly installed keyboard -- perform installation into end-user settings }
      FEnabled := True;
    end;
  finally
    Free;
  end;

  FIconFileName := GetKeyboardIconFileName(FKeymanFile);   // I3581   // I3599
  if not FileExists(FIconFileName, False) then
    FIconFileName := '';

  if FLoadKeyboard then
  try
    try
      GetKeyboardInfo(FKeymanFile, False, ki, True);
    except
      on E:EFileStreamError do
      begin
        { I1105 - Fix crash when keyboard file is invalid - just mark keyboard as in list but not loaded }
        FEnabled := False;
        FKeyboardName := GetShortKeyboardName(FName);
        FIsRegistered := True;
        FMessage := 'This keyboard could not be loaded.  It may be corrupt or missing: '+E.Message;
        FIcon := nil;
        Exit;
      end;
      on E:EKMXError do
      begin
        { I1675 - Fix crash when KMX file is corrupt - mark keyboard as in list but not loaded }
        FEnabled := False;
        FKeyboardName := GetShortKeyboardName(FName);
        FIsRegistered := True;
        FMessage := 'This keyboard could not be loaded.  It may be corrupt or missing: '+E.Message;
        FIcon := nil;
        Exit;
      end;
    end;
    FKeyboardName := ki.KeyboardName;
    FEncodings := ki.Encodings;
    FIsRegistered := ki.IsRegistered;
    FMessage := ki.MessageString;
    FCopyright := ki.CopyrightString;
    FWindowsLanguages := ki.WindowsLanguages;
    FISO6393Languages := ki.ISO6393Languages;
    FKeyboardLanguageID := ki.KeyboardID;
    FDefaultHotkey := ki.DefaultHotkey;
    FKeyboardVersion := ki.KeyboardVersion;   // I4136

    if ki.Icon <> nil then
    begin
      FIcon := ki.Icon;
    end
    else if ki.Bitmap <> nil then
    begin
      FIcon := TIcon.Create;
      LoadIconFromBitmap(FIcon, ki.Bitmap);
      FreeAndNil(ki.Bitmap);
    end
    else
    begin
      FIcon := TIcon.Create;
      FIcon.Handle := LoadImage(HInstance, 'kbd_noicon', IMAGE_ICON, 16, 16, LR_DEFAULTCOLOR);
      //FIcon.LoadFromResourceName(HInstance, 'kbd_noicon');
    end;

    FProductID := ki.ProductID;
    FMnemonicLayout := ki.MnemonicLayout;
  except
    on E:Exception do
      KL.LogError(E.Message);
    // swallow exceptions here
  end;

  KL.Log('TRegKeyboard.Load: '+FKeyboardName);
end;

procedure TRegKeyboard.Save;
begin
  with TRegistryErrorControlled.Create do  // I2890
  try
    RootKey := HKEY_CURRENT_USER;
    if OpenKey(GetRegistryKeyboardActiveKey_CU(FName), True) then
    begin
      if FKeymanID >= 0 then
        WriteString(SRegValue_KeymanID, IntToStr(FKeymanID))
      else if ValueExists(SRegValue_KeymanID) then
        DeleteValue(SRegValue_KeymanID);

      CloseKey;
    end;
  finally
    Free;
  end;
end;

destructor TRegKeyboard.Destroy;
begin
  FreeAndNil(FIcon);
  inherited Destroy;
end;

function GetSystemStore(Memory: PChar; SystemID: DWord; var Buffer: WideString): Boolean;
var
  pch: PChar;
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
      pch := PChar(Memory);
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

function TRegKeyboard.GetUsedChars: WideString;
begin
  Result := KMXFile_GetUsedChars(FKeymanFile);
end;

function TRegKeyboard.GetUsedScripts: TUnicodeBlockDataArray;
begin
  Result := KMXFile_GetUsedScripts(FKeymanFile);
end;

{$R+}

end.
