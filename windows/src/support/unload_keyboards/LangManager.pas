(*
  Name:             LangSwitchManager
  Copyright:        Copyright (C) SIL International.
  Documentation:    
  Description:      
  Create Date:      22 Oct 2010

  Modified Date:    2 Jun 2015
  Authors:          mcdurdin
  Related Files:    
  Dependencies:     

  Bugs:             
  Todo:             
  Notes:            
  History:          22 Oct 2010 - mcdurdin - I2522 - Initial version of language switch window
                    30 Nov 2010 - mcdurdin - I2543 - Support switching to TSF addins in Language Switch Window
                    17 Dec 2010 - mcdurdin - I2575 - Wrong keyboard layout selected for default language
                    18 Mar 2011 - mcdurdin - I2826 - Don't show TSF addin in language switch window
                    05 Apr 2011 - mcdurdin - I2864 - TSF interfaces not supported on XP, check fails
                    03 May 2011 - mcdurdin - I2867 - When no keyboards listed for a language, lang switch can crash
                    03 May 2011 - mcdurdin - I2890 - Record diagnostic data when encountering registry errors
                    19 Aug 2011 - mcdurdin - I3024 - If the default language is not the first in the TSF list, it can confuse the end user
                    03 Oct 2011 - mcdurdin - I3087 - Crash opening language switch window when only 1 language detected
                    08 Jun 2012 - mcdurdin - I3311 - V9.0 - Change 'published' to 'public' on classes that don't need RTTI
                    02 Dec 2011 - mcdurdin - I3161 - When multiple OEM products installed, lang switch window can show keyboards for non-running products
                    03 Nov 2012 - mcdurdin - I3518 - V9.0 - Merge of I3161 - When multiple OEM products installed, lang switch window can show keyboards for non-running products
                    24 Oct 2013 - mcdurdin - I3933 - V9.0 - Keyman tray icon menu is not showing installed keyboards
                    07 Nov 2013 - mcdurdin - I3949 - V9.0 - Keyboard selection and notification needs consolidation
                    07 Nov 2013 - mcdurdin - I3950 - V9.0 - Keyman needs to show TIP icons when showing keyboard menu
                    11 Nov 2013 - mcdurdin - I3961 - V9.0 - Clean up communication between keyman32 and keyman
                    17 Dec 2013 - mcdurdin - I4004 - V9.0 - Remove old Keyman keyboard code from LangSwitchManager
                    17 Dec 2013 - mcdurdin - I4005 - V9.0 - Exclude non-keyboard TIPs from keyboard switch menu
                    01 May 2014 - mcdurdin - I4207 - V9.0 - After installing + uninstalling keyboards, found extra blank entries in the Keyman keyboard list
                    02 Jun 2015 - mcdurdin - I4715 - Language profile change notification while Keyman menu is visible sometimes causes a crash [CrashID:keyman.exe_9.0.492.0_00000000_EAccessViolation]
*)
unit LangManager;

interface

uses
  Windows,
  ActiveX,
  ComObj,
  msctf,
  Classes,
  SysUtils,
  Contnrs,
  Graphics;

type
  PWord1 = ^Word;

  TLangSwitchItemType = (lsitUnknown, lsitWinKeyboard, lsitTIP);   // I4004

  TLangSwitchManager = class;
  TLangSwitchLanguage = class;

  TLangSwitchObject = class
  private
    FManager: TLangSwitchManager;
  public
    constructor Create(AManager: TLangSwitchManager);
    property Manager: TLangSwitchManager read FManager;
  end;

  TLangSwitchList = class(TObjectList)
  private
    FManager: TLangSwitchManager;
  public
    constructor Create(AManager: TLangSwitchManager);
    property Manager: TLangSwitchManager read FManager;
  end;

  TLangSwitchKeyboard = class(TLangSwitchObject)
  private
    FToolTip: WideString;
    FItemType: TLangSwitchItemType;
    FBitmap: TBitmap;
    FCaption: WideString;
    FValid: Boolean;   // I4207
    //FKeymanID: Integer; // If associated with a Keyman keyboard, otherwise KEYMANID_NONKEYMAN   // I3949
    FLanguage: TLangSwitchLanguage;
    FEnabled: BOolean;   // I3961n
  protected
    function GetItemID: Integer; virtual; abstract;
  public
    constructor Create(AManager: TLangSwitchManager; ALanguage: TLangSwitchLanguage);   // I3949
    destructor Destroy; override;
    procedure Activate; virtual; abstract;   // I4715
    property Caption: WideString read FCaption;
    property Tooltip: WideString read FToolTip;
    property ItemType: TLangSwitchItemType read FItemType;
    property ItemID: Integer read GetItemID;
    property Bitmap: TBitmap read FBitmap;
    //property KeymanID: Integer read FKeymanID;   // I3949
    property Language: TLangSwitchLanguage read FLanguage;   // I3961
    property Valid: Boolean read FValid;   // I4207
    property Enabled: Boolean read FEnabled;
  end;

  TLangSwitchKeyboards = class(TLangSwitchList)
  private
    function GetItem(Index: Integer): TLangSwitchKeyboard;
    procedure SetItem(Index: Integer; const Value: TLangSwitchKeyboard);
  public
    property Items[Index: Integer]: TLangSwitchKeyboard read GetItem write SetItem; default;
  end;

  TLangSwitchLanguage = class(TLangSwitchObject)
  private
    FKeyboards: TLangSwitchKeyboards;
    FLangID: DWORD;
    FCaption: WideString;
    FIconText: WideString;
    FDefault: Boolean;  // I3024
    FItemIndex: Integer;  // I3024
    function GetKeyboard(Index: Integer): TLangSwitchKeyboard;
    function GetKeyboardCount: Integer;
  public
    constructor Create(AManager: TLangSwitchManager; ALangID: DWORD);
    destructor Destroy; override;
    procedure Add(Item: TLangSwitchKeyboard);
    procedure Delete(Index: Integer);
    property IconText: WideString read FIconText;
    property Caption: WideString read FCaption;
    property LangID: DWORD read FLangID;
    property KeyboardCount: Integer read GetKeyboardCount;
    property Keyboards[Index: Integer]: TLangSwitchKeyboard read GetKeyboard; default;
    property ItemIndex: Integer read FItemIndex;  // I3024
    property Default: Boolean read FDefault;  // I3024
  end;

  TLangSwitchLanguages = class(TLangSwitchList)  // I3311
  private
    function GetItem(Index: Integer): TLangSwitchLanguage;
    procedure SetItem(Index: Integer; const Value: TLangSwitchLanguage);
  public
    function IndexOfLangID(LangID: Cardinal): Integer;
    property Items[Index: Integer]: TLangSwitchLanguage read GetItem write SetItem; default;
  end;

  TLangSwitchManager = class(TLangSwitchObject)
  private
    FProfiles: ITfInputProcessorProfiles;
    FProfileMgr: ITfInputProcessorProfileMgr;
    FLanguages: TLangSwitchLanguages;
    FDefaultLanguage: TLangSwitchLanguage;
    function GetLanguage(Index: Integer): TLangSwitchLanguage;
    procedure EnumTSFKeyboards;
    procedure EnumLanguages;
    function GetLanguageCount: Integer;
    procedure FindDefaultLanguage;  // I2575
//    procedure RemoveEmptyLanguages;  // I2867
    procedure InstantiateProfiles;   // I4207
  public
    constructor Create; reintroduce;
    destructor Destroy; override;
    procedure Refresh;
    property Languages[Index: Integer]: TLangSwitchLanguage read GetLanguage;
    property LanguageCount: Integer read GetLanguageCount;
  end;

  TLangSwitchKeyboard_TIP = class(TLangSwitchKeyboard)
  private
    FProfile: TF_INPUTPROCESSORPROFILE;
  protected
    function GetItemID: Integer; override;
  public
    constructor Create(AManager: TLangSwitchManager; ALanguage: TLangSwitchLanguage; AProfile: TF_INPUTPROCESSORPROFILE);   // I3961
    procedure Activate; override;   // I4715
    property Profile: TF_INPUTPROCESSORPROFILE read FProfile;
  end;

  TLangSwitchKeyboard_WinKeyboard = class(TLangSwitchKeyboard)
  private
    FHKL: HKL;
    procedure GetLayoutName;
  protected
    function GetItemID: Integer; override;
  public
    constructor Create(AManager: TLangSwitchManager; ALanguage: TLangSwitchLanguage; Ahkl: HKL); //; AActive: Boolean);   // I3961
    procedure Activate; override;   // I4715
    property HKL: HKL read FHKL;
  end;

const
  SRegKey_KeyboardLayouts = '\SYSTEM\CurrentControlSet\Control\Keyboard Layouts';
  SRegKey_KeyboardLayoutPreload = '\Keyboard Layout\Preload';
  SRegKey_KeyboardLayoutSubstitutes = '\Keyboard Layout\Substitutes';

implementation

uses
  Winapi.ShellApi,

//  System.StrUtils,

  glossary,
  LoadIndirectStringUnit,
  ErrorControlledRegistry,
  Registry,
  RegistryKeys,
  WideStrings,
  keyman_msctf;

{ TLangSwitchItems }

function TLangSwitchKeyboards.GetItem(Index: Integer): TLangSwitchKeyboard;
begin
  Result := inherited GetItem(Index) as TLangSwitchKeyboard;
end;

procedure TLangSwitchKeyboards.SetItem(Index: Integer; const Value: TLangSwitchKeyboard);
begin
  inherited SetItem(Index, Value);
end;

{ TLangSwitchManager }

constructor TLangSwitchManager.Create;
begin
  inherited Create(Self);
  FLanguages := TLangSwitchLanguages.Create(Self);
  InstantiateProfiles;   // I4207
end;

procedure TLangSwitchManager.InstantiateProfiles;   // I4207
begin
  FProfiles := CreateComObject(CLASS_TF_InputProcessorProfiles) as ITfInputProcessorProfiles;
  if not Supports(FProfiles, ITfInputProcessorProfileMgr, FProfileMgr) then  // I2864
    FProfileMgr := nil;
end;

destructor TLangSwitchManager.Destroy;
begin
  FreeAndNil(FLanguages);
  FProfiles := nil;
  FProfileMgr := nil;
  inherited Destroy;
end;

function TLangSwitchManager.GetLanguage(Index: Integer): TLangSwitchLanguage;
begin
  Result := FLanguages[Index];
end;

function TLangSwitchManager.GetLanguageCount: Integer;
begin
  Result := FLanguages.Count;
end;

procedure TLangSwitchManager.EnumLanguages;
var
  i: Integer;
  pLangID: PWord1;
  ulCount: Cardinal;
  FLanguage: TLangSwitchLanguage;
begin
  FProfiles.GetLanguageList(@pLangID, ulCount);
  for I := 0 to ulCount - 1 do
  begin
    FLanguage := TLangSwitchLanguage.Create(Self, pLangID^);
    FLanguages.Add(FLanguage);
    Inc(pLangID);
  end;
  Dec(pLangID, ulCount);
  CoTaskMemFree(pLangID);
end;

const
//** TODO: this needs to change because v9 TSF works so differently **
TSF_TIP_GUID: TGUID = (D1:$7ba04432; D2:$8609; D3: $4fe6; D4: ($bf,$f7, $97,$10,$91,$de,$09,$33));

type
  PHKL = ^HKL;

function GetKeyboardLayoutListX(nBuff: Integer; List: PHKL): UINT; stdcall; external 'user32.dll' name 'GetKeyboardLayoutList';

procedure TLangSwitchManager.EnumTSFKeyboards;
var
  i: Integer;
  pcFetch: Cardinal;
  profile: msctf.TF_INPUTPROCESSORPROFILE;
  ppEnum: msctf.IEnumTfInputProcessorProfiles;
  FItem: TLangSwitchKeyboard;
begin
  FItem := nil;
  Assert(Assigned(FProfileMgr));  // I2864

  for I := 0 to FLanguages.Count - 1 do
  begin
    FProfileMgr.EnumProfiles(FLanguages[i].LangID, ppEnum);
    while ppEnum.Next(1, profile, pcFetch) = S_OK do
    begin
      if profile.dwProfileType = TF_PROFILETYPE_KEYBOARDLAYOUT then
      begin
        FItem := TLangSwitchKeyboard_WinKeyboard.Create(Self, FLanguages[i], profile.HKL); //, (profile.dwFlags and TF_IPP_FLAG_ACTIVE) = TF_IPP_FLAG_ACTIVE);   // I3961
      end
      else if (profile.catid = GUID_TFCAT_TIP_KEYBOARD) then  // I2826   // I3933   // I4005
      begin
        FItem := TLangSwitchKeyboard_TIP.Create(Self, FLanguages[i], profile);   // I3961
      end
      else
      begin
        Continue;
      end;

      FItem.FEnabled := (profile.dwFlags and TF_IPP_FLAG_ENABLED) = TF_IPP_FLAG_ENABLED;
      FLanguages[i].Add(FItem);
    end;
  end;
end;

procedure TLangSwitchManager.Refresh;   // I3933
begin
  FDefaultLanguage := nil;
  FLanguages.Clear;

  EnumLanguages;
  EnumTSFKeyboards;
  FindDefaultLanguage;  // I2575
//  RemoveEmptyLanguages; // I2867
end;

//procedure TLangSwitchManager.RemoveEmptyLanguages;  // I2867
//var
//  I: Integer;
//begin
//  for I := FLanguages.Count - 1 downto 0 do
//    if FLanguages[I].KeyboardCount = 0 then
//      FLanguages.Delete(I);
//end;


function SortDefault(Item1, Item2: Pointer): Integer;  // I3024
var
  L1, L2: TLangSwitchLanguage;
begin
  L1 := TLangSwitchLanguage(Item1);
  L2 := TLangSwitchLanguage(Item2);
  if L1.Default then Result := -1
  else if L2.Default then Result := 1
  else Result := L1.ItemIndex - L2.ItemIndex;
end;

procedure TLangSwitchManager.FindDefaultLanguage;  // I2575
var
  i: Integer;
  FDefaultHKL: HKL;
begin
  if FLanguages.Count = 0 then Exit;

  FDefaultLanguage := nil;  // I3024

  if SystemParametersInfo(SPI_GETDEFAULTINPUTLANG, 0, @FDefaultHKL, 0) then
  begin
    for i := 0 to FLanguages.Count - 1 do
    begin
      FLanguages[i].FItemIndex := i;  // I3024
      if FLanguages[i].FLangID = HKLToLanguageID(FDefaultHKL) then
      begin
        FDefaultLanguage := FLanguages[i];
        FLanguages[i].FDefault := True;  // I3024
      end;
    end;

    if FLanguages.Count > 1 then  // I3087
      FLanguages.Sort(SortDefault);  // I3024
  end;
end;

{ TLangSwitchLanguage }

procedure TLangSwitchLanguage.Add(Item: TLangSwitchKeyboard);
begin
  FKeyboards.Add(Item);
end;

constructor TLangSwitchLanguage.Create(AManager: TLangSwitchManager; ALangID: DWORD);
var
  szLangName: array[0..MAX_PATH] of WideChar;
begin
  inherited Create(AManager);
  FKeyboards := TLangSwitchKeyboards.Create(Manager);
  FLangID := ALangID;

  if GetLocaleInfoW(FLangID, LOCALE_SLANGUAGE, szLangName, MAX_PATH) <> 0
    then FCaption := Copy(szLangName, 0, MAX_PATH - 1)
    else FCaption := 'Unknown '+IntToHex(FLangID, 8);

  if GetLocaleInfoW(FLangID, LOCALE_SISO639LANGNAME, szLangName, MAX_PATH) <> 0
    then FIconText := WideUpperCase(Copy(szLangName, 0, MAX_PATH - 1))
    else FIconText := '??';

  //BuildIcon;
end;

procedure TLangSwitchLanguage.Delete(Index: Integer);
begin
  FKeyboards.Delete(Index);
end;

destructor TLangSwitchLanguage.Destroy;
begin
  FreeAndNil(FKeyboards);
  inherited Destroy;
end;

function TLangSwitchLanguage.GetKeyboard(Index: Integer): TLangSwitchKeyboard;
begin
  Result := FKeyboards[Index];
end;

function TLangSwitchLanguage.GetKeyboardCount: Integer;
begin
  Result := FKeyboards.Count;
end;

{ TLangSwitchItem_WinKeyboard }

procedure TLangSwitchKeyboard_WinKeyboard.Activate;   // I4715
begin
  ActivateKeyboardLayout(FHKL, 0);
end;

constructor TLangSwitchKeyboard_WinKeyboard.Create(AManager: TLangSwitchManager; ALanguage: TLangSwitchLanguage; AHkl: HKL); //; AActive: Boolean);   // I3961
begin
  inherited Create(AManager, ALanguage);   // I3961
  FHkl := AHkl;
  FItemType := lsitWinKeyboard;
  GetLayoutName;
end;

function TLangSwitchKeyboard_WinKeyboard.GetItemID: Integer;
begin
  Result := FHKL;
end;

procedure TLangSwitchKeyboard_WinKeyboard.GetLayoutName;
var
  v, kbd: LongWord;
begin
  kbd := HKLToKeyboardID(FHKL);
  { Find the name of the keyboard layout }
  with TRegistryErrorControlled.Create do  // I2890
  try
    RootKey := HKEY_LOCAL_MACHINE;
    if HIWORD(kbd) > $F
      then v := HIWORD(kbd)
      else v := kbd;
    if OpenKeyReadOnly('\'+SRegKey_KeyboardLayouts+'\'+IntToHex(v, 8)) then
    begin
      FValid := True;   // I4207
      if ValueExists(SRegValue_LayoutDisplayName) then
      begin
        FCaption := LoadIndirectString(ReadString(SRegValue_LayoutDisplayName));
      end
      else if ValueExists(SRegValue_KeyboardLayoutText) then
        FCaption := ReadString(SRegValue_KeyboardLayoutText)
      else
        FCaption := IntToHex(v, 8);
    end
    else
      FCaption := IntToHex(v, 8);
  finally
    Free;
  end;
end;

{ TLangSwitchItem_TIP }

function ExpandRegString(sz: string): string;   // I3950
var
  buf: array[0..260] of char;
begin
  if ExpandEnvironmentStrings(PChar(sz), buf, Length(buf)) = 0
    then Result := ''
    else Result := buf;
end;

procedure TLangSwitchKeyboard_TIP.Activate;   // I4715
begin
  inherited;
  FManager.FProfiles.ChangeCurrentLanguage(FProfile.langid);
  FManager.FProfileMgr.ActivateProfile(FProfile.dwProfileType,
    FProfile.langid, FProfile.clsid, FProfile.guidProfile, 0,
    TF_IPPMF_DONTCARECURRENTINPUTLANGUAGE);
end;

constructor TLangSwitchKeyboard_TIP.Create(AManager: TLangSwitchManager; ALanguage: TLangSwitchLanguage; AProfile: msctf.TF_INPUTPROCESSORPROFILE);   // I3961
var
  FIconFile: string;
  FIconIndex: Integer;
  FIconHandleLarge: HICON;
  FIconHandleSmall: HICON;
//  i: Integer;
//  kbd: IKeymanKeyboardInstalled3;
//  lang: IKeymanKeyboardLanguage;
//  j: Integer;
//  Found: Boolean;
begin
  inherited Create(AManager, ALanguage);   // I3961

  FProfile := AProfile;
  FItemType := lsitTIP;   // I3933
  if not Succeeded(FManager.FProfiles.GetLanguageProfileDescription(FProfile.clsid, FProfile.langid, FProfile.guidProfile, FCaption)) then
    Exit; // FValid will be false   // I4207

  FValid := True;

  { Get Keyman-specific details }

  (*if IsEqualGUID(FProfile.clsid, TSF_TIP_GUID) then   // I3949
  begin
    Found := False;
    for i := 1 to frmKeyman7Main.KeymanInterface.Keyboards.Count do
    begin
      kbd := frmKeyman7Main.KeymanInterface.Keyboards[i] as IKeymanKeyboardInstalled3;
      for j := 1 to kbd.Languages.Count do
      begin
        lang := kbd.Languages[j];
        if IsEqualGUID(FProfile.guidProfile, lang.ProfileGUID) then
        begin
          FKeymanID := kbd.KeymanID;
          Found := True;
          Break;
        end;
      end;
      if Found then
        Break;
    end;
  end;*)

  { Load the icon for the TIP }
  with TRegistry.Create do   // I3950
  try
    // TODO: 64-bit only TIPs?
    RootKey := HKEY_LOCAL_MACHINE;
    if OpenKeyReadOnly(
      Format('Software\Microsoft\CTF\TIP\%s\LanguageProfile\0x%08.8x\%s',
      [GUIDToString(FProfile.clsid), FProfile.langid, GUIDToString(FProfile.guidProfile)])) then
    begin
      if ValueExists('IconFile') and ValueExists('IconIndex') then
      begin
        FIconFile := ExpandRegString(ReadString('IconFile'));
        FIconIndex := ReadInteger('IconIndex');
        if FileExists(FIconFile) then
        begin
          if ExtractIconEx(PChar(FIconFile), FIconIndex, FIconHandleLarge, FIconHandleSmall, 1) > 0 then
          begin
            if FIconHandleLarge <> 0 then
              DestroyIcon(FIconHandleLarge);

            if FIconHandleSmall <> 0 then
            begin
              FBitmap := TBitmap.Create;
              FBitmap.Width := 16;
              FBitmap.Height := 16;
              FBitmap.PixelFormat := pf24Bit;
              FBitmap.Transparent := False;
              DrawIconEx(FBitmap.Canvas.Handle, 0, 0, FIconHandleSmall, 16, 16, 0, 0, DI_NORMAL);
              DestroyIcon(FIconHandleSmall);
            end;
          end;
        end;
      end;
    end;
  finally
    Free;
  end;
end;

function TLangSwitchKeyboard_TIP.GetItemID: Integer;
begin
  Result := 0;
end;

{ TLangSwitchObject }

constructor TLangSwitchObject.Create(AManager: TLangSwitchManager);
begin
  FManager := AManager;
  inherited Create;
end;

{ TLangSwitchList }

constructor TLangSwitchList.Create(AManager: TLangSwitchManager);
begin
  FManager := AManager;
  inherited Create;
end;

{ TLangSwitchLanguages }

function TLangSwitchLanguages.GetItem(Index: Integer): TLangSwitchLanguage;
begin
  Result := inherited GetItem(Index) as TLangSwitchLanguage;
end;

function TLangSwitchLanguages.IndexOfLangID(LangID: Cardinal): Integer;
var
  i: Integer;
begin
  for i := 0 to Count - 1 do
    if Items[i].LangID = LangID then
    begin
      Result := i;
      Exit;
    end;
  Result := -1;
end;

procedure TLangSwitchLanguages.SetItem(Index: Integer; const Value: TLangSwitchLanguage);
begin
  inherited SetItem(Index, Value);
end;

{ TLangSwitchKeyboard }

constructor TLangSwitchKeyboard.Create(AManager: TLangSwitchManager; ALanguage: TLangSwitchLanguage);   // I3961
begin
  inherited Create(AManager);
//  FKeymanID := KEYMANID_NONKEYMAN;   // I3949
  FLanguage := ALanguage;   // I3961
end;

destructor TLangSwitchKeyboard.Destroy;
begin
  FreeAndNil(FBitmap);
  inherited Destroy;
end;

end.
