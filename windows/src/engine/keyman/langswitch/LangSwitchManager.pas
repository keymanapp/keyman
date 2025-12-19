(*
  Name:             LangSwitchManager
  Copyright:        Copyright (C) SIL International.
  Documentation:
  Description:
  Create Date:      22 Oct 2010

  Modified Date:    12 Sep 2016
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
                    28 May 2014 - mcdurdin - I4220 - V9.0 - Remove references to LoadKeyboardLayout, Preload, Substitutes, etc. and use only TSF
                    03 Jun 2014 - mcdurdin - I4248 - V9.0 - Refactor of kmtip
                    10 Jun 2014 - mcdurdin - I4202 - V9.0 - Keyman installed keyboards do not seem to appear in Windows Language control panel in Win 8
                    05 Mar 2015 - mcdurdin - I4606 - V9.0 - Support single keyboard buttons on OSK toolbar
                    07 Apr 2015 - mcdurdin - I4648 - V9.0 - Add logging for registration of keyboards for hotkey matching
                    22 Apr 2015 - mcdurdin - I4674 - V9.0 - Hotkeys do not always work consistently
                    14 May 2015 - mcdurdin - I4713 - V9.0 - MSKLC keyboards are not shown in the Keyman menu
                    02 Jun 2015 - mcdurdin - I4715 - Language profile change notification while Keyman menu is visible sometimes causes a crash [CrashID:keyman.exe_9.0.492.0_00000000_EAccessViolation]
                    12 Sep 2016 - mcdurdin - I5086 - Keyboard hotkey toggles are not working in 9.0
*)
unit LangSwitchManager;

interface

uses
  ErrorControlledRegistry,
  System.Classes,
  System.Contnrs,
  System.SysUtils,
  System.Win.ComObj,
  Vcl.Graphics,
  Winapi.ActiveX,
  Winapi.msctf,
  Winapi.Windows,

  keymanapi_TLB,
  keyman_msctf,
  Windows8LanguageList;

type
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
    FBitmap: Vcl.Graphics.TBitmap;
    FCaption: WideString;
    FValid: Boolean;   // I4207
    FActive: Boolean;   // I3933
    FKeymanID: Integer; // If associated with a Keyman keyboard, otherwise KEYMANID_NONKEYMAN   // I3949
    FLanguage: TLangSwitchLanguage;
  protected
    function GetBitmap: Vcl.Graphics.TBitmap; virtual;
    function GetItemID: Integer; virtual; abstract;
    function GetID: string; virtual; abstract;
    function GetKeymanLanguage: IKeymanLanguage; virtual; abstract;
    function HasHKL(AHKL: HKL): Boolean; virtual; abstract;   // I5086
  public
    constructor Create(AManager: TLangSwitchManager; ALanguage: TLangSwitchLanguage);   // I3949
    destructor Destroy; override;
    procedure Activate(hwnd: THandle); virtual; abstract;
    property Active: Boolean read FActive;   // I3933
    property Caption: WideString read FCaption;
    property Tooltip: WideString read FToolTip;
    property ItemType: TLangSwitchItemType read FItemType;
    property ItemID: Integer read GetItemID;
    property ID: string read GetID;
    property Bitmap: Vcl.Graphics.TBitmap read GetBitmap;
    property KeymanID: Integer read FKeymanID;   // I3949
    property KeymanLanguage: IKeymanLanguage read GetKeymanLanguage;
    property Language: TLangSwitchLanguage read FLanguage;   // I3961
    property Valid: Boolean read FValid;   // I4207
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
  protected
    procedure SetDefaultKeyboard(AHKL: HKL);   // I5086
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
    FWin8Languages: TWindows8LanguageList;
    FDefaultLanguage: TLangSwitchLanguage;
    function GetLanguage(Index: Integer): TLangSwitchLanguage;
    procedure EnumTSFKeyboards;
    procedure EnumLanguages;
    function GetLanguageCount: Integer;
    procedure FindDefaultLanguage;  // I2575
    procedure RemoveEmptyLanguages;  // I2867
    procedure SetActiveItem(Item: TLangSwitchKeyboard);   // I3949
    function GetActiveKeyboard: TLangSwitchKeyboard;   // I3933   // I3949
    procedure InstantiateProfiles;   // I4207
    function GetTotalKeyboardCount: Integer;   // I4606
  protected
    property Win8Languages: TWindows8LanguageList read FWin8Languages;
  public
    constructor Create; reintroduce;
    destructor Destroy; override;
    procedure UpdateActive(FLangID: Cardinal; FActiveItemType: TLangSwitchItemType; FHKL: HKL); overload;   // I3933
    procedure UpdateActive(FLangID: Cardinal; FActiveItemType: TLangSwitchItemType; FClsid, FProfileGuid: TGUID); overload;   // I3933
    procedure Refresh;
    function FindKeyboard(FHKL: HKL; FProfileGuid: TGUID): TLangSwitchKeyboard; overload;
    function FindKeyboard(id: string): TLangSwitchKeyboard; overload;
    property Languages[Index: Integer]: TLangSwitchLanguage read GetLanguage;
    property LanguageCount: Integer read GetLanguageCount;
    property TotalKeyboardCount: Integer read GetTotalKeyboardCount;   // I4606
    property ActiveKeyboard: TLangSwitchKeyboard read GetActiveKeyboard;   // I3949
  end;

  TLangSwitchKeyboard_TIP = class(TLangSwitchKeyboard)
  private
    FProfile: TF_INPUTPROCESSORPROFILE;
    FIconHandle: HICON;
  protected
    function GetItemID: Integer; override;
    function GetID: string; override;
    function GetKeymanLanguage: IKeymanLanguage; override;
    function HasHKL(AHKL: HKL): Boolean; override;   // I5086
  public
    constructor Create(AManager: TLangSwitchManager; ALanguage: TLangSwitchLanguage; AProfile: TF_INPUTPROCESSORPROFILE);   // I3961
    destructor Destroy; override;
    procedure Activate(hwnd: THandle); override;
    property Profile: TF_INPUTPROCESSORPROFILE read FProfile;
    property IconHandle: HICON read FIconHandle;
  end;

  TLangSwitchKeyboard_WinKeyboard = class(TLangSwitchKeyboard)
  private
    FHKL: HKL;
    procedure GetLayoutName;
  protected
    function GetBitmap: Vcl.Graphics.TBitmap; override;
    function GetItemID: Integer; override;
    function GetID: string; override;
    function GetKeymanLanguage: IKeymanLanguage; override;
    function HasHKL(AHKL: HKL): Boolean; override;   // I5086
  public
    constructor Create(AManager: TLangSwitchManager; ALanguage: TLangSwitchLanguage; Ahkl: HKL); //; AActive: Boolean);   // I3961
    procedure Activate(hwnd: THandle); override;
    property HKL: HKL read FHKL;
  end;

  TLangSwitchConfiguration = class
  private
    FCurrentHotkey: DWord;
    FLanguageToggle: string;
    FLayoutToggle: string;
    procedure LoadCurrentHotkey;
    (**
     * Ensures that the specified registry value has a valid string data type.
     * If the registry value exists but is not of type `rdString` or `rdExpandString`,
     * the function deletes the existing value and writes a default string value
     * of `'3'`.
     *
     * @param    RegistryKey   The registry key object wrapper used to access the registry.
     * @param    ValueName     The name of the registry value to check and correct.
     *
     * @returns  True if the data type was incorrect and the value was reset;
     *           False if the value did not exist or was already a valid string type.
     *)
    function  FixRegistryDataType(RegistryKey: TRegistryErrorControlled; const ValueName: string): Boolean;
    procedure DisableWindowsHotkey;
    procedure RestoreWindowsHotkey;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Refresh;
  end;

implementation

uses
  Winapi.ShellApi,

//  System.StrUtils,

  Keyman.System.DebugLogClient,
  Keyman.System.SharedBuffers,
  glossary,
  InterfaceHotkeys,
  kmint,
  LoadIndirectStringUnit,
  Registry,
  RegistryKeys,
  UfrmKeyman7Main,
  utiltsf,
  WideStrings;

var
  FWinKeyboardBitmap: Vcl.Graphics.TBitmap = nil;

{$R LangSwitchManager.res}

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
  FWin8Languages := TWindows8LanguageList.Create(False);
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
  FreeAndNil(FWin8Languages);
  inherited Destroy;
end;

function TLangSwitchManager.GetActiveKeyboard: TLangSwitchKeyboard;   // I3949
var
  i: Integer;
  j: Integer;
begin
  for i := 0 to FLanguages.Count - 1 do
    for j := 0 to FLanguages[i].KeyboardCount - 1 do
      if FLanguages[i].Keyboards[j].Active then
        Exit(FLanguages[i].Keyboards[j]);
  Exit(nil);
end;

function TLangSwitchManager.GetLanguage(Index: Integer): TLangSwitchLanguage;
begin
  Result := FLanguages[Index];
end;

function TLangSwitchManager.GetLanguageCount: Integer;
begin
  Result := FLanguages.Count;
end;

function TLangSwitchManager.GetTotalKeyboardCount: Integer;   // I4606
var
  I: Integer;
begin
  Result := 0;
  for I := 0 to FLanguages.Count - 1 do
    Inc(Result, FLanguages[I].KeyboardCount);
end;

procedure TLangSwitchManager.EnumLanguages;
var
  i: Integer;
  pLangID: PWord;
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

type
  PHKL = ^HKL;

procedure TLangSwitchManager.EnumTSFKeyboards;
var
  i: Integer;
  hr: HResult;
  pcFetch: Cardinal;
  profile: TF_INPUTPROCESSORPROFILE;
  ppEnum: IEnumTfInputProcessorProfiles;
  FItem: TLangSwitchKeyboard;
begin
  TDebugLogClient.Instance.WriteMessage('[EnumTSFKeyboards] ENTER -------------', []);
  Assert(Assigned(FProfileMgr));  // I2864   // I4220

  for I := 0 to FLanguages.Count - 1 do
  begin
    TDebugLogClient.Instance.WriteMessage('[EnumTSFKeyboards] LangID=%x', [FLanguages[i].LangID]);
    FProfileMgr.EnumProfiles(FLanguages[i].LangID, ppEnum);
    hr := ppEnum.Next(1, profile, pcFetch);
    while hr = S_OK do
    begin
      try
        TDebugLogClient.Instance.WriteMessage('[EnumTSFKeyboards] LangID=%x type=%d hkl=%x flags=%x', [FLanguages[i].LangID, profile.dwProfileType, profile.HKL, profile.dwFlags]);

        if (profile.dwFlags and TF_IPP_FLAG_ENABLED) = 0 then   // I4207
        begin
          Continue;
        end;

        if profile.dwProfileType = TF_PROFILETYPE_KEYBOARDLAYOUT then
        begin
          FItem := TLangSwitchKeyboard_WinKeyboard.Create(Self, FLanguages[i], profile.HKL); //, (profile.dwFlags and TF_IPP_FLAG_ACTIVE) = TF_IPP_FLAG_ACTIVE);   // I3961
          TDebugLogClient.Instance.WriteMessage('[EnumTSFKeyboards]   Creating WinKeyboard hkl=%x, valid=%s', [profile.HKL, BoolToStr(FItem.Valid)]);   // I4648
        end
        else if (profile.catid = GUID_TFCAT_TIP_KEYBOARD) then  // I2826   // I3933   // I4005
        begin
          FItem := TLangSwitchKeyboard_TIP.Create(Self, FLanguages[i], profile);   // I3961
          TDebugLogClient.Instance.WriteMessage('[EnumTSFKeyboards]   Creating TSF TIP guid=%s keymanid=%d, valid=%s', [GuidToString(profile.guidProfile), FItem.KeymanID, BoolToStr(FItem.Valid)]);   // I4648
        end
        else
        begin
          Continue;
        end;

        if FItem.Valid   // I4207
          then FLanguages[i].Add(FItem)
          else FreeAndNil(FItem);
      finally
        hr := ppEnum.Next(1, profile, pcFetch);
      end;
    end;
  end;
  TDebugLogClient.Instance.WriteMessage('[EnumTSFKeyboards] EXIT ---------------', []);
end;

procedure TLangSwitchManager.Refresh;   // I3933
var
  i: Integer;
  j: Integer;
  FActiveLangID: Cardinal;
  FActiveItemType: TLangSwitchItemType;
  FActiveHKL: HKL;
  FActiveProfileGuid: TGUID;
  FActiveClsid: TGUID;

//  templang: array[0..5] of TLangSwitchLanguage;   // I4715
//  tempkbd: array[0..15] of TLangSwitchKeyboard;   // I4715
begin
  FWin8Languages.Refresh;

  FActiveItemType := lsitUnknown;
  FActiveLangID := 0;
  FActiveHKL := 0;

  for I := 0 to FLanguages.Count - 1 do
    for j := 0 to FLanguages[i].KeyboardCount - 1 do
      if FLanguages[I].Keyboards[J].Active then
      begin
        FActiveLangID := FLanguages[i].LangID;
        FActiveItemType := FLanguages[i].Keyboards[j].ItemType;
        case FActiveItemType of
          lsitWinKeyboard:
            FActiveHKL := (FLanguages[i].Keyboards[j] as TLangSwitchKeyboard_WinKeyboard).FHKL;
          lsitTIP:
            begin
              with (FLanguages[i].Keyboards[j] as TLangSwitchKeyboard_TIP) do
              begin
                FActiveProfileGuid := FProfile.guidProfile;
                FActiveClsid := FProfile.clsid;
              end;
            end;
        end;
      end;

  FDefaultLanguage := nil;
  FLanguages.Clear;

//  for i :=0 to 5 do templang[i] := TLangSwitchLanguage.Create(Self, 0);   // I4715
//  for i :=0 to 15 do tempkbd[i] := TLangSwitchKeyboard_WinKeyboard.Create(Self, templang[0], 0);   // I4715

  EnumLanguages;
  EnumTSFKeyboards;
  FindDefaultLanguage;  // I2575
  RemoveEmptyLanguages; // I2867

  case FActiveItemType of
    lsitUnknown: ;
    lsitWinKeyboard: UpdateActive(FActiveLangID, FActiveItemType, FActiveHKL);
    lsitTIP: UpdateActive(FActiveLangID, FActiveItemType, FActiveClsid, FActiveProfileGuid);
  end;

//  for i :=0 to 5 do templang[i].Free;   // I4715
//  for i :=0 to 15 do tempkbd[i].Free;   // I4715
end;

procedure TLangSwitchManager.RemoveEmptyLanguages;  // I2867
var
  I: Integer;
begin
  for I := FLanguages.Count - 1 downto 0 do
    if FLanguages[I].KeyboardCount = 0 then
      FLanguages.Delete(I);
end;

procedure TLangSwitchManager.SetActiveItem(Item: TLangSwitchKeyboard);   // I3933
var
  i, j: Integer;
begin
  for i := 0 to FLanguages.Count - 1 do
    for j := 0 to FLanguages[i].KeyboardCount - 1 do
      FLanguages[i].Keyboards[j].FActive :=
        FLanguages[i].Keyboards[j] = Item;
end;

procedure TLangSwitchManager.UpdateActive(FLangID: Cardinal; FActiveItemType: TLangSwitchItemType;
  FHKL: HKL);   // I3933
var
  i: Integer;
  j: Integer;
begin
  Assert(FActiveItemType = lsitWinKeyboard);

  //OutputDebugString(PWideChar('TLangSwitchManager.UpdateActive('+IntToStr(FLangID)+'|'+IntToHex(FHKL,8)+')'#13#10));

  for i := 0 to FLanguages.Count - 1 do
    for j := 0 to FLanguages[i].KeyboardCount - 1 do
    begin
      FLanguages[i].Keyboards[j].FActive :=
        (FLanguages[i].LangID = FLangID) and
        (FLanguages[i].Keyboards[j].ItemType = lsitWinKeyboard) and
        ((FLanguages[i].Keyboards[j] as TLangSwitchKeyboard_WinKeyboard).FHKL = FHKL);

      //OutputDebugString(PWideChar('  TLangSwitchManager.UpdateActive -> '+IntToStr(i)+','+IntToStr(j)+' => '+IfThen(FLanguages[i].Keyboards[j].FActive,'True','False')+#13#10));
    end;
end;

procedure TLangSwitchManager.UpdateActive(FLangID: Cardinal; FActiveItemType: TLangSwitchItemType;
  FClsid, FProfileGuid: TGUID);   // I3933
var
  i: Integer;
  j: Integer;
begin
  Assert(FActiveItemType = lsitTIP);

  //OutputDebugString(PWideChar('TLangSwitchManager.UpdateActive('+IntToStr(FLangID)+'|'+GuidToString(FClsID)+'|'+GuidToString(FProfileGuid)+')'#13#10));

  for i := 0 to FLanguages.Count - 1 do
    for j := 0 to FLanguages[i].KeyboardCount - 1 do
    begin
      FLanguages[i].Keyboards[j].FActive :=
        (FLanguages[i].LangID = FLangID) and
        (FLanguages[i].Keyboards[j].ItemType = lsitTip) and
        IsEqualGuid((FLanguages[i].Keyboards[j] as TLangSwitchKeyboard_TIP).FProfile.clsid, FClsid) and
        IsEqualGuid((FLanguages[i].Keyboards[j] as TLangSwitchKeyboard_TIP).FProfile.guidProfile, FProfileGuid);
      //OutputDebugString(PWideChar('  TLangSwitchManager.UpdateActive -> '+IntToStr(i)+','+IntToStr(j)+' => '+IfThen(FLanguages[i].Keyboards[j].FActive,'True','False')+#13#10));
    end;
end;

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

    if Assigned(FDefaultLanguage) then
      FDefaultLanguage.SetDefaultKeyboard(FDefaultHKL);   // I5086
  end;
end;

function TLangSwitchManager.FindKeyboard(id: string): TLangSwitchKeyboard;
var
  i: Integer;
  j: Integer;
begin
  for i := 0 to FLanguages.Count - 1 do
    for j := 0 to FLanguages[i].KeyboardCount - 1 do
      if FLanguages[i].Keyboards[j].ID = id then
        Exit(FLanguages[i].Keyboards[j]);
  Result := nil;
end;

function TLangSwitchManager.FindKeyboard(FHKL: HKL;
  FProfileGuid: TGUID): TLangSwitchKeyboard;
var
  i: Integer;
  j: Integer;
begin
  for i := 0 to FLanguages.Count - 1 do
    for j := 0 to FLanguages[i].KeyboardCount - 1 do
    begin
      Result := FLanguages[i].Keyboards[j];
      if (FHKL = 0) and (Result is TLangSwitchKeyboard_TIP) then
      begin
        if FProfileGUID = (Result as TLangSwitchKeyboard_TIP).Profile.guidProfile then
          Exit;
      end else if (FHKL <> 0) and (Result is TLangSwitchKeyboard_WinKeyboard) then
      begin
        if FHKL = (Result as TLangSwitchKeyboard_WinKeyboard).HKL then
          Exit;
      end;
    end;

  Result := nil;
end;

{ TLangSwitchLanguage }

procedure TLangSwitchLanguage.Add(Item: TLangSwitchKeyboard);
begin
  FKeyboards.Add(Item);
end;

constructor TLangSwitchLanguage.Create(AManager: TLangSwitchManager; ALangID: DWORD);
var
  szLangName: array[0..MAX_PATH] of WideChar;
  i: Integer;
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

  // Allow override if found by Win8Language manager; name is not always cached
  if AManager.Win8Languages.IsSupported then
    for i := 0 to AManager.Win8Languages.Count - 1 do
      if AManager.Win8Languages[i].LangID = Integer(ALangID) then
      begin
        if AManager.Win8Languages[i].LocaleName <> '' then
          FCaption := AManager.Win8Languages[i].LocaleName;
        FIconText := UpperCase(Copy(AManager.Win8Languages[i].BCP47Tag, 1, 2));
        Exit;
      end;


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

procedure TLangSwitchLanguage.SetDefaultKeyboard(AHKL: HKL);   // I5086
var
  i: Integer;
begin
  for i := 1 to FKeyboards.Count - 1 do
    if FKeyboards[i].HasHKL(AHKL) then
    begin
      FKeyboards.Move(i, 0);
      Exit;
    end;
end;

{ TLangSwitchItem_WinKeyboard }

procedure TLangSwitchKeyboard_WinKeyboard.Activate(hwnd: THandle);
begin
//  Assert(FValid);   // I4715
  FManager.SetActiveItem(Self);   // I3933
  TDebugLogClient.Instance.WriteMessage('TLangSwitchKeyboard_WinKeyboard.Activate hwnd=%x keyboard=%x', [hwnd, FHKL]);   // I4674
  PostMessage(hwnd, wm_keyman_control_internal, KMCI_SELECTKEYBOARD, FHKL);   // I3933
end;

constructor TLangSwitchKeyboard_WinKeyboard.Create(AManager: TLangSwitchManager; ALanguage: TLangSwitchLanguage; AHkl: HKL); //; AActive: Boolean);   // I3961
begin
  inherited Create(AManager, ALanguage);   // I3961
  FHkl := AHkl;
  FItemType := lsitWinKeyboard;
  GetLayoutName;
end;

function TLangSwitchKeyboard_WinKeyboard.GetBitmap: Vcl.Graphics.TBitmap;
begin
  if not Assigned(FWinKeyboardBitmap) then
  begin
    FWinKeyboardBitmap := Vcl.Graphics.TBitmap.Create;
    FWinKeyboardBitmap.LoadFromResourceName(HInstance, 'LangSwitchManager_WinKeyboard');
    FWinKeyboardBitmap.Transparent := True;
  end;
  Result := FWinKeyboardBitmap;
end;

function TLangSwitchKeyboard_WinKeyboard.GetID: string;
begin
  Result := IntToHex(FHKL, 8);
end;

function TLangSwitchKeyboard_WinKeyboard.GetItemID: Integer;
begin
  Result := FHKL;
end;

function TLangSwitchKeyboard_WinKeyboard.GetKeymanLanguage: IKeymanLanguage;
var
  i: Integer;
begin
  for i := 0 to kmcom.Languages.Count - 1 do
    if kmcom.Languages[i].HKL = FHKL then
      Exit(kmcom.Languages[i]);
  Result := nil;
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
//    if HIWORD(kbd) > $F then
//      v := HIWORD(kbd)
//    else
      v := kbd;   // I4713
    if OpenKeyReadOnly('\'+SRegKey_KeyboardLayouts_LM+'\'+IntToHex(v, 8)) then
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

function TLangSwitchKeyboard_WinKeyboard.HasHKL(AHKL: HKL): Boolean;   // I5086
begin
  Result := AHKL = FHKL;
end;

{ TLangSwitchItem_TIP }

procedure TLangSwitchKeyboard_TIP.Activate(hwnd: THandle);
var
  skb: TSelectKeyboardBuffer;
  FIndex: DWORD;
begin
  FManager.SetActiveItem(Self);   // I3933
  skb.LangID := FProfile.langid;
  skb.CLSID := FProfile.clsid;
  skb.GUIDProfile := FProfile.guidProfile;
  FIndex := TSharedBufferManager.Identity.WriteSelectKeyboardBuffer(skb);
  TDebugLogClient.Instance.WriteMessage('TLangSwitchKeyboard_TIP.Activate identity=%d hwnd=%x keyboard=%s %s', [
    FIndex, hwnd, GuidToString(skb.CLSID), GuidToString(skb.GUIDProfile)]);   // I4674
  PostMessage(hwnd, wm_keyman_control_internal, KMCI_SELECTKEYBOARD_TSF, FIndex);   // I3933
end;

function ExpandRegString(sz: string): string;   // I3950
var
  buf: array[0..260] of char;
begin
  if ExpandEnvironmentStrings(PChar(sz), buf, Length(buf)) = 0
    then Result := ''
    else Result := buf;
end;

constructor TLangSwitchKeyboard_TIP.Create(AManager: TLangSwitchManager; ALanguage: TLangSwitchLanguage; AProfile: TF_INPUTPROCESSORPROFILE);   // I3961
var
  FIconFile: string;
  FIconIndex: Integer;
  FIconHandleLarge: HICON;
  FIconHandleSmall: HICON;
  i: Integer;
  kbd: IKeymanKeyboardInstalled;
  lang: IKeymanKeyboardLanguageInstalled;
  j: Integer;
  Found: Boolean;
begin
  inherited Create(AManager, ALanguage);   // I3961

  FProfile := AProfile;
  FItemType := lsitTIP;   // I3933
  if not Succeeded(FManager.FProfiles.GetLanguageProfileDescription(FProfile.clsid, FProfile.langid, FProfile.guidProfile, FCaption)) then
    Exit; // FValid will be false   // I4207

  FValid := True;

  { Get Keyman-specific details }

  if IsEqualGUID(FProfile.clsid, c_clsidKMTipTextService) then   // I3949   // I4248
  begin
    Found := False;
    for i := 0 to kmcom.Keyboards.Count - 1 do
    begin
      kbd := kmcom.Keyboards[i];
      for j := 0 to kbd.Languages.Count - 1 do
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
  end;

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

            FIconHandle := FIconHandleSmall;   // I4202

            if FIconHandleSmall <> 0 then
            begin
              FBitmap := Vcl.Graphics.TBitmap.Create;
              FBitmap.Width := 16;
              FBitmap.Height := 16;
              FBitmap.PixelFormat := pf24Bit;
              FBitmap.Transparent := False;
              DrawIconEx(FBitmap.Canvas.Handle, 0, 0, FIconHandleSmall, 16, 16, 0, 0, DI_NORMAL);
//              DestroyIcon(FIconHandleSmall);
            end;
          end;
        end;
      end;
    end;
  finally
    Free;
  end;
end;

destructor TLangSwitchKeyboard_TIP.Destroy;
begin
  if FIconHandle <> 0 then DestroyIcon(FIconHandle);   // I4202
  FIconHandle := 0;   // I4715
  inherited Destroy;
end;

function TLangSwitchKeyboard_TIP.GetID: string;
begin
  Result := Format('%04.4x:%s:%s',
    [FProfile.langid, GuidToString(FProfile.clsid), GuidToString(FProfile.guidProfile)]);
end;

function TLangSwitchKeyboard_TIP.GetItemID: Integer;
begin
  Result := 0;
end;

function TLangSwitchKeyboard_TIP.GetKeymanLanguage: IKeymanLanguage;
var
  i: Integer;
begin
  for i := 0 to kmcom.Languages.Count - 1 do
    if kmcom.Languages[i].ProfileGUID = FProfile.guidProfile then
      Exit(kmcom.Languages[i]);
  Result := nil;
end;

function TLangSwitchKeyboard_TIP.HasHKL(AHKL: HKL): Boolean;   // I5086
begin
  Result := AHKL = FProfile.HKL;
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

{ TLangSwitchConfiguration }

constructor TLangSwitchConfiguration.Create;
begin
  inherited Create;
  LoadCurrentHotkey;
  DisableWindowsHotkey;
end;

destructor TLangSwitchConfiguration.Destroy;
begin
  RestoreWindowsHotkey;
  inherited Destroy;
end;

procedure TLangSwitchConfiguration.LoadCurrentHotkey;
    function FindHotkey(Hotkeys: IKeymanHotkeys): DWord;
    var
      j: Integer;
    begin
      for j := 0 to Hotkeys.Count - 1 do
        if Hotkeys[j].Target = khLanguageSwitch then
        begin
          Result := Hotkeys[j].RawValue;
          Exit;
        end;
      Result := 0;
    end;
begin
  FCurrentHotkey := FindHotkey(kmcom.Hotkeys);
end;

function TLangSwitchConfiguration.FixRegistryDataType(
  RegistryKey: TRegistryErrorControlled;
  const ValueName: string
): Boolean;
var
  RegType: TRegDataType;
  const CHotkeyNotAssigned = '3';
begin
  Result := False;

  if RegistryKey.ValueExists(ValueName) then
  begin
    RegType := RegistryKey.GetDataType(ValueName);
    if not ((RegType = rdString) or (RegType = rdExpandString)) then
    begin
      RegistryKey.DeleteValue(ValueName);
      RegistryKey.WriteString(ValueName, CHotkeyNotAssigned);
      Result := True;
    end;
  end;

end;

procedure TLangSwitchConfiguration.DisableWindowsHotkey;
var
  MatchValue: string;
  KeyboardToggleReg: TRegistryErrorControlled;
  FReset: Boolean;
  const CHotkeyNotAssigned = '3';
begin
  if FCurrentHotkey = (HK_ALT or HK_SHIFT) then MatchValue := '1'
  else if FCurrentHotkey = (HK_CTRL or HK_SHIFT) then MatchValue := '2'
  else Exit;

  FLanguageToggle := CHotkeyNotAssigned;
  FLayoutToggle := CHotkeyNotAssigned;

  FReset := False;

  KeyboardToggleReg := TRegistryErrorControlled.Create; // I2890
  try
    if not KeyboardToggleReg.OpenKey(SRegKey_KeyboardLayoutToggle, True) then  // I2890
      KeyboardToggleReg.RaiseLastRegistryError;
    // Fix potential corrupted registry keys
    if FixRegistryDataType(KeyboardToggleReg, SRegValue_Toggle_Hotkey) then
      FReset := True;
    if FixRegistryDataType(KeyboardToggleReg, SRegValue_Toggle_LanguageHotkey) then
      FReset := True;
    if FixRegistryDataType(KeyboardToggleReg, SRegValue_Toggle_LayoutHotkey) then
      FReset := True;

    if KeyboardToggleReg.ValueExists(SRegValue_Toggle_Hotkey) then
      FLanguageToggle := KeyboardToggleReg.ReadString(SRegValue_Toggle_Hotkey);
    if FLanguageToggle = MatchValue then
    begin
      KeyboardToggleReg.WriteString(SRegValue_Toggle_Hotkey, CHotkeyNotAssigned);
      KeyboardToggleReg.WriteString(SRegValue_Toggle_LanguageHotkey, CHotkeyNotAssigned);
      FReset := True;
    end;

    if KeyboardToggleReg.ValueExists(SRegValue_Toggle_LayoutHotkey) then
      FLayoutToggle := KeyboardToggleReg.ReadString(SRegValue_Toggle_LayoutHotkey);

    if FLayoutToggle = MatchValue then
    begin
      KeyboardToggleReg.WriteString(SRegValue_Toggle_LayoutHotkey, CHotkeyNotAssigned);
      FReset := True;
    end;
  finally
    KeyboardToggleReg.Free;
  end;

  if FReset then SystemParametersInfo(SPI_SETLANGTOGGLE, 0, nil, 0);
end;

procedure TLangSwitchConfiguration.RestoreWindowsHotkey;
var
  FReset: Boolean;
  KeyboardToggleReg: TRegistryErrorControlled;
begin
  FReset := False;

  if FLanguageToggle = '' then Exit;

  KeyboardToggleReg := TRegistryErrorControlled.Create; // I2890
  try
    if not KeyboardToggleReg.OpenKey(SRegKey_KeyboardLayoutToggle, True) then  // I2890
      KeyboardToggleReg.RaiseLastRegistryError;

    // Fix potential corrupted registry keys
    if FixRegistryDataType(KeyboardToggleReg, SRegValue_Toggle_Hotkey) then
      FReset := True;
    if FixRegistryDataType(KeyboardToggleReg, SRegValue_Toggle_LanguageHotkey) then
      FReset := True;
    if FixRegistryDataType(KeyboardToggleReg, SRegValue_Toggle_LayoutHotkey) then
      FReset := True;

    if not KeyboardToggleReg.ValueExists(SRegValue_Toggle_Hotkey) or
      (KeyboardToggleReg.ReadString(SRegValue_Toggle_Hotkey) <> FLanguageToggle) then
    begin
      KeyboardToggleReg.WriteString(SRegValue_Toggle_Hotkey, FLanguageToggle);
      KeyboardToggleReg.WriteString(SRegValue_Toggle_LanguageHotkey, FLanguageToggle);
      FReset := True;
    end;

    if not KeyboardToggleReg.ValueExists(SRegValue_Toggle_LayoutHotkey) or
      (KeyboardToggleReg.ReadString(SRegValue_Toggle_LayoutHotkey) <> FLayoutToggle) then
    begin
      KeyboardToggleReg.WriteString(SRegValue_Toggle_LayoutHotkey, FLayoutToggle);
      FReset := True;
    end;
  finally
    KeyboardToggleReg.Free;
  end;

  if FReset then
    SystemParametersInfo(SPI_SETLANGTOGGLE, 0, nil, 0);
end;

procedure TLangSwitchConfiguration.Refresh;
begin
  RestoreWindowsHotkey;
  FLanguageToggle := '';
  FLayoutToggle := '';
  LoadCurrentHotkey;
  DisableWindowsHotkey;
end;

{ TLangSwitchKeyboard }

constructor TLangSwitchKeyboard.Create(AManager: TLangSwitchManager; ALanguage: TLangSwitchLanguage);   // I3961
begin
  inherited Create(AManager);
  FKeymanID := KEYMANID_NONKEYMAN;   // I3949
  FLanguage := ALanguage;   // I3961
end;

destructor TLangSwitchKeyboard.Destroy;
begin
  FreeAndNil(FBitmap);
  FValid := False;   // I4715

  inherited Destroy;
end;

function TLangSwitchKeyboard.GetBitmap: Vcl.Graphics.TBitmap;
begin
  Result := FBitmap;
end;

initialization
finalization
  FreeAndNil(FWinKeyboardBitmap);
end.
