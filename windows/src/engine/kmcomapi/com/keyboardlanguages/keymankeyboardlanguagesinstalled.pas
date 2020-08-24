(*
  Name:             keymankeyboardlanguagesinstalled
  Copyright:        Copyright (C) SIL International.
  Documentation:    
  Description:      
  Create Date:      11 Aug 2013

  Modified Date:    4 Mar 2015
  Authors:          mcdurdin
  Related Files:    
  Dependencies:     

  Bugs:             
  Todo:             
  Notes:            
  History:          11 Aug 2013 - mcdurdin - I3768 - V9.0 - Remove TSF substitution code
                    16 Apr 2014 - mcdurdin - I4169 - V9.0 - Mnemonic layouts should be recompiled to positional based on user-selected base keyboard
                    04 Mar 2015 - mcdurdin - I4607 - V9.0 - Support install of keyboard against fallback locales
*)
unit keymankeyboardlanguagesinstalled;

interface

uses
  System.Win.ComObj,
  System.Win.StdVcl,
  Winapi.ActiveX,
  Winapi.Windows,

  keymanapi_TLB,
  keymanautoobject,
  KeymanContext,
  keymanerrorcodes,
  internalinterfaces;

type
  TKeymanKeyboardLanguageList = TAutoObjectList;

  TKeymanKeyboardLanguagesInstalled = class(TKeymanAutoCollectionObject, IKeymanKeyboardLanguagesInstalled, IKeymanKeyboardLanguagesInstalled2)   // I4169
  private
    FLanguages: TKeymanKeyboardLanguageList;
    FOwner: IKeymanKeyboardInstalled;
  protected
    procedure DoRefresh; override;

    { IKeymanKeyboardLanguagesInstalled }
    function Get_Items(Index: Integer): IKeymanKeyboardLanguageInstalled; safecall;

    procedure Install(const BCP47Code: WideString); safecall;
    procedure InstallByLangID(LangID: Integer); safecall;
    function IndexOfBCP47Code(const BCP47Code: string): Integer;

    { IKeymanKeyboardLanguagesInstalled2 }
    function Add(const BCP47Tag: WideString): IKeymanKeyboardLanguageInstalled; safecall;

  public
    constructor Create(AContext: TKeymanContext; AOwner: IKeymanKeyboardInstalled);
    destructor Destroy; override;
  end;

implementation

uses
  System.Classes,
  System.SysUtils,
  System.Variants,

  ErrorControlledRegistry,

  Keyman.System.LanguageCodeUtils,
  Keyman.System.Process.KPInstallKeyboardLanguage,
  BCP47Tag,
  keymankeyboardlanguageinstalled,
  KLog,
  RegistryKeys,
  utilkeyman,
  utiltsf;

{ TKeymanKeyboardLanguagesInstalled }

function TKeymanKeyboardLanguagesInstalled.Add(
  const BCP47Tag: WideString): IKeymanKeyboardLanguageInstalled;
var
  FKeyboardLanguage: TKeymanKeyboardLanguageInstalled;
  FCanonicalBCP47Tag: string;
  i: Integer;
begin
  // This adds an in-memory item to the array so that it can be installed
  FCanonicalBCP47Tag := TBCP47Tag.GetCanonicalTag(BCP47Tag);
  for i := 0 to FLanguages.Count - 1 do
    if SameText((FLanguages[i] as IKeymanKeyboardLanguageInstalled).BCP47Code, FCanonicalBCP47Tag) then
      Exit(nil);

  FKeyboardLanguage := TKeymanKeyboardLanguageInstalled.Create(Context, FOwner, FCanonicalBCP47Tag, BCP47Tag, 0, GUID_NULL, BCP47Tag, False);
  FLanguages.Add(FKeyboardLanguage);
  Result := FKeyboardLanguage;
end;

constructor TKeymanKeyboardLanguagesInstalled.Create(AContext: TKeymanContext; AOwner: IKeymanKeyboardInstalled);
begin
  _SetContext(AContext);
  FOwner := AOwner;
  FLanguages := TKeymanKeyboardLanguageList.Create;
  inherited Create(AContext, IKeymanKeyboardLanguagesInstalled, FLanguages);
  Refresh;
end;

destructor TKeymanKeyboardLanguagesInstalled.Destroy;
begin
  FLanguages.Free;
  inherited Destroy;
end;

procedure TKeymanKeyboardLanguagesInstalled.DoRefresh;


  procedure RefreshProfiles;
  var
    FProfiles: TStringList;
    i: Integer;
    FKeyboardLanguage: TKeymanKeyboardLanguageInstalled;
    RootPath: string;
    FLangID: Integer;
    FName, FLocale: string;
    FGUID: TGUID;
    FCanonicalBCP47Tag: string;
    regLM: TRegistryErrorControlled;
    FIsInstalled: Boolean;
  begin
    FProfiles := TStringList.Create;
    regLM := TRegistryErrorControlled.Create(KEY_READ);
    try
      regLM.RootKey := HKEY_LOCAL_MACHINE;
      RootPath := GetRegistryKeyboardInstallKey_LM(FOwner.ID) + SRegSubKey_LanguageProfiles;
      if regLM.OpenKeyReadOnly(RootPath) then
      begin
        regLM.GetKeyNames(FProfiles);
        for i := 0 to FProfiles.Count - 1 do
        begin
          if regLM.OpenKeyReadOnly('\'+RootPath+'\'+FProfiles[i]) and
            regLM.ValueExists(SRegValue_LanguageProfileLangID) and
            regLM.ValueExists(SRegValue_LanguageProfileLocale) and
            regLM.ValueExists(SRegValue_KeymanProfileGUID) then
          begin
            FLangID := regLM.ReadInteger(SRegValue_LanguageProfileLangID);
            FLocale := regLM.ReadString(SRegValue_LanguageProfileLocale);
            FGUID := StringToGUID(regLM.ReadString(SRegValue_KeymanProfileGUID));
            if regLM.ValueExists(SRegValue_LanguageProfileName)
              then FName := regLM.ReadString(SRegValue_LanguageProfileName)
              else FName := '';

            FCanonicalBCP47Tag := TBCP47Tag.GetCanonicalTag(FLocale);
            if IndexOfBCP47Code(FCanonicalBCP47Tag) < 0 then
            begin
              // TODO: move IsTIPInstalledForCurrentUser to TKeymanKeyboardLanguageInstalled.Create
              FIsInstalled := IsTIPInstalledForCurrentUser(FCanonicalBCP47Tag, FLangID, FGUID);
              FKeyboardLanguage := TKeymanKeyboardLanguageInstalled.Create(Context, FOwner, FCanonicalBCP47Tag, FLocale, FLangID, FGUID, FName, FIsInstalled);
              FLanguages.Add(FKeyboardLanguage);
            end;
          end;
        end;
      end;
    finally
      regLM.Free;
      FProfiles.Free;
    end;
  end;

  function HasLanguage(BCP47: string): Boolean;
  begin
    Result := IndexOfBCP47Code(BCP47) >= 0;
  end;

  procedure RefreshSuggestedLanguages;
  var
    RootPath: string;
    FIDs: TStringList;
    i: Integer;
    FName: string;
    FKeyboardLanguage: TKeymanKeyboardLanguageInstalled;
    FCanonicalBCP47Tag: string;
    reg: TRegistryErrorControlled;
  begin
    FIDs := TStringList.Create;
    reg := TRegistryErrorControlled.Create(KEY_READ);
    try
      reg.RootKey := HKEY_LOCAL_MACHINE;
      RootPath := GetRegistryKeyboardInstallKey_LM(FOwner.ID) + '\' + SRegSubKey_SuggestedLanguages;
      if reg.OpenKeyReadOnly(RootPath) then
      begin
        reg.GetValueNames(FIDs);
        for i := 0 to FIDs.Count - 1 do
        begin
          FCanonicalBCP47Tag := TBCP47Tag.GetCanonicalTag(FIDs[i]);
          if not HasLanguage(FIDs[i]) and not HasLanguage(FCanonicalBCP47Tag) then
          begin
            FName := reg.ReadString(FIDs[i]);
            FKeyboardLanguage := TKeymanKeyboardLanguageInstalled.Create(Context, FOwner, FCanonicalBCP47Tag, FIDs[i], 0, GUID_NULL, FName, False);
            FLanguages.Add(FKeyboardLanguage);
          end;
        end;
      end;
    finally
      reg.Free;
      FIDs.Free;
    end;
  end;

begin
  KL.MethodEnter(Self, 'DoRefresh', []);
  try
    RefreshProfiles;
    RefreshSuggestedLanguages;
  finally
    KL.MethodExit(Self, 'DoRefresh');
  end;
end;

function TKeymanKeyboardLanguagesInstalled.Get_Items(Index: Integer): IKeymanKeyboardLanguageInstalled;
begin
  if (Index < Get_Count) and (Index >= 0) then
    Result := FLanguages[Index] as IKeymanKeyboardLanguageInstalled
  else
    ErrorFmt(KMN_E_Collection_InvalidIndex, VarArrayOf([VarToStr(Index)]));
end;

function TKeymanKeyboardLanguagesInstalled.IndexOfBCP47Code(
  const BCP47Code: string): Integer;
var
  i: Integer;
begin
  for i := 0 to FLanguages.Count - 1 do
    if SameText(Get_Items(i).BCP47Code, BCP47Code) then
      Exit(i);
  Result := -1;
end;

procedure TKeymanKeyboardLanguagesInstalled.Install(const BCP47Code: WideString);
var
  Tag: string;
  lang: IKeymanKeyboardLanguageInstalled2;
  TemporaryKeyboardID: WideString;
  RegistrationRequired: WordBool;
  LangID: Integer;
begin
  Tag := TBCP47Tag.GetCanonicalTag(BCP47Code);
  if IndexOfBCP47Code(Tag) >= 0 then
    // Already installed; should we warn?
    Exit;

  lang := Add(Tag) as IKeymanKeyboardLanguageInstalled2;
  if lang.FindInstallationLangID(LangID, TemporaryKeyboardID, RegistrationRequired, ilkInstallTransitoryLanguage) then
  begin
    lang.InstallTip(LangID, TemporaryKeyboardID);
  end;
end;

procedure TKeymanKeyboardLanguagesInstalled.InstallByLangID(LangID: Integer);
var
  Tag: string;
  lang: IKeymanKeyboardLanguageInstalled2;
begin
  Tag := TLanguageCodeUtils.TranslateWindowsLanguagesToBCP47(LangID);
  if (Tag = '') or (IndexOfBCP47Code(Tag) >= 0) then
    // Already installed; should we warn?
    Exit;

  lang := Add(Tag) as IKeymanKeyboardLanguageInstalled2;
  lang.InstallTip(LangID, '');
end;


end.
