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
  Windows, ActiveX, ComObj, keymanapi_TLB, StdVcl, keymanautoobject, KeymanContext,
  keymanerrorcodes, internalinterfaces;

type
  TKeymanKeyboardLanguageList = TAutoObjectList;

  TKeymanKeyboardLanguagesInstalled = class(TKeymanAutoCollectionObject, IKeymanKeyboardLanguagesInstalled)   // I4169
  private
    FLanguages: TKeymanKeyboardLanguageList;
    FOwner: IKeymanKeyboardInstalled;
  protected
    procedure DoRefresh; override;

    { IKeymanKeyboardLanguagesInstalled }
    function Get_Items(Index: Integer): IKeymanKeyboardLanguageInstalled; safecall;

    procedure Install(const BCP47Code: WideString); safecall;
    procedure InstallByLangID(LangID: Integer); safecall;
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

  keymankeyboardlanguageinstalled,
  KLog,
  kpinstallkeyboardlanguageprofiles,
  RegistryKeys,
  utilkeyman;

{ TKeymanKeyboardLanguagesInstalled }

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
var
  FProfiles: TStringList;
  i: Integer;
  FKeyboardLanguage: TKeymanKeyboardLanguageInstalled;
  RootPath: string;
  FLangID: Integer;
  FLocale: string;
  FGUID: TGUID;
begin
  KL.MethodEnter(Self, 'DoRefresh', []);
  try
    { Iterate through something somewhere and get the languages associated with this profile? }

    FProfiles := TStringList.Create;
    with TRegistryErrorControlled.Create do
    try
      RootKey := HKEY_LOCAL_MACHINE;  /// TODO: Is this correct? Do we ever have any
      RootPath := GetRegistryKeyboardInstallKey_LM(FOwner.ID) + SRegSubKey_LanguageProfiles;
      if OpenKeyReadOnly(RootPath) then
      begin
        GetKeyNames(FProfiles);
        for i := 0 to FProfiles.Count - 1 do
        begin
          if OpenKeyReadOnly('\'+RootPath+'\'+FProfiles[i]) and
            ValueExists(SRegValue_LanguageProfileLangID) and
            ValueExists(SRegValue_LanguageProfileLocale) and
            ValueExists(SRegValue_KeymanProfileGUID) then
          begin
            FLangID := ReadInteger(SRegValue_LanguageProfileLangID);
            FLocale := ReadString(SRegValue_LanguageProfileLocale);
            FGUID := StringToGUID(ReadString(SRegValue_KeymanProfileGUID));
            FKeyboardLanguage := TKeymanKeyboardLanguageInstalled.Create(Context, FOwner, FLocale, FLangID, FGUID);
            FLanguages.Add(FKeyboardLanguage);
          end;
        end;
      end;
    finally
      Free;
      FProfiles.Free;
    end;
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

procedure TKeymanKeyboardLanguagesInstalled.Install(const BCP47Code: WideString);
var
  FIconFileName: string;
begin
  with TKPInstallKeyboardLanguageProfiles.Create(Context) do
  try
    FIconFileName := GetKeyboardIconFileName(FOwner.Filename);
    if not FileExists(FIconFileName) then
      FIconFileName := '';
    Execute(FOwner.ID, FOwner.Name, BCP47Code, FIconFileName);   // I3768   // I4607
  finally
    Free;
  end;
end;

procedure TKeymanKeyboardLanguagesInstalled.InstallByLangID(LangID: Integer);
var
  FLangIDs: array of Integer;
  FIconFileName: string;
begin
  with TKPInstallKeyboardLanguageProfiles.Create(Context) do
  try
    FIconFileName := GetKeyboardIconFileName(FOwner.Filename);
    if not FileExists(FIconFileName) then
      FIconFileName := '';
    SetLength(FLangIDs, 1);
    FLangIDs[0] := LangID;
    Execute(FOwner.ID, FOwner.Name, FLangIDs, FIconFileName, True);   // I3768   // I4607
  finally
    Free;
  end;
end;


end.
