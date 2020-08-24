(*
  Name:             keymankeyboardlanguageinstalled
  Copyright:        Copyright (C) 2003-2017 SIL International.
  Documentation:    
  Description:      
  Create Date:      16 Apr 2014

  Modified Date:    4 Nov 2014
  Authors:          mcdurdin
  Related Files:    
  Dependencies:     

  Bugs:             
  Todo:             
  Notes:            
  History:          16 Apr 2014 - mcdurdin - I4169 - V9.0 - Mnemonic layouts should be recompiled to positional based on user-selected base keyboard
                    17 Aug 2014 - mcdurdin - I4376 - V9.0 - Unticked keyboards in configuration should be removed from language profile
                    04 Nov 2014 - mcdurdin - I4494 - Crash calling TSF [CrashID:kmshell.exe_9.0.473.0_2C45D42D_EOleSysError]
*)
unit keymankeyboardlanguageinstalled;   // I4169

interface

uses
  Winapi.Windows,
  Winapi.ActiveX,
  System.Classes,

  keymanautoobject,
  keymanapi_TLB,
  keymankeyboardlanguage,
  internalinterfaces,
  keymancontext,
  msctf;

type
  TKeymanKeyboardLanguageInstalled = class(TKeymanKeyboardLanguage, IKeymanKeyboardLanguageInstalled, IKeymanKeyboardLanguageInstalled2, IIntKeymanKeyboardLanguage)   // I4376
  private
    FOwner: IKeymanKeyboardInstalled;
    FProfileGUID: TGUID;
    FIsInstalled: Boolean;
  protected
    function Serialize(Flags: TOleEnum; const ImagePath: WideString; References: TStrings): WideString;
      override;

    { IKeymanKeyboardLanguageInstalled }
    function Get_OwnerKeyboard: IKeymanKeyboardInstalled; safecall;
    function Get_ProfileGUID: TGUID; safecall;
    function Get_IsInstalled: WordBool; safecall;

    procedure Install; safecall;
    procedure Uninstall; safecall;

    { IKeymanKeyboardLanguageInstalled2 }
    function FindInstallationLangID(out LangID: Integer;
      out TemporaryKeyboardID: WideString; out RegistrationRequired: WordBool;
      Flags: tagKeymanInstallFlags): WordBool; safecall;
    procedure InstallTip(LangID: Integer;
      const TemporaryKeyboardToRemove: WideString); safecall;
    procedure RegisterTip(LangID: Integer); safecall;
    function Get_IsRegistered: WordBool; safecall;

    { IIntKeymanKeyboardLanguageInstalled }
  public
    constructor Create(AContext: TKeymanContext; AOwner: IKeymanKeyboardInstalled; const AOriginalBCP47Code, ABCP47Code: string;
      ALangID: Integer; AProfileGUID: TGUID; const AName: string);
  end;

implementation

uses
  System.SysUtils,
  System.Variants,
  System.Win.ComObj,

  glossary,
  keymanerrorcodes,
  Keyman.System.Process.KPInstallKeyboardLanguage,
  Keyman.System.Process.KPUninstallKeyboardLanguage,
  utiltsf,
  utilxml;

{ TKeymanKeyboardLanguageInstalled }

constructor TKeymanKeyboardLanguageInstalled.Create(AContext: TKeymanContext;
  AOwner: IKeymanKeyboardInstalled; const AOriginalBCP47Code, ABCP47Code: string;
  ALangID: Integer; AProfileGUID: TGUID; const AName: string);
begin
  FOwner := AOwner;
  FProfileGUID := AProfileGUID;
  FIsInstalled := IsTIPInstalledForCurrentUser(aBCP47Code, ALangID, AProfileGUID);
  inherited Create(AContext, AOwner, AOriginalBCP47Code, ABCP47Code, ALangID, AName);
end;

function TKeymanKeyboardLanguageInstalled.FindInstallationLangID(
  out LangID: Integer; out TemporaryKeyboardID: WideString;
  out RegistrationRequired: WordBool; Flags: tagKeymanInstallFlags): WordBool;
var
  kp: TKPInstallKeyboardLanguage;
  s: string;
  KPFlags: TKPInstallKeyboardLanguageFlags;
begin
  LangID := Self.Get_LangID;
  TemporaryKeyboardID := '';
  RegistrationRequired := False;

  if LangID <> 0 then
    Exit(True);

  RegistrationRequired := True;
  kp := TKPInstallKeyboardLanguage.Create(Context);
  try
    KPFlags := [];
    if (Flags and kifInstallTransitoryLanguage) <> 0 then
      Include(KPFlags, ilkInstallTransitoryLanguage);
    Result := kp.FindInstallationLangID(Self.Get_BCP47Code, LangID, s, KPFlags);
    TemporaryKeyboardID := s;
  finally
    kp.Free;
  end;
end;

procedure TKeymanKeyboardLanguageInstalled.RegisterTip(LangID: Integer);
var
  kp: TKPInstallKeyboardLanguage;
begin
  kp := TKPInstallKeyboardLanguage.Create(Context);
  try
    kp.RegisterTip(FOwner.ID, Get_BCP47Code, FOwner.Name, LangID, FOwner.IconFilename, GetLangName);
  finally
    kp.Free;
  end;
end;

procedure TKeymanKeyboardLanguageInstalled.InstallTip(LangID: Integer;
  const TemporaryKeyboardToRemove: WideString);
var
  kp: TKPInstallKeyboardLanguage;
begin
  kp := TKPInstallKeyboardLanguage.Create(Context);
  try
    kp.InstallTip(FOwner.ID, Get_BCP47Code, LangID);
    if TemporaryKeyboardToRemove <> '' then
      kp.UninstallTemporaryLayout(TemporaryKeyboardToRemove);
  finally
    kp.Free;
  end;
end;

function TKeymanKeyboardLanguageInstalled.Get_IsInstalled: WordBool;
begin
  Result := FIsInstalled;
end;

function TKeymanKeyboardLanguageInstalled.Get_IsRegistered: WordBool;
begin
  Result := not IsEqualGuid(FProfileGUID, GUID_NULL);
end;

function TKeymanKeyboardLanguageInstalled.Get_OwnerKeyboard: IKeymanKeyboardInstalled;
begin
  Result := FOwner;
end;

function TKeymanKeyboardLanguageInstalled.Get_ProfileGUID: TGUID;
begin
  Result := FProfileGUID;
end;

procedure TKeymanKeyboardLanguageInstalled.Install;
begin
  if not Get_IsInstalled then
    with TKPInstallKeyboardLanguage.Create(Context) do
    try
      InstallTip(FOwner.ID, Get_BCP47Code, Get_LangID);
    finally
      Free;
    end;
end;

function TKeymanKeyboardLanguageInstalled.Serialize(Flags: TOleEnum;
  const ImagePath: WideString; References: TStrings): WideString;
begin
  Result := XMLFormat([
    'langid', IntToHex(Get_LangID, 4),
    'profileguid', GUIDToString(FProfileGUID),
    'bcp47code', Get_BCP47Code,
    'langname', GetLangName,
    'isinstalled', Get_IsInstalled,
    'isregistered', Get_IsRegistered
  ]);
end;

procedure TKeymanKeyboardLanguageInstalled.Uninstall;
begin
  if Get_IsInstalled then
    with TKPUninstallKeyboardLanguage.Create(Context) do
    try
      if IsTransientLanguageID(Get_LangID)
        then UninstallTip(FOwner.ID, Get_LangID)
        else UninstallTip(FOwner.ID, Get_BCP47Code);
    finally
      Free;
    end;
end;

end.
