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
    FWindowsBCP47Code: string;
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
    function Get_WindowsBCP47Code: WideString; safecall;

    { IIntKeymanKeyboardLanguageInstalled }
    procedure Disable;
  public
    constructor Create(AContext: TKeymanContext; AOwner: IKeymanKeyboardInstalled; const ABCP47Code: string;
      ALangID: Integer; AProfileGUID: TGUID; const AName: string);
  end;

implementation

uses
  System.SysUtils,
  System.Variants,
  System.Win.ComObj,
  System.Win.Registry,

  glossary,
  keymanerrorcodes,
  Keyman.System.Process.KPInstallKeyboardLanguage,
  Keyman.System.Process.KPUninstallKeyboardLanguage,
  RegistryKeys,
  utiltsf,
  utilxml;

{ TKeymanKeyboardLanguageInstalled }

constructor TKeymanKeyboardLanguageInstalled.Create(AContext: TKeymanContext;
  AOwner: IKeymanKeyboardInstalled; const ABCP47Code: string;
  ALangID: Integer; AProfileGUID: TGUID; const AName: string);

  function IsLanguageInstalledForKeyboard: Boolean;
  var
    reg: TRegistry;
  begin
    reg := TRegistry.Create(KEY_READ);
    try
      Result :=
        reg.OpenKeyReadOnly(BuildKeyboardLanguagesKey_CU(FOwner.ID)) and
        reg.ValueExists(ABCP47Code);
    finally
      reg.Free;
    end;
  end;

begin
  FOwner := AOwner;
  FProfileGUID := AProfileGUID;
  FWindowsBCP47Code := ABCP47Code;

  // Note, if the TIP is installed, the WindowsBCP47Code may be modified to
  // match the canonicalization that Windows gives it. The primary difference
  // will usually be that a region code may be stripped if Windows only
  // recognises a single region for the language. This data is not available to
  // us, though, so we just have to rely on what Windows reports back to us.

  // We test both if the TIP is installed, and if it is expected to be installed
  // according to Keyman's registry settings. This caters for when a user adds the
  // keyboard outside of Keyman (IsTIPInstalledForCurrentUser), and when a keyboard
  // is installed but not loaded (IsLanguageInstalledForKeyboard).
  FIsInstalled :=
    IsTIPInstalledForCurrentUser(FWindowsBCP47Code, ALangID, AProfileGUID) or
    IsLanguageInstalledForKeyboard;

  inherited Create(AContext, AOwner, ABCP47Code, ALangID, AName);
end;

procedure TKeymanKeyboardLanguageInstalled.Disable;
begin
  if Get_IsInstalled then
    with TKPUninstallKeyboardLanguage.Create(Context) do
    try
      UninstallTip(FOwner.ID, Get_LangID, Get_ProfileGUID, False);
      if IsTransientLanguageID(Get_LangID) then
      begin
        SetLangID(0);
        FProfileGUID := GUID_NULL;
      end;
    finally
      Free;
    end;
end;

function TKeymanKeyboardLanguageInstalled.FindInstallationLangID(
  out LangID: Integer; out TemporaryKeyboardID: WideString;
  out RegistrationRequired: WordBool; Flags: tagKeymanInstallFlags): WordBool;
var
  kp: TKPInstallKeyboardLanguage;
  BCP47Code, s: string;
  KPFlags: TKPInstallKeyboardLanguageFlags;
begin
  LangID := Self.Get_LangID;
  TemporaryKeyboardID := '';
  RegistrationRequired := False;

  if (LangID <> 0) then
    Exit(True);

  kp := TKPInstallKeyboardLanguage.Create(Context);
  try
    KPFlags := [];
    if (Flags and kifInstallTransientLanguage) <> 0 then
      Include(KPFlags, ilkInstallTransientLanguage);
    BCP47Code := Self.Get_BCP47Code;
    Result := kp.FindInstallationLangID(BCP47Code, LangID, s, KPFlags);

    // We only need to register a TIP for user custom installations of languages:
    // languages that are suggested already have a TIP registered, and the
    // four transient language codes should have TIPs registered at install time.
    RegistrationRequired :=
      not IsTransientLanguageID(LangID) and
      not Self.Get_IsRegistered;

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
  guid: TGUID;
begin
  kp := TKPInstallKeyboardLanguage.Create(Context);
  try
    kp.InstallTip(FOwner.ID, Get_BCP47Code, LangID, guid);
    if TemporaryKeyboardToRemove <> '' then
      kp.UninstallTemporaryLayout(TemporaryKeyboardToRemove);
    Self.SetLangID(LangID);
    Self.FProfileGUID := guid;
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

function TKeymanKeyboardLanguageInstalled.Get_WindowsBCP47Code: WideString;
begin
  Result := FWindowsBCP47Code;
end;

procedure TKeymanKeyboardLanguageInstalled.Install;
var
  guid: TGUID;
begin
  if not Get_IsInstalled then
    with TKPInstallKeyboardLanguage.Create(Context) do
    try
      InstallTip(FOwner.ID, Get_BCP47Code, Get_LangID, guid);
      Self.FProfileGUID := guid;
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
      UninstallTip(FOwner.ID, Get_LangID, Get_ProfileGUID, True);
    finally
      Free;
    end;
end;

end.
