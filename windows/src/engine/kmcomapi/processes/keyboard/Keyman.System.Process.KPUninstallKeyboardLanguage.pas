unit Keyman.System.Process.KPUninstallKeyboardLanguage;

interface

uses
  System.Win.Registry,
  Winapi.Windows,

  kpbase,
  KeymanContext,
  Windows8LanguageList,
  Winapi.msctf,
  keyman_msctf;

type
  TKPUninstallKeyboardLanguage = class(TKPBase)
    /// <summary>Unregisters all TIPs from the Local Machine registry for a given
    /// keyboard, including transient profiles</summary>
    /// <param name="KeyboardID">The ID of the keyboard</param>
    procedure UnregisterTip(const KeyboardID: string); overload;

    /// <summary>Uninstalls a TIP for the current user for a given keyboard</summary>
    /// <param name="KeyboardID">The ID of the keyboard</param>
    /// <param name="BCP47Tag">This should be a BCP47 tag that is currently installed</param>
    procedure UninstallTip(const KeyboardID, BCP47Tag: string); overload;

    /// <summary>Uninstalls a TIP for the current user for a given keyboard</summary>
    /// <param name="KeyboardID">The ID of the keyboard</param>
    /// <param name="LangID">The transient profile to uninstall, $2000, $2400, $2800, $2C00</param>
    procedure UninstallTip(const KeyboardID: string; LangID: Word); overload;

    /// <summary>Uninstalls all TIPs for the current user for a given keyboard</summary>
    /// <param name="KeyboardID">The ID of the keyboard</param>
    procedure UninstallTip(const KeyboardID: string); overload;

  protected
    /// <summary>Unregisters a TIP from the Local Machine registry</summary>
    /// <param name="BCP47Tag">This should be a BCP47 tag</param>
    /// <param name="KeyboardID">The ID of the keyboard</param>
    procedure UnregisterTip(const KeyboardID, BCP47Tag: string); overload;

    /// <summary>Unregisters a transient profile tip from the Local Machine registry</summary>
    /// <param name="KeyboardID">The ID of the keyboard</param>
    /// <param name="LangID">The transient profile to unregister, $2000, $2400, $2800 or $2C00</param>
    procedure UnregisterTip(const KeyboardID: string; LangID: Word); overload;

  private
    procedure DoUnregisterTip(const KeyboardID, BCP47Tag: string; pInputProcessorProfileMgr: ITfInputProcessorProfileMgr);
    procedure DoUnregisterTipTransientProfile(const KeyboardID: string; LangID: Word; pInputProcessorProfileMgr: ITfInputProcessorProfileMgr);
    function GetInputProcessorProfileMgr: ITfInputProcessorProfileMgr;
  end;

implementation

uses
  System.Classes,
  System.SysUtils,
  System.Variants,
  System.Win.ComObj,
  Winapi.ActiveX,

  input_installlayoutortip,
  isadmin,
  Keyman.System.MitigateWin10_1803LanguageInstall,
  keymanerrorcodes,
  KeymanPaths,
  RegistryKeys,
  BCP47Tag,
  glossary,
  TempFileManager,
  utilexecute,
  utilfiletypes,
  utilkeyman,
  utilstr,
  utiltsf;

{ TKPUninstallKeyboardLanguage }

procedure TKPUninstallKeyboardLanguage.DoUnregisterTip(const KeyboardID,
  BCP47Tag: string; pInputProcessorProfileMgr: ITfInputProcessorProfileMgr);
var
  reg: TRegistry;
  FIsAdmin: Boolean;
  guid: TGUID;
  LangID: Integer;
  RootPath: string;
begin
  if not KeyboardInstalled(KeyboardID, FIsAdmin) then
    ErrorFmt(KMN_E_ProfileUninstall_KeyboardNotFound, VarArrayOf([KeyboardID]));   // I3888

  reg := TRegistry.Create;
  try
    reg.RootKey := HKEY_LOCAL_MACHINE;
    RootPath := GetRegistryKeyboardInstallKey_LM(KeyboardID) + SRegSubKey_LanguageProfiles;
    if not reg.OpenKeyReadOnly(RootPath + '\' + BCP47Tag) then
      ErrorFmt(KMN_E_ProfileUninstall_ProfileNotFound, VarArrayOf([KeyboardID]));

    if not reg.ValueExists(SRegValue_LanguageProfileLangID) or
        not reg.ValueExists(SRegValue_KeymanProfileGUID) then
    begin
      WarnFmt(KMN_W_ProfileUninstall_RegistryCorrupt, VarArrayOf([KeyboardID]));
      Exit;
    end;

    guid := StringToGuid(reg.ReadString(SRegValue_KeymanProfileGUID));
    LangID := reg.ReadInteger(SRegValue_LanguageProfileLangID);

    if not reg.DeleteKey('\' + RootPath + '\' + BCP47Tag) then
      ErrorFmt(KMN_E_ProfileUninstall_FailedToDeleteProfile, VarArrayOf([KeyboardID, reg.LastError]));

    try
      OleCheck(pInputProcessorProfileMgr.UnregisterProfile(   // I3743
        c_clsidKMTipTextService,
        LangID,
        guid,
        0));
    except
      on E:EOleSysError do
      begin
        ErrorFmt(KMN_W_TSF_COMError, VarArrayOf(['UnregisterProfile: '+E.Message+' ('+IntToHex(E.ErrorCode,8)+')']));
      end;
    end;
  finally
    reg.Free;
  end;

  Context.Control.AutoApplyKeyman;
end;

procedure TKPUninstallKeyboardLanguage.DoUnregisterTipTransientProfile(
  const KeyboardID: string; LangID: Word;
  pInputProcessorProfileMgr: ITfInputProcessorProfileMgr);
var
  reg: TRegistry;
  FIsAdmin: Boolean;
  guid: TGUID;
  RootPath: string;
begin
  if not KeyboardInstalled(KeyboardID, FIsAdmin) then
    ErrorFmt(KMN_E_ProfileUninstall_KeyboardNotFound, VarArrayOf([KeyboardID]));   // I3888

  if not IsTransientLanguageID(LangID) then
    ErrorFmt(KMN_E_ProfileUninstall_NotATransientLanguageCode, VarArrayOf([KeyboardID, LangID]));

  reg := TRegistry.Create;
  try
    reg.RootKey := HKEY_LOCAL_MACHINE;
    RootPath := GetRegistryKeyboardInstallKey_LM(KeyboardID) + SRegSubKey_TransientLanguageProfiles;
    if not reg.OpenKey(RootPath + '\' + IntToHex(LangID, 4), False) then
      // Assume that the profile has been removed already
      Exit;

    if not reg.ValueExists(SRegValue_KeymanProfileGUID) then
      // Assume that the profile has been removed already
      Exit;

    guid := StringToGuid(reg.ReadString(SRegValue_KeymanProfileGUID));

    if not reg.DeleteKey('\' + RootPath + '\' + IntToHex(LangID, 4)) then
      ErrorFmt(KMN_E_ProfileUninstall_FailedToDeleteProfile, VarArrayOf([KeyboardID, reg.LastError]));

    try
      OleCheck(pInputProcessorProfileMgr.UnregisterProfile(   // I3743
        c_clsidKMTipTextService,
        LangID,
        guid,
        0));
    except
      on E:EOleSysError do
      begin
        WarnFmt(KMN_W_TSF_COMError, VarArrayOf(['UnregisterProfile: '+E.Message+' ('+IntToHex(E.ErrorCode,8)+')']));
      end;
    end;
  finally
    reg.Free;
  end;

  Context.Control.AutoApplyKeyman;
end;

function TKPUninstallKeyboardLanguage.GetInputProcessorProfileMgr: ITfInputProcessorProfileMgr;
var
  pInputProcessorProfiles: ITfInputProcessorProfiles;
begin
  pInputProcessorProfiles := nil;

  OleCheck(CoCreateInstance(CLASS_TF_InputProcessorProfiles, nil, CLSCTX_INPROC_SERVER,
                          IID_ITfInputProcessorProfiles, pInputProcessorProfiles));

  if not Supports(pInputProcessorProfiles, IID_ITfInputProcessorProfileMgr, Result) then   // I3743
    raise Exception.Create('Missing interface IID_ITfInputProcessorProfileMgr');
end;

procedure TKPUninstallKeyboardLanguage.UnregisterTip(const KeyboardID, BCP47Tag: string);
begin
  DoUnregisterTip(KeyboardID, BCP47Tag, GetInputProcessorProfileMgr);
end;

procedure TKPUninstallKeyboardLanguage.UninstallTip(const KeyboardID, BCP47Tag: string);
var
  reg: TRegistry;
  FIsAdmin: Boolean;
  langid: Integer;
  guid: TGUID;
  FLayoutInstallString: string;
  RootPath: string;
begin
  if not KeyboardInstalled(KeyboardID, FIsAdmin) then
    ErrorFmt(KMN_E_ProfileInstall_KeyboardNotFound, VarArrayOf([KeyboardID]));   // I3888

  reg := TRegistry.Create(KEY_READ);
  try
    reg.RootKey := HKEY_LOCAL_MACHINE;
    RootPath := GetRegistryKeyboardInstallKey_LM(KeyboardID) + SRegSubKey_LanguageProfiles;
    if not reg.OpenKeyReadOnly(RootPath + '\' + BCP47Tag) then
      ErrorFmt(KMN_E_ProfileUninstall_ProfileNotFound, VarArrayOf([KeyboardID]));

    if not reg.ValueExists(SRegValue_KeymanProfileGUID) or
      not reg.ValueExists(SRegValue_LanguageProfileLangID) then
    begin
      WarnFmt(KMN_W_ProfileUninstall_RegistryCorrupt, VarArrayOf([KeyboardID]));
      Exit;
    end;

    guid := StringToGuid(reg.ReadString(SRegValue_KeymanProfileGUID));
    langid := reg.ReadInteger(SRegValue_LanguageProfileLangID);
  finally
    reg.Free;
  end;

  if not IsTipInstalledForCurrentUser(BCP47Tag, langid, guid) then
    Exit;

  //
  // Uninstall the TIP from Windows current user
  //

  FLayoutInstallString := GetLayoutInstallString(LangID, guid);

  if not InstallLayoutOrTip(PChar(FLayoutInstallString), ILOT_UNINSTALL) then   // I4302
    WarnFmt(KMN_W_ProfileUninstall_InstallLayoutOrTipFailed, VarArrayOf([KeyboardID]));
end;

procedure TKPUninstallKeyboardLanguage.UninstallTip(const KeyboardID: string;
  LangID: Word);
var
  reg: TRegistry;
  FIsAdmin: Boolean;
  guid: TGUID;
  FLayoutInstallString: string;
  RootPath: string;
begin
  if not KeyboardInstalled(KeyboardID, FIsAdmin) then
    ErrorFmt(KMN_E_ProfileUninstall_KeyboardNotFound, VarArrayOf([KeyboardID]));

  if not IsTransientLanguageID(LangID) then
    ErrorFmt(KMN_E_ProfileUninstall_NotATransientLanguageCode, VarArrayOf([KeyboardID, LangID]));

  reg := TRegistry.Create(KEY_READ);
  try
    reg.RootKey := HKEY_LOCAL_MACHINE;
    RootPath := GetRegistryKeyboardInstallKey_LM(KeyboardID) + SRegSubKey_TransientLanguageProfiles;
    if not reg.OpenKeyReadOnly(RootPath + '\' + IntToHex(LangID, 4)) then
      // We'll assume it's not installed
      Exit;

    if not reg.ValueExists(SRegValue_KeymanProfileGUID) then
    begin
      WarnFmt(KMN_W_ProfileUninstall_RegistryCorrupt, VarArrayOf([KeyboardID]));
      Exit;
    end;

    guid := StringToGuid(reg.ReadString(SRegValue_KeymanProfileGUID));
  finally
    reg.Free;
  end;

  //
  // We need to check if the TIP is installed before attempting to remove it,
  // because otherwise Windows will attempt to 'tidy up' and may leave a stray
  // keyboard layout installed.
  //
  // This scenario can arise if the user has removed the TIP through Windows
  // UI/APIs after we install it.
  //
  if not IsTipInstalledForCurrentUser('', langid, guid) then
    Exit;

  //
  // Uninstall the TIP from Windows current user
  //
  FLayoutInstallString := GetLayoutInstallString(LangID, guid);
  if not InstallLayoutOrTip(PChar(FLayoutInstallString), ILOT_UNINSTALL) then   // I4302
    WarnFmt(KMN_W_ProfileUninstall_InstallLayoutOrTipFailed, VarArrayOf([KeyboardID]));
end;

procedure TKPUninstallKeyboardLanguage.UninstallTip(const KeyboardID: string);
var
  sLocales: TStrings;
  sLocale: string;
  FInstByAdmin: Boolean;
  reg: TRegistry;
  RootPath: string;
begin
  if not KeyboardInstalled(KeyboardID, FInstByAdmin) then
    ErrorFmt(KMN_E_ProfileUninstall_KeyboardNotFound, VarArrayOf([KeyboardID]));   // I3888

  // We need to iterate through the Windows language profiles?
  sLocales := TStringList.Create;
  try
    reg := TRegistry.Create(KEY_READ);
    try
      reg.RootKey := HKEY_LOCAL_MACHINE;
      RootPath := GetRegistryKeyboardInstallKey_LM(KeyboardID) + SRegSubKey_LanguageProfiles;
      if reg.OpenKeyReadOnly(RootPath) then
      begin
        reg.GetKeyNames(sLocales);
      end;
    finally
      reg.Free;
    end;

    for sLocale in sLocales do
      // Note: if TIP is not installed, this still succeeds
      UninstallTip(KeyboardID, sLocale);
  finally
    sLocales.Free;
  end;

  Context.Control.AutoApplyKeyman;
end;

procedure TKPUninstallKeyboardLanguage.UnregisterTip(const KeyboardID: string);
var
  sLocales: TStrings;
  sLocale: string;
  FInstByAdmin: Boolean;
  RootPath: string;
  pInputProcessorProfileMgr: ITfInputProcessorProfileMgr;
  reg: TRegistry;
begin
  if not KeyboardInstalled(KeyboardID, FInstByAdmin) then
    ErrorFmt(KMN_E_ProfileUninstall_KeyboardNotFound, VarArrayOf([KeyboardID]));   // I3888

  if FInstByAdmin and not IsAdministrator then   // I3723
    ErrorFmt(KMN_E_ProfileUninstall_MustBeAllUsers, VarArrayOf([KeyboardID]));   // I3764

  pInputProcessorProfileMgr := GetInputProcessorProfileMgr;

  sLocales := TStringList.Create;
  try
    reg := TRegistry.Create(KEY_READ);
    try
      reg.RootKey := HKEY_LOCAL_MACHINE;
      RootPath := GetRegistryKeyboardInstallKey_LM(KeyboardID) + SRegSubKey_LanguageProfiles;
      if reg.OpenKeyReadOnly(RootPath) then
        reg.GetKeyNames(sLocales);
    finally
      reg.Free;
    end;

    for sLocale in sLocales do
      DoUnregisterTip(KeyboardID, sLocale, pInputProcessorProfileMgr);
  finally
    sLocales.Free;
  end;

  DoUnregisterTipTransientProfile(KeyboardID, $2000, pInputProcessorProfileMgr);
  DoUnregisterTipTransientProfile(KeyboardID, $2400, pInputProcessorProfileMgr);
  DoUnregisterTipTransientProfile(KeyboardID, $2800, pInputProcessorProfileMgr);
  DoUnregisterTipTransientProfile(KeyboardID, $2C00, pInputProcessorProfileMgr);

  Context.Control.AutoApplyKeyman;
end;

procedure TKPUninstallKeyboardLanguage.UnregisterTip(const KeyboardID: string;
  LangID: Word);
begin
  DoUnregisterTipTransientProfile(KeyboardID, LangID, GetInputProcessorProfileMgr);
end;

end.
