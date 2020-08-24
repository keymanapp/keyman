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
    /// <summary>Unregisters a TIP from the Local Machine registry</summary>
    /// <param name="BCP47Tag">This should be a BCP47 tag or one of the four transient pseudo-tags ($2000, $2400, $2800, $2C00)
    /// </param>
    /// <param name="KeyboardID">The ID of the keyboard</param>
    procedure UnregisterTip(const KeyboardID, BCP47Tag: string); overload;

    /// <summary>Unregisters all TIPs from the Local Machine registry for a given keyboard</summary>
    /// <param name="KeyboardID">The ID of the keyboard</param>
    procedure UnregisterTip(const KeyboardID: string); overload;

    /// <summary>Uninstalls a TIP for the current user for a given keyboard</summary>
    /// <param name="KeyboardID">The ID of the keyboard</param>
    /// <param name="BCP47Tag">This should be a BCP47 tag that is currently installed</param>
    procedure UninstallTip(const KeyboardID, BCP47Tag: string); overload;

    /// <summary>Uninstalls all TIPs for the current user for a given keyboard</summary>
    /// <param name="KeyboardID">The ID of the keyboard</param>
    procedure UninstallTip(const KeyboardID: string); overload;
  private
    procedure DoUnregisterTip(const KeyboardID, BCP47Tag: string; pInputProcessorProfileMgr: ITfInputProcessorProfileMgr);
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
    ErrorFmt(KMN_E_ProfileInstall_KeyboardNotFound, VarArrayOf([KeyboardID]));   // I3888

  reg := TRegistry.Create;
  try
    reg.RootKey := HKEY_LOCAL_MACHINE;
    RootPath := GetRegistryKeyboardInstallKey_LM(KeyboardID) + SRegSubKey_LanguageProfiles;
    if not reg.OpenKey(RootPath + '\' + BCP47Tag, False) then
      ErrorFmt(KMN_E_ProfileUninstall_MustBeAllUsers, VarArrayOf([KeyboardID]));

    if not reg.ValueExists(SRegValue_LanguageProfileLangID) or
        not reg.ValueExists(SRegValue_KeymanProfileGUID) then
      ErrorFmt(KMN_E_ProfileInstall_KeyboardNotFound, VarArrayOf([KeyboardID]));

    guid := StringToGuid(reg.ReadString(SRegValue_KeymanProfileGUID));
    LangID := reg.ReadInteger(SRegValue_LanguageProfileLangID);

    if not reg.OpenKey('\' + RootPath, False) then
      // TODO: Error code
      ErrorFmt(KMN_E_ProfileUninstall_MustBeAllUsers, VarArrayOf([KeyboardID]));

    reg.DeleteKey(BCP47Tag);

    OleCheck(pInputProcessorProfileMgr.UnregisterProfile(   // I3743
      c_clsidKMTipTextService,
      LangID,
      guid,
      0));
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
      // TODO: better error code
      ErrorFmt(KMN_E_ProfileInstall_KeyboardNotFound, VarArrayOf([KeyboardID]));

    if not reg.ValueExists(SRegValue_KeymanProfileGUID) or not
      reg.ValueExists(SRegValue_LanguageProfileLangID) then
      // TODO: better error code
      ErrorFmt(KMN_E_ProfileInstall_KeyboardNotFound, VarArrayOf([KeyboardID]));

    guid := StringToGuid(reg.ReadString(SRegValue_KeymanProfileGUID));
    langid := reg.ReadInteger(SRegValue_LanguageProfileLangID);
  finally
    reg.Free;
  end;

  // TODO: handle $2000-$2C00 BCP47Tags
  if not IsTipInstalledForCurrentUser(BCP47Tag, langid, guid) then
    Exit;

  //
  // Uninstall the TIP from Windows current user
  //

  FLayoutInstallString := Format('%04.4x:%s%s', [LangID, GuidToString(c_clsidKMTipTextService),   // I4244
    GuidToString(guid)]);

  try   // I4494
    if not InstallLayoutOrTip(PChar(FLayoutInstallString), ILOT_UNINSTALL) then   // I4302
      // TODO: better error code
      WarnFmt(KMN_E_ProfileInstall_KeyboardNotFound, VarArrayOf([KeyboardID]));
  except
    on E:EOleException do
    begin
      WarnFmt(KMN_W_TSF_COMError, VarArrayOf(['EOleException: '+E.Message+' ('+E.Source+', '+IntToHex(E.ErrorCode,8)+')']));
    end;
    on E:EOleSysError do
    begin
      WarnFmt(KMN_W_TSF_COMError, VarArrayOf(['EOleSysError: '+E.Message+' ('+IntToHex(E.ErrorCode,8)+')']));
    end;
    on E:Exception do
    begin
      WarnFmt(KMN_W_TSF_COMError, VarArrayOf([E.Message]));
    end;
  end;
end;

procedure TKPUninstallKeyboardLanguage.UninstallTip(const KeyboardID: string);
var
  sLocales: TStrings;
  sLocale: string;
  FInstByAdmin: Boolean;
  i: Integer;
  reg: TRegistry;
  RootPath: string;
begin
  if not KeyboardInstalled(KeyboardID, FInstByAdmin) then
    ErrorFmt(KMN_E_ProfileInstall_KeyboardNotFound, VarArrayOf([KeyboardID]));   // I3888

  if FInstByAdmin and not IsAdministrator then   // I3723
    ErrorFmt(KMN_E_ProfileUninstall_MustBeAllUsers, VarArrayOf([KeyboardID]));   // I3764

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
        for i := 0 to sLocales.Count - 1 do
        begin
          // Note, if TIP is not installed, silently succeeds
          UninstallTip(KeyboardID, sLocales[i]);
        end;
      end;
    finally
      reg.Free;
    end;

    for i := 0 to sLocales.Count - 1 do
    begin
      sLocale := sLocales[i];
      UninstallTip(KeyboardID, sLocale);
    end;
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
  i: Integer;
  Path: string;
  RootPath: string;
  pInputProcessorProfileMgr: ITfInputProcessorProfileMgr;
  reg: TRegistry;
begin
  if not KeyboardInstalled(KeyboardID, FInstByAdmin) then
    ErrorFmt(KMN_E_ProfileInstall_KeyboardNotFound, VarArrayOf([KeyboardID]));   // I3888

  if FInstByAdmin and not IsAdministrator then   // I3723
    ErrorFmt(KMN_E_ProfileUninstall_MustBeAllUsers, VarArrayOf([KeyboardID]));   // I3764

  pInputProcessorProfileMgr := GetInputProcessorProfileMgr;

  sLocales := TStringList.Create;
  reg := TRegistry.Create(KEY_READ);
  try
    reg.RootKey := HKEY_LOCAL_MACHINE;
    Path := GetRegistryKeyboardInstallKey_LM(KeyboardID);
    RootPath := Path + SRegSubKey_LanguageProfiles;
    if reg.OpenKeyReadOnly(RootPath) then
    begin
      reg.GetKeyNames(sLocales);
      for i := 0 to sLocales.Count - 1 do
      begin
        sLocale := sLocales[i];
        DoUnregisterTip(KeyboardID, sLocale, pInputProcessorProfileMgr);
      end;
    end;
  finally
    reg.Free;
    sLocales.Free;
  end;
  Context.Control.AutoApplyKeyman;
end;

end.
