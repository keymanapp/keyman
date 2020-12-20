unit Keyman.System.Process.KPInstallKeyboardLanguage;

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
  TKPInstallKeyboardLanguageFlags = set of (ilkInstallTransientLanguage);

type
  TKPInstallKeyboardLanguage = class(TKPBase)
  public
    ///  <summary>Given a BCP47 Tag, finds a corresponding LangID, including installing
    ///  a temporary keyboard if necessary to establish a transient LangID.
    ///  Current User.</summary>
    ///  <returns>True if a language ID can be found</return>
    ///  <param name="BCP47Tag">A canonical BCP47 tag to install. May be updated
    ///  for compatibility reasons; see #1285.</param>
    ///  <param name="LangID">LangID found that corresponds to BCP47Tag</param>
    ///  <param name="TemporaryLayoutString">If a transient LangID is installed, then
    ///  Windows will have added a default keyboard for the language which should be
    ///  removed after we install the Keyman TIP.</param>
    ///  <param name="Flags">If ilkInstallTransientLanguage is included in flags,
    ///  will not install a transient language and will return False if no standard
    ///  Windows LangID can be found that corresponds to the BCP47 tag.</param>
    function FindInstallationLangID(
      var BCP47Tag: string;
      var LangID: Integer;
      var TemporaryLayoutString: string;
      Flags: TKPInstallKeyboardLanguageFlags): Boolean;

    ///  <summary>Registers a TIP for a given keyboard + language, and adds metadata to the
    ///  HKLM registry about the Keyman keyboard association. Requires elevation.</summary>
    ///  <param name="KeyboardID">Keyman keyboard to register</param>
    ///  <param name="BCP47Tag">BCP47 tag associated with the TIP</param>
    ///  <param name="KeyboardName">Name of keyboard to display in Windows UI</param>
    ///  <param name="LangID">Language ID found with FindInstallationLangID</param>
    ///  <param name="IconFileName">Icon to display in Windows UI</param>
    ///  <param name="LanguageName">Name of language to display in Keyman UI</param>
    procedure RegisterTip(const KeyboardID, BCP47Tag, KeyboardName: string; LangID: Integer; IconFileName, LanguageName: string);

    ///  <summary>Installs a TIP for a given keyboard + language, for the current user.</summary>
    ///  <param name="KeyboardID">Keyman keyboard to associate with TIP</param>
    ///  <param name="BCP47Tag">BCP47 tag associated with the TIP</param>
    ///  <param name="LangID">Language ID found with FindInstallationLangID</param>
    ///  <param name="guidProfile">Profile GUID that is assigned on exit</param>
    procedure InstallTip(const KeyboardID, BCP47Tag: string; LangID: Integer; var guidProfile: TGUID);

    ///  <summary>Removes a temporary keyboard layout that was installed by
    ///  FindInstallationLangID. Current User.</summary>
    ///  <param name="LayoutString">TemporaryLayoutString value returned from
    ///  FindInstallationLangID</param>
    procedure UninstallTemporaryLayout(const LayoutString: string);

    ///  <summary>Registers a TIP for each of the four transient language IDs,
    ///  and adds metadata to the HKLM registry about the Keyman keyboard
    ///  association. Requires elevation.</summary>
    ///  <param name="KeyboardID">Keyman keyboard to register</param>
    ///  <param name="KeyboardName">Name of keyboard to display in Windows UI</param>
    ///  <param name="IconFileName">Icon to display in Windows UI</param>
    procedure RegisterTransientTips(const KeyboardID, KeyboardName: string; IconFileName: string);

    constructor Create(AContext: TKeymanContext);
    destructor Destroy; override;
  private
    FWin8Languages: TWindows8LanguageList;
    function ConvertBCP47TagToLangID(Locale: string; var LangID: Integer): Boolean;
    function InstallBCP47Language(const FLocaleName: string): Boolean;
    function GetInputProcessorProfileMgr: ITfInputProcessorProfileMgr;
    function AreMaximumTransientLanguagesInstalled: Boolean;
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

{ TKPInstallKeyboardLanguage }

constructor TKPInstallKeyboardLanguage.Create(AContext: TKeymanContext);
begin
  FWin8Languages := TWindows8LanguageList.Create;
  inherited Create(AContext);
end;

destructor TKPInstallKeyboardLanguage.Destroy;
begin
  FreeAndNil(FWin8Languages);
  inherited Destroy;
end;

function TKPInstallKeyboardLanguage.AreMaximumTransientLanguagesInstalled: Boolean;
const
  MAX_TRANSIENT_LANGUAGES = 4;
var
  r: TRegistry;
  keys: TStringList;
  isInstalled: array[0..MAX_TRANSIENT_LANGUAGES-1] of Boolean;
  i: Integer;
  v: Integer;
  key: string;
begin
  for i := 0 to high(isInstalled) do isInstalled[i] := False;

  // We'll use Control Panel/International/User Profile to find the full set of
  // transient languages
  r := TRegistry.Create;
  keys := TStringList.Create;
  try
    if r.OpenKeyReadOnly(SRegKey_ControlPanelInternationalUserProfile) then
    begin
      r.GetKeyNames(keys);
      for key in keys do
      begin
        if r.OpenKeyReadOnly('\' + SRegKey_ControlPanelInternationalUserProfile + '\' + key) then
        begin
          if r.ValueExists(SRegValue_CPIUP_TransientLangId) then
          begin
            v := r.ReadInteger(SRegValue_CPIUP_TransientLangId);
            // Transient languages start at $2000 and go up by $400
            v := (v - $2000) div $400;
            if (v >= 0) and (v <= High(isInstalled)) then
              isInstalled[v] := True;
          end;
        end;
      end;
    end;
  finally
    keys.Free;
    r.Free;
  end;

  for i := 0 to High(isInstalled) do
    if not isInstalled[i] then
      Exit(False);

  Result := True;
end;

// TODO: change LangID to Winapi.Windows.LANGID
function TKPInstallKeyboardLanguage.FindInstallationLangID(var BCP47Tag: string; var LangID: Integer; var TemporaryLayoutString: string; Flags: TKPInstallKeyboardLanguageFlags): Boolean;
var
  ml: TMitigateWin10_1803.TMitigatedLanguage;
  Win8Lang: TWindows8Language;
begin
  if BCP47Tag = '' then
    ErrorFmt(KMN_E_ProfileInstall_InvalidBCP47Tag, VarArrayOf([BCP47Tag]));

  TemporaryLayoutString := '';

  if ConvertBCP47TagToLangID(BCP47Tag, LangID) then
  begin
    if TMitigateWin10_1803.IsMitigationRequired(LangID, ml) then
    begin
      LangID := ml.NewLanguage.Code;
      BCP47Tag := ml.NewLanguage.BCP47;
      WarnFmt(KMN_W_ProfileInstall_Win10_1803_MitigationApplied, VarArrayOf([ml.OriginalLanguage.Name, ml.NewLanguage.Name]));
    end;

    if LangID <> 0 then
      Exit(True);
  end;

  if not (ilkInstallTransientLanguage in Flags) then
  begin
    // No warning, because this is to be expected
    Exit(False);
  end;

  //
  // Installing a custom language only supported with Win8 and later
  //

  if not FWin8Languages.IsSupported then
  begin
    Warn(KMN_W_ProfileInstall_CustomLocalesNotSupported);
    Exit(False);
  end;

  //
  // Windows only appears to support 4 transient languages: 0x2000, 0x2400,
  // 0x2800, 0x2C00. While Powershell will add extra languages above those,
  // it will not install TIPs for any additional languages (e.g. 0x3000+),
  // so we should stop after 4
  //

  if AreMaximumTransientLanguagesInstalled then
  begin
    Warn(KMN_W_ProfileInstall_CustomLocalesNotSupported); // TODO new code
    Exit(False);
  end;

  //
  // Install user language with Powershell if it isn't present
  //

  LangID := 0;

  if (BCP47Tag = '') or not InstallBCP47Language(BCP47Tag) then
  begin
    WarnFmt(KMN_W_ProfileInstall_FailedToInstallLanguage, VarArrayOf([BCP47Tag]));
    Exit(False);
  end;

  //
  // Find the new language ID
  //

  FWin8Languages.Refresh;
  Win8Lang := FWin8Languages.FindClosestByBCP47Tag(BCP47Tag);
  if not Assigned(Win8Lang) then
  begin
    WarnFmt(KMN_W_ProfileInstall_LanguageInstalledButNotFound, VarArrayOf([BCP47Tag]));
    Exit(False);
  end;

  LangID := Win8Lang.LangID;

  if Win8Lang.InputMethods.Count = 1 then
  begin
    TemporaryLayoutString := Win8Lang.InputMethods[0];
  end
  else
    // We'll continue on, but this is unexpected, so we won't try and uninstall the temporary input method. The user
    // will have more than one input method installed.
    WarnFmt(KMN_W_ProfileInstall_MoreThanOneInputMethodInstalled, VarArrayOf([Win8Lang.InputMethods.Text]));

  Result := True;
end;


function TKPInstallKeyboardLanguage.ConvertBCP47TagToLangID(Locale: string;
  var LangID: Integer): Boolean;
var
  Win8Lang: TWindows8Language;
  tag1, tag2: TBCP47Tag;
begin
  if FWin8Languages.IsSupported then
  begin
    // We have to find closest because we can't install e.g. am-ET and am and am-Ethi-ET
    Win8Lang := FWin8Languages.FindClosestByBCP47Tag(Locale);
    if Assigned(Win8Lang) then
    begin
      LangID := Win8Lang.LangID;
      Exit(True);
    end;
  end
  else
  begin
    //If tag is a Language-Script-Region triplet at most, then we can use
    // LocaleNameTOLCID on the Language-Region to get a LangID. If you use
    // -Variant or -Extension then we won't get a valid LangID. On Win7/8,
    // this will result in installation under default locale.

    tag1 := TBCP47Tag.Create(Locale);
    try
      tag1.Script := '';
      tag2 := TBCP47Tag.Create(tag1.Language);
      try
        tag2.Region := tag1.Region;
        if tag2.Tag <> tag1.Tag then
        begin
          // We have a complex tag, Windows won't convert it with LocaleNameToLCID
          Exit(False);
        end;
      finally
        tag2.Free;
      end;
      Locale := tag1.Tag;
    finally
      tag1.Free;
    end;
  end;

  LangID := LocaleNameToLCID(PWideChar(Locale), 0);

  if (PRIMARYLANGID(LangID) = 0) and (
    (SUBLANGID(LangID) < 8) or
    (SUBLANGID(LangID) > 11)
  ) then
  begin
    // LocaleNameToLCID can return a language code that is 'transient' but
    // outside of the supported range of $2000, $2400, $2800, $2C00. For
    // example, we have seen values of $3000, $3400, $3800.
    // Set-WinUserLanguageList refuses to play ball with these values, so we
    // reject them as invalid.
    //
    // This may be a bug in Windows? TODO: Investigate further and report to
    // Microsoft
    Exit(False);
  end;


  case LangID of
    LOCALE_CUSTOM_DEFAULT,
    LOCALE_CUSTOM_UNSPECIFIED,
    0: Result := False;
    else Result := True;
  end;
end;

function TKPInstallKeyboardLanguage.GetInputProcessorProfileMgr: ITfInputProcessorProfileMgr;
var
  pInputProcessorProfiles: ITfInputProcessorProfiles;
begin
  pInputProcessorProfiles := nil;

  OleCheck(CoCreateInstance(CLASS_TF_InputProcessorProfiles, nil, CLSCTX_INPROC_SERVER,
                          IID_ITfInputProcessorProfiles, pInputProcessorProfiles));

  if not Supports(pInputProcessorProfiles, IID_ITfInputProcessorProfileMgr, Result) then   // I3743
    raise Exception.Create('Missing interface IID_ITfInputProcessorProfileMgr');
end;

procedure TKPInstallKeyboardLanguage.RegisterTip(const KeyboardID,
  BCP47Tag, KeyboardName: string; LangID: Integer; IconFileName, LanguageName: string);
var
  FIsAdmin: Boolean;
  RootPath, FKeyboardName: string;
  reg: TRegistry;
  guid: TGUID;
  IconIndex: Integer;
  pInputProcessorProfileMgr: ITfInputProcessorProfileMgr;
begin
  if IsTransientLanguageID(LangID) then
    ErrorFmt(KMN_E_ProfileInstall_IsATransientLanguageCode, VarArrayOf([KeyboardID, LangID]));

  if not KeyboardInstalled(KeyboardID, FIsAdmin) then
    ErrorFmt(KMN_E_ProfileInstall_KeyboardNotFound, VarArrayOf([KeyboardID]));   // I3888

  if KeyboardName = ''
    then FKeyboardName := KeyboardID + Ext_KeymanFile
    else FKeyboardName := KeyboardName;

  reg := TRegistry.Create;
  try
    reg.RootKey := HKEY_LOCAL_MACHINE;
    RootPath := GetRegistryKeyboardInstallKey_LM(KeyboardID) + SRegSubKey_LanguageProfiles;
    if not reg.OpenKey(RootPath + '\' + BCP47Tag, True) then
      ErrorFmt(KMN_E_ProfileInstall_MustBeAllUsers, VarArrayOf([KeyboardID]));

    reg.WriteInteger(SRegValue_LanguageProfileLangID, LangID);
    reg.WriteString(SRegValue_LanguageProfileLocale, BCP47Tag);
    reg.WriteString(SRegValue_LanguageProfileName, LanguageName);

    if not reg.ValueExists(SRegValue_KeymanProfileGUID) then
    begin
      CreateGuid(&guid);
      reg.WriteString(SRegValue_KeymanProfileGUID, GuidToString(guid));
    end
    else
      guid := StringToGuid(reg.ReadString(SRegValue_KeymanProfileGUID));

    if IconFileName = '' then   // I4555
    begin
      IconFileName := TKeymanPaths.KeymanEngineInstallPath(TKeymanPaths.S_KeymanExe);
      IconIndex := 1;
    end
    else
      IconIndex := 0;

    pInputProcessorProfileMgr := GetInputProcessorProfileMgr;
    try
      OleCheck(pInputProcessorProfileMgr.RegisterProfile(   // I3743
        c_clsidKMTipTextService,
        LangID,
        guid,
        PWideChar(KeyboardName),
        Length(KeyboardName),
        PWideChar(IconFileName),
        Length(IconFileName),
        IconIndex,
        0,
        0,
        0,
        0));
    except
      on E:EOleSysError do
      begin
        ErrorFmt(KMN_E_ProfileInstall_RegisterProfileFailed, VarArrayOf([E.Message, E.ErrorCode]));
      end;
    end;
  finally
    reg.Free;
  end;

  Context.Control.AutoApplyKeyman;
end;

procedure TKPInstallKeyboardLanguage.RegisterTransientTips(const KeyboardID,
  KeyboardName: string; IconFileName: string);
var
  FIsAdmin: Boolean;
  RootPath, FKeyboardName: string;
  guid: TGUID;
  i, LangID: Integer;
  IconIndex: Integer;
  reg: TRegistry;
  pInputProcessorProfileMgr: ITfInputProcessorProfileMgr;
begin
  if not KeyboardInstalled(KeyboardID, FIsAdmin) then
    ErrorFmt(KMN_E_ProfileInstall_KeyboardNotFound, VarArrayOf([KeyboardID]));   // I3888

  if KeyboardName = ''
    then FKeyboardName := KeyboardID + Ext_KeymanFile
    else FKeyboardName := KeyboardName;

  if IconFileName = '' then   // I4555
  begin
    IconFileName := TKeymanPaths.KeymanEngineInstallPath(TKeymanPaths.S_KeymanExe);
    IconIndex := 1;
  end
  else
    IconIndex := 0;

  pInputProcessorProfileMgr := GetInputProcessorProfileMgr;

  for i := 0 to 3 do
  begin
    LangID := i * $400 + $2000;

    reg := TRegistry.Create;
    try
      reg.RootKey := HKEY_LOCAL_MACHINE;
      RootPath := GetRegistryKeyboardInstallKey_LM(KeyboardID) + SRegSubKey_TransientLanguageProfiles;
      if not reg.OpenKey(RootPath + '\' + IntToHex(LangID, 4), True) then
        ErrorFmt(KMN_E_ProfileInstall_MustBeAllUsers, VarArrayOf([KeyboardID]));

      reg.WriteInteger(SRegValue_LanguageProfileLangID, LangID);
      if not reg.ValueExists(SRegValue_KeymanProfileGUID) then
      begin
        CreateGuid(&guid);
        reg.WriteString(SRegValue_KeymanProfileGUID, GuidToString(guid));
      end
      else
        guid := StringToGuid(reg.ReadString(SRegValue_KeymanProfileGUID));

      try
        OleCheck(pInputProcessorProfileMgr.RegisterProfile(   // I3743
          c_clsidKMTipTextService,
          LangID,
          guid,
          PWideChar(KeyboardName),
          Length(KeyboardName),
          PWideChar(IconFileName),
          Length(IconFileName),
          IconIndex,
          0,
          0,
          0,
          0));
      except
        on E:EOleSysError do
        begin
          ErrorFmt(KMN_E_ProfileInstall_RegisterProfileFailed, VarArrayOf([E.Message, E.ErrorCode]));
        end;
      end;
    finally
      reg.Free;
    end;
  end;

  Context.Control.AutoApplyKeyman;
end;

procedure TKPInstallKeyboardLanguage.InstallTip(const KeyboardID, BCP47Tag: string; LangID: Integer; var guidProfile: TGUID);
var
  FIsAdmin: Boolean;
  reg: TRegistry;
  RootPath, FLayoutInstallString: string;
begin
  if not KeyboardInstalled(KeyboardID, FIsAdmin) then
    ErrorFmt(KMN_E_ProfileInstall_KeyboardNotFound, VarArrayOf([KeyboardID]));   // I3888

  reg := TRegistry.Create(KEY_READ);
  try
    reg.RootKey := HKEY_LOCAL_MACHINE;

    if IsTransientLanguageID(LangID) then
    begin
      RootPath := GetRegistryKeyboardInstallKey_LM(KeyboardID) + SRegSubKey_TransientLanguageProfiles;
      if not reg.OpenKeyReadOnly(RootPath + '\' + IntToHex(LangID, 4)) then
        ErrorFmt(KMN_E_ProfileInstall_ProfileNotFound, VarArrayOf([KeyboardID, IntToHex(LangID, 4)]));
    end
    else
    begin
      RootPath := GetRegistryKeyboardInstallKey_LM(KeyboardID) + SRegSubKey_LanguageProfiles;
      if not reg.OpenKeyReadOnly(RootPath + '\' + BCP47Tag) then
        ErrorFmt(KMN_E_ProfileInstall_ProfileNotFound, VarArrayOf([KeyboardID, BCP47Tag]));
    end;

    if not reg.ValueExists(SRegValue_KeymanProfileGUID) then
      ErrorFmt(KMN_E_ProfileInstall_RegistryCorrupt, VarArrayOf([KeyboardID]));

    guidProfile := StringToGuid(reg.ReadString(SRegValue_KeymanProfileGUID));
  finally
    reg.Free;
  end;

  //
  // Install the TIP into Windows
  //

  FLayoutInstallString := GetLayoutInstallString(LangID, guidProfile);

  if not InstallLayoutOrTip(PChar(FLayoutInstallString), 0) then   // I4302
    ErrorFmt(KMN_E_ProfileInstall_InstallLayoutOrTipFailed, VarArrayOf([KeyboardID]));

  //
  // Record the installation of the language
  //
  reg := TRegistry.Create;
  try
    if reg.OpenKey(BuildKeyboardLanguagesKey_CU(KeyboardID), True) then
      reg.WriteString(BCP47Tag, FLayoutInstallString);
  finally
    reg.Free;
  end;
end;

function TKPInstallKeyboardLanguage.InstallBCP47Language(const FLocaleName: string): Boolean;
var
  ScriptFile: TTempFile;
  FLogText: string;
begin
  // Use PowerShell
  ScriptFile := TTempFileManager.Get('.ps1');

  with TStringList.Create do
  try
    Add('$list = Get-WinUserLanguageList');
    Add('$list.Add("'+FLocaleName+'")');
    Add('Set-WinUserLanguageList $list -force');
    SaveToFile(ScriptFile.Name);
    TUtilExecute.Console('powershell.exe -ExecutionPolicy Unrestricted "& ""'+ScriptFile.Name+'"""', ExtractFilePath(ScriptFile.Name), FLogText);
  finally
    Free;
    ScriptFile.Free;
  end;

  Result := True;
end;

procedure TKPInstallKeyboardLanguage.UninstallTemporaryLayout(const LayoutString: string);
begin
  if not InstallLayoutOrTip(PChar(LayoutString), ILOT_UNINSTALL) then
    ErrorFmt(KMN_E_ProfileInstall_UninstallLayoutOrTipFailed, VarArrayOf([LayoutString]));
end;

end.
