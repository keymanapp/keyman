unit Keyman.Configuration.System.UImportOlderVersionKeyboards11To13;

interface

type
  TImportOlderVersionKeyboards11To13 = class
  public
    class procedure Execute;
    class procedure BackupCurrentUser;
    class procedure ReRegisterTips;
    class procedure ImportCurrentUser;
  end;

implementation

uses
  System.Classes,
  System.Generics.Collections,
  System.SysUtils,
  System.Win.ComObj,
  System.Win.Registry,
  Winapi.ActiveX,
  Winapi.msctf,
  Winapi.Windows,

  input_installlayoutortip,
  keymanapi_tlb,
  keyman_msctf,
  Keyman.System.UpgradeRegistryKeys,
  kmint,
  RegistryKeys,
  UImportOlderVersionKeyboards9Plus,
  UImportOlderKeyboardUtils,
  utilexecute,
  utilkmshell,
  utiltsf;

class procedure TImportOlderVersionKeyboards11To13.Execute;  // I2361
begin
  // execute KMshell as login user to backup list of installed Keyman TIPs
  TUtilExecute.CreateProcessAsShellUser(ParamStr(0), '"'+ParamStr(0)+'" -upgradekeyboards=13,backup', True);

  // Refreshes the installed TIPs (LM)
  TImportOlderVersionKeyboards11To13.ReRegisterTips;

  // This runs after the admin run, we'll take the backed up list of installed
  // Keyman TIPs and re-install them as current user.
  TUtilExecute.CreateProcessAsShellUser(ParamStr(0), '"'+ParamStr(0)+'" -upgradekeyboards=13,import', True)
end;

{ TImportOlderVersionKeyboards11To13 }

type
  TUpgradeKeyboard = record
    KeyboardID: string;
    BCP47Code: string;
  end;
  TUpgradeKeyboardList = class(TList<TUpgradeKeyboard>);

class procedure TImportOlderVersionKeyboards11To13.BackupCurrentUser;

  function LoadUpgradeKeyboards: TUpgradeKeyboardList;
  var
    r: TRegistry;
    profiles, keyboards: TStringList;
    profile, keyboard: string;
    uk: TUpgradeKeyboard;
  begin
    Result := TUpgradeKeyboardList.Create;
    r := TRegistry.Create;
    keyboards := TStringList.Create;
    profiles := TStringList.Create;
    try
      r.RootKey := HKEY_LOCAL_MACHINE;
      if r.OpenKeyReadOnly(SRegKey_InstalledKeyboards_LM) then
      begin
        r.GetKeyNames(keyboards);
        for keyboard in keyboards do
        begin
          if r.OpenKeyReadOnly('\' + BuildKeyboardLanguageProfilesKey_LM(keyboard)) then
          begin
            r.GetKeyNames(profiles);
            for profile in profiles do
            begin
              uk.KeyboardID := keyboard;
              uk.BCP47Code := profile;
              Result.Add(uk);
            end;
          end;
        end;
      end;
    finally
      keyboards.Free;
      profiles.Free;
      r.Free;
    end;
  end;
var
  r: TRegistry;
  i: Integer;
  uks: TUpgradeKeyboardList;
  uk: TUpgradeKeyboard;
begin
  uks := LoadUpgradeKeyboards;
  r := TRegistry.Create;
  try
    if not r.OpenKey(SRegKey_Keyman_Temp_BackupProfiles, True) then
      Exit;

    i := 0;
    for uk in uks do
    begin
      // For transient language codes, we will need to install using an assigned id
      // which may vary, so we will work from the BCP47 code.
      r.WriteString(IntToStr(i), uk.KeyboardID+'='+uk.BCP47Code);
      Inc(i);
    end;

  finally
    r.Free;
    uks.Free;
  end;
end;

class procedure TImportOlderVersionKeyboards11To13.ReRegisterTips;
var
  pInputProcessorProfiles: ITfInputProcessorProfiles;
  pInputProcessorProfileMgr: keyman_msctf.ITfInputProcessorProfileMgr;
  ippEnum: IEnumTfInputProcessorProfiles;
  pcFetch: Cardinal;
  profile: keyman_msctf.TF_INPUTPROCESSORPROFILE;
  r: TRegistry;
  keys: TStringList;
  i: Integer;
begin
  // Remove the registration of existing TIPs because they may have the
  // wrong BCP47 codes
  OleCheck(CoCreateInstance(CLASS_TF_InputProcessorProfiles, nil, CLSCTX_INPROC_SERVER,
                          IID_ITfInputProcessorProfiles, pInputProcessorProfiles));

  if not Supports(pInputProcessorProfiles, IID_ITfInputProcessorProfileMgr, pInputProcessorProfileMgr) then   // I3743
  begin
    Exit;
  end;

  // Unregister all the existing Keyman 11-13 profiles
  OleCheck(pInputProcessorProfileMgr.EnumProfiles(0, ippEnum));
  while ippEnum.Next(1, profile, pcFetch) = S_OK do
  begin
    if (profile.dwProfileType = TF_PROFILETYPE_INPUTPROCESSOR) and
      IsEqualGuid(profile.clsid, c_clsidKMTipTextService) then
    begin
      pInputProcessorProfileMgr.UnregisterProfile(profile.clsid, profile.langid, profile.guidProfile, 0);
    end;
  end;

  r := TRegistry.Create;
  keys := TStringList.Create;
  try
    r.RootKey := HKEY_LOCAL_MACHINE;
    // Delete each of the registered profile keys
    if not r.OpenKey(SRegKey_InstalledKeyboards_LM, True) then
      Exit;
    r.GetKeyNames(keys);
    for i := 0 to keys.Count - 1 do
    begin
      r.DeleteKey('\' + BuildKeyboardLanguageProfilesKey_LM(keys[i]));
      r.DeleteKey('\' + BuildKeyboardSuggestedLanguagesKey_LM(keys[i]));
    end;
  finally
    r.Free;
    keys.Free;
  end;

  // The following code re-registers all the profiles
  kmcom.Refresh;
  (kmcom.Keyboards as IKeymanKeyboardsInstalled2).RefreshInstalledKeyboards;
end;

class procedure TImportOlderVersionKeyboards11To13.ImportCurrentUser;
var
  r: TRegistry;
  strings: TStringList;
  s: string;
  p: TArray<string>;
  BCP47Code, KeyboardID: string;
  LangID: Integer;
  kbd: IKeymanKeyboardInstalled;
  lang: IKeymanKeyboardLanguageInstalled2;
  TemporaryKeyboardID: WideString;
  RegistrationRequired: WordBool;
  i: Integer;
begin
  r := TRegistry.Create;
  strings := TStringList.Create;
  try
    if not r.OpenKeyReadOnly(SRegKey_Keyman_Temp_BackupProfiles) then
      Exit;

    r.GetValueNames(strings);

    for s in strings do
    begin
      // each string is saved in BackupCurrentUser and is keyboardid=bcp47
      p := r.ReadString(s).Split(['=']);
      KeyboardID := p[0];
      BCP47Code := p[1];
      kbd := kmcom.Keyboards[KeyboardID];
      if not Assigned(kbd) then
      begin
        // Avoid errors if a package is uninstalled midway through
        Continue;
      end;

      // Installing a language is a 2-step process. (We can assume that
      // the transient language codes have been installed correctly as Register
      // would have been called immediately prior to this.)
      lang := nil;
      BCP47Code := (kmcom as IKeymanBCP47Canonicalization).GetCanonicalTag(BCP47Code);
      for i := 0 to kbd.Languages.Count - 1 do
      begin
        if SameText(kbd.Languages[i].BCP47Code, BCP47Code) then
        begin
          lang := kbd.Languages[i] as IKeymanKeyboardLanguageInstalled2;
          Break;
        end;
      end;

      if lang = nil then
      begin
        // The BCP47 code was not in the list of languages; this could possibly
        // happen if we had registered a code that was canonicalized differently
        // in the past?
        lang := (kbd.Languages as IKeymanKeyboardLanguagesInstalled2).Add(BCP47Code) as IKeymanKeyboardLanguageInstalled2;
        if lang = nil then
        begin
          // This should never happen, because .Add only fails if the language
          // is already in the list, which we just searched through, or if the
          // BCP47Code is empty
          Continue;
        end;
      end;

      if lang.IsInstalled then
      begin
        // Don't attempt to reinstall; this should not normally be the case but
        // if we canonicalize two languages which were previously installed into
        // a single code, then in theory this could happen.
        Continue;
      end;

      if lang.FindInstallationLangID(LangID, TemporaryKeyboardID, RegistrationRequired, kifInstallTransientLanguage) then
      begin
        if RegistrationRequired then
        begin
          // This can happen for custom language codes. TODO: This is not ideal because of potential for multiple elevation prompts
          WaitForElevatedConfiguration(0, '-register-tip '+IntToHex(LangID,4)+' "'+KeyboardID+'" "'+lang.BCP47Code+'"');
        end;

        lang.InstallTip(LangID, TemporaryKeyboardID);
      end;
    end;

    r.CloseKey;
    r.DeleteKey(SRegKey_Keyman_Temp_BackupProfiles);

    //
    // Reapply the loaded state for keyboards; this may cause TIPs to disappear again
    //

    kmcom.Refresh;

    if r.OpenKeyReadOnly('\' + SRegKey_ActiveKeyboards_CU) then
    begin
      strings.Clear;
      r.GetKeyNames(strings);
      for s in strings do
      begin
        kbd := kmcom.Keyboards[s];
        if Assigned(kbd) then
          kbd.Loaded := r.OpenKeyReadOnly('\' + SRegKey_ActiveKeyboards_CU + '\' + s) and r.ValueExists(SRegValue_KeymanID);
      end;
    end;

  finally
    strings.Free;
    r.Free;
  end;

  kmcom.Apply;
  kmcom.Refresh;
end;

end.
