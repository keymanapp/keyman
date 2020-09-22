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
    LangID: Integer;
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
              if r.OpenKeyReadOnly('\' + BuildKeyboardLanguageProfilesKey_LM(keyboard) + '\' + profile) and r.ValueExists(SRegValue_LanguageProfileLangID) then
              begin
                uk.KeyboardID := keyboard;
                uk.LangID := r.ReadInteger(SRegValue_LanguageProfileLangID);
                Result.Add(uk);
              end;
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
begin
  // TODO: figure out Keyman "disabled" keyboards
  // TODO: figure out transient language registrations
  uks := LoadUpgradeKeyboards;
  r := TRegistry.Create;
  try
    if not r.OpenKey(SRegKey_Keyman_Temp_BackupProfiles, True) then
      Exit;

    for i := 0 to uks.Count - 1 do
    begin
      r.WriteString(IntToStr(i), uks[i].KeyboardID+'='+IntToStr(uks[i].LangID));
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

  kmcom.Refresh;
  (kmcom.Keyboards as IKeymanKeyboardsInstalled2).RefreshInstalledKeyboards;
end;

class procedure TImportOlderVersionKeyboards11To13.ImportCurrentUser;
var
  r: TRegistry;
  strings: TStringList;
  s: string;
  p: TArray<string>;
begin
  r := TRegistry.Create;
  strings := TStringList.Create;
  try
    if not r.OpenKeyReadOnly(SRegKey_Keyman_Temp_BackupProfiles) then
      Exit;

    r.GetValueNames(strings);

    for s in strings do
    begin
      // each string is saved in BackupCurrentUser and is keyboardid=langid
      p := r.ReadString(s).Split(['=']);
      kmcom.Keyboards[p[0]].Languages.InstallByLangID(StrToInt(p[1]));
    end;

    r.CloseKey;
    r.DeleteKey(SRegKey_Keyman_Temp_BackupProfiles);
  finally
    strings.Free;
    r.Free;
  end;
end;

end.
