unit Keyman.Configuration.System.TIPMaintenance;

interface

uses
  System.Classes,

  KeymanAPI_TLB;

type
  TTIPMaintenance = class
  public
    /// <summary>Install a TIP for the current user (user context)</summary>
    class function DoInstall(const KeyboardID, BCP47Tag: string): Boolean;

    /// <summary>Install a TIP for the current user (user context)</summary>
    class function InstallTip(LangID: Integer; const KeyboardID, BCP47Tag, KeyboardToRemove: string): Boolean;

    /// <summary>Install TIPs for packages for the current user (user context)</summary>
    class function InstallTipsForPackages(Packages: TStrings): Boolean; static;

    /// <summary>Install TIP for a package for the current user (user context)</summary>
    class function InstallTipForPackage(const PackageFilename, BCP47Tag: string): Boolean; static;

    /// <summary>Register TIP for the local machine (elevated context)</summary>
    class function DoRegister(const KeyboardID, BCP47Tag: string): Boolean;

    /// <summary>Register TIP for the local machine (elevated context)</summary>
    class function RegisterTip(LangID: Integer; const KeyboardID, BCP47Tag: string): Boolean;

    /// <summary>Helper function to get default BCP47 tag for an installed keyboard</summary>
    class function GetFirstLanguage(Keyboard: IKeymanKeyboardInstalled): string; overload;
    class function GetFirstLanguage(Keyboard: IKeymanKeyboardFile): string; overload;

    /// <summary>Get the canonicalized BCP47 tag for the user's default language</summary>
    class function GetUserDefaultLanguage: string; overload; static;
    class procedure GetUserDefaultLanguage(var BCP47: string; var LangID: Integer); overload; static;

    /// <summary>Get the canonicalized -default-lang parameter string for kmshell</summary>
    class function GetUserDefaultLangParameterString: string; static;
  private
    // Get the IKeymanKeyboardLanguageInstalled Object corresponding to the BCBP47Tag or **Add** it if not found for the supplied KeyboardID.
    // Returns Nil if no Keyboard is found with the supplied KeyboardID.
    class function GetKeyboardLanguage(const KeyboardID,
      BCP47Tag: string): IKeymanKeyboardLanguageInstalled; static;
  end;

implementation

uses
  System.SysUtils,
  System.Win.Registry,
  Winapi.Windows,

  Keyman.System.LanguageCodeUtils,

  KLog,
  BCP47Tag,
  glossary,
  kmint,
  RegistryKeys,
  utilkmshell,
  utilexecute,
  utilsystem;

{ TTIPMaintenance }

class function TTIPMaintenance.InstallTipsForPackages(Packages: TStrings): Boolean;
var
  i: Integer;
begin
  Result := True;
  KL.MethodEnter(nil, 'TTIPMaintenance.InstallTipsForPackages', [Packages.Text]);
  try
    for i := 0 to Packages.Count - 1 do
      // We'll attempt every package but return failure if any of them have issues
      Result := InstallTipForPackage(Packages.Names[i], Packages.ValueFromIndex[i]) and Result;
  finally
    KL.MethodExit(nil, 'TTIPMaintenance.InstallTipsForPackages', [Result]);
  end;
end;

class function TTIPMaintenance.InstallTipForPackage(const PackageFilename, BCP47Tag: string): Boolean;
var
  PackageID: string;
  n: Integer;
  pack: IKeymanPackageInstalled;
begin
  Result := False;
  KL.MethodEnter(nil, 'TTIPMaintenance.InstallTipForPackage', [PackageFilename, BCP47Tag]);
  try
    // This function has a known limitation: if a package contains more than one keyboard,
    // then the BCP47 association will be made only for the first keyboard. This is
    // considered an acceptable limitation at this time

    PackageID := ChangeFileExt(ExtractFileName(PackageFilename), '');
    n := kmcom.Packages.IndexOf(PackageID);
    if n < 0 then
      Exit(False);

    pack := kmcom.Packages[n];

    if pack.Keyboards.Count = 0 then
      Exit(False);

    if BCP47Tag = ''
      then Result := DoInstall(pack.Keyboards[0].ID, GetFirstLanguage(pack.Keyboards[0] as IKeymanKeyboardInstalled))
      else Result := DoInstall(pack.Keyboards[0].ID, BCP47Tag);
  finally
    KL.MethodExit(nil, 'TTIPMaintenance.InstallTipForPackage', [Result]);
  end;
end;

class function TTIPMaintenance.InstallTip(LangID: Integer; const KeyboardID, BCP47Tag,
  KeyboardToRemove: string): Boolean;
var
  lang: IKeymanKeyboardLanguageInstalled;
begin
  Result := False;
  KL.MethodEnter(nil, 'TTIPMaintenance.InstallTip', [LangID, KeyboardID, BCP47Tag, KeyboardToRemove]);
  try
    lang := GetKeyboardLanguage(KeyboardID, BCP47Tag);
    if lang = nil then
      Exit(False);

    // TODO: can this fail?
    (lang as IKeymanKeyboardLanguageInstalled2).InstallTip(LangID, KeyboardToRemove);
    Result := True;
  finally
    KL.MethodExit(nil, 'TTIPMaintenance.InstallTip', [Result]);
  end;
end;

class function TTIPMaintenance.RegisterTip(LangID: Integer; const KeyboardID,
  BCP47Tag: string): Boolean;
var
  lang: IKeymanKeyboardLanguageInstalled;
begin
  Result := False;
  KL.MethodEnter(nil, 'TTIPMaintenance.RegisterTip', [KeyboardID, BCP47Tag]);
  try
    lang := GetKeyboardLanguage(KeyboardID, BCP47Tag);
    if lang = nil then
      Exit(False);

    // TODO: can this fail?
    (lang as IKeymanKeyboardLanguageInstalled2).RegisterTip(LangID);
    Result := True;
  finally
    KL.MethodExit(nil, 'TTIPMaintenance.RegisterTip', [Result]);
  end;
end;

class function TTIPMaintenance.DoInstall(const KeyboardID,
  BCP47Tag: string): Boolean;
var
  lang: IKeymanKeyboardLanguageInstalled;
  RegistrationRequired: WordBool;
  TemporaryKeyboardID: WideString;
  LangID: Integer;
  childExitCode: Cardinal;
  CanonicalTag, Command: string;
begin
  Result := False;
  KL.MethodEnter(nil, 'TTIPMaintenance.DoInstall', [KeyboardID, BCP47Tag]);
  try
    CanonicalTag := (kmcom as IKeymanBCP47Canonicalization).GetCanonicalTag(BCP47Tag);
    lang := GetKeyboardLanguage(KeyboardID, CanonicalTag);
    if lang = nil then
    begin
      // The keyboard was not found
      KL.Log('lang not found: BCP47Tag = %s, CanonicalTag = %s', [BCP47Tag, CanonicalTag]);
      Exit(False);
    end;

    KL.Log('BCP47Tag = %s, CanonicalTag = %s, lang.BCP47Code = %s', [BCP47Tag, CanonicalTag, lang.BCP47Code]);

    if lang.IsInstalled then
      // After canonicalization, we may find the language is already installed
      Exit(True);

    TemporaryKeyboardID := '';
    LangID := 0;
    RegistrationRequired := False;

    if not (lang as IKeymanKeyboardLanguageInstalled2).FindInstallationLangID(LangID, TemporaryKeyboardID, RegistrationRequired, kifInstallTransientLanguage) then
    begin
      KL.Log('Failed to find installation langid');
      // We were not able to find a TIP, perhaps all transient TIPs have been used
      Exit(False);
    end;

    KL.Log('LangID=%x, TemporaryKeyboardID=%s, RegistrationRequired=%s', [LangID, TemporaryKeyboardID, BoolToStr(RegistrationRequired, True)]);

    if RegistrationRequired then
    begin
      Command := '-register-tip '+IntToHex(LangID,4)+' "'+KeyboardID+'" "'+lang.BCP47Code+'" '+GetUserDefaultLangParameterString;
      KL.Log('Calling elevated kmshell %s', [Command]);
      // This calls back into TTIPMaintenance.RegisterTip
      if WaitForElevatedConfiguration(0, Command) <> 0 then
        Exit(False);
    end;

    Command := '-install-tip '+IntToHex(LangID,4)+' "'+KeyboardID+'" "'+lang.BCP47Code+'" "'+TemporaryKeyboardID+'"';
    KL.Log('Calling user kmshell %s', [Command]);
    // This calls back into TTIPMaintenance.InstallTip
    if not TUtilExecute.WaitForProcess('"'+ParamStr(0)+'" '+Command, GetCurrentDir, childExitCode) or
      (childExitCode <> 0) then
    begin
      kmcom.Refresh;
      Exit(False);
    end;

    kmcom.Refresh;
    Result := True;
  finally
    KL.MethodExit(nil, 'TTIPMaintenance.DoInstall', [Result]);
  end;
end;

function GetDefaultHKL: HKL;
begin
  if not SystemParametersInfo(SPI_GETDEFAULTINPUTLANG, 0, @Result, 0) then
    Result := 0;
end;

class function TTIPMaintenance.DoRegister(const KeyboardID,
  BCP47Tag: string): Boolean;
var
  lang: IKeymanKeyboardLanguageInstalled;
  LangID: Integer;
  TemporaryKeyboardID: WideString;
  RegistrationRequired: WordBool;
begin
  Result := False;
  KL.MethodEnter(nil, 'TTIPMaintenance.DoRegister', [KeyboardID, BCP47Tag]);
  try
    lang := GetKeyboardLanguage(KeyboardID, (kmcom as IKeymanBCP47Canonicalization).GetCanonicalTag(BCP47Tag));
    if lang = nil then
      // The keyboard was not found
      Exit(False);

    if lang.IsInstalled or (lang as IKeymanKeyboardLanguageInstalled2).IsRegistered then
      // After canonicalization, we may find the language is already installed
      Exit(True);

    TemporaryKeyboardID := '';
    LangID := 0;
    RegistrationRequired := False;

    if (lang as IKeymanKeyboardLanguageInstalled2).FindInstallationLangID(LangID, TemporaryKeyboardID, RegistrationRequired, 0) then
    begin
      Result := not RegistrationRequired or RegisterTip(LangID, KeyboardID, lang.BCP47Code);
    end
    else
      Result := False;
  finally
    KL.MethodExit(nil, 'TTIPMaintenance.DoRegister', [Result]);
  end;
end;

class function TTIPMaintenance.GetUserDefaultLangParameterString: string;
var
  LangID: Integer;
  BCP47: string;
begin
  KL.MethodEnter(nil, 'TTIPMaintenance.GetUserDefaultLangParameterString', []);
  try
    GetUserDefaultLanguage(BCP47, LangID);
    Result := '-default-lang '+BCP47+' '+IntToHex(LangID,4);
  finally
    KL.MethodExit(nil, 'TTIPMaintenance.GetUserDefaultLangParameterString', [Result]);
  end;
end;

class function TTIPMaintenance.GetUserDefaultLanguage: string;
var
  LangID: Integer;
begin
  KL.MethodEnter(nil, 'TTIPMaintenance.GetUserDefaultLanguage', []);
  try
    GetUserDefaultLanguage(Result, LangID);
  finally
    KL.MethodExit(nil, 'TTIPMaintenance.GetUserDefaultLanguage', [Result]);
  end;
end;

class procedure TTIPMaintenance.GetUserDefaultLanguage(var BCP47: string; var LangID: Integer);
var
  r: TRegistry;
  tags: TStringList;
  v: string;
  keys: TStringList;
  key: string;

  function GetLangIDFromValueName: Integer;
  var
    values: TStringList;
    v: string;
  begin
    // In HKCU\Control Panel\International\User Profile\<bcp47>, already opened
    // by the caller, look for a value name such as '0453:00000453' or
    // '0804:{81D4E9C9-1D3B-41BC-9E6C-4B40BF79E35E}{FA550B04-5AD7-411F-A5AC-CA038EC515D7}'
    // and grab the LangID from there.
    values := TStringList.Create;
    try
      r.GetValueNames(values);
      for v in values do
      begin
        if Copy(v, 5, 1) = ':' then
        begin
          Result := StrToIntDef('$' + Copy(v, 1, 4), 0);
          if Result > 0 then
            Exit;
        end;
      end;
    finally
      values.Free;
    end;
    Result := 0;
  end;

begin
  KL.MethodEnter(nil, 'TTIPMaintenance.GetUserDefaultLanguage', []);
  try
    // Fallback result
    LangID := HKLToLanguageID(GetDefaultHKL);
    BCP47 := TLanguageCodeUtils.TranslateWindowsLanguagesToBCP47(LangID);

    // For Win10, look in CPL/International/UserProfile
    r := TRegistry.Create;
    try
      if not r.OpenKeyReadOnly(SRegKey_ControlPanelInternationalUserProfile) then
        Exit;
      if r.ValueExists(SRegValue_CPIUP_InputMethodOverride) then
      begin
        // Lookup the override input method BCP 47 tag
        v := r.ReadString(SRegValue_CPIUP_InputMethodOverride);
        keys := TStringList.Create;
        try
          r.GetKeyNames(keys);
          for key in keys do
          begin
            if r.OpenKeyReadOnly('\' + SRegKey_ControlPanelInternationalUserProfile + '\' + key) and
              r.ValueExists(v) then
            begin
              BCP47 := (kmcom as IKeymanBCP47Canonicalization).GetCanonicalTag(key);
              LangID := GetLangIDFromValueName;
              Break;
            end;
          end;
        finally
          keys.Free;
        end;
      end
      else if r.ValueExists(SRegValue_CPIUP_Languages) then
      begin
        // The first tag is the default language tag
        tags := TStringList.Create;
        try
          r.ReadMultiString(SRegValue_CPIUP_Languages, tags);
          if tags.Count > 0 then
          begin
            BCP47 := (kmcom as IKeymanBCP47Canonicalization).GetCanonicalTag(tags[0].Trim);
            if r.OpenKeyReadOnly('\' + SRegKey_ControlPanelInternationalUserProfile + '\' + BCP47) then
              LangID := GetLangIDFromValueName;
          end;
        finally
          tags.Free;
        end;
      end;
    finally
      r.Free;
    end;
  finally
    KL.Log('TTIPMaintenance.GetUserDefaultLanguage = BCP47:%s LangID:%d', [BCP47, LangID]);
    KL.MethodExit(nil, 'TTIPMaintenance.GetUserDefaultLanguage');
  end;
end;

class function TTIPMaintenance.GetFirstLanguage(Keyboard: IKeymanKeyboardFile): string;
begin
  if Keyboard.Languages.Count > 0
    then Result := Keyboard.Languages[0].BCP47Code
    else Result := GetUserDefaultLanguage;
end;

class function TTIPMaintenance.GetFirstLanguage(Keyboard: IKeymanKeyboardInstalled): string;
begin
  if Keyboard.Languages.Count > 0
    then Result := Keyboard.Languages[0].BCP47Code
    else Result := GetUserDefaultLanguage;
end;

class function TTIPMaintenance.GetKeyboardLanguage(const KeyboardID,
  BCP47Tag: string): IKeymanKeyboardLanguageInstalled;
var
  keyboard: IKeymanKeyboardInstalled;
  i, n: Integer;
begin
  n := kmcom.Keyboards.IndexOf(KeyboardID);
  if n < 0 then
  begin
    KL.Log('TTIPMaintenance.GetKeyboardLanguage = KeyboardID:%s not found', [KeyboardID]);
    Exit(nil);
  end;

  keyboard := kmcom.Keyboards[n];

  for i := 0 to keyboard.Languages.Count - 1 do
    if SameText(keyboard.Languages[i].BCP47Code, BCP47Tag) then
      Exit(keyboard.Languages[i]);

  Result := (keyboard.Languages as IKeymanKeyboardLanguagesInstalled2).Add(BCP47Tag);
end;

end.
