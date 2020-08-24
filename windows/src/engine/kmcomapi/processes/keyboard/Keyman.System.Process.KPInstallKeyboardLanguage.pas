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
  TKPInstallKeyboardLanguageFlags = set of (ilkInstallTransitoryLanguage);

type
  TKPInstallKeyboardLanguage = class(TKPBase)
    function FindInstallationLangID(const BCP47Tag: string;
      var LangID: Integer;
      var TemporaryKeyboardID: string;
      Flags: TKPInstallKeyboardLanguageFlags): Boolean;
    procedure RegisterTip(const KeyboardID, BCP47Tag, KeyboardName: string; LangID: Integer; IconFileName, LanguageName: string);
    procedure InstallTip(const KeyboardID, BCP47Tag: string; LangID: Integer);
    procedure UninstallTemporaryLayout(const LayoutString: string);

    procedure RegisterTransientTips(const KeyboardID, KeyboardName: string; IconFileName: string);

    constructor Create(AContext: TKeymanContext);
    destructor Destroy; override;
  private
    FWin8Languages: TWindows8LanguageList;
    function ConvertBCP47TagToLangID(Locale: string; var LangID: Integer): Boolean;
    function InstallBCP47Language(const FLocaleName: string): Boolean;
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

// TODO: change LangID to Winapi.Windows.LANGID
function TKPInstallKeyboardLanguage.FindInstallationLangID(const BCP47Tag: string; var LangID: Integer; var TemporaryKeyboardID: string; Flags: TKPInstallKeyboardLanguageFlags): Boolean;
var
  ml: TMitigateWin10_1803.TMitigatedLanguage;
  Win8Lang: TWindows8Language;
  tag: string;
begin
  if BCP47Tag = '' then
    ErrorFmt(KMN_E_ProfileInstall_InvalidBCP47Tag, VarArrayOf([BCP47Tag]));

  tag := BCP47Tag;
  TemporaryKeyboardID := '';

  if ConvertBCP47TagToLangID(tag, LangID) then
  begin
    if TMitigateWin10_1803.IsMitigationRequired(LangID, ml) then
    begin
      LangID := ml.NewLanguage.Code;
      WarnFmt(KMN_W_ProfileInstall_Win10_1803_MitigationApplied, VarArrayOf([ml.OriginalLanguage.Name, ml.NewLanguage.Name]));
    end;
  end
  else
  begin
    if not (ilkInstallTransitoryLanguage in Flags) then
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
    // Install user language with Powershell if it isn't present
    //

    LangID := 0;

    if (tag = '') or not InstallBCP47Language(tag) then
    begin
      WarnFmt(KMN_W_ProfileInstall_FailedToInstallLanguage, VarArrayOf([tag]));
      Exit(False);
    end;

    //
    // Find the new language ID
    //

    FWin8Languages.Refresh;
    Win8Lang := FWin8Languages.FindClosestByBCP47Tag(tag);
    if not Assigned(Win8Lang) then
    begin
      WarnFmt(KMN_W_ProfileInstall_LanguageInstalledButNotFound, VarArrayOf([tag]));
      Exit(False);
    end;

    LangID := Win8Lang.LangID;

    if Win8Lang.InputMethods.Count = 1 then
    begin
      TemporaryKeyboardID := Win8Lang.InputMethods[0];
    end
    else
      // We'll continue on, but this is unexpected, so we won't try and uninstall the temporary input method. The user
      // will have more than one input method installed.
      WarnFmt(KMN_W_ProfileInstall_MoreThanOneInputMethodInstalled, VarArrayOf([Win8Lang.InputMethods.Text]));
  end;

  Result := True;
end;


function TKPInstallKeyboardLanguage.ConvertBCP47TagToLangID(Locale: string;
  var LangID: Integer): Boolean;
var
  Win8Lang: TWindows8Language;
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
    // Assuming that the tag is a Language-Script-Region triplet at most.
    // If you use -Variant or -Extension then YMMV.
    with TBCP47Tag.Create(Locale) do
    try
      Script := '';
      Locale := Tag;
    finally
      Free;
    end;
  end;

  LangID := LocaleNameToLCID(PWideChar(Locale), 0);

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

procedure TKPInstallKeyboardLanguage.InstallTip(const KeyboardID, BCP47Tag: string; LangID: Integer);
var
  FIsAdmin: Boolean;
  guid: TGUID;
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

    guid := StringToGuid(reg.ReadString(SRegValue_KeymanProfileGUID));
  finally
    reg.Free;
  end;

  //
  // Install the TIP into Windows
  //

  FLayoutInstallString := GetLayoutInstallString(LangID, guid);

  if not InstallLayoutOrTip(PChar(FLayoutInstallString), 0) then   // I4302
    ErrorFmt(KMN_E_ProfileInstall_InstallLayoutOrTipFailed, VarArrayOf([KeyboardID]));
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
