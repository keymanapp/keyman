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

const
  ilkInstallTransitoryLanguage= 1; // TODO move this to TLB

type
  TKPInstallKeyboardLanguage = class(TKPBase)
    // Expects LANGIDs and LocaleNames in format 'en-US en 0409';

    function FindInstallationLangID(const BCP47Tag: string;
      var LangID: Integer;
      var TemporaryKeyboardID: string;
      Flags: DWORD): Boolean;
    procedure RegisterTip(const KeyboardID, BCP47Tag, KeyboardName: string; LangID: Integer; IconFileName, LanguageName: string);
    procedure InstallTip(const KeyboardID, BCP47Tag: string; LangID: Integer);
    procedure UninstallTemporaryLayout(const LayoutString: string);

    procedure RegisterTransientTips(const KeyboardID, KeyboardName: string; IconFileName: string);

    constructor Create(AContext: TKeymanContext);
    destructor Destroy; override;
  private
    reg: TRegistry;
    RootPath: string;
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
function TKPInstallKeyboardLanguage.FindInstallationLangID(const BCP47Tag: string; var LangID: Integer; var TemporaryKeyboardID: string; Flags: DWORD): Boolean;
var
  ml: TMitigateWin10_1803.TMitigatedLanguage;
  Win8Lang: TWindows8Language;
  tag: string;
begin
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
    //
    // Installing a custom language only supported with Win8 and later
    //

    if not FWin8Languages.IsSupported then
    begin
      WarnFmt(KMN_W_TSF_COMError, VarArrayOf(['Can not install custom locales on Windows 7']));
      Exit(False);
    end;

    if (Flags and ilkInstallTransitoryLanguage) = 0 then
    begin
      // No warning, because this is to be expected
      Exit(False);
    end;

    //
    // Install user language with Powershell if it isn't present
    //
    LangID := 0;

    if (tag = '') or not InstallBCP47Language(tag) then
    begin
      WarnFmt(KMN_W_TSF_COMError, VarArrayOf(['Could not install BCP47 language '+BCP47Tag]));
      Exit(False);
    end;

    //
    // Find the new language ID
    //

    FWin8Languages.Refresh;
    Win8Lang := FWin8Languages.FindClosestByBCP47Tag(tag);
    if not Assigned(Win8Lang) then
    begin
      WarnFmt(KMN_W_TSF_COMError, VarArrayOf(['BCP47 language was installed but could not be located']));
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
      WarnFmt(KMN_W_TSF_COMError, VarArrayOf(['Expecting exactly one input method; instead got '+Win8Lang.InputMethods.Text]));
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
  FKeyboardName: string;
  guid: TGUID;
  IconIndex: Integer;
  pInputProcessorProfileMgr: ITfInputProcessorProfileMgr;
begin
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
  finally
    reg.Free;
  end;

  Context.Control.AutoApplyKeyman;
end;

procedure TKPInstallKeyboardLanguage.RegisterTransientTips(const KeyboardID,
  KeyboardName: string; IconFileName: string);
var
  FIsAdmin: Boolean;
  FKeyboardName: string;
  guid: TGUID;
  i, LangID: Integer;
  IconIndex: Integer;
  pInputProcessorProfileMgr: ITfInputProcessorProfileMgr;
begin
  if not KeyboardInstalled(KeyboardID, FIsAdmin) then
    ErrorFmt(KMN_E_ProfileInstall_KeyboardNotFound, VarArrayOf([KeyboardID]));   // I3888

  if KeyboardName = ''
    then FKeyboardName := KeyboardID + Ext_KeymanFile
    else FKeyboardName := KeyboardName;

  for i := 0 to 3 do
  begin
    LangID := i * $400 + $2000;

    reg := TRegistry.Create;
    try
      reg.RootKey := HKEY_LOCAL_MACHINE;
      RootPath := GetRegistryKeyboardInstallKey_LM(KeyboardID) + SRegSubKey_LanguageProfiles;
      if not reg.OpenKey(RootPath + '\$' + IntToHex(LangID, 4), True) then
        ErrorFmt(KMN_E_ProfileInstall_MustBeAllUsers, VarArrayOf([KeyboardID]));

      reg.WriteInteger(SRegValue_LanguageProfileLangID, LangID);
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
  FLayoutInstallString: string;
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

    if not reg.ValueExists(SRegValue_KeymanProfileGUID) then
      // TODO: better error code
      ErrorFmt(KMN_E_ProfileInstall_KeyboardNotFound, VarArrayOf([KeyboardID]));

    guid := StringToGuid(reg.ReadString(SRegValue_KeymanProfileGUID));
  finally
    reg.Free;
  end;

  //
  // Install the TIP into Windows
  //

  FLayoutInstallString := Format('%04.4x:%s%s', [LangID, GuidToString(c_clsidKMTipTextService),   // I4244
    GuidToString(guid)]);

  try   // I4494
    if not InstallLayoutOrTip(PChar(FLayoutInstallString), 0) then   // I4302
      // TODO: better error code
      ErrorFmt(KMN_E_ProfileInstall_KeyboardNotFound, VarArrayOf([KeyboardID]));
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
  // TODO: handle errors
  InstallLayoutOrTip(PChar(LayoutString), ILOT_UNINSTALL);
end;

end.
