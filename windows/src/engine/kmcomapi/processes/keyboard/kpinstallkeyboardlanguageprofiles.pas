(*
  Name:             kpinstallkeyboardlanguageprofiles
  Copyright:        Copyright (C) SIL International.
  Documentation:
  Description:
  Create Date:      1 Dec 2012

  Modified Date:    4 Mar 2015
  Authors:          mcdurdin
  Related Files:
  Dependencies:

  Bugs:
  Todo:
  Notes:
  History:          01 Dec 2012 - mcdurdin - I3624 - V9.0 - Install keyboard language profiles from Keyman COM API
                    01 Jan 2013 - mcdurdin - I3707 - V9.0 - Installed keyboards show keyboard short name instead of full name in Language Bar
                    01 Jan 2013 - mcdurdin - I3720 - V9.0 - Installing language profiles will fail if not administrator, so we should make that a precondition
                    17 Jan 2013 - mcdurdin - I3764 - V9.0 - Error and warning messages from COM API are not clear for language profiles
                    11 Aug 2013 - mcdurdin - I3768 - V9.0 - Remove TSF substitution code
                    11 Aug 2013 - mcdurdin - I3888 - V9.0 - Error messages when installing profiles are not accurate
                    01 May 2014 - mcdurdin - I3743 - V9.0 - switch from ITfInputProcessorProfiles to ITfInputProcessorProfileMgr in kmcomapi lang install
                    28 May 2014 - mcdurdin - I4244 - V9.0 - Use InstallLayoutOrTip to record changes to TIP installs
                    26 Jun 2014 - mcdurdin - I4302 - V9 - Refactor uninstall of TIP and its profiles into separate unit
                    04 Nov 2014 - mcdurdin - I4494 - Crash calling TSF [CrashID:kmshell.exe_9.0.473.0_2C45D42D_EOleSysError]
                    31 Dec 2014 - mcdurdin - I4555 - V9.0 - Keyboards without an icon must specify a default icon when registering to prevent control panel crashing
                    04 Mar 2015 - mcdurdin - I4607 - V9.0 - Support install of keyboard against fallback locales
*)
unit kpinstallkeyboardlanguageprofiles;   // I3624

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
  TKPInstallKeyboardLanguageProfiles = class(TKPBase)
    // Expects LANGIDs and LocaleNames in format 'en-US en 0409';
    procedure Execute(const KeyboardID, KeyboardName: string; LangIDs: array of Integer; IconFileName: string; InstallFirstOnly: Boolean); overload;  // I3707   // I3768   // I4607
    function Execute(const KeyboardID, KeyboardName, BCP47Tag, IconFileName, LanguageName: string): Boolean; overload;  // I3707   // I3768   // I4607
    constructor Create(AContext: TKeymanContext);
    destructor Destroy; override;
  private
    reg: TRegistry;
    RootPath: string;
    FWin8Languages: TWindows8LanguageList;
    function RegisterLocale(KeyboardName, BCP47Tag: string; LangID: Integer; IconFileName, LanguageName: string): Boolean;   // I3768
    function ConvertLangIDToBCP47Tag(LangID: Integer; var Locale: string): Boolean;
    function ConvertBCP47TagToLangID(Locale: string; var LangID: Integer): Boolean;
    function InstallBCP47Language(const FLocaleName: string): Boolean;
    function RemoveDefaultKeyboardForLanguage(const uninstall_keyboard: string): Boolean;
    function ExtractBaseBCP47Tag(BCP47Tag: string): string;
    function GetInputProcessorProfileMgr: ITfInputProcessorProfileMgr;
    function WaitForKeyboardToBeAvailable(LangID: Integer; var UninstallKeyboard: string): Boolean;
  end
  deprecated 'Use TKPInstallKeyboardLanguage';

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

{ TKPInstallKeyboardLanguageProfiles }

function TKPInstallKeyboardLanguageProfiles.ConvertLangIDToBCP47Tag(LangID: Integer; var Locale: string): Boolean;
var
  buf: array[0..LOCALE_NAME_MAX_LENGTH-1] of char;
  i: Integer;
begin
  if FWin8Languages.IsSupported then
    for i := 0 to FWin8Languages.Count - 1 do
      if FWin8Languages[i].LangID = LangID then
      begin
        Locale := FWin8Languages[i].BCP47Tag;
        Exit(True);
      end;

  Result := LCIDToLocaleName(LangID, buf, LOCALE_NAME_MAX_LENGTH, 0) > 0;
  if Result
    then Locale := buf
    else Locale := '';
end;

function TKPInstallKeyboardLanguageProfiles.GetInputProcessorProfileMgr: ITfInputProcessorProfileMgr;
var
  pInputProcessorProfiles: ITfInputProcessorProfiles;
begin
  pInputProcessorProfiles := nil;

  OleCheck(CoCreateInstance(CLASS_TF_InputProcessorProfiles, nil, CLSCTX_INPROC_SERVER,
                          IID_ITfInputProcessorProfiles, pInputProcessorProfiles));

  if not Supports(pInputProcessorProfiles, IID_ITfInputProcessorProfileMgr, Result) then   // I3743
    raise Exception.Create('Missing interface IID_ITfInputProcessorProfileMgr');
end;

constructor TKPInstallKeyboardLanguageProfiles.Create(AContext: TKeymanContext);
begin
  FWin8Languages := TWindows8LanguageList.Create;
  inherited Create(AContext);
end;

destructor TKPInstallKeyboardLanguageProfiles.Destroy;
begin
  FreeAndNil(FWin8Languages);
  inherited Destroy;
end;

procedure TKPInstallKeyboardLanguageProfiles.Execute(const KeyboardID, KeyboardName: string; LangIDs: array of Integer; IconFileName: string; InstallFirstOnly: Boolean);   // I3707   // I3768   // I4607
var
  FIsAdmin: Boolean;
  i: Integer;
  FKeyboardName: string;
begin
  if not KeyboardInstalled(KeyboardID, FIsAdmin) then
    ErrorFmt(KMN_E_ProfileInstall_KeyboardNotFound, VarArrayOf([KeyboardID]));   // I3888

  if not IsAdministrator and FIsAdmin then   // I3720
    ErrorFmt(KMN_E_ProfileInstall_MustBeAllUsers, VarArrayOf([KeyboardID]));   // I3764   // I3888

  if KeyboardName = ''
    then FKeyboardName := KeyboardID + Ext_KeymanFile
    else FKeyboardName := KeyboardName;

  reg := TRegistry.Create;
  try
    if FIsAdmin then
    begin
      reg.RootKey := HKEY_LOCAL_MACHINE;
      RootPath := GetRegistryKeyboardInstallKey_LM(KeyboardID) + SRegSubKey_LanguageProfiles;
    end
    else
    begin
      reg.RootKey := HKEY_CURRENT_USER;
      RootPath := GetRegistryKeyboardInstallKey_CU(KeyboardID) + SRegSubKey_LanguageProfiles;
    end;

    if reg.OpenKey(RootPath, True) then
    begin
      for i := 0 to High(LangIDs) do
        if RegisterLocale(FKeyboardName, '', LangIDs[i], IconFileName, '') and InstallFirstOnly then   // I3707   // I3768   // I4607
          Break;
    end;
  finally
    reg.Free;
  end;

  Context.Control.AutoApplyKeyman;
end;

function  TKPInstallKeyboardLanguageProfiles.Execute(const KeyboardID, KeyboardName, BCP47Tag, IconFileName, LanguageName: string): Boolean;
var
  FIsAdmin: Boolean;
  FKeyboardName: string;
begin
  if not KeyboardInstalled(KeyboardID, FIsAdmin) then
    ErrorFmt(KMN_E_ProfileInstall_KeyboardNotFound, VarArrayOf([KeyboardID]));   // I3888

  if not IsAdministrator and FIsAdmin then   // I3720
    ErrorFmt(KMN_E_ProfileInstall_MustBeAllUsers, VarArrayOf([KeyboardID]));   // I3764   // I3888

  if KeyboardName = ''
    then FKeyboardName := KeyboardID + Ext_KeymanFile
    else FKeyboardName := KeyboardName;

  reg := TRegistry.Create;
  try
    if FIsAdmin then
    begin
      reg.RootKey := HKEY_LOCAL_MACHINE;
      RootPath := GetRegistryKeyboardInstallKey_LM(KeyboardID) + SRegSubKey_LanguageProfiles;
    end
    else
    begin
      reg.RootKey := HKEY_CURRENT_USER;
      RootPath := GetRegistryKeyboardInstallKey_CU(KeyboardID) + SRegSubKey_LanguageProfiles;
    end;

    if reg.OpenKey(RootPath, True)
      then Result := RegisterLocale(FKeyboardName, BCP47Tag, 0, IconFileName, LanguageName)   // I3707   // I3768   // I4607
      else Result := False;
  finally
    reg.Free;
  end;

  Context.Control.AutoApplyKeyman;
end;

function TKPInstallKeyboardLanguageProfiles.ConvertBCP47TagToLangID(Locale: string; var LangID: Integer): Boolean;
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

function TKPInstallKeyboardLanguageProfiles.RegisterLocale(KeyboardName, BCP47Tag: string; LangID: Integer;
  IconFileName, LanguageName: string): Boolean;   // I3768
var
  guid: TGUID;
  FLayoutInstallString: string;
  IconIndex: Integer;
  Win8Lang: TWindows8Language;
  ml: TMitigateWin10_1803.TMitigatedLanguage;
  FMitigationApplied: Boolean;
  TempLangID: Integer;
  AEnabledInt: Integer;
  pInputProcessorProfileMgr: ITfInputProcessorProfileMgr;
  uninstall_keyboard: string;   // I3743
begin
  Result := False;

  //
  // Begin #1285 mitigation
  //
  if BCP47Tag = '' then
  begin
    FMitigationApplied := TMitigateWin10_1803.IsMitigationRequired(LangID, ml);
  end
  else if ConvertBCP47TagToLangID(BCP47Tag, TempLangID) then
  begin
    FMitigationApplied := TMitigateWin10_1803.IsMitigationRequired(TempLangID, ml);
  end
  else
    FMitigationApplied := False;

  if FMitigationApplied then
  begin
    BCP47Tag := ml.NewLanguage.BCP47;
    LangID := ml.NewLanguage.Code;
    WarnFmt(KMN_W_ProfileInstall_Win10_1803_MitigationApplied, VarArrayOf([ml.OriginalLanguage.Name, ml.NewLanguage.Name]));
  end;
  //
  // End #1285 mitigation
  //

  if BCP47Tag = '' then
  begin
    if not ConvertLangIDToBCP47Tag(LangID, BCP47Tag) then
      Exit;
  end
  else if not ConvertBCP47TagToLangID(BCP47Tag, LangID) then
  begin
    //
    // Installing a custom language only supported with Win8 and later
    //

    if not FWin8Languages.IsSupported then
      Exit;

    //
    // Install user language with Powershell if it isn't present
    //

    LangID := 0;

    BCP47Tag := ExtractBaseBCP47Tag(BCP47Tag);

    if (BCP47Tag = '') or not InstallBCP47Language(BCP47Tag) then
    begin
      WarnFmt(KMN_W_TSF_COMError, VarArrayOf(['Could not install BCP47 language '+BCP47Tag]));
      Exit;
    end;

    //
    // Find the new language ID
    //

    FWin8Languages.Refresh;
    Win8Lang := FWin8Languages.FindClosestByBCP47Tag(BCP47Tag);
    if not Assigned(Win8Lang) then
    begin
      WarnFmt(KMN_W_TSF_COMError, VarArrayOf(['BCP47 language was installed but could not be located']));
      Exit;
    end;

    // BCP47Tag will now be reduced to the installed tag, which may mean script and region have been stripped

    LangID := Win8Lang.LangID;

    // We'll wait for the keyboard to made available so we can remove it again...
    if not WaitForKeyboardToBeAvailable(LangID, uninstall_keyboard) then
      uninstall_keyboard := '';
  end;

  reg.OpenKey('\' + RootPath + '\' + BCP47Tag, True);
  reg.WriteInteger(SRegValue_LanguageProfileLangID, LangID);
  reg.WriteString(SRegValue_LanguageProfileLocale, BCP47Tag);
  reg.WriteString(SRegValue_LanguageProfileName, LanguageName);

  if not reg.ValueExists(SRegValue_KeymanProfileGUID) then
  begin
    CreateGuid(&guid);
    reg.WriteString(SRegValue_KeymanProfileGUID, GuidToString(guid));
  end
  else
  begin
    //TODO: fix this
  end;

  if IconFileName = '' then   // I4555
  begin
    IconFileName := TKeymanPaths.KeymanEngineInstallPath(TKeymanPaths.S_KeymanExe);
    IconIndex := 1;
  end
  else
    IconIndex := 0;

  FLayoutInstallString := Format('%04.4x:%s%s', [LangID, GuidToString(c_clsidKMTipTextService),   // I4244
    GuidToString(guid)]);

  { Register the keyboard profile }

  if Context.Control.IsKeymanRunning
    then AEnabledInt := 1
    else AEnabledInt := 0;

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
    AEnabledInt,
    0));

  { Save TIP to registry }   // I4244

  try   // I4494
    if not InstallLayoutOrTip(PChar(FLayoutInstallString), 0) then   // I4302
      ErrorFmt(KMN_E_ProfileInstall_KeyboardNotFound, VarArrayOf(['bogus2'])); //TODO FIX CODE
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

  if uninstall_keyboard <> '' then
  begin
    // TODO: this is going to mess things up because of cached data.
    RemoveDefaultKeyboardForLanguage(uninstall_keyboard);
  end;

  Result := True;
end;

// TODO Replace with TBCP47Tag.GetCanonicalTag
function TKPInstallKeyboardLanguageProfiles.ExtractBaseBCP47Tag(BCP47Tag: string): string;
var
  FTag: TBCP47Tag;
begin
  Result := BCP47Tag;

  FTag := TBCP47Tag.Create(BCP47Tag);
  try
    FTag.Canonicalize;
    Result := FTag.Tag;
  finally
    FTag.Free;
  end;
end;

//procedure TKPIntsallKeyboardLanguageProfiles.InstallKeymanKeyboardW

function TKPInstallKeyboardLanguageProfiles.InstallBCP47Language(const FLocaleName: string): Boolean;
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

{
  Profile list updates can be asynchronous. This can happen when the profile was
  installed by a separate process. We need to wait at this point before the
  initial installation of the base keyboard is visible in the list of
  profiles.

  Warning: this function can be re-entrant due to a message loop. This
  appears to be unavoidable.
}
function TKPInstallKeyboardLanguageProfiles.WaitForKeyboardToBeAvailable(LangID: Integer; var UninstallKeyboard: string): Boolean;
var
  ppEnum: IEnumTfInputProcessorProfiles;
  profile: TF_INPUTPROCESSORPROFILE;
  pcFetch: LongWord;
  pInputProcessorProfileMgr: ITfInputProcessorProfileMgr;   // I3743
  msg: TMSG;
  Tries: Integer;
  t: Cardinal;
begin
  Tries := 0;
  repeat
    pInputProcessorProfileMgr := GetInputProcessorProfileMgr;   // I3743

    pInputProcessorProfileMgr.EnumProfiles(LangID, ppEnum);
    while ppEnum.Next(1, profile, pcFetch) = S_OK do
    begin
      if profile.dwProfileType = TF_PROFILETYPE_KEYBOARDLAYOUT then
      begin
        UninstallKeyboard := Format('%04.4x:%08.8x', [LangID, HKLToKeyboardID(profile.HKL)]);
        Exit(True);
      end
      else if (profile.dwProfileType = TF_PROFILETYPE_INPUTPROCESSOR) and (profile.catid = GUID_TFCAT_TIP_KEYBOARD) then
      begin
        UninstallKeyboard := Format('%04.4x:%s%s', [LangID, GUIDToString(profile.clsid), GUIDToString(profile.guidProfile)]);
        Exit(True);
      end;
    end;

    pInputProcessorProfileMgr := nil;
    ppEnum := nil;

    t := GetTickCount;

    // Sleep 1 second and retry
    while (GetTickCount - t < 1000) do
    begin
      Sleep(10);
      while PeekMessage(msg, 0, 0, 0, PM_REMOVE) do
        DispatchMessage(msg);
    end;

    Inc(Tries);
  until Tries >= 10;

  // We won't fail the keyboard install, but we'll be leaving a spurious keyboard
  // in the system.
  WarnFmt(KMN_W_UninstallFileInUse, VarArrayOf(['keyboard not found']));
  Result := False;
end;


function TKPInstallKeyboardLanguageProfiles.RemoveDefaultKeyboardForLanguage(const uninstall_keyboard: string): Boolean; //; FLocaleName: string; FInstalledGUID: TGUID): Boolean;
begin
  Result := InstallLayoutOrTip(PChar(uninstall_keyboard), ILOT_UNINSTALL);
  if not Result then
  begin
    WarnFmt(KMN_W_UninstallFileInUse, VarArrayOf([uninstall_keyboard]));
    Result := False;
  end;
end;

end.
