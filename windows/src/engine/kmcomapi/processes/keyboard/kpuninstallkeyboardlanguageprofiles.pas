(*
  Name:             kpuninstallkeyboardlanguageprofiles
  Copyright:        Copyright (C) 2003-2017 SIL International.
  Documentation:    
  Description:      
  Create Date:      13 Dec 2012

  Modified Date:    4 Nov 2014
  Authors:          mcdurdin
  Related Files:    
  Dependencies:     

  Bugs:             
  Todo:             
  Notes:            
  History:          13 Dec 2012 - mcdurdin - I3663 - V9.0 - Uninstall of keyboard does not remove KMTIP entries
                    01 Jan 2013 - mcdurdin - I3624 - V9.0 - Install keyboard language profiles from Keyman COM API
                    01 Jan 2013 - mcdurdin - I3721 - V9.0 - If uninstalling a language profile fails, warn but do not error
                    01 Jan 2013 - mcdurdin - I3722 - V9.0 - Uninstalling a language profile does not remove it from the registry
                    01 Jan 2013 - mcdurdin - I3723 - V9.0 - Uninstalling a language profile requires Admin, so precondition
                    17 Jan 2013 - mcdurdin - I3764 - V9.0 - Error and warning messages from COM API are not clear for language profiles
                    11 Aug 2013 - mcdurdin - I3888 - V9.0 - Error messages when installing profiles are not accurate
                    01 May 2014 - mcdurdin - I3743 - V9.0 - switch from ITfInputProcessorProfiles to ITfInputProcessorProfileMgr in kmcomapi lang install
                    28 May 2014 - mcdurdin - I4244 - V9.0 - Use InstallLayoutOrTip to record changes to TIP installs
                    26 Jun 2014 - mcdurdin - I4302 - V9 - Refactor uninstall of TIP and its profiles into separate unit
                    04 Nov 2014 - mcdurdin - I4494 - Crash calling TSF [CrashID:kmshell.exe_9.0.473.0_2C45D42D_EOleSysError]
*)
unit kpuninstallkeyboardlanguageprofiles;   // I3663

interface

uses
  System.Win.Registry,
  Winapi.Windows,

  kpbase,
  Winapi.msctf,
  keyman_msctf;

type
  TKPUninstallKeyboardLanguageProfiles = class(TKPBase)
    procedure Execute(const KeyboardName: string); overload;   // I3624
    procedure Execute(const KeyboardName, LocaleName: string); overload;   // I3624
  private
    pInputProcessorProfiles: ITfInputProcessorProfiles;
    pInputProcessorProfileMgr: ITfInputProcessorProfileMgr;   // I3743
    reg: TRegistry;
    RootPath: string;
    function UnregisterLocale(const KeyboardName, Locale: string): Boolean;
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
  keymanerrorcodes,
  RegistryKeys,
  utilkeyman,
  utilstr,
  utiltsf;

{ TKPUninstallKeyboardLanguageProfiles }

procedure TKPUninstallKeyboardLanguageProfiles.Execute(const KeyboardName,   // I3624
  LocaleName: string);
var
  FInstByAdmin: Boolean;
begin
  if not KeyboardInstalled(KeyboardName, FInstByAdmin) then
    ErrorFmt(KMN_E_ProfileInstall_KeyboardNotFound, VarArrayOf([KeyboardName]));   // I3888

  if FInstByAdmin and not IsAdministrator then
    ErrorFmt(KMN_E_ProfileUninstall_MustBeAllUsers, VarArrayOf([KeyboardName]));   // I3888

  OleCheck(CoCreateInstance(CLASS_TF_InputProcessorProfiles, nil, CLSCTX_INPROC_SERVER,
                          IID_ITfInputProcessorProfiles, pInputProcessorProfiles));

  if not Supports(pInputProcessorProfiles, IID_ITfInputProcessorProfileMgr, pInputProcessorProfileMgr) then   // I3743
    raise Exception.Create('Missing interface IID_ITfInputProcessorProfileMgr');

  reg := TRegistry.Create;
  try
    if FInstByAdmin then
    begin
      reg.RootKey := HKEY_LOCAL_MACHINE;
      RootPath := GetRegistryKeyboardInstallKey_LM(KeyboardName) + SRegSubKey_LanguageProfiles;
    end
    else
    begin
      reg.RootKey := HKEY_CURRENT_USER;
      RootPath := GetRegistryKeyboardInstallKey_CU(KeyboardName) + SRegSubKey_LanguageProfiles;
    end;

    UnregisterLocale(KeyboardName, LocaleName);
  finally
    reg.Free;
    pInputProcessorProfiles := nil;
    pInputProcessorProfileMgr := nil;   // I3743
  end;

end;

function TKPUninstallKeyboardLanguageProfiles.UnregisterLocale(const KeyboardName, Locale: string): Boolean;
var
  nLocale: Integer;
  guid: TGUID;
  FLayoutInstallString: string;
begin
  Result := False;

  if not reg.OpenKey('\' + RootPath + '\' + Locale, False) or
      not reg.ValueExists(SRegValue_LanguageProfileLangID) or
      not reg.ValueExists(SRegValue_KeymanProfileGUID) then
    Exit;

  nLocale := reg.ReadInteger(SRegValue_LanguageProfileLangID);
  guid := StringToGuid(reg.ReadString(SRegValue_KeymanProfileGUID));

  FLayoutInstallString := Format('%04.4x:%s%s', [nLocale, GuidToString(c_clsidKMTipTextService),
    GuidToString(guid)]);

  try   // I4494
    if not InstallLayoutOrTip(PChar(FLayoutInstallString), ILOT_UNINSTALL) then   // I4302
      WarnFmt(KMN_E_ProfileInstall_KeyboardNotFound, VarArrayOf([FLayoutInstallString]));
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

  if pInputProcessorProfileMgr.UnregisterProfile(c_clsidKMTipTextService, nLocale, guid, 0) <> S_OK then   // I3743
  //if pInputProcessorProfiles.RemoveLanguageProfile(c_clsidKMTipTextService, nLocale, guid) = E_FAIL then   // I3721
    WarnFmt(KMN_W_KeyboardUninstall_ProfileNotFound, VarArrayOf([Locale, KeyboardName]));

  { Remove TIP from registry }   // I4244

  reg.CloseKey;   // I3722
  reg.DeleteKey('\'+RootPath+'\'+Locale);   // I3722

  Result := True;
end;

procedure TKPUninstallKeyboardLanguageProfiles.Execute(const KeyboardName: string);
var
  sLocales: TStrings;
  sLocale: string;
  FInstByAdmin: Boolean;
  i: Integer;
  Path: string;
begin
  if not KeyboardInstalled(KeyboardName, FInstByAdmin) then
    ErrorFmt(KMN_E_ProfileInstall_KeyboardNotFound, VarArrayOf([KeyboardName]));   // I3888

  if FInstByAdmin and not IsAdministrator then   // I3723
    ErrorFmt(KMN_E_ProfileUninstall_MustBeAllUsers, VarArrayOf([KeyboardName]));   // I3764

  OleCheck(CoCreateInstance(CLASS_TF_InputProcessorProfiles, nil, CLSCTX_INPROC_SERVER,
                          IID_ITfInputProcessorProfiles, pInputProcessorProfiles));

  if not Supports(pInputProcessorProfiles, IID_ITfInputProcessorProfileMgr, pInputProcessorProfileMgr) then   // I3743
    raise Exception.Create('Missing interface IID_ITfInputProcessorProfileMgr');

  sLocales := TStringList.Create;
  reg := TRegistry.Create;
  with reg do
  try
    if FInstByAdmin then
    begin
      RootKey := HKEY_LOCAL_MACHINE;
      Path := GetRegistryKeyboardInstallKey_LM(KeyboardName);
    end
    else
    begin
      RootKey := HKEY_CURRENT_USER;
      Path := GetRegistryKeyboardInstallKey_CU(KeyboardName);
    end;
    RootPath := Path + SRegSubKey_LanguageProfiles;
    if OpenKey(RootPath, True) then
    begin
      GetKeyNames(sLocales);
      for i := 0 to sLocales.Count - 1 do
      begin
        sLocale := sLocales[i];
        UnregisterLocale(KeyboardName, sLocale);
      end;
    end;
  finally
    Free;
    sLocales.Free;
  end;

  pInputProcessorProfiles := nil;
  pInputProcessorProfileMgr := nil;   // I3743

  Context.Control.AutoApplyKeyman;
end;

end.
