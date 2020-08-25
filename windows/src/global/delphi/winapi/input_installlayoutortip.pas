unit input_installlayoutortip;

interface

uses
  Winapi.Windows;

const
  ILOT_UNINSTALL = 1;

function InstallLayoutOrTip(FLayoutInstallString: PChar; Flags: DWord): Boolean;
function InstallLayoutOrTipUserReg(pszUserReg, pszSystemReg, pszSoftwareReg, FLayoutInstallString: PChar; dwFlags: DWord): Boolean;
procedure UnregisterTIPAndItsProfiles(AClsid: TGUID);

implementation

uses
  Winapi.ActiveX,
  System.SysUtils,
  System.Win.Comobj,
  Winapi.msctf,
  keyman_msctf,
  utiltsf;

function InstallLayoutOrTip(FLayoutInstallString: PChar; Flags: DWord): Boolean;
type
  TInstallLayoutOrTipFunc = function(psz: PWideChar; dwFlags: DWord): BOOL; stdcall;   // I4244
var
  hInputDll: THandle;
  PInstallLayoutOrTip: TInstallLayoutOrTipFunc;
begin
  hInputDll := LoadLibrary('input.dll');
  if hInputDll = 0 then
  begin
    Exit(False);
  end;

  try
    PInstallLayoutOrTip := TInstallLayoutOrTipFunc(GetProcAddress(hInputDll, 'InstallLayoutOrTip'));
    if not Assigned(PInstallLayoutOrTip) then
    begin
      Exit(False);
    end;

    if not PInstallLayoutOrTip(FLayoutInstallString, Flags) then
    begin
      Exit(False);
    end;
  finally
    FreeLibrary(hInputDll);
  end;

  Result := True;
end;

function InstallLayoutOrTipUserReg(pszUserReg, pszSystemReg, pszSoftwareReg, FLayoutInstallString: PChar; dwFlags: DWord): Boolean;
type
  TInstallLayoutOrTipUserRegFunc = function(pszUserReg, pszSystemReg, pszSoftwareReg, psz: PWideChar; dwFlags: DWord): BOOL; stdcall;   // I4244
var
  hInputDll: THandle;
  PInstallLayoutOrTipUserReg: TInstallLayoutOrTipUserRegFunc;
begin
  hInputDll := LoadLibrary('input.dll');
  if hInputDll = 0 then
  begin
    Exit(False);
  end;

  try
    PInstallLayoutOrTipUserReg := TInstallLayoutOrTipUserRegFunc(GetProcAddress(hInputDll, 'InstallLayoutOrTipUserReg'));
    if not Assigned(PInstallLayoutOrTipUserReg) then
    begin
      Exit(False);
    end;

    Result := PInstallLayoutOrTipUserReg(pszUserReg, pszSystemReg, pszSoftwareReg, FLayoutInstallString, dwFlags);
  finally
    FreeLibrary(hInputDll);
  end;
end;
procedure UnregisterTIPAndItsProfiles(AClsid: TGUID);
var
  pInputProcessorProfiles: ITfInputProcessorProfiles;
  pInputProcessorProfileMgr: keyman_msctf.ITfInputProcessorProfileMgr;
  ippEnum: IEnumTfInputProcessorProfiles;
  pcFetch: Cardinal;
  profile: keyman_msctf.TF_INPUTPROCESSORPROFILE;
  FLayoutInstallString: string;
begin
  OleCheck(CoCreateInstance(CLASS_TF_InputProcessorProfiles, nil, CLSCTX_INPROC_SERVER,
                          IID_ITfInputProcessorProfiles, pInputProcessorProfiles));

  if not Supports(pInputProcessorProfiles, IID_ITfInputProcessorProfileMgr, pInputProcessorProfileMgr) then   // I3743
  begin
    Exit;
  end;

  OleCheck(pInputProcessorProfileMgr.EnumProfiles(0, ippEnum));

  while ippEnum.Next(1, profile, pcFetch) = S_OK do
  begin
    if (profile.dwProfileType = TF_PROFILETYPE_INPUTPROCESSOR) and
      IsEqualGuid(profile.clsid, AClsid) then
    begin
      FLayoutInstallString := GetLayoutInstallString(profile.LangID, profile.guidProfile);
      InstallLayoutOrTip(PChar(FLayoutInstallString), ILOT_UNINSTALL);
      pInputProcessorProfileMgr.UnregisterProfile(AClsid, profile.langid, profile.guidProfile, 0);
    end;
  end;

  pInputProcessorProfiles.Unregister(AClsid);
end;

end.
