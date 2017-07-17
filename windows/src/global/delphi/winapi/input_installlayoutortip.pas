unit input_installlayoutortip;

interface

uses
  Winapi.Windows;

const
  ILOT_UNINSTALL = 1;

function InstallLayoutOrTip(FLayoutInstallString: PChar; Flags: DWord): Boolean;
procedure UnregisterTIPAndItsProfiles(AClsid: TGUID);

implementation

uses
  Winapi.ActiveX,
  System.SysUtils,
  System.Win.Comobj,
  Winapi.msctf,
  keyman_msctf;

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

    if not PInstallLayoutOrTip(FLayoutInstallString, 0) then
    begin
      Exit(False);
    end;
  finally
    FreeLibrary(hInputDll);
  end;

  Result := True;
end;

procedure UnregisterTIPAndItsProfiles(AClsid: TGUID);
var
  pInputProcessorProfiles: ITfInputProcessorProfiles;
  pInputProcessorProfileMgr: ITfInputProcessorProfileMgr;
  ippEnum: IEnumTfInputProcessorProfiles;
  pcFetch: Cardinal;
  profile: TF_INPUTPROCESSORPROFILE;
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
      FLayoutInstallString := Format('%04.4x:%s%s', [profile.langid, GuidToString(AClsid),   // I4244
        GuidToString(profile.guidProfile)]);
      InstallLayoutOrTip(PChar(FLayoutInstallString), ILOT_UNINSTALL);
      pInputProcessorProfileMgr.UnregisterProfile(AClsid, profile.langid, profile.guidProfile, 0);
    end;
  end;

  pInputProcessorProfiles.Unregister(AClsid);
end;

end.
