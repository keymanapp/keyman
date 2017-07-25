(*
  Name:             KeyboardTIPCheck
  Copyright:        Copyright (C) SIL International.
  Documentation:    
  Description:      
  Create Date:      23 Jun 2015

  Modified Date:    23 Jun 2015
  Authors:          mcdurdin
  Related Files:    
  Dependencies:     

  Bugs:             
  Todo:             
  Notes:            
  History:          23 Jun 2015 - mcdurdin - I4773 - Keyman needs to rebuild its language profiles if they are inadvertently deleted
                    
*)
unit KeyboardTIPCheck;   // I4773

interface

uses
  System.SysUtils,
  System.Win.ComObj,
  Winapi.ActiveX,
  kmint,
  keymanapi_TLB,
  Winapi.msctf,
  keyman_msctf;

type
  TKeyboardTIPCheck = class
  private
    class procedure ReinstallProfile(pInputProcessorProfileMgr: ITfInputProcessorProfileMgr; kmkbd: IKeymanKeyboardInstalled; kmlang: IKeymanKeyboardLanguageInstalled);
  public
    class function CheckKeyboardTIPInstallStatus: Boolean;
  end;

implementation

uses
  KeymanPaths,
  utiltsf;

{ TKeyboardTIPCheck }

class function TKeyboardTIPCheck.CheckKeyboardTIPInstallStatus: Boolean;
var
  i, j: Integer;
  k: IKeymanKeyboardInstalled;
  ipp: ITfInputProcessorProfiles;
  ippm: ITfInputProcessorProfileMgr;
  ppe: IEnumTfInputProcessorProfiles;
  profile: TF_INPUTPROCESSORPROFILE;
  n: Cardinal;
  Found: Boolean;
begin
  Result := True;

  ipp := CreateComObject(CLASS_TF_InputProcessorProfiles) as ITfInputProcessorProfiles;
  ippm := ipp as ITfInputProcessorProfileMgr;

  for i := 0 to kmcom.Keyboards.Count - 1 do
  begin
    k := kmcom.Keyboards[i];
    for j := 0 to k.Languages.Count - 1 do
    begin
      Found := False;

      ippm.EnumProfiles(k.Languages[j].LangID, ppe);
      while ppe.Next(1, profile, n) = S_OK do
      begin
        if IsEqualGUID(k.Languages[j].ProfileGUID, profile.guidProfile) then
        begin
          Found := True;
          Break;
        end;
      end;

      if not Found then
      begin
        if kmcom.SystemInfo.IsAdministrator then
        begin
          ReinstallProfile(ippm, k, k.Languages[j]);
        end
        else
          Result := False;
      end;
    end;
  end;
end;

function GetKeyboardIconFileName(const KeyboardFileName: string): string;   // I3599
begin
  Result := ChangeFileExt(KeyboardFileName, '.kmx.ico');   // I3581
end;

class procedure TKeyboardTIPCheck.ReinstallProfile(pInputProcessorProfileMgr: ITfInputProcessorProfileMgr;
  kmkbd: IKeymanKeyboardInstalled; kmlang: IKeymanKeyboardLanguageInstalled);
var
  IconFileName: string;
  IconIndex: Integer;
  guidProfile: TGUID;
  FLayoutInstallString: string;
begin
  IconFileName := GetKeyboardIconFileName(kmkbd.Filename);
  if not FileExists(IconFileName) then
    IconFileName := '';

  if IconFileName = '' then   // I4555
  begin
    IconFileName := TKeymanPaths.KeymanEngineInstallPath(TKeymanPaths.S_KeymanExe);
    IconIndex := 1;
  end
  else
    IconIndex := 0;

  guidProfile := kmlang.ProfileGUID;

  OleCheck(pInputProcessorProfileMgr.RegisterProfile(   // I3743
    c_clsidKMTipTextService,
    kmlang.LangID,
    guidProfile,
    PWideChar(kmkbd.Name),
    Length(kmkbd.Name),
    PWideChar(IconFileName),
    Length(IconFileName),
    IconIndex,
    0,
    0,
    1,
    0));

  FLayoutInstallString := Format('%04.4x:%s%s', [kmlang.LangID, GuidToString(c_clsidKMTipTextService),   // I4244
    GuidToString(guidProfile)]);

  (*
  pInputProcessorProfilesEx := pInputProcessorProfiles as ITfInputProcessorProfilesEx;
  ResPath := 'c:\temp\res.dll';
  OleCheck(pInputProcessorProfilesEx.SetLanguageProfileDisplayName(c_clsidKMTipTextService, nLocale, guid, ResPath, Length(ResPath), 101));
  *)
(*
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
*)



//      k.Languages[j].ProfileGUID

end;

end.
