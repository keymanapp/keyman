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

      if not Succeeded(ippm.EnumProfiles(k.Languages[j].LangID, ppe)) then
        Exit(False);

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
          //TTIPMaintenance.DoRegister(k.ID, k.Languages[j].BCP47Code);
          (k.Languages[j] as IKeymanKeyboardLanguageInstalled2).RegisterTip(k.Languages[j].LangID);
        end
        else
          Result := False;
      end;
    end;
  end;
end;

end.
