(*
  Name:             DebugUtils
  Copyright:        Copyright (C) SIL International.
  Documentation:    
  Description:      
  Create Date:      10 Jan 2014

  Modified Date:    22 Jun 2015
  Authors:          mcdurdin
  Related Files:    
  Dependencies:     

  Bugs:             
  Todo:             
  Notes:            
  History:          10 Jan 2014 - mcdurdin - I4020 - V9.0 - Refactor TSF debug profile management into TDebugUtils
                    03 Jun 2014 - mcdurdin - I4248 - V9.0 - Refactor of kmtip
                    31 Dec 2014 - mcdurdin - I4331 - V9.0 - Debugger development
                    - disable debug tip when stopping debugging
                    27 Mar 2015 - mcdurdin - I4156 - V9.0 - Debug host keyboard needs to map through forced keyboard's preserved keys
                    22 Jun 2015 - mcdurdin - I4771 - Update regressiontest app for Engine and Developer 9.0
*)
unit DebugUtils;

interface

uses
  keymanapi_TLB,
  msctf;

type
  TDebugUtilProfile = record   // I4020
    Profile: TF_INPUTPROCESSORPROFILE;
  end;

  TDebugUtils = class
  private
    class var FProfiles: ITfInputProcessorProfiles;   // I3655
    class var FProfileMgr: ITfInputProcessorProfileMgr;   // I3655
    class procedure InitMSCTF;   // I3655
  public
    class function GetDebugHostKeyboard: IKeymanKeyboardInstalled;
    class function SelectTSFProfileForKeyboardLanguage(Lang: IKeymanKeyboardLanguage): HRESULT;   // I3655   // I4020
    class function GetActiveTSFProfile(var Profile: TDebugUtilProfile): HRESULT;   // I4020
    class function SetActiveTSFProfile(Profile: TDebugUtilProfile): HRESULT;   // I4020
  end;

var
  kmcom: ITavultesoftKeyman = nil;

procedure InitKeyman;

implementation

uses
  System.SysUtils,
  System.Win.ComObj,
  Vcl.Controls,
  Vcl.Dialogs,
  Vcl.Forms,
  Winapi.ActiveX,

//  UfrmMessages,

//  KeymanDeveloperUtils,
  utiltsf;

const
  SDebugHostKeyboardName = 'debughost';

{ TDebugUtils }

procedure InitKeyman;
begin
  kmcom := CreateComObject(CLASS_TavultesoftKeyman) as ITavultesoftKeyman;
end;

class function TDebugUtils.GetActiveTSFProfile(
  var Profile: TDebugUtilProfile): HRESULT;   // I4020
begin
  if not Assigned(FProfiles) then
    InitMSCTF;

  Result := FProfileMgr.GetActiveProfile(GUID_TFCAT_TIP_KEYBOARD, Profile.profile);
end;

class function TDebugUtils.GetDebugHostKeyboard: IKeymanKeyboardInstalled;
    function FindDebugHostKeyboard: IKeymanKeyboardInstalled;
    var
      i: Integer;
    begin
      for i := 1 to kmcom.Keyboards.Count do
      begin
        if kmcom.Keyboards[i].Name = SDebugHostKeyboardName then
        begin
          Result := kmcom.Keyboards[i];
          Exit;
        end;
      end;
      Result := nil;
    end;
var
  DebugHostKeyboard: string;
begin
  Result := FindDebugHostKeyboard;
  if not Assigned(Result) then
  begin
    DebugHostKeyboard := ExtractFilePath(ParamStr(0))+SDebugHostKeyboardName+'.kmx';
    if not FileExists(DebugHostKeyboard) then
    begin
      ShowMessage('Debug Host Keyboard file debughost.kmx is missing from '+ExtractFilePath(ParamStr(0))+'.');
      Exit(nil);
    end;

    ShowMessage('Debug Host Keyboard file debughost.kmx must be installed to enable debugging.');
    Exit(nil);
  end;

  if (Result as IKeymanKeyboardInstalled3).Languages.Count = 0 then
  begin
    ShowMessage('Debug Host Keyboard file debughost.kmx must be configured to enable debugging.');
    Exit(nil);
  end;
end;

class procedure TDebugUtils.InitMSCTF;   // I4020
begin
  FProfiles := CreateComObject(CLASS_TF_InputProcessorProfiles) as ITfInputProcessorProfiles;
  if not Supports(FProfiles, ITfInputProcessorProfileMgr, FProfileMgr) then  // I2864
    FProfileMgr := nil;
end;

class function TDebugUtils.SelectTSFProfileForKeyboardLanguage(Lang: IKeymanKeyboardLanguage): HRESULT;   // I4020
begin
  if not Assigned(FProfiles) then
    InitMSCTF;

  Result := FProfiles.ChangeCurrentLanguage(Lang.langID);
  if FAILED(Result) then Exit;

//  frmMessages.Add('Debugger', Format('Activating Debugger: Lang=%x clsid=%s guid=%s', [Lang.LangID, GUIDToString(c_clsidKMTipTextService), GUIDToString(Lang.ProfileGUID)]));

  Result := FProfileMgr.ActivateProfile(TF_PROFILETYPE_INPUTPROCESSOR, Lang.langID, c_clsidKMTipTextService, Lang.ProfileGUID, 0, TF_IPPMF_DONTCARECURRENTINPUTLANGUAGE);   // I4248
end;

class function TDebugUtils.SetActiveTSFProfile(Profile: TDebugUtilProfile): HRESULT;   // I4020   // I4331
const
  GUID_NULL: TGUID = '{00000000-0000-0000-0000-000000000000}';
begin
  if not Assigned(FProfiles) then
    InitMSCTF;

  with Profile.Profile do
  begin
//    frmMessages.Add('Debugger', Format('Deactivating Debugger: dwProfileType=%d Lang=%x HKL=%x', [dwProfileType, langid, hkl]));
    if dwProfileType = TF_PROFILETYPE_KEYBOARDLAYOUT then
      // Appears that some fields are not correctly nulled in GetProfile
      Result := FProfileMgr.ActivateProfile(dwProfileType, langid, GUID_NULL, GUID_NULL, HKL, 0)
    else
      Result := FProfileMgr.ActivateProfile(dwProfileType, langid, clsid, guidProfile, 0, 0);   // I4156
  end;
end;

end.
