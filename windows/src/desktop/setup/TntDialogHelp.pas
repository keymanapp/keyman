(*
  Name:             TntDialogHelp
  Copyright:        Copyright (C) SIL International.
  Documentation:    
  Description:      
  Create Date:      19 Jun 2007

  Modified Date:    18 May 2012
  Authors:          mcdurdin
  Related Files:    
  Dependencies:     

  Bugs:             
  Todo:             
  Notes:            
  History:          19 Jun 2007 - mcdurdin - I817 - Translate to Unicode and remove Forms dependence
                    18 May 2012 - mcdurdin - I3306 - V9.0 - Remove TntControls + Win9x support
*)
unit TntDialogHelp;  // I3306

interface

uses
  Windows;

type
  TMsgDlgType = (mtWarning, mtError, mtInformation, mtConfirmation, mtCustom);
  TMsgDlgBtn = (mbYes, mbNo, mbOK, mbCancel, mbAbort, mbRetry, mbIgnore,
    mbAll, mbNoToAll, mbYesToAll, mbHelp);
  TMsgDlgButtons = set of TMsgDlgBtn;

const
  mbYesNo = [mbYes, mbNo];
  mbYesNoCancel = [mbYes, mbNo, mbCancel];
  mbYesAllNoAllCancel = [mbYes, mbYesToAll, mbNo, mbNoToAll, mbCancel];
  mbOKCancel = [mbOK, mbCancel];
  mbAbortRetryIgnore = [mbAbort, mbRetry, mbIgnore];
  mbAbortIgnore = [mbAbort, mbIgnore];

const
  mrNone     = 0;
  mrOk       = idOk;
  mrCancel   = idCancel;
  mrAbort    = idAbort;
  mrRetry    = idRetry;
  mrIgnore   = idIgnore;
  mrYes      = idYes;
  mrNo       = idNo;
  mrAll      = mrNo + 1;
  mrNoToAll  = mrAll + 1;
  mrYesToAll = mrNoToAll + 1;

type
  TModalResult = Low(Integer)..High(Integer);

procedure ShowMessageW(const Message: WideString);
function MessageDlgW(const Msg: WideString; DlgType: TMsgDlgType;
  Buttons: TMsgDlgButtons; HelpCtx: Longint): Integer;

implementation

uses
  bootstrapmain,
  SetupStrings; // for localization

function Tnt_MessageBoxW(hWnd: HWND; lpText, lpCaption: PWideChar; uType: UINT): Integer;
begin
  Result := MessageBoxW{TNT-ALLOW MessageBoxW}(hWnd, lpText, lpCaption, uType);
end;


function MessageDlgW(const Msg: WideString; DlgType: TMsgDlgType;
  Buttons: TMsgDlgButtons; HelpCtx: Longint): Integer;
const
//mtWarning, mtError, mtInformation, mtConfirmation, mtCustom
  MBFlags: array[TMsgDlgType] of Integer = (MB_ICONWARNING, MB_ICONERROR, MB_ICONINFORMATION, MB_ICONQUESTION, 0);
  //MBButtons: array[TMsgDlgBtn] of Integer = (MB_YES, MB_NO, MB_OK, MB_CANCEL, MB_ABORT, MB_RETRY, MB_IGNORE,
var
  FButtons: Integer;
begin
  if Buttons = [mbOk] then FButtons := MB_OK
  else if Buttons = [mbOk, mbCancel] then FButtons := MB_OKCANCEL
  else if Buttons = [mbYes, mbNo] then FButtons := MB_YESNO
  else if Buttons = [mbYes, mbNo, mbCancel] then FButtons := MB_YESNOCANCEL
  else if Buttons = [mbAbort, mbRetry, mbIgnore] then FButtons := MB_ABORTRETRYIGNORE
  else if Buttons = [mbRetry, mbCancel] then FButtons := MB_RETRYCANCEL
  else FButtons := MB_OK;

  case Tnt_MessageBoxW(GetActiveWindow, PWideChar(Msg), PChar(FInstallInfo.Text(ssMessageBoxTitle)), MBFlags[DlgType] or FButtons) of
    IDOK: Result := mrOk;
    IDCANCEL: Result := mrCancel;
    IDYES: Result := mrYes;
    IDNO: Result := mrNo;
    IDABORT: Result := mrAbort;
    IDIGNORE: Result := mrIgnore;
    IDRETRY: Result := mrRetry;
    else Result := mrOk;
  end;
end;

procedure ShowMessageW(const Message: WideString);
begin
  if Assigned(FInstallInfo) then
    Tnt_MessageBoxW(GetActiveWindow, PWideChar(Message), PChar(FInstallInfo.Text(ssMessageBoxTitle)), MB_OK)
  else
    Tnt_MessageBoxW(GetActiveWindow, PWideChar(Message), PChar('Setup'), MB_OK);
end;

end.
