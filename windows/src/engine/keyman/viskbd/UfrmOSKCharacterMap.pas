(*
  Name:             UfrmOSKCharacterMap
  Copyright:        Copyright (C) SIL International.
  Documentation:    
  Description:      
  Create Date:      14 Sep 2006

  Modified Date:    25 Sep 2014
  Authors:          mcdurdin
  Related Files:    
  Dependencies:     

  Bugs:             
  Todo:             
  Notes:            
  History:          14 Sep 2006 - mcdurdin - Initial version
                    04 Dec 2006 - mcdurdin - Support inserting characters/codes/names
                    04 Dec 2006 - mcdurdin - Handle stay-on-top with child dialogs
                    12 Dec 2006 - mcdurdin - Add product referencing for localized strings
                    14 Jun 2008 - mcdurdin - I1463 - Cancel focus on double click
                    16 Jan 2009 - mcdurdin - I1512 - Focus back to original window before stuffing in characters
                    03 May 2011 - mcdurdin - I2890 - Record diagnostic data when encountering registry errors
                    18 May 2012 - mcdurdin - I3306 - V9.0 - Remove TntControls + Win9x support
                    25 Sep 2014 - mcdurdin - I4411 - V9.0 - Character map allows Ctrl+Click to insert character
                    25 Sep 2014 - mcdurdin - I4412 - V9.0 - Character Map needs to insert characters using SendInput
                    
*)
unit UfrmOSKCharacterMap;  // I3306

interface

uses
  System.UITypes,
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, UfrmOSKPlugInBase, UfrmCharacterMapNew, CharMapInsertMode, CharacterDragObject,
  ComCtrls, UnicodeData, keymanapi_TLB,
  UfrmUnicodeDataStatus, StdCtrls;

type
  TfrmOSKCharacterMap = class(TfrmOSKPlugInBase, IUnicodeDataUIManager)
    lblDesktopLight: TLabel;
    lblLightLink: TLabel;
    procedure FormCreate(Sender: TObject);
    procedure FormResize(Sender: TObject);
  private
    FUnicodeDataStatus: TfrmUnicodeDataStatus;
    procedure CharMapCancelFocus(Sender: TObject);
    procedure CharMapCanInsertCode(Sender: TObject; Control: TWinControl;
      var Result: Boolean);
    procedure CharMapInsertCode(Sender: TObject; Control: TWinControl;
      DragObject: TCharacterDragObject);
    { Private declarations }
    procedure CharMapFilterEntered(Sender: TObject);
    procedure CharMapFilterExited(Sender: TObject);
    procedure DoRestoreFocus;
    procedure CharMapDialogOpening(Sender: TObject);
    procedure CharMapDialogClosing(Sender: TObject);

    procedure WMMouseActivate(var Message: TWMMouseActivate); message WM_MOUSEACTIVATE;
  public
    procedure UDUI_Error(Sender: TUnicodeData; Error: TUnicodeDataError; const Details: WideString);
    function UDUI_ShouldStartRebuildOnError(const Msg: WideString): Boolean;
    function UDUI_StartRebuild(ACallback: TNotifyEvent; AskFirst: Boolean): Boolean;
    procedure UDUI_UpdateStatus(const Msg: WideString; Pos: Integer; Max: Integer);
    { Public declarations }

    procedure Deactivating; override;

    class function GetUnicodeSourceRootPath: string;
    class function IsCharMapAvailable: Boolean;
    class function IsCharMapDataInstalled: Boolean;
  end;

implementation

uses
  custinterfaces,
  kmint,
  MessageIdentifierConsts,
  MessageIdentifiers,
  ErrorControlledRegistry,
  RegistryKeys,
  USendInputString,
  UfrmVisualKeyboard;

{$R *.dfm}

class function TfrmOSKCharacterMap.IsCharMapAvailable: Boolean;
begin
  Result := True; // TODO REFACTOR
end;

class function TfrmOSKCharacterMap.IsCharMapDataInstalled: Boolean;
begin
  Result :=
    FileExists(GetUnicodeSourceRootPath + UnicodeDataMdbName) or
    FileExists(GetUnicodeSourceRootPath + UnicodeDataTxtName);
end;

class function TfrmOSKCharacterMap.GetUnicodeSourceRootPath: string;
begin
  Result := '';

  with TRegistryErrorControlled.Create do  // I2890
  try
    RootKey := HKEY_LOCAL_MACHINE;
    if OpenKeyReadOnly(SRegKey_KeymanDesktop_CU) and ValueExists(SRegValue_CharMapSourceData)
      then Result := IncludeTrailingPathDelimiter(ReadString(SRegValue_CharMapSourceData));
  finally
    Free;
  end;
end;

procedure TfrmOSKCharacterMap.UDUI_Error(Sender: TUnicodeData;
  Error: TUnicodeDataError; const Details: WideString);
begin
  case Error of
    udeCouldNotDeleteDatabaseForRebuild:
      ShowMessage(MsgFromIdFormat(SKUnicodeData_DatabaseCouldNotBeDeleted, [Details]));
    udeCouldNotCreateDatabase:
      ShowMessage(MsgFromIdFormat(SKUnicodeData_CouldNotCreateDatabase, [Details]));
  end;

end;

function TfrmOSKCharacterMap.UDUI_ShouldStartRebuildOnError(const Msg: WideString): Boolean;
begin
  Result := MessageDlg(MsgFromIdFormat(SKUnicodeData_DatabaseLoadFailedRebuild, [Msg]),
    mtConfirmation, [mbYes, mbNo], 0) = mrYes;
end;

function TfrmOSKCharacterMap.UDUI_StartRebuild(ACallback: TNotifyEvent;
  AskFirst: Boolean): Boolean;
begin
  Result := False;
  if AskFirst and (MessageDlg(MsgFromId(SKUnicodeData_Build),
      mtConfirmation, [mbYes, mbNo], 0) = mrNo) then
    Exit;

  FUnicodeDataStatus := TfrmUnicodeDataStatus.Create(Self);
  try
    FUnicodeDataStatus.Callback := ACallback;
    Result := FUnicodeDataStatus.ShowModal = mrOk;
  finally
    FreeAndNil(FUnicodeDataStatus);
  end;
end;

procedure TfrmOSKCharacterMap.UDUI_UpdateStatus(const Msg: WideString; Pos,
  Max: Integer);
begin
  if Assigned(FUnicodeDataStatus) then
    FUnicodeDataStatus.UpdateStatus(msg, pos, max);
end;

procedure TfrmOSKCharacterMap.WMMouseActivate(var Message: TWMMouseActivate);
var
  pt: TPoint;
  h: Cardinal;
begin
  if not Assigned(frmCharacterMapNew) then
    inherited
  else
  begin
    GetCursorPos(pt);
    h := WindowFromPoint(pt);
    if ((h = frmCharacterMapNew.editFilter.Handle) or (h = frmCharacterMapNew.editCharName.Handle))
      then Message.Result := MA_ACTIVATE
      else inherited;
  end;
end;

procedure TfrmOSKCharacterMap.FormCreate(Sender: TObject);
var
  s: WideString;
begin
  inherited;
  s := GetUnicodeSourceRootPath;
  CreateUnicodeData(s, Self, s);

  frmCharacterMapNew := TfrmCharacterMapNew.Create(Self, SRegKey_KeymanOSK_CharMap_CU);
  frmCharacterMapNew.BorderStyle := bsNone;
  frmCharacterMapNew.Align := alClient;
  frmCharacterMapNew.Parent := Self;
  frmCharacterMapNew.Visible := True;
  frmCharacterMapNew.InsertMode := cmimCharacter;
  frmCharacterMapNew.IgnoreLastActiveControl := True;
  frmCharacterMapNew.CharMapDrag := False;
  frmCharacterMapNew.CtrlClickIsDblClick := True;   // I4411
  frmCharacterMapNew.ShouldCancelFocusOnDblClick := True;
  frmCharacterMapNew.OnCancelFocus := CharMapCancelFocus;
  frmCharacterMapNew.OnInsertCode := CharMapInsertCode;
  frmCharacterMapNew.OnCanInsertCode := CharMapCanInsertCode;
  frmCharacterMapNew.OnFilterEntered := CharMapFilterEntered;
  frmCharacterMapNew.OnFilterExited := CharMapFilterExited;
  frmCharacterMapNew.OnDialogOpening := CharMapDialogOpening;
  frmCharacterMapNew.OnDialogClosing := CharMapDialogClosing;
end;

procedure TfrmOSKCharacterMap.FormResize(Sender: TObject);
begin
  inherited;
  if Assigned(frmCharacterMapNew) then
    frmCharacterMapNew.CharNameNextToFilter := Width > 250;

  lblDesktopLight.SetBounds(
    (ClientWidth - lblDesktopLight.Width) div 2,
    (ClientHeight - lblDesktopLight.Height - lblLightLink.Height) div 2,
    lblDesktopLight.Width,
    lblDesktopLight.Height);

  lblLightLink.SetBounds(
    (ClientWidth - lblLightLink.Width) div 2,
    (ClientHeight + lblLightLink.Height) div 2,
    lblLightLink.Width,
    lblLightLink.Height);
end;

procedure TfrmOSKCharacterMap.CharMapInsertCode(Sender: TObject; Control: TWinControl; DragObject: TCharacterDragObject);
var
  ch: WideString;
  hwnd: THandle;
begin
  ch := DragObject.Text[cmimText];
  if ch = '' then
    Exit;

  hwnd := kmcom.Control.LastFocusWindow;
  SendInputString(hwnd, ch);   // I4412


end;

procedure TfrmOSKCharacterMap.CharMapCanInsertCode(Sender: TObject; Control: TWinControl; var Result: Boolean);
begin
  Result := True;
end;

procedure TfrmOSKCharacterMap.CharMapDialogOpening(Sender: TObject);
var
  hwnd: THandle;
  frm: TCustomForm;
begin
  frm := GetParentForm(Self);
  SetWindowPos(frm.Handle, HWND_NOTOPMOST, 0, 0, 0, 0, SWP_NOMOVE or SWP_NOSIZE);

  hwnd := kmcom.Control.LastFocusWindow;

  AttachThreadInput(GetCurrentThreadId, GetWindowThreadProcessId(hwnd, nil), TRUE);

  SetForegroundWindow(frm.Handle);
  SetActiveWindow(frm.Handle);

  AttachThreadInput(GetCurrentThreadId, GetwindowThreadProcessId(hwnd, nil), FALSE);
end;

procedure TfrmOSKCharacterMap.CharMapDialogClosing(Sender: TObject);
var
  frm: TCustomForm;
begin
  frm := GetParentForm(Self);
  SetWindowPos(frm.Handle, HWND_TOPMOST, 0, 0, 0, 0, SWP_NOMOVE or SWP_NOSIZE or SWP_NOACTIVATE);

  if GetFocus = 0 then
    DoRestoreFocus;
end;

procedure TfrmOSKCharacterMap.CharMapFilterEntered(Sender: TObject);
var
  hwnd: THandle;
  frm: TCustomForm;
begin
  frm := GetParentForm(Self);
  hwnd := kmcom.Control.LastFocusWindow;

  AttachThreadInput(GetWindowThreadProcessId(hwnd, nil), GetCurrentThreadId, TRUE);

  SetForegroundWindow(frm.Handle);
  SetActiveWindow(frm.Handle);

  AttachThreadInput(GetwindowThreadProcessId(hwnd, nil), GetCurrentThreadId, FALSE);

  if Assigned(frmCharacterMapNew) then
    frmCharacterMapNew.editFilter.SetFocus;
  Windows.SetFocus(frmCharacterMapNew.editFilter.Handle);
end;

procedure TfrmOSKCharacterMap.CharMapFilterExited(Sender: TObject);
begin
end;

procedure TfrmOSKCharacterMap.CharMapCancelFocus(Sender: TObject);
var
  frm: TCustomForm;
begin
  frm := GetParentForm(Self);
  SetWindowPos(frm.Handle, HWND_TOPMOST, 0, 0, 0, 0, SWP_NOMOVE or SWP_NOSIZE or SWP_NOACTIVATE);

  DoRestoreFocus;
end;

procedure TfrmOSKCharacterMap.Deactivating;
var
  frm: TCustomForm;
begin
  frm := GetParentForm(Self);
  SetWindowPos(frm.Handle, HWND_TOPMOST, 0, 0, 0, 0, SWP_NOMOVE or SWP_NOSIZE or SWP_NOACTIVATE);
end;

procedure TfrmOSKCharacterMap.DoRestoreFocus;
var
  hwnd: THandle;
begin
  hwnd := kmcom.Control.LastFocusWindow;

  AttachThreadInput(GetCurrentThreadId, GetWindowThreadProcessId(hwnd, nil), TRUE);
  Windows.SetForegroundWindow(hwnd);
  Windows.SetFocus(hwnd);
  AttachThreadInput(GetCurrentThreadId, GetwindowThreadProcessId(hwnd, nil), FALSE);
end;

end.
