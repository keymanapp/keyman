(*
  Name:             UfrmVisualKeyboardImportKMX
  Copyright:        Copyright (C) SIL International.
  Documentation:
  Description:
  Create Date:      19 Mar 2007

  Modified Date:    27 Mar 2015
  Authors:          mcdurdin
  Related Files:
  Dependencies:

  Bugs:
  Todo:
  Notes:
  History:          19 Mar 2007 - mcdurdin - I683 - Fixed import of kmx to on screen keyboard
                    30 May 2007 - mcdurdin - I726 - Fixed 102 key not displaying when it should
                    10 Sep 2008 - mcdurdin - I1471 - Add key caps to all non-matched keys (option)
                    10 Jan 2014 - mcdurdin - I4022 - V9.0 - Rework Import KMX to work with V9.0 Debug Host Keyboard
                    19 Mar 2014 - mcdurdin - I4143 - V9.0 - Support modifier layers of KVK when importing a KMX
                    27 Mar 2015 - mcdurdin - I4156 - V9.0 - Debug host keyboard needs to map through forced keyboard's preserved keys
*)
unit UfrmVisualKeyboardImportKMX;   // I4022

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  ComCtrls, StdCtrls, kmxfile, VisualKeyboard, UfrmTike,
  keymanapi_TLB,
  DebugUtils;

const
  WM_User_FormShown = WM_USER+201;
  WM_User_SendNextKey = WM_USER+200;
  WM_User_ProcessKeyOutput = WM_USER+202;

type
  TfrmVisualKeyboardImportKMX = class(TTIKEForm)
    Label1: TLabel;
    lblStatus: TLabel;
    procedure FormShow(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
  private
    nkey: Integer;
    keys: TList;
    data: WideString;
    FVK: TVisualKeyboard;
    FIsKeyUp: Boolean;
    FFileName: WideString;
    FHasANSI, FHasUnicode, FUnicode: Boolean;
    FLeftRightCtrlAlt: Boolean;
    FShow102Key: Boolean;
    FDebugHostKeyboard: IKeymanKeyboardInstalled;
    FLastProfile: TDebugUtilProfile;

    procedure WMUserSendNextKey(var Message: TMessage); message WM_User_SendNextKey;
    procedure WMUserFormShown(var Message: TMessage); message WM_User_FormShown; //WM_USER+201;
    procedure WMUserProcessKeyOutput(var Message: TMessage); message WM_User_ProcessKeyOutput;
    procedure SendKey;
    function GetKeyboardKeys: Boolean;
    procedure AddKey;
    procedure ValidateLeftRightCtrlAlt;
    procedure ReadKeys(kfh: PKeyboardFileHeader; groupindex: Integer);
    procedure Validate102Key;
    procedure SetStatus(const msg: string);
  protected
    function GetHelpTopic: string; override;
    procedure WndProc(var Message: TMessage); override;
  public
    property FileName: WideString read FFileName write FFileName;
    property VK: TVisualKeyboard read FVK write FVK;
    property LeftRightCtrlAlt: Boolean read FLeftRightCtrlAlt;
    property Show102Key: Boolean read FShow102Key;
  end;

implementation

uses
  Keyman.Developer.System.HelpTopics,
  keyman32_int,
  KeymanDeveloperUtils,
  kmxfileconsts,
  debugging,
  VKeys;

{$R *.DFM}

type
  TVKKey = class
    vkey: Word;
    kmshift, shift: Integer;
  end;

var
  WM_KEYMANDEBUG_CANDEBUG,
  WM_KEYMANDEBUG_GETUNICODESTATUS,
  WM_KEYMANDEBUG_GETCONTEXT,
  WM_KEYMANDEBUG_ACTION,
  WM_KEYMANDEBUG_RULEMATCH: UINT;

{-------------------------------------------------------------------------------
 -                                                                             -
 ------------------------------------------------------------------------------}

procedure TfrmVisualKeyboardImportKMX.FormCreate(Sender: TObject);
begin
  inherited;
  keys := TList.Create;
  TDebugUtils.GetActiveTSFProfile(FLastProfile);
end;

procedure TfrmVisualKeyboardImportKMX.FormDestroy(Sender: TObject);
var
  i: Integer;
begin
  Keyman_StopForcingKeyboard;
  for i := 0 to keys.Count - 1 do TVKKey(keys[i]).Free;
  keys.Free;
  TDebugUtils.SetActiveTSFProfile(FLastProfile);
end;

procedure TfrmVisualKeyboardImportKMX.FormShow(Sender: TObject);
begin
  PostMessage(Handle, WM_User_FormShown, 0, 0);
end;

{-------------------------------------------------------------------------------
 - Prepare for key processing                                                  -
 ------------------------------------------------------------------------------}

procedure TfrmVisualKeyboardImportKMX.SetStatus(const msg: string);
begin
  lblStatus.Caption := msg;
  lblStatus.Update;
end;

procedure TfrmVisualKeyboardImportKMX.WMUserFormShown(var Message: TMessage);

  procedure DoFail(const msg: string);
  begin
    ShowMessage(msg);
    ModalResult := mrCancel;
  end;
var
  FWasStarted: Boolean;
  hr: HRESULT;
begin
  SetStatus('Loading keyboard data');
  if (Message.WParam = 0) and not GetKeyboardKeys then
  begin
    DoFail('Could not load keyboard for import.');
    Exit;
  end;

  SetStatus('Finding debug host keyboard');
  FDebugHostKeyboard := TDebugUtils.GetDebugHostKeyboard;
  if FDebugHostKeyboard = nil then
  begin
    DoFail('Unable to import keyboard layout -- the debug host keyboard is not installed.');
    Exit;
  end;

  SetStatus('Starting Keyman');
  if not StartKeymanDesktopPro(FWasStarted) then
  begin
    DoFail('Unable to start Keyman for debugging - please make sure that Keyman is correctly installed (the error code was '+IntToHex(GetLastError, 8)+').');  // I3173   // I3504
    Exit;
  end;

  if FWasStarted then
  begin
    // Give Keyman Engine a chance to correctly attach focus after it starts
    Windows.SetFocus(0);
    Windows.SetFocus(Handle);
    PostMessage(Handle, WM_User_FormShown, 1, 0);
    Exit;
  end;

  if GetForegroundWindow <> Handle then
  begin
    ShowMessage('Keyman Engine was reconfigured, please click OK to continue import.');
  end;

  SetStatus('Selecting keyboard');
  if Keyman_ForceKeyboard(FFileName) then
  begin
    nkey := 0;
    FUnicode := not FHasANSI;

    hr := TDebugUtils.SelectTSFProfileForKeyboardLanguage(FDebugHostKeyboard.Languages[0]);   // I3655   // I4156
    if FAILED(hr) then
    begin
      DoFail(Format('Unable to start debugging -- failed to switch to language with error %x', [hr]));
      Exit;
    end;

//    ShowMessage(Format('nkeys: %d', [keys.count]));
    PostMessage(Handle, WM_User_SendNextKey, 0, 0);
    Exit;
  end;

  ShowMessage('Could not load keyboard for import.');
  ModalResult := mrCancel;
end;

{-------------------------------------------------------------------------------
 - Send a keystroke for processing                                             -
 ------------------------------------------------------------------------------}

procedure TfrmVisualKeyboardImportKMX.WMUserSendNextKey(var Message: TMessage);
begin
  SendKey;
end;

procedure TfrmVisualKeyboardImportKMX.SendKey;
var
  vk: TVKKey;
  inputs: array[0..10] of TInput;
  n: Integer;
  shiftText: string;
  keyText: string;
    procedure AddInput(vk: WORD; isDown, isExtended: Boolean);   // I4143
    const
      DownFlag: array[Boolean] of DWORD = (KEYEVENTF_KEYUP, 0);
      ExtendedFlag: array[Boolean] of DWORD = (0, KEYEVENTF_EXTENDEDKEY);
    begin
      inputs[n].Itype := INPUT_KEYBOARD;
      inputs[n].ki.wVk := vk;
      inputs[n].ki.wScan := 0;
      inputs[n].ki.dwFlags := DownFlag[isDown] or ExtendedFlag[isExtended];
      inputs[n].ki.time := 0;
      inputs[n].ki.dwExtraInfo := 0;
      Inc(n);
    end;
begin
  if nkey >= keys.Count then
  begin
    if FUnicode or (keys.Count = 0) or (not FUnicode and not FHasUnicode) then
    begin
      ModalResult := mrOk;
      Exit;
    end
    else
    begin
      FUnicode := True;
      nkey := 0;
    end;
  end;

  data := '';
  vk := TVKKey(keys[nkey]);

//  if FUnicode
//    then lb.items.Add(Format('SendKey: nkey: %d vkey: %s shift: %d unicode', [nkey, VKeyNames[vk.vkey], vk.kmshift]))
//    else lb.items.Add(Format('SendKey: nkey: %d vkey: %s shift: %d ansi', [nkey, VKeyNames[vk.vkey], vk.kmshift]));
//  ShowMessage(Format('Sending key: %s / %d', [VKeyNames[vk.VKey], vk.shift]));

  n := GetVKLegalShiftStateIndex(vk.shift);
  if n < 0
    then shiftText := Format('%x', [vk.shift])
    else shiftText := VKLegalShiftStates[n].Desc;

  if vk.vkey < 256
    then keyText := VKeyNames[vk.vkey]
    else keyText := Format('%x', [vk.vkey]);

  SetStatus(Format('Importing %s %s', [shiftText, keyText]));

  n := 0;

  if (vk.kmshift and KMX_SHIFTFLAG) = KMX_SHIFTFLAG then AddInput(VK_SHIFT, True, False);   // I4143
  if (vk.kmshift and KMX_RALTFLAG) = KMX_RALTFLAG then AddInput(VK_MENU, True, True);
  if (vk.kmshift and KMX_LALTFLAG) = KMX_LALTFLAG then AddInput(VK_MENU, True, False);   // I4156
  if (vk.kmshift and KMX_RCTRLFLAG) = KMX_RCTRLFLAG then AddInput(VK_CONTROL, True, True);
  if (vk.kmshift and KMX_LCTRLFLAG) = KMX_LCTRLFLAG then AddInput(VK_CONTROL, True, False);   // I4156

  if (vk.kmshift and KMX_CTRLFLAG) = KMX_CTRLFLAG then AddInput(VK_CONTROL, True, False);
  if (vk.kmshift and KMX_ALTFLAG) = KMX_ALTFLAG then AddInput(VK_MENU, True, False);   // I4156

  AddInput(vk.vkey, True, False);
  AddInput(vk.vkey, False, False);

  if (vk.kmshift and KMX_ALTFLAG) = KMX_ALTFLAG then AddInput(VK_MENU, False, False);   // I4156
  if (vk.kmshift and KMX_CTRLFLAG) = KMX_CTRLFLAG then AddInput(VK_CONTROL, False, False);

  if (vk.kmshift and KMX_LCTRLFLAG) = KMX_LCTRLFLAG then AddInput(VK_CONTROL, False, False);   // I4156
  if (vk.kmshift and KMX_RCTRLFLAG) = KMX_RCTRLFLAG then AddInput(VK_CONTROL, False, True);
  if (vk.kmshift and KMX_LALTFLAG) = KMX_LALTFLAG then AddInput(VK_MENU, False, False);   // I4156
  if (vk.kmshift and KMX_RALTFLAG) = KMX_RALTFLAG then AddInput(VK_MENU, False, True);
  if (vk.kmshift and KMX_SHIFTFLAG) = KMX_SHIFTFLAG then AddInput(VK_SHIFT, False, False);

  if SendInput(n, inputs[0], sizeof(TInput)) = 0 then   // I4143
    RaiseLastOSError;

//  lb.items.Add('SendKey: EXIT');
end;

{-------------------------------------------------------------------------------
 - Process the results of the keystroke                                        -
 ------------------------------------------------------------------------------}

procedure TfrmVisualKeyboardImportKMX.WndProc(var Message: TMessage);
begin
  if Message.Msg = WM_KEYMANDEBUG_CANDEBUG then
  begin
    //AddDebug('WM_KEYMANDEBUG_CANDEBUG');
    Message.Result := 1
  end
  else if Message.Msg = WM_KEYMANDEBUG_GETUNICODESTATUS then
  begin
//    AddDebug('WM_KEYMANDEBUG_GETUNICODESTATUS');
    if not FUnicode then Message.Result := 0 else Message.Result := 1;
  end
  else if Message.Msg = WM_KEYMANDEBUG_GETCONTEXT then
  begin
  //  AddDebug('WM_KEYMANDEBUG_GETCONTEXT');
    PWideChar(Message.lParam)^ := #0;
    Message.Result := 1;
  end
  else if Message.Msg = WM_KEYMANDEBUG_ACTION then
  begin
//    AddDebug('WM_KEYMANDEBUG_ACTION');
//    ShowMessage('data received: '+IntToStr(Message.wParam));
    if Message.wParam = QIT_CHAR then
    begin
//      lb.items.Add('QIT_CHAR');
      data := data + WChar(Message.lParam);
    end;
  end
  else if Message.Msg = WM_KEYMANDEBUG_RULEMATCH then
  begin
//    AddDebug('WM_KEYMANDEBUG_RULEMATCH');
    if (Message.wParam = QID_BEGIN_UNICODE) or (Message.wParam = QID_BEGIN_ANSI) then
      FIsKeyUp := PAIDebugKeyInfo(PAIDebugInfo(Message.lParam).Flags).IsUp
    else if (Message.wParam = QID_END) then //and FIsKeyUp then
    begin
//      lb.items.Add('QID_END');
      PostMessage(Handle, WM_User_ProcessKeyOutput, 0, 0);
    end;
  end
  else
    inherited;
end;

procedure TfrmVisualKeyboardImportKMX.WMUserProcessKeyOutput(var Message: TMessage);
begin
  AddKey;
end;

procedure TfrmVisualKeyboardImportKMX.AddKey;
var
  k: TVisualKeyboardKey;
  vk: TVKKey;
  n: Integer;
begin
//  lb.items.Add('AddKey: ENTER');

  if nkey >= keys.count then
  begin
//    lb.items.Add('AddKey: EXIT: bug!');
    exit;
  end;

//  lb.items.Add(Format('AddKey: nkey: %d data: "%s" vk: %s ', [nkey, data, VKeyNames[TVKKey(keys[nkey]).vkey]]));
  if (data <> '') then //and (nkey < keys.Count) then
  begin
    vk := TVKKey(keys[nkey]);
//    ShowMessage(Format('key: %d shift: %d text: "%s"', [vk.vkey, vk.shift, data]));
    n := FVK.Keys.IndexOf(vk.vkey, vk.shift);
    if n < 0
      then k := TVisualKeyboardKey.Create
      else k := FVK.Keys[n];
    if FUnicode then k.Flags := [kvkkUnicode] else k.Flags := [];
    k.VKey := vk.vkey;
    k.Shift := vk.shift;
    k.Text := data;
    if n < 0 then FVK.Keys.Add(k);
  end;

//  data := '';
  Inc(nkey);

  PostMessage(Handle, WM_User_SendNextKey, 0, 0);   // I4156
//  lb.items.Add('AddKey: Exit (nkey now='+IntToStr(nkey));
end;

{-------------------------------------------------------------------------------
 - Read a list of keys from the keyboard                                       -
 ------------------------------------------------------------------------------}

function KMXShiftToVKShift(shift: DWord): Integer;
begin
  Result := 0;
  if (shift and KMX_LCTRLFLAG) <> 0 then Result := Result or KVKS_LCTRL;
  if (shift and KMX_RCTRLFLAG) <> 0 then Result := Result or KVKS_RCTRL;
  if (shift and KMX_LALTFLAG) <> 0  then Result := Result or KVKS_LALT;
  if (shift and KMX_RALTFLAG) <> 0  then Result := Result or KVKS_RALT;
  if (shift and KMX_SHIFTFLAG) <> 0 then Result := Result or KVKS_SHIFT;
  if (shift and KMX_CTRLFLAG) <> 0  then Result := Result or KVKS_CTRL;
  if (shift and KMX_ALTFLAG) <> 0   then Result := Result or KVKS_ALT;
end;

function KMXShiftToActiveShift(shift: DWord): DWord;
begin
  Result := 0;
  if (shift and KMX_LCTRLFLAG) <> 0 then Result := Result or KMX_LCTRLFLAG;
  if (shift and KMX_RCTRLFLAG) <> 0 then Result := Result or KMX_RCTRLFLAG;
  if (shift and KMX_LALTFLAG) <> 0  then Result := Result or KMX_LALTFLAG;
  if (shift and KMX_RALTFLAG) <> 0  then Result := Result or KMX_RALTFLAG;
  if (shift and KMX_SHIFTFLAG) <> 0 then Result := Result or KMX_SHIFTFLAG;
  if (shift and KMX_CTRLFLAG) <> 0  then Result := Result or KMX_LCTRLFLAG;
  if (shift and KMX_ALTFLAG) <> 0   then Result := Result or KMX_LALTFLAG;
end;

function CharToVKey(ch: Word; var ShiftState, KMShiftState: Integer): Word;
var
  w: Word;
begin
  if ch > 127 then ch := 127;
  w := VkKeyScan(Char(ch));
  Result := LOBYTE(w);
  w := HIBYTE(w);
  ShiftState := 0;
  if (w and 1) = 1 then ShiftState := ShiftState or KVKS_SHIFT;
  if (w and 6) = 6 then ShiftState := ShiftState or KVKS_RALT
  else
  begin
    if (w and 2) = 2 then ShiftState := ShiftState or KVKS_CTRL;
    if (w and 4) = 4 then ShiftState := ShiftState or KVKS_ALT;
  end;

  KMShiftState := 0;
  if (w and 1) = 1 then KMShiftState := KMShiftState or KMX_SHIFTFLAG;
  if (w and 6) = 6 then KMShiftState := KMShiftState or KMX_RALTFLAG
  else
  begin
    if (w and 2) = 2 then KMShiftState := KMShiftState or KMX_CTRLFLAG;
    if (w and 4) = 4 then KMShiftState := KMShiftState or KMX_ALTFLAG;
  end;
end;

procedure TfrmVisualKeyboardImportKMX.ReadKeys(kfh: PKeyboardFileHeader; groupindex: Integer);
var
  gp: PKeyboardFileGroup;
  kp: PKeyboardFileKey;
  vkey, kmshift, shift, i, j: Integer;
  vk: TVKKey;
  Found: Boolean;
begin
  gp := PKeyboardFileGroup(DWord(kfh)+kfh.dpGroupArray+DWord(groupindex)*sizeof(TKeyboardFileGroup));
  if not gp.fUsingKeys then Exit;

  kp := PKeyboardFileKey(DWord(kfh)+gp.dpKeyArray);
  for i := 0 to Integer(gp.cxKeyArray) - 1 do
  begin
    Found := False;
    if (kp.ShiftFlags and KMX_ISVIRTUALKEY) = 0 then
    begin
      vkey := CharToVKey(kp.Key, shift, kmshift);
    end
    else
    begin
      vkey := kp.Key;
      shift := KMXShiftToVKShift(kp.ShiftFlags);
      kmshift := KMXShiftToActiveShift(kp.ShiftFlags);
    end;

    if vkey > 255 then
    begin
      // We don't try and import keys that are T_ touch virtual keys
      Inc(kp);
      Continue;
    end;

    for j := 0 to keys.Count - 1 do
      if (TVKKey(keys[j]).VKey = vkey) and (TVKKey(keys[j]).Shift = shift) then
      begin
        Found := True;
        Break;
      end;
    if Found then
    begin
      Inc(kp);
      Continue;
    end;
    vk := TVKKey.Create;
    vk.VKey := vkey;
    vk.Shift := shift;
    vk.kmshift := kmshift;
    keys.Add(vk);

    Inc(kp);
  end;

end;

function TfrmVisualKeyboardImportKMX.GetHelpTopic: string;
begin
  Result := SHelpTopic_Context_VisualKeyboardImportKMX;
end;

function TfrmVisualKeyboardImportKMX.GetKeyboardKeys: Boolean;
var
  ki: TKeyboardInfo;
  i: Integer;
  kfh: PKeyboardFileHeader;
begin
  Result := False;
  try
    GetKeyboardInfo(FFileName, True, ki);
  except
    Exit;
  end;

  try
    kfh := PKeyboardFileHeader(ki.MemoryDump.Memory);

    FHasANSI := kfh.StartGroupANSI >= 0;
    FHasUnicode := kfh.StartGroupUnicode >= 0;

    for i := 0 to kfh.cxGroupArray - 1 do
      ReadKeys(kfh, i);
  finally
    ki.MemoryDump.Free;
  end;

  ValidateLeftRightCtrlAlt;
  Validate102Key;

  Result := True;
end;

procedure TfrmVisualKeyboardImportKMX.Validate102Key;
var
  i: Integer;
begin
  FShow102Key := False;

  for i := 0 to keys.Count - 1 do
    if TVKKey(keys[i]).vkey = $E2 then
    begin
      FShow102Key := True;
      Exit;
    end;
end;

procedure TfrmVisualKeyboardImportKMX.ValidateLeftRightCtrlAlt;
var
  i: Integer;
  FLR: Boolean;
  vk: TVKKey;
begin
  FLR := False;
  for i := 0 to keys.Count - 1 do
  begin
    vk := TVKKey(keys[i]);
    if (vk.shift and (KVKS_LALT or KVKS_RALT or KVKS_LCTRL or KVKS_RCTRL)) <> 0 then
    begin
      FLR := True;
      Break;
    end;
  end;

  if FLR then
    for i := 0 to keys.Count - 1 do
    begin
      vk := TVKKey(keys[i]);
      if (vk.shift and KVKS_ALT) <> 0 then
        vk.shift := (vk.shift and (not KVKS_ALT)) or KVKS_LALT;
      if (vk.shift and KVKS_CTRL) <> 0 then
        vk.shift := (vk.shift and (not KVKS_CTRL)) or KVKS_LCTRL;
    end;

  FLeftRightCtrlAlt := FLR;
end;

initialization
	WM_KEYMANDEBUG_CANDEBUG         := RegisterWindowMessage('WM_KEYMANDEBUG_CANDEBUG');
	WM_KEYMANDEBUG_GETUNICODESTATUS := RegisterWindowMessage('WM_KEYMANDEBUG_GETUNICODESTATUS');
	WM_KEYMANDEBUG_GETCONTEXT       := RegisterWindowMessage('WM_KEYMANDEBUG_GETCONTEXT');
	WM_KEYMANDEBUG_ACTION           := RegisterWindowMessage('WM_KEYMANDEBUG_ACTION');
	WM_KEYMANDEBUG_RULEMATCH        := RegisterWindowMessage('WM_KEYMANDEBUG_RULEMATCH');
end.

