(*
  Name:             UfrmOSKKeyboardUsage
  Copyright:        Copyright (C) SIL International.
  Documentation:
  Description:
  Create Date:      27 Mar 2008

  Modified Date:    11 Feb 2015
  Authors:          mcdurdin
  Related Files:
  Dependencies:

  Bugs:
  Todo:
  Notes:
  History:          27 Mar 2008 - mcdurdin - I1373 - Initial version
                    14 Jun 2008 - mcdurdin - I1429 - Use U+XXXX when inserting characters from keyboard usage (so we avoid Unicode URLs for compat)
                    29 Mar 2010 - mcdurdin - I2199 - Shift+click
                    17 Dec 2010 - mcdurdin - I2570 - Upgrade EmbeddedWB (also I2393)
                    18 Feb 2011 - mcdurdin - I2721 - Override Javascript-disabled security for web controls
                    18 May 2012 - mcdurdin - I3306 - V9.0 - Remove TntControls + Win9x support
                    08 Jun 2012 - mcdurdin - I3349 - V9.0 - Consolidate all process creation into TUtilExecute
                    24 Jan 2012 - mcdurdin - I3214 - Support InsertKeys functionality
                    03 Nov 2012 - mcdurdin - I3521 - V9.0 - Merge of I3214 - Support InsertKeys functionality
                    01 May 2014 - mcdurdin - I4181 - V9.0 - Stop using DeleteFileAlways, MOVEFILE_DELAY_UNTIL_REBOOT
                    16 Jun 2014 - mcdurdin - I4268 - If Keyboard usage refreshes during exit, Keyman crashes [CrashID:keyman.exe_9.0.452.0_2C5FB0CD_EAccessViolation]
                    11 Feb 2015 - mcdurdin - I4593 - V9.0 - The keyboard usage page can appear outside the OSK in some situations
*)
unit UfrmOSKKeyboardUsage;  // I3306

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, UfrmOSKPlugInBase, Keyman.UI.UframeCEFHost, ExtShiftState,
  keymanapi_TLB, xmlrenderer, UfrmKeymanBase, UserMessages,
  TempFileManager;

type
  TKeyboardProps = record
    KeyboardName: WideString;
    UsageFileName: WideString;
    HasOSK: Boolean;
    HasWelcome: Boolean;
  end;

  TfrmOSKKeyboardUsage = class(TfrmOSKPlugInBase)  // I2721
    procedure FormDestroy(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    {$MESSAGE HINT 'TODO: Refactor to use TWebBrowserManager'}
    cef: TframeCEFHost;
    FLastActiveKeymanID: Integer;
    FXMLFileName: TTempFile;   // I4181
    FXMLRenderers: TXMLRenderers;
    FDialogName: WideString;
    FXML: WideString;
    wm_keyman_char: UINT;
    FInput: array of TInput;  // I3214   // I3521

    function GetKeyboardProps(APackage: IKeymanPackage; AKeyboard: IKeymanKeyboardInstalled): TKeyboardProps;
    procedure Content_Render;
    procedure WMUser_ContentRender(var Message: TMessage); message WM_USER_ContentRender;
    procedure FireCommand(const command: WideString; params: TStringList);
    procedure SendKeys(keys: WideString);  // I3214   // I3521
    function GetTag(var s: WideString): WideString;  // I3214   // I3521
    function ParseKey(var s: WideString; var key: Integer;  // I3214   // I3521
      var shiftstate: TExtShiftState; var commandtype: Integer): Boolean;
    function GetAsyncShiftState: TExtShiftState;  // I3214   // I3521
    procedure do_keybd_event(bVk, bScan: Byte; dwFlags, dwExtraInfo: DWORD);  // I3214   // I3521
    procedure SendChars(const chars: string);
    procedure cefBeforeBrowse(Sender: TObject; const Url, command: string;
      params: TStringList; wasHandled: Boolean);
    procedure cefLoadEnd(Sender: TObject);
    procedure cefPreKeySyncEvent(Sender: TObject; e: TCEFHostKeyEventData; out isShortcut, Handled: Boolean);
    procedure cefKeyEvent(Sender: TObject; e: TCEFHostKeyEventData; wasShortcut, wasHandled: Boolean);
    { Private declarations }
  public
    { Public declarations }
    procedure SelectKeyboard(KeymanID: Integer);
    function HasKeyboardUsage(KeymanID: Integer): Boolean;
  end;

implementation

uses
  KLog,
  kmint,
  custinterfaces,
  StockFileNames,
  uCEFTypes,
  uCEFConstants,
  UfrmKeyman7Main,
  UfrmVisualKeyboard,
  Unicode,
  USendInputString,
  utilexecute,
  utilhttp,
  utilsystem,
  utilxml,
  vkeys,
  WideStringClass,
  ComCtrls;

{$R *.dfm}

{ TfrmOSKWelcome }

function TfrmOSKKeyboardUsage.GetKeyboardProps(APackage: IKeymanPackage; AKeyboard: IKeymanKeyboardInstalled): TKeyboardProps;
var
  FUsageFile: IKeymanPackageContentFile;
begin
  FUsageFile := APackage.UsageFile;
  if Assigned(FUsageFile)
    then Result.UsageFilename := FUsageFile.FullFilename
    else Result.UsageFilename := '';

  Result.KeyboardName := AKeyboard.Name;
  Result.HasOSK := AKeyboard.VisualKeyboard <> nil;
  Result.HasWelcome := APackage.WelcomeFile <> nil;
end;


function TfrmOSKKeyboardUsage.HasKeyboardUsage(KeymanID: Integer): Boolean;
var
  i: Integer;
  kbds: IKeymanKeyboardsInstalled;
  pkg: IKeymanPackageInstalled;
begin
  Result := False;

  if KeymanID = -1 then
    Exit;

  kbds := kmcom.Keyboards;
  for i := 0 to kbds.Count - 1 do
  begin
    if kbds[i].KeymanID = KeymanID then
    begin
      pkg := kbds[i].OwnerPackage;
      if pkg <> nil then
      begin
        // find if package has welcome
        Result := GetKeyboardProps(pkg, kbds[i]).UsageFileName <> '';
        pkg := nil;
        Exit;
      end;
    end;
  end;

end;

procedure TfrmOSKKeyboardUsage.SelectKeyboard(KeymanID: Integer);
var
  FFileName_: WideString;
  i: Integer;
  kbds: IKeymanKeyboardsInstalled;
  pkg: IKeymanPackageInstalled;
  FProps: TKeyboardProps;
begin
  FXML := frmKeyman7Main.frmVisualKeyboard.GetToolbarPositionXML;

  if FLastActiveKeymanID = KeymanID then Exit;
  FLastActiveKeymanID := KeymanID;

  if KeymanID = -1 then
  begin
    // show the default welcome - "Keyman is running - choose a keyboard to get started"
    FXMLRenderers := TXMLRenderers.Create;
    FXMLRenderers.RenderTemplate := 'Usage_Off.xsl';
    FXML := FXML + '<Keyboards>';
    with kmcom.Keyboards do
    begin
      for i := 0 to Count - 1 do
      begin
        if not Items[i].Loaded then Continue;

        FXML := FXML + '<Keyboard KeymanID="'+IntToStr(Items[i].KeymanID)+'" ID="'+XMLEncode(Items[i].ID)+'" '+
          'Name="'+XMLEncode(Items[i].Name)+'" />';
      end;
    end;
    FXML := FXML + '</Keyboards>';
    Content_Render;
    FXMLRenderers.Free;

    if not Assigned(FXMLFileName) then
      Exit;   // I4268
    FFileName_ := FXMLFileName.Name;   // I4181
  end
  else
  begin
    FFileName_ := '';

    kbds := kmcom.Keyboards;
    for i := 0 to kbds.Count - 1 do
    begin
      if kbds[i].KeymanID = KeymanID then
      begin
        pkg := kbds[i].OwnerPackage;
        if pkg <> nil then
        begin
          // find if package has welcome
          FProps := GetKeyboardProps(pkg, kbds[i]);
          FFileName_ := FProps.UsageFileName;
          FXML := '<KeyboardName>'+XMLEncode(FProps.KeyboardName)+'</KeyboardName>';
          if FProps.HasOSK then FXML := FXML + '<HasOSK />';
          if FProps.HasWelcome then FXML := FXML + '<HasWelcome />';
          pkg := nil;
        end
        else
        begin
          FFileName_ := '';
          FXML := '<KeyboardName>'+XMLEncode(kbds[i].Name)+'</KeyboardName>';
        end;
        Break;
      end;
    end;

    if FFileName_ = '' then
    begin
      FXMLRenderers := TXMLRenderers.Create;
      FXMLRenderers.RenderTemplate := 'Usage_Keyboard.xsl';
      Content_Render;
      FXMLRenderers.Free;

      if not Assigned(FXMLFileName) then
        Exit;   // I4268
      FFileName_ := FXMLFileName.Name;   // I4181
    end;
  end;

  // show the "no welcome" welcome
  if (FFileName_ <> '') and FileExists(FFileName_) then
    cef.Navigate(FFileName_);
end;


procedure TfrmOSKKeyboardUsage.do_keybd_event(bVk: Byte; bScan: Byte; dwFlags, dwExtraInfo: DWORD);  // I3214   // I3521
begin
  KL.Log('kbdKeyPressed: keybd_event vk=%x scan=%x flags=%x', [bVk, bScan, dwFlags]);
  SetLength(FInput, Length(FInput)+1);
  with FInput[High(FInput)] do
  begin
    Itype := INPUT_KEYBOARD;
    ki.wVk := bVk;
    ki.wScan := $FF;
    ki.dwFlags := dwFlags;
    ki.dwExtraInfo := dwExtraInfo;
  end;
  //keybd_event(bVk, bScan, dwFlags, dwExtraInfo);
end;

function TfrmOSKKeyboardUsage.GetTag(var s: WideString): WideString;  // I3214   // I3521
var
  n: Integer;
  ch: WideChar;
    function IsWhiteSpace: Boolean;
    begin
      Result := CharInSet(s[n], [#1..#32]);
    end;
    function Finished: Boolean;
    begin
      Result := n > Length(s);
    end;
    function IsPunctuation: Boolean;
    begin
      Result := CharInSet(s[n], ['(',')','[',']','>']);
    end;
    function IsQuote: Boolean;
    begin
      Result := CharInSet(s[n], ['''','"']);
    end;
    function IsNumber: Boolean;
    begin
      Result := CharInSet(s[n], ['0'..'9']);
    end;
begin
  Result := '';
  n := 1;
  while not Finished and IsWhiteSpace do Inc(n);
  if Finished then Exit;

  Delete(s,1,n-1); n := 1;

  { The next character determines the tag type }

  case s[n] of
    'a'..'z','A'..'Z', '$', '&', '_': { a symbol name or potentially xvvv dvvv U+vvvv - allow any characters apart from ' " ( ) [ ] (whitespace) to continue it }
    begin
      while not Finished and not IsWhiteSpace and not IsPunctuation and not IsQuote do Inc(n);
      Dec(n);
    end;
    '''','"': { a string }
    begin
      ch := s[n]; Inc(n);
      while not Finished and (s[n] <> ch) do Inc(n);
    end;
    '0'..'9': { an 'octal' value (usually) }
    begin
      while not Finished and IsNumber do Inc(n);
      Dec(n);
    end;
    {else punctuation - do nothing}
  end;

  Result := Copy(s,1,n);
  Delete(s,1,n);
end;

function TfrmOSKKeyboardUsage.ParseKey(var s: WideString; var key: Integer; var shiftstate: TExtShiftState; var commandtype: Integer): Boolean;  // I3214   // I3521
var
  u: WideString;
begin
  Result := False;

  commandtype := 0;
  shiftstate := [];
  key := $FFFF;

  if GetTag(s) <> '[' then Exit;

  u := UpperCase(GetTag(s));

  if u = '+' then
  begin
    commandtype := 1; u := UpperCase(GetTag(s));
  end
  else if u = '-' then
  begin
    commandtype := 2; u := UpperCase(GetTag(s));
  end
  else if u = '~' then
  begin
    commandtype := 3; u := UpperCase(GetTag(s));
  end;

  while u <> ']' do
  begin
    if u = 'SHIFT' then Include(shiftstate, essShift)
    else if u = 'CTRL'  then Include(shiftstate, essCtrl)
    else if u = 'ALT'   then Include(shiftstate, essAlt)
    else if u = 'LCTRL' then Include(shiftstate, essLCtrl)
    else if u = 'LALT'  then Include(shiftstate, essLAlt)
    else if u = 'RCTRL' then Include(shiftstate, essRCtrl)
    else if u = 'RALT'  then Include(shiftstate, essRAlt)
    else
    begin
      if key <> $FFFF then Exit;
      key := FindVKeyName(u);
      if key = $FFFF then Exit;
    end;
    u := UpperCase(GetTag(s));
  end;

  Result := (key <> $FFFF) or (commandtype <> 0);
end;

function TfrmOSKKeyboardUsage.GetAsyncShiftState: TExtShiftState;  // I3214   // I3521
begin
  Result := [];
  if (GetAsyncKeyState(VK_SHIFT) and $8000) = $8000 then Result := Result + [essShift];
  if (GetAsyncKeyState(VK_CONTROL) and $8000) = $8000 then Result := Result + [essCtrl];
  if (GetAsyncKeyState(VK_MENU) and $8000) = $8000 then Result := Result + [essAlt];
  if (GetAsyncKeyState(VK_LCONTROL) and $8000) = $8000 then Result := Result + [essLCtrl];
  if (GetAsyncKeyState(VK_RCONTROL) and $8000) = $8000 then Result := Result + [essRCtrl];
  if (GetAsyncKeyState(VK_LMENU) and $8000) = $8000 then Result := Result + [essLAlt];
  if (GetAsyncKeyState(VK_RMENU) and $8000) = $8000 then Result := Result + [essRAlt];
end;

procedure TfrmOSKKeyboardUsage.SendKeys(keys: WideString);  // I3214   // I3521

  procedure PrepState(fkcss, ass: TExtShiftState; shift: TExtShiftStateValue; vk: Integer);
  var
    FExtended: Dword;
  begin
    if vk in [VK_RCONTROL, VK_RMENU] then FExtended := KEYEVENTF_EXTENDEDKEY else FExtended := 0;
    if (shift in fkcss) and not (shift in ass) then do_keybd_event(vk, 0, FExtended, 0)
    else if not (shift in fkcss) and (shift in ass) then do_keybd_event(vk, 0, FExtended or KEYEVENTF_KEYUP, 0);
  end;

  procedure FinalState(fkcss, ass: TExtShiftState; shift: TExtShiftStateValue; vk: Integer);
  var
    FExtended: Dword;
  begin
    if vk in [VK_RCONTROL, VK_RMENU] then FExtended := KEYEVENTF_EXTENDEDKEY else FExtended := 0;
    if (shift in fkcss) and not (shift in ass) then do_keybd_event(vk, 0, FExtended or KEYEVENTF_KEYUP, 0)
    else if not (shift in fkcss) and (shift in ass) then do_keybd_event(vk, 0, FExtended, 0);
  end;

var
  key, commandtype: Integer;
  shiftstate: TExtShiftState;
  ass: TExtShiftState;
  flags: Integer;
begin
  { format: [CTRL SHIFT ALT K_XXXX][...] }
  { format: [+CTRL][-CTRL] }
  { format: [+K_XXXX][-K_XXXX] }
  { format: [~K_XXXX] - this does not reset the shift state }

  SetLength(FInput, 0);

  while ParseKey(keys, key, shiftstate, commandtype) do
  begin
    if commandtype = 3 then
    begin
      do_keybd_event(key, 0, 0, 0);
      do_keybd_event(key, 0, KEYEVENTF_KEYUP, 0);
    end
    else if commandtype = 0 then
    begin
      ass := GetAsyncShiftState;
      KL.Log('SendKey: key=%x kbdShift=%s asyncShift=%s ', [key, ExtShiftStateToString(shiftstate), ExtShiftStateToString(ass)]);

      PrepState(shiftstate, ass, essShift, VK_SHIFT);
      PrepState(shiftstate, ass, essCtrl, VK_CONTROL);
      PrepState(shiftstate, ass, essAlt, VK_MENU);
      PrepState(shiftstate, ass, essLCtrl, VK_LCONTROL);
      PrepState(shiftstate, ass, essLAlt, VK_LMENU);
      PrepState(shiftstate, ass, essRCtrl, VK_RCONTROL);
      PrepState(shiftstate, ass, essRAlt, VK_RMENU);

      do_keybd_event(key, 0, 0, 0);
      do_keybd_event(key, 0, KEYEVENTF_KEYUP, 0);

      FinalState(shiftstate, ass, essShift, VK_SHIFT);
      FinalState(shiftstate, ass, essCtrl, VK_CONTROL);
      FinalState(shiftstate, ass, essAlt, VK_MENU);
      FinalState(shiftstate, ass, essLCtrl, VK_LCONTROL);
      FinalState(shiftstate, ass, essLAlt, VK_LMENU);
      FinalState(shiftstate, ass, essRCtrl, VK_RCONTROL);
      FinalState(shiftstate, ass, essRAlt, VK_RMENU);
    end
    else
    begin
      if commandtype = 1 then flags := 0 else flags := KEYEVENTF_KEYUP;

      if key = $FFFF then
      begin
        if essShift in shiftstate then do_keybd_event(VK_SHIFT, 0, flags, 0);
        if essCtrl in shiftstate then do_keybd_event(VK_CONTROL, 0, flags, 0);
        if essAlt in shiftstate then do_keybd_event(VK_MENU, 0, flags, 0);
        if essLCtrl in shiftstate then do_keybd_event(VK_LCONTROL, 0, flags, 0);
        if essLAlt in shiftstate then do_keybd_event(VK_LMENU, 0, flags, 0);
        if essRCtrl in shiftstate then do_keybd_event(VK_RCONTROL, 0, flags or KEYEVENTF_EXTENDEDKEY, 0);
        if essRAlt in shiftstate then do_keybd_event(VK_RMENU, 0, flags or KEYEVENTF_EXTENDEDKEY, 0);
      end
      else
        do_keybd_event(key, 0, flags, 0);
    end;
  end;

  if Length(FInput) > 0 then
    Windows.SendInput(Length(FInput), FInput[0], sizeof(FInput[0]));
end;

procedure TfrmOSKKeyboardUsage.SendChars(const chars: string);
var
  ch, s, t: string;
  n, v: Integer;
  hwnd: THandle;
begin
  hwnd := kmcom.Control.LastFocusWindow;

  if wm_keyman_char = 0 then
    wm_keyman_char := RegisterWindowMessage('WM_KEYMANCHAR');

  ch := Trim(chars);

  // I1429 - fix U+xxxx in insertchars URL

  n := Pos(' ', ch); if n = 0 then n := Length(ch)+1;
  t := '';

  while ch <> '' do
  begin
    s := Copy(ch, 1, n-1); if s = '' then Break;

    if Copy(s,1,2) = 'U+' then v := StrToIntDef('$'+Copy(s,3,6),0)
    else if s[1] = 'x' then v := StrToIntDef('$'+Copy(s,2,6),0)
    else if s[1] = 'd' then v := StrToIntDef(Copy(s,2,7), 0)
    else v := 0;

    if (v > 32) and (v <= $10FFFF) then
      t := t + Uni_UTF32CharToUTF16(v);

    Delete(ch, 1, n); ch := Trim(ch);
    n := Pos(' ', ch);
    if n = 0 then n := Length(ch)+1;
  end;

  if t <> '' then
    SendInputString(hwnd, t);
end;

procedure TfrmOSKKeyboardUsage.cefBeforeBrowse(Sender: TObject; const Url, command: string; params: TStringList; wasHandled: Boolean);
begin
  FireCommand(command, params);
end;

procedure TfrmOSKKeyboardUsage.cefKeyEvent(Sender: TObject;
  e: TCEFHostKeyEventData; wasShortcut, wasHandled: Boolean);
begin
  if e.event.kind in [KEYEVENT_RAWKEYDOWN, KEYEVENT_KEYDOWN] then
  begin
    if (e.event.windows_key_code = VK_F5) and ((e.event.modifiers and EVENTFLAG_CONTROL_DOWN) = EVENTFLAG_CONTROL_DOWN) then
      PostMessage(Handle, WM_USER_ContentRender, 0, 0)
    else if e.event.windows_key_code = VK_F1 then
      Application.HelpJump('context_'+lowercase(FDialogName));
  end;
end;

procedure TfrmOSKKeyboardUsage.cefLoadEnd(Sender: TObject);
begin
  FreeAndNil(FXMLFileName);   // I4181
  frmKeyman7Main.frmVisualKeyboard.SetTopMost(True);   // I4593
end;

procedure TfrmOSKKeyboardUsage.cefPreKeySyncEvent(Sender: TObject;
  e: TCEFHostKeyEventData; out isShortcut, Handled: Boolean);
begin
  if e.event.kind in [KEYEVENT_RAWKEYDOWN, KEYEVENT_KEYDOWN] then
    if (e.event.windows_key_code = VK_F5) and ((e.event.modifiers and EVENTFLAG_CONTROL_DOWN) = EVENTFLAG_CONTROL_DOWN) then
      Handled := True
    else if e.event.windows_key_code = VK_F1 then
      Handled := True;
end;

{$MESSAGE HINT 'TODO: Handle script errors'}
{procedure TfrmOSKKeyboardUsage.webScriptError(Sender: TObject; ErrorLine,
  ErrorCharacter, ErrorCode, ErrorMessage, ErrorUrl: string; var ScriptErrorAction: TScriptErrorAction);
begin
  ScriptErrorAction := eaCancel;
  //TODO: Log message to event log
end;}

{$MESSAGE HINT 'TODO: Support context menu'}
{
procedure TfrmOSKKeyboardUsage.webShowContextMenu(Sender: TCustomEmbeddedWB;
  const dwID: Cardinal; const ppt: PPoint; const CommandTarget: IInterface;
  const Context: IDispatch; var Result: HRESULT);
begin
  PostMessage(Handle, WM_CONTEXTMENU, web.Handle, MAKELONG(ppt.X, ppt.Y));
  Result := S_OK;
end;
}


procedure TfrmOSKKeyboardUsage.WMUser_ContentRender(var Message: TMessage);
begin
  Content_Render;
end;

procedure TfrmOSKKeyboardUsage.Content_Render;
var
  AdditionalData: WideString;
begin
  AdditionalData := FXML;

  FreeAndNil(FXMLFileName);   // I4181

  if not FileExists(GetXMLTemplatePath(FXMLRenderers.RenderTemplate)+FXMLRenderers.RenderTemplate) then
    Exit;

  FXMLFileName := FXMLRenderers.RenderToFile(False, AdditionalData);

  FDialogName := ChangeFileExt(ExtractFileName(FXMLRenderers.RenderTemplate), '');
  //HelpType := htKeyword;
  //HelpKeyword := FDialogName;
end;

procedure TfrmOSKKeyboardUsage.FireCommand(const command: WideString; params: TStringList);
begin
  if command = 'link' then TUtilExecute.URL(params.Values['url'])  // I3349
  else if command = 'tutorial' then
    //(ActiveProduct as IKeymanProduct2).OpenTutorial
  else if command = 'osk' then
    frmKeyman7Main.frmVisualKeyboard.ActivePage := apKeyboard
  else if command = 'welcome' then
    frmKeyman7Main.MnuOpenKeyboardHelp(nil)
  else if command = 'config' then
    frmKeyman7Main.MnuOpenKeymanConfiguration(frmKeyman7Main.frmVisualKeyboard)
  else if command = 'help' then
    frmKeyman7Main.MnuOpenProductHelp(frmKeyman7Main.frmVisualKeyboard)
  else if command = 'insertkeys' then
    SendKeys(params.Values['keys'])  // I3214   // I3521
  else if command = 'insertchars' then
    SendChars(params.Values['chars']);
end;

procedure TfrmOSKKeyboardUsage.FormCreate(Sender: TObject);
begin
  inherited;
  FLastActiveKeymanID := -2;

  cef := TframeCEFHost.Create(Self);
  cef.Parent := Self;
  cef.Visible := True;
  cef.ShouldOpenRemoteUrlsInBrowser := True;
  cef.OnBeforeBrowse := cefBeforeBrowse;
  cef.OnLoadEnd := cefLoadEnd;
  cef.OnKeyEvent := cefKeyEvent;
  cef.OnPreKeySyncEvent := cefPreKeySyncEvent;
end;

procedure TfrmOSKKeyboardUsage.FormDestroy(Sender: TObject);
begin
  inherited;
  FreeAndNil(FXMLFileName);   // I4181
end;

end.
