{
 * Keyman is copyright (C) SIL International. MIT License.
 *
 * Interactive LDML keyboard test host form
}
unit Keyman.Developer.UI.Debug.UfrmLdmlKeyboardDebug;

interface

uses
  System.Classes,
  System.Generics.Collections,
  System.StrUtils,
  System.SysUtils,
  Vcl.AppEvnts,
  Vcl.Buttons,
  Vcl.ComCtrls,
  Vcl.Controls,
  Vcl.Dialogs,
  Vcl.ExtCtrls,
  Vcl.Forms,
  Vcl.Graphics,
  Vcl.Grids,
  Vcl.Menus,
  Vcl.StdCtrls,
  Winapi.Messages,
  Winapi.Windows,

  CaptionPanel,
  debugdeadkeys,
  debugkeyboard,
  PaintPanel,
  KeymanDeveloperDebuggerMemo,
  Keyman.System.Debug.DebugCore,
  Keyman.System.Debug.DebugEvent,
  Keyman.System.Debug.DebugUIStatus,
  Keyman.System.KeymanCore,
  Keyman.System.KeymanCoreDebug,
  UframeTextEditor,
  UfrmMDIEditor,
  UfrmTike,
  UserMessages;

type
  TfrmLdmlKeyboardDebug = class(TTikeForm)
    memo: TKeymanDeveloperDebuggerMemo;
    mnuPopup: TPopupMenu;   // I4796
    mnuPopupFont: TMenuItem;   // I4796
    mnuPopupResetToQuotedFont: TMenuItem;   // I4796
    appEvents: TApplicationEvents;
    sgChars: TStringGrid;
    N1: TMenuItem;
    mnuCut: TMenuItem;
    mnuCopy: TMenuItem;
    mnuPaste: TMenuItem;   // I4808
    procedure FormCreate(Sender: TObject);
    procedure memoGotFocus(Sender: TObject);
    procedure memoLostFocus(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure memoChange(Sender: TObject);
    procedure memoSelMove(Sender: TObject);
    procedure cmdStopClick(Sender: TObject);
    procedure memoMessage(Sender: TObject; var Message: TMessage;
      var Handled: Boolean);
    procedure sgCharsDrawCell(Sender: TObject; ACol, ARow: Integer;
      Rect: TRect; State: TGridDrawState);   // I4808
    procedure memoClick(Sender: TObject);   // I4808
    procedure memoKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure FormResize(Sender: TObject);
  private

    FDebugVisible: Boolean;
    FRunning: Boolean;
    FFileName: string;
    debugkeyboard: TDebugKeyboard;
    _FCurrentEvent: Integer;
    FDefaultFont: Boolean;
    FEvents: TDebugEventList;
    FUIStatus: TDebugUIStatus;
    FUIDisabled: Boolean;

    { Deadkey member variables }
    FSelectedDeadkey: TDeadKeyInfo;
    FSavedSelection: TMemoSelection;

    procedure ResetEvents;
    procedure ExecuteEvent(n: Integer);
    procedure ExecuteEventAction(n: Integer);
    procedure SetUIStatus(const Value: TDebugUIStatus);
    procedure DisableUI;
    procedure EnableUI;
    function GetStatusText: string;
    procedure SetStatusText(Value: string);

  private
    FDebugCore: TDebugCore;
    FDeadkeys: TDebugDeadkeyInfoList;
    FEditorMemo: TframeTextEditor;

    FDebugFileName: WideString;
    procedure ClearDeadkeys;
    procedure ClearDeadkeyStyle;
    function GetCurrentEvent: TDebugEvent;
    procedure UninitDeadkeys;
    procedure UpdateDeadkeyDisplay;
    procedure UpdateDeadkeys;
    procedure ResetDebug;
    function SetupDebug: Boolean;
    procedure UpdateCharacterGrid;

    function ProcessKeyEvent(var Message: TMessage): Boolean;
    procedure CleanupCoreState;
    function SetKeyEventContext: Boolean;
    function HandleMemoKeydown(var Message: TMessage): Boolean;
    procedure SetCurrentEvent(Value: Integer);
    procedure Run;
    function GetContextFromMemo(
      IncludeMarkers: Boolean): TArray<km_core_context_item>;

  protected
    function GetHelpTopic: string; override;

  public
    procedure SelectDeadKey(DeadKey: TDeadKeyInfo);

  { General }
  protected
    property StatusText: string read GetStatusText write SetStatusText;
  public
    procedure UpdateFont(FFont: TFont);
    property DefaultFont: Boolean read FDefaultFont;

    function GetDebugKeyboard: TDebugKeyboard;

    property UIStatus: TDebugUIStatus read FUIStatus write SetUIStatus;

    property DebugFileName: WideString read FDebugFileName write FDebugFileName;
    property CompiledFileName: string read FFileName write FFileName;   // I4695
    property EditorMemo: TframeTextEditor read FEditorMemo write FEditorMemo;

    procedure ShowDebugForm;
    procedure HideDebugForm;

    property DebugVisible: Boolean read FDebugVisible;

    function ShortcutDisabled(Key: Word): Boolean;

    property CurrentEvent: TDebugEvent read GetCurrentEvent;
  end;

implementation

{$R *.DFM}

uses
  Keyman.Developer.System.HelpTopics,

  ActiveX,
  dmActionsDebugger,
  dmActionsMain,
  Glossary,
  Keyman.Developer.System.Project.ProjectLog,
  Keyman.Developer.UI.UfrmLdmlKeyboardEditor,
  Keyman.UI.Debug.CharacterGridRenderer,
  KeyNames,
  kmxfile,
  kmxfileconsts,
  ErrorControlledRegistry,
  RegistryKeys,
  KeymanDeveloperOptions,
  KeymanDeveloperUtils,
  ScanCodeMap,
  TextFileFormat,
  UfrmEditor,
  UfrmMain,
  UfrmMessages,
  UfrmRegressionTestFailure,
  UfrmSelectSystemKeyboard,
  Unicode,
  utilstr,
  UTikeDebugMode,
  VKeys,
  XString;

const
  // WM_KEYDOWN bits; KEYFLAG_KEYMAN is a reserved value
  KEYFLAG_KEYMAN   = $02000000;
  KEYFLAG_EXTENDED = $01000000;

{-------------------------------------------------------------------------------
 - Form events                                                                 -
 ------------------------------------------------------------------------------}

procedure TfrmLdmlKeyboardDebug.FormCreate(Sender: TObject);
begin
  inherited;

  FDeadkeys := TDebugDeadkeyInfoList.Create;

  FUIStatus := duiInvalid;
  FEvents := TDebugEventList.Create;
  FDefaultFont := True;
  memo.Align := alClient;

  UIStatus := duiReadyForInput;
end;

procedure TfrmLdmlKeyboardDebug.FormDestroy(Sender: TObject);
begin
  ResetDebug;
  FreeAndNil(FEvents);
  UninitDeadkeys;
end;

procedure TfrmLdmlKeyboardDebug.FormResize(Sender: TObject);
begin
  inherited;
  UpdateCharacterGrid;
end;

{-------------------------------------------------------------------------------
 - Memo management                                                             -
 ------------------------------------------------------------------------------}

procedure TfrmLdmlKeyboardDebug.memoGotFocus(Sender: TObject);
begin
  if UIStatus = duiReadyForInput then
    UIStatus := duiFocusedForInput;

  memoSelMove(memo);
end;

procedure TfrmLdmlKeyboardDebug.memoKeyUp(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  memoSelMove(memo);
end;

procedure TfrmLdmlKeyboardDebug.memoLostFocus(Sender: TObject);
begin
  if UIStatus = duiFocusedForInput then
    UIStatus := duiReadyForInput;
end;

function TfrmLdmlKeyboardDebug.HandleMemoKeydown(var Message: TMessage): Boolean;
begin
  if (Message.wParam = VK_ESCAPE) and
      (GetKeyState(VK_SHIFT) < 0) and
      (UIStatus <> duiPaused) then   // I4033
    UIStatus := duiPaused
  else if (Message.wParam = VK_F6) and
    (GetKeyState(VK_CONTROL) >= 0) and
    (GetKeyState(VK_MENU) >= 0) and
    (UIStatus <> duiFocusedForInput) then
  begin
    EditorMemo.SetFocus;
  end
  else if (Message.wParam = VK_ESCAPE) and
      (GetKeyState(VK_SHIFT) < 0) and
      (UIStatus = duiPaused) then
    UIStatus := duiFocusedForInput
  else if UIStatus in [duiTest, duiFocusedForInput] then
  begin
    Exit(ProcessKeyEvent(Message));
  end
  else
    Exit(False);

  Result := True;
end;

procedure TfrmLdmlKeyboardDebug.memoMessage(Sender: TObject; var Message: TMessage;
  var Handled: Boolean);
begin
  Handled := False;

  if UIStatus = duiClosing then
  begin
    Exit;  // Don't process while destroying...
  end;

  case Message.Msg of
    WM_KEYDOWN,
    WM_SYSKEYDOWN:
      Handled := HandleMemoKeydown(Message);
    WM_SYSCHAR:
      Handled := FUIDisabled;
    WM_CHAR:
      Handled := True;
  end;
end;

function TfrmLdmlKeyboardDebug.GetContextFromMemo(IncludeMarkers: Boolean): TArray<km_core_context_item>;
var
  n, i: Integer;
  ch: Char;
  dk: TDeadKeyInfo;
begin
  n := 0;
  SetLength(Result, Length(memo.Text)+1);
  i := 1;
  while i <= memo.SelStart + memo.SelLength do
  begin
    ch := memo.Text[i];
    if Uni_IsSurrogate1(ch) and (i < Length(memo.Text)) and
      Uni_IsSurrogate2(memo.Text[i+1]) then
    begin
      Result[n]._type := KM_CORE_CT_CHAR;
      Result[n].character := Uni_SurrogateToUTF32(ch, memo.Text[i+1]);
      Inc(i);
    end
    else if Ord(ch) = $FFFC then
    begin
      if IncludeMarkers then
      begin
        Result[n]._type := KM_CORE_CT_MARKER;
        dk := FDeadkeys.GetFromPosition(i-1);
        Assert(Assigned(dk));
        Result[n].marker := dk.Deadkey.Value;
      end
      else
      begin
        Inc(i);
        Continue;
      end;
    end
    else
    begin
      Result[n]._type := KM_CORE_CT_CHAR;
      Result[n].character := Ord(ch);
    end;
    Inc(i);
    Inc(n);
  end;

  Result[n]._type := KM_CORE_CT_END;
end;

function TfrmLdmlKeyboardDebug.SetKeyEventContext: Boolean;
var
  context: pkm_core_context;
  context_items: TArray<km_core_context_item>;
begin
  if memo.SelLength > 0 then
  begin
    // When there is a selection, we'll treat it as
    // empty context, because it does not make sense
    // for the keyboard to interact either with text
    // to the left of the selection, nor at the end
    // of the selection.
    //
    // Furthermore, telling Keyman Core that there is
    // an empty context helps it to do a 'normal'
    // backspace in the case of backspace key, rather
    // than deleting the last character of the selection
    // (Keyman Core is not aware of selection).
    km_core_state_context_clear(FDebugCore.State);
    Exit(True);
  end;

  // Set the cached context
  context_items := GetContextFromMemo(True);
  context := km_core_state_context(FDebugCore.State);
  Result := km_core_context_set(context, @context_items[0]) = KM_CORE_STATUS_OK;

  if Result then
  begin
    // Set the app context
    context_items := GetContextFromMemo(False);
    context := km_core_state_app_context(FDebugCore.State);
    Result := km_core_context_set(context, @context_items[0]) = KM_CORE_STATUS_OK;
  end;
end;

function TfrmLdmlKeyboardDebug.ProcessKeyEvent(var Message: TMessage): Boolean;
  function IsModifierKey(key: WORD): Boolean;
  begin
    case key of
      VK_LCONTROL,
      VK_RCONTROL,
      VK_CONTROL,
      VK_MENU,
      VK_LMENU,
      VK_RMENU,
      VK_SHIFT,
      VK_CAPITAL,
      VK_NUMLOCK,
      VK_SCROLL:
        Result := True;
    else
      Result := False;
    end;
  end;
var
  vkey, modifier: uint16_t;
begin
  Assert(Assigned(FDebugCore));

  // We always use the US virtual key code as a basis for our keystroke
  // mapping; the best way to do this is to extract the scan code from
  // the message data and work from that
  vkey := MapScanCodeToUSVK((Message.LParam and $FF0000) shr 16);
  modifier := 0;

  if GetKeyState(VK_LCONTROL) < 0 then modifier := modifier or KM_CORE_MODIFIER_LCTRL;
  if GetKeyState(VK_RCONTROL) < 0 then modifier := modifier or KM_CORE_MODIFIER_RCTRL;
  if GetKeyState(VK_LMENU) < 0 then modifier := modifier or KM_CORE_MODIFIER_LALT;
  if GetKeyState(VK_RMENU) < 0 then modifier := modifier or KM_CORE_MODIFIER_RALT;
  if GetKeyState(VK_SHIFT) < 0 then modifier := modifier or KM_CORE_MODIFIER_SHIFT;
  if (GetKeyState(VK_CAPITAL) and 1) = 1 then modifier := modifier or KM_CORE_MODIFIER_CAPS;

  if (modifier and (KM_CORE_MODIFIER_LCTRL or KM_CORE_MODIFIER_RALT)) = (KM_CORE_MODIFIER_LCTRL or KM_CORE_MODIFIER_RALT) then
  begin
    // #7506: Windows emits LCtrl+RAlt for AltGr for European keyboards; we want
    // to ignore this combination
    modifier := modifier and not KM_CORE_MODIFIER_LCTRL;
  end;

  if not SetKeyEventContext then
    Exit(False);

  if km_core_process_event(FDebugCore.State, Message.WParam, modifier, 1, KM_CORE_EVENT_FLAG_DEFAULT) = KM_CORE_STATUS_OK then
  begin
    // Process keystroke -- true = swallow keystroke
    Result := True;

    if IsModifierKey(vkey) then
    begin
      // We don't want to trigger the debugger on modifier keys
      Exit(True);
    end;

    FEvents.Clear;
    FEvents.AddLdmlStateItems(FDebugCore.State, vkey, modifier, debugkeyboard);

    FRunning := True;
    SetCurrentEvent(0);
    Run;
  end
  else
  begin
    FEvents.Clear;
    Result := False;
  end;
end;

procedure TfrmLdmlKeyboardDebug.SetCurrentEvent(Value: Integer);
begin
  _FCurrentEvent := Value;
end;

procedure TfrmLdmlKeyboardDebug.Run;
begin
  FRunning := True;
  try
    while (_FCurrentEvent < FEvents.Count) do
    begin
      ExecuteEvent(_FCurrentEvent);
      SetCurrentEvent(_FCurrentEvent + 1);
      if UIStatus = duiPaused then   // I4033
        Exit;
    end;
  finally
    FRunning := False;
    EnableUI;
    UpdateCharacterGrid;
    // We want to refresh the memo and character grid for rapid typing
    memo.Update;
    sgChars.Update;
  end;

  if UIStatus <> duiTest then
    if memo.Focused
      then UIStatus := duiFocusedForInput
      else UIStatus := duiReadyForInput;
end;

{ Stores }

procedure TfrmLdmlKeyboardDebug.ExecuteEvent(n: Integer);
begin
  memo.ReadOnly := False;
  memo.Selection := FSavedSelection;
  ExecuteEventAction(n);
  FSavedSelection := memo.Selection;
  memo.ReadOnly := True;
end;

procedure TfrmLdmlKeyboardDebug.ExecuteEventAction(n: Integer);
  type
    TMemoSelectionState = record
      Selection: TMemoSelection;
      Length: Integer;
    end;

  function SaveMemoSelectionState: TMemoSelectionState;
  var
    dk: TDeadKeyInfo;
  begin
    Result.Selection := memo.Selection;
    Result.Length := Length(memo.Text);
    for dk in FDeadkeys do
    begin
      if dk.Position >= Result.Selection.Start
        then dk.SavedPosition := -(Result.Length - dk.Position)
        else dk.SavedPosition := dk.Position;
    end;
  end;

  procedure RealignMemoSelectionState(state: TMemoSelectionState);
  var
    dk: TDeadKeyInfo;
    L: Integer;
    s: string;
  begin
    s := memo.Text;
    L := Length(s);
    for dk in FDeadkeys do
      if dk.SavedPosition < 0 then
      begin
        dk.Position := L + dk.SavedPosition;
        if (dk.Position < 0) or (dk.Position >= L) or (s[dk.Position + 1] <> #$FFFC) then
          // This can happen if the deadkey was in a replaced selection
          dk.Delete;
      end;

    UpdateDeadkeys;
  end;

  procedure DoBackspace(BackspaceType: km_core_backspace_type);
  var
    m, n: Integer;
    dk: TDeadKeyInfo;
    state: TMemoSelectionState;
  begin
    // Offset is zero-based, but string is 1-based. Beware!
    state := SaveMemoSelectionState;
    n := memo.SelStart;
    m := n;

    if memo.SelLength > 0 then
    begin
      // If the memo has a selection, we have given Core an empty context,
      // which forces it to emit a KM_CORE_BT_UNKNOWN backspace, which is
      // exactly what we want here. We just delete the selection
      Assert(BackspaceType = KM_CORE_BT_UNKNOWN);
      memo.SelText := '';
      RealignMemoSelectionState(state);
      Exit;
    end;

    case BackspaceType of
      KM_CORE_BT_MARKER:
        begin
          Assert(m >= 1);
          Assert(memo.Text[m] = #$FFFC);
          dk := FDeadkeys.GetFromPosition(m-1);
          Assert(Assigned(dk));
          dk.Delete;
          Dec(m);
        end;
      KM_CORE_BT_CHAR:
        begin
          Assert(m >= 1);
          Assert(memo.Text[m] <> #$FFFC);
          // Delete surrogate pairs
          if (m > 1) and
              Uni_IsSurrogate2(memo.Text[m]) and
              Uni_IsSurrogate1(memo.Text[m-1]) then
            Dec(m, 2)
          // Delete \r\n line breaks
          else if (m > 1) and
              (memo.Text[m] = #$0A) and
              (memo.Text[m-1] = #$0D) then
            Dec(m, 2)
          else
            Dec(m);
        end;
      KM_CORE_BT_UNKNOWN:
        begin
          while (m >= 1) and (memo.Text[m] = #$FFFC) do
          begin
            dk := FDeadkeys.GetFromPosition(m-1);
            Assert(Assigned(dk));
            dk.Delete;
            Dec(m);
          end;

          // Delete character
          if (m > 1) and
              Uni_IsSurrogate2(memo.Text[m]) and
              Uni_IsSurrogate1(memo.Text[m-1]) then
            Dec(m, 2)
          else
            Dec(m);

          // Also delete deadkeys to left of current character
          while (m >= 1) and (memo.Text[m] = #$FFFC) do
          begin
            dk := FDeadkeys.GetFromPosition(m-1);
            Assert(Assigned(dk));
            dk.Delete;
            Dec(m);
          end;
        end;
    else
      Assert(False, 'Unrecognised backspace type');
    end;

    memo.Text := Copy(memo.Text, 1, m) + Copy(memo.Text, n+1, MaxInt);
    memo.SelStart := m;

    RealignMemoSelectionState(state);
  end;

  procedure DoDeadkey(dkCode: Integer);
  var
    dk: TDeadKeyInfo;
    i: Integer;
    state: TMemoSelectionState;
  begin
    dk := TDeadKeyInfo.Create;
    dk.Memo := memo;
    dk.Deadkey := nil;
    for i := 0 to debugkeyboard.Deadkeys.Count - 1 do
      if debugkeyboard.Deadkeys[i].Value = dkCode then
      begin
        dk.Deadkey := debugkeyboard.Deadkeys[i];
        Break;
      end;
    if not Assigned(dk.Deadkey) then
      dk.Free //silent failure
    else
    begin
      state := SaveMemoSelectionState;

      memo.SelText := WideChar($FFFC);
      memo.SelStart := memo.SelStart + memo.SelLength;  // I1603
      memo.SelLength := 0;
      dk.Position := memo.SelStart - 1;

      RealignMemoSelectionState(state);

      FDeadkeys.Add(dk);
      UpdateDeadkeyDisplay;

    end;
  end;

  procedure DoHandleShortcut(vk: UINT);
  begin
    // Because we disable shortcuts in the debug memo, there are a small set of
    // editor Ctrl+Key shortcuts we need to rehandle here. Note that Ctrl+Ins,
    // Shift+Ins, Shift+Del appear to be handled natively by the debug memo
    // control (Windows code?), as are all cursor movement / selection keys,
    // apart from Ctrl+A

    if GetKeyState(VK_CONTROL) >= 0 then
      Exit;

    case vk of
      Ord('A'): memo.SelectAll;
      Ord('C'): memo.CopyToClipboard;
      Ord('V'): memo.PasteFromClipboard;
      Ord('X'): memo.CutToClipboard;
      Ord('Y'): ; // redo not supported in edit
      Ord('Z'): memo.Undo;
    end;
  end;

  procedure DoEmitKeystroke(dwData: DWord);
  var
    flag: Integer;
    msg: TMessage;
    state: TMemoSelectionState;
  begin
    if (LOBYTE(dwData) < VK_F1) or (LOBYTE(dwData) > VK_F12) then
    begin
      flag := KEYFLAG_KEYMAN;
      if (dwData and $100) = $100 then
        // Extended Key State
        flag := flag or KEYFLAG_EXTENDED;

      state := SaveMemoSelectionState;
      if GetKeyState(VK_MENU) < 0
        then msg.Msg := WM_SYSKEYDOWN
        else msg.Msg := WM_KEYDOWN;
      msg.WParam := LOBYTE(dwData);
      msg.LParam := flag;
      memo.Dispatch(msg);
      if GetKeyState(VK_MENU) < 0
        then msg.Msg := WM_SYSKEYUP
        else msg.Msg := WM_KEYUP;
      memo.Dispatch(msg);

      DoHandleShortcut(LOBYTE(dwData));

      RealignMemoSelectionState(state);
    end;
  end;

  procedure DoChar(const text: string);
  var
    state: TMemoSelectionState;
  begin
    state := SaveMemoSelectionState;
    // Line breaks: replace \r (0x0D) with \r\n (0x0D 0x0A) so line breaks work
    memo.SelText := ReplaceStr(Text, #$0D, #$0D#$0A);
    memo.SelStart := memo.SelStart + memo.SelLength;  // I1603
    memo.SelLength := 0;
    RealignMemoSelectionState(state);
  end;

  procedure DoBell;
  begin
    MessageBeep(0);
  end;

begin
  DisableUI;
  with FEvents[n].Action do
  begin
    case ActionType of
      KM_CORE_IT_EMIT_KEYSTROKE: DoEmitKeystroke(dwData);
      KM_CORE_IT_CHAR:           DoChar(Text);
      KM_CORE_IT_MARKER:         DoDeadkey(dwData);
      KM_CORE_IT_ALERT:          DoBell;
      KM_CORE_IT_BACK:           DoBackspace(km_core_backspace_type(dwData));
      KM_CORE_IT_PERSIST_OPT: ; //TODO
      KM_CORE_IT_CAPSLOCK:    ; //TODO
      KM_CORE_IT_INVALIDATE_CONTEXT: ; // no-op
    end;
//    AddDEBUG(Format('%d: %d [%s]', [ActionType, dwData, Text]));
  end;
  EnableUI;
end;

procedure TfrmLdmlKeyboardDebug.DisableUI;
begin
  FUIDisabled := True;
//  UpdateControlCaptions;
end;

procedure TfrmLdmlKeyboardDebug.EnableUI;
begin
  if not FRunning then
  begin
    FUIDisabled := False;
  end;
end;

procedure TfrmLdmlKeyboardDebug.CleanupCoreState;
begin
  FreeAndNil(FDebugCore);
end;

procedure TfrmLdmlKeyboardDebug.ResetEvents;
begin
  if _FCurrentEvent > 0 then
  begin
    FEvents.Clear;
    SetCurrentEvent(0);
  end;
end;

procedure TfrmLdmlKeyboardDebug.HideDebugForm;
begin
  frmKeymanDeveloper.ShowDebug(False);   // I4796
  FDebugVisible := False;
  ResetDebug;
  UIStatus := duiReadyForInput;
end;

procedure TfrmLdmlKeyboardDebug.ShowDebugForm;
begin
  frmKeymanDeveloper.ShowDebug(True);   // I4796

  FDebugVisible := True;

  //UpdateFont(nil);
  if not SetupDebug then
    Exit;

  memo.SetFocus;
end;

procedure TfrmLdmlKeyboardDebug.ResetDebug;
begin
  FreeAndNil(debugkeyboard);
  SetCurrentEvent(1);
  ResetEvents;
  ClearDeadkeys;  // I1699
  CleanupCoreState;
  memo.Text := '';
end;

function TfrmLdmlKeyboardDebug.SetupDebug: Boolean;
var
  buf: array[0..KL_NAMELENGTH] of Char;
begin
  GetKeyboardLayoutName(buf);

  try
    FDebugCore := TDebugCore.Create(FFileName, True);
  except
    on E:Exception do
    begin
      CleanupCoreState;
      Winapi.Windows.SetFocus(0);
      HideDebugForm;
      ShowMessage('Unable to start the debugger: '+E.Message);
      Exit(False);
    end;
  end;

  debugkeyboard := TDebugKeyboard.Create(FFileName);
  Result := True;
end;

procedure TfrmLdmlKeyboardDebug.UpdateFont(FFont: TFont);
begin
  if FFont = nil then
  begin
    FDefaultFont := True;
    FFont := EditorMemo.CharFont;
  end
  else
    FDefaultFont := False;
  memo.Font := FFont;
end;

procedure TfrmLdmlKeyboardDebug.SetUIStatus(const Value: TDebugUIStatus);
var
  FOldUIStatus: TDebugUIStatus;
begin
  if FUIStatus <> Value then
  begin
    FOldUIStatus := FUIStatus;
    FUIStatus := Value;

    case Value of
      duiTest:
        begin
          StatusText := 'Simple Test';
          memo.ReadOnly := False;
          memo.IsDebugging := True;
        end;
      duiFocusedForInput:
        begin
          DisableUI;
          StatusText := 'Focused for input';
          memo.ReadOnly := False;
          memo.IsDebugging := True;
        end;
      duiReadyForInput:
        begin
          if memo.Focused then UIStatus := duiFocusedForInput
          else
          begin
            EnableUI;
            StatusText := 'Ready for input';
            memo.ReadOnly := True;
            memo.IsDebugging := False;
          end;
        end;
      duiPaused:
        begin
          EnableUI;
          FUIDisabled := False;   // I4033
          StatusText := 'Paused';
          memo.ReadOnly := True;
          memo.IsDebugging := False;
        end;
    end;
    if FOldUIStatus = duiTest then
    begin
      if memo.Focused then
      begin
        GetParentForm(memo).ActiveControl := nil;
        memo.SetFocus;
      end;
    end
  end;
end;

function TfrmLdmlKeyboardDebug.GetCurrentEvent: TDebugEvent;
begin
  if (_FCurrentEvent >= 0) and (_FCurrentEvent < FEvents.Count)
    then Result := FEvents[_FCurrentEvent]
    else Result := nil;
end;

function TfrmLdmlKeyboardDebug.GetDebugKeyboard: TDebugKeyboard;
begin
  Result := debugkeyboard;
end;

function TfrmLdmlKeyboardDebug.GetHelpTopic: string;
begin
  Result := SHelpTopic_Context_Debug;
end;

function TfrmLdmlKeyboardDebug.GetStatusText: string;
begin
  Result := frmKeymanDeveloper.barStatus.Panels[1].Text;
end;

procedure TfrmLdmlKeyboardDebug.sgCharsDrawCell(Sender: TObject; ACol, ARow: Integer;
  Rect: TRect; State: TGridDrawState);
begin
  TCharacterGridRenderer.Render(sgChars, ACol, ARow, Rect, State, memo.Font);
end;

procedure TfrmLdmlKeyboardDebug.SetStatusText(Value: string);
begin
  frmKeymanDeveloper.barStatus.Panels[1].Text := Value;
end;

procedure TfrmLdmlKeyboardDebug.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  UIStatus := duiClosing;
end;

{-------------------------------------------------------------------------------
 - Deadkeys                                                                    -
 ------------------------------------------------------------------------------}

procedure TfrmLdmlKeyboardDebug.UpdateDeadkeys;
var
  i: Integer;
  Found: Boolean;
begin
  Found := False;
  for i := FDeadkeys.Count - 1 downto 0 do
    if FDeadkeys[i].Deleted then
    begin
      ClearDeadkeyStyle;
      FDeadkeys.Delete(i);
      Found := True;
    end;
  if Found then UpdateDeadkeyDisplay;
end;

procedure TfrmLdmlKeyboardDebug.UpdateDeadkeyDisplay;
begin
  if not (csDestroying in ComponentState) then
  begin
    UpdateCharacterGrid;   // I4808
  end;
end;

procedure TfrmLdmlKeyboardDebug.ClearDeadkeys;
begin
  ClearDeadkeyStyle;
  FDeadkeys.Clear;
  UpdateDeadkeyDisplay;
end;

procedure TfrmLdmlKeyboardDebug.UninitDeadkeys;
begin
  ClearDeadkeys;
  FreeAndNil(FDeadkeys);
end;

procedure TfrmLdmlKeyboardDebug.ClearDeadkeyStyle;
begin
  if Assigned(FSelectedDeadkey) then
  begin
    FSelectedDeadkey := nil;
  end;
end;

procedure TfrmLdmlKeyboardDebug.SelectDeadKey(DeadKey: TDeadKeyInfo);
begin
  ClearDeadkeyStyle;
  if not Assigned(DeadKey) then Exit;
  FSelectedDeadkey := DeadKey;
  memo.SelStart := FSelectedDeadkey.Position;
  memo.SelLength := 1;
  memo.SetFocus;
end;

procedure TfrmLdmlKeyboardDebug.memoChange(Sender: TObject);
begin
  UpdateDeadkeys;
  memoSelMove(memo);
end;

procedure TfrmLdmlKeyboardDebug.memoClick(Sender: TObject);
begin
  memoSelMove(memo);
end;

{-------------------------------------------------------------------------------
 - Control captions                                                            -
 ------------------------------------------------------------------------------}

function TfrmLdmlKeyboardDebug.ShortcutDisabled(Key: Word): Boolean;
begin
  Result := False;
  if not FUIDisabled then Exit;
  if Key in [VK_F1..VK_F12] then
    Result := True
  else if GetKeyState(VK_CONTROL) < 0 then
  begin
    if Key in [Ord('A')..Ord('Z'), Ord('0')..Ord('9')] then
      Result := True;
  end;
end;

procedure TfrmLdmlKeyboardDebug.memoSelMove(Sender: TObject);
begin
  if memo.Focused then
  begin
    frmKeymanDeveloper.barStatus.Panels[0].Text := 'Debugger Active';
  end;

  if not memo.ReadOnly then
  begin
    FSavedSelection := memo.Selection;
    UpdateCharacterGrid;   // I4808
  end;
end;

procedure TfrmLdmlKeyboardDebug.UpdateCharacterGrid;   // I4808
begin
  if csDestroying in ComponentState then
    Exit;

  TCharacterGridRenderer.Fill(sgChars, memo.Text, FDeadkeys, memo.SelStart, memo.SelLength, memo.Selection.Anchor);
  TCharacterGridRenderer.Size(sgChars, memo.Font);
end;

{-------------------------------------------------------------------------------
 - System keyboard layout                                                      -
 ------------------------------------------------------------------------------}

procedure TfrmLdmlKeyboardDebug.cmdStopClick(Sender: TObject);
begin
  modActionsDebugger.actDebugStopDebugger.Execute;
end;

end.

