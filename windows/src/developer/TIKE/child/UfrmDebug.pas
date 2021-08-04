{
 * Keyman is copyright (C) SIL International. MIT License.
 *
 * Interactive keyboard debugger host form
}
unit UfrmDebug;

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
  HintLabel,
  PaintPanel,
  KeymanDeveloperDebuggerMemo,
  Keyman.System.Debug.DebugCore,
  Keyman.System.Debug.DebugEvent,
  Keyman.System.Debug.DebugUIStatus,
  Keyman.System.KeymanCore,
  Keyman.System.KeymanCoreDebug,
  UframeTextEditor,
  UfrmDebugStatus,
  UfrmMDIEditor,
  UfrmTike,
  UserMessages;

type
  TDebugLineEvent = procedure(Sender: TObject; ALine: Integer) of object;

  TDebugBreakpoint = class
  strict private
    FOwner: TframeTextEditor;
  private
    FTrueLineNumber: Integer;
    function GetTrueLineNumber: Integer;
    procedure SetTrueLineNumber(const Value: Integer);
  public
    constructor Create(AOwner: TframeTextEditor);
    property TrueLineNumber: Integer read GetTrueLineNumber write SetTrueLineNumber;
  end;

  TDebugBreakpoints = class(TObjectList<TDebugBreakpoint>);

  TfrmDebug = class(TTikeForm)
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
    FBreakpoints: TDebugBreakpoints;
    FRunning, FFoundBreakpoint, FForceKeyboard: Boolean;
    FFileName: string;
    FExecutionPointLine: Integer;
    debugkeyboard: TDebugKeyboard;
    _FCurrentEvent: Integer;
    FDefaultFont: Boolean;
    FEvents: TDebugEventList;
    FUIStatus: TDebugUIStatus;
    FUIDisabled: Boolean;
    LastSelStart: Integer;

    { Deadkey member variables }
    FSelectedDeadkey: TDeadKeyInfo;

    { Keyman32 integration functions }
    function frmDebugStatus: TfrmDebugStatus;

    procedure ResetEvents;
    procedure ExecuteEvent(n: Integer);
    procedure ExecuteEventAction(n: Integer);
    procedure ExecuteEventRule(n: Integer);
    procedure SetExecutionPointLine(ALine: Integer);
    procedure SetUIStatus(const Value: TDebugUIStatus);
    procedure DisableUI;
    procedure EnableUI;
    procedure SetForceKeyboard(Value: Boolean); // TODO: refactor this away -- we should use only FUIStatus
    function GetStatusText: string;
    procedure SetStatusText(Value: string);
    procedure UpdateDebugStatusForm;   // I4809

  private
    FDebugCore: TDebugCore;
    FDeadkeys: TDebugDeadkeyInfoList;
    FEditorMemo: TframeTextEditor;

    FDebugFileName: WideString;
    FSingleStepMode: Boolean;
    FCanDebug: Boolean;
    FOnUpdateExecutionPoint: TDebugLineEvent;
    FOnClearBreakpoint: TDebugLineEvent;
    FOnSetBreakpoint: TDebugLineEvent;
    procedure ClearDeadkeys;
    procedure ClearDeadkeyStyle;
    function GetCurrentEvent: TDebugEvent;
    procedure UninitDeadkeys;
    procedure UpdateDeadkeyDisplay;
    procedure UpdateDeadkeys;
    procedure SetSingleStepMode(const Value: Boolean);
    procedure ResetDebug;
    procedure SetupDebug;
    procedure UpdateCharacterGrid;

    function ProcessKeyEvent(var Message: TMessage): Boolean;
    procedure CleanupCoreState;
    function SetKeyEventContext: Boolean;
    function HandleMemoKeydown(var Message: TMessage): Boolean;
    procedure SetCurrentEvent(Value: Integer);

  protected
    function GetHelpTopic: string; override;

  public
    procedure SelectDeadKey(DeadKey: TDeadKeyInfo);
    procedure StepForward;
    procedure Run;
    procedure Pause;
    procedure Unpause;

  { General }
  protected
    property ForceKeyboard: Boolean read FForceKeyboard write SetForceKeyboard;
    property StatusText: string read GetStatusText write SetStatusText;
  public
    procedure UpdateFont(FFont: TFont);
    property DefaultFont: Boolean read FDefaultFont;

    procedure SetBreakpoint(ALine: Integer);
    procedure ClearBreakpoint(ALine: Integer);
    procedure ToggleBreakpoint(ALine: Integer);

    function IsBreakPointLine(ALine: Integer): Boolean;
    function IsExecutionPointLine(ALine: Integer): Boolean;

    function GetDebugKeyboard: TDebugKeyboard;

    property CanDebug: Boolean read FCanDebug write FCanDebug;
    property UIStatus: TDebugUIStatus read FUIStatus write SetUIStatus;

    property SingleStepMode: Boolean read FSingleStepMode write SetSingleStepMode;

    property DebugFileName: WideString read FDebugFileName write FDebugFileName;
    property CompiledFileName: string read FFileName write FFileName;   // I4695
    property EditorMemo: TframeTextEditor read FEditorMemo write FEditorMemo;

    property ExecutionPointLine: Integer read FExecutionPointLine write SetExecutionPointLine;

    procedure ShowDebugForm;
    procedure HideDebugForm;

    property DebugVisible: Boolean read FDebugVisible;

    procedure ListBreakpoints;

    function ShortcutDisabled(Key: Word): Boolean;

    property CurrentEvent: TDebugEvent read GetCurrentEvent;

    property OnSetBreakpoint: TDebugLineEvent read FOnSetBreakpoint write FOnSetBreakpoint;
    property OnClearBreakpoint: TDebugLineEvent read FOnClearBreakpoint write FOnClearBreakpoint;
    property OnUpdateExecutionPoint: TDebugLineEvent read FOnUpdateExecutionPoint write FOnUpdateExecutionPoint;
  end;

implementation

{$R *.DFM}

uses
  Keyman.Developer.System.HelpTopics,

  ActiveX,
  dmActionsKeyboardEditor,
  dmActionsMain,
  Glossary,
  Keyman.Developer.System.Project.ProjectLog,
  KeyNames,
  kmxfile,
  kmxfileconsts,
  OnlineConstants,
  ErrorControlledRegistry,
  RegistryKeys,
  KeymanDeveloperOptions,
  KeymanDeveloperUtils,
  TextFileFormat,
  UfrmEditor,
  UfrmKeymanWizard,
  UfrmMain,
  UfrmMessages,
  UfrmRegressionTestFailure,
  UfrmSelectSystemKeyboard,
  Unicode,
  utilstr,
  UTikeDebugMode,
  VKeys,
  XString;

{
F5/Shift-F5: Enable/disable keyboard debugging/testing
F9: Set/clear breakpoint
F10: Step/run mode
F6: Usual window pane switching
F11: Step forward through rules
Shift-F11: Step backward through rules
F12: Run normally
}

const
  // WM_KEYDOWN bits; KEYFLAG_KEYMAN is a reserved value
  KEYFLAG_KEYMAN   = $02000000;
  KEYFLAG_EXTENDED = $01000000;

{-------------------------------------------------------------------------------
 - Form events                                                                 -
 ------------------------------------------------------------------------------}

procedure TfrmDebug.FormCreate(Sender: TObject);
begin
  inherited;

  FBreakpoints := TDebugBreakpoints.Create;
  FDeadkeys := TDebugDeadkeyInfoList.Create;

  FExecutionPointLine := -1;
  FUIStatus := duiInvalid;
  FEvents := TDebugEventList.Create;
  FDefaultFont := True;
  memo.Align := alClient;

  UIStatus := duiReadyForInput;

  frmDebugStatus.SetDebugMemo(memo);
  frmDebugStatus.Deadkeys.OnSelectDeadkey := SelectDeadkey;
  frmDebugStatus.DeadKeys.Deadkeys := FDeadkeys;
  frmDebugStatus.RegTest.Deadkeys := FDeadkeys;
  frmDebugStatus.Visible := True;
end;

procedure TfrmDebug.FormDestroy(Sender: TObject);
begin
  ResetDebug;
  FreeAndNil(FBreakpoints);
  FreeAndNil(FEvents);
  UninitDeadkeys;
  frmDebugStatus.Deadkeys.OnSelectDeadkey := nil;
  frmDebugStatus.DeadKeys.Deadkeys := nil;
  frmDebugStatus.RegTest.Deadkeys := nil;
  frmDebugStatus.Visible := False;
end;

procedure TfrmDebug.FormResize(Sender: TObject);
begin
  inherited;
  UpdateCharacterGrid;
end;

function TfrmDebug.frmDebugStatus: TfrmDebugStatus;
begin
  Result := (Owner as TfrmKeymanWizard).DebugStatusForm;
end;

{-------------------------------------------------------------------------------
 - Memo management                                                             -
 ------------------------------------------------------------------------------}

procedure TfrmDebug.memoGotFocus(Sender: TObject);
begin
  case UIStatus of
    duiReadyForInput: UIStatus := duiFocusedForInput;
    duiTest: ForceKeyboard := True;
  end;

  memoSelMove(memo);
end;

procedure TfrmDebug.memoKeyUp(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  memoSelMove(memo);
end;

procedure TfrmDebug.memoLostFocus(Sender: TObject);
begin
  case UIStatus of
    duiTest: ForceKeyboard := False;
    duiFocusedForInput: UIStatus := duiReadyForInput;
  end;
end;

function TfrmDebug.HandleMemoKeydown(var Message: TMessage): Boolean;
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
  else if (Message.Msg = WM_KEYDOWN) and
    (UIStatus = duiFocusedForInput) then
  begin
    Exit(ProcessKeyEvent(Message));
  end
  else
    Exit(False);

  Result := True;
end;

procedure TfrmDebug.memoMessage(Sender: TObject; var Message: TMessage;
  var Handled: Boolean);
begin
  Handled := False;

  if UIStatus = duiClosing then
  begin
    Exit;  // Don't process while destroying...
  end;

  case Message.Msg of
    WM_KEYDOWN:
      Handled := HandleMemoKeydown(Message);
    WM_SYSCHAR:
      Handled := FUIDisabled;
    WM_CHAR:
      Handled := True;
  end;
end;

function TfrmDebug.SetKeyEventContext: Boolean;
var
  context: pkm_kbp_context;
  context_items: TArray<km_kbp_context_item>;
  n, i: Integer;
  ch: Char;
  dk: TDeadKeyInfo;
begin
  context := km_kbp_state_context(FDebugCore.State);

  n := 0;
  SetLength(context_items, Length(memo.Text)+1);
  i := 1;
  while i <= memo.SelStart + memo.SelLength do
  begin
    ch := memo.Text[i];
    if Uni_IsSurrogate1(ch) and (i < Length(memo.Text)) and
      Uni_IsSurrogate2(memo.Text[i+1]) then
    begin
      context_items[n]._type := KM_KBP_CT_CHAR;
      context_items[n].character := Uni_SurrogateToUTF32(ch, memo.Text[i+1]);
      Inc(i);
    end
    else if Ord(ch) = $FFFC then
    begin
      context_items[n]._type := KM_KBP_CT_MARKER;
      dk := FDeadkeys.GetFromPosition(i-1);
      Assert(Assigned(dk));
      context_items[n].marker := dk.Deadkey.Value+1; // TODO: remove +1 -1 messes for deadkey codes //GetDeadkeyMarker(i);
    end
    else
    begin
      context_items[n]._type := KM_KBP_CT_CHAR;
      context_items[n].character := Ord(ch);
    end;
    Inc(i);
    Inc(n);
  end;

  context_items[n]._type := KM_KBP_CT_END;
  Result := km_kbp_context_set(context, @context_items[0]) = KM_KBP_STATUS_OK;
end;

function TfrmDebug.ProcessKeyEvent(var Message: TMessage): Boolean;
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
  modifier: uint16_t;
begin
  Assert(Assigned(FDebugCore));
  modifier := 0;
  if GetKeyState(VK_LCONTROL) < 0 then modifier := modifier or KM_KBP_MODIFIER_LCTRL or KM_KBP_MODIFIER_CTRL;
  if GetKeyState(VK_RCONTROL) < 0 then modifier := modifier or KM_KBP_MODIFIER_RCTRL or KM_KBP_MODIFIER_CTRL;
  if GetKeyState(VK_LMENU) < 0 then modifier := modifier or KM_KBP_MODIFIER_LALT or KM_KBP_MODIFIER_ALT;
  if GetKeyState(VK_RMENU) < 0 then modifier := modifier or KM_KBP_MODIFIER_RALT or KM_KBP_MODIFIER_ALT;
  if GetKeyState(VK_SHIFT) < 0 then modifier := modifier or KM_KBP_MODIFIER_SHIFT;
  if (GetKeyState(VK_CAPITAL) and 1) = 1 then modifier := modifier or KM_KBP_MODIFIER_CAPS;

  if not SetKeyEventContext then
    Exit(False);

  if km_kbp_process_event(FDebugCore.State, Message.WParam, modifier, 1) = KM_KBP_STATUS_OK then
  begin
    // Process keystroke
    Result := True;

    if IsModifierKey(Message.WParam) then
    begin
      // We don't want to trigger the debugger on modifier keys
      Exit(True);
    end;

    FEvents.Clear;
    FEvents.AddStateItems(FDebugCore.State, Message.WParam, modifier, debugkeyboard);

    if FSingleStepMode then
    begin
      UIStatus := duiDebugging;
      SetCurrentEvent(1);
      frmDebugStatus.Events.SetEvents(FEvents);
      ExecuteEvent(0);
    end
    else
    begin
      FRunning := True;
      UIStatus := duiDebugging;
      SetCurrentEvent(0);
      Run;
    end;
  end
  else
  begin
    FEvents.Clear;
    Result := False;
  end;
end;

procedure TfrmDebug.SetCurrentEvent(Value: Integer);
begin
  _FCurrentEvent := Value;
  frmDebugStatus.CurrentEvent := GetCurrentEvent;
end;

procedure TfrmDebug.StepForward;
begin
  if UIStatus = duiTest then Exit;

  if _FCurrentEvent < FEvents.Count then
  begin
    while (_FCurrentEvent < FEvents.Count) and (CurrentEvent.EventType = etAction) do
    begin
      ExecuteEvent(_FCurrentEvent);
      SetCurrentEvent(_FCurrentEvent+1);
    end;
    if _FCurrentEvent < FEvents.Count then
    begin
      ExecuteEvent(_FCurrentEvent);
      SetCurrentEvent(_FCurrentEvent+1);
    end;
  end;

  if _FCurrentEvent = FEvents.Count then
  begin
    if memo.Focused
      then UIStatus := duiFocusedForInput
      else UIStatus := duiReadyForInput;

    frmDebugStatus.RegTest.RegTestLogContext;
    frmDebugStatus.RegTest.RegTestNextKey;
  end;
end;

procedure TfrmDebug.Run;
begin
  if UIStatus = duiTest then Exit;

  FFoundBreakpoint := False;
  FRunning := True;
  try
    while (_FCurrentEvent < FEvents.Count) do
    begin
      ExecuteEvent(_FCurrentEvent);
      SetCurrentEvent(_FCurrentEvent + 1);
      if FFoundBreakpoint then
      begin
        FRunning := False;
        ExecutionPointLine := ExecutionPointLine;

        if _FCurrentEvent > 0
          then frmDebugStatus.Elements.UpdateStores(FEvents[_FCurrentEvent-1])
          else frmDebugStatus.Elements.UpdateStores(nil);

        if FKeymanDeveloperOptions.DebuggerSingleStepAfterBreak then
          FSingleStepMode := True;

        FUIStatus := duiInvalid; // Update control states
        UIStatus := duiDebugging;
        Exit;
      end
      else if UIStatus = duiPaused then   // I4033
        Exit;
    end;
  finally
    FRunning := False;
  end;

  if memo.Focused
    then UIStatus := duiFocusedForInput
    else UIStatus := duiReadyForInput;

  frmDebugStatus.RegTest.RegTestLogContext;
  frmDebugStatus.RegTest.RegTestNextKey;
end;

{ Stores }

procedure TfrmDebug.ExecuteEvent(n: Integer);
begin
  memo.ReadOnly := False;
  if FEvents[n].EventType = etAction
    then ExecuteEventAction(n)
    else ExecuteEventRule(n);
  memo.ReadOnly := True;
end;

procedure TfrmDebug.ExecuteEventRule(n: Integer);
  procedure ExecuteBegin(ev: TDebugEvent);
  begin
    frmDebugStatus.Key.ShowKey(@ev.Rule.Key);
    frmDebugStatus.RegTest.RegTestLogKey(@ev.Rule.Key);
  end;
begin
  with FEvents[n].Rule do
  begin
    if Line > 0 then
    begin
      ExecutionPointLine := Line - 1;
      if not FRunning
        then frmDebugStatus.Elements.UpdateStores(FEvents[n])
        else frmDebugStatus.Elements.UpdateStores(nil);
      if IsBreakPointLine(Line-1) then
      begin
        { Stop running, if breakpoint options are also met }
        if not (ItemType in [
          KM_KBP_DEBUG_GROUP_EXIT,
          KM_KBP_DEBUG_RULE_EXIT,
          KM_KBP_DEBUG_MATCH_EXIT,
          KM_KBP_DEBUG_NOMATCH_EXIT,
          KM_KBP_DEBUG_END
        ]) or FKeymanDeveloperOptions.DebuggerBreakWhenExitingLine then
          FFoundBreakpoint := True;
      end;
    end;

    { Update call stack, execution point line }

    case ItemType of
      KM_KBP_DEBUG_BEGIN:
        ExecuteBegin(FEvents[n]);
      KM_KBP_DEBUG_GROUP_ENTER,
      KM_KBP_DEBUG_RULE_ENTER,
      KM_KBP_DEBUG_NOMATCH_ENTER,
      KM_KBP_DEBUG_MATCH_ENTER:
        frmDebugStatus.CallStack.CallStackPush(FEvents[n].Rule);
      KM_KBP_DEBUG_RULE_EXIT,
      KM_KBP_DEBUG_NOMATCH_EXIT,
      KM_KBP_DEBUG_MATCH_EXIT,
      KM_KBP_DEBUG_GROUP_EXIT:
        frmDebugStatus.CallStack.CallStackPop;
      KM_KBP_DEBUG_END:
        begin frmDebugStatus.CallStack.CallStackClear; ExecutionPointLine := -1; end;
    else
      Assert(False);
    end;
  end;
end;


procedure TfrmDebug.ExecuteEventAction(n: Integer);
  procedure DoBackspace(BackspaceType: km_kbp_backspace_type);
  var
    m, n: Integer;
    dk: TDeadKeyInfo;
  begin
    // Offset is zero-based, but string is 1-based. Beware!
    n := memo.SelStart;
    m := n;

    case BackspaceType of
      KM_KBP_BT_MARKER:
        begin
          Assert(m >= 1);
          Assert(memo.Text[m] = #$FFFC);
          dk := FDeadkeys.GetFromPosition(m-1);
          Assert(Assigned(dk));
          dk.Delete;
          Dec(m);
        end;
      KM_KBP_BT_CHAR:
        begin
          Assert(m >= 1);
          Assert(memo.Text[m] <> #$FFFC);
          if (m > 1) and
              Uni_IsSurrogate2(memo.Text[m]) and
              Uni_IsSurrogate1(memo.Text[m-1]) then
            Dec(m, 2)
          else
            Dec(m);
        end;
      KM_KBP_BT_UNKNOWN:
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
  end;

  procedure ClearKeyStack;
  var
    msg: TMsg;
  begin
    while PeekMessage(msg, memo.Handle, WM_KEYFIRST, WM_KEYLAST, PM_REMOVE) do;
  end;

  procedure DoDeadkey(dkCode: Integer);
  var
    dk: TDeadKeyInfo;
    i: Integer;
  begin
    dk := TDeadKeyInfo.Create;
    //dk.Position := memo.SelStart;
    dk.Memo := memo;
    dk.Deadkey := nil;
    for i := 0 to debugkeyboard.Deadkeys.Count - 1 do
      if debugkeyboard.Deadkeys[i].Value+1 = dkCode then
      begin
        dk.Deadkey := debugkeyboard.Deadkeys[i];
        Break;
      end;
    if not Assigned(dk.Deadkey) then
      dk.Free //silent failure
    else
    begin
      // TODO: this method of inserting chars differs from Action_Char;
      memo.SelText := WideChar($FFFC);
      memo.SelStart := memo.SelStart + memo.SelLength;  // I1603
      memo.SelLength := 0;
      dk.Position := memo.SelStart - 1;
      FDeadkeys.Add(dk);
      //PostMessage(memo.Handle, WM_UNICHAR, $FFFC, 0); Application.ProcessMessages;
      UpdateDeadkeyDisplay;
    end;
  end;

  procedure DoEmitKeystroke(dwData: DWord);
  var
    flag: Integer;
  begin
    if (LOBYTE(dwData) < VK_F1) or (LOBYTE(dwData) > VK_F12) then
    begin
      flag := KEYFLAG_KEYMAN;
      if (dwData and $100) = $100 then
        // Extended Key State
        flag := flag or KEYFLAG_EXTENDED;

      if GetKeyState(VK_MENU) < 0
        then PostMessage(memo.Handle, WM_SYSKEYDOWN, LOBYTE(dwData), flag)
        else PostMessage(memo.Handle, WM_KEYDOWN, LOBYTE(dwData), flag);

  {   if GetKeyState(VK_MENU) < 0
        then PostMessage(memo.Handle, WM_SYSKEYUP, dwData, 0)
        else PostMessage(memo.Handle, WM_KEYUP, dwData, 0);}
    end;
  end;

  procedure DoChar(const text: string);
  var
    i: Integer;
    msg: TMessage;
  begin
    for i := 1 to Length(Text) do
    begin
      ClearKeyStack;
      msg.Msg := WM_CHAR;
      msg.WParam := Ord(Text[i]);
      msg.LParam := 0;
      memo.Dispatch(msg);
//                          else PostMessageW(memo.Handle, WM_CHAR, Ord(Text[i]), 0);  // I3310
//                        Application.ProcessMessages;
    end;
  end;

  procedure DoBell;
  begin
    MessageBeep(0);
  end;

begin
  DisableUI;
  ClearKeyStack;
  frmDebugStatus.Elements.UpdateStores(nil);
  with FEvents[n].Action do
  begin
    case ActionType of
      KM_KBP_IT_EMIT_KEYSTROKE: DoEmitKeystroke(dwData);
      KM_KBP_IT_CHAR:           DoChar(Text);
      KM_KBP_IT_MARKER:         DoDeadkey(dwData);
      KM_KBP_IT_ALERT:          DoBell;
      KM_KBP_IT_BACK:           DoBackspace(km_kbp_backspace_type(dwData));
      KM_KBP_IT_PERSIST_OPT: ; //TODO
      KM_KBP_IT_INVALIDATE_CONTEXT: ; // no-op
    end;
    Application.ProcessMessages;
//    AddDEBUG(Format('%d: %d [%s]', [ActionType, dwData, Text]));
  end;
  EnableUI;
end;

procedure TfrmDebug.DisableUI;
begin
  FUIDisabled := True;
//  UpdateControlCaptions;
end;

procedure TfrmDebug.EnableUI;
begin
  if not FRunning then
  begin
    FUIDisabled := False;
  end;
end;

procedure TfrmDebug.SetForceKeyboard(Value: Boolean);
begin
  if Value <> FForceKeyboard then
  begin
    FForceKeyboard := Value;

    if FForceKeyboard then
    begin
      try
        memo.SetFocus;

        FDebugCore := TDebugCore.Create(FFileName, True);
        frmDebugStatus.SetDebugCore(FDebugCore);

      except
        on E:Exception do
        begin
          CleanupCoreState;
          Winapi.Windows.SetFocus(0);
          HideDebugForm;
          FForceKeyboard := False;
          ShowMessage(E.Message);
          Exit;
        end;
      end;
    end
    else
    begin
      CleanupCoreState;
    end;
  end;
end;

procedure TfrmDebug.CleanupCoreState;
begin
  FreeAndNil(FDebugCore);
  frmDebugStatus.SetDebugCore(nil);
end;

procedure TfrmDebug.ResetEvents;
begin
  if _FCurrentEvent > 0 then
  begin
    SetCurrentEvent(0);
    FEvents.Clear;
  end;
end;

procedure TfrmDebug.HideDebugForm;
begin
  frmKeymanDeveloper.ShowDebug(False);   // I4796
  FDebugVisible := False;
  ResetDebug;
  UIStatus := duiReadyForInput;
end;

procedure TfrmDebug.ShowDebugForm;
begin
  frmKeymanDeveloper.ShowDebug(True);   // I4796

  FDebugVisible := True;

  //UpdateFont(nil);
  SetupDebug;

  memo.SetFocus;
end;

procedure TfrmDebug.ResetDebug;
begin
  ForceKeyboard := False;
  FreeAndNil(debugkeyboard);
  if (frmDebugStatus <> nil) then
    frmDebugStatus.RegTest.SetDebugKeyboard(nil);
  SetCurrentEvent(1);
  ResetEvents;
  if (frmDebugStatus <> nil) and Assigned(frmDebugStatus.Key) then   // I2594
    frmDebugStatus.Key.ShowKey(nil);
  ExecutionPointLine := -1;
  ClearDeadkeys;  // I1699
  CleanupCoreState;
  memo.Text := '';
end;

procedure TfrmDebug.SetupDebug;
var
  buf: array[0..KL_NAMELENGTH] of Char;
begin
  if UIStatus <> duiTest then
  begin
    GetKeyboardLayoutName(buf);

    debugkeyboard := TDebugKeyboard.Create(FFileName);
    frmDebugStatus.RegTest.RegTestSetup(buf, FFileName, False);   // I3655
    frmDebugStatus.RegTest.SetDebugKeyboard(debugkeyboard);
    frmDebugStatus.Elements.ClearStores;
  end
  else ResetDebug;
end;

{-------------------------------------------------------------------------------
 - Breakpoint management                                                       -
 ------------------------------------------------------------------------------}

procedure TfrmDebug.SetBreakpoint(ALine: Integer);
var
  b: TDebugBreakpoint;
begin
  if IsBreakpointLine(ALine) then
    Exit;

  b := TDebugBreakpoint.Create(EditorMemo);
  b.TrueLineNumber := ALine;
  FBreakpoints.Add(b);

  if Assigned(FOnSetBreakpoint) then
    FOnSetBreakpoint(Self, ALine);
end;

procedure TfrmDebug.ClearBreakpoint(ALine: Integer);
var
  b: TDebugBreakpoint;
begin
  for b in FBreakpoints do
    if b.TrueLineNumber = ALine then
    begin
      FBreakpoints.Remove(b);
      if Assigned(FOnClearBreakpoint) then
        FOnClearBreakpoint(Self, ALine);
      Exit;
    end;
end;

procedure TfrmDebug.ToggleBreakpoint(ALine: Integer);
begin
  if IsBreakpointLine(ALine)
    then ClearBreakpoint(ALine)
    else SetBreakpoint(ALine)
end;

function TfrmDebug.IsBreakPointLine(ALine: Integer): Boolean;
var
  b: TDebugBreakpoint;
begin
  for b in FBreakpoints do
    if b.TrueLineNumber = ALine then
      Exit(True);
  Exit(False);
end;

procedure TfrmDebug.ListBreakpoints;
var
  b: TDebugBreakpoint;
begin
  memo.Clear;
  for b in FBreakpoints do
    if Assigned(FOnSetBreakpoint) then
      FOnSetBreakpoint(Self, b.TrueLineNumber);
end;

{-------------------------------------------------------------------------------
 - Execution point management                                                  -
 ------------------------------------------------------------------------------}

function TfrmDebug.IsExecutionPointLine(ALine: Integer): Boolean;
begin
  Result := ALine = FExecutionPointLine;
end;

procedure TfrmDebug.SetExecutionPointLine(ALine: Integer);
begin
  FExecutionPointLine := ALine;
  if FRunning and (FExecutionPointLine >= 0) then Exit;
  if Assigned(FOnUpdateExecutionPoint) then
    FOnUpdateExecutionPoint(Self, ALine);
end;

procedure TfrmDebug.UpdateFont(FFont: TFont);
begin
  if FFont = nil then
  begin
    FDefaultFont := True;
    FFont := EditorMemo.CharFont;
  end
  else
    FDefaultFont := False;
  memo.Font := FFont;
  frmDebugStatus.DisplayFont := FFont;
end;

procedure TfrmDebug.Pause;
begin
  if UIStatus = duiTest then Exit;
  UIStatus := duiPaused;
end;

procedure TfrmDebug.Unpause;
begin
  if UIStatus = duiTest then Exit;
  if UIStatus = duiPaused then
    if memo.Focused
      then UIStatus := duiFocusedForInput
      else UIStatus := duiReadyForInput;
end;

procedure TfrmDebug.SetUIStatus(const Value: TDebugUIStatus);
var
  FOldUIStatus: TDebugUIStatus;
begin
  if FUIStatus <> Value then
  begin
    if not FCanDebug and (Value <> duiTest) then Exit;

    FOldUIStatus := FUIStatus;
    FUIStatus := Value;

    if FOldUIStatus = duiTest then
    begin
      ForceKeyboard := False;
      SetupDebug;
    end;

    case Value of
      duiTest:
        begin
          StatusText := 'Simple Test';
          memo.ReadOnly := False;
          ResetDebug;
          if memo.Focused then
          begin
            GetParentForm(memo).ActiveControl := nil;
            memo.SetFocus;
            memoGotFocus(Self);
          end;
        end;
      duiDebugging:
        begin
          //SelectSystemLayout(False);
          EnableUI;
          //ForceKeyboard := False;
          StatusText := 'Debugging';
          memo.ReadOnly := True;
        end;
      duiReceivingEvents:
        begin
          DisableUI;
          StatusText := 'Receiving events';
          memo.ReadOnly := True;
        end;
      duiFocusedForInput:
        begin
          DisableUI;
          frmDebugStatus.Elements.UpdateStores(nil);
          frmDebugStatus.Key.ShowKey(nil);
          ForceKeyboard := True;
          StatusText := 'Focused for input';
          memo.ReadOnly := False;
          //SelectSystemLayout(True);
        end;
      duiReadyForInput:
        begin
          if memo.Focused then UIStatus := duiFocusedForInput
          else
          begin
            //SelectSystemLayout(False);
            EnableUI;
            if (frmDebugStatus <> nil) then // I2770, I2713
            begin
              frmDebugStatus.Elements.UpdateStores(nil);
              frmDebugStatus.Key.ShowKey(nil);
            end;
            ForceKeyboard := False;
            StatusText := 'Ready for input';
            memo.ReadOnly := True;
          end;
        end;
      duiPaused:
        begin
          //SelectSystemLayout(False);
          EnableUI;
          FUIDisabled := False;   // I4033
          ForceKeyboard := False;   // I4033
          StatusText := 'Paused';
          memo.ReadOnly := True;
        end;
    end;
    UpdateDebugStatusForm;   // I4809
    if FOldUIStatus = duiTest then
    begin
      frmDebugStatus.Parent.Visible := True;
      if memo.Focused then
      begin
        GetParentForm(memo).ActiveControl := nil;
        //ActiveControl := memo;
        memo.SetFocus;
      end;
    end
    else if FUIStatus = duiTest then
      frmDebugStatus.Parent.Visible := False;
  end;
end;

function TfrmDebug.GetCurrentEvent: TDebugEvent;
begin
  if (_FCurrentEvent >= 0) and (_FCurrentEvent < FEvents.Count)
    then Result := FEvents[_FCurrentEvent]
    else Result := nil;
end;

function TfrmDebug.GetDebugKeyboard: TDebugKeyboard;
begin
  Result := debugkeyboard;
end;

function TfrmDebug.GetHelpTopic: string;
begin
  Result := SHelpTopic_Context_Debug;
end;

function TfrmDebug.GetStatusText: string;
begin
  Result := frmKeymanDeveloper.barStatus.Panels[1].Text;
end;

procedure TfrmDebug.sgCharsDrawCell(Sender: TObject; ACol, ARow: Integer;
  Rect: TRect; State: TGridDrawState);   // I4808
begin
  if (ARow = 0) and (sgChars.Objects[ACol, ARow] = Pointer(0))
    then sgChars.Canvas.Font := memo.Font
    else sgChars.Canvas.Font := sgChars.Font;
  sgChars.Canvas.TextRect(Rect,
    (Rect.Right + Rect.Left - sgChars.Canvas.TextWidth(sgChars.Cells[ACol, ARow])) div 2,
    (Rect.Top + Rect.Bottom - sgChars.Canvas.TextHeight(sgChars.Cells[ACol, ARow])) div 2,
    sgChars.Cells[ACol, ARow]);
end;

procedure TfrmDebug.SetSingleStepMode(const Value: Boolean);
begin
  FSingleStepMode := Value;
end;

procedure TfrmDebug.SetStatusText(Value: string);
begin
  frmKeymanDeveloper.barStatus.Panels[1].Text := Value;
end;

procedure TfrmDebug.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  UIStatus := duiClosing;
end;

{function TfrmDebug.GetUIDisabled: Boolean;
begin
  Result := FUIStatus in [duiFocusedForInput, duiReceivingEvents];
end;}

{-------------------------------------------------------------------------------
 - Deadkeys                                                                    -
 ------------------------------------------------------------------------------}

procedure TfrmDebug.UpdateDeadkeys;
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

procedure TfrmDebug.UpdateDebugStatusForm;   // I4809
begin
  if (frmDebugStatus <> nil) then
    frmDebugStatus.Key.UIStatus := FUIStatus;
end;

procedure TfrmDebug.UpdateDeadkeyDisplay;
begin
  if not (csDestroying in ComponentState) then
    frmDebugStatus.DeadKeys.UpdateDeadKeyDisplay;
  UpdateCharacterGrid;   // I4808
end;

procedure TfrmDebug.ClearDeadkeys;
begin
  ClearDeadkeyStyle;
  FDeadkeys.Clear;
  UpdateDeadkeyDisplay;
end;

procedure TfrmDebug.UninitDeadkeys;
begin
  ClearDeadkeys;
  FreeAndNil(FDeadkeys);
end;

procedure TfrmDebug.ClearDeadkeyStyle;
begin
  if Assigned(FSelectedDeadkey) then
  begin
    FSelectedDeadkey := nil;
  end;
end;

procedure TfrmDebug.SelectDeadKey(DeadKey: TDeadKeyInfo);
begin
  ClearDeadkeyStyle;
  if not Assigned(DeadKey) then Exit;
  FSelectedDeadkey := DeadKey;
  memo.SelStart := FSelectedDeadkey.Position;
  memo.SelLength := 1;
  memo.SetFocus;
end;

procedure TfrmDebug.memoChange(Sender: TObject);
begin
  LastSelStart := memo.SelStart;
  UpdateDeadkeys;
  memoSelMove(memo);
end;

procedure TfrmDebug.memoClick(Sender: TObject);
begin
  memoSelMove(memo);
end;

{-------------------------------------------------------------------------------
 - Control captions                                                            -
 ------------------------------------------------------------------------------}

function TfrmDebug.ShortcutDisabled(Key: Word): Boolean;
begin
  Result := False;
  if not FUIDisabled then Exit;
  if Key in [VK_F1..VK_F12] then
    Result := True
  else if GetKeyState(VK_CONTROL) < 0 then
  begin
    if Key in [Ord('A').. Ord('Z'), Ord('0')..Ord('9')] then
      Result := True;
  end;
end;

procedure TfrmDebug.memoSelMove(Sender: TObject);
var
  ch: WideString;
begin
  if memo.Focused then
  begin
    frmKeymanDeveloper.barStatus.Panels[0].Text := 'Debugger Active';
    if memo.SelText = ''
      then ch := Unicode.CopyChar(memo.Text, memo.SelStart-1, 1)
      else ch := Copy(memo.SelText, 1, 16);

    if ch = ''
      then frmKeymanDeveloper.barStatus.Panels[2].Text := ''
      else frmKeymanDeveloper.barStatus.Panels[2].Text := FormatUnicode(ch);
  end;

  UpdateCharacterGrid;   // I4808
end;

procedure TfrmDebug.UpdateCharacterGrid;   // I4808
var
  SelStart, SelLength, MaxCols, I: Integer;
  s: string;
  J: Integer;
  K: Integer;
begin
  MaxCols := sgChars.ClientWidth div (sgChars.DefaultColWidth + 1);

  if memo.SelLength < 0 then
  begin
    SelStart := memo.SelStart + memo.SelLength;
    SelLength := -memo.SelLength;
  end
  else if memo.SelLength > 0 then
  begin
    SelStart := memo.SelStart;
    SelLength := memo.SelLength;
  end
  else
  begin
    SelStart := 0;
    SelLength := memo.SelStart;
  end;

  s := Copy(memo.Text, 1, SelStart+SelLength);

  J := 0; I := SelStart + SelLength; // I is 1-based Delphi string index
  while (J < MaxCols) and (I > SelStart) do
  begin
    if (I > 1) and Uni_IsSurrogate2(s[I]) and Uni_IsSurrogate1(s[I-1]) then
      Dec(I);
    Inc(J); Dec(I);
  end;

  if J = 0 then
  begin
    sgChars.ColCount := 1;
    sgChars.Objects[0,0] := Pointer(0);
    sgChars.Cells[0,0] := '';
    sgChars.Cells[0,1] := '';
    Exit;
  end
  else
    sgChars.ColCount := J;

  Inc(I);
  J := 0;
  while J < sgChars.ColCount do
  begin
    if Ord(S[I]) = $FFFC then
    begin
      sgChars.Objects[J, 0] := Pointer(1);
      sgChars.Cells[J, 0] := '???';
      for K := 0 to FDeadkeys.Count-1 do
        if FDeadkeys[K].Position = I-1 then
        begin
          sgChars.Cells[J, 0] := FDeadkeys[K].Deadkey.Name;
          break;
        end;
      sgChars.Cells[J, 1] := 'Deadkey';
    end
    else if Uni_IsSurrogate1(s[I]) and (I < Length(s)) and Uni_IsSurrogate2(s[I+1]) then
    begin
      sgChars.Objects[J, 0] := Pointer(0);
      sgChars.Cells[J, 0] := Copy(s, I ,2);
      sgChars.Cells[J, 1] := 'U+'+IntToHex(Uni_SurrogateToUTF32(s[I], s[I+1]), 5);
      Inc(I);
    end
    else
    begin
      sgChars.Objects[J, 0] := Pointer(0);
      sgChars.Cells[J, 0] := s[I];
      sgChars.Cells[J, 1] := 'U+'+IntToHex(Ord(s[i]), 4);
    end;
    Inc(J); Inc(I);
  end;

  with sgChars.Canvas do
  begin
    Font := memo.Font;
    sgChars.RowHeights[0] := TextHeight('A') + 4;
    Font := sgChars.Font;
    sgChars.RowHeights[1] := TextHeight('A') + 4;
  end;
  sgChars.Height := sgChars.RowHeights[0] + sgChars.RowHeights[1] + 4;
end;

{-------------------------------------------------------------------------------
 - System keyboard layout                                                      -
 ------------------------------------------------------------------------------}

procedure TfrmDebug.cmdStopClick(Sender: TObject);
begin
  modActionsKeyboardEditor.actDebugStopDebugger.Execute;
end;

{ TDebugBreakpoint }

constructor TDebugBreakpoint.Create(AOwner: TframeTextEditor);
begin
  inherited Create;
  FOwner := AOwner;
end;

function TDebugBreakpoint.GetTrueLineNumber: Integer;
begin
  Result := FTrueLineNumber;
end;

procedure TDebugBreakpoint.SetTrueLineNumber(const Value: Integer);
begin
  FTrueLineNumber := Value;
end;

end.

