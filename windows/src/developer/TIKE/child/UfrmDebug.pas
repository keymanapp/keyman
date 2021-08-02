(*
  Name:             UfrmDebug
  Copyright:        Copyright (C) SIL International.
  Documentation:
  Description:
  Create Date:      20 Jun 2006

  Modified Date:    3 Aug 2015
  Authors:          mcdurdin
  Related Files:
  Dependencies:

  Bugs:
  Todo:             If input is entered into the editor, close the debug form after query -- don't worry about this.
  Notes:
  History:          20 Jun 2006 - mcdurdin - Initial version
                    01 Aug 2006 - mcdurdin - Rename FTikeOptiosn to FKeymanDeveloperOptions
                    23 Aug 2006 - mcdurdin - Extracted from TfrmEditor to be a dockable window
                    14 Sep 2006 - mcdurdin - Rework as a dockable panel
                    28 Sep 2006 - mcdurdin - Added CanDebug flag
                    04 Dec 2006 - mcdurdin - Tweak debugging startup
                    04 Jan 2007 - mcdurdin - Fix Keyman Debugger activation when switching in and out of Keyman Desktop
                    22 Jan 2007 - mcdurdin - Fix font display
                    30 Jan 2007 - mcdurdin - Remove pro edition watermark
                    05 Nov 2007 - mcdurdin - Added system debugging
                    05 Nov 2007 - mcdurdin - I1129 - Block rapid typing (99%)
                    19 Nov 2007 - mcdurdin - I1157 - const string parameters
                    16 Jan 2009 - mcdurdin - I1699 - Fix crash in debugger when clearing deadkeys
                    17 Dec 2010 - mcdurdin - I2594 - Fix crash when closing debugger
                    18 Mar 2011 - mcdurdin - I2801 - Debugger doesn't work on first attempt
                    18 Mar 2011 - mcdurdin - I2713 - Fix crash when closing debugger
                    18 Mar 2011 - mcdurdin - I2770 - Fix crash when closing debugger
                    18 Mar 2011 - mcdurdin - I1603 - Cursor position can go wrong in debugger when deadkeys are used
                    03 May 2011 - mcdurdin - I2890 - Record diagnostic data when encountering registry errors
                    18 May 2012 - mcdurdin - I3323 - V9.0 - Change from Plus-MemoU to Plus-Memo
                    18 May 2012 - mcdurdin - I3306 - V9.0 - Remove TntControls + Win9x support
                    17 Aug 2012 - mcdurdin - I3310 - V9.0 - Unicode in Delphi fixes
                    24 Jan 2012 - mcdurdin - I3173 - Unusual crash when ending debug session now handled and additional error info provided
                    23 Mar 2012 - mcdurdin - I3283 - Debugger should start Keyman Desktop if it isn't running
                    03 Nov 2012 - mcdurdin - I3503 - V9.0 - Merge of I3283 - Debugger should start Keyman Desktop if it isn't running
                    03 Nov 2012 - mcdurdin - I3504 - V9.0 - Merge of I3173 - Unusual crash when ending debug session now handled and additional error info provided
                    04 Dec 2013 - mcdurdin - I3655 - V9.0 - Keyboard debugger does not appear to function in 9.0.419.0
                    10 Jan 2014 - mcdurdin - I4020 - V9.0 - Refactor TSF debug profile management into TDebugUtils
                    07 Feb 2014 - mcdurdin - I4033 - V9.0 - Enable SHIFT+ESC to pause debugger with TSF
                    31 Dec 2014 - mcdurdin - I4331 - V9.0 - Debugger development
                    04 May 2015 - mcdurdin - I4695 - V9.0 - Debugger needs to use project file filename not internal name
                    23 Jun 2015 - mcdurdin - I4767 - Keyboard debugger does not always activate profile correctly
                    24 Jul 2015 - mcdurdin - I4796 - Refresh Keyman Developer look and feel for release
                    03 Aug 2015 - mcdurdin - I4808 - Add character preview to debug window
                    03 Aug 2015 - mcdurdin - I4809 - Track keystrokes in debug status form

*)


unit UfrmDebug;  // I3323  // I3306

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
  debugging,
  debugkeyboard,
  DebugUtils,
  HintLabel,
  PaintPanel,
  KeymanDeveloperDebuggerMemo,
  Keyman.System.KeymanCore,
  Keyman.System.KeymanCoreDebug,
  msctf,
  UframeTextEditor,
  UfrmMDIEditor,
  UfrmTike,
  UserMessages;

type
  TDebugLineEvent = procedure(Sender: TObject; ALine: Integer) of object;

  TDebugUIStatus = (duiInvalid, duiPaused, duiFocusedForInput, duiReadyForInput, duiReceivingEvents, duiDebugging,
    duiClosing, duiDebuggingOutput, duiTest);

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
    //FEditor: TfrmTikeEditor;
    FExecutionPointLine: Integer;
    debugkeyboard: TDebugKeyboard;
    FCurrentEvent: Integer;
    FDefaultFont: Boolean;
    FEvents: TDebugEventList;
    FUIStatus: TDebugUIStatus;
    FUIDisabled: Boolean;
    LastSelStart: Integer;

    { Control caption member variables }
    //FControlCaptions: TStringList;

    { Deadkey member variables }
    deadkeys: TDebugDeadkeyInfoList;
    FSelectedDeadkey: TDeadKeyInfo;
    //hklSystemKeyboard: HKL;
    FSaveShiftState: Integer;
    FLastActiveProfile: TDebugUtilProfile;   // I4331

    { Editor integration functions }
//    function EditorMemo: TPlusMemoU;

    { Keyman32 integration functions }
    function frmDebugStatus: TForm;

    function DiscoverRuleLine(debug: pkm_kbp_state_debug_item): Integer;
    procedure ResetEvents;
    procedure ExecuteEvent(n: Integer);
    procedure WMUserDebugEnd(var Message: TMessage); message WM_USER_DebugEnd;
    procedure ExecuteEventAction(n: Integer);
    procedure ExecuteEventRule(n: Integer);
    procedure SetExecutionPointLine(ALine: Integer);
    procedure SetUIStatus(const Value: TDebugUIStatus);
    procedure DisableUI;
    procedure EnableUI;
    procedure SetForceKeyboard(Value: Boolean);
    function GetStatusText: string;
    procedure SetStatusText(Value: string);
    procedure UpdateDebugStatusForm;   // I4809

//    procedure KeymanGetContext(var Message: TMessage);

    { Debug panel functions }
    procedure AddDeadkey(dkCode: Integer);

  { ANSI Test}
  private
    keyboard: pkm_kbp_keyboard;
    state: pkm_kbp_state;
    FANSITest: Boolean;
    FEditorMemo: TframeTextEditor;

    FDebugFileName: WideString;
    FSingleStepMode: Boolean;
    FCanDebug: Boolean;
    FOnUpdateExecutionPoint: TDebugLineEvent;
    FOnClearBreakpoint: TDebugLineEvent;
    FOnSetBreakpoint: TDebugLineEvent;
    procedure SetANSITest(const Value: Boolean);
    procedure UpdateANSITest;
    procedure ClearDeadkeys;
    procedure ClearDeadkeyStyle;
    function GetCurrentEvent: TDebugEvent;
    procedure InitDeadkeys;
    procedure UninitDeadkeys;
    procedure UpdateDeadkeyDisplay;
    procedure UpdateDeadkeys;
    procedure SetSingleStepMode(const Value: Boolean);
    procedure ResetDebug;
    procedure SetupDebug;
    procedure UpdateCharacterGrid;

    function ProcessKeyEvent(var Message: TMessage): Boolean;
    function ProcessActionItem(key: Word; action: pkm_kbp_action_item): Boolean;
    procedure CleanupCoreState;
    procedure Action_DeleteBack(expected_type: uint8_t; expected_value: uintptr_t);
    procedure Action_Char(const character: km_kbp_usv);
    procedure Action_Marker(marker: uintptr_t);
    function SetKeyEventContext: Boolean;
    procedure Action_EmitKeystroke(const key: Word);
    procedure ProcessDebugItem(debug: pkm_kbp_state_debug_item);
    function HandleMemoKeydown(var Message: TMessage): Boolean;

  protected
    function GetHelpTopic: string; override;

  public
    function CanChangeANSITest: Boolean;
    property ANSITest: Boolean read FANSITest write SetANSITest;

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

    procedure FillDeadkeys(startpos: Integer; var s: WideString);

    //procedure SetSystemKeyboardID(ID: string);

    property DebugFileName: WideString read FDebugFileName write FDebugFileName;
    property CompiledFileName: string read FFileName write FFileName;   // I4695
    //property Editor: TfrmTikeEditor read FEditor write FEditor;
    property EditorMemo: TframeTextEditor read FEditorMemo write FEditorMemo;

    property ExecutionPointLine: Integer read FExecutionPointLine write SetExecutionPointLine;

    procedure ShowDebugForm;
    procedure HideDebugForm;

    property DebugVisible: Boolean read FDebugVisible;

    procedure ListBreakpoints;

    //property UIDisabled: Boolean read GetUIDisabled;
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
  UfrmDebugStatus,
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
F4: switch between ANSI/Unicode?
F5/Shift-F5: Enable/disable keyboard debugging/testing
F9: Set/clear breakpoint
F10: Step/run mode
F6: Usual window pane switching
F11: Step forward through rules
Shift-F11: Step backward through rules
F12: Run normally
}
const
  qid: array[0..10] of string = ('BEGIN_UNICODE', 'BEGIN_ANSI',
    'QID_GROUP_ENTER', 'QID_GROUP_EXIT', 'QID_RULE_ENTER',
    'QID_RULE_EXIT', 'QID_MATCH_ENTER', 'QID_MATCH_EXIT',
    'QID_NOMATCH_ENTER', 'QID_NOMATCH_EXIT', 'QID_END');

  qit: array[0..7] of string = ('QIT_VKEYDOWN', 'QIT_VKEYUP',
    'QIT_VSHIFTDOWN', 'QIT_VSHIFTUP', 'QIT_CHAR',
    'QIT_DEADKEY', 'QIT_BELL', 'QIT_BACK');

const
  // WM_KEYDOWN bits; KEYFLAG_KEYMAN is a reserved value
  KEYFLAG_KEYMAN   = $02000000;
  KEYFLAG_EXTENDED = $01000000;

{-------------------------------------------------------------------------------
 - Form events                                                                 -
 ------------------------------------------------------------------------------}

var
 KeymanCoreLoaded: Boolean = False;

procedure InitKeymanCore;
begin
  if not KeymanCoreLoaded then
  begin
    // TODO: debugpath for this
    _km_kbp_set_library_path(ExtractFilePath(ParamStr(0)) + '..\..\..\..\..\..\..\common\core\desktop\build\x86\debug\src\' + kmnkbp0);
    KeymanCoreLoaded := True;
  end;
end;
procedure TfrmDebug.FormCreate(Sender: TObject);
begin
  inherited;
//  InitMSCTF;   // I3655

  InitKeymanCore;

  TDebugUtils.GetActiveTSFProfile(FLastActiveProfile);   // I4331
  FBreakpoints := TDebugBreakpoints.Create;

  FExecutionPointLine := -1;
  //InitControlCaptions;
  FUIStatus := duiInvalid;
  FEvents := TDebugEventList.Create;
  FDefaultFont := True;
  memo.Align := alClient;

  //InitSystemKeyboard;
  UIStatus := duiReadyForInput;
  InitDeadKeys;
end;

procedure TfrmDebug.FormDestroy(Sender: TObject);
begin
  ResetDebug;
  FreeAndNil(FBreakpoints);
  FreeAndNil(FEvents);
  UninitDeadkeys;
end;

procedure TfrmDebug.FormResize(Sender: TObject);
begin
  inherited;
  UpdateCharacterGrid;
end;

function TfrmDebug.frmDebugStatus: TForm;
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
  context := km_kbp_state_context(state);

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
      dk := deadkeys.GetFromPosition(i-1);
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
var
  modifier: uint16_t;
  action: pkm_kbp_action_item;
  debug: pkm_kbp_state_debug_item;
  action_index: Integer;
begin
  Assert(Assigned(state));
  modifier := 0;
  if GetKeyState(VK_LCONTROL) < 0 then modifier := modifier or KM_KBP_MODIFIER_LCTRL or KM_KBP_MODIFIER_CTRL;
  if GetKeyState(VK_RCONTROL) < 0 then modifier := modifier or KM_KBP_MODIFIER_RCTRL or KM_KBP_MODIFIER_CTRL;
  if GetKeyState(VK_LMENU) < 0 then modifier := modifier or KM_KBP_MODIFIER_LALT or KM_KBP_MODIFIER_ALT;
  if GetKeyState(VK_RMENU) < 0 then modifier := modifier or KM_KBP_MODIFIER_RALT or KM_KBP_MODIFIER_ALT;
  if GetKeyState(VK_SHIFT) < 0 then modifier := modifier or KM_KBP_MODIFIER_SHIFT;
  if (GetKeyState(VK_CAPITAL) and 1) = 1 then modifier := modifier or KM_KBP_MODIFIER_CAPS;

  if not SetKeyEventContext then
    Exit(False);

  FEvents.Clear;

  if km_kbp_process_event(state, Message.WParam, modifier, 1) = KM_KBP_STATUS_OK then
  begin
    // Process keystroke
    Result := True;

    debug := km_kbp_state_debug_items(state, nil);
    action := km_kbp_state_action_items(state, nil);
    action_index := 0;
    while debug._type <> KM_KBP_DEBUG_END do
    begin
      if debug.kmx_info.first_action > action_index then
      begin
        while (action._type <> KM_KBP_IT_END) and (action_index < debug.kmx_info.first_action) do
        begin
          Result := Result and ProcessActionItem(Message.WParam, action);
          Inc(action);
          Inc(action_index);
        end;
      end;
      ProcessDebugItem(debug);
      Inc(debug);
    end;

    ProcessDebugItem(debug);

    if action._type = KM_KBP_IT_INVALIDATE_CONTEXT then
    begin
      // We may receive a context invalidation but we can ignore it
      Inc(action);
    end;

    // By the time we get to the end of rule processing, all actions should have
    // already been undertaken
    Assert(action._type = KM_KBP_IT_END);

    if FSingleStepMode then
    begin
      UIStatus := duiDebugging;
      FCurrentEvent := 1;
      (frmDebugStatus as TfrmDebugStatus).Events.SetEvents(FEvents);
      ExecuteEvent(0);
    end
    else
    begin
      FRunning := True;
      UIStatus := duiDebugging;
      FCurrentEvent := 0;
      Run;
    end;
  end
  else
    Result := False;
end;

procedure TfrmDebug.ProcessDebugItem(debug: pkm_kbp_state_debug_item);
var
  ev: TDebugEvent;
  rule: PKeymanKey;
  group: PKeymanGroup;
begin
  if not Assigned(debugkeyboard) then Exit;

  ev := TDebugEvent.Create;
  ev.EventType := etRuleMatch;
  ev.Rule.ItemType := debug._type;
  ev.Rule.Flags := debug.flags; // TODO: are these flags right?

  if Assigned(debug.kmx_info.rule) then
  begin
    rule := PKeymanKey(debug.kmx_info.rule);
    ev.Rule.Line := rule.Line;
    ev.Rule.Rule.Key := rule.Key;
    ev.Rule.Rule.Line := rule.Line;
    ev.Rule.Rule.ShiftFlags := rule.ShiftFlags;
    ev.Rule.Rule.dpOutput := rule.dpOutput;
    ev.Rule.Rule.dpContext := rule.dpContext;
    ev.Rule.FillStoreList(debug, debugkeyboard.Memory.Memory);
  end
  else
    ev.Rule.Line := DiscoverRuleLine(debug);

  ev.Rule.Context := debug.kmx_info.Context;
  if Assigned(debug.kmx_info.group) then
  begin
    group := PKeymanGroup(debug.kmx_info.group);
    ev.Rule.Group.dpName := group.dpName;
    ev.Rule.Group.dpMatch := group.dpMatch;
    ev.Rule.Group.dpNoMatch := group.dpNoMatch;
    ev.Rule.Group.fUsingKeys := group.fUsingKeys;
  end;

  FEvents.Add(ev);

  { Update user interface }

  (*
  if ev.Rule.ItemType = KM_KBP_DEBUG_BEGIN then
  begin
    UIStatus := duiReceivingEvents;
    ev.Rule.Key := PAIDebugKeyInfo(di.Flags)^;
    if (ev.Rule.key.VirtualKey = VK_ESCAPE) and ((ev.Rule.key.ShiftFlags and K_SHIFTFLAG) <> 0) then Exit; // Shift+Esc
    (frmDebugStatus as TfrmDebugStatus).Key.ShowKey(@ev.Rule.Key);
    (frmDebugStatus as TfrmDebugStatus).RegTest.RegTestLogKey(@ev.Rule.Key);

    if not ev.Rule.Key.IsUp then
      AddDebug('KEYDOWN: Char: '+ ev.Rule.Key.Character+
        ' VK: '+IntToHex(ev.Rule.Key.VirtualKey,4)+
        ' Shift: '+IntToHex(ev.Rule.Key.ShiftFlags, 4)+
        ' DeadChar: '+ev.Rule.Key.DeadKeyCharacter);
  end;

  if ev.Rule.ItemType = QID_END then
    { Now we disable Keyman keyboard as we are at the end of the key processing sequence }
    PostMessage(Handle, WM_USER_DebugEnd, 0, 0);
  *)
end;

function TfrmDebug.ProcessActionItem(key: Word; action: pkm_kbp_action_item): Boolean;
begin
  Result := True;
  case action._type of
    KM_KBP_IT_CHAR:           Action_Char(action.character);
    KM_KBP_IT_MARKER:         Action_Marker(action.marker); //      = 2,  // Correlates to kmn's "deadkey" markers.
    KM_KBP_IT_ALERT:          ; //       = 3,  // The keyboard has triggered a alert/beep/bell.
    KM_KBP_IT_BACK:           Action_DeleteBack(action.backspace.expected_type, action.backspace.expected_value);
    KM_KBP_IT_PERSIST_OPT:    ; // = 5,  // The indicated option needs to be stored.
    KM_KBP_IT_EMIT_KEYSTROKE: Action_EmitKeystroke(key);
    else Assert(False, 'Action type '+IntToStr(Ord(action._type))+' is unexpected.');
  end;
end;

procedure TfrmDebug.Action_EmitKeystroke(const key: Word);
var
  event: TDebugEvent;
begin
  case key of
    VK_TAB: Action_Char(9);
    VK_RETURN: Action_Char(13);
    VK_BACK:   Action_DeleteBack(Ord(KM_KBP_BT_UNKNOWN), 0);
    else
    begin
      // TODO: VSHIFTDOWN, VSHIFTUP
      event := TDebugEvent.Create;
      event.EventType := etAction;
      event.Action.ActionType := QIT_VKEYDOWN;
      event.Action.dwData := key;
      FEvents.Add(event);
      event := TDebugEvent.Create;
      event.EventType := etAction;
      event.Action.ActionType := QIT_VKEYUP;
      event.Action.dwData := key;
      FEvents.Add(event);
    end;
    //else Assert(False);
  end;
end;

///
/// Insert a UTF-32 character at the insertion point.
/// TODO: define behaviour around selection
///
procedure TfrmDebug.Action_Char(const character: km_kbp_usv);
var
  event: TDebugEvent;
begin
  event := TDebugEvent.Create;
  event.EventType := etAction;
  event.Action.ActionType := QIT_CHAR;
  event.Action.Text := Uni_UTF32CharToUTF16(character);
  FEvents.Add(event);
end;

///
/// Delete the codepoint preceding the insertion point.
/// TODO: define behaviour around selection
///
procedure TfrmDebug.Action_DeleteBack(
  expected_type: uint8_t;            /// one of KM_KBP_IT_CHAR, KM_KBP_IT_MARKER, KM_KBP_IT_END (when type to delete is unknown)
  expected_value: uintptr_t         /// used mainly in unit tests
);
var
  event: TDebugEvent;
begin
  event := TDebugEvent.Create;
  event.EventType := etAction;
  event.Action.ActionType := QIT_BACK;
  event.Action.dwData := expected_type;
  FEvents.Add(event);
end;

///
///
procedure TfrmDebug.Action_Marker(marker: uintptr_t);
var
  event: TDebugEvent;
begin
  // kmx requires that markers are between 1 and $FFFF
  // ($FFFD in practical terms)
  Assert((marker > 0) and (marker <= High(WORD)));

  event := TDebugEvent.Create;
  event.EventType := etAction;
  event.Action.ActionType := QIT_DEADKEY;
  event.Action.dwData := marker;
  FEvents.Add(event);
end;


procedure TfrmDebug.StepForward;
begin
  if UIStatus = duiTest then Exit;

  if FCurrentEvent < FEvents.Count then
  begin
    while (FCurrentEvent < FEvents.Count) and (FEvents[FCurrentEvent].EventType = etAction) do
    begin
      ExecuteEvent(FCurrentEvent);
      Inc(FCurrentEvent);
    end;
    if FCurrentEvent < FEVents.Count then
    begin
      ExecuteEvent(FCurrentEvent);
      Inc(FCurrentEvent);
    end;
  end;

  if FCurrentEvent = FEvents.Count then
  begin
    if memo.Focused
      then UIStatus := duiFocusedForInput
      else UIStatus := duiReadyForInput;

    (frmDebugStatus as TfrmDebugStatus).RegTest.RegTestLogContext;
    (frmDebugStatus as TfrmDebugStatus).RegTest.RegTestNextKey;
  end;
end;

procedure TfrmDebug.Run;
begin
  if UIStatus = duiTest then Exit;

  FFoundBreakpoint := False;
  FRunning := True;
  try
    while (FCurrentEvent < FEvents.Count) do
    begin
      ExecuteEvent(FCurrentEvent);
      Inc(FCurrentEvent);
      if FFoundBreakpoint then
      begin
        FRunning := False;
        ExecutionPointLine := ExecutionPointLine;

        if FCurrentEvent > 0
          then (frmDebugStatus as TfrmDebugStatus).Elements.UpdateStores(FEvents[FCurrentEvent-1])
          else (frmDebugStatus as TfrmDebugStatus).Elements.UpdateStores(nil);

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

  (frmDebugStatus as TfrmDebugStatus).RegTest.RegTestLogContext;
  (frmDebugStatus as TfrmDebugStatus).RegTest.RegTestNextKey;
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
var
  s: string;
begin
  with FEvents[n].Rule do
  begin
    if (Flags and KM_KBP_DEBUG_FLAG_NOMATCH) = KM_KBP_DEBUG_FLAG_NOMATCH then s := ' NoMatch in Group' else s := '';
    s := Format('RULEMATCH %s%s', [qid[ItemType],  s]);
    if Line > 0 then
    begin
      s := s + Format(' Line:%d', [Line]);
      ExecutionPointLine := Line - 1;
      if not FRunning
        then (frmDebugStatus as TfrmDebugStatus).Elements.UpdateStores(FEvents[n])
        else (frmDebugStatus as TfrmDebugStatus).Elements.UpdateStores(nil);
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
//    AddDEBUG(s);

    { Update call stack, execution point line }

    case ItemType of
      KM_KBP_DEBUG_BEGIN,
      KM_KBP_DEBUG_GROUP_ENTER,
      KM_KBP_DEBUG_RULE_ENTER,
      KM_KBP_DEBUG_NOMATCH_ENTER,
      KM_KBP_DEBUG_MATCH_ENTER:
        (frmDebugStatus as TfrmDebugStatus).CallStack.CallStackPush(FEvents[n].Rule);
      KM_KBP_DEBUG_RULE_EXIT,
      KM_KBP_DEBUG_NOMATCH_EXIT,
      KM_KBP_DEBUG_MATCH_EXIT,
      KM_KBP_DEBUG_GROUP_EXIT:
        (frmDebugStatus as TfrmDebugStatus).CallStack.CallStackPop;
      KM_KBP_DEBUG_END:
        begin (frmDebugStatus as TfrmDebugStatus).CallStack.CallStackClear; ExecutionPointLine := -1; end;
    else
      Assert(False);
    end;
  end;
end;


procedure TfrmDebug.ExecuteEventAction(n: Integer);
  procedure Backspace(BackspaceType: km_kbp_backspace_type);
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
          dk := deadkeys.GetFromPosition(m-1);
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
            dk := deadkeys.GetFromPosition(m-1);
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
            dk := deadkeys.GetFromPosition(m-1);
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

var
  i: Integer;
  msg: TMessage;
begin
  DisableUI;
  ClearKeyStack;
  (frmDebugStatus as TfrmDebugStatus).Elements.UpdateStores(nil);
  with FEvents[n].Action do
  begin
    case ActionType of
      QIT_VKEYDOWN:   if (LOBYTE(dwData) < VK_F1) or (LOBYTE(dwData) > VK_F12) then
                      begin
                        i := KEYFLAG_KEYMAN;
                        if (dwData and $100) = $100 then
                          // Extended Key State
                          i := i or KEYFLAG_EXTENDED;

                        if GetKeyState(VK_MENU) < 0
                          then PostMessage(memo.Handle, WM_SYSKEYDOWN, LOBYTE(dwData), i)
                          else PostMessage(memo.Handle, WM_KEYDOWN, LOBYTE(dwData), i);
                      end;
{     QIT_VKEYUP:     if GetKeyState(VK_MENU) < 0
                        then PostMessage(memo.Handle, WM_SYSKEYUP, dwData, 0)
                        else PostMessage(memo.Handle, WM_KEYUP, dwData, 0);}
      QIT_VSHIFTDOWN: begin FSaveShiftState := SaveShiftState; SetShiftState(memo.Handle, dwData); end;
      QIT_VSHIFTUP:   SetShiftState(memo.Handle, FSaveShiftState); //ClearShiftState(memo.Handle, dwData);
      QIT_CHAR:       for i := 1 to Length(Text) do
                      begin
                        ClearKeyStack;
                        if FANSITest
                          then PostMessageA(memo.Handle, WM_CHAR, Ord(AnsiChar(Text[i])), 0) //TODO: ANSI support
                          else
                          begin
                            msg.Msg := WM_CHAR;
                            msg.WParam := Ord(Text[i]);
                            msg.LParam := 0;
                            memo.Dispatch(msg);
                          end;
//                          else PostMessageW(memo.Handle, WM_CHAR, Ord(Text[i]), 0);  // I3310
//                        Application.ProcessMessages;
                      end;
      QIT_DEADKEY:    AddDeadkey(dwData);
      QIT_BELL:       MessageBeep(0);
      QIT_BACK:       Backspace(km_kbp_backspace_type(dwData));
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
//    UpdateControlCaptions;
  end;
end;

procedure TfrmDebug.SetForceKeyboard(Value: Boolean);
var
  hkl: THandle;
  status: km_kbp_status;
begin
  if Value <> FForceKeyboard then
  begin
    FForceKeyboard := Value;

    if FForceKeyboard then
    begin
      try
        if SystemParametersInfo(SPI_GETDEFAULTINPUTLANG, 0, @hkl, 0) then
        begin
          FLastActiveProfile.Profile.dwProfileType := TF_PROFILETYPE_KEYBOARDLAYOUT;
          FLastActiveProfile.Profile.langid := HKLToLanguageID(hkl);
          FLastActiveProfile.Profile.HKL := hkl;
        end
        else
          TDebugUtils.GetActiveTSFProfile(FLastActiveProfile);   // I4331

        memo.SetFocus;

        keyboard := nil;
        state := nil;

        status := km_kbp_keyboard_load(PChar(FFileName), keyboard);
        if status <> KM_KBP_STATUS_OK then
          raise Exception.CreateFmt('Unable to start debugger -- keyboard load failed with error %x', [Ord(status)]);

        status := km_kbp_state_create(keyboard, @KM_KBP_OPTIONS_END, state);
        if status <> KM_KBP_STATUS_OK then
          raise Exception.CreateFmt('Unable to start debugger -- state creation failed with error %x', [Ord(status)]);

        status := km_kbp_state_debug_set(state, 1);
        if status <> KM_KBP_STATUS_OK then
          raise Exception.CreateFmt('Unable to start debugger -- enabling debug failed with error %x', [Ord(status)]);
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
      TDebugUtils.SetActiveTSFProfile(FLastActiveProfile);   // I4331
      CleanupCoreState;
    end;
  end;
end;

procedure TfrmDebug.CleanupCoreState;
begin
  if state <> nil then
    km_kbp_state_dispose(state);
  state := nil;
  if keyboard <> nil then
    km_kbp_keyboard_dispose(keyboard);
  keyboard := nil;
end;

function TfrmDebug.DiscoverRuleLine(debug: pkm_kbp_state_debug_item): Integer;
var
  grp, i: Integer;
  group: PKeymanGroup;
begin
  grp := -1;
  Result := 0;

  if Assigned(debug.kmx_info.group) then
  begin
    group := PKeymanGroup(debug.kmx_info.group);
    for i := 0 to debugkeyboard.Groups.Count - 1 do
      if group.dpName = debugkeyboard.Groups[i].Name then
      begin
        grp := i;
        Break;
      end;
  end;

  case debug._type of
    KM_KBP_DEBUG_END:           Result := debugkeyboard.BeginUnicodeLine;
    KM_KBP_DEBUG_BEGIN:         Result := debugkeyboard.BeginUnicodeLine;
    KM_KBP_DEBUG_GROUP_ENTER,
    KM_KBP_DEBUG_GROUP_EXIT:    if grp > -1 then Result := debugkeyboard.Groups[grp].Line;
    KM_KBP_DEBUG_MATCH_ENTER,
    KM_KBP_DEBUG_MATCH_EXIT:    if grp > -1 then Result := debugkeyboard.Groups[grp].MatchLine;
    KM_KBP_DEBUG_NOMATCH_ENTER,
    KM_KBP_DEBUG_NOMATCH_EXIT:  if grp > -1 then Result := debugkeyboard.Groups[grp].NomatchLine;
  end;
end;

procedure TfrmDebug.ResetEvents;
begin
  if FCurrentEvent > 0 then
  begin
    FCurrentEvent := 0;
    FEvents.Clear;
  end;
end;

procedure TfrmDebug.WMUserDebugEnd(var Message: TMessage);
begin
  if UIStatus = duiPaused then
  begin
    FCurrentEvent := 1;
    ResetEvents;
    (frmDebugStatus as TfrmDebugStatus).Key.ShowKey(nil);
    Exit;
  end;

  if FEvents.Count > 0 then
  begin
    if FSingleStepMode then
    begin
      UIStatus := duiDebugging;
      FCurrentEvent := 1;
      ExecuteEvent(0);
    end
    else
    begin
      FRunning := True;
      UIStatus := duiDebugging;
      FCurrentEvent := 0;
      Run;
    end;
  end
  else
    if memo.Focused
      then UIStatus := duiFocusedForInput
      else UIStatus := duiReadyForInput;
end;

{-------------------------------------------------------------------------------
 - Preliminary debug stuff only                                                -
 ------------------------------------------------------------------------------}

// QueueDebugInformation ItemTypes

function HexString(s: WideString): string;
var
  i: Integer;
begin
  Result := '';
  for i := 1 to Length(s) do
    Result := Result + 'U+'+IntToHex(Ord(s[i]), 4)+' ';
  Result := Trim(Result);
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

procedure TfrmDebug.ListBreakpoints;
var
  b: TDebugBreakpoint;
begin
  memo.Clear;
  for b in FBreakpoints do
    if Assigned(FOnSetBreakpoint) then
      FOnSetBreakpoint(Self, b.TrueLineNumber);
end;


procedure TfrmDebug.HideDebugForm;
begin
  (frmDebugStatus as TfrmDebugStatus).DebugForm := nil;
  frmKeymanDeveloper.ShowDebug(False);   // I4796
  FDebugVisible := False;
  ResetDebug;
  UIStatus := duiReadyForInput;
end;

procedure TfrmDebug.ShowDebugForm;
begin
  frmKeymanDeveloper.ShowDebug(True);   // I4796

  FDebugVisible := True;
//  FFileName := ChangeFileExt(DebugFileName, '.kmx');

  //UpdateFont(nil);
  SetupDebug;

  //Keyman_Initialise(Application.MainFormHandle, True);  // I2801  // I3283 - remove   // I3503
  memo.SetFocus;
  (frmDebugStatus as TfrmDebugStatus).DebugForm := Self;
end;

procedure TfrmDebug.ResetDebug;
begin
  //SelectSystemLayout(False);
  ForceKeyboard := False;
  debugkeyboard.Free;
  debugkeyboard := nil;
  if (frmDebugStatus <> nil) and Assigned((frmDebugStatus as TfrmDebugStatus).RegTest) then   // I2594
    (frmDebugStatus as TfrmDebugStatus).RegTest.SetDebugKeyboard(nil);
  FCurrentEvent := 1;
  ResetEvents;
  if (frmDebugStatus <> nil) and Assigned((frmDebugStatus as TfrmDebugStatus).Key) then   // I2594
    (frmDebugStatus as TfrmDebugStatus).Key.ShowKey(nil);
  ExecutionPointLine := -1;
  ClearDeadkeys;  // I1699
  CleanupCoreState;
end;

procedure TfrmDebug.SetupDebug;
var
  buf: array[0..KL_NAMELENGTH] of Char;
begin
  if UIStatus <> duiTest then
  begin
    GetKeyboardLayoutName(buf);

    debugkeyboard := TDebugKeyboard.Create(FFileName);
    (frmDebugStatus as TfrmDebugStatus).RegTest.RegTestSetup(buf, FFileName, FANSITest);   // I3655
    (frmDebugStatus as TfrmDebugStatus).RegTest.SetDebugKeyboard(debugkeyboard);
    (frmDebugStatus as TfrmDebugStatus).Elements.ClearStores;
    UpdateANSITest;
  end
  else ResetDebug;
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
  (frmDebugStatus as TfrmDebugStatus).DisplayFont := FFont;
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
          (frmDebugStatus as TfrmDebugStatus).Elements.UpdateStores(nil);
          (frmDebugStatus as TfrmDebugStatus).Key.ShowKey(nil);
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
              (frmDebugStatus as TfrmDebugStatus).Elements.UpdateStores(nil);
              (frmDebugStatus as TfrmDebugStatus).Key.ShowKey(nil);
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
  if FCurrentEvent >= 0
    then Result := FEvents[FCurrentEvent]
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
  for i := deadkeys.Count - 1 downto 0 do
    if deadkeys[i].Deleted then
    begin
      ClearDeadkeyStyle;
      deadkeys.Delete(i);
      Found := True;
    end;
  if Found then UpdateDeadkeyDisplay;
end;

procedure TfrmDebug.UpdateDebugStatusForm;   // I4809
begin
  if (frmDebugStatus <> nil) then
    (frmDebugStatus as TfrmDebugStatus).Key.UIStatus := FUIStatus;
end;

procedure TfrmDebug.UpdateDeadkeyDisplay;
begin
  if not (csDestroying in ComponentState) then
    (frmDebugStatus as TfrmDebugStatus).DeadKeys.UpdateDeadKeyDisplay(deadkeys);
  UpdateCharacterGrid;   // I4808
end;

procedure TfrmDebug.InitDeadkeys;
begin
  deadkeys := TDebugDeadkeyInfoList.Create;
end;

procedure TfrmDebug.ClearDeadkeys;
begin
  ClearDeadkeyStyle;
  deadkeys.Clear;
  UpdateDeadkeyDisplay;
end;

procedure TfrmDebug.UninitDeadkeys;
begin
  ClearDeadkeys;
  FreeAndNil(deadkeys);
end;

procedure TfrmDebug.AddDeadkey(dkCode: Integer);
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
    deadkeys.Add(dk);
    //PostMessage(memo.Handle, WM_UNICHAR, $FFFC, 0); Application.ProcessMessages;
    UpdateDeadkeyDisplay;
  end;
end;

procedure TfrmDebug.FillDeadkeys(startpos: Integer; var s: WideString);
var
  i: Integer;
  dk: TDeadKeyInfo;
begin
//  Dec(startpos);
  i := 1;
  while i <= Length(s) do
  begin
    if s[i] = #$FFFC then
    begin
      for dk in deadkeys do
      begin
        if dk.Position = startpos then
        begin
          s[i] := WChr(UC_SENTINEL);
          s := Copy(s, 1, i) + WChr(CODE_DEADKEY) + WChr(dk.Deadkey.Value+1) + Copy(s, i+1, Length(s));
          Inc(i, 2);
          Break;
        end;
      end;
    end;
    Inc(startpos);
    Inc(i);
  end;
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
      for K := 0 to deadkeys.Count-1 do
        if deadkeys[K].Position = I-1 then
        begin
          sgChars.Cells[J, 0] := deadkeys[K].Deadkey.Name;
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

{-------------------------------------------------------------------------------
 - ANSI vs Unicode tests                                                       -
 ------------------------------------------------------------------------------}

procedure TfrmDebug.UpdateANSITest;
begin
  FANSITest := debugkeyboard.BeginUnicodeLine < 0;
end;

function TfrmDebug.CanChangeANSITest: Boolean;
begin
  Result := Assigned(debugkeyboard) and (debugkeyboard.BeginUnicodeLine >= 0) and (debugkeyboard.BeginANSILine >= 0);
end;

procedure TfrmDebug.SetANSITest(const Value: Boolean);
begin
  FANSITest := Value;
end;

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

