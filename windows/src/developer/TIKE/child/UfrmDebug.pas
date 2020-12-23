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
  keymanapi_TLB,
  KeymanDeveloperDebuggerMemo,
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
  private

    FDebugVisible: Boolean;
    FBreakpoints: TDebugBreakpoints;
    FRunning, FFoundBreakpoint, FForceKeyboard: Boolean;
    FFileName: string;
    //FEditor: TfrmTikeEditor;
    FExecutionPointLine: Integer;
    debugkeyboard: TDebugKeyboard;
    FCurrentEvent: Integer;
    FIgnoreKeyUp, FDefaultFont: Boolean;
    FEvents: TDebugEventList;
    FUIStatus: TDebugUIStatus;
    FUIDisabled: Boolean;
    LastSelStart: Integer;

    { Control caption member variables }
    //FControlCaptions: TStringList;

    { Deadkey member variables }
    deadkeys: TList;
    FSelectedDeadkey: TDeadKeyInfo;
    //hklSystemKeyboard: HKL;
    FSaveShiftState: Integer;
    FLastActiveProfile: TDebugUtilProfile;   // I4331

    { Editor integration functions }
//    function EditorMemo: TPlusMemoU;

    { Keyman32 integration functions }
    function frmDebugStatus: TForm;

    function DiscoverRuleLine(ItemType: Integer; di: PAIDebugInfo): Integer;
    procedure ResetEvents;
    procedure ExecuteEvent(n: Integer);
    procedure WMUserDebugEnd(var Message: TMessage); message WM_USER_DebugEnd;
    procedure ExecuteEventAction(n: Integer);
    procedure ExecuteEventRule(n: Integer);
    procedure SetExecutionPointLine(ALine: Integer);
    procedure AddDebugAction(ItemType, dwData: Integer);
    procedure AddRuleMatch(ItemType: Integer; di: PAIDebugInfo);
    procedure SetUIStatus(const Value: TDebugUIStatus);
    procedure DisableUI;
    procedure EnableUI;
    procedure SetForceKeyboard(Value: Boolean);
    function GetStatusText: string;
    procedure SetStatusText(Value: string);
    procedure UpdateDebugStatusForm;   // I4809

    procedure KeymanGetContext(var Message: TMessage);

    procedure AddDebug(s: WideString);

    { Debug panel functions }
    procedure AddDeadkey(dkCode: Integer);

  { ANSI Test}
  private
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
    procedure WMUSERUpdateForceKeyboard(var Message: TMessage); message WM_USER_UpdateForceKeyboard;   // I4767
    procedure UpdateCharacterGrid;   // I4808

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
  keyman32_int,
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

{-------------------------------------------------------------------------------
 - Form events                                                                 -
 ------------------------------------------------------------------------------}

procedure TfrmDebug.FormCreate(Sender: TObject);
begin
  inherited;
//  InitMSCTF;   // I3655

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
  FBreakpoints.Free;
  FEvents.Free;
  //UninitControlCaptions;
  //UninitSystemKeyboard;
  UninitDeadkeys;
end;

function TfrmDebug.frmDebugStatus: TForm;
begin
  Result := (Owner as TfrmKeymanWizard).DebugStatusForm;
end;

{-------------------------------------------------------------------------------
 - Memo management                                                             -
 ------------------------------------------------------------------------------}

procedure TfrmDebug.KeymanGetContext(var Message: TMessage);
var
  selstart, selend: Integer;
  i: Integer;
  s: string;
  ws: WideString;
begin
  selstart := memo.SelStart;
  selend := selstart + memo.SelLength;
  if selend < selstart then
  begin
    //n := selend;
    selstart := selend;
    //selend := n;
  end;
  if selstart = 0 then
    Message.Result := 0
  else
  begin
    if selstart < Integer(Message.wParam) then
    begin
      ws := Copy(memo.Text, 1, selstart);
      FillDeadkeys(0, ws);
    end
    else
    begin
      ws := Copy(memo.Text, selstart - Integer(Message.wParam) + 2, selstart - 1);
      FillDeadKeys(selstart - Integer(Message.wParam) + 1, ws);  // 26-09-2002: Fixed bug reported by Karljurgen
    end;

    if FANSITest then
    begin
      // zero extend ANSI string -- not convert to Unicode
      s := ws;  // ws := '';  // Fix bug reported by D.Thormoset, 12/8/02... BAD MARC -- should have waited until post release
      for i := 1 to Length(s) do
          // START Fix bug reported by D.Thormoset, 12/8/02... BAD MARC -- should have waited until post release
        if ws[i] <> WideChar(UC_SENTINEL) then
          // END Fix bug reported by D.Thormoset, 12/8/02... BAD MARC -- should have waited until post release
          ws[i] := WideChar(Ord(s[i]));
    end;

    //if not FIgnoreKeyUp then AddDebug('ws(NOT KeyUp pre): '+ws)
    //else AddDebug('ws(KeyUp pre): '+ws);

    // Need to update the buffer with the contents of the current debug messages
    //if  = 0 then
      for i := FCurrentEvent to FEvents.Count - 1 do    { was i := 0 to FEvents.Count - 1 }
        if FEvents[i].EventType = etAction then
          case FEvents[i].Action.ActionType of
            QIT_CHAR:       ws := ws + FEvents[i].Action.Text;
            QIT_DEADKEY:    ws := ws + WChr(UC_SENTINEL) + WChr(CODE_DEADKEY) + WChr(FEvents[i].Action.dwData);
            QIT_BACK:       Delete(ws, Length(ws) - Length(FEvents[i].Action.Text) + 1, 1000);
          end;

    while Length(ws) > Integer(Message.wParam) - 1 do
      // After FillDeadKeys, can be too many chrs in buffer.  So delete until we have less than MAXCONTEXT
      if ws[1] = WChr(UC_SENTINEL) then Delete(ws, 1, 3) else Delete(ws, 1, 1);

    lstrcpyW(PWideChar(Message.lParam), PWideChar(ws));
    if not FIgnoreKeyUp then AddDebug('KeymanGetContext (KeyDown): Context="'+ws+'"')
    else AddDebug('KeymanGetContext (KeyUp): Context="'+ws+'"');
    Message.Result := 1;
  end;
end;

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

procedure TfrmDebug.memoMessage(Sender: TObject; var Message: TMessage;
  var Handled: Boolean);
begin
  if UIStatus = duiClosing then Exit;  // Don't process while destroying...

  Handled := True;
  if (Message.Msg = WM_SYSCHAR) and FUIDisabled then
    Exit
  else if (Message.Msg = WM_KEYDOWN) and
      (Message.wParam = VK_ESCAPE) and
      (GetKeyState(VK_SHIFT) < 0) and
      (UIStatus <> duiPaused) then   // I4033
    UIStatus := duiPaused
  else if (Message.Msg = WM_KEYDOWN) and
    (Message.wParam = VK_F6) and
    (GetKeyState(VK_CONTROL) >= 0) and
    (GetKeyState(VK_MENU) >= 0) and
    (UIStatus <> duiFocusedForInput) then
  begin
    EditorMemo.SetFocus;
  end
  else if (Message.Msg = WM_KEYDOWN) and
      (Message.wParam = VK_ESCAPE) and
      (GetKeyState(VK_SHIFT) < 0) and
      (UIStatus = duiPaused) then
    UIStatus := duiFocusedForInput
  else if Message.Msg = WM_KEYDOWN then
  begin
    Handled := False;
    Exit;    // Stop adding rules to be executed if paused
  end
  else if not (UIStatus in [duiFocusedForInput, duiReceivingEvents, duiReadyForInput]) then
  begin
    Handled := False;
    Exit;    // Stop adding rules to be executed if paused
  end
  else if Message.Msg = WM_KEYMANDEBUG_CANDEBUG then
  begin
//    AddDebug('WM_KEYMANDEBUG_CANDEBUG');
    Message.Result := 1
  end
  else if Message.Msg = WM_KEYMANDEBUG_GETUNICODESTATUS then
  begin
//    AddDebug('WM_KEYMANDEBUG_GETUNICODESTATUS');
    if FANSITest then Message.Result := 0 else Message.Result := 1;
  end
  else if Message.Msg = WM_KEYMANDEBUG_GETCONTEXT then
  begin
    AddDebug('WM_KEYMANDEBUG_GETCONTEXT');
    KeymanGetContext(Message);
  end
  else if Message.Msg = WM_KEYMANDEBUG_ACTION then
  begin
//    AddDebug('WM_KEYMANDEBUG_ACTION');
    AddDebugAction(Message.wParam, Message.lParam)
  end
  else if Message.Msg = WM_KEYMANDEBUG_RULEMATCH then
  begin
//    AddDebug('WM_KEYMANDEBUG_RULEMATCH');
    AddRuleMatch(Message.wParam, PAIDebugInfo(Message.lParam))
  end
  else
    Handled := False;
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
    ExecuteEvent(FCurrentEvent);
    Inc(FCurrentEvent);
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
    if (Flags and QID_FLAG_NOMATCH) = QID_FLAG_NOMATCH then s := ' NoMatch in Group' else s := '';
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
        if not (ItemType in [QID_GROUP_EXIT, QID_RULE_EXIT, QID_MATCH_EXIT, QID_NOMATCH_EXIT, QID_END]) or
            FKeymanDeveloperOptions.DebuggerBreakWhenExitingLine then
          FFoundBreakpoint := True;
      end;
    end;
//    AddDEBUG(s);

    { Update call stack, execution point line }

    case ItemType of
      QID_BEGIN_UNICODE, QID_BEGIN_ANSI, QID_GROUP_ENTER,
      QID_RULE_ENTER, QID_NOMATCH_ENTER, QID_MATCH_ENTER:
        (frmDebugStatus as TfrmDebugStatus).CallStack.CallStackPush(FEvents[n].Rule);
      QID_RULE_EXIT, QID_NOMATCH_EXIT, QID_MATCH_EXIT, QID_GROUP_EXIT:
        (frmDebugStatus as TfrmDebugStatus).CallStack.CallStackPop;
      QID_END:
        begin (frmDebugStatus as TfrmDebugStatus).CallStack.CallStackClear; ExecutionPointLine := -1; end;
    end;
  end;
end;

procedure TfrmDebug.ExecuteEventAction(n: Integer);
  procedure Backspace(BackspaceType: Integer);
  var
    i, m, n: Integer;
  begin
    n := memo.SelStart;
    m := n - 1;

    if (n > 1) and Uni_IsSurrogate2(memo.Text[n]) then
      Dec(m);

    for i := 0 to deadkeys.Count-1 do
      if (TDeadKeyInfo(deadkeys[i]).Position >= m-1) and
        (TDeadKeyInfo(deadkeys[i]).Position < n-1) then
        TDeadKeyInfo(deadkeys[i]).Delete;

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
begin
  DisableUI;
  ClearKeyStack;
  (frmDebugStatus as TfrmDebugStatus).Elements.UpdateStores(nil);
  with FEvents[n].Action do
  begin
    case ActionType of
      QIT_VKEYDOWN:   if (LOBYTE(dwData) < VK_F1) or (LOBYTE(dwData) > VK_F12) then
                      begin
                        if (dwData and $100) = $100 then i := $01000000 else i := 0;
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
                          else PostMessageW(memo.Handle, WM_CHAR, Ord(Text[i]), 0);  // I3310
                        Application.ProcessMessages;
                      end;
      QIT_DEADKEY:    AddDeadkey(dwData);
      QIT_BELL:       MessageBeep(0);
      QIT_BACK:       for i := 1 to Length(Text) do Backspace(Ord(Text[i])-1);
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
begin
  if Value <> FForceKeyboard then
  begin
    FForceKeyboard := Value;
    PostMessage(Handle, WM_USER_UpdateForceKeyboard, WPARAM(FForceKeyboard), 0);   // I4767
  end;
end;

procedure TfrmDebug.WMUSERUpdateForceKeyboard(var Message: TMessage);   // I4767
    procedure FailTidyUp;
    begin
      Winapi.Windows.SetFocus(0);
      HideDebugForm;
      FForceKeyboard := False;
    end;

var
  FLastError: Cardinal;
  FWasStarted: Boolean;
  FDebugHostKeyboard: IKeymanKeyboardInstalled;
  hr: HRESULT;
  hkl: THandle;
begin
  if {memo.Focused and} Message.WParam <> 0 then
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
      FDebugHostKeyboard := TDebugUtils.GetDebugHostKeyboard;   // I3655
      if FDebugHostKeyboard = nil then
      begin
        FailTidyUp;
        ShowMessage('Unable to start debugging -- the debug host keyboard is not installed.');
        Exit;
      end;

      if not StartKeymanDesktopPro(FWasStarted) then  // I3283   // I3503
      begin
        FLastError := GetLastError;
        FailTidyUp;
        ShowMessage('Unable to start Keyman for debugging - please make sure that Keyman is correctly installed (the error code was '+IntToHex(FLastError, 8)+').');  // I3173   // I3504
        Exit;
      end;
      if FWasStarted then  // I3283   // I3503
      begin
        // Give Keyman Engine a chance to correctly attach focus after it starts
        Winapi.Windows.SetFocus(0);
        Winapi.Windows.SetFocus(memo.Handle);
        Exit;
      end;

      hr := TDebugUtils.SelectTSFProfileForKeyboardLanguage(FDebugHostKeyboard.Languages[0]);   // I3655   // I4020
      if FAILED(hr) then
      begin
        FailTidyUp;
        ShowMessage(Format('Unable to start debugging -- failed to switch to language with error %x', [hr]));
        Exit;
      end;

      //kmcom.Control.ActiveKeyboard := FDebugHostKeyboard;

      if not Keyman_ForceKeyboard(FFileName) then
      begin
        FLastError := GetLastError;  // I3173   // I3504
        {Keyman_Exit; I3283   // I3503
        if not Keyman_Initialise(Application.MainForm.Handle, True) or not Keyman_ForceKeyboard(FFileName) then
        begin}
          Winapi.Windows.SetFocus(0);
          HideDebugForm;
          ShowMessage('Unable to start debugger - please make sure that Keyman Developer is correctly installed (the error code was '+IntToHex(FLastError, 8)+').');  // I3173   // I3504
          //(Editor as TfrmEditor).HideDebugForm;
          FForceKeyboard := False;
          Exit;
        {end;}
        end;
    except
      on E:Exception do
      begin
        ShowMessage(E.Message);
        FForceKeyboard := False;
        Exit;
      end;
{        on E:EKeymanNotInstalled do
      begin
        FForceKeyboard := False;
        ShowMessage(E.Message);
        Exit;
      end;}
    end;
  end
  else
  begin
    if not Keyman_StopForcingKeyboard then
    begin
      if GetLastError <> $20000009 then  // I3283 - ignore unload errors that may arise when Keyman Engine is starting   // I3503
        ShowMessage('The keyboard failed to unload with the error '+IntToHex(GetLastError,8)+'.');  // I3173   // I3504
    end;
    TDebugUtils.SetActiveTSFProfile(FLastActiveProfile);   // I4331
  end;
end;

function TfrmDebug.DiscoverRuleLine(ItemType: Integer; di: PAIDebugInfo): Integer;
var
  grp, i: Integer;
begin
  grp := -1;
  Result := 0;

  if Assigned(di.Group) then
    for i := 0 to debugkeyboard.Groups.Count - 1 do
      if di.Group.dpName = debugkeyboard.Groups[i].Name then
      begin
        grp := i;
        Break;
      end;

  case ItemType of
    QID_END:           Result := debugkeyboard.BeginUnicodeLine;
    QID_BEGIN_UNICODE: Result := debugkeyboard.BeginUnicodeLine;
    QID_BEGIN_ANSI:    Result := debugkeyboard.BeginANSILine;
    QID_GROUP_ENTER,
    QID_GROUP_EXIT:    if grp > -1 then Result := debugkeyboard.Groups[grp].Line;
    QID_MATCH_ENTER,
    QID_MATCH_EXIT:    if grp > -1 then Result := debugkeyboard.Groups[grp].MatchLine;
    QID_NOMATCH_ENTER,
    QID_NOMATCH_EXIT:  if grp > -1 then Result := debugkeyboard.Groups[grp].NomatchLine;
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

procedure TfrmDebug.AddRuleMatch(ItemType: Integer; di: PAIDebugInfo);
var
  ev: TDebugEvent;
begin
  if not Assigned(debugkeyboard) then Exit;

  ResetEvents;

  if (ItemType = QID_BEGIN_UNICODE) or (ItemType = QID_BEGIN_ANSI) then
  begin
    FIgnoreKeyUp := PAIDebugKeyInfo(di.Flags).IsUp;
    (frmDebugStatus as TfrmDebugStatus).DeadKeys.DeselectDeadkeys;
  end;
  if FIgnoreKeyUp then Exit;

  ev := TDebugEvent.Create;
  FEvents.Add(ev);
  ev.EventType := etRuleMatch;
  ev.Rule.ItemType := ItemType;
  ev.Rule.Flags := di.Flags;

  if Assigned(di.Rule) then
  begin
    ev.Rule.Line := di.Rule.Line;
    ev.Rule.Rule.Key := di.Rule^.Key;
    ev.Rule.Rule.Line := di.Rule^.Line;
    ev.Rule.Rule.ShiftFlags := di.Rule^.ShiftFlags;
    ev.Rule.Rule.dpOutput := di.Rule^.dpOutput;
    ev.Rule.Rule.dpContext := di.Rule^.dpContext;
    ev.Rule.FillStoreList(di, debugkeyboard.Memory.Memory);
  end
  else
    ev.Rule.Line := DiscoverRuleLine(ItemType, di);

  if Assigned(di.Context) then ev.Rule.Context := di.Context;
  if Assigned(di.Output) then ev.Rule.Output := di.Output;
  if Assigned(di.Group) then
  begin
    ev.Rule.Group.dpName := di.Group^.dpName;
    ev.Rule.Group.dpMatch := di.Group^.dpMatch;
    ev.Rule.Group.dpNoMatch := di.Group^.dpNoMatch;
    ev.Rule.Group.fUsingKeys := di.Group^.fUsingKeys;
  end;

  { Update user interface }

  if (ev.Rule.ItemType = QID_BEGIN_UNICODE) or (ev.Rule.ItemType = QID_BEGIN_ANSI) then
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
end;

procedure TfrmDebug.AddDebug(s: WideString);
begin
  if TikeDebugMode then
    frmMessages.Add(plsInfo, 'Debugger', s, 0, 0);
  // Do NOTHING AT PRESENT
end;

procedure TfrmDebug.AddDebugAction(ItemType, dwData: Integer);
var
  ev: TDebugEvent;
begin
  if not Assigned(debugkeyboard) then Exit;

  ResetEvents;

  if FIgnoreKeyUp then Exit;

  if FEvents.Count > 0 then
  begin
    ev := FEvents[FEvents.Count-1];
    if (ev.EventType = etAction) and (ev.Action.ActionType = ItemType) then
    begin
      case ItemType of
        QIT_CHAR: begin ev.Action.Text := ev.Action.Text + WChar(dwData); Exit; end;
        QIT_BACK: begin ev.Action.Text := ev.Action.Text + WChar(dwData+1); Exit; end;
      end;
    end;
  end;

  ev := TDebugEvent.Create;
  FEvents.Add(ev);
  ev.EventType := etAction;
  ev.Action.ActionType := ItemType;
  ev.Action.dwData := dwData;

  case ItemType of
    QIT_CHAR: ev.Action.Text := WChar(dwData);
    QIT_BACK: ev.Action.Text := WChar(dwData+1);
  end;
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
    if TDeadKeyInfo(deadkeys[i]).Deleted then
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
  deadkeys := TList.Create;
end;

procedure TfrmDebug.ClearDeadkeys;
var
  i: Integer;
begin
  ClearDeadkeyStyle;
  for i := 0 to deadkeys.Count - 1 do TDeadKeyInfo(deadkeys[i]).Free;
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
  i, j: Integer;
  dk: TDeadKeyInfo;
begin
//  Dec(startpos);
  i := 1;
  while i <= Length(s) do
  begin
    if s[i] = #$FFFC then
    begin
      for j := 0 to deadkeys.Count - 1 do
      begin
        dk := TDeadKeyInfo(deadkeys[j]);
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
 - Control captions -- adding and removing '&' depending on FUIDisabled        -
 ------------------------------------------------------------------------------}

(*
procedure TfrmDebug.InitControlCaptions;
begin
  FControlCaptions := TStringList.Create;
  ControlCaption[tabDebugStores] := 'Element&s';
  ControlCaption[tabDebugCallStack] := '&Call stack';
  ControlCaption[tabDebugDeadkeys] := 'Deadke&ys';
  ControlCaption[tabDebugRegressionTesting] := '&Regression testing';

  ControlCaption[cmdRegTestStartStopLog] := 'Start &log';
  ControlCaption[cmdRegTestStartStopTest] := 'R&un test';
  ControlCaption[cmdRegTestOptions] := '&Options';
end;

procedure TfrmDebug.UninitControlCaptions;
begin
  FreeAndNil(FControlCaptions);
end;

procedure TfrmDebug.UpdateControlCaption(i: Integer);
    function RemoveAmp(s: string): string;
    var
      n: Integer;
    begin
      for n := Length(s) downto 1 do
        if s[n] = '&' then Delete(s, n, 1);
      Result := s;
    end;
var
  s: string;
begin
  if FUIDisabled then s := RemoveAmp(FControlCaptions[i]) else s := FControlCaptions[i];
  if FControlCaptions.Objects[i] is TSpeedButton then
    (FControlCaptions.Objects[i] as TSpeedButton).Caption := s
  else if FControlCaptions.Objects[i] is TBitBtn then
    (FControlCaptions.Objects[i] as TBitBtn).Caption := s
  else if FControlCaptions.Objects[i] is TTabSheet then
    (FControlCaptions.Objects[i] as TTabSheet).Caption := s;
end;

procedure TfrmDebug.UpdateControlCaptions;
var
  i: Integer;
begin
  if not Assigned(FControlCaptions) then Exit;
  for i := 0 to FControlCaptions.Count - 1 do
    UpdateControlCaption(i);
end;

procedure TfrmDebug.SetControlCaption(FControl: TControl; const Caption: TCaption);
var
  n: Integer;
begin
  if not Assigned(FControlCaptions) then Exit;

  n := FControlCaptions.IndexOfObject(FControl);
  if n = -1
    then n := FControlCaptions.AddObject(Caption, FControl)
    else FControlCaptions[n] := Caption;

  UpdateControlCaption(n);
end;

function TfrmDebug.GetControlCaption(FControl: TControl): TCaption;
var
  n: Integer;
begin
  if not Assigned(FControlCaptions) then Exit;
  n := FControlCaptions.IndexOfObject(FControl);
  if n = -1
    then Result := ''
    else Result := FControlCaptions[n];
end;
*)

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
  SelStart, MaxCols, I: Integer;
  s: WideString;
  J: Integer;
  K: Integer;
begin
  MaxCols := (sgChars.ClientWidth div (sgChars.DefaultColWidth + 1) - 1) * 2; // Handle surrogate pairs easily

  if memo.SelLength <> 0 then
  begin
    SelStart := memo.SelStart + 1;
    if memo.SelLength < 0 then Inc(SelStart, memo.SelLength);
    s := memo.SelText;
  end
  else
  begin
    I := memo.SelStart - MaxCols;
    if I < 1 then I := 1;
    SelStart := I;
    s := Copy(memo.Text, I, memo.SelStart - I + 1);
  end;

  if Length(s) > MaxCols then
  begin
    Inc(SelStart, Length(s) - MaxCols - 1);
    Delete(s, 1, Length(s) - MaxCols - 1);
  end;

  if (s<>'') and Uni_IsSurrogate2(s[1]) then
  begin
    Inc(SelStart);
    Delete(s,1,1);
  end;

  if Length(s) = 0 then
  begin
    sgChars.ColCount := 1;
    sgChars.Objects[0,0] := Pointer(0);
    sgChars.Cells[0,0] := '';
    sgChars.Cells[0,1] := '';
  end
  else
    sgChars.ColCount := Length(s);

  MaxCols := MaxCols div 2; // Handle surrogate pairs easily

  I := 1; J := 0;
  while I <= Length(s) do
  begin
    if Ord(S[I]) = $FFFC then
    begin
      sgChars.Objects[J, 0] := Pointer(1);
      sgChars.Cells[J, 0] := '???';
      for K := 0 to deadkeys.Count-1 do
        if TDeadKeyInfo(deadkeys[K]).Position = I+SelStart-2 then
        begin
          sgChars.Cells[J, 0] := TDeadKeyInfo(deadkeys[K]).Deadkey.Name;// IntToStr(K);//deadkeys '???';
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
    if J >= MaxCols then
      Break;
  end;

  sgChars.ColCount := J;

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

