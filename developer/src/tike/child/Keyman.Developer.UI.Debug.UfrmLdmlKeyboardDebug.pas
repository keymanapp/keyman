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
  Winapi.RichEdit,
  Winapi.Windows,

  CaptionPanel,
  debugdeadkeys,
  debugkeyboard,
  PaintPanel,
  KeymanDeveloperDebuggerMemo,
  Keyman.System.Debug.DebugCore,
  Keyman.System.Debug.DebugUtils,
  Keyman.System.KeymanCore,
  Keyman.System.KeymanCoreDebug,
  UframeTextEditor,
  UfrmMDIEditor,
  UfrmTike,
  UserMessages, Keyman.Developer.UI.RichEdit41;

type
  TLdmlDebugUIStatus = (
    duiStarting,
    duiTest,
    duiClosing
  );

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
    FFileName: string;
    debugkeyboard: TDebugKeyboard;
    FDefaultFont: Boolean;
    FUIStatus: TLdmlDebugUIStatus;

    FIgnoreNextUIKey: Boolean;

    { Deadkey member variables }
    FSelectedDeadkey: TDeadKeyInfo;

    function GetStatusText: string;
    procedure SetStatusText(Value: string);

  private
    FDebugCore: TDebugCore;
    FDeadkeys: TDebugDeadkeyInfoList;
    FEditorMemo: TframeTextEditor;

    FDebugFileName: WideString;
    procedure ClearDeadkeys;
    procedure ClearDeadkeyStyle;
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
    procedure Run(vkey: Word);

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

    property UIStatus: TLdmlDebugUIStatus read FUIStatus write FUIStatus;

    property DebugFileName: WideString read FDebugFileName write FDebugFileName;
    property CompiledFileName: string read FFileName write FFileName;   // I4695
    property EditorMemo: TframeTextEditor read FEditorMemo write FEditorMemo;

    procedure ShowDebugForm;
    procedure HideDebugForm;

  end;

implementation

{$R *.DFM}

uses
  System.Character,
  System.Math,

  Keyman.Developer.System.HelpTopics,

  dmActionsDebugger,
  dmActionsMain,
  Keyman.Developer.System.Project.ProjectLog,
  Keyman.Developer.UI.Project.ProjectUI,
  Keyman.Developer.UI.UfrmLdmlKeyboardEditor,
  Keyman.UI.Debug.CharacterGridRenderer,
  KeyNames,
  KeymanDeveloperOptions,
  KeymanDeveloperUtils,
  ScanCodeMap,
  TextFileFormat,
  UfrmEditor,
  UfrmMain,
  UfrmMessages,
  UfrmSelectSystemKeyboard,
  Unicode,
  utilstr;

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

  FUIStatus := duiStarting;
  FDefaultFont := True;
  memo.Align := alClient;
  memo.ReadOnly := False;
  memo.IsDebugging := True;
  StatusText := 'Simple Test';

  UIStatus := duiTest;
end;

procedure TfrmLdmlKeyboardDebug.FormDestroy(Sender: TObject);
begin
  ResetDebug;
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
  memoSelMove(memo);
end;

procedure TfrmLdmlKeyboardDebug.memoKeyUp(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  memoSelMove(memo);
end;

function TfrmLdmlKeyboardDebug.HandleMemoKeydown(var Message: TMessage): Boolean;
begin
  if (Message.wParam = VK_F6) and
    (GetKeyState(VK_CONTROL) >= 0) and
    (GetKeyState(VK_MENU) >= 0) then
  begin
    EditorMemo.SetFocus;
  end
  else if UIStatus = duiTest then
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

  if UIStatus <> duiTest then
  begin
    Exit;
  end;

  case Message.Msg of
    WM_KEYDOWN,
    WM_SYSKEYDOWN:
      Handled := HandleMemoKeydown(Message);
    WM_SYSCHAR:
      begin
        Handled := FIgnoreNextUIKey;
        FIgnoreNextUIKey := False;
      end;
    WM_CHAR:
      Handled := True;
  end;
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
  context_items := GetContextFromMemo(memo, FDeadkeys, True);
  context := km_core_state_context(FDebugCore.State);
  Result := km_core_context_set(context, @context_items[0]) = KM_CORE_STATUS_OK;

  if Result then
  begin
    // Set the app context
    context_items := GetContextFromMemo(memo, FDeadkeys, False);
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

  if km_core_process_event(FDebugCore.State, VKToScanCodeToVK(Message.WParam, GetKeyboardLayout(0)), modifier, 1, KM_CORE_EVENT_FLAG_DEFAULT) = KM_CORE_STATUS_OK then
  begin
    // Process keystroke -- true = swallow keystroke
    Result := True;

    if IsModifierKey(vkey) then
    begin
      // We don't want to trigger the debugger on modifier keys
      Exit(True);
    end;

    Run(vkey);

    UpdateDeadkeys;
  end
  else
  begin
    Result := False;
  end;
end;

function UsvToString(p: pkm_core_usv): string;
begin
  Result := '';
  while p^ <> 0 do
  begin
    Result := Result + Char.ConvertFromUtf32(p^);
    Inc(p);
  end;
end;

procedure TfrmLdmlKeyboardDebug.Run(vkey: Word);

  procedure DoAlert;
  begin
    MessageBeep(0);
  end;



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

  procedure DoHandleShortcut(vk: UINT);
  begin
    // Because we disable shortcuts in the debug memo, there are a small set of
    // editor Ctrl+Key shortcuts we need to rehandle here. Note that Ctrl+Ins,
    // Shift+Ins, Shift+Del appear to be handled natively by the debug memo
    // control (Windows code?), as are all cursor movement / selection keys,
    // apart from Ctrl+A

    if GetKeyState(VK_CONTROL) >= 0 then
    begin
      if (vk = VK_RETURN) and (GetKeyState(VK_MENU) >= 0) then
      begin
        memo.SelText := #13#10;
      end;
      Exit;
    end;

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
    FIgnoreNextUIKey := False;
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


  // Returns the length of the context items in UTF16, treating markers as a
  // single UTF16 code unit (U+FFFC)
  function ContextItemsLengthUTF16(pc: pkm_core_context_item): Integer;
  begin
    Result := 0;
    while pc._type <> KM_CORE_CT_END do
    begin
      if pc._type = KM_CORE_CT_CHAR then
      begin
        if Uni_IsSurrogate(pc.character) then
          Inc(Result);
      end;
      Inc(Result);
      Inc(pc);
    end;
  end;

  function ContextToDebugString(context_items: pkm_core_context_item; offset, length: Integer): string;
  var
    pc: pkm_core_context_item;
    dk: TDeadKeyInfo;
  begin
    Result := '';
    pc := context_items;
    while pc._type <> KM_CORE_CT_END do
    begin
      if pc._type = KM_CORE_CT_CHAR then
      begin
        Result := Result + Uni_UTF32CharToUTF16(pc.character);
      end
      else
      begin
        dk := TDeadKeyInfo.Create;
        dk.Deadkey := TDebugDeadkey.Create;
        dk.Deadkey.Name := '\m{'+IntToStr(pc.marker)+'}';
        dk.Deadkey.Value := pc.marker;
        dk.Position := Result.Length + offset;
        dk.SavedPosition := Result.Length + offset;
        FDeadkeys.Add(dk);
        Result := Result + #$FFFC;
      end;
      Inc(pc);
    end;
  end;

type
  TSetTextEx = record
    flags: DWord;
    codepage: UINT;
  end;
var
  actions: pkm_core_actions;
  context_items: pkm_core_context_item;
  selection: TMemoSelection;
  output: string;
  lhs, rhs, context: string;
  status: km_core_status;
  dk: TDeadKeyInfo;
  context_items_length: Integer;
  state: TMemoSelectionState;
  Adjustment: Integer;
  ste: TSetTextEx;
  str: string;
begin
  FIgnoreNextUIKey := True;

  actions := km_core_state_get_actions(FDebugCore.State);
  if actions = nil then
  begin
    GetGlobalProjectUI.Log(plsError, FDebugFileName,
      'Failed to get actions from Keyman Core', 0, 0);
    Exit;
  end;

  context_items := nil;
  status := km_core_context_get(km_core_state_context(FDebugCore.State), @context_items);
  if status <> KM_CORE_STATUS_OK then
  begin
    GetGlobalProjectUI.Log(plsError, FDebugFileName,
      'Failed to get context from Keyman Core with error '+IntToStr(Ord(Status)), 0, 0);
    Exit;
  end;

  selection := memo.Selection;

  // TODO: #10471 deleting U+000D U+000A should delete as a unit

  try
    if (actions.emit_keystroke = 1) and (vkey <> VK_BACK) and
      (actions.code_points_to_delete = 0) and (actions.output^ = 0) then
    begin
      // If the keystroke is being emitted, then it was not matched by LDML, and
      // must be a frame key. We exclude Bksp as a special case which needs
      // additional processing. All other frame keys should be passed to the
      // text control for default processing.
    end
    else
    begin
      if selection.Finish > selection.Start then
      begin
        // We have a selection, so we should remove it first and then continue
        // as normal. We will have given Core a zero-length context, so we
        // need to save everything before the start of selection before
        // continuing, and cleanup any markers that were in the selection
        if (actions.emit_keystroke = 0) or (vkey = VK_BACK) then
        begin
          state := SaveMemoSelectionState;
          memo.SelText := '';
          RealignMemoSelectionState(state);
          selection := memo.Selection;
        end;
        lhs := Copy(memo.Text, 1, selection.Start);
      end
      else
        lhs := '';

      context := Copy(memo.Text, lhs.Length + 1, selection.Start - lhs.Length);
      rhs := Copy(memo.Text, lhs.Length + context.Length + 1, MaxInt);

      // Reinsert the context

      context_items_length := ContextItemsLengthUTF16(context_items);
      Adjustment := context_items_length - selection.Start + lhs.Length;

      // Delete all markers that are in the context, because we will reinsert
      // them shortly, and adjust marker positions to right of insertion point

      for dk in FDeadkeys do
      begin
        if (dk.Position > lhs.Length) and (dk.Position < selection.Start) then
        begin
          dk.Delete;
        end
        else if dk.Position >= selection.Start then
        begin
          dk.Position := dk.Position + Adjustment;
        end;
      end;

      // Build the new context string and create new markers

      output := ContextToDebugString(context_items, lhs.Length, context_items_length);

      // Merge left of context, context, and right of context and update memo
      // insertion point position

      memo.Lines.BeginUpdate;
      try
        // Setting text directly for improved performance
        ste.flags := $01 {ST_KEEPUNDO} or $04 {ST_NEWCHARS} or $08 {ST_UNICODE} or $20 {ST_PLAINTEXTONLY};
        ste.codepage := 1200 {Unicode};
        str := lhs + output + rhs;
        SendMessage(memo.Handle, (WM_USER + 97) {EM_SETTEXTEX}, NativeUint(@ste), NativeUInt(PChar(str)));
        selection.Start := lhs.Length + output.Length;
        selection.Finish := selection.Start;
        selection.Anchor := selection.Start;
        memo.Selection := selection;
      finally
        memo.Lines.EndUpdate;
      end;
    end;

    // actions.persist_options are not currently supported by LDML

    if actions.do_alert <> 0 then
    begin
      DoAlert;
    end;

    if actions.emit_keystroke <> 0 then
    begin
      DoEmitKeystroke(vkey);
    end;

  finally
    km_core_context_items_dispose(context_items);
  end;
end;

{ Stores }

procedure TfrmLdmlKeyboardDebug.CleanupCoreState;
begin
  FreeAndNil(FDebugCore);
end;

procedure TfrmLdmlKeyboardDebug.HideDebugForm;
begin
  frmKeymanDeveloper.ShowDebug(False);   // I4796
  ResetDebug;
end;

procedure TfrmLdmlKeyboardDebug.ShowDebugForm;
begin
  UIStatus := duiTest;

  frmKeymanDeveloper.ShowDebug(True);   // I4796

  if not SetupDebug then
    Exit;

  memo.SetFocus;
end;

procedure TfrmLdmlKeyboardDebug.ResetDebug;
begin
  FreeAndNil(debugkeyboard);
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

procedure TfrmLdmlKeyboardDebug.memoSelMove(Sender: TObject);
begin
  if memo.Focused then
  begin
    frmKeymanDeveloper.barStatus.Panels[0].Text := 'Debugger Active';
  end;

  if not memo.ReadOnly and not memo.SelectionChanging then
  begin
    UpdateCharacterGrid;   // I4808
  end;
end;

procedure TfrmLdmlKeyboardDebug.UpdateCharacterGrid;   // I4808
var
  start, len: Integer;
begin
  if csDestroying in ComponentState then
    Exit;

  start := memo.SelStart;
  len := memo.SelLength;
  if start + len > Length(memo.Text) then
  begin
    // RichEdit has a virtual final character, which is selected when
    // pressing Ctrl+A, etc.
    len := Length(memo.Text) - start;
  end;
  TCharacterGridRenderer.Fill(sgChars, memo.Text, FDeadkeys, start, len,
    memo.Selection.Anchor, True);
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

