{
 * Keyman is copyright (C) SIL International. MIT License.
 *
 * Wrapper around Core debug events and actions, interleaved into a single list.
}
unit Keyman.System.Debug.DebugEvent;

interface

uses
  System.Classes,
  System.Generics.Collections,
  System.SysUtils,
  Winapi.Windows,

  debugkeyboard,
  Keyman.System.KeymanCore,
  Keyman.System.KeymanCoreDebug;

const
  MAXSTOREOFFSETS=20;

type
  TAIDebugKeyInfo = record
    VirtualKey: UINT;
	  Modifiers: DWORD;
  end;

  PAIDebugKeyInfo = ^TAIDebugKeyInfo;

  { DebugEvent structure -- an event has occurred }

  TDebugEventActionData = class
    ActionType: km_core_action_type;
    dwData: Integer;
    nExpectedValue: NativeUInt;
    Text: WideString;
  end;

  TDebugEventRuleData = class
    ItemType, Line: Integer;
    Flags: DWord;
    Rule: TKeymanKeyEx;
    Group: TKeymanGroupEx;
    OptionStoreName: string;
    OptionValue: string;
    Key: TAIDebugKeyInfo;
    Context: WideString;
    StoreOffsets: array[0..20] of Word; //TKeymanStoreEx;
    nStores: Integer;
    procedure FillStoreList(event: pkm_core_state_debug_item; KeyboardMemory: PChar);
  end;

  TDebugEventType = (etAction, etRuleMatch);

  TDebugEvent = class
  private
    FEventType: TDebugEventType;
    FAction: TDebugEventActionData;
    FRule: TDebugEventRuleData;
    procedure SetEventType(const Value: TDebugEventType);
  public
    constructor Create;
    destructor Destroy; override;
    property Action: TDebugEventActionData read FAction;
    property Rule: TDebugEventRuleData read FRule;
    property EventType: TDebugEventType read FEventType write SetEventType;
  end;

  TDebugEventList = class(TObjectList<TDebugEvent>)
  private
    procedure Action_Char(const character: km_core_usv);
    procedure Action_DeleteBack(expected_type: uint8_t;
      expected_value: uintptr_t);
    procedure Action_EmitKeystroke(const key: Word);
    procedure Action_Marker(marker: uintptr_t);
    function AddActionItem(key: Word; action: pkm_core_action_item): Boolean;
    procedure AddDebugItem(
      debug: pkm_core_state_debug_item;
      debugkeyboard: TDebugKeyboard;
      vk: uint16_t;
      modifier_state: uint16_t
    );
  public
    function AddStateItems(
      state: pkm_core_state;
      vk: uint16_t;
      modifier_state: uint16_t;
      debugkeyboard: TDebugKeyboard
    ): Boolean; overload;

    function AddStateItems(
      state: pkm_core_state;
      vk: uint16_t;
      modifier_state: uint16_t
    ): Boolean; overload;
  end;

implementation

uses
  kmxfile,
  Unicode;

{ TDebugEvent }

constructor TDebugEvent.Create;
begin
  inherited Create;
  FEventType := etAction;
  FAction := TDebugEventActionData.Create;
end;

destructor TDebugEvent.Destroy;
begin
  FreeAndNil(FAction);
  FreeAndNil(FRule);
  inherited;
end;

procedure TDebugEvent.SetEventType(const Value: TDebugEventType);
begin
  if FEventType <> Value then
  begin
    case FEventType of
      etAction:    FreeAndNil(FAction);
      etRuleMatch: FreeAndNil(FRule);
    end;
    FEventType := Value;
    case FEventType of
      etAction:    FAction := TDebugEventActionData.Create;
      etRuleMatch: FRule := TDebugEventRuleData.Create;
    end;
  end;
end;

function TDebugEventList.AddActionItem(key: Word; action: pkm_core_action_item): Boolean;
begin
  Result := True;
  case action._type of
    KM_CORE_IT_CHAR:           Action_Char(action.character);
    KM_CORE_IT_MARKER:         Action_Marker(action.marker); // Correlates to kmn's "deadkey" markers.
    KM_CORE_IT_ALERT:          ; // TODO: The keyboard has triggered a alert/beep/bell.
    KM_CORE_IT_BACK:           Action_DeleteBack(action.backspace.expected_type, action.backspace.expected_value);
    KM_CORE_IT_PERSIST_OPT:    ; // TODO: The indicated option needs to be stored.
    KM_CORE_IT_EMIT_KEYSTROKE: Action_EmitKeystroke(key);
    KM_CORE_IT_CAPSLOCK:       ; // TODO: Caps lock state needs to be updated
    KM_CORE_IT_INVALIDATE_CONTEXT: ;
    else Assert(False, 'Action type '+IntToStr(Ord(action._type))+' is unexpected.');
  end;
end;

procedure TDebugEventList.Action_EmitKeystroke(const key: Word);
var
  event: TDebugEvent;
begin
  case key of
    VK_TAB:    Action_Char(9);  // Emit a tab character
    VK_RETURN: Action_Char(13); // New line character
    VK_BACK:   Action_DeleteBack(Ord(KM_CORE_BT_UNKNOWN), 0);
    else
    begin
      event := TDebugEvent.Create;
      event.EventType := etAction;
      event.Action.ActionType := KM_CORE_IT_EMIT_KEYSTROKE;
      event.Action.dwData := key;
      Add(event);
    end;
  end;
end;

///
/// Insert a UTF-32 character at the insertion point.
/// TODO: define behaviour around selection
///
procedure TDebugEventList.Action_Char(const character: km_core_usv);
var
  event: TDebugEvent;
begin
  event := TDebugEvent.Create;
  event.EventType := etAction;
  event.Action.ActionType := KM_CORE_IT_CHAR;
  event.Action.Text := Uni_UTF32CharToUTF16(character);
  Add(event);
end;

///
/// Delete the codepoint preceding the insertion point.
/// TODO: define behaviour around selection
///
procedure TDebugEventList.Action_DeleteBack(
  expected_type: uint8_t;    /// one of KM_CORE_BT_CHAR, KM_CORE_BT_MARKER, KM_CORE_BT_UNKNOWN
  expected_value: uintptr_t  /// used mainly in unit tests
);
var
  event: TDebugEvent;
begin
  event := TDebugEvent.Create;
  event.EventType := etAction;
  event.Action.ActionType := KM_CORE_IT_BACK;
  event.Action.dwData := expected_type;
  event.Action.nExpectedValue := expected_value;
  Add(event);
end;

///
/// Insert a deadkey marker into the output
///
procedure TDebugEventList.Action_Marker(marker: uintptr_t);
var
  event: TDebugEvent;
begin
  // kmx requires that markers are between 1 and $FFFF
  // ($FFFD in practical terms)
  Assert((marker > 0) and (marker <= High(WORD)));

  event := TDebugEvent.Create;
  event.EventType := etAction;
  event.Action.ActionType := KM_CORE_IT_MARKER;
  event.Action.dwData := marker;
  Add(event);
end;

procedure TDebugEventList.AddDebugItem(
  debug: pkm_core_state_debug_item;
  debugkeyboard: TDebugKeyboard;
  vk: uint16_t;
  modifier_state: uint16_t);

  function DiscoverRuleLine: Integer;
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
      KM_CORE_DEBUG_END:           Result := debugkeyboard.BeginUnicodeLine;
      KM_CORE_DEBUG_BEGIN:         Result := debugkeyboard.BeginUnicodeLine;
      KM_CORE_DEBUG_GROUP_ENTER,
      KM_CORE_DEBUG_GROUP_EXIT:    if grp > -1 then Result := debugkeyboard.Groups[grp].Line;
      KM_CORE_DEBUG_MATCH_ENTER,
      KM_CORE_DEBUG_MATCH_EXIT:    if grp > -1 then Result := debugkeyboard.Groups[grp].MatchLine;
      KM_CORE_DEBUG_NOMATCH_ENTER,
      KM_CORE_DEBUG_NOMATCH_EXIT:  if grp > -1 then Result := debugkeyboard.Groups[grp].NomatchLine;
    end;
  end;

var
  ev: TDebugEvent;
  rule: PKeymanKey;
  group: PKeymanGroup;
  store: PKeymanStore;
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
    ev.Rule.Line := DiscoverRuleLine;

  ev.Rule.Context := debug.kmx_info.Context;
  if Assigned(debug.kmx_info.group) then
  begin
    group := PKeymanGroup(debug.kmx_info.group);
    ev.Rule.Group.dpName := group.dpName;
    ev.Rule.Group.dpMatch := group.dpMatch;
    ev.Rule.Group.dpNoMatch := group.dpNoMatch;
    ev.Rule.Group.fUsingKeys := group.fUsingKeys;
  end;

  Add(ev);

  { Update user interface }

  if ev.Rule.ItemType = KM_CORE_DEBUG_BEGIN then
  begin
    ev.Rule.Key.VirtualKey := vk;
    ev.Rule.Key.Modifiers := modifier_state;
  end;

  if ev.Rule.ItemType = KM_CORE_DEBUG_SET_OPTION then
  begin
    store := PKeymanStore(debug.kmx_info.option.store);
    ev.Rule.OptionStoreName := store.dpName;
    ev.Rule.OptionValue := debug.kmx_info.option.value;
  end;
end;

function TDebugEventList.AddStateItems(
  state: pkm_core_state;
  vk: uint16_t;
  modifier_state: uint16_t
): Boolean;
var
  action: pkm_core_action_item;
begin
  Result := True;
  action := km_core_state_action_items(state, nil);
  while (action._type <> KM_CORE_IT_END) do
  begin
    Result := Result and AddActionItem(vk, action);
    Inc(action);
  end;
end;

function TDebugEventList.AddStateItems(
  state: pkm_core_state;
  vk: uint16_t;
  modifier_state: uint16_t;
  debugkeyboard: TDebugKeyboard
): Boolean;
var
  action: pkm_core_action_item;
  debug: pkm_core_state_debug_item;
  action_index: Integer;
begin
  Result := True;
  debug := km_core_state_debug_items(state, nil);
  action := km_core_state_action_items(state, nil);
  action_index := 0;
  while debug._type <> KM_CORE_DEBUG_END do
  begin
    if debug.kmx_info.first_action > action_index then
    begin
      while (action._type <> KM_CORE_IT_END) and (action_index < debug.kmx_info.first_action) do
      begin
        Result := Result and AddActionItem(vk, action);
        Inc(action);
        Inc(action_index);
      end;
    end;
    AddDebugItem(debug, debugkeyboard, vk, modifier_state);
    Inc(debug);
  end;

  AddDebugItem(debug, debugkeyboard, vk, modifier_state);

  if action._type = KM_CORE_IT_INVALIDATE_CONTEXT then
  begin
    // We always ignore invalidate context which can come when a frame key is
    // pressed (#11172, #11486)
    Inc(action);
  end;

  if action._type = KM_CORE_IT_EMIT_KEYSTROKE then
  begin
    // The EMIT_KEYSTROKE action comes after all rules have completed processing
    Result := Result and AddActionItem(vk, action);
    Inc(action);
  end;

  // By the time we get to the end of rule processing, all actions should have
  // already been undertaken
  Assert(action._type = KM_CORE_IT_END);
end;

{ TDebugEventRuleData }

procedure TDebugEventRuleData.FillStoreList(event: pkm_core_state_debug_item; KeyboardMemory: PChar);
  function StoreOffset(kfh: PKeyboardFileHeader; i: Word): PChar;
  begin
    Result := KeyboardMemory;
    Inc(Result, kfh.dpStoreArray);
    while i > 0 do
    begin
      Inc(Result, SizeOf(TKeyboardFileStore));
      Dec(i);
    end;
  end;
begin
  nStores := 0;
//  kfh := PKeyboardFileHeader(KeyboardMemory);
  while event.kmx_info.store_offsets[nStores*2] <> $FFFF do
  begin
    StoreOffsets[nStores] := event.kmx_info.store_offsets[nStores*2+1];
//    Stores[nStores].Store := PKeyboardFileStore(StoreOffset(kfh, di.StoreOffsets[nStores*2]))^;
//    Stores[nStores].MatchPosition := di.StoreOffsets[nStores*2+1];
    Inc(nStores);
  end;
end;

end.
