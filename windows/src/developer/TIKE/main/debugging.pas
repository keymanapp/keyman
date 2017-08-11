(*
  Name:             debugging
  Copyright:        Copyright (C) SIL International.
  Documentation:    
  Description:      
  Create Date:      1 Aug 2006

  Modified Date:    14 Sep 2006
  Authors:          mcdurdin
  Related Files:    
  Dependencies:     

  Bugs:             
  Todo:             
  Notes:            
  History:          01 Aug 2006 - mcdurdin - Fix packing for TKeymanKey and TKeymanGroup
                    14 Sep 2006 - mcdurdin - Add SaveShiftState function
*)
unit debugging;

interface

uses Windows, Messages, Classes, kmxfile, debugkeyboard;

type
  { KMX file structures }

  TKeymanStoreEx = record
    Store: TKeyboardFileStore;
    MatchPosition: Integer;
  end;

  PKeymanStoreEx = ^TKeymanStoreEx;

  TKeymanKey = packed record
    Key: WCHAR; packing: WORD;
    Line: DWord;
    ShiftFlags: DWord;
    dpOutput: PWideChar;
    dpContext: PWideChar;
  end;

  PKeymanKey = ^TKeymanKey;

  TKeymanKeyEx = record
    Key: WCHAR;
    Line: DWord;
    ShiftFlags: DWord;
    dpOutput: WideString;
    dpContext: WideString;
  end;

  TKeymanGroup = packed record
    dpName: PWideChar;
    dpKeyArray: PKeymanKey;
    dpMatch: PWideChar;
    dpNoMatch: PWideChar;
    cxKeyArray: DWord;
    fUsingKeys: BOOL;
  end;

  PKeymanGroup = ^TKeymanGroup;

  TKeymanGroupEx = record
    dpName: WideString;
    //dpKeyArray: PKeymanKey;
    dpMatch: WideString;
    dpNoMatch: WideString;
    //cxKeyArray: DWord;
    fUsingKeys: BOOL;
  end;

  { DebugInfo structure passed from Keyman32 }

const
  MAXSTOREOFFSETS=20;

type
  TAIDebugInfo = record
    cbSize: Integer;
    ItemType: Integer;
    Context, Output: PWideChar;
    Rule: PKeymanKey;
    Group: PKeymanGroup;
    Flags: DWord;
    StoreOffsets: array[0..MAXSTOREOFFSETS*2] of Word;
  end;

  PAIDebugInfo = ^TAIDebugInfo;

  TAIDebugKeyInfo = record
    VirtualKey: UINT;
	  ShiftFlags: DWORD;
	  Character, DeadKeyCharacter: WCHAR;
    IsUp: BOOL;
  end;

  PAIDebugKeyInfo = ^TAIDebugKeyInfo;

  { DebugEvent structure -- an event has occurred }

  TDebugEventActionData = class
    ActionType, dwData: Integer;
    Text: WideString;
  end;

  TDebugEventRuleData = class
    ItemType, Line: Integer;
    Flags: DWord;
    Rule: TKeymanKeyEx;
    Group: TKeymanGroupEx;
    Key: TAIDebugKeyInfo;
    Context, Output: WideString;
    StoreOffsets: array[0..20] of Word; //TKeymanStoreEx;
    nStores: Integer;
    procedure FillStoreList(di: PAIDebugInfo; KeyboardMemory: PChar);
  end;

  TDebugEventCursor = record
    X, Y: Integer;
  end;

  TDebugEventType = (etAction, etRuleMatch);

  TDebugEvent = class
  private
    FEventType: TDebugEventType;
    FAction: TDebugEventActionData;
    FRule: TDebugEventRuleData;
    FCursor: TDebugEventCursor;
    procedure SetEventType(const Value: TDebugEventType);
  public
    constructor Create;
    destructor Destroy; override;
    property Action: TDebugEventActionData read FAction;
    property Rule: TDebugEventRuleData read FRule;
    property Cursor: TDebugEventCursor read FCursor;
    property EventType: TDebugEventType read FEventType write SetEventType;
  end;

  TDebugEventList = class(TList)
  protected
    function Get(Index: Integer): TDebugEvent;
    procedure Put(Index: Integer; Item: TDebugEvent);
  public
    destructor Destroy; override;
    procedure Clear; override;
    property Items[Index: Integer]: TDebugEvent read Get write Put; default;
    function Add(Item: TDebugEvent): Integer;
  end;

const
  QID_FLAG_RECURSIVE_OVERFLOW = $0001;
  QID_FLAG_NOMATCH = $0002;

const
  QID_BEGIN_UNICODE = 0;
  QID_BEGIN_ANSI    = 1;
  QID_GROUP_ENTER   = 2;
  QID_GROUP_EXIT    = 3;
  QID_RULE_ENTER    = 4;
  QID_RULE_EXIT     = 5;
  QID_MATCH_ENTER   = 6;
  QID_MATCH_EXIT    = 7;
  QID_NOMATCH_ENTER = 8;
  QID_NOMATCH_EXIT  = 9;
  QID_END           = 10;

  QIT_VKEYDOWN   = 0;
  QIT_VKEYUP     = 1;
  QIT_VSHIFTDOWN = 2;
  QIT_VSHIFTUP   = 3;
  QIT_CHAR       = 4;
  QIT_DEADKEY    = 5;
  QIT_BELL       = 6;
  QIT_BACK       = 7;

const
  K_LCTRLFLAG      = $0001; // Left Control flag
  K_RCTRLFLAG      = $0002; // Right Control flag
  K_LALTFLAG       = $0004; // Left Alt flag
  K_RALTFLAG       = $0008; // Right Alt flag
  K_SHIFTFLAG      = $0010; // Either shift flag
  K_CTRLFLAG       = $0020; // Either ctrl flag
  K_ALTFLAG        = $0040; // Either alt flag
  K_CAPITALFLAG    = $0100; // Caps lock on
  K_NOTCAPITALFLAG = $0200; // Caps lock NOT on
  K_NUMLOCKFLAG    = $0400; // Num lock on
  K_NOTNUMLOCKFLAG = $0800; // Num lock NOT on
  K_SCROLLFLAG     = $1000; // Scroll lock on
  K_NOTSCROLLFLAG  = $2000; // Scroll lock NOT on
  K_ISVIRTUALKEY   = $4000; // It is a Virtual Key Sequence

var
	WM_KEYMANDEBUG_CANDEBUG,
 	WM_KEYMANDEBUG_GETUNICODESTATUS,
	WM_KEYMANDEBUG_GETCONTEXT,
	WM_KEYMANDEBUG_ACTION,
 	WM_KEYMANDEBUG_RULEMATCH,
  WM_KEYMANKEYDOWN: Word;



procedure SetShiftState(hwnd: HWND; AShiftFlags: Integer);
procedure ClearShiftState(hwnd: HWND; AShiftFlags: Integer);
function SaveShiftState: Integer;

implementation

uses SysUtils;

{ TDebugEventList }

function TDebugEventList.Get(Index: Integer): TDebugEvent;        begin Result := TDebugEvent(inherited Get(Index)); end;
procedure TDebugEventList.Put(Index: Integer; Item: TDebugEvent); begin inherited Put(Index, Pointer(Item)); end;
function TDebugEventList.Add(Item: TDebugEvent): Integer;         begin Result := inherited Add(Pointer(Item)); end;

procedure TDebugEventList.Clear;
var
  i: Integer;
begin
  for i := 0 to Count - 1 do Items[i].Free;
  inherited Clear;
end;

destructor TDebugEventList.Destroy;
begin
  Clear;
  inherited Destroy;
end;

{ Shift state code for the debug window }

const
  KEYMAN_CHARFLAG: DWord   = $02000000;
  CHAR_TRANSTATE: DWord    = $00000001;  // Flag for WM_CHAR: key is down, first repeat
  KEYUP_TRANSTATE: DWord   = $C0000001;  // Flag for WM_KEYUP: key is up, first repeat
  KEYDOWN_TRANSTATE: DWord = $00000001;  // Flag for WM_KEYDOWN: key is down, first rpt
  ALT_TRANSTATE: DWord     = $20000000;  // Flag for WM_KEYBOARD messages: alt is down

  LCTRLFLAG      = $0001;  // Left Control flag
  RCTRLFLAG      = $0002;  // Right Control flag
  LALTFLAG       = $0004;  // Left Alt flag
  RALTFLAG       = $0008;  // Right Alt flag
  CAPITALFLAG    = $0100;  // Caps lock on
  NOTCAPITALFLAG = $0200;  // Caps lock NOT on
  NUMLOCKFLAG    = $0400;  // Num lock on
  NOTNUMLOCKFLAG = $0800;  // Num lock NOT on
  SCROLLFLAG     = $1000;  // Scroll lock on
  NOTSCROLLFLAG  = $2000;  // Scroll lock NOT on
  ISVIRTUALKEY   = $4000;  // It is a Virtual Key Sequence


procedure SetShiftState(hwnd: HWND; AShiftFlags: Integer);
var
  kbstate: TKeyboardState;
    procedure SetShift(VKey, GenVKey, shift: Integer);
    begin
      if ((kbstate[VKey] and $80) <> 0) and ((AShiftFlags and shift) = 0) then
      begin
        //PostMessage(hwnd, WM_KEYUP, GenVKey, LongInt(KEYMAN_CHARFLAG or KEYUP_TRANSTATE));
        kbstate[VKey] := kbstate[VKey] and $7F;
        kbstate[GenVKey] := kbstate[GenVKey] and $7F;
      end
      else if ((kbstate[VKey] and $80) = 0) and ((AShiftFlags and shift) <> 0) then
      begin
        //PostMessage(hwnd, WM_KEYDOWN, GenVKey, LongInt(KEYMAN_CHARFLAG or KEYDOWN_TRANSTATE));
        kbstate[VKey] := kbstate[VKey] or $80;
        kbstate[GenVKey] := kbstate[GenVKey] or $80;
      end;
    end;
begin
 	GetKeyboardState(kbstate);

	SetShift(VK_LMENU, VK_MENU, LALTFLAG or K_ALTFLAG);
	SetShift(VK_RMENU, VK_MENU, RALTFLAG);
	SetShift(VK_LCONTROL, VK_CONTROL, LCTRLFLAG or K_CTRLFLAG);
	SetShift(VK_RCONTROL, VK_CONTROL, RCTRLFLAG);
	SetShift(VK_SHIFT, VK_SHIFT, K_SHIFTFLAG);

	SetKeyboardState(kbstate);
end;

function SaveShiftState: Integer;
var
  kbstate: TKeyboardState;
  procedure VKState(vk, shift: Integer);
  begin
    if (kbstate[vk] and $80) <> 0 then Result := Result or shift;
  end;
begin
  Result := 0;
 	GetKeyboardState(kbstate);

  VKState(VK_LMENU, LALTFLAG);
  VKState(VK_RMENU, RALTFLAG);
  VKState(VK_MENU, K_ALTFLAG);

  VKState(VK_LCONTROL, LCTRLFLAG);
  VKState(VK_RCONTROL, RCTRLFLAG);
  VKState(VK_CONTROL, K_CTRLFLAG);

  VKState(VK_SHIFT, K_SHIFTFLAG);
end;

procedure ClearShiftState(hwnd: HWND; AShiftFlags: Integer);
var
  kbstate: TKeyboardState;
    procedure ClearShift(VKey, shift: Integer);
    begin
      //if (AShiftFlags and shift) <> 0 then
        //PostMessage(hwnd, WM_KEYUP, VKey, KEYMAN_CHARFLAG or KEYUP_TRANSTATE);
    end;
begin
	ClearShift(VK_LMENU, LALTFLAG or RALTFLAG or K_ALTFLAG);
	ClearShift(VK_LCONTROL, LCTRLFLAG or RCTRLFLAG or K_CTRLFLAG);
	ClearShift(VK_SHIFT, K_SHIFTFLAG);
	GetKeyboardState(kbstate);
	kbstate[VK_LMENU]    := 0;
	kbstate[VK_RMENU]    := 0;
	kbstate[VK_MENU]     := 0;
	kbstate[VK_LCONTROL] := 0;
	kbstate[VK_RCONTROL] := 0;
	kbstate[VK_CONTROL]  := 0;
	kbstate[VK_SHIFT]    := 0;
	SetKeyboardState(kbstate);
end;

{ TDebugEvent }

constructor TDebugEvent.Create;
begin
  inherited Create;
  FEventType := etRuleMatch;
  EventType := etAction;    // Force change of EventType
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

{ TDebugEventRuleData }

procedure TDebugEventRuleData.FillStoreList(di: PAIDebugInfo; KeyboardMemory: PChar);
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

{var
  kfh: PKeyboardFileHeader;}
begin
  nStores := 0;
//  kfh := PKeyboardFileHeader(KeyboardMemory);
  while di.StoreOffsets[nStores*2] <> $FFFF do
  begin
    StoreOffsets[nStores] := di.StoreOffsets[nStores*2+1];
//    Stores[nStores].Store := PKeyboardFileStore(StoreOffset(kfh, di.StoreOffsets[nStores*2]))^;
//    Stores[nStores].MatchPosition := di.StoreOffsets[nStores*2+1];
    Inc(nStores);
  end;
end;

initialization
	WM_KEYMANDEBUG_CANDEBUG         := RegisterWindowMessage('WM_KEYMANDEBUG_CANDEBUG');
	WM_KEYMANDEBUG_GETUNICODESTATUS := RegisterWindowMessage('WM_KEYMANDEBUG_GETUNICODESTATUS');
	WM_KEYMANDEBUG_GETCONTEXT       := RegisterWindowMessage('WM_KEYMANDEBUG_GETCONTEXT');
	WM_KEYMANDEBUG_ACTION           := RegisterWindowMessage('WM_KEYMANDEBUG_ACTION');
	WM_KEYMANDEBUG_RULEMATCH        := RegisterWindowMessage('WM_KEYMANDEBUG_RULEMATCH');
  WM_KEYMANKEYDOWN                := RegisterWindowMessage('WM_KEYMANKEYDOWN');
end.
