(*
  Name:             RegressionTest
  Copyright:        Copyright (C) SIL International.
  Documentation:    
  Description:      
  Create Date:      1 Aug 2006

  Modified Date:    26 Jun 2012
  Authors:          mcdurdin
  Related Files:    
  Dependencies:     

  Bugs:             
  Todo:             
  Notes:            
  History:          19 Nov 2007 - mcdurdin - I1157 - const string parameters
                    18 Mar 2011 - mcdurdin - I2794 - Fix handle leaks
                    18 May 2012 - mcdurdin - I3324 - V9.0 - Replace XDOM with MSDOM
                    08 Jun 2012 - mcdurdin - I3324 - V9.0 - Replace XDOM with MSDOM
                    08 Jun 2012 - mcdurdin - I3310 - V9.0 - Unicode in Delphi fixes
                    26 Jun 2012 - mcdurdin - I3377 - KM9 - Update code references from 8.0 to 9.0
*)
unit RegressionTest;  // I3324 TO DO

interface

uses
  System.Classes,
  System.Contnrs,
  System.SysUtils,
  Winapi.Windows,

  debugging,
  debugkeyboard;

type
  TRegressionTest = class;
  
  { Exceptions }

  ERegressionTestBase = class(Exception);
  ERegressionTestFile = class(ERegressionTestBase);
  ERegressionTest = class(ERegressionTestBase);

  { Base Classes }

  TRegressionTestBase = class
  end;

  TRegressionTestBaseList = class(TObjectList)
  protected
    function Get(Index: Integer): TRegressionTestBase;
    procedure Put(Index: Integer; Item: TRegressionTestBase);
  public
    property Items[Index: Integer]: TRegressionTestBase read Get write Put; default;
    function Add(Item: TRegressionTestBase): Integer;
  end;

  { Events }

  TRegressionTestEvent = class(TRegressionTestBase)
  private
    FVKey: Word;
    FShiftState: DWord;
    FPostContext: WideString;
    FRegressionTest: TRegressionTest;
    function DeadKeyName(dkCode: Integer): WideString;
  public
    constructor Create(AOwner: TRegressionTest);
    procedure Execute;
    function ShiftStateAsString: string;
    function ShiftStateElement: WideString;
    function PostContextElement: WideString;
    property VKey: Word read FVKey write FVKey;
    property ShiftState: DWord read FShiftState write FShiftState;
    property PostContext: WideString read FPostContext write FPostContext;
  end;

  TRegressionTestEventList = class(TRegressionTestBaseList)
  protected
    function Get(Index: Integer): TRegressionTestEvent;
    procedure Put(Index: Integer; Item: TRegressionTestEvent);
  public
    property Items[Index: Integer]: TRegressionTestEvent read Get write Put; default;
    function Add(Item: TRegressionTestEvent): Integer;
  end;

  { TRegressionTest }

  TRegressionTestBeginMode = (rtbmANSI, rtbmUnicode);

  TRegressionTest = class(TRegressionTestBase)
  private
    FSystemKeyboard: WideString;
    FFileName: WideString;
    FBeginMode: TRegressionTestBeginMode;
    FEvents: TRegressionTestEventList;
    FCurrentEvent: Integer;
    FDebugKeyboard: TDebugKeyboard;
    FTestFileName: string;
    function GetDeadkeyCode(name: WideString): Integer;
    procedure SetCurrentEvent(const Value: Integer);
    procedure SetDebugKeyboard(const Value: TDebugKeyboard);
  public
    constructor Create;
    destructor Destroy; override;
    procedure Load(FileName: string);
    procedure Save(FileName: string);
    procedure Clear;
    procedure ExecuteCurrentEvent;
    function XMLWideString: Widestring;
    property SystemKeyboard: WideString read FSystemKeyboard write FSystemKeyboard;
    property FileName: WideString read FFileName write FFileName;
    property TestFileName: string read FTestFileName;
    property BeginMode: TRegressionTestBeginMode read FBeginMode write FBeginMode;
    property Events: TRegressionTestEventList read FEvents;
    property CurrentEvent: Integer read FCurrentEvent write SetCurrentEvent;
    property DebugKeyboard: TDebugKeyboard read FDebugKeyboard write SetDebugKeyboard;
  end;

implementation

uses
  System.Variants,
  Vcl.Controls,
  Vcl.Dialogs,
  Vcl.Forms,
  Xml.xmlintf,
  Xml.xmldoc,

  KeymanVersion,
  kmxfileconsts,
  utilstr,
  Unicode,
  VKeys;

const
  RegressionTestBeginModeText: array[TRegressionTestBeginMode] of WideString = ('ANSI', 'Unicode');

const
  ERTCurrentEventOutOfBounds = 'Regression testing: current event was out of bounds.';

{-------------------------------------------------------------------------------
 - TRegressionTestBaseList                                                     -
 ------------------------------------------------------------------------------}

function TRegressionTestBaseList.Add(Item: TRegressionTestBase): Integer;
begin
  Result := inherited Add(Item);
end;

function TRegressionTestBaseList.Get(Index: Integer): TRegressionTestBase;
begin
  Result := inherited Items[Index] as TRegressionTestBase;
end;

procedure TRegressionTestBaseList.Put(Index: Integer; Item: TRegressionTestBase);
begin
  inherited Put(Index, Item);
end;

{-------------------------------------------------------------------------------
 - TRegressionTestEvent                                                        -
 ------------------------------------------------------------------------------}

procedure TRegressionTestEvent.Execute;
var
  keys: array[0..12] of TInput;
  n: Integer;

  procedure AddInput(vk: Word; flags: Word);
  begin
    keys[n].Itype := INPUT_KEYBOARD;
    keys[n].ki.wVk := vk;
    keys[n].ki.wScan := MapVirtualKeyEx(vk, MAPVK_VK_TO_VSC, GetKeyboardLayout(0));
    keys[n].ki.dwFlags := flags;
    keys[n].ki.time := 0;
    keys[n].ki.dwExtraInfo := 0;
    Inc(n);
  end;
begin
  // Send modifier states
  n := 0;
  if (FShiftState and K_SHIFTFLAG) <> 0 then AddInput(VK_SHIFT, 0);
  if FShiftState and K_LCTRLFLAG <> 0 then AddInput(VK_CONTROL, 0);
  if FShiftState and K_RCTRLFLAG <> 0 then AddInput(VK_CONTROL, KEYEVENTF_EXTENDEDKEY);
  if FShiftState and K_LALTFLAG <> 0 then  AddInput(VK_MENU, 0);
  if FShiftState and K_RALTFLAG <> 0 then  AddInput(VK_MENU, KEYEVENTF_EXTENDEDKEY);

  case FShiftState and K_CAPITALFLAG of
    1: if (GetKeyState(VK_CAPITAL) and 1) = 0 then AddInput(VK_CAPITAL, 0);
    0: if (GetKeyState(VK_CAPITAL) and 1) = 1 then AddInput(VK_CAPITAL, KEYEVENTF_KEYUP);
  end;

  AddInput(FVKey, 0);
  AddInput(FVKey, KEYEVENTF_KEYUP);

  if FShiftState and K_RALTFLAG <> 0 then  AddInput(VK_MENU, KEYEVENTF_KEYUP or KEYEVENTF_EXTENDEDKEY);
  if FShiftState and K_LALTFLAG <> 0 then  AddInput(VK_MENU, KEYEVENTF_KEYUP);
  if FShiftState and K_RCTRLFLAG <> 0 then AddInput(VK_CONTROL, KEYEVENTF_KEYUP or KEYEVENTF_EXTENDEDKEY);
  if FShiftState and K_LCTRLFLAG <> 0 then AddInput(VK_CONTROL, KEYEVENTF_KEYUP);
  if (FShiftState and K_SHIFTFLAG) <> 0 then AddInput(VK_SHIFT, KEYEVENTF_KEYUP);

  if SendInput(n, keys[0], sizeof(TInput)) = 0 then
    RaiseLastOSError;
end;

{-------------------------------------------------------------------------------
 - TRegressionTestEventList                                                    -
 ------------------------------------------------------------------------------}

function TRegressionTestEventList.Add(Item: TRegressionTestEvent): Integer;
begin
  Result := inherited Add(Item);
end;

function TRegressionTestEventList.Get(Index: Integer): TRegressionTestEvent;
begin
  Result := inherited Get(Index) as TRegressionTestEvent;
end;

procedure TRegressionTestEventList.Put(Index: Integer; Item: TRegressionTestEvent);
begin
  inherited Put(Index, Item);
end;

{-------------------------------------------------------------------------------
 - TRegressionTest                                                             -
 ------------------------------------------------------------------------------}

constructor TRegressionTest.Create;
begin
  inherited Create;
  FEvents := TRegressionTestEventList.Create;
end;

destructor TRegressionTest.Destroy;
begin
  FreeAndNil(FEvents);  // I2794
  inherited Destroy;
end;

procedure TRegressionTest.ExecuteCurrentEvent;
begin
  if (FCurrentEvent < 0) or (FCurrentEvent >= FEvents.Count) then
    raise ERegressionTest.Create(ERTCurrentEventOutOfBounds);

  FEvents[FCurrentEvent].Execute;
end;

function TRegressionTest.GetDeadkeyCode(name: WideString): Integer;
var
  i: Integer;
begin
  Result := 1;
  for i := 0 to DebugKeyboard.Deadkeys.Count - 1 do
    if DebugKeyboard.Deadkeys[i].name = name then
    begin
      Result := DebugKeyboard.Deadkeys[i].Value+1;
      Exit;
    end;
end;

{-------------------------------------------------------------------------------
 - XML File Loading                                                            -
 ------------------------------------------------------------------------------}

const
  nl = #13#10;
  FDTD: WideString =
    '  <!ELEMENT regressiontest (info, events)>'+nl+
    '  <!ELEMENT info (version, systemkeyboard, keyboard, beginmode)>'+nl+
    '  <!ELEMENT version (#PCDATA)>'+nl+
    '  <!ELEMENT systemkeyboard (#PCDATA)>'+nl+
    '  <!ELEMENT keyboard (#PCDATA)>'+nl+
    '  <!ELEMENT beginmode (#PCDATA)>'+nl+
    '  <!ELEMENT events (event+)>'+nl+
    '  <!ELEMENT event (key, postcontext?)>'+nl+
    '  <!ELEMENT key (shiftstate?, vkey)>'+nl+
    '  <!ELEMENT shiftstate (shift?, ctrl?, rctrl?, alt?, altgr?, caps?)>'+nl+
    '  <!ELEMENT shift EMPTY>'+nl+
    '  <!ELEMENT ctrl EMPTY>'+nl+
    '  <!ELEMENT rctrl EMPTY>'+nl+
    '  <!ELEMENT alt EMPTY>'+nl+
    '  <!ELEMENT altgr EMPTY>'+nl+
    '  <!ELEMENT caps EMPTY>'+nl+
    '  <!ELEMENT vkey (#PCDATA)>'+nl+
    '  <!ELEMENT postcontext ((text | deadkey)+)>'+nl+
    '  <!-- data in text and deadkey must not have extraneous space formatting as it will be read exactly as is -->'+nl+
    '  <!ELEMENT deadkey (#PCDATA)>'+nl+
    '  <!ELEMENT text (#PCDATA)>'+nl+
    '  <!ATTLIST text xml:space CDATA #IMPLIED>'+nl;

procedure TRegressionTest.Load(FileName: string);  // I3324
var
  doc: IXMLDocument;
  eventsNode, info, c1, key, ch: IXMLNode;
  s: WideString;
  ev: TRegressionTestEvent;
      function FindNode(NodeName: string; parent: IXMLNode = nil): IXMLNode;
      begin
        if parent = nil then parent := doc.DocumentElement;
        Result := parent.ChildNodes.FindNode(NodeName);
      end;
begin
  FTestFileName := '';

  doc := LoadXMLDocument(FileName);

  info := FindNode('info');
  if not Assigned(info) then
    raise ERegressionTestFile.Create('Invalid xml file: no info section');

  { version }
  ch := FindNode('version', info);
  if ch.NodeValue <> SKeymanVersion60 then
    raise ERegressionTestFile.Create('Invalid xml file: unrecognised version (should be '+SKeymanVersion60+')');

  { systemkeyboard }
  ch := FindNode('systemkeyboard', info);
  if StrToIntDef('$'+Trim(ch.NodeValue), 0) < $400 then
    raise ERegressionTestFile.Create('Invalid xml file: systemkeyboard value invalid');
  FSystemKeyboard := trim(ch.NodeValue);

  { keyboard }
  ch := FindNode('keyboard', info);
  if Trim(ch.NodeValue) = '' then
    raise ERegressionTestFile.Create('Invalid xml file: keyboard value invalid');
  FFileName := trim(ch.NodeValue);

  { beginmode }
  ch := FindNode('beginmode', info);
  s := Trim(ch.NodeValue);
  if (s <> 'ANSI') and (s <> 'Unicode') then
    raise ERegressionTestFile.Create('Invalid xml file: beginmode value invalid (can be ANSI or Unicode)');
  if s = 'ANSI' then FBeginMode := rtbmANSI else FBeginMode := rtbmUnicode;

  FEvents.Clear;

  eventsNode := FindNode('events');
  if not Assigned(eventsNode) then
    raise ERegressionTestFile.Create('Invalid xml file: no events');

  ch := FindNode('event', eventsNode); // Find the first event
  while assigned(ch) do
  begin
    ev := TRegressionTestEvent.Create(Self);
    FEvents.Add(ev);
    key := FindNode('key', ch);
    c1 := FindNode('vkey', key);
    ev.FVKey := FindVKeyName(Trim(VarToStr(c1.NodeValue)));
    if ev.FVKey = $FFFF then
      raise ERegressionTestFile.Create('Invalid xml file: vkey not valid: '+Trim(c1.NodeValue));

    ev.FShiftState := 0;
    c1 := FindNode('shiftstate', key);
    if Assigned(c1) then
    begin
      // Find all the shift states
      if Assigned(FindNode('shift', c1)) then ev.FShiftState := ev.FShiftState or K_SHIFTFLAG;
      if Assigned(FindNode('ctrl', c1)) then ev.FShiftState := ev.FShiftState or K_LCTRLFLAG;
      if Assigned(FindNode('rctrl', c1)) then ev.FShiftState := ev.FShiftState or K_RCTRLFLAG;
      if Assigned(FindNode('alt', c1)) then ev.FShiftState := ev.FShiftState or K_LALTFLAG;
      if Assigned(FindNode('altgr', c1)) then ev.FShiftState := ev.FShiftState or K_RALTFLAG;
      if Assigned(FindNode('caps', c1)) then ev.FShiftState := ev.FShiftState or K_CAPITALFLAG;
    end;

    ev.FPostContext := '';

    c1 := FindNode('postcontext', ch);
    if assigned(c1) then
    begin
      c1 := c1.ChildNodes.First;
      while Assigned(c1) do
      begin
        if c1.nodeName = 'text' then
          ev.FPostContext := ev.FPostContext + VarToStr(c1.NodeValue)
        else if c1.nodeName = 'deadkey' then
          ev.FPostContext := ev.FPostContext + WChr(UC_SENTINEL) + WChr(CODE_DEADKEY) +
            WChr(GetDeadkeyCode(Trim(VarToStr(c1.NodeValue))));
        c1 := c1.nextSibling;
      end;
    end;

    ch := ch.NextSibling;
    while Assigned(ch) and (ch.NodeName <> 'event') do
      ch := ch.NextSibling;
  end;

  FTestFileName := FileName;
end;

function TRegressionTest.XMLWideString: Widestring;
var
  i: Integer;
  s, ws: WideString;
begin
  ws := '<?xml version="1.0" encoding="UTF-8"?>'#13#10 +
        '<!DOCTYPE regressiontest SYSTEM "https://api.keyman.com/schemas/regtest/regtest.dtd">'#13#10+
        '<regressiontest>'#13#10 +
        '  <info>'#13#10 +
        '    <version>'+SKeymanVersion60+'</version>'#13#10 +
        '    <systemkeyboard>'+FSystemKeyboard+'</systemkeyboard>'#13#10 +
        '    <keyboard>'+FFileName+'</keyboard>'#13#10 +
        '    <beginmode>'+RegressionTestBeginModeText[FBeginMode]+'</beginmode>'#13#10+
        '  </info>'#13#10 +
        '  <events>'#13#10;

  for i := 0 to FEvents.Count - 1 do
  begin
    ws := ws + '    <event>'#13#10 +
               '       <key>'+FEvents[i].ShiftStateElement+'<vkey>'+VKeyNames[FEvents[i].Vkey]+'</vkey></key>'#13#10;
    s := FEvents[i].PostContextElement;
    if s <> '' then
      ws := ws + '       <postcontext>'+s+'</postcontext>'#13#10;
    ws := ws + '    </event>'#13#10;
  end;

  ws := ws + '  </events>';
  ws := ws + '</regressiontest>';

  Result := ws;
end;

procedure TRegressionTest.Save(FileName: string);
begin
  with TStringList.Create do  // I3310
  try
    Text := XMLWideString;  // I3310
    SaveToFile(FileName, TEncoding.UTF8);  // I3310
  finally
    Free;
  end;
  FTestFileName := FileName;
end;

procedure TRegressionTest.SetCurrentEvent(const Value: Integer);
begin
  if (Value <> 0) and ((Value < 0) or (Value >= FEvents.Count)) then
    raise ERegressionTest.Create(ERTCurrentEventOutOfBounds); // 0 is okay if there are no events in the list
  FCurrentEvent := Value;
end;

function TRegressionTestEvent.DeadKeyName(dkCode: Integer): WideString;
var
  i: Integer;
begin
  for i := 0 to FRegressionTest.DebugKeyboard.Deadkeys.Count - 1 do
    if FRegressionTest.DebugKeyboard.Deadkeys[i].Value+1 = dkCode then
    begin
      Result := FRegressionTest.DebugKeyboard.Deadkeys[i].Name;
      Exit;
    end;
  Result := '???';
end;

function TRegressionTestEvent.PostContextElement: WideString;
var
  ws: WideString;
  FInText: Boolean;
  i: Integer;
begin
  if FPostContext = '' then
  begin
    Result := '';
    Exit;
  end;
  i := 1;
  FInText := False;
  while i <= Length(FPostContext) do
  begin
    if Ord(FPostContext[i]) = UC_SENTINEL then
    begin
      if FInText then ws := ws + '</text>';
      FInText := False;
      ws := ws + '<deadkey>'+DeadKeyName(Ord(FPostContext[i+2]))+'</deadkey>';
      Inc(i, 2);
    end
    else
    begin
      if not FInText then ws := ws + '<text xml:space="preserve">';
      FInText := True;
      ws := ws + FPostContext[i];
    end;
    Inc(i);
  end;
  if FInText then ws := ws + '</text>';
  Result := ws;
end;

function TRegressionTestEvent.ShiftStateElement: WideString;
var
  ws: WideString;
begin
  Result := '';
  if FShiftState = 0 then Exit;

  ws := '';
  if (FShiftState and (K_CTRLFLAG or K_LCTRLFLAG)) <> 0 then ws := ws + '<ctrl/>';
  if (FShiftState and (K_RCTRLFLAG)) <> 0               then ws := ws + '<rctrl/>';
  if (FShiftState and (K_SHIFTFLAG)) <> 0               then ws := ws + '<shift/>';
  if (FShiftState and (K_ALTFLAG or K_LALTFLAG)) <> 0   then ws := ws + '<alt/>';
  if (FShiftState and (K_RALTFLAG)) <> 0                then ws := ws + '<altgr/>';
  if (FShiftState and (K_CAPITALFLAG)) <> 0             then ws := ws + '<capslock/>';

  Result := '<shiftstate>'+ws+'</shiftstate>';
end;

function TRegressionTestEvent.ShiftStateAsString: string;
var
  s: string;
      procedure Add(ss: string);
      begin
        s:=s + ss + ' + ';
      end;
begin
  Result := '';
  if FShiftState = 0 then Exit;
  s := '';
  if (FShiftState and (K_CTRLFLAG or K_LCTRLFLAG)) <> 0 then Add('Control');
  if (FShiftState and (K_RCTRLFLAG)) <> 0               then Add('RControl');
  if (FShiftState and (K_SHIFTFLAG)) <> 0               then Add('Shift');
  if (FShiftState and (K_ALTFLAG or K_LALTFLAG)) <> 0   then Add('Alt');
  if (FShiftState and (K_RALTFLAG)) <> 0                then Add('AltGr');
  if (FShiftState and (K_CAPITALFLAG)) <> 0             then Add('CapsLock');
  Result := s;
end;

procedure TRegressionTest.SetDebugKeyboard(const Value: TDebugKeyboard);
begin
  FDebugKeyboard := Value;
end;

constructor TRegressionTestEvent.Create(AOwner: TRegressionTest);
begin
  inherited Create;
  FRegressionTest := AOwner;
end;

procedure TRegressionTest.Clear;
begin
  FEvents.Clear;
  FTestFileName := '';
end;

end.
