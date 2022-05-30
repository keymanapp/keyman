(*
  Name:             OnScreenKeyboard
  Copyright:        Copyright (C) SIL International.
  Documentation:
  Description:
  Create Date:      23 Aug 2006

  Modified Date:    24 Jul 2015
  Authors:          mcdurdin
  Related Files:
  Dependencies:

  Bugs:
  Todo:
  Notes:
  History:          23 Aug 2006 - mcdurdin - Initial version
                    30 Aug 2006 - mcdurdin - Fix underlying layout published bug
                    14 Sep 2006 - mcdurdin - Add Transparent flag, retrieve aspected size, polish drawing
                    04 Dec 2006 - mcdurdin - Fix display of bitmaps, underlying scan codes
                    30 May 2007 - mcdurdin - I763 - Key names for non-character keys should follow active locale
                    04 Jun 2007 - mcdurdin - I763 - Fix size of font for non-character keys
                    04 Jun 2007 - mcdurdin - I764 - Fix layout of ENTER key for European keyboards
                    05 Jun 2007 - mcdurdin - I763 - Add GetItemsByScanCode for verifying UK keyboard 102 key
                    19 Nov 2007 - mcdurdin - I1157 - const string parameters
                    27 Mar 2008 - mcdurdin - I1227, I904, I1102 - Fix crash exporting OSK
                    06 Apr 2010 - mcdurdin - I2262 - Improve performance of OSK when resizing
                    06 Apr 2010 - mcdurdin - I2200 - Fix conflict in OSK and mnemonic layouts
                    06 Apr 2010 - mcdurdin - I764 - Distinguish between 102nd key and European layouts
                    11 Jan 2011 - mcdurdin - I764 - Fixup problems identifying UK layout
                    31 Jan 2011 - mcdurdin - I2576 - Fix positioning and size of text on OSK keys
                    19 Aug 2011 - mcdurdin - I3021 - Backslash key still shows wide keycap in OSK even on Euro layout with narrower key
                    08 Jun 2012 - mcdurdin - I3309 - V9.0 - Migrate to Delphi XE2, VS2010, svn 1.7
                    07 Nov 2013 - mcdurdin - I3945 - V9.0 - Touch Layout Editor should allow import from existing On Screen Keyboard
                    28 Feb 2014 - mcdurdin - I4098 - V9.0 - OSK is still 8.0 style
                    12 Aug 2014 - mcdurdin - I4363 - V9.0 - OSK does not always show base layout when keyboard active
                    02 Oct 2014 - mcdurdin - I4415 - V9.0 - OSK does not show underlying characters if base keyboard is not loaded
                    24 Jul 2015 - mcdurdin - I4799 - Preview keys are wrong colour in Developer
*)
unit OnScreenKeyboard;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls,
  ExtShiftState, StdCtrls, Buttons, VisualKeyboardParameters, Contnrs,
  OnScreenKeyboardData,
  CleartypeDrawCharacter;

type
  TOnScreenKeyboard = class;
  TOnScreenKeyboardKey = class;

  TOnScreenKeyboardKeyPressedEvent = procedure(Sender: TOnScreenKeyboard; Key: TOnScreenKeyboardKey) of object;

  TOnScreenKeyboardKey = class
  private
    FKeyboard: TOnScreenKeyboard;
    FX, FY, FW, FH, FX2, FY2: Integer;
    FDefaultPos: TRect;
    FScanCode: Integer;
    FUSVKey, FVKey: Integer;
    FKeyType: TOnScreenKeyboardKeyType;
    FKeyCaps: array[0..High(ValidExtShiftStates)] of WideString;
    FFontSize: Integer;
    FKeyValue: WideString;
    FEnabled: Boolean;
    FKeyGlyph: TBitmap;
    FData: TOnScreenKeyboardKeyData;   // I3945
    procedure SetKeyCap(Index: Integer; Value: WideString);
    procedure SetKeyValue(Value: WideString);
    procedure DrawKey(DestCanvas: TCanvas; FIsDown, FIsHover, FIsSelected: Boolean);
    function CalcFontSize(Canvas: TCanvas): Integer;
    procedure SetEnabled(const Value: Boolean);
    procedure SetKeyGlyph(const Value: TBitmap);
    procedure Invalidate;
    function GetKeyRect: TRect;
    procedure UpdateKeyCap;
    function GetKeyCap(Index: Integer): WideString;
    function GetActiveKeyCap: WideString;
  public
    constructor Create(AKeyboard: TOnScreenKeyboard; AData: TOnScreenKeyboardKeyData);
    destructor Destroy; override;
    property ScanCode: Integer read FScanCode;
    property VKey: Integer read FVKey;
    property USVKey: Integer read FUSVKey;
    property KeyType: TOnScreenKeyboardKeyType read FKeyType;
    property KeyCaps[Index: Integer]: WideString read GetKeyCap write SetKeyCap;
    property ActiveKeyCap: WideString read GetActiveKeyCap;
    property KeyValue: WideString read FKeyValue write SetKeyValue;
    property KeyGlyph: TBitmap read FKeyGlyph write SetKeyGlyph;
    property KeyRect: TRect read GetKeyRect;
    property KeyData: TOnScreenKeyboardKeyData read FData;   // I3945
    property Enabled: Boolean read FEnabled write SetEnabled default true;
    property FontSize: Integer read FFontSize write FFontSize;
  end;

  TOnScreenKeyboardKeys = class(TObjectList)
  private
    FVKeys: array[0..255] of TOnScreenKeyboardKey;
    FUSVKeys: array[0..255] of TOnScreenKeyboardKey;
    FKeysByType: array[TOnScreenKeyboardKeyType] of TOnScreenKeyboardKey;
    function GetItem(Index: Integer): TOnScreenKeyboardKey;
    function GetItemByVK(Index: Integer): TOnScreenKeyboardKey;
    procedure FillVKeys;
    function GetItemByKeyType(Index: TOnScreenKeyboardKeyType): TOnScreenKeyboardKey;
    function GetItemByUSVK(Index: Integer): TOnScreenKeyboardKey;
    function GetItemsByScanCode(Index: Integer): TOnScreenKeyboardKey;
    function GetItemsByKeyCap(Index: WideChar; Shift: Integer): TOnScreenKeyboardKey;
  public
    procedure ClearValues;
    property Items[Index: Integer]: TOnScreenKeyboardKey read GetItem; default;
    property ItemsByVK[Index: Integer]: TOnScreenKeyboardKey read GetItemByVK;
    property ItemsByUSVK[Index: Integer]: TOnScreenKeyboardKey read GetItemByUSVK;
    property ItemsByKeyType[Index: TOnScreenKeyboardKeyType]: TOnScreenKeyboardKey read GetItemByKeyType;
    property ItemsByKeyCap[Index: WideChar; Shift: Integer]: TOnScreenKeyboardKey read GetItemsByKeyCap;
    property ItemsByScanCode[Index: Integer]: TOnScreenKeyboardKey read GetItemsByScanCode;
  end;

  TQuickBitmap = class
  private
    FOldHBitmap, FHandle: THandle;
    FCanvas: TCanvas;
    function GetCanvas: TCanvas;
    //function GetHDC: THandle;
    //procedure ReleaseDC;
    //FHDC: THandle;
    //function GetHDC: THandle;
  public
    constructor Create(AWidth, AHeight: Integer);
    destructor Destroy; override;
    //procedure ReleaseDC;
    procedure ReleaseCanvas;
    property Handle: THandle read FHandle;
    property Canvas: TCanvas read GetCanvas;
    //property HDC: THandle read GetHDC;
  end;

  TOnScreenKeyboard = class(TCustomControl)
  private
    FScale: Extended;
    FDataFont: TFont;
    FDisplayUnderlyingChar: Boolean;
    F102Key, FHoverKey, FDownKey, FSelectedKey: TOnScreenKeyboardKey;
    FBufferBitmap, FBackgroundBitmap: TQuickBitmap;
    FKeys: TOnScreenKeyboardKeys;
    FOnSelectionChange: TNotifyEvent;
    FIsDown: Boolean;
    FUnderlyingLayout: THandle;
    FShiftState: TExtShiftState;
    FDisableExtendedKeys: Boolean;
    FLRShift: Boolean;
    FOnShiftChange: TNotifyEvent;
    FSelectMode: Boolean;
    FOnKeyPressed: TOnScreenKeyboardKeyPressedEvent;
    FDisplay102Key: Boolean;
    FEuroLayout: Boolean;
    FDrawChar: TCleartypeDrawCharacter;
    FTransparent: Boolean;
    FLargeCapFont: Boolean;

    FUpdateLevel: Integer;
    FUpdateKeys: TList;
    FUpdateInvalidate: Boolean;
    FLoadedKeyboardLayout: HKL;   // I4363

    procedure SetDataFont(const Value: TFont);
    procedure SetDisplayUnderlyingChar(const Value: Boolean);

    procedure WMEraseBkGnd(var msg: TWMEraseBkGnd); message WM_ERASEBKGND;
    procedure CMMouseLeave(var Message: TMessage); message CM_MOUSELEAVE;
    procedure SetSelectedKey(const Value: TOnScreenKeyboardKey);

    procedure MapScanCodes(FScanCode: Integer; var Values: array of WideString; var VKey: Integer);

    procedure DoDrawKey(Canvas: TCanvas; Key: TOnScreenKeyboardKey);

    procedure ResizeKeys;
    procedure SetShiftState(const Value: TExtShiftState);
    procedure SetLRShift(const Value: Boolean);
    procedure SetSelectMode(const Value: Boolean);
    procedure SetDisableExtendedKeys(const Value: Boolean);
    procedure SetDisplay102Key(const Value: Boolean);
    procedure SetUnderlyingLayout(const Value: THandle);
    procedure SetTransparent(const Value: Boolean);
    procedure FillBkRect(HDC: THandle; r: TRect);
    procedure SetLargeCapFont(const Value: Boolean);
    procedure CalcKeyFontSizes;
    procedure UpdateEuroLayout;
    procedure Resize102Key;

  protected
    procedure CreateParams(var params: TCreateParams); override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X: Integer; Y: Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X: Integer; Y: Integer); override;
    procedure MouseMove(Shift: TShiftState; X: Integer; Y: Integer); override;

    procedure SelectionChange; virtual;
    procedure ShiftChange; virtual;
    procedure KeyPressed(Key: TOnScreenKeyboardKey); virtual;

  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure BeginUpdate;
    procedure EndUpdate;

    procedure AdjustBoundsRect(var r: TRect; BiasX: Boolean);
    function GetKeyAtPoint(X, Y: Integer): TOnScreenKeyboardKey;

    procedure InvalidateKey(Key: TOnScreenKeyboardKey);
    procedure InvalidateShift(ShiftState: TExtShiftState);

    procedure Paint; override;
    procedure Resize; override;

    procedure Invalidate; override;

    property Keys: TOnScreenKeyboardKeys read FKeys;

    property UnderlyingLayout: THandle read FUnderlyingLayout write SetUnderlyingLayout;
    property EuroLayout: Boolean read FEuroLayout;  // I764

    property LargeCapFont: Boolean read FLargeCapFont write SetLargeCapFont;
    property LRShift: Boolean read FLRShift write SetLRShift;
    property ShiftState: TExtShiftState read FShiftState write SetShiftState;
    property SelectedKey: TOnScreenKeyboardKey read FSelectedKey write SetSelectedKey;
  published
    property OnSelectionChange: TNotifyEvent read FOnSelectionChange write FOnSelectionChange;
    property OnShiftChange: TNotifyEvent read FOnShiftChange write FOnShiftChange;
    property OnKeyPressed: TOnScreenKeyboardKeyPressedEvent read FOnKeyPressed write FOnKeyPressed;

    property DataFont: TFont read FDataFont write SetDataFont;
    property Display102Key: Boolean read FDisplay102Key write SetDisplay102Key default true;
    property DisableExtendedKeys: Boolean read FDisableExtendedKeys write SetDisableExtendedKeys default true;
    property DisplayUnderlyingChar: Boolean read FDisplayUnderlyingChar write SetDisplayUnderlyingChar;
    //property Extended

    property SelectMode: Boolean read FSelectMode write SetSelectMode default true;
    property Transparent: Boolean read FTransparent write SetTransparent default true;

    property Align;
    property Anchors;
    property Color;
    property OnDragOver;
    property OnDragDrop;
    property ParentColor;
    property Enabled;
    property TabOrder;
    property TabStop;
    property Font;
    property ParentFont;
    property Visible;
  end;

procedure Register;

const   // I4098   // I4799
  KeyFontColor_Disabled = $C3C1C0;
  KeyFontColor_Base = $5B5958;
  KeyFontColor_BaseSelected = $5B5958;
  KeyFontColor_Cap = $201f23;

implementation

uses
  System.Math,

  Glossary,
  ScanCodeMap,
  Types,
  Unicode;

{$R onscreenkeyboard.res}

function KeyTypeToExtShiftStateValue(KeyType: TOnScreenKeyboardKeyType; LRShift: Boolean): TExtShiftStateValue;
const
  ShiftStates: array[Boolean, TOnScreenKeyboardKeyType] of TExtShiftStateValue = (
    (TExtShiftStateValue(-1), TExtShiftStateValue(-1), TExtShiftStateValue(-1),
    essShift, essCtrl, essAlt,
    TExtShiftStateValue(-1), TExtShiftStateValue(-1),
    essShift, essCtrl, essAlt),
    (TExtShiftStateValue(-1), TExtShiftStateValue(-1), TExtShiftStateValue(-1),
    essShift, essLCtrl, essLAlt,
    TExtShiftStateValue(-1), TExtShiftStateValue(-1),
    essShift, essRCtrl, essRAlt));
begin
  Result := ShiftStates[LRShift, KeyType];
end;

procedure Register;
begin
  RegisterComponents('Keyman', [TOnScreenKeyboard]);
end;

procedure TOnScreenKeyboard.WMEraseBkGnd(var msg: TWMEraseBkGnd);
begin
  PerformEraseBackground(Self, FBackgroundBitmap.Canvas.Handle); // msg.DC);
  //BitBlt(FBackgroundBitmap.Canvas.Handle, 0, 0, Width, Height, msg.DC, 0, 0, SRCCOPY);
  //SetBkMode(msg.DC, Windows.TRANSPARENT);
  FillBkRect(FBufferBitmap.Canvas.Handle, Rect(0,0,Width,Height));
  msg.result := 1;
end;

procedure TOnScreenKeyboard.AdjustBoundsRect(var r: TRect; BiasX: Boolean);
var
  cx, cy, dx, dy: Integer;
  FScale: Extended;
begin
  cx := r.Right - r.Left;
  cy := r.Bottom - r.Top;

  with FKeys.ItemsByKeyType[kktCtrlRight] do
  begin
    dx := FDefaultPos.Right;
    dy := FDefaultPos.Bottom;
  end;

  if not BiasX then //(cx/dx) > (cy/dy) then
  begin
    // y constrains
    FScale := cy / dy;
    r.Right := r.Left + Trunc(dx * FScale);
  end
  else
  begin
    // x constrains
    FScale := cx / dx;
    r.Bottom := r.Top + Trunc(dy * FScale);
  end;
end;

procedure TOnScreenKeyboard.CMMouseLeave(var Message: TMessage);
var
  k: TOnScreenKeyboardKey;
begin
  if FHoverKey <> nil then
  begin
    k := FHoverKey;
    FHoverKey := nil;
    DoDrawKey(Canvas, k); //InvalidateKey(k);
  end;
  inherited;
end;

constructor TOnScreenKeyboard.Create(AOwner: TComponent);
var
  i: Integer;
  k: Integer;
begin
  inherited Create(AOwner);

  FUpdateKeys := TList.Create;

  Color := $F2F2F1;   // I4098

  FTransparent := True;
  FDrawChar := TCleartypeDrawCharacter.Create;

  FSelectMode := True;
  FDisplay102Key := True;
  FEuroLayout := False;

  FUnderlyingLayout := GetKeyboardLayout(0);

  F102Key := nil;
  FKeys := TOnScreenKeyboardKeys.Create;
  for i := 0 to High(KeyData) do
  begin
    k := FKeys.Add(TOnScreenKeyboardKey.Create(Self, KeyData[i]));
    if KeyData[i].ScanCode = $56 then F102Key := FKeys[k];
  end;
  FKeys.FillVKeys;

  DisableExtendedKeys := True;

  FDataFont := TFont.Create;
  FBackgroundBitmap := TQuickBitmap.Create(Width, Height);
  FBufferBitmap := TQuickBitmap.Create(Width, Height);
  ControlStyle := ControlStyle - [csOpaque];
  ParentBackground := True;

  UpdateEuroLayout;  // I764

  if HasParent then // I1227, I904, I1102 - crash export OSK to BMP, PNG
    CalcKeyFontSizes;
end;

procedure TOnScreenKeyboard.CalcKeyFontSizes;
var
  minsz: Integer;
  i: Integer;
  n: Integer;
  b: TBitmap;
begin
  b := TBitmap.Create;
  try
    minsz := MAXINT;
    for i := 0 to FKeys.Count - 1 do
      if FKeys[i].KeyType <> kktNormal then
      begin
        n := FKeys[i].CalcFontSize(b.Canvas);
        if (n > 0) and (n < minsz) then
          minsz := n;
      end;

    if minsz = MAXINT then minsz := 0;
    for i := 0 to FKeys.Count - 1 do
      if FKeys[i].KeyType <> kktNormal then
        FKeys[i].FontSize := minsz;
  finally
    b.Free;
  end;
end;

procedure TOnScreenKeyboard.CreateParams(var params: TCreateParams);
begin
  inherited CreateParams(params);
  //params.ExStyle := params.ExStyle or WS_EX_TRANSPARENT;
end;

destructor TOnScreenKeyboard.Destroy;
begin
  FDataFont.Free;
  FreeAndNil(FBackgroundBitmap);
  FreeAndNil(FBufferBitmap);
  FreeAndNil(FDrawChar);
  FKeys.Free;
  FUpdateKeys.Free;

  if FLoadedKeyboardLayout <> 0 then   // I4363
    UnloadKeyboardLayout(FLoadedKeyboardLayout);
  FLoadedKeyboardLayout := 0;

  inherited Destroy;
end;

procedure TOnScreenKeyboard.BeginUpdate;
begin
  if FUpdateLevel = 0 then
  begin
    FUpdateInvalidate := False;
    FUpdateKeys.Clear;
  end;

  Inc(FUpdateLevel);
end;

procedure TOnScreenKeyboard.EndUpdate;
var
  i: Integer;
begin
  Dec(FUpdateLevel);
  if FUpdateLevel = 0 then
  begin
    if FUpdateInvalidate then
    begin
      Invalidate;
    end
    else
      for i := 0 to FUpdateKeys.Count - 1 do
        DoDrawKey(Canvas, TOnScreenKeyboardKey(FUpdateKeys[i]));
    FUpdateKeys.Clear;
  end;
end;
procedure TOnScreenKeyboard.DoDrawKey(Canvas: TCanvas; Key: TOnScreenKeyboardKey);
begin
  if not Assigned(Key) then Exit;
  if not Showing or (csCreating in ControlState) then Exit;
  if not FDisplay102Key and not FEuroLayout and (Key = F102Key) then Exit;

  if FUpdateLevel > 0 then
  begin
    if FUpdateKeys.IndexOf(Key) < 0 then
      FUpdateKeys.Add(Key);
  end
  else
    Key.DrawKey(Canvas, FDownKey = Key, FHoverKey = Key, (FSelectedKey = Key) or (KeyTypeToExtShiftStateValue(Key.KeyType, FLRShift) in FShiftState));
end;

procedure TOnScreenKeyboard.SelectionChange;
begin
  if Assigned(FOnSelectionChange) then
    FOnSelectionChange(Self);
end;

procedure TOnScreenKeyboard.SetDataFont(const Value: TFont);
begin
  FDataFont.Assign(Value);
  Invalidate;
end;

procedure TOnScreenKeyboard.SetSelectedKey(const Value: TOnScreenKeyboardKey);
var
  k: TOnScreenKeyboardKey;
begin
  k := FSelectedKey;
  FSelectedKey := Value;
  if Assigned(k) then DoDrawKey(Canvas, k);//InvalidateKey(k);
  if Assigned(FSelectedKey) then DoDrawKey(Canvas, FSelectedKey);//InvalidateKey(FSelectedKey);
end;

procedure TOnScreenKeyboard.SetSelectMode(const Value: Boolean);
var
  k: TOnScreenKeyboardKey;
begin
  FSelectMode := Value;
  if not Value then
  begin
    k := FSelectedKey;
    FSelectedKey := nil;
    if k <> nil then DoDrawKey(Canvas, k);
  end;
end;

procedure TOnScreenKeyboard.SetShiftState(const Value: TExtShiftState);
begin
  if FShiftState <> Value then
  begin
    FShiftState := Value;
    Invalidate;
  end;
end;

procedure TOnScreenKeyboard.SetTransparent(const Value: Boolean);
begin
  FTransparent := Value;
  if HandleAllocated then Invalidate;
end;

procedure TOnScreenKeyboard.SetUnderlyingLayout(const Value: THandle);
var
  i: Integer;
  FLastKeyboardLayout: Cardinal;
begin
  if FUnderlyingLayout <> Value then
  begin
    FUnderlyingLayout := Value;

    FLastKeyboardLayout := GetKeyboardLayout(0);
    if FLastKeyboardLayout <> FUnderlyingLayout then   // I4363   // I4415
    begin
      if FLoadedKeyboardLayout <> 0 then
        UnloadKeyboardLayout(FLoadedKeyboardLayout);

      if ActivateKeyboardLayout(FUnderlyingLayout, KLF_NOTELLSHELL) = 0
        then FLoadedKeyboardLayout := LoadKeyboardLayout(IntToHex(FUnderlyingLayout,8), KLF_ACTIVATE or KLF_NOTELLSHELL)
        else FLoadedKeyboardLayout := 0;
    end;

    for i := 0 to FKeys.Count - 1 do
      FKeys[i].UpdateKeyCap;

    UpdateEuroLayout;

    CalcKeyFontSizes;

    if HKLToKeyboardID(FLastKeyboardLayout) <> FUnderlyingLayout then   // I4363
      ActivateKeyboardLayout(FLastKeyboardLayout, KLF_NOTELLSHELL);

    FKeys.FillVKeys;
  end;
end;

procedure TOnScreenKeyboard.UpdateEuroLayout;
var
  k102, kbackslash: UINT;
  Value: Boolean;
begin
  if GetKeyboardType(0) in [1,2,3,4] then  // I764
  begin
    k102 := MapVirtualKeyExW($56, 1, FUnderlyingLayout);
    kbackslash := MapVirtualKeyExW($2b, 1, FUnderlyingLayout);

    if k102 <> 0 then k102 := MapVirtualKeyExW(k102, 2, FUnderlyingLayout);
    if kbackslash <> 0 then kbackslash := MapVirtualKeyExW(kbackslash, 2, FUnderlyingLayout);

    Value := (kbackslash <> k102) and (k102 <> 0);
  end
  else
    Value := False;

  if FEuroLayout <> Value then
  begin
    FEuroLayout := Value;
    Resize102Key;
  end;
end;

procedure TOnScreenKeyboard.ShiftChange;
begin
  if Assigned(FOnShiftChange) then
    FOnShiftChange(Self);
end;

procedure TOnScreenKeyboard.Invalidate;
begin
  if FUpdateLevel > 0 then
    FUpdateInvalidate := True
  else
    inherited Invalidate;
end;

procedure TOnScreenKeyboard.InvalidateKey(Key: TOnScreenKeyboardKey);
var
  r: TRect;
begin
  if not Assigned(key) then Exit;

  r := Rect(Key.FX, Key.FY, Key.FX+Key.FW, Key.FY+Key.FH);
  InvalidateRect(Handle, @r, False);
end;

procedure TOnScreenKeyboard.InvalidateShift(ShiftState: TExtShiftState);
begin
  if essShift in ShiftState then
  begin
    InvalidateKey(Keys.ItemsByKeyType[kktShiftLeft]);
    InvalidateKey(Keys.ItemsByKeyType[kktShiftRight]);
  end;

  if [essCtrl, essLCtrl] * ShiftState <> [] then InvalidateKey(Keys.ItemsByKeyType[kktCtrlLeft]);
  if [essCtrl, essRCtrl] * ShiftState <> [] then InvalidateKey(Keys.ItemsByKeyType[kktCtrlRight]);
  if [essAlt, essLAlt] * ShiftState <> [] then InvalidateKey(Keys.ItemsByKeyType[kktAltLeft]);
  if [essAlt, essRAlt] * ShiftState <> [] then InvalidateKey(Keys.ItemsByKeyType[kktAltRight]);
end;

procedure TOnScreenKeyboard.KeyPressed(Key: TOnScreenKeyboardKey);
begin
  if Assigned(FOnKeyPressed) then
    FOnKeyPressed(Self, Key);
end;

function TOnScreenKeyboard.GetKeyAtPoint(X, Y: Integer): TOnScreenKeyboardKey;
var
  i: Integer;
begin
  for i := 0 to FKeys.Count - 1 do
  begin
    with FKeys[i] do
      if PtInRect(KeyRect, Point(X, Y)) and (FDisplay102Key or FEuroLayout or (FKeys[i] <> F102Key)) then
      begin
        Result := FKeys[i];
        Exit;
      end;
  end;
  Result := nil;
end;

var
  FRunningNT: Boolean = False;
  FScanCodeDecimal: Integer = 0;

procedure TOnScreenKeyboard.MapScanCodes(FScanCode: Integer; var Values: array of WideString; var vkey: Integer);
var
  //vk: Integer;
  //hUser32: THandle;
  buf: array[0..9] of WideChar;
  keystate: TKeyboardState;
  n: Integer;
  i: Integer;

    procedure SetKeyStateFromShiftState(FShiftState: TExtShiftState);
    const
      ShiftStateValues: array[TExtShiftStateValue] of Integer = (VK_SHIFT, VK_CONTROL, VK_MENU, VK_LCONTROL, VK_RCONTROL, VK_LMENU, VK_RMENU);
    var
      i: TExtShiftStateValue;
    begin
      if essRAlt in FShiftState then
        FShiftState := FShiftState - [essRalt] + [essCtrl, essAlt];
      for i := Low(i) to High(i) do
        if i in FShiftState
          then keystate[ShiftStateValues[i]] := $80;
    end;

    procedure ClearKeyboardBuffer;
    var
      keystate: TKeyboardState;
    begin
      FillChar(keystate, sizeof(TKeyboardState), 0);  // I3309
      while not ToUnicodeEx(VK_DECIMAL, FScanCodeDecimal, keystate, buf, 10, 0, FUnderlyingLayout) in [0, 1] do;  // I3309
    end;

begin
  if FScanCodeDecimal = 0 then
    FScanCodeDecimal := MapVirtualKeyEx(VK_DECIMAL, 0, FUnderlyingLayout);

  vkey := MapVirtualKeyEx(FScanCode, 1, FUnderlyingLayout);  // I3309
  //ch := FMapVirtualKeyEx(vk, 2, FUnderlyingLayout);

  for i := Low(Values) to High(Values) do
  begin
    FillChar(keystate, sizeof(TKeyboardState), 0);  // I3309
    SetKeyStateFromShiftState(ValidExtShiftStates[i]);

    n := ToUnicodeEx(vkey, FScanCode, keystate, buf, 10, 0, FUnderlyingLayout);  // I3309
    case n of
      -1: Values[i] := Copy(buf, 1, 1);
      0: Values[i] := '';
      else Values[i] := Copy(buf, 1, n);
    end;
    if n < 0 then ClearKeyboardBuffer;
  end;
end;

procedure TOnScreenKeyboard.MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  k: TOnScreenKeyboardKey;
begin
  if Button = mbLeft then
  begin
    k := GetKeyAtPoint(X, Y);
    if Assigned(k) and k.Enabled then
    begin
      FHoverKey := k;
      FDownKey := k;
      FIsDown := True;
      DoDrawKey(Canvas, FDownKey);//InvalidateKey(FDownKey);
    end;
  end;
end;

procedure TOnScreenKeyboard.MouseMove(Shift: TShiftState; X, Y: Integer);
var
  k, k2: TOnScreenKeyboardKey;
begin
  k := GetKeyAtPoint(X, Y);
  if k <> FHoverKey then
  begin
    if FIsDown then
    begin
      if k = FDownKey
        then FHoverKey := k
        else FHoverKey := nil;
      DoDrawKey(Canvas, FDownKey);
    end
    else
    begin
      k2 := FHoverKey;
      if Assigned(k) and not k.Enabled then k := nil;

      FHoverKey := k;
      DoDrawKey(Canvas, k); //InvalidateKey(k);
      DoDrawKey(Canvas, k2); //InvalidateKey(k2);
      Update;
    end;
  end;
end;

procedure TOnScreenKeyboard.MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  k, k2: TOnScreenKeyboardKey;
  ess: TExtShiftStateValue;
begin
  if FIsDown then
  begin
    FIsDown := False;
    k := GetKeyAtPoint(X, Y);
    if k = FDownKey then
    begin
      case k.KeyType of
        kktNormal:
          begin
            if SelectMode then
            begin
              k2 := FSelectedKey;
              FSelectedKey := k;
              if k2 <> nil then DoDrawKey(Canvas, k2);
              SelectionChange;
            end
            else
              KeyPressed(k);
          end;
        kktShiftLeft, kktCtrlLeft, kktAltLeft, kktShiftRight, kktCtrlRight, kktAltRight:
          begin
            ess := KeyTypeToExtShiftStateValue(k.KeyType, FLRShift);
            if FLRShift then
              case ess of
                essLCtrl: Exclude(FShiftState, essRCtrl);
                essRCtrl: Exclude(FShiftState, essLCtrl);
                essLAlt: Exclude(FShiftState, essRAlt);
                essRAlt: Exclude(FShiftState, essLAlt);
              end;

            if ess in FShiftState
              then Exclude(FShiftState, ess)
              else Include(FShiftState, ess);

            InvalidateShift([KeyTypeToExtShiftStateValue(k.KeyType, False)]);

            ShiftChange;
          end;
        else //kktTab, kktCaps, kktBackSpace, kktEnter:
          // later, fire the keystroke
          KeyPressed(k);
          ;
      end;
    end;
    k := FDownKey;
    FDownKey := nil;
    if k <> nil then DoDrawKey(Canvas, k); //InvalidateKey(k);
    if (k <> FSelectedKey) and (FSelectedKey <> nil) then DoDrawKey(Canvas, FSelectedKey); //InvalidateKey(FSelectedKey);
  end;
end;

procedure TOnScreenKeyboard.SetDisableExtendedKeys(const Value: Boolean);
begin
  if FDisableExtendedKeys <> Value then
  begin
    FDisableExtendedKeys := Value;
    Keys.ItemsByKeyType[kktTab].Enabled := not Value;
    Keys.ItemsByKeyType[kktCaps].Enabled := not Value;
    Keys.ItemsByKeyType[kktEnter].Enabled := not Value;
    Keys.ItemsByKeyType[kktBackSpace].Enabled := not Value;
  end;
end;

procedure TOnScreenKeyboard.FillBkRect(HDC: THandle; r: TRect);
var
  hbr: Cardinal;
begin
  if FTransparent then
  begin
    BitBlt(HDC, r.Left, r.Top, r.Right - r.Left,
      r.Bottom - r.Top, FBackgroundBitmap.Canvas.Handle, r.Left, r.Top,
      SRCCOPY);
    //Canvas.CopyRect(r, FBackgroundBitmap.Canvas, r);
  end
  else
  begin
    hbr := CreateSolidBrush(ColorToRGB(Color));
    FillRect(HDC, r, hbr);
    DeleteObject(hbr);
    //Canvas.Brush.Color := Color;
    //Canvas.FillRect(r);
  end;
end;

procedure TOnScreenKeyboard.SetDisplay102Key(const Value: Boolean);
begin
  if FDisplay102Key <> Value then
  begin
    FDisplay102Key := Value;
    Resize102Key;
  end;
end;

procedure TOnScreenKeyboard.Resize102Key;
var
  k: TOnScreenKeyboardKey;
  r: TRect;
begin
  k := FKeys.ItemsByKeyType[kktShiftLeft];
  if not FDisplay102Key and not FEuroLayout
    then k.FDefaultPos.Right := F102Key.FDefaultPos.Right
    else k.FDefaultPos.Right := F102Key.FDefaultPos.Left - 2;
  k.FW := Trunc(FScale*(k.FDefaultPos.Right))-k.FX;

  ResizeKeys;

  if HandleAllocated then
  begin
    if FDisplay102Key or FEuroLayout then
    begin
      r := Rect(k.FX, k.FY, F102Key.FX+F102Key.FW, F102Key.FY+F102Key.FH);
      FillBkRect(Canvas.Handle, r);
    end;
    DoDrawKey(Canvas, k);
    DoDrawKey(Canvas, F102Key);
  end;
end;

procedure TOnScreenKeyboard.SetDisplayUnderlyingChar(const Value: Boolean);
begin
  FDisplayUnderlyingChar := Value;
  Invalidate;
end;

procedure TOnScreenKeyboard.SetLargeCapFont(const Value: Boolean);
begin
  FLargeCapFont := Value;
  Invalidate;
end;

procedure TOnScreenKeyboard.SetLRShift(const Value: Boolean);
var
  FChanged: Boolean;
begin
  FLRShift := Value;
  FChanged := False;

  if FLRShift then
  begin
    if essCtrl in FShiftState then
    begin
      FShiftState := FShiftState - [essCtrl] + [essLCtrl];
      FChanged := True;
    end;

    if essAlt in FShiftState then
    begin
      FShiftState := FShiftState - [essAlt] + [essLAlt];
      FChanged := True;
    end;

    {FKeys.ItemsByKeyType[kktCtrlLeft].KeyCaps[0] := 'L Ctrl';
    FKeys.ItemsByKeyType[kktCtrlRight].KeyCaps[0] := 'R Ctrl';
    FKeys.ItemsByKeyType[kktAltLeft].KeyCaps[0] := 'L Alt';
    FKeys.ItemsByKeyType[kktAltRight].KeyCaps[0] := 'R Alt';}
  end
  else
  begin
    if (essLCtrl in FShiftState) or (essRCtrl in FShiftState) then
    begin
      FShiftState := FShiftState - [essLCtrl, essRCtrl] + [essCtrl];
      FChanged := True;
    end;

    if (essLAlt in FShiftState) or (essRAlt in FShiftState) then
    begin
      FShiftState := FShiftState - [essLAlt, essRAlt] + [essAlt];
      FChanged := True;
    end;

    {FKeys.ItemsByKeyType[kktCtrlLeft].KeyCaps[0]:= 'Ctrl';
    FKeys.ItemsByKeyType[kktCtrlRight].KeyCaps[0] := 'Ctrl';
    FKeys.ItemsByKeyType[kktAltLeft].KeyCaps[0] := 'Alt';
    FKeys.ItemsByKeyType[kktAltRight].KeyCaps[0] := 'Alt';}
  end;

  if FChanged then
  begin
    { update shift state ... }
    InvalidateShift([essCtrl]);
    InvalidateShift([essAlt]);
    //ShiftChange;
  end;
end;

procedure TOnScreenKeyboard.Paint;
var
  i: Integer;
  FClipped: Boolean;
  r: TRect;
begin
  FClipped := not IsRectEmpty(Canvas.ClipRect);
  for i := 0 to FKeys.Count - 1 do
    if not FClipped or IntersectRect(r, Canvas.ClipRect, FKeys[i].KeyRect) then
      DoDrawKey(FBufferBitmap.Canvas, FKeys[i]);
  if FClipped then
    with Canvas.ClipRect do
      BitBlt(Canvas.Handle, Left, Top, Right-Left, Bottom-Top, FBufferBitmap.Canvas.Handle, Left, Top, SRCCOPY)
  else
    BitBlt(Canvas.Handle, 0, 0, Width, Height, FBufferBitmap.Canvas.Handle, 0, 0, SRCCOPY);

  FBufferBitmap.ReleaseCanvas;
end;

procedure TOnScreenKeyboard.Resize;
begin
  inherited;
  FBackgroundBitmap.Free;
  FBackgroundBitmap := TQuickBitmap.Create(Width, Height);
  //FBackgroundBitmap.Width := Width;
  //FBackgroundBitmap.Height := Height;
  FBufferBitmap.Free;
  FBufferBitmap := TQuickBitmap.Create(Width, Height);
  //FBufferBitmap.Width := Width;
  //FBufferBitmap.Height := Height;
  ResizeKeys;
end;

procedure TOnScreenKeyboard.ResizeKeys;
var
  i: Integer;
  cx, cy, dx, dy: Integer;
  ox, oy: Integer;
begin

  cx := Width;
  cy := Height;

  with FKeys.ItemsByKeyType[kktCtrlRight] do
  begin
    dx := FDefaultPos.Right;
    dy := FDefaultPos.Bottom;
  end;

  if (cx/dx) > (cy/dy) then
  begin
    // y constrains
    FScale := cy / dy;
    oy := 0;
    ox := Trunc(cx - FScale * dx) div 2;
  end
  else
  begin
    // x constrains
    FScale := cx / dx;
    oy := Trunc(cy - FScale * dy) div 2;
    ox := 0;
  end;

  for i := 0 to FKeys.Count - 1 do
  begin
    with FKeys[i], FDefaultPos do
    begin
      if FScanCode = $2B then	// I3021
        FKeys[i].FDefaultPos.Right := 42 + FKeys[i].FDefaultPos.Left;  // I3021

      FX := Trunc(FScale*Left) + ox;
      FY := Trunc(FScale*Top) + oy;
      FW := Trunc(FScale*(Right))-FX + ox;
      FH := Trunc(FScale*(Bottom))-FY + oy;

      if FEuroLayout then
      begin
        if FScanCode = $2B then
        begin
          FX := Trunc(FScale * 432) + ox;
          FY := Trunc(FScale * 68) + oy;
          FW := Trunc(FScale * 33);
          FKeys[i].FDefaultPos.Right := 33 + FKeys[i].FDefaultPos.Left;  // I3021
        end
        else if FKeyType = kktEnter then
        begin
          FX := Trunc(FScale * 458) + ox;
          FY := Trunc(FScale * 34) + oy;
          FX2 := Trunc(FScale * (432+34)) + ox;
          FY2 := Trunc(FScale * (34+33)) + oy;
          FW := Trunc(FScale * 42);
          FH := Trunc(FScale * 67);
        end;
      end;
    end;
  end;

  CalcKeyFontSizes;

  if HandleAllocated then
    InvalidateRect(Handle, nil, True);
end;

{ TOnScreenKeyboardKeys }

procedure TOnScreenKeyboardKeys.ClearValues;
var
  i: Integer;
begin
  for i := 0 to Count - 1 do
  begin
    Items[i].KeyValue := '';
    Items[i].KeyGlyph := nil;
  end;
end;

procedure TOnScreenKeyboardKeys.FillVKeys;
var
  i: Integer;
begin
  for i := Low(FVKeys) to High(FVKeys) do FVKeys[i] := nil;
  for i := Low(FUSVKeys) to High(FUSVKeys) do FVKeys[i] := nil;

  for i := 0 to Count - 1 do
  begin
    if Items[i].VKey > 0 then
      FVKeys[Items[i].VKey] := Items[i];
    if Items[i].USVKey > 0 then
      FUSVKeys[Items[i].USVKey] := Items[i];
  end;
end;

function TOnScreenKeyboardKeys.GetItem(Index: Integer): TOnScreenKeyboardKey;
begin
  Result := inherited GetItem(Index) as TOnScreenKeyboardKey;
end;

function TOnScreenKeyboardKeys.GetItemByKeyType(
  Index: TOnScreenKeyboardKeyType): TOnScreenKeyboardKey;
var
  i: Integer;
begin
  if FKeysByType[Index] = nil then
  begin
    for i := 0 to Count - 1 do
      if Items[i].KeyType <> kktNormal then
        FKeysByType[Items[i].KeyType] := Items[i];
  end;
  Result := FKeysByType[Index];
end;

function TOnScreenKeyboardKeys.GetItemByUSVK(
  Index: Integer): TOnScreenKeyboardKey;
begin
  Assert((Index >= 0) and (Index <= 255));
  Result := FUSVKeys[Index];
end;

function TOnScreenKeyboardKeys.GetItemByVK(Index: Integer): TOnScreenKeyboardKey;
begin
  Assert((Index >= 0) and (Index <= 255));
  Result := FVKeys[Index];
end;

function TOnScreenKeyboardKeys.GetItemsByKeyCap(
  Index: WideChar; Shift: Integer): TOnScreenKeyboardKey;
var
  i: Integer;
begin
  for i := 0 to Count - 1 do
    if Items[i].KeyCaps[Shift] = Index then
    begin
      Result := Items[i];
      Exit;
    end;
  Result := nil;
end;

function TOnScreenKeyboardKeys.GetItemsByScanCode(
  Index: Integer): TOnScreenKeyboardKey;
var
  i: Integer;
begin
  for i := 0 to Count - 1 do
    if Items[i].ScanCode = Index then
    begin
      Result := Items[i];
      Exit;
    end;
  Result := nil;
end;

{ TOnScreenKeyboardKey }

function TOnScreenKeyboardKey.CalcFontSize(Canvas: TCanvas): Integer;
var
  ARect: TRect;
  R: TRect;
begin
  Result := 0;
  with Canvas do
  begin
    if FKeyType = kktNormal then Exit;

    Font.Size := Trunc(OnScreen_KeyBitmap.Parameters.CapFontSize*FKeyboard.FScale);
    FKeyboard.FDrawChar.SetFontDetails(Font.Name, Font.Height);

    R := Rect(FX, FY, FX+FW, FY+FH);

    with OnScreen_KeyBitmap.Parameters.TextRect[0] do
    begin
      ARect := Rect(R.Left+Trunc(Left*FKeyboard.FScale), R.Top+Trunc(1*FKeyboard.FScale),
        R.Left+Trunc(Right*FKeyboard.FScale), R.Top+FH);

      if FKeyType <> kktNormal then
        ARect.Right := R.Right - 4;
    end;

    //Result := TextWidth(FKeyCaps[0]);
    Result := FKeyboard.FDrawChar.CalcTextSize(Handle, ARect, FKeyCaps[0]);
  end;
end;

constructor TOnScreenKeyboardKey.Create(AKeyboard: TOnScreenKeyboard;
  AData: TOnScreenKeyboardKeyData);
begin
  inherited Create;
  FData := AData;   // I3945
  FEnabled := True;
  FKeyboard := AKeyboard;
  FDefaultPos := Rect(AData.X, AData.Y, AData.X+AData.Width, AData.Y+33);
  FX := AData.X; FY := AData.Y; FW := AData.Width; FH := 33;
  FScanCode := AData.ScanCode;
  FKeyType := AData.KeyType;
  UpdateKeyCap;
end;

destructor TOnScreenKeyboardKey.Destroy;
begin
  FreeAndNil(FKeyGlyph);
  inherited Destroy;
end;

procedure
 TransparentStretchBlt2(hDC: HDC; left, top, width, height: Integer; hBitmap: THandle; bmLeft, bmTop, bmWidth, bmHeight: Integer; colorMask: TColor);
var
  hMemDC, hStretchDC, hMaskDC: THandle;
  hStretchBm, hMaskBm, hOldMemBm, hOldStretchBm, hOldMaskBm: THandle;
  oldColor: COLORREF;
begin
    // Create the memory DC's.
    hMemDC := CreateCompatibleDC(hDC);
    hStretchDC := CreateCompatibleDC(hDC);
    hMaskDC := CreateCompatibleDC(hDC);

    // Create the bitmaps needed for the memory DC's.
    hStretchBm := CreateCompatibleBitmap(hDC, width, height);
    hMaskBm := CreateBitmap(width, height, 1, 1, nil);

    // Select the bitmaps into the memory DC's.
    hOldMemBm := SelectObject(hMemDC, hBitmap);
    hOldStretchBm := SelectObject(hStretchDC, hStretchBm);
    hOldMaskBm := SelectObject(hMaskDC, hMaskBm);

    // StretchBlt to a colored DC.
    StretchBlt(hStretchDC, 0, 0, width, height,
               hMemDC, bmLeft, bmTop, bmWidth, bmHeight, SRCCOPY);

    // BitBlt to a monochrome DC.
    oldColor := SetBkColor(hStretchDC, colorMask and $FFFFFF);
    BitBlt(hMaskDC, 0, 0, width, height,
           hStretchDC, 0, 0, SRCCOPY);
    SetBkColor(hStretchDC, oldColor);

    // Transparent BitBlt technique.
    BitBlt(hDC, left, top, width, height,
           hMaskDC, 0, 0, SRCAND);
    BitBlt(hDC, left, top, width, height,
           hStretchDC, 0, 0, SRCPAINT);

    // Restore the memory DC's.
    SelectObject(hMemDC, hOldMemBm);
    SelectObject(hStretchDC, hOldStretchBm);
    SelectObject(hMaskDC, hOldMaskBm);

    // Delete the allocated bitmaps.
    DeleteObject(hStretchBm);
    DeleteObject(hMaskBm);

    // Delete the allocated memory DC's.
    DeleteDC(hMemDC);
    DeleteDC(hStretchDC);
    DeleteDC(hMaskDC);
end;

procedure TOnScreenKeyboardKey.DrawKey(DestCanvas: TCanvas; FIsDown, FIsHover, FIsSelected: Boolean);
var
  ARect, RGlyph, R: TRect;

  procedure DrawKeyBitmap(Canvas: TCanvas);
      function CompareRect(r1, r2: TRect): Boolean;
      begin
        Result := (r1.Left = r2.Left) and (r1.Top = r2.Top) and (r1.Right = r2.Right) and (r1.Bottom = r2.Bottom);
      end;

  var
    bIndex, n: Integer;
    ssi: Integer;
    Save: THandle;
    MaskDC: THandle;
    s: string;
    FTextExtent: TSize;
    tm: TTextMetric;
    BaseLineHeight: Integer;
    RText: TRect;
  begin
    if Enabled then
    begin
      bIndex := 0;
      if FIsDown and FIsHover then bIndex := bIndex or 1;
      if FIsSelected then bIndex := bIndex or 2;
      if FIsHover and not FIsDown then bIndex := bIndex or 4;
      if FKeyType <> kktNormal then bIndex := bIndex or 8;
    end
    else
      bIndex := 16;

    if FIsDown and FIsHover then n := 1 else n := 0;

    if (FKeyType = kktEnter) and FKeyboard.FEuroLayout then
      DrawEuropeanEnterKeyParam(Canvas, OnScreen_KeyBitmap.Parameters.Border[n], R.Left, R.Top, FW, FH, bIndex, OnScreen_KeyBitmap.Bitmap,
        FX2, FY2)
    else
      DrawKeyParam(Canvas, OnScreen_KeyBitmap.Parameters.Border[n], R.Left, R.Top, FW, FH, bIndex, OnScreen_KeyBitmap.Bitmap);

    with Canvas, OnScreen_KeyBitmap.Parameters do
    begin
      SetBkMode(Handle, TRANSPARENT);

      Font := FKeyboard.Font;
      Font.Name := CapFont;

      //if FFontSize > 0 then
        //Font.Size := FFontSize
      //else
      if FKeyboard.FLargeCapFont and (FKeyType = kktNormal) then
        Font.Size := Trunc((TextRect[n].Top - TextRect[n].Bottom) * FKeyboard.FScale)
      else //if FKeyType = kktNormal then
        Font.Size := Trunc(CapFontSize*FKeyboard.FScale);

      if not Enabled then    Font.Color := KeyFontColor_Disabled   // I4098
      else if (FKeyType = kktNormal) and not FIsSelected then Font.Color := KeyFontColor_Base   // I4098
      else if FKeyType = kktNormal then Font.Color := KeyFontColor_BaseSelected   // I4098
      else Font.Color := KeyFontColor_Cap;   // I4098

      RText := TextRect[n]; // I2576
      RText.Right := FDefaultPos.Right - FDefaultPos.Left - (33 - RText.Right); // I2576 -  Adjust for key width

      with RText do
      begin
        ARect := Rect(R.Left+Trunc(Left*FKeyboard.FScale), R.Top+Trunc(1*FKeyboard.FScale),
          R.Left+Trunc(Right*FKeyboard.FScale), R.Top+FH);

        if FKeyType <> kktNormal then
          ARect.Right := R.Right - 4;
      end;

      if FKeyType <> kktNormal then
      begin
        s := FKeyCaps[0];
        Font.Size := FontSize;
        if (Abs(Font.Height) < 8) or (Abs(Font.Height) > 24) then
        begin
          Font.Height := Font.Height * 2 div 3;
          if Font.Height < 0 then
          begin
            if Font.Height > -8 then Font.Height := -8
          end
          else if Font.Height < 8 then Font.Height := 8;
        end;
        ARect.Top := ARect.Top + CapPos[n].Y;
        DrawText(Handle, PChar(s), Length(FKeyCaps[0]), ARect, DT_NOPREFIX or DT_WORDBREAK);
      end
      else if FKeyboard.FDisplayUnderlyingChar or (FKeyType <> kktNormal) then
      begin
        if FKeyType <> kktNormal
          then ssi := 0
          else ssi := ValidExtShiftStateIndex(FKeyboard.ShiftState);
        FKeyboard.FDrawChar.DisplayQuality := ctCleartype;
        FKeyboard.FDrawChar.Color := Font.Color;
        FKeyboard.FDrawChar.SetFontDetails(Font.Name, Font.Height);
        if ((FKeyCaps[ssi] = '') or (FKeyCaps[ssi][1] < #32)) and (ssi > 0) and (FKeyType = kktNormal) and Enabled then
        begin
          FKeyboard.FDrawChar.Color := $B0B0B0;
          FKeyboard.FDrawChar.DrawText(Handle, TA_LEFT or TA_TOP, R.Left+Trunc(CapPos[n].X*FKeyboard.FScale),
            R.Top+Trunc(CapPos[n].Y*FKeyboard.FScale), ARect, FKeyCaps[0]);
        end
        else
          FKeyboard.FDrawChar.DrawText(Handle, TA_LEFT or TA_TOP, R.Left+Trunc(CapPos[n].X*FKeyboard.FScale),
            R.Top+Trunc(CapPos[n].Y*FKeyboard.FScale), ARect, FKeyCaps[ssi]);
        //ExtTextOutW(Handle, R.Left+Trunc(CapPos[n].X*FKeyboard.FScale), R.Top+Trunc(CapPos[n].Y*FKeyboard.FScale), 0, @ARect, PWideChar(FKeyCap), Length(FKeyCap), nil);
      end;

      if Assigned(FKeyGlyph) and not FKeyGlyph.Empty then
      begin
        // image is anchored to
        RGlyph := Rect(
          R.Left+Trunc(RText.Right*FKeyboard.FScale) - Trunc(FKeyGlyph.Width*FKeyboard.FScale),  // I2576
          R.Top+Trunc(RText.Bottom*FKeyboard.FScale) - Trunc(FKeyGlyph.Height*FKeyboard.FScale),
          R.Left+Trunc(RText.Right*FKeyboard.FScale),
          R.Top+Trunc(RText.Bottom*FKeyboard.FScale));

          if RGlyph.Right - RGlyph.Left > (RText.Right - RText.Left) * FKeyboard.FScale then
          begin
            RGlyph.Top := RGlyph.Bottom - Trunc((RGlyph.Bottom - RGlyph.Top) * ((RText.Right-RText.Left)*FKeyboard.FScale) / (RGlyph.Right - RGlyph.Left));
            RGlyph.Left := R.Left + Trunc(RText.Left * FKeyboard.FScale);
          end;

          if RGlyph.Bottom - RGlyph.Top > (RText.Bottom - RText.Top) * FKeyboard.FScale then
          begin
            RGlyph.Left := RGlyph.Right - Trunc((RGlyph.Right - RGlyph.Left) * ((RText.Bottom-RText.Top)*FKeyboard.FScale) / (RGlyph.Bottom - RGlyph.Top));
            RGlyph.Top := R.Top + Trunc(RText.Top * FKeyboard.FScale);
          end;

          SetStretchBltMode(Handle, HALFTONE);
          SetBrushOrgEx(Handle, 0, 0, nil);
          FKeyGlyph.TransparentMode := tmAuto;
          FKeyGlyph.Transparent := True;
          FKeyGlyph.PixelFormat := pf24Bit;


          Save := 0;
          MaskDC := 0;
          try
            MaskDC := CreateCompatibleDC(0);
            Save := SelectObject(MaskDC, FKeyGlyph.MaskHandle);
            TransparentStretchBlt(Handle, RGlyph.Left, RGlyph.Top, RGlyph.Right - RGlyph.Left,
              RGlyph.Bottom - RGlyph.Top, FKeyGlyph.Canvas.Handle, 0, 0, FKeyGlyph.Width,
              FKeyGlyph.Height, MaskDC, 0, 0);
          finally
            if Save <> 0 then SelectObject(MaskDC, Save);
            if MaskDC <> 0 then DeleteDC(MaskDC);
          end;
          //StretchDraw(RGlyph, FKeyGlyph);
        //Draw(R.Left+CapPos[n].X-FKeyGlyph.Width, R.Top+CapPos[n].Y-FKeyGlyph.Height, FKeyGlyph);
      end
      else
      begin
        Font := FKeyboard.FDataFont;
        Font.Height := Trunc((RText.Top - RText.Bottom) * FKeyboard.FScale);  // I2576

        FillChar(tm, Sizeof(tm), 0);  // I2576
        GetTextMetrics(Canvas.Handle, tm);
        BaseLineHeight := tm.tmDescent;

        if GetTextExtentPoint32W(Canvas.Handle, PWideChar(FKeyValue), Length(FKeyValue), FTextExtent) then  // I2576
        begin
          if (FTextExtent.cx > (RText.Right - RText.Left) * FKeyboard.FScale) or
            (FTextExtent.cy > (RText.Bottom - RText.Top) * FKeyboard.FScale) then
          begin
            // font.height may be either positive or negative, depending on whether
            // internal leading is included. It doesn't matter, so long as we keep
            // the same factor.
            if Font.Height < 0 then
              Font.Height := System.Math.Max(
                Trunc(Font.Height * (RText.Right - RText.Left) * FKeyboard.FScale / FTextExtent.cx),
                Trunc(Font.Height * (RText.Bottom - RText.Top) * FKeyboard.FScale / FTextExtent.cy)
              )
            else
              Font.Height := System.Math.Min(
                Trunc(Font.Height * (RText.Right - RText.Left) * FKeyboard.FScale / FTextExtent.cx),
                Trunc(Font.Height * (RText.Bottom - RText.Top) * FKeyboard.FScale / FTextExtent.cy)
              );
          end;
        end;

        // TODO: in future re-scale the font here per designer's preference
        //       but this requires a file format change, so a much bigger scope

        if not Enabled then      Font.Color := $808080
        else                     Font.Color := clWindowText;

        FKeyboard.FDrawChar.DisplayQuality := ctCleartype;
        FKeyboard.FDrawChar.Color := Font.Color;
        FKeyboard.FDrawChar.SetFontDetails(Font.Name, Font.Height);

        FKeyboard.FDrawChar.DrawText(Handle, TA_RIGHT or TA_BASELINE, R.Left+Trunc(RText.Right*FKeyboard.FScale),  // I2576
          R.Top+Trunc(RText.Bottom*FKeyboard.FScale)-BaseLineHeight, ARect, FKeyValue, False, True);
        //GetTextExtentPoint32W(Handle, PWideChar(FKeyValue), Length(FKeyValue), sz);
        //ExtTextOutW(Handle, ETO_CLIPPED, @ARect, PWideChar(FKeyValue), Length(FKeyValue), nil);
      end;
    end;
  end;

begin
  //FKeyboard.FBufferBitmap.SetSize(FW, FH);

  R := Rect(FX, FY, FX+FW, FY+FH);

  if (FKeyType = kktEnter) and FKeyboard.FEuroLayout then
  begin
    if FKeyboard.FTransparent then
    begin
      BitBlt(FKeyboard.FBufferBitmap.Canvas.Handle, FX, FY, FW, FY2-FY, FKeyboard.FBackgroundBitmap.Canvas.Handle, FX, FY, SRCCOPY);
      BitBlt(FKeyboard.FBufferBitmap.Canvas.Handle, FX2, FY2, FW-(FX2-FX), FH-(FY2-FY), FKeyboard.FBackgroundBitmap.Canvas.Handle, FX2, FY2, SRCCOPY);
    end
    else
    begin
      FKeyboard.FBufferBitmap.Canvas.Brush.Color := FKeyboard.Color;
      FKeyboard.FBufferBitmap.Canvas.FillRect(Rect(FX, FY, FX+FW, FY2));
      FKeyboard.FBufferBitmap.Canvas.FillRect(Rect(FX2, FY2, FX+FW, FY+FH));
    end;

    DrawKeyBitmap(FKeyboard.FBufferBitmap.Canvas);
    if DestCanvas <> FKeyboard.FBufferBitmap.Canvas then
    begin
      BitBlt(DestCanvas.Handle, FX, FY, FW, FY2-FY, FKeyboard.FBufferBitmap.Canvas.Handle, FX, FY, SRCCOPY);
      BitBlt(DestCanvas.Handle, FX2, FY2, FW-(FX2-FX), FH-(FY2-FY), FKeyboard.FBufferBitmap.Canvas.Handle, FX2, FY2, SRCCOPY);
    end;
  end
  else
  begin
    if FKeyboard.FTransparent then
      BitBlt(FKeyboard.FBufferBitmap.Canvas.Handle, FX, FY, FW, FH, FKeyboard.FBackgroundBitmap.Canvas.Handle, FX, FY, SRCCOPY)
    else
    begin
      FKeyboard.FBufferBitmap.Canvas.Brush.Color := FKeyboard.Color;
      FKeyboard.FBufferBitmap.Canvas.FillRect(R);
    end;

    DrawKeyBitmap(FKeyboard.FBufferBitmap.Canvas);
    if DestCanvas <> FKeyboard.FBufferBitmap.Canvas then
      BitBlt(DestCanvas.Handle, FX, FY, FW, FH, FKeyboard.FBufferBitmap.Canvas.Handle, FX, FY, SRCCOPY);
  end;
end;

function TOnScreenKeyboardKey.GetActiveKeyCap: WideString;
begin
  Result := FKeyCaps[ValidExtShiftStateIndex(FKeyboard.ShiftState)];
end;

function TOnScreenKeyboardKey.GetKeyCap(Index: Integer): WideString;
begin
  Assert((Index >= Low(FKeyCaps)) and (Index <= High(FKeyCaps)));
  Result := FKeyCaps[Index];
end;

function TOnScreenKeyboardKey.GetKeyRect: TRect;
begin
  Result := Rect(FX, FY, FX+FW, FY+FH);
end;

procedure TOnScreenKeyboardKey.SetEnabled(const Value: Boolean);
begin
  FEnabled := Value;
  FKeyboard.DoDrawKey(FKeyboard.Canvas, Self); //InvalidateKey(Self);
end;

procedure TOnScreenKeyboardKey.SetKeyCap(Index: Integer; Value: WideString);
begin
  FKeyCaps[Index] := Value;
  FKeyboard.DoDrawKey(FKeyboard.Canvas, Self); //FKeyboard.InvalidateKey(Self);
end;

procedure TOnScreenKeyboardKey.SetKeyGlyph(const Value: TBitmap);
begin
  if Assigned(Value) then
  begin
    if not Assigned(FKeyGlyph) then FKeyGlyph := TBitmap.Create;
    FKeyGlyph.Assign(Value);
  end
  else
    FreeAndNil(FKeyGlyph);
  Invalidate;
end;

procedure TOnScreenKeyboardKey.SetKeyValue(Value: WideString);
begin
  FKeyValue := Value;
  FKeyboard.DoDrawKey(FKeyboard.Canvas, Self); //Invalidate;
end;

procedure TOnScreenKeyboardKey.UpdateKeyCap;
var
  lParam: Integer;
  bufW: array[0..64] of WideChar;
begin
  if FKeyType <> kktNormal then
  begin
    lParam := FScanCode shl 16;
    if FKeyType in [kktCtrlRight, kktAltRight] then
      lParam := lParam or (1 shl 24);

    if GetKeyNameText(lParam, bufW, 64) = 0 then FKeyCaps[0] := OnScreenKeyboardKeyTypeCaption[FKeyType]  // I3309
    else FKeyCaps[0] := bufW;

    FVKey := MapScanCodeToUSVK(FScanCode);
    FUSVKey := FVKey;
    //FKeyCaps[0] := OnScreenKeyboardKeyTypeCaption[FKeyType];
  end
  else
  begin
    FKeyboard.MapScanCodes(FScanCode, FKeyCaps, FVKey);
    FUSVKey := MapScanCodeToUSVK(FScanCode);
    //FVKey := MapScanCodeToVK(FScanCode);
  end;
end;

procedure TOnScreenKeyboardKey.Invalidate;
begin
  FKeyboard.InvalidateKey(Self);
end;

{ TQuickBitmap }

constructor TQuickBitmap.Create(AWidth, AHeight: Integer);
var
  FHDC: THandle;
begin
  inherited Create;
  FHDC := GetDC(0);
  FHandle := CreateCompatibleBitmap(FHDC, AWidth, AHeight);
  ReleaseDC(0, FHDC);
end;

destructor TQuickBitmap.Destroy;
begin
  ReleaseCanvas;
  DeleteObject(FHandle);
  inherited Destroy;
end;

function TQuickBitmap.GetCanvas: TCanvas;
begin
  if not Assigned(FCanvas) then
  begin
    FCanvas := TCanvas.Create;
    FCanvas.Handle := CreateCompatibleDC(0);
    FOldHBitmap := SelectObject(FCanvas.Handle, FHandle);
  end;
  Result := FCanvas;
end;

{function TQuickBitmap.GetHDC: THandle;
begin
  if FHDC = 0 then
  begin
    FHDC := CreateCompatibleDC(0);
    FOldHBitmap := SelectObject(FHDC, FHandle);
  end;
  Result := FHDC;
end;}

procedure TQuickBitmap.ReleaseCanvas;
var
  h: THandle;
begin
  if Assigned(FCanvas) then
  begin
    SelectObject(FCanvas.Handle, FOldHBitmap);
    h := FCanvas.Handle;
    FCanvas.Handle := 0;
    DeleteDC(h);
  end;
  FreeAndNil(FCanvas);
end;

{procedure TQuickBitmap.ReleaseDC;
begin
  if FHDC <> 0 then
  begin
    SelectObject(FHDC, FOldHBitmap);
    DeleteDC(FHDC);
    FHDC := 0;
  end;
end;}

end.
