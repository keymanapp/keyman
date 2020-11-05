unit Keyman.Developer.System.ImportKeyboardDLL;

interface

uses
  System.SysUtils,
  Winapi.Windows;

type
  EImportKeyboardDLL = class(Exception);

  TImportKeyboardDLL = class
  private
    FLayoutFile: string;
    FLayoutText: string;
    FRecommendedOutputKMNFileName: string;
    FInputHKL: string;
    FKMN: string;
    FKVKS: string;
  public
    constructor Create(const inputHKL: string);
    procedure Execute;
    property InputHKL: string read FInputHKL;
    property KMN: string read FKMN;
    property KVKS: string read FKVKS;
    property RecommendedOutputKMNFileName: string read FRecommendedOutputKMNFileName;
  end;

implementation

uses
  System.Classes,
  System.Generics.Collections,
  System.Math,
  System.Win.Registry,

  ScanCodeMap,
  Unicode,
  VisualKeyboard,
  VKeys;


type
  TKBDShiftState = (
    Base = 0,  // 0
    Shft = 1,  // 1
    Ctrl = 2,  // 2
    ShftCtrl = 3, //Shft or Ctrl, // 3
    Menu = 4, // 4 - not used
    ShftMenu = 5, //Shft or Menu, // 5 - not used
    MenuCtrl = 6, //Menu or Ctrl, // 6
    ShftMenuCtrl = 7, //Shft or Menu or Ctrl, // 7
    Xxxx = 8,  // 8
    ShftXxxx = 9 //Shft or Xxxx // 9
  );

const
  KBDShiftStateToVisualKeyboardShiftState: array[TKBDShiftState] of WORD = (
    KVKS_NORMAL,                //Base = 0,
    KVKS_SHIFT,                 //Shft = 1,
    KVKS_CTRL,                  //Ctrl = 2,
    KVKS_SHIFT or KVKS_CTRL,    //ShftCtrl = 3,
    KVKS_ALT,                   //Menu = 4,
    KVKS_SHIFT or KVKS_ALT,     //ShftMenu = 5,
    KVKS_RALT,                  //MenuCtrl = 6,
    KVKS_SHIFT or KVKS_RALT,    //ShftMenuCtrl = 7,
    $FFFF,                      //Xxxx = 8,
    $FFFF                       //ShftXxxx = 9
  );

{ TDeadKey }

type
  TDeadKey = class
  private
    FDeadChar: Char;
    FRGBaseChar: TList<Char>;
    FRGCombChar: TList<Char>;
    function GetCount: Integer;
    function GetBaseCharacter(Index: Integer): Char;
    function GetCombinedCharacter(Index: Integer): Char;
  public
    constructor Create(deadCharacter: Char);
    destructor Destroy; override;
    procedure AddDeadKeyRow(baseCharacter, combinedCharacter: Char);
    function ContainsBaseCharacter(baseCharacter: Char): Boolean;
    property DeadCharacter: Char read FDeadChar;
    property Count: Integer read GetCount;
    property BaseCharacter[Index: Integer]: Char read GetBaseCharacter;
    property CombinedCharacter[Index: Integer]: Char read GetCombinedCharacter;
  end;

type
  TVirtualKey = class
  private
    FHKL: HKL;
    FVK: UINT;
    FSC: UINT;
    FRGFDeadKey: array[TKBDShiftState, Boolean] of Boolean;
    FRGSS: array[TKBDShiftState, Boolean] of string;
    function GetIsSGCaps: Boolean;
    function GetIsCapsEqualToShift: Boolean;
    function GetIsAltGrCapsEqualToAltGrShift: Boolean;
    function GetIsXxxxGrCapsEqualToXxxxShift: Boolean;
    function GetIsEmpty: Boolean;
    function GetIsKeymanUsedKey: Boolean;
    function GetVKeyName: string;
  public
    constructor CreateFromVirtualKey(hkl: HKL; virtualKey: UINT);
    constructor CreateFromScanCode(hkl: HKL; scanCode: UINT);
    function GetShiftState(shiftState: TKBDShiftState; capsLock: Boolean): string;
    procedure SetShiftState(shiftState: TKBDShiftState; value: string; isDeadKey, capsLock: Boolean);
    function GetShiftStateName(capsLock: UINT; caps: Boolean; ss: TKBDShiftState): string;
    function LayoutRow: string;
    procedure AddKeysToVisualKeyboard(vk: TVisualKeyboard);

    property VK: UINT read FVK;
    property SC: UINT read FSC;
    property IsSGCaps: Boolean read GetIsSGCaps;
    property IsCapsEqualToShift: Boolean read GetIsCapsEqualToShift;
    property IsAltGrCapsEqualToAltGrShift: Boolean read GetIsAltGrCapsEqualToAltGrShift;
    property IsXxxxGrCapsEqualToXxxxShift: Boolean read GetIsXxxxGrCapsEqualToXxxxShift;
    property IsEmpty: Boolean read GetIsEmpty;
    property IsKeymanUsedKey: Boolean read GetIsKeymanUsedKey;
  end;

type
  TVirtualKeyList = class(TObjectList<TVirtualKey>);
  TDeadKeyList = class(TObjectList<TDeadKey>);

type
  TLoader = class
  private
    inputHKL, layoutFile, layoutText: string;
    rgkey: array[0..255] of TVirtualKey;
    alDead: TDeadKeyList;

    FKeyStateNull: TKeyboardState;
    FXxxxVk: UINT;
    FHas102ndKey: Boolean;
    function GetMaxShiftState: TKBDShiftState;
    function ProcessDeadKey(
        iKeyDead: UINT;             // The index into the VirtualKey of the dead key
        shiftStateDead: TKBDShiftState;  // The shiftstate that contains the dead key
        pKeyStateDead: TKeyboardState;    // The key state for the dead key
        fCapsLock: Boolean;             // Was the caps lock key pressed?
        hkl: HKL): TDeadKey;      // The keyboard layout
    procedure ClearKeyboardBuffer(vk: UINT; sc: UINT; hkl: HKL);
    procedure ScanKeyboard(hkl: HKL);
    function WriteOutputKMNFile: string;
    function WriteOutputKVKSFile: string;
    procedure Detect102ndKey(hkl: HKL);
  public
    constructor Create(inputHKL, layoutFile, layoutText: string);
    destructor Destroy; override;
    procedure Main(var KMN, KVKS: string);
    procedure FillKeyState(var pKeyState: TKeyboardState; ss: TKBDShiftState; fCapsLock: Boolean);
    property XxxxVk: UINT read FXxxxVk write FXxxxVk;
    property MaxShiftState: TKBDShiftState read GetMaxShiftState;
  end;

var
  Loader: TLoader = nil;

function IntToHexLC(Value, Digits: Integer): string;
begin
  Result := IntToHex(Value, Digits).ToLower;
end;

function TDeadKey.ContainsBaseCharacter(baseCharacter: Char): Boolean;
begin
  Result := FRGBaseChar.Contains(baseCharacter);
end;

constructor TDeadKey.Create(deadCharacter: Char);
begin
  FRGBaseChar := TList<Char>.Create;
  FRGCombChar := TList<Char>.Create;

  FDeadChar := deadCharacter;
end;

destructor TDeadKey.Destroy;
begin
  FRGBaseChar.Free;
  FRGCombChar.Free;
  inherited;
end;

function TDeadKey.GetBaseCharacter(Index: Integer): Char;
begin
  Result := FRGBaseChar[Index];
end;

function TDeadKey.GetCombinedCharacter(Index: Integer): Char;
begin
  Result := FRGCombChar[Index];
end;

function TDeadKey.GetCount: Integer;
begin
  Result := FRGBaseChar.Count;
end;

procedure TDeadKey.AddDeadKeyRow(baseCharacter, combinedCharacter: Char);
begin
  FRGBaseChar.Add(baseCharacter);
  FRGCombChar.Add(combinedCharacter);
end;

{ TVirtualKey }

constructor TVirtualKey.CreateFromVirtualKey(hkl: HKL; virtualKey: UINT);
begin
  FSC := MapVirtualKeyEx(virtualKey, 0, hkl);
  FHKL := hkl;
  FVK := virtualKey;
end;

constructor TVirtualKey.CreateFromScanCode(hkl: HKL; scanCode: UINT);
begin
  FVK := MapVirtualKeyEx(scanCode, 1, hkl);
  FHKL := hkl;
  FSC := scanCode;
end;

function TVirtualKey.GetShiftState(shiftState: TKBDShiftState;
  capsLock: Boolean): string;
begin
  Result := FRGSS[shiftState, capsLock];
end;

procedure TVirtualKey.SetShiftState(shiftState: TKBDShiftState; value: string;
  isDeadKey, capsLock: Boolean);
begin
  FRGFDeadKey[shiftState, capsLock] := isDeadKey;
  FRGSS[shiftState, capsLock] := value;
end;

function TVirtualKey.GetIsSGCaps: Boolean;
var
  stBase, stShift, stCaps, stShiftCaps: string;
begin
  stBase := GetShiftState(TKBDShiftState.Base, False);
  stShift := GetShiftState(TKBDShiftState.Shft, False);
  stCaps := GetShiftState(TKBDShiftState.Base, True);
  stShiftCaps := GetShiftState(TKBDShiftState.Shft, True);

  Result :=
    ((stCaps.Length > 0) and
    (not stBase.Equals(stCaps)) and
    (not stShift.Equals(stCaps))) or
    ((stShiftCaps.Length > 0) and
    (not stBase.Equals(stShiftCaps)) and
    (not stShift.Equals(stShiftCaps)));
end;

function TVirtualKey.GetIsCapsEqualToShift: Boolean;
var
  stBase, stShift, stCaps: string;
begin
  stBase := GetShiftState(TKBDShiftState.Base, False);
  stShift := GetShiftState(TKBDShiftState.Shft, False);
  stCaps := GetShiftState(TKBDShiftState.Base, True);
  Result :=
    (stBase.Length > 0) and
    (stShift.Length > 0) and
    (not stBase.Equals(stShift)) and
    (stShift.Equals(stCaps));
end;

function TVirtualKey.GetIsAltGrCapsEqualToAltGrShift: Boolean;
var
  stBase, stShift, stCaps: string;
begin
  stBase := GetShiftState(TKBDShiftState.MenuCtrl, False);
  stShift := GetShiftState(TKBDShiftState.ShftMenuCtrl, False);
  stCaps := GetShiftState(TKBDShiftState.MenuCtrl, True);
  Result :=
    (stBase.Length > 0) and
    (stShift.Length > 0) and
    (not stBase.Equals(stShift)) and
    (stShift.Equals(stCaps));
end;

function TVirtualKey.GetIsXxxxGrCapsEqualToXxxxShift: Boolean;
var
  stBase, stShift, stCaps: string;
begin
  stBase := GetShiftState(TKBDShiftState.Xxxx, False);
  stShift := GetShiftState(TKBDShiftState.ShftXxxx, False);
  stCaps := GetShiftState(TKBDShiftState.Xxxx, True);
  Result :=
    (stBase.Length > 0) and
    (stShift.Length > 0) and
    (not stBase.Equals(stShift)) and
    (stShift.Equals(stCaps));
end;

function TVirtualKey.GetIsEmpty: Boolean;
var
  i: TKBDShiftState;
  j: Boolean;
begin
  for i := Low(TKBDShiftState) to High(TKBDShiftState) do
  begin
    for j := False to True do
    begin
      if GetShiftState(i, j).Length > 0 then
        Exit(False);
    end;
  end;

  Result := True;
end;

function TVirtualKey.GetIsKeymanUsedKey: Boolean;
begin
  Result := ((FVK >= $20) and (FVK <= $5F)) or (FVK >= $88);
end;

function TVirtualKey.GetShiftStateName(capsLock: UINT; caps: Boolean;
  ss: TKBDShiftState): string;
begin
  Result := '';

  if capslock <> 0 then
    if caps
      then Result := Result + 'CAPS '
      else Result := Result + 'NCAPS ';

  case ss of
    Base: ;
    Shft: Result := Result + 'SHIFT ';
    Ctrl: Result := Result + 'CTRL ';
    ShftCtrl: Result := Result + 'SHIFT CTRL ';
    Menu: Result := Result + 'ALT ';
    ShftMenu: Result := Result + 'SHIFT ALT ';
    MenuCtrl: Result := Result + 'RALT ';
    ShftMenuCtrl: Result := Result + 'SHIFT RALT ';
    Xxxx: Result := Result + 'XXXX ';
    ShftXxxx: Result := Result + 'SHIFT XXXX ';
  end;
end;

function TVirtualKey.GetVKeyName: string;
var
  i: Integer;
begin
  for i := 0 to High(USVirtualKeyToScanCode) do
    if USVirtualKeyToScanCode[i] = FSC then
      Exit(VKeyNames[i]);
  Result := 'K_???';
end;

function TVirtualKey.LayoutRow: string;
var
  capslock: UINT;
  ss: TKBDShiftState;
  caps: Boolean;
  st: string;
  ich: Integer;
  isvalid: Boolean;
begin
  Result := '';

  capslock :=
    IfThen(IsCapsEqualToShift, 1, 0) or
    IfThen(IsSGCAPS, 2 , 0) or
    IfThen(IsAltGrCapsEqualToAltGrShift, 4, 0) or
    IfThen(IsXxxxGrCapsEqualToXxxxShift, 8, 0);

  for ss := Low(TKBDShiftState) to Loader.MaxShiftState do
  begin
    if (ss = Menu) or (ss = ShftMenu) then
    begin
      // Alt and Shift+Alt don't work, so skip them
      Continue;
    end;
    for caps := False to True do
    begin
      st := GetShiftState(ss, caps);
      if st.Length = 0 then
      begin
        // No character assigned here
      end
      else if caps and (st = GetShiftState(ss, not caps)) then
      begin
        // It's a CAPS LOCK state and the assigned character(s) are
        // identical to the non-CAPS LOCK state.
      end
      else if FRGFDeadKey[ss, caps] then
      begin
        // It's a dead key
        Result := Result + Format('+ [%s%s] > dk(%s)'#13#10, [
          GetShiftStateName(capslock, caps, ss),
          GetVKeyName,
          IntToHexLC(Ord(st[1]), 4)]);
      end
      else
      begin
        isvalid := True;
        for ich := 1 to st.Length do
        begin
          if (st[ich] < #$20) or (st[ich] = #$7F) then
          begin
            isvalid := False;
            Break;
          end;
        end;

        // We don't add Shift+Space, Ctrl+Space as they are not
        // useful
        if (st = #$20) and (FVK = VK_SPACE) and (ss <> Base) then
          isvalid := False;

        if isvalid then
        begin
          // It's some characters; put 'em in there.
          Result := Result + Format('+ [%s%s] >', [GetShiftStateName(capslock, caps, ss), GetVKeyName]);

          ich := 1;
          while ich <= st.Length do
          begin
            if (ich < st.Length) and Uni_IsSurrogate1(st[ich]) and Uni_IsSurrogate2(st[ich+1]) then
            begin
              Result := Result + ' U+' + IntToHexLC(Uni_SurrogateToUTF32(st[ich], st[ich+1]), 6);
              Inc(ich, 2);
            end
            else
            begin
              Result := Result + ' U+' + IntToHexLC(Ord(st[ich]), 4);
              Inc(ich);
            end;
          end;

          Result := Result + #13#10;
        end;
      end;
    end;
  end;
end;

procedure TVirtualKey.AddKeysToVisualKeyboard(vk: TVisualKeyboard);
var
  ss: TKBDShiftState;
  st: string;
  ich: Integer;
  isvalid: Boolean;
  vkk: TVisualKeyboardKey;
begin
  for ss := Low(TKBDShiftState) to Loader.MaxShiftState do
  begin
    if (ss = Menu) or (ss = ShftMenu) then
    begin
      // Alt and Shift+Alt don't work, so skip them
      Continue;
    end;

    st := GetShiftState(ss, False);
    if st.Length = 0 then
    begin
      // No character assigned here
    end
    else if FRGFDeadKey[ss, False] then
    begin
      // It's a dead key
      // This does not match the "Fill from layout" result because deadkeys are
      // left blank in "Fill from layout". However, this is better because it
      // matches the actual Windows layout. "Fill from layout" cannot deduce the
      // character to put onto a deadkey currently (unlike Windows deadkeys,
      // there is no isolated "default" for a Keyman deadkey).
      vkk := TVisualKeyboardKey.Create;
      vkk.VKey := MapScanCodeToUSVK(FSC);
      vkk.Text := st[1];
      vkk.Flags := [kvkkUnicode];
      vkk.Shift := KBDShiftStateToVisualKeyboardShiftState[ss];
      vk.Keys.Add(vkk);
    end
    else
    begin
      isvalid := True;
      for ich := 1 to st.Length do
      begin
        if (st[ich] < #$20) or (st[ich] = #$7F) then
        begin
          isvalid := False;
          Break;
        end;
      end;

      if isvalid then
      begin
        // It's some characters; put 'em in there.
        vkk := TVisualKeyboardKey.Create;
        vkk.VKey := MapScanCodeToUSVK(FSC);
        vkk.Text := st;
        vkk.Flags := [kvkkUnicode];
        vkk.Shift := KBDShiftStateToVisualKeyboardShiftState[ss];
        vk.Keys.Add(vkk);
      end;
    end;
  end;
end;

{ TLoader }

function TLoader.GetMaxShiftState: TKBDShiftState;
begin
  if FXxxxVk = 0
    then Result := ShftMenuCtrl
    else Result := ShftXxxx;
end;

constructor TLoader.Create(inputHKL, layoutFile, layoutText: string);
begin
  inherited Create;
  Self.inputHKL := inputHKL;
  Self.layoutFile := layoutFile;
  Self.layoutText := layoutText;

  alDead := TDeadKeyList.Create;
end;

destructor TLoader.Destroy;
var
  i: Integer;
begin
  for i := 0 to High(rgkey) do
    FreeAndNil(rgkey[i]);
  FreeandNil(alDead);
  inherited Destroy;
end;

procedure TLoader.FillKeyState(var pKeyState: TKeyboardState; ss: TKBDShiftState; fCapsLock: Boolean);
begin
  pKeyState[VK_SHIFT] := IfThen((Ord(ss) and Ord(Shft)) <> 0, $80, $00);
  pKeyState[VK_CONTROL] := IfThen((Ord(ss) and Ord(Ctrl)) <> 0, $80, $00);
  pKeyState[VK_MENU] := IfThen((Ord(ss) and Ord(Menu)) <> 0, $80, $00);

  if FXxxxVk <> 0 then
  begin
    // The Xxxx key has been assigned, so let's include it
    pKeyState[FXxxxVk] := IfThen((Ord(ss) and Ord(Xxxx)) <> 0, $80, $00);
  end;

  pKeyState[VK_CAPITAL] := IfThen(fCapsLock, $01, $00);
end;

function TLoader.ProcessDeadKey(iKeyDead: UINT; shiftStateDead: TKBDShiftState;
  pKeyStateDead: TKeyboardState; fCapsLock: Boolean;
  hkl: HKL): TDeadKey;
var
  pKeyState: TKeyboardState;
  deadKey: TDeadKey;
  iKey: UINT;
  rc: Integer;
  sbBuffer: array[0..10] of Char;
  ss: TKBDShiftState;
  caps: Boolean;
  combchar, basechar: Char;
begin
  deadKey := TDeadKey.Create(rgKey[iKeyDead].GetShiftState(shiftStateDead, fCapsLock)[1]);
  for iKey := 0 to Length(rgKey) - 1 do
  begin
    if rgKey[iKey] <> nil then
    begin
      FillChar(sbBuffer, sizeof(sbBuffer), 0);
      for ss := Low(TKBDShiftState) to MaxShiftState do
      begin
        rc := 0;
        if (ss = Menu) or (ss = ShftMenu) then
        begin
          // Alt and Shift+Alt don't work, so skip them
          Continue;
        end;

        for caps := False to True do
        begin
          // First the dead key
          while rc >= 0 do
          begin
            // We know that this is a dead key coming up, otherwise
            // this function would never have been called. If we do
            // *not* get a dead key then that means the state is
            // messed up so we run again and again to clear it up.
            // Risk is technically an infinite loop but per Hiroyama
            // that should be impossible here.
            rc := ToUnicodeEx(rgKey[iKeyDead].VK, rgKey[iKeyDead].SC, pKeyStateDead, sbBuffer, High(sbBuffer)+1, 0, hkl);
          end;

          // Now fill the key state for the potential base character
          FillKeyState(pKeyState, ss, caps);

          FillChar(sbBuffer, sizeof(sbBuffer), 0);
          rc := ToUnicodeEx(rgKey[iKey].VK, rgKey[iKey].SC, pKeyState, sbBuffer, High(sbBuffer)+1, 0, hkl);
          if rc = 1 then
          begin
            // That was indeed a base character for our dead key.
            // And we now have a composite character. Let's run
            // through one more time to get the actual base
            // character that made it all possible?
            combchar := sbBuffer[0];
            FillChar(sbBuffer, sizeof(sbBuffer), 0);
            rc := ToUnicodeEx(rgKey[iKey].VK, rgKey[iKey].SC, pKeyState, sbBuffer, High(sbBuffer)+1, 0, hkl);

            basechar := sbBuffer[0];


            if deadKey.DeadCharacter = combchar then
            begin
              // Since the combined character is the same as the dead key,
              // we must clear out the keyboard buffer.
              ClearKeyboardBuffer(VK_DECIMAL, rgKey[VK_DECIMAL].SC, hkl);
            end;

            if (((ss = Ctrl) or (ss = ShftCtrl)) and
              (basechar < #$20)) or
              (basechar = combchar) then
            begin
              // ToUnicodeEx has an internal knowledge about those
              // VK_A ~ VK_Z keys to produce the control characters,
              // when the conversion rule is not provided in keyboard
              // layout files

              // Additionally, dead key state is lost for some of these
              // character combinations, for unknown reasons.

              // Therefore, if the base character and combining are equal,
              // and its a CTRL or CTRL+SHIFT state, and a control character
              // is returned, then we do not add this "dead key" (which
              // is not really a dead key).
              Continue;
            end;

            if not deadKey.ContainsBaseCharacter(basechar) then
            begin
              deadKey.AddDeadKeyRow(basechar, combchar);
            end;
          end
          else if rc > 1 then
          begin
            // Not a valid dead key combination, sorry! We just ignore it.
          end
          else if rc < 0 then
          begin
            // It's another dead key, so we ignore it (other than to flush it from the state)
            ClearKeyboardBuffer(VK_DECIMAL, rgKey[VK_DECIMAL].SC, hkl);
          end;
        end;
      end;
    end;
  end;

  result := deadKey;
end;

procedure TLoader.ClearKeyboardBuffer(vk, sc: UINT; hkl: HKL);
var
  sb: array[0..10] of Char;
  rc: Integer;
begin
  rc := 0;
  while rc <> 1 do
  begin
    rc := ToUnicodeEx(vk, sc, FKeyStateNull, sb, High(sb)+1, 0, hkl);
  end;
end;

procedure TLoader.ScanKeyboard(hkl: HKL);
var
  pKeyState: TKeyboardState;
  sc: Integer;
  key: TVirtualKey;
  ke: UINT;
  vk: Integer;
  vkL: Integer;
  vkR: Integer;
  iKey: UINT;
  sbBuffer: array[0..10] of Char;
  ss: TKBDShiftState;
  caps: Boolean;
  rc: Integer;
  dk: TDeadKey;
  iDead: Integer;
begin
  FillChar(rgKey, sizeof(rgKey), 0);
  FillChar(pKeyState, sizeof(pKeyState), 0);

  for sc := $01 to $7f do
  begin
    key := TVirtualKey.CreateFromScanCode(hkl, sc);
    if key.VK <> 0 then
    begin
      rgKey[key.VK] := key;
    end
    else
      key.Free;
  end;

  // add the special keys that do not get added from the code above
  for ke := VK_NUMPAD0 to VK_NUMPAD9 do
  begin
    rgKey[ke] := TVirtualKey.CreateFromVirtualKey(hkl, ke);
  end;

  rgKey[VK_DIVIDE] := TVirtualKey.CreateFromVirtualKey(hkl, VK_DIVIDE);
  rgKey[VK_CANCEL] := TVirtualKey.CreateFromVirtualKey(hkl, VK_CANCEL);
  rgKey[VK_DECIMAL] := TVirtualKey.CreateFromVirtualKey(hkl, VK_DECIMAL);

  // See if there is a special shift state added
  for vk := 0 to VK_OEM_CLEAR do
  begin
    sc := MapVirtualKeyEx(vk, 0, hkl);
    vkL := MapVirtualKeyEx(sc, 1, hkl);
    vkR := MapVirtualKeyEx(sc, 3, hkl);
    if (vkL <> vkR) and (vk <> vkL) then
    begin
      case vk of
        VK_LCONTROL,
        VK_RCONTROL,
        VK_LSHIFT,
        VK_RSHIFT,
        VK_LMENU,
        VK_RMENU:
          ;
        else
          XxxxVk := vk;
      end;
    end;
  end;

  for iKey := 0 to Length(rgKey) - 1 do
  begin
    if rgKey[iKey] <> nil then
    begin
      for ss := Base to MaxShiftState do
      begin
        if (ss = Menu) or (ss = ShftMenu) then
        begin
          // Alt and Shift+Alt don't work, so skip them
          Continue;
        end;

        for caps := False to True do
        begin
          ClearKeyboardBuffer(VK_DECIMAL, rgKey[VK_DECIMAL].SC, hkl);
          ////FillKeyState(lpKeyState, ss, (caps != 0)); //http://blogs.msdn.com/michkap/archive/2006/04/18/578557.aspx
          FillKeyState(pKeyState, ss, not caps);
          FillChar(sbBuffer, sizeof(sbBuffer), 0);
          rc := ToUnicodeEx(rgKey[iKey].VK, rgKey[iKey].SC, pKeyState, sbBuffer, High(sbBuffer)+1, 0, hkl);
          if rc > 0 then
          begin
            if sbBuffer[0] = #0 then
            begin
              // Someone defined NULL on the keyboard; let's coddle them
              rgKey[iKey].SetShiftState(ss, #0, false, not caps);
            end
            else
            begin
              if (rc = 1) and
                  ((ss = Ctrl) or (ss = ShftCtrl)) and
                  (rgKey[iKey].VK = (Ord(sbBuffer[0]) + $40)) then
              begin
                // ToUnicodeEx has an internal knowledge about those
                // VK_A ~ VK_Z keys to produce the control characters,
                // when the conversion rule is not provided in keyboard
                // layout files
                Continue;
              end;

              rgKey[iKey].SetShiftState(ss, Copy(sbBuffer, 1, rc), False, not caps);
            end;
          end
          else if rc < 0 then
          begin
            rgKey[iKey].SetShiftState(ss, sbBuffer[0], True, not caps);

            // It's a dead key; let's flush out whats stored in the keyboard state.
            ClearKeyboardBuffer(VK_DECIMAL, rgKey[VK_DECIMAL].SC, hkl);

            dk := nil;
            for iDead := 0 to alDead.Count - 1 do
            begin
              dk := alDead[iDead];
              if dk.DeadCharacter = rgKey[iKey].GetShiftState(ss, not caps)[1] then
                Break;
              dk := nil;
            end;

            if dk = nil then
              alDead.Add(ProcessDeadKey(iKey, ss, pKeyState, not caps, hkl));
          end;
        end;
      end;
    end;
  end;

  Detect102ndKey(hkl);
end;

// duplicate of TOnScreenKeyboard.UpdateEuroLayout
procedure TLoader.Detect102ndKey(hkl: HKL);
var
  k102, kbackslash: UINT;
begin
  k102 := MapVirtualKeyExW($56, 1, hkl);
  kbackslash := MapVirtualKeyExW($2b, 1, hkl);

  if k102 <> 0 then k102 := MapVirtualKeyExW(k102, 2, hkl);
  if kbackslash <> 0 then kbackslash := MapVirtualKeyExW(kbackslash, 2, hkl);

  FHas102ndKey := (kbackslash <> k102) and (k102 <> 0);
end;

procedure TLoader.Main(var KMN, KVKS: string);
type
  PHKL = ^HKL;
var
  hq, hp: PHKL;
  cKeyboards: Cardinal;
  i: Integer;
  hkl: THandle;
begin
  hkl := 0;
  inputHKL := IntToHex(StrToInt('$'+inputHKL), 8);

  { Load the layout (if not already loaded) and determine if it should be unloaded again }
  hp := nil;
  cKeyboards := GetKeyboardLayoutList(0, hp^);
  hp := PHKL(AllocMem(cKeyboards * sizeof(HKL)));
  try
    GetKeyboardLayoutList(cKeyboards, hp^);

    hkl := LoadKeyboardLayout(inputHKL, KLF_NOTELLSHELL);
    if hkl = 0 then
      raise EImportKeyboardDLL.CreateFmt('The keyboard %s could not be loaded (error %d, %s)',
        [inputHKL, GetLastError, SysErrorMessage(GetLastError)]);

    ScanKeyboard(hkl);
  finally
    hq := hp;
    for i := 0 to cKeyboards - 1 do
      if hq^ = hkl
        then hkl := 0
        else Inc(hq);

    if hkl <> 0 then
      UnloadKeyboardLayout(hkl);

    FreeMem(hp);
  end;

  KMN := WriteOutputKMNFile;
  KVKS := WriteOutputKVKSFile;
end;

function TLoader.WriteOutputKMNFile: string;
var
  iKey: UINT;
  dk: TDeadKey;
  outFile: TStringList;
  sbDKFrom: string;
  sbDKTo: string;
  id: Integer;
begin

  outFile := TStringList.Create;
  try
    outFile.Add('c');
    outFile.Add('c Keyman keyboard generated by ImportKeyboard');
    outFile.Add('c Imported: ' + FormatDateTime('yyyy-mm-dd hh:nn:ss', Now));
    outFile.Add('c');
    outFile.Add('c Source Keyboard File: '+layoutFile);
    outFile.Add('c Source KeyboardID: '+inputHKL.ToLower);
    outFile.Add('c');
    outFile.Add('c '#13#10);
    outFile.Add('store(&Version) "10.0"');
    outFile.Add(Format('store(&Name) "%s"', [layoutText]));
    outFile.Add('store(&Targets) "windows macosx linux web"');
    outFile.Add('');
    outFile.Add('begin Unicode > use(main)'#13#10);
    outFile.Add('group(main) using keys'#13#10);

    for iKey := 0 to Length(rgKey) - 1 do
    begin
      if (rgKey[iKey] <> nil) and rgKey[iKey].IsKeymanUsedKey and not rgKey[iKey].IsEmpty then
      begin
        outFile.Add(rgKey[iKey].LayoutRow);
      end;
    end;

    if alDead.Count > 0 then
    begin
      outFile.Add(#13#10'match > use(deadkeys)'#13#10);
      outFile.Add('group(deadkeys)'#13#10);
    end;

    for dk in alDead do
    begin
      sbDKFrom := Format('store(dkf%s)', [IntToHexLC(Ord(dk.DeadCharacter),4)]);
      sbDKTo := Format('store(dkt%s)', [IntToHexLC(Ord(dk.DeadCharacter), 4)]);
      for id := 0 to dk.Count - 1 do
      begin
        // Surrogate pairs not supported in kbd.dll
        sbDKFrom := sbDKFrom + ' U+'+IntToHexLC(Ord(dk.GetBaseCharacter(id)), 4);
        sbDKTo := sbDKTo + ' U+'+IntToHexLC(Ord(dk.GetCombinedCharacter(id)), 4);
      end;

      outFile.Add(sbDKFrom);
      outFile.Add(sbDKTo);
      outFile.Add(Format('dk(%0:s) any(dkf%0:s) > index(dkt%0:s, 2)'#13#10,
        [IntToHexLC(Ord(dk.DeadCharacter), 4)]));
    end;

    outFile.Add('');

    Result := outFile.Text;
  finally
    outFile.Free;
  end;
end;

function TLoader.WriteOutputKVKSFile: string;
var
  iKey: UINT;
  vk: TVisualKeyboard;
  ss: TStringStream;
begin
  vk := TVisualKeyboard.Create;
  ss := TStringStream.Create('', TEncoding.UTF8);
  try
    for iKey := 0 to Length(rgKey) - 1 do
    begin
      if (rgKey[iKey] <> nil) and rgKey[iKey].IsKeymanUsedKey and not rgKey[iKey].IsEmpty then
      begin
        rgKey[iKey].AddKeysToVisualKeyboard(vk);
      end;
    end;

    if FHas102ndKey then
      vk.Header.Flags := vk.Header.Flags + [kvkh102];

    vk.SaveToStream(ss, kvksfXML);

    Result := ss.DataString;
  finally
    vk.Free;
    ss.Free;
  end;
end;

{ TImportKeyboardDLL }

const
  CRootKey = 'System\CurrentControlSet\Control\Keyboard Layouts';

constructor TImportKeyboardDLL.Create(const inputHKL: string);
var
  r: TRegistry;
begin
  inherited Create;
  FInputHKL := inputHKL;

  // Because the output filename is optional, we read layoutFile, layoutText
  // here in order to populate the outputFileName. layoutText is also used in
  // comment data when genrating the .kmn file
  r := TRegistry.Create;
  try
    r.RootKey := HKEY_LOCAL_MACHINE;
    if not r.OpenKeyReadOnly(CRootKey + '\' + inputHKL) then
      raise EImportKeyboardDLL.CreateFmt('The keyboard %s could not be found in the registry (error %d, %s)',
        [inputHKL, GetLastError, SysErrorMessage(GetLastError)]);

    FLayoutFile := r.ReadString('Layout File');
    FLayoutText := r.ReadString('Layout Text');
  finally
    r.Free;
  end;

  FRecommendedOutputKMNFileName := ChangeFileExt(FLayoutFile, '.kmn');
end;

procedure TImportKeyboardDLL.Execute;
begin
  Loader := TLoader.Create(FInputHKL, FLayoutFile, FLayoutText);
  try
    Loader.Main(FKMN, FKVKS);
  finally
    FreeAndNil(Loader);
  end;
end;

end.
