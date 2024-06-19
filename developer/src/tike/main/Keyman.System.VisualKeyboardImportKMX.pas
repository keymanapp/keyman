{
 * Keyman is copyright (C) SIL International. MIT License.
 *
 * Generate a visual keyboard (.kvk[s]) from a .kmx
}
unit Keyman.System.VisualKeyboardImportKMX;

interface

uses
  System.Generics.Collections,
  System.SysUtils,

  Keyman.System.Debug.DebugCore,
  Keyman.System.Debug.DebugEvent,
  Keyman.System.KeymanCore,
  kmxfile,
  VisualKeyboard;

type
  EVisualKeyboardImportKMX = class(Exception);

  TVisualKeyboardImportKMX = class
  private
    type
      TVKKey = class
        vkey: Word;
        kmshift, shift: Integer;
      end;
  private
    keys: TObjectList<TVKKey>;
    FVK: TVisualKeyboard;
    FEvents: TDebugEventList;
    FCore: TDebugCore;
    FShow102Key: Boolean;
    FLeftRightCtrlAlt: Boolean;
    procedure GetKeyboardKeys(const AFilename: string);
    function KMXShiftToActiveShift(shift: Cardinal): Cardinal;
    function KMXShiftToVKShift(shift: Cardinal): Integer;
    procedure ReadKeys(kfh: PKeyboardFileHeader; groupindex: Integer);
    procedure Validate102Key;
    procedure ValidateLeftRightCtrlAlt;
    procedure AddKey(vk: TVKKey; const data: string);
    procedure ImportKey(vk: TVKKey);
  public
    constructor Create(const AFilename: string; AVK: TVisualKeyboard);
    destructor Destroy; override;
    procedure ImportKeys;

    property Show102Key: Boolean read FShow102Key;
    property LeftRightCtrlAlt: Boolean read FLeftRightCtrlAlt;
  end;

implementation

uses
  kmxfileconsts,
  Unicode,
  VKeyChars;

{ TVisualKeyboardImportKMX }

constructor TVisualKeyboardImportKMX.Create(
  const AFilename: string;
  AVK: TVisualKeyboard
);
begin
  inherited Create;

  FVK := AVK;

  keys := TObjectList<TVKKey>.Create;

  GetKeyboardKeys(AFilename);

  FEvents := TDebugEventList.Create;
  try
    FCore := TDebugCore.Create(AFileName, False);
  except
    on E:Exception do
      raise EVisualKeyboardImportKMX.Create('Could not load Core to import keyboard: '+E.Message);
  end;
end;

destructor TVisualKeyboardImportKMX.Destroy;
begin
  FreeAndNil(keys);
  FreeAndNil(FEvents);
  FreeAndNil(FCore);
  inherited Destroy;
end;

procedure TVisualKeyboardImportKMX.ImportKeys;
var
  key: TVKKey;
begin
  for key in keys do
    ImportKey(key);
end;

{-------------------------------------------------------------------------------
 - Send a keystroke for processing                                             -
 ------------------------------------------------------------------------------}

procedure TVisualKeyboardImportKMX.ImportKey(vk: TVKKey);
var
  data: string;
  i: Integer;
begin
  km_core_state_context_clear(FCore.State);
  if km_core_process_event(FCore.State, vk.vkey, vk.kmshift, 1, KM_CORE_EVENT_FLAG_DEFAULT) = KM_CORE_STATUS_OK then
  begin
    FEvents.Clear;
    FEvents.AddStateItems(FCore.State, vk.vkey, vk.kmshift);

    data := '';
    for i := 0 to FEvents.Count - 1 do
    begin
      if FEvents[i].EventType <> etAction then
      begin
        Continue;
      end;

      case FEvents[i].Action.ActionType of
        KM_CORE_IT_CHAR:
          data := data + FEvents[i].Action.Text;
        KM_CORE_IT_BACK:
          if data.Length > 0 then
          begin
            if (data.Length > 1) and
                Uni_IsSurrogate1(data[data.Length-1]) and
                Uni_IsSurrogate2(data[data.Length])
              then Delete(data, data.Length - 1, 2)
              else Delete(data, data.Length, 1);
          end;
      end;
    end;

    if Trim(data) <> '' then
    begin
      AddKey(vk, data);
    end;
  end;
end;

{-------------------------------------------------------------------------------
 - Process the results of the keystroke                                        -
 ------------------------------------------------------------------------------}

procedure TVisualKeyboardImportKMX.AddKey(vk: TVKKey; const data: string);
var
  k: TVisualKeyboardKey;
  n: Integer;
begin
  n := FVK.Keys.IndexOf(vk.vkey, vk.shift);
  if n < 0
    then k := TVisualKeyboardKey.Create
    else k := FVK.Keys[n];
  k.Flags := [kvkkUnicode];
  k.VKey := vk.vkey;
  k.Shift := vk.shift;
  k.Text := data;
  if n < 0 then FVK.Keys.Add(k);
end;

{-------------------------------------------------------------------------------
 - Read a list of keys from the keyboard                                       -
 ------------------------------------------------------------------------------}

procedure TVisualKeyboardImportKMX.GetKeyboardKeys(const AFilename: string);
var
  ki: TKeyboardInfo;
  i: Integer;
  kfh: PKeyboardFileHeader;
begin
  try
    GetKeyboardInfo(AFileName, True, ki);
  except
    on E:Exception do
      raise EVisualKeyboardImportKMX.Create('Unable to load keyboard: '+E.Message);
  end;

  try
    kfh := PKeyboardFileHeader(ki.MemoryDump.Memory);
    for i := 0 to kfh.cxGroupArray - 1 do
      ReadKeys(kfh, i);
  finally
    ki.MemoryDump.Free;
  end;

  ValidateLeftRightCtrlAlt;
  Validate102Key;
end;

procedure TVisualKeyboardImportKMX.Validate102Key;
var
  i: Integer;
begin
  FShow102Key := False;

  for i := 0 to keys.Count - 1 do
    if TVKKey(keys[i]).vkey = KM_CORE_VKEY_oE2 then
    begin
      FShow102Key := True;
      Exit;
    end;
end;

procedure TVisualKeyboardImportKMX.ValidateLeftRightCtrlAlt;
var
  i: Integer;
  FLR: Boolean;
  vk: TVKKey;
begin
  FLR := False;
  for i := 0 to keys.Count - 1 do
  begin
    vk := TVKKey(keys[i]);
    if (vk.shift and (KVKS_LALT or KVKS_RALT or KVKS_LCTRL or KVKS_RCTRL)) <> 0 then
    begin
      FLR := True;
      Break;
    end;
  end;

  if FLR then
    for i := 0 to keys.Count - 1 do
    begin
      vk := TVKKey(keys[i]);
      if (vk.shift and KVKS_ALT) <> 0 then
        vk.shift := (vk.shift and (not KVKS_ALT)) or KVKS_LALT;
      if (vk.shift and KVKS_CTRL) <> 0 then
        vk.shift := (vk.shift and (not KVKS_CTRL)) or KVKS_LCTRL;
    end;

  FLeftRightCtrlAlt := FLR;
end;

function TVisualKeyboardImportKMX.KMXShiftToVKShift(shift: Cardinal): Integer;
begin
  Result := 0;
  if (shift and KMX_LCTRLFLAG) <> 0 then Result := Result or KVKS_LCTRL;
  if (shift and KMX_RCTRLFLAG) <> 0 then Result := Result or KVKS_RCTRL;
  if (shift and KMX_LALTFLAG) <> 0  then Result := Result or KVKS_LALT;
  if (shift and KMX_RALTFLAG) <> 0  then Result := Result or KVKS_RALT;
  if (shift and KMX_SHIFTFLAG) <> 0 then Result := Result or KVKS_SHIFT;
  if (shift and KMX_CTRLFLAG) <> 0  then Result := Result or KVKS_CTRL;
  if (shift and KMX_ALTFLAG) <> 0   then Result := Result or KVKS_ALT;
end;

function TVisualKeyboardImportKMX.KMXShiftToActiveShift(shift: Cardinal): Cardinal;
begin
  Result := 0;
  if (shift and KMX_LCTRLFLAG) <> 0 then Result := Result or KMX_LCTRLFLAG;
  if (shift and KMX_RCTRLFLAG) <> 0 then Result := Result or KMX_RCTRLFLAG;
  if (shift and KMX_LALTFLAG) <> 0  then Result := Result or KMX_LALTFLAG;
  if (shift and KMX_RALTFLAG) <> 0  then Result := Result or KMX_RALTFLAG;
  if (shift and KMX_SHIFTFLAG) <> 0 then Result := Result or KMX_SHIFTFLAG;
  if (shift and KMX_CTRLFLAG) <> 0  then Result := Result or KMX_LCTRLFLAG;
  if (shift and KMX_ALTFLAG) <> 0   then Result := Result or KMX_LALTFLAG;
end;

procedure TVisualKeyboardImportKMX.ReadKeys(kfh: PKeyboardFileHeader; groupindex: Integer);
var
  gp: PKeyboardFileGroup;
  kp: PKeyboardFileKey;
  vkey, kmshift, shift, i, j: Integer;
  vk: TVKKey;
  Found: Boolean;
  usvkey: TUSVKey;
begin
  gp := PKeyboardFileGroup(NativeUInt(kfh)+kfh.dpGroupArray+NativeUInt(groupindex)*sizeof(TKeyboardFileGroup));
  if not gp.fUsingKeys then Exit;

  kp := PKeyboardFileKey(NativeUInt(kfh)+gp.dpKeyArray);
  for i := 0 to Integer(gp.cxKeyArray) - 1 do
  begin
    Found := False;
    if (kp.ShiftFlags and KMX_ISVIRTUALKEY) = 0 then
    begin
      usvkey := CharToUSVKey(AnsiChar(kp.Key));
      vkey := usvkey.VKey;
      if usvkey.IsShift
        then kmshift := KMX_SHIFTFLAG
        else kmshift := 0;
      shift := KMXShiftToVKShift(kmshift);
    end
    else
    begin
      vkey := kp.Key;
      shift := KMXShiftToVKShift(kp.ShiftFlags);
      kmshift := KMXShiftToActiveShift(kp.ShiftFlags);
    end;

    if (vkey > 255) or (vkey = 0) then
    begin
      // We don't try and import keys that are T_ touch virtual keys or any
      // unrecognised keys (vkey = 0)
      Inc(kp);
      Continue;
    end;

    for j := 0 to keys.Count - 1 do
      if (TVKKey(keys[j]).VKey = vkey) and (TVKKey(keys[j]).Shift = shift) then
      begin
        Found := True;
        Break;
      end;
    if Found then
    begin
      Inc(kp);
      Continue;
    end;

    vk := TVKKey.Create;
    vk.VKey := vkey;
    vk.Shift := shift;
    vk.kmshift := kmshift;
    keys.Add(vk);

    Inc(kp);
  end;
end;

end.
