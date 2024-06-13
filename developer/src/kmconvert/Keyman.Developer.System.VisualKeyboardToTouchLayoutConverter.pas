unit Keyman.Developer.System.VisualKeyboardToTouchLayoutConverter;

interface

uses
  System.Classes,

  VisualKeyboard;

type
  TVisualKeyboardToTouchLayoutConverter = class
  private
    FOwnsVisualKeyboard: Boolean;
    FVisualKeyboard: TVisualKeyboard;
    function ImportFromKVK: string;
    function SetupModifierKeysForImportedLayout(TouchLayout: string): string;
  public
    constructor Create(const AFileName: string); overload;
    constructor Create(const AStream: TStream); overload;
    constructor Create(AVisualKeyboard: TVisualKeyboard); overload;
    function Execute(var KeymanTouchLayout: string): Boolean;
    destructor Destroy; override;
  end;

implementation

uses
  System.SysUtils,

  OnScreenKeyboardData,
  RedistFiles,
  ScanCodeMap,
  TouchLayout,
  Unicode,
  VKeys;

constructor TVisualKeyboardToTouchLayoutConverter.Create(
  const AFileName: string);
begin
  inherited Create;
  FOwnsVisualKeyboard := True;
  FVisualKeyboard := TVisualKeyboard.Create;
  FVisualKeyboard.LoadFromFile(AFileName);
end;

constructor TVisualKeyboardToTouchLayoutConverter.Create(
  AVisualKeyboard: TVisualKeyboard);
begin
  inherited Create;
  FOwnsVisualKeyboard := False;
  FVisualKeyboard := AVisualKeyboard;
end;

constructor TVisualKeyboardToTouchLayoutConverter.Create(
  const AStream: TStream);
begin
  inherited Create;
  FOwnsVisualKeyboard := True;
  FVisualKeyboard := TVisualKeyboard.Create;
  FVisualKeyboard.LoadFromStream(AStream);
end;

destructor TVisualKeyboardToTouchLayoutConverter.Destroy;
begin
  if FOwnsVisualKeyboard then
    FreeAndNil(FVisualKeyboard);
  inherited Destroy;
end;

function TVisualKeyboardToTouchLayoutConverter.Execute(
  var KeymanTouchLayout: string): Boolean;
begin
  KeymanTouchLayout := ImportFromKVK;
  Result := KeymanTouchLayout <> '';
end;

function TVisualKeyboardToTouchLayoutConverter.ImportFromKVK: string;
type
  TKeyData = record
    Data: TOnScreenKeyboardKeyData;
    VKey: Integer;
    Cap: string;
  end;

var
  I: Integer;
  f: AnsiString;
  n: Integer;
  nrow: Integer;
  LocalKeyData: array[0..58] of TKeyData;
  baseTemplate: AnsiString;
  baseTemplateLayer: AnsiString;
  FTouchLayout: TTouchLayout;
  FOldLayout: TTouchLayout;

    function GetKeyIndex(VK: Word): Integer;
    var
      I: Integer;
    begin
      for I := 0 to High(LocalKeyData) do
        if LocalKeyData[I].VKey = VK then
        begin
          Result := I;
          Exit;
        end;
      Result := -1;
    end;

    function FillOSK(Shift: Word): Boolean;
    var
      i, n: Integer;
      Found: Boolean;
    begin
      Found := False;
      for i := 0 to High(LocalKeyData) do
      begin
        LocalKeyData[i].Data := KeyData[i];
        LocalKeyData[i].VKey := MapScanCodeToUSVK(KeyData[i].ScanCode);
        LocalKeyData[i].Cap := '';   // I4146
      end;

      for i := 0 to FVisualKeyboard.Keys.Count - 1 do
      begin
        if (FVisualKeyboard.Keys[i].Shift = Shift) and (kvkkUnicode in FVisualKeyboard.Keys[i].Flags) then
        begin
          n := GetKeyIndex(FVisualKeyboard.Keys[i].VKey);
          if n < 0 then Continue;
          LocalKeyData[n].Cap := FVisualKeyboard.Keys[i].Text;
          Found := True;
        end;
      end;

      Result := Found;   // I4146
    end;

// string escaping and un-escaping

    function EscapeString(const s:WideString): ansistring;
    var
      i:Integer;
    begin
      Result := '"';
      for i:=1 to Length(s) do
        case s[i] Of
          '/', '\', '"': Result := Result + '\' + AnsiChar(s[i]);
          #8: Result := Result + '\b';
          #9: Result := Result + '\t';
          #10: Result := Result + '\n';
          #12: Result := Result + '\f';
          #13: Result := Result + '\r';
          else
            if CharInSet(s[i], [WideChar(' ') .. WideChar('~')])
              then Result := Result + AnsiChar(s[i])
              else Result := Result + '\u'+AnsiString(IntToHex(Ord(s[i]),4));
        end;
      Result := Result + '"';
    end;

type
  TLayoutShiftState = record Shift: Word; id: ansistring end;
const
  // These layer names match those in KeymanWeb, kmwosk.ts osk.modifierSpecials
  ShiftStates: array[0..21] of TLayoutShiftState = (
    (Shift: KVKS_NORMAL; id: 'default'),
    (Shift: KVKS_SHIFT; id: 'shift'),
    (Shift: KVKS_CTRL; id: 'ctrl'),
    (Shift: KVKS_SHIFT or KVKS_CTRL; id: 'shift-ctrl'),
    (Shift: KVKS_ALT; id: 'alt'),
    (Shift: KVKS_SHIFT or KVKS_ALT; id: 'shift-alt'),
    (Shift: KVKS_CTRL or KVKS_ALT; id: 'ctrl-alt'),
    (Shift: KVKS_SHIFT or KVKS_CTRL or KVKS_ALT; id: 'shift-ctrl-alt'),

    (Shift: KVKS_NORMAL; id: 'default'),
    (Shift: KVKS_LCTRL; id: 'leftctrl'),
    (Shift: KVKS_RCTRL; id: 'rightctrl'),
    (Shift: KVKS_LALT; id: 'leftalt'),
    (Shift: KVKS_LCTRL or KVKS_LALT; id: 'leftctrl-leftalt'),
    (Shift: KVKS_RALT; id: 'rightalt'),
    (Shift: KVKS_RCTRL or KVKS_RALT; id: 'rightctrl-rightalt'),
    (Shift: KVKS_SHIFT; id: 'shift'),
    (Shift: KVKS_SHIFT or KVKS_LCTRL; id: 'leftctrl-shift'),
    (Shift: KVKS_SHIFT or KVKS_RCTRL; id: 'rightctrl-shift'),
    (Shift: KVKS_SHIFT or KVKS_LALT; id: 'leftalt-shift'),
    (Shift: KVKS_SHIFT or KVKS_RALT; id: 'rightalt-shift'),
    (Shift: KVKS_SHIFT or KVKS_LCTRL or KVKS_LALT; id: 'leftctrl-leftalt-shift'),
    (Shift: KVKS_SHIFT or KVKS_RCTRL or KVKS_RALT; id: 'rightctrl-rightalt-shift')
  );
const
  NFirstChiralShiftState = 8;
var
  n1, n2: Integer;
begin


  baseTemplate := '{ "tablet": { "layer": [';
  f := '{ "desktop": { "font": '+EscapeString(FVisualKeyboard.Header.UnicodeFont.Name)+', "layer": [';

  if kvkhAltGr in FVisualKeyboard.Header.Flags then
  begin
    n1 := NFirstChiralShiftState;
    n2 := High(ShiftStates);
  end
  else
  begin
    n1 := 0;
    n2 := NFirstChiralShiftState - 1;
  end;

  with TStringList.Create do
  try
    LoadFromFile(GetLayoutBuilderPath + 'physical-keyboard-template.js');
    baseTemplateLayer := String_UtoA(Text);
  finally
    Free;
  end;

  for n := n1 to n2 do
  begin
    if not FillOSK(ShiftStates[n].Shift) then
    begin
      Continue;
    end;

    if n > n1 then
    begin
      f := f + ',';
      baseTemplate := baseTemplate + ',';
    end;

    f := f + '{' +
      '"id": "'+ShiftStates[n].id+'",'+
      '"row": [';

    baseTemplate := baseTemplate + '{' +
      '"id": "'+ShiftStates[n].id+'",' +
      baseTemplateLayer+
      '}';

    nrow := 1;
    for I := 0 to High(LocalKeyData) do
    begin
      if LocalKeyData[i].Data.Row then
      begin
        if I > 0 then
          f := f + ']},';
        f := f + '{"id": '+AnsiString(IntToStr(nrow))+',"key": [ ';
        Inc(nrow);
      end
      else
        f := f + ',';

      f := f + '{"id": "'+AnsiString(VKeyNames[LocalKeyData[i].VKey])+'",'+
                '"text": '+EscapeString(LocalKeyData[i].Cap);
      if LocalKeyData[i].Data.Width <> 33 then
        f := f + ',"width": "'+AnsiString(IntToStr((LocalKeyData[i].Data.Width-1)*2))+'"';
      if LocalKeyData[i].Data.KeyType <> kktNormal then
        f := f + ',"sp": "1", "fontsize": "0.5em"';
      f := f + '}';
    end;

    f := f + ']}'+  // END key, last row
             ']}';  // END row array, layer
  end;
  f := f + ']}}';

  baseTemplate := baseTemplate + ']}}';

  Result := String_AtoU(f);

  // Merge the two files -- the base template and the constructed KVK keys
  //Load(String_AtoU(baseTemplate),True,True);

  FTouchLayout := TTouchLayout.Create;   // I3642
  try
    if FTouchLayout.Load(String_AtoU(baseTemplate)) then
    begin
      FOldLayout := TTouchLayout.Create;
      try
        FOldLayout.Load(Result);
        if FTouchLayout.Merge(FOldLayout)
          then Result := FTouchLayout.Save(False);
      finally
        FOldLayout.Free;
      end;
    end;
  finally
    FTouchLayout.Free;
  end;

  // Setup modifier keys in the final keyboard
  Result := SetupModifierKeysForImportedLayout(Result);
end;

{**
 * Sets up the K_LCONTROL modifier key to access all other layers for
 * each layer on each given platform. Will rewrite the K_LCONTROL key
 * or remove it if there are no extended layers.
 *}
function TVisualKeyboardToTouchLayoutConverter.SetupModifierKeysForImportedLayout(TouchLayout: string): string;
  procedure SetupModifierKeysForPlatform(p: TTouchLayoutPlatform);
  var
    l, l2: TTouchLayoutLayer;
    k: TTouchLayoutKey;
    nExtendedLayers: Integer;
    FirstExtendedLayerID: string;
    sk: TTouchLayoutSubKey;
  begin
    nExtendedLayers := 0;

    for l in p.Layers do
    begin
      if (l.id <> 'default') and (l.Id <> 'shift') then
      begin
        Inc(nExtendedLayers);
        if FirstExtendedLayerID = '' then
          FirstExtendedLayerID := l.Id;
      end;
    end;

    for l in p.Layers do
    begin
      // Find the Ctrl key for the layer
      k := l.FindKeyById('K_LCONTROL');
      if not Assigned(k) then
        Continue;

      // If there are no extended layers, delete it
      if nExtendedLayers = 0 then
      begin
        (k.Parent as TTouchLayoutRow).Keys.Remove(k);
        // Stretch Spacebar?
        Continue;
      end;

      // If there are extended layers, set it up to
      // go to the first extended layer from Default, Shift, or
      // return to default from any other layer.
      // Long-press will go to other extended layers.

      if (l.id = 'default') or (l.Id = 'shift') then
      begin
        k.NextLayer := FirstExtendedLayerID;
        k.Text := FirstExtendedLayerID;
      end
      else
      begin
        k.NextLayer := 'default';
        k.Text := 'default';
      end;

      if nExtendedLayers > 1 then
      begin
        for l2 in p.Layers do
        begin
          if (l2.id = 'default') or (l2.Id = 'shift') then Continue;
          if l2.id = l.id then Continue;
          sk := TTouchLayoutSubKey.Create(k);
          sk.Id := 'K_LCONTROL';
          sk.Text := l2.id;
          sk.SpT := tktSpecial;
          sk.NextLayer := l2.Id;
          k.Sk.Add(sk);
        end;
      end;
    end;
  end;
var
  p: TTouchLayoutPlatform;
begin
  with TTouchLayout.Create do
  try
    Load(TouchLayout);
    for p in Data.Platforms do
    begin
      SetupModifierKeysForPlatform(p);
    end;
    Result := Write(True);
  finally
    Free;
  end;
end;

end.
