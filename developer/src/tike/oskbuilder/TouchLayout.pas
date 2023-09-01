(*
  Name:             TouchLayout
  Copyright:        Copyright (C) SIL International.
  Documentation:
  Description:
  Create Date:      29 Nov 2013

  Modified Date:    27 May 2015
  Authors:          mcdurdin
  Related Files:
  Dependencies:

  Bugs:
  Todo:
  Notes:
  History:          29 Nov 2013 - mcdurdin - I3982 - V9.0 - Touch layout editor should not override special keys when swapping layouts
                    07 Feb 2014 - mcdurdin - I4035 - V9.0 - Parsing of UTF-8 in JSON layout file crashes TIKE
                    21 Feb 2014 - mcdurdin - I4060 - V9.0 - KeymanWeb compiler should validate the layout file
                    21 Feb 2014 - mcdurdin - I4062 - V9.0 - Touch Layout platforms do not allow font size specification
                    27 Feb 2014 - mcdurdin - I4083 - V9.0 - When errors encountered in JSON layout file, locate the error in the source view
                    06 Mar 2014 - mcdurdin - I4119 - V9.0 - KMW compiler should only warn unassociated keys that are not special keys
                    27 May 2015 - mcdurdin - I4655 - Developer crashes when changing font settings and in code view for touch layout if not on first line [CrashID:tike.exe_9.0.487.0_0069BE51_ERangeError]
*)
unit TouchLayout;   // I4060

interface

uses
  System.JSON,
  System.Generics.Collections,
  System.SysUtils;

type
  ETouchLayoutValidate = class(Exception)
  end;

  TTouchLayoutFlickDirection = (
    tlfdNorth, tlfdSouth, tlfdEast, tlfdWest,
    tlfdNorthEast, tlfdNorthWest,
    tlfdSouthEast, tlfdSouthWest
  );

const
  STouchLayoutFlickDirectionName: array[TTouchLayoutFlickDirection] of string = (
    'n', 's', 'e', 'w',
    'ne', 'nw',
    'se', 'sw'
  );

type
  // TouchKeyType
  TTouchKeyType = (tktNormal = 0, tktSpecial = 1, tktSpecialActive = 2, tktDeadkey = 8, tktBlank = 9, tktSpacer = 10);   // I4119

  TTouchLayoutClass = class of TTouchLayoutObject;

  TTouchLayoutObject = class;

  TTouchLayoutAddProc = reference to procedure(obj:TTouchLayoutObject);

  TTouchLayoutObject = class
  private
    FJSON: TJSONObject;
    FParent: TTouchLayoutObject;
  protected
    function GetValue(const name: string; var value: TJSONValue): Boolean; overload;
    function GetValue(const name: string; var value: string): Boolean; overload;
    function GetValue(const name: string; var value: Integer): Boolean; overload;
    function GetValue(const name: string; var value: Boolean): Boolean; overload;

    procedure AddJSONValue(JSON: TJSONObject; const name, value: string); overload;
    procedure AddJSONValue(JSON: TJSONObject; const name: string;
      value: Integer); overload;
    procedure AddJSONValue(JSON: TJSONObject; const name: string;
      value: Boolean); overload;

    //function GetArray(const name: string): TJSONArray; overload;
    procedure ReadArray(const name: string; cls: TTouchLayoutClass; add: TTouchLayoutAddProc);
    procedure DoRead; virtual; abstract;
    procedure DoWrite(JSON: TJSONObject); virtual; abstract;
  public
    constructor Create(AParent: TTouchLayoutObject); virtual;
    procedure Read(JSON: TJSONObject);
    procedure Write(JSON: TJSONObject);
    property Parent: TTouchLayoutObject read FParent;
  end;

  TTouchLayoutSubKey = class(TTouchLayoutObject)
  private
    FNextLayer: string;
    FFontSize: string;
    FLayer: string;
    FWidth: Integer;
    FDk: Integer;
    FFont: string;
    FId: string;
    FPad: Integer;
    FSp: Integer;
    FText: string;
    FDefault: boolean;
    function GetSpT: TTouchKeyType;   // I4119
    procedure SetSpT(const Value: TTouchKeyType);   // I4119
  protected
    procedure DoRead; override;
    procedure DoWrite(JSON: TJSONObject); override;
  public
    property Id: string read FId write FId;
    property Text: string read FText write FText;
    property Sp: Integer read FSp write FSp;
    property SpT: TTouchKeyType read GetSpT write SetSpT;   // I4119
    property Width: Integer read FWidth write FWidth;
    property Pad: Integer read FPad write FPad;
    property Dk: Integer read FDk write FDk;
    property Layer: string read FLayer write FLayer;
    property NextLayer: string read FNextLayer write FNextLayer;
    property Font: string read FFont write FFont;
    property FontSize: string read FFontSize write FFontSize;
    property Default: boolean read FDefault write FDefault;
  end;

  TTouchLayoutSubKeys = class(TObjectList<TTouchLayoutSubKey>)
  end;

  TTouchLayoutMultiTaps = class(TObjectList<TTouchLayoutSubKey>)
  end;

  TTouchLayoutFlicks = class(TObjectDictionary<TTouchLayoutFlickDirection, TTouchLayoutSubKey>)
  end;

  TTouchLayoutKey = class(TTouchLayoutObject)
  private
    FNextLayer: string;
    FFontSize: string;
    FLayer: string;
    FWidth: Integer;
    FDk: Integer;
    FFont: string;
    FId: string;
    FPad: Integer;
    FSp: Integer;
    FSk: TTouchlayoutSubKeys;
    FFlick: TTouchLayoutFlicks;
    FMultiTap: TTouchLayoutMultiTaps;
    FText: string;
    FHint: string;
    function GetSpT: TTouchKeyType;   // I4119
    procedure SetSpT(const Value: TTouchKeyType);   // I4119
  protected
    procedure DoRead; override;
    procedure DoWrite(JSON: TJSONObject); override;
  public
    constructor Create(AParent: TTouchLayoutObject); override;
    destructor Destroy; override;
    property Id: string read FId write FId;
    property Text: string read FText write FText;
    property Hint: string read FHint write FHint;
    property Sp: Integer read FSp write FSp;
    property SpT: TTouchKeyType read GetSpT write SetSpT;   // I4119
    property Width: Integer read FWidth write FWidth;
    property Pad: Integer read FPad write FPad;
    property Dk: Integer read FDk write FDk;
    property Layer: string read FLayer write FLayer;
    property NextLayer: string read FNextLayer write FNextLayer;
    property Font: string read FFont write FFont;
    property FontSize: string read FFontSize write FFontSize;
    property Sk: TTouchlayoutSubKeys read FSk;
    property Flick: TTouchLayoutFlicks read FFlick;
    property MultiTap: TTouchLayoutMultiTaps read FMultiTap;
  end;

  TTouchLayoutKeys = class(TObjectList<TTouchLayoutKey>)
  end;

  TTouchLayoutRow = class(TTouchLayoutObject)
  private
    FId: string;
    FKeys: TTouchLayoutKeys;
  protected
    procedure DoRead; override;
    procedure DoWrite(JSON: TJSONObject); override;
  public
    constructor Create(AParent: TTouchLayoutObject); override;
    destructor Destroy; override;
    function FindKeyById(Id: string): TTouchLayoutKey;
    property Id: string read FId write FId;
    property Keys: TTouchLayoutKeys read FKeys;
  end;

  TTouchLayoutRows = class(TObjectList<TTouchLayoutRow>)
  end;

  TTouchLayoutLayer = class(TTouchLayoutObject)
  private
    FRows: TTouchLayoutRows;
    FId: string;
  protected
    procedure DoRead; override;
    procedure DoWrite(JSON: TJSONObject); override;
  public
    constructor Create(AParent: TTouchLayoutObject); override;
    destructor Destroy; override;
    function FindKeyById(Id: string): TTouchLayoutKey;
    property Id: string read FId write FId;
    property Rows: TTouchLayoutRows read FRows;
  end;

  TTouchLayoutLayers = class(TObjectList<TTouchLayoutLayer>)
  public
    function IndexOfId(const AId: string): Integer;
  end;

  TTouchLayoutDefaultHint = (
    tldhNone, tldhLongpress, tldhMultitap, tldhFlick,
    tldhFlickN, tldhFlickNE, tldhFlickE, tldhFlickSE, tldhFlickS, tldhFlickSW, tldhFlickW, tldhFlickNW
  );

const
  STouchLayoutDefaultHintName: array[TTouchLayoutDefaultHint] of string =
    ('none','longpress','multitap','flick','flick-n','flick-ne','flick-e','flick-se','flick-s','flick-sw','flick-w','flick-nw');


function DefaultHintFromString(const s: string): TTouchLayoutDefaultHint;

type
  TTouchLayoutPlatform = class(TTouchLayoutObject)
  private
    FName: string;
    FLayers: TTouchLayoutLayers;
    FFontSize: string;   // I4062
    FDisplayUnderlying: Boolean;
    FDefaultHint: TTouchLayoutDefaultHint;
    FFont: string;
  protected
    procedure DoRead; override;
    procedure DoWrite(JSON: TJSONObject); override;
  public
    constructor Create(AParent: TTouchLayoutObject); override;
    destructor Destroy; override;
    property DisplayUnderlying: Boolean read FDisplayUnderlying write FDisplayUnderlying;
    property DefaultHint: TTouchLayoutDefaultHint read FDefaultHint write FDefaultHint;
    property Name: string read FName write FName;
    property Font: string read FFont write FFont;
    property FontSize: string read FFontSize write FFontSize;   // I4062
    property Layers: TTouchLayoutLayers read FLayers;
  end;

  TTouchLayoutPlatforms = class(TObjectList<TTouchLayoutPlatform>)
  end;

  TTouchLayoutData = class(TTouchLayoutObject)
    FPlatforms: TTouchLayoutPlatforms;
  protected
    procedure DoRead; override;
    procedure DoWrite(JSON: TJSONObject); override;
  public
    constructor Create(AParent: TTouchLayoutObject); override;
    destructor Destroy; override;
    property Platforms: TTouchLayoutPlatforms read FPlatforms;
  end;

  TTouchLayoutMessageEvent = reference to procedure(Sender: TObject; const msg: string);

  TTouchLayout = class
  private
    FLoadError: string;
    FLayout: TJSONObject;
    FData: TTouchLayoutData;
    FOnMessage: TTouchLayoutMessageEvent;
    FLoadErrorOffset: Integer;   // I4083
    procedure Validate(v: TJSONObject);
    function GetLayoutPlatform(const Index: string): TJSONPair;
    procedure DoError(E: Exception; Offset: Integer); overload;   // I4083
    procedure DoError(const s: string; Offset: Integer); overload;   // I4083
    procedure DoMessage(const s: string);
  public
    constructor Create;
    destructor Destroy; override;
    function Load(json: string): Boolean;
    function Write(AFormatted: Boolean): string;
    function Save(AFormatted: Boolean): string;   // I4655
    function Merge(ASource: TTouchLayout): Boolean;
    function HasPlatform(const APlatform: string): Boolean;
    property LayoutPlatform[const Index: string]: TJSONPair read GetLayoutPlatform;
    property LoadError: string read FLoadError;
    property LoadErrorOffset: Integer read FLoadErrorOffset;   // I4083
    property OnMessage: TTouchLayoutMessageEvent read FOnMessage write FOnMessage;
    property Data: TTouchLayoutData read FData;
  end;

implementation

uses
  System.Classes,

  JsonUtil,
  TouchLayoutDefinitions;

{ TTouchLayout }

procedure TTouchLayout.DoError(E: Exception; Offset: Integer);   // I4083
begin
  DoError('Exception ' + E.ClassName + ' reading JSON data: '+E.Message, Offset);
end;

procedure TTouchLayout.DoError(const s: string; Offset: Integer);   // I4083
begin
  DoMessage(s);
  FLoadError := s;
  FLoadErrorOffset := Offset;
end;

procedure TTouchLayout.DoMessage(const s: string);
begin
  if Assigned(FOnMessage) then
    FOnMessage(Self, s);
end;

function TTouchLayout.Load(json: string): Boolean;

    function ParseJSONValue(const Data: string; var Offset: Integer): TJSONObject;   // I4035
    var
      UTF8Data: TBytes;
    begin
      UTF8Data := TEncoding.UTF8.GetBytes(Data);
      Result := TJSONObject.ParseJSONValue(UTF8Data, Offset, True) as TJSONObject;   // I4083
    end;

var
  v: TJSONObject;
  Offset: Integer;
begin
  Offset := 0;
  try
    v := ParseJSONValue(TrimRight(JSON), Offset) as TJSONObject;   // I4035
    if not Assigned(v) then
    begin
      DoError('Invalid JSON at offset '+IntToStr(Offset), Offset);   // I4035   // I4083
      Exit(False);
    end;
  except
    on E:EJSONException do
    begin
      DoError(E, 0);   // I4083
      Exit(False);
    end;
  end;

  try
    Validate(v);
  except
    on E:ETouchLayoutValidate do
    begin
      DoError(E, 0);   // I4083
      Exit(False);
    end;
  end;

  FreeAndNil(FData);
  FData := TTouchLayoutData.Create(nil);
  FData.Read(v);

  FLayout := v;
  Result := True;
end;

type
  TKeyCallback = reference to function(ALayer, AKey: TJSONObject): Boolean;

procedure EachKey(APlatform: TJSONObject; ACall: TKeyCallback);
var
  FLayers: TJSONArray;
  FLayer: TJSONObject;
  FRows: TJSONArray;
  FRow: TJSONObject;
  FKeys: TJSONArray;
  FKey: TJSONObject;
  FLayerBase: TJSONValue;
  FRowBase: TJSONValue;
  FKeyBase: TJSONValue;
begin
  FLayers := APlatform.Get('layer').JsonValue as TJsonArray;
  for FLayerBase in FLayers do
  begin
    FLayer := FLayerBase as TJsonObject;
    FRows := FLayer.Get('row').JsonValue as TJsonArray;
    for FRowBase in FRows do
    begin
      FRow := FRowBase as TJsonObject;
      FKeys := FRow.Get('key').JsonValue as TJsonArray;
      for FKeyBase in FKeys do
      begin
        FKey := FKeyBase as TJsonObject;
        if not ACall(FLayer, FKey) then Exit;
      end
    end;
  end;
end;

function GetKeyPredefinedLayer(layer, key: TJSONObject): TTouchLayoutPredefinedLayer;
var
  pair: TJSONPair;
  i: Integer;
begin
  pair := key.Get('layer');
  if pair <> nil
    then Result := (pair.JsonValue as TJSONString).Value
    else Result := (layer.Get('id').JsonValue as TJSONString).Value;

  for i := 0 to High(PredefinedLayers) do
    if PredefinedLayers[i] = Result then Exit;

  Result := 'default';
end;

function TTouchLayout.Merge(ASource: TTouchLayout): Boolean;



  procedure MergePlatform(const ASourcePlatformName, ADestPlatformName: string);
  var
    dstPlatform: TJSONObject;
  begin
    dstPlatform := FLayout.Get(ADestPlatformName).JsonValue as TJSONObject;

    // For each key in the dstPlatform, find the equivalent in srcPlatform
    // Copy the key caps in

    EachKey(dstPlatform,
      function(dstLayer, dstKey: TJSONObject): Boolean
      var
        srcPlatform: TJSONObject;
        dstPredefinedLayer: TTouchLayoutPredefinedLayer;
      begin
        srcPlatform := ASource.FLayout.Get(ASourcePlatformName).JsonValue as TJSONObject;
        dstPredefinedLayer := GetKeyPredefinedLayer(dstLayer, dstKey);

        EachKey(srcPlatform,
          function(srcLayer, srcKey: TJSONObject): Boolean
          var
            srcPredefinedLayer: TTouchLayoutPredefinedLayer;
            v: TJSONString;
            srcText: TJSONPair;
            srcId: TJSONPair;
            dstId: TJSONPair;
            dstSp: TJSONPair;
          begin
            srcId := srcKey.Get('id');
            dstId := dstKey.Get('id');

            dstSp := dstKey.Get('sp');
            if Assigned(dstSp) and (dstSp.JsonValue.ToString = '"1"') then   // I3982
            begin
              // We don't override the template's special keys
              Exit(True);
            end;

            if Assigned(srcId) and Assigned(dstId) and (srcId.JsonValue.ToString = dstId.JsonValue.ToString) then
            begin
              srcPredefinedLayer := GetKeyPredefinedLayer(srcLayer, srcKey);
              if srcPredefinedLayer = dstPredefinedLayer then
              begin
                dstKey.RemovePair('text');
                srcText := srcKey.Get('text');
                if Assigned(srcText) then
                begin
                  v := TJSONString.Create((srcText.JsonValue as TJsonString).Value);
                  dstKey.AddPair('text', v);
                end;
              end;
            end;
            Result := True;
          end
        );
        Result := True;
      end
    );
  end;

begin
  if HasPlatform('desktop') then
  begin
    if ASource.HasPlatform('desktop') then MergePlatform('desktop', 'desktop')
    else if ASource.HasPlatform('tablet') then MergePlatform('tablet', 'desktop')
    else if ASource.HasPlatform('phone') then MergePlatform('phone', 'desktop');
  end;

  if HasPlatform('tablet') then
  begin
    if ASource.HasPlatform('tablet') then MergePlatform('tablet', 'tablet')
    else if ASource.HasPlatform('phone') then MergePlatform('phone', 'tablet')
    else if ASource.HasPlatform('desktop') then MergePlatform('desktop', 'tablet');
  end;

  if HasPlatform('phone') then
  begin
    if ASource.HasPlatform('phone') then MergePlatform('phone', 'phone')
    else if ASource.HasPlatform('tablet') then MergePlatform('tablet', 'phone')
    else if ASource.HasPlatform('desktop') then MergePlatform('desktop', 'phone');
  end;

  Result := True;
end;

constructor TTouchLayout.Create;
begin
  inherited Create;
  FData := TTouchLayoutData.Create(nil);
end;

destructor TTouchLayout.Destroy;
begin
  FreeAndNil(FData);
  inherited Destroy;
end;

function TTouchLayout.GetLayoutPlatform(const Index: string): TJSONPair;
begin
  Result := FLayout.Get(Index);
end;

function TTouchLayout.HasPlatform(const APlatform: string): Boolean;
begin
  Result := FLayout.Get(APlatform) <> nil;
end;

function TTouchLayout.Save(AFormatted: Boolean): string;   // I4655
var
  str: TStringList;
begin
  if not Assigned(FLayout) then
    Result := ''
  else if AFormatted then   // I4655
  begin
    str := TStringList.Create;
    try
      PrettyPrintJSON(FLayout, str);
      Result := str.Text;
    finally
      str.Free;
    end;
  end
  else
    Result := JSONToString(FLayout);
end;

procedure TTouchLayout.Validate(v: TJSONObject);

  procedure Failure(const Message: string; Source: TJSONAncestor = nil);
  begin
    raise ETouchLayoutValidate.Create('Validation failure: '+Message);
  end;

  procedure TouchAssert(v: Boolean; const Message: string; Source: TJSONAncestor = nil);
  begin
    if not v then
      Failure(Message, Source);
  end;

  procedure ValidateObject(AObject: TJSONObject; PDef: PJSONDef; PDefSize: Integer);
  var
    pair: TJSONPair;
    i: Integer;
    Found: Boolean;
    valueArray: TJSONArray;
    member: TJSONValue;
    Def: TJSONDefArray;
  begin
    // Validate required members

    SetLength(Def, PDefSize);
    for i := 0 to PDefSize - 1 do
    begin
      Def[i] := PDef^;
      Inc(PDef);
    end;

    for i := Low(Def) to High(Def) do
    begin
      TouchAssert(not Def[i].Required or (AObject.Get(Def[i].Name) <> nil),
        'Member '+Def[i].Name+' is required', AObject);
    end;

    // Validate all members

    for pair in AObject do
    begin
      Found := False;
      for i := Low(Def) to High(Def) do
      begin
        if Def[i].Name = pair.JsonString.Value then
        begin
          Found := True;

          TouchAssert(pair.JsonValue is Def[i].ClassType,
            pair.JsonString.Value + ' is not a valid type');

          if Def[i].ClassType = TJSONArray then
          begin
            valueArray := pair.JsonValue as TJSONArray;
            for member in valueArray do
            begin
              if Assigned(Def[i].Value) then
              begin
                TouchAssert(member is TJSONObject, 'Array members must be objects', member);
                ValidateObject(member as TJSONObject, Def[i].Value, Def[i].ValueSize);
              end;
            end;
          end
          else if Def[i].ClassType = TJSONObject then
          begin
            ValidateObject(pair.JsonValue as TJSONObject, Def[i].Value, Def[i].ValueSize);
          end;
          Break;
        end;
      end;
      TouchAssert(Found, 'Member '+pair.JsonString.Value+' is not valid', AObject);
    end;
  end;

begin
  TouchAssert(Assigned(v), 'Layout file must not be empty');
  TouchAssert(v.Count > 0, 'At least one layout is required', FLayout);

  ValidateObject(v, @LayoutDef[0], Length(LayoutDef));
end;

function TTouchLayout.Write(AFormatted: Boolean): string;
var
  o: TJSONObject;
begin
  o := TJSONObject.Create;
  FData.Write(o);
  FreeAndNil(FLayout);
  FLayout := o;
  FreeAndNil(FData);
  FData := TTouchLayoutData.Create(nil);
  FData.Read(FLayout);

  Result := Save(AFormatted);
end;

{ TTouchLayoutObject }

function TTouchLayoutObject.GetValue(const name: string; var value: TJSONValue): Boolean;
var
  p: TJSONPair;
begin
  value := nil;
  p := FJSON.Get(name);
  if not Assigned(p) then Exit(False);
  value := p.JsonValue;
  Result := True;
end;

function TTouchLayoutObject.GetValue(const name: string; var value: string): Boolean;
var
  v: TJSONValue;
begin
  value := '';
  if not GetValue(name, v) then Exit(False);
  value := v.Value;
  Result := True;
end;

procedure TTouchLayoutObject.AddJSONValue(JSON: TJSONObject; const name: string; value: Boolean);
begin
  JSON.AddPair(name, TJSONBool.Create(value));
end;

constructor TTouchLayoutObject.Create(AParent: TTouchLayoutObject);
begin
  inherited Create;
  FParent := AParent;
end;

function TTouchLayoutObject.GetValue(const name: string; var value: Integer): Boolean;
var
  v: string;
begin
  value := 0;
  Result := GetValue(name, v) and TryStrToInt(v, value);
end;

procedure TTouchLayoutObject.Read(JSON: TJSONObject);
begin
  FJSON := JSON;
  DoRead;
end;

procedure TTouchLayoutObject.ReadArray(const name: string;
  cls: TTouchLayoutClass; add: TTouchLayoutAddProc);
var
  a: TJSONArray;
  i: Integer;
  ask: TTouchLayoutObject;
  v: TJSONValue;
begin
  if GetValue(name, v) and (v is TJSONArray) then
  begin
    a := v as TJSONArray;
    for i := 0 to a.Count - 1 do
    begin
      ask := cls.Create(Self);
      ask.Read(a.Items[i] as TJSONObject);
      add(ask);
    end;
  end;
end;

procedure TTouchLayoutObject.Write(JSON: TJSONObject);
begin
  DoWrite(JSON);
end;

function TTouchLayoutObject.GetValue(const name: string;
  var value: Boolean): Boolean;
var
  p: TJSONPair;
begin
  value := False;
  p := FJSON.Get(name);
  if not Assigned(p) then Exit(False);
  Result := p.JsonValue.TryGetValue<Boolean>(value);
end;

{ TTouchLayoutSubKey }

procedure TTouchLayoutSubKey.DoRead;
begin
  GetValue('nextlayer', FNextLayer);
  GetValue('fontsize', FFontSize);
  GetValue('layer', FLayer);
  GetValue('width', FWidth);
  GetValue('dk', FDk);
  GetValue('font', FFont);
  GetValue('id', FId);
  GetValue('pad', FPad);
  GetValue('sp', FSp);
  GetValue('text', FText);
  GetValue('default', FDefault);
end;

procedure TTouchLayoutObject.AddJSONValue(JSON: TJSONObject; const name, value: string);
begin
  if value <> '' then
    JSON.AddPair(name, value);
end;

procedure TTouchLayoutObject.AddJSONValue(JSON: TJSONObject; const name: string; value: Integer);
begin
  if value <> 0 then
    JSON.AddPair(name, TJSONNumber.Create(value));
end;

procedure TTouchLayoutSubKey.DoWrite(JSON: TJSONObject);
begin
  AddJSONValue(JSON, 'nextlayer', FNextLayer);
  AddJSONValue(JSON, 'fontsize', FFontSize);
  AddJSONValue(JSON, 'layer', FLayer);
  if FWidth <> 0 then AddJSONValue(JSON, 'width', IntToStr(FWidth));
  AddJSONValue(JSON, 'dk', FDk);
  AddJSONValue(JSON, 'font', FFont);
  AddJSONValue(JSON, 'id', FId);
  if FPad <> 0 then AddJSONValue(JSON, 'pad', IntToStr(FPad));
  if FSp <> 0 then AddJSONValue(JSON, 'sp', IntToStr(FSp));
  AddJSONValue(JSON, 'text', FText);
  if FDefault then AddJSONValue(JSON, 'default', FDefault);
end;

function TTouchLayoutSubKey.GetSpT: TTouchKeyType;   // I4119
begin
  Result := TTouchKeyType(FSp);
end;

procedure TTouchLayoutSubKey.SetSpT(const Value: TTouchKeyType);   // I4119
begin
  FSp := Ord(Value);
end;

{ TTouchLayoutPlatform }

constructor TTouchLayoutPlatform.Create(AParent: TTouchLayoutObject);
begin
  inherited Create(AParent);
  FLayers := TTouchLayoutLayers.Create;
end;

destructor TTouchLayoutPlatform.Destroy;
begin
  FreeAndNil(FLayers);
  inherited Destroy;
end;

procedure TTouchLayoutPlatform.DoRead;
var
  s: string;
begin
  GetValue('font', FFont);
  GetValue('fontsize', FFontSize);
  GetValue('displayUnderlying', FDisplayUnderlying);
  if GetValue('defaultHint', s)
    then FDefaultHint := DefaultHintFromString(s)
    else FDefaultHint := tldhNone;
  ReadArray('layer', TTouchLayoutLayer,
    procedure (obj: TTouchLayoutObject) begin FLayers.Add(obj as TTouchLayoutLayer); end);
end;

procedure TTouchLayoutPlatform.DoWrite(JSON: TJSONObject);
var
  v: TJSONArray;
  o: TJSONObject;
  i: Integer;
begin
  AddJSONValue(JSON, 'font', FFont);
  AddJSONValue(JSON, 'fontsize', FFontSize);
  AddJSONValue(JSON, 'displayUnderlying', FDisplayUnderlying);
  if FDefaultHint <> tldhNone then
    AddJSONValue(JSON, 'defaultHint', STouchLayoutDefaultHintName[FDefaultHint]);

  if FLayers.Count > 0 then
  begin
    v := TJSONArray.Create;
    for i := 0 to FLayers.Count - 1 do
    begin
      o := TJSONObject.Create;
      FLayers[i].Write(o);
      v.Add(o);
    end;
    JSON.AddPair('layer', v);
  end;
end;

{ TTouchLayoutData }

constructor TTouchLayoutData.Create(AParent: TTouchLayoutObject);
begin
  inherited Create(AParent);
  FPlatforms := TTouchLayoutPlatforms.Create;
end;

destructor TTouchLayoutData.Destroy;
begin
  FreeAndNil(FPlatforms);
  inherited Destroy;
end;

procedure TTouchLayoutData.DoRead;
  procedure ReadPlatform(name: string);
  var
    v: TJSONValue;
    p: TTouchLayoutPlatform;
  begin
    if GetValue(name, v) and (v is TJSONObject) then
    begin
      p := TTouchLayoutPlatform.Create(Self);
      p.FName := name;
      p.Read(v as TJSONObject);
      FPlatforms.Add(p);
    end;
  end;
begin
  ReadPlatform('phone');
  ReadPlatform('tablet');
  ReadPlatform('desktop');
end;

procedure TTouchLayoutData.DoWrite(JSON: TJSONObject);
var
  o: TJSONObject;
  i: Integer;
begin
  for i := 0 to FPlatforms.Count - 1 do
  begin
    o := TJSONObject.Create;
    FPlatforms[i].Write(o);
    JSON.AddPair(FPlatforms[i].Name, o);
  end;
end;

{ TTouchLayoutKey }

constructor TTouchLayoutKey.Create(AParent: TTouchLayoutObject);
begin
  inherited Create(AParent);
  FSk := TTouchLayoutSubKeys.Create;
  FFlick := TTouchLayoutFlicks.Create;
  FMultiTap := TTouchLayoutMultiTaps.Create;
end;

destructor TTouchLayoutKey.Destroy;
begin
  FreeAndNil(FSk);
  FreeAndNil(FFlick);
  FreeAndNil(FMultiTap);
  inherited Destroy;
end;

procedure TTouchLayoutKey.DoRead;
var
  v: TJSONValue;
  o: TJSONObject;
  d: TTouchLayoutFlickDirection;
  sk: TTouchLayoutSubKey;
begin
  GetValue('nextlayer', FNextLayer);
  GetValue('fontsize', FFontSize);
  GetValue('layer', FLayer);
  GetValue('width', FWidth);
  GetValue('dk', FDk);
  GetValue('font', FFont);
  GetValue('id', FId);
  GetValue('pad', FPad);
  GetValue('sp', FSp);
  GetValue('text', FText);
  GetValue('hint', FHint);
  ReadArray('sk', TTouchLayoutSubKey,
    procedure (obj: TTouchLayoutObject) begin FSk.Add(obj as TTouchLayoutSubKey); end);
  if GetValue('flick', v) and (v is TJSONObject) then
  begin
    o := v as TJSONObject;
    for d := Low(TTouchLayoutFlickDirection) to High(TTouchLayoutFlickDirection) do
    begin
      v := o.GetValue(STouchLayoutFlickDirectionName[d]);
      if Assigned(v) and (v is TJSONObject) then
      begin
        sk := TTouchLayoutSubKey.Create(Self);
        sk.Read(v as TJSONObject);
        FFlick.Add(d, sk);
      end;
    end;
  end;
  ReadArray('multitap', TTouchLayoutSubKey,
    procedure (obj: TTouchLayoutObject) begin FMultiTap.Add(obj as TTouchLayoutSubKey); end);
end;

procedure TTouchLayoutKey.DoWrite(JSON: TJSONObject);
var
  i: Integer;
  v: TJSONArray;
  o, f: TJSONObject;
  d: TTouchLayoutFlickDirection;
begin
  AddJSONValue(JSON, 'nextlayer', FNextLayer);
  AddJSONValue(JSON, 'fontsize', FFontSize);
  AddJSONValue(JSON, 'layer', FLayer);
  if FWidth <> 0 then AddJSONValue(JSON, 'width', IntToStr(FWidth));
  AddJSONValue(JSON, 'dk', FDk);
  AddJSONValue(JSON, 'font', FFont);
  AddJSONValue(JSON, 'id', FId);
  if FPad <> 0 then AddJSONValue(JSON, 'pad', IntToStr(FPad));
  if FSp <> 0 then AddJSONValue(JSON, 'sp', IntToStr(FSp));
  AddJSONValue(JSON, 'text', FText);
  AddJSONValue(JSON, 'hint', FHint);

  if FSk.Count > 0 then
  begin
    v := TJSONArray.Create;
    for i := 0 to FSk.Count - 1 do
    begin
      o := TJSONObject.Create;
      FSk[i].Write(o);
      v.Add(o);
    end;
    JSON.AddPair('sk', v);
  end;

  if FFlick.Count > 0 then
  begin
    f := TJSONObject.Create;
    for d in FFlick.Keys do
    begin
      o := TJSONObject.Create;
      FFlick[d].Write(o);
      f.AddPair(STouchLayoutFlickDirectionName[d], o);
    end;
    JSON.AddPair('flick', f);
  end;

  if FMultiTap.Count > 0 then
  begin
    v := TJSONArray.Create;
    for i := 0 to FMultiTap.Count - 1 do
    begin
      o := TJSONObject.Create;
      FMultiTap[i].Write(o);
      v.Add(o);
    end;
    JSON.AddPair('multitap', v);
  end;

end;

function TTouchLayoutKey.GetSpT: TTouchKeyType;   // I4119
begin
  Result := TTouchKeyType(FSp);
end;

procedure TTouchLayoutKey.SetSpT(const Value: TTouchKeyType);   // I4119
begin
  FSp := Ord(Value);
end;

{ TTouchLayoutRow }

constructor TTouchLayoutRow.Create(AParent: TTouchLayoutObject);
begin
  inherited Create(AParent);
  FKeys := TTouchLayoutKeys.Create;
end;

destructor TTouchLayoutRow.Destroy;
begin
  FreeAndNil(FKeys);
  inherited Destroy;
end;

procedure TTouchLayoutRow.DoRead;
begin
  GetValue('id', FId);
  ReadArray('key', TTouchLayoutKey,
    procedure (obj: TTouchLayoutObject) begin FKeys.Add(obj as TTouchLayoutKey); end);
end;

procedure TTouchLayoutRow.DoWrite(JSON: TJSONObject);
var
  v: TJSONArray;
  o: TJSONObject;
  i: Integer;
begin
  AddJSONValue(JSON, 'id', FId);

  if FKeys.Count > 0 then
  begin
    v := TJSONArray.Create;
    for i := 0 to FKeys.Count - 1 do
    begin
      o := TJSONObject.Create;
      FKeys[i].Write(o);
      v.Add(o);
    end;
    JSON.AddPair('key', v);
  end;
end;

function TTouchLayoutRow.FindKeyById(Id: string): TTouchLayoutKey;
var
  k: TTouchLayoutKey;
begin
  for k in Keys do
  begin
    if SameText(k.Id, Id) then
      Exit(k);
  end;

  Result := nil;
end;

{ TTouchLayoutLayer }

constructor TTouchLayoutLayer.Create(AParent: TTouchLayoutObject);
begin
  inherited Create(AParent);
  FRows := TTouchLayoutRows.Create;
end;

destructor TTouchLayoutLayer.Destroy;
begin
  FreeAndNil(FRows);
  inherited Destroy;
end;

procedure TTouchLayoutLayer.DoRead;
begin
  GetValue('id', FId);
  ReadArray('row', TTouchLayoutRow,
    procedure (obj: TTouchLayoutObject) begin FRows.Add(obj as TTouchLayoutRow); end);
end;

procedure TTouchLayoutLayer.DoWrite(JSON: TJSONObject);
var
  v: TJSONArray;
  o: TJSONObject;
  i: Integer;
begin
  AddJSONValue(JSON, 'id', FId);

  if FRows.Count > 0 then
  begin
    v := TJSONArray.Create;
    for i := 0 to FRows.Count - 1 do
    begin
      o := TJSONObject.Create;
      FRows[i].Write(o);
      v.Add(o);
    end;
    JSON.AddPair('row', v);
  end;
end;

function TTouchLayoutLayer.FindKeyById(Id: string): TTouchLayoutKey;
var
  r: TTouchLayoutRow;
begin
  for r in Rows do
  begin
    Result := r.FindKeyById(Id);
    if Assigned(Result) then Exit;
  end;
  Result := nil;
end;

{ TTouchLayoutLayers }

function TTouchLayoutLayers.IndexOfId(const AId: string): Integer;
var
  i: Integer;
begin
  for i := 0 to Count - 1 do
    if Items[i].Id = AId then
      Exit(i);
  Result := -1;
end;

function DefaultHintFromString(const s: string): TTouchLayoutDefaultHint;
begin
  for Result := Low(TTouchLayoutDefaultHint) to High(TTouchLayoutDefaultHint) do
    if s = STouchLayoutDefaultHintName[Result] then Exit;
  Result := tldhNone;
end;

end.
