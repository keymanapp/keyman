(*
  Name:             JSONKeyboardInfo
  Copyright:        Copyright (C) SIL International.
  Documentation:
  Description:
  Create Date:      3 Feb 2015

  Modified Date:    24 Aug 2015
  Authors:          mcdurdin
  Related Files:
  Dependencies:

  Bugs:
  Todo:
  Notes:
  History:          24 Aug 2015 - mcdurdin - I4877 - JSON format is inconsistent with documentation

                    24 Aug 2015 - mcdurdin - I4875 - RTL flag is not mapped in the JSON editor

*)
unit JSONKeyboardInfo;

interface

uses
  System.Generics.Collections,
  System.JSON;

type
  TJSONKeyboardInfo = class;

  TJSONKeyboardFont = class
  private
    FFileName: string;
    FFamily: string;
    FSize: string;
  public
    property Family: string read FFamily write FFamily;
    property Size: string read FSize write FSize; //usually ems
    property FileName: string read FFileName write FFileName;
  end;

  TJSONKeyboardFonts = class(TObjectList<TJSONKeyboardFont>)
  private
    FParent: TJSONKeyboardInfo;
  public
    constructor Create(AParent: TJSONKeyboardInfo);
    function Read(jsonPair: TJSONPair): Boolean;
    procedure Write(jsonObject: TJSONObject);   // I4877
  end;

  TJSONKeyboardLanguage = class
  private
    FName: string;
    FID: string;
  public
    property ID: string read FID write FID;
    property Name: string read FName write FName;
  end;

  TJSONKeyboardLanguages = class(TObjectList<TJSONKeyboardLanguage>)
  private
    FParent: TJSONKeyboardInfo;
  public
    constructor Create(AParent: TJSONKeyboardInfo);
    function Read(jsonPair: TJSONPair): Boolean;
    procedure Write(jsonArray: TJSONArray);
  end;

  TJSONKeyboardInfo = class
  private
    FJSONText: string;
    FLastModified: TDateTime;
    FFonts: TJSONKeyboardFonts;
    FKeyboardBaseUri: string;
    FKeyboardID: string;
    FOskFonts: TJSONKeyboardFonts;
    FReadError: string;
    FLanguages: TJSONKeyboardLanguages;
    FKeyboardVersion: string;
    FKeyboardFilename: string;
    FKeyboardName: string;
    FFontBaseUri: string;
    FDevice: string;
    FReadErrorOffset: Integer;
    FRTL: Boolean;   // I4875
    function GetDateValue(o: TJSONObject; const name: string): TDateTime;
    function GetBooleanValue(o: TJSONObject; const name: string): Boolean;   // I4875
    function GetValue(o: TJSONObject; const name: string): string;
    function DoRead(var json, jsonOptions, jsonKeyboard: TJSONObject): Boolean;
    procedure SetValue(o: TJSONObject; const name, value: string);
    procedure SetBooleanValue(o: TJSONObject; const name: string; value: Boolean);   // I4875
  public
    constructor Create;
    destructor Destroy; override;
    function Read(const JSONText: string): Boolean;
    function Write(Formatted: Boolean = True): string;

    property ReadError: string read FReadError;
    property ReadErrorOffset: Integer read FReadErrorOffset;

    property Device: string read FDevice write FDevice;
    property KeyboardBaseUri: string read FKeyboardBaseUri write FKeyboardBaseUri;
    property FontBaseUri: string read FFontBaseUri write FFontBaseUri;
    property KeyboardID: string read FKeyboardID write FKeyboardID;       //keyboard
    property KeyboardName: string read FKeyboardName write FKeyboardName;     //my cool keyboard
    property KeyboardFilename: string read FKeyboardFilename write FKeyboardFilename; //keyboard-x.y.js
    property Fonts: TJSONKeyboardFonts read FFonts;
    property OSKFonts: TJSONKeyboardFonts read FOskFonts;
    property KeyboardVersion: string read FKeyboardVersion write FKeyboardVersion;  //x.y
    property LastModified: TDateTime read FLastModified write FLastModified;
    property Languages: TJSONKeyboardLanguages read FLanguages;
    property RTL: Boolean read FRTL write FRTL;   // I4875
  end;

implementation

uses
  System.Classes,
  System.SysUtils,

  JSONUtil;

{ TJSONKeyboardInfo }

constructor TJSONKeyboardInfo.Create;
begin
  inherited Create;
  FFonts := TJSONKeyboardFonts.Create(Self);
  FOskFonts := TJSONKeyboardFonts.Create(Self);
  FLanguages := TJSONKeyboardLanguages.Create(Self);
end;

destructor TJSONKeyboardInfo.Destroy;
begin
  FreeAndNil(FFonts);
  FreeAndNil(FOskFonts);
  FreeAndNil(FLanguages);
  inherited Destroy;
end;

function TJSONKeyboardInfo.GetValue(o: TJSONObject; const name: string): string;
var
  jsonPair: TJSONPair;
begin
  jsonPair := o.Get(name);
  if Assigned(jsonPair) and (jsonPair.JsonValue is TJSONString)
    then Result := jsonPair.JsonValue.Value
    else Result := '';
end;

procedure TJSONKeyboardInfo.SetBooleanValue(o: TJSONObject; const name: string;   // I4875
  value: Boolean);
begin
  o.RemovePair(name).Free;
  if Value then o.AddPair(name, TJSONTrue.Create);
end;

procedure TJSONKeyboardInfo.SetValue(o: TJSONObject; const name, value: string);
begin
  o.RemovePair(name).Free;
  o.AddPair(name, value);
end;

function TJSONKeyboardInfo.GetBooleanValue(o: TJSONObject;   // I4875
  const name: string): Boolean;
var
  jsonPair: TJSONPair;
begin
  jsonPair := o.Get(name);
  if Assigned(jsonPair) and (jsonPair.JsonValue is TJSONTrue)
    then Result := True
    else Result := False;
end;

function TJSONKeyboardInfo.GetDateValue(o: TJSONObject; const name: string): TDateTime;
var
  s: string;
begin
  s := GetValue(o, name);
  if s = '' then
    Exit(0);
  JSONDateToDateTime(s, Result);
end;

function TJSONKeyboardInfo.DoRead(var json, jsonOptions, jsonKeyboard: TJSONObject): Boolean;
var
  Offset: Integer;
  jsonOptionsPair, jsonKeyboardPair: TJSONPair;
begin
  Offset := 0;
  FReadErrorOffset := -1;
  try
    json := ParseJSONValue(FJSONText, Offset) as TJSONObject;
    if not Assigned(json) then
    begin
      FReadErrorOffset := Offset;
      FReadError := 'JSON file is invalid at '+IntToStr(Offset)+': No error message given';
      Exit(False);
    end;
  except
    on E:EJSONException do
    begin
      FReadErrorOffset := Offset;
      FReadError := 'Error parsing JSON file at '+IntToStr(Offset)+': '+E.Message;
      Exit(False);
    end;
  end;


  jsonOptionsPair := json.Get('options');
  if jsonOptionsPair = nil then
  begin
    jsonOptionsPair := TJSONPair.Create('options', TJSONObject.Create);
    json.AddPair(jsonOptionsPair);
  end;

  jsonKeyboardPair := json.Get('keyboard');
  if (jsonKeyboardPair = nil) then
  begin
    jsonKeyboardPair := TJSONPair.Create('keyboard', TJSONObject.Create);
    json.AddPair(jsonKeyboardPair);
  end;

  if not (jsonOptionsPair.JsonValue is TJSONObject) or
     not (jsonKeyboardPair.JsonValue is TJSONObject) then
  begin
    FReadError := 'options and keyboard members must be objects.';
    json.Free;
    Exit(False);
  end;

  jsonOptions := jsonOptionsPair.JsonValue as TJSONObject;
  jsonKeyboard := jsonKeyboardPair.JsonValue as TJSONObject;

  Result := True;
end;

function TJSONKeyboardInfo.Read(const JSONText: string): Boolean;
var
  json: TJSONObject;
  jsonOptions, jsonKeyboard: TJSONObject;
begin
  FJSONText := Trim(JSONText);
  if not DoRead(json, jsonOptions, jsonKeyboard) then
    Exit(False);

  try
    FDevice := GetValue(jsonOptions, 'device');
    FKeyboardBaseUri := GetValue(jsonOptions, 'keyboardBaseUri');
    FFontBaseUri := GetValue(jsonOptions, 'fontBaseUri');

    if not FFonts.Read(jsonKeyboard.Get('font')) then
      Exit(False);

    if not FOskFonts.Read(jsonKeyboard.Get('oskFont')) then

    if FOskFonts.Count = 0 then // Fix for broken oskFont key   // I4877
      if not FOskFonts.Read(jsonKeyboard.Get('oskfont')) then   // I4877
        Exit(False);

    if not FLanguages.Read(jsonKeyboard.Get('languages')) then
      Exit(False);

    FKeyboardID := GetValue(jsonKeyboard, 'id');
    FKeyboardName := GetValue(jsonKeyboard, 'name');
    FKeyboardFilename := GetValue(jsonKeyboard, 'filename');
    FKeyboardVersion := GetValue(jsonKeyboard, 'version');
    FLastModified := GetDateValue(jsonKeyboard, 'lastModified');
    FRTL := GetBooleanValue(jsonKeyboard, 'rtl');   // I4875
  finally
    json.Free;
  end;

  Result := True;
end;

function TJSONKeyboardInfo.Write(Formatted: Boolean = True): string;
var
  str: TStringList;
  json: TJSONObject;
  jsonOptions, jsonFont, jsonKeyboard: TJSONObject;
  jsonLanguagesArray: TJSONArray;
begin
  if not DoRead(json, jsonOptions, jsonKeyboard) then
    Exit(FJSONText);

  try
    SetValue(jsonOptions, 'device', FDevice);
    SetValue(jsonOptions, 'keyboardBaseUri', FKeyboardBaseUri);
    SetValue(jsonOptions, 'fontBaseUri', FFontBaseUri);

    SetValue(jsonKeyboard, 'id', FKeyboardID);
    SetValue(jsonKeyboard, 'name', FKeyboardName);
    SetValue(jsonKeyboard, 'filename', FKeyboardFilename);
    SetValue(jsonKeyboard, 'version', FKeyboardVersion);
    SetValue(jsonKeyboard, 'lastModified', DateTimeToJSONDate(FLastModified));
    SetBooleanValue(jsonKeyboard, 'rtl', FRTL);   // I4875

    jsonFont := TJSONObject.Create;   // I4877
    jsonKeyboard.RemovePair('font').Free;
    if FFonts.Count > 0 then   // I4877
    begin
      jsonKeyboard.AddPair('font', jsonFont);
      FFonts.Write(jsonFont);
    end;

    jsonFont := TJSONObject.Create;   // I4877
    jsonKeyboard.RemovePair('oskfont').Free;   // I4877
    jsonKeyboard.RemovePair('oskFont').Free;   // I4877
    if FOskFonts.Count > 0 then   // I4877
    begin
      jsonKeyboard.AddPair('oskFont', jsonFont);
      FOskFonts.Write(jsonFont);
    end;

    jsonLanguagesArray := TJSONArray.Create;
    jsonKeyboard.RemovePair('languages').Free;
    jsonKeyboard.AddPair('languages', jsonLanguagesArray);
    FLanguages.Write(jsonLanguagesArray);

    if Formatted then
    begin
      str := TStringList.Create;
      try
        PrettyPrintJSON(json, str);
        Result := str.Text;
      finally
        str.Free;
      end;
    end
    else
      Result := JSONToString(json, True);
  finally
    json.Free;
  end;
end;

{ TJSONKeyboardFonts }

constructor TJSONKeyboardFonts.Create(AParent: TJSONKeyboardInfo);
begin
  inherited Create;
  FParent := AParent;
end;

function TJSONKeyboardFonts.Read(jsonPair: TJSONPair): Boolean;   // I4877
  function AddItem(v: TJSONValue; AllowArray: Boolean): Boolean;
  var
    a: TJSONArray;
    i: Integer;
    o: TJSONObject;
    p: TJSONPair;
    font: TJSONKeyboardFont;
  begin
    if not (v is TJSONObject) then
    begin
      FParent.FReadError := 'Font item must be an object';
      Exit(False);
    end;

    o := v as TJSONObject;

    p := o.Get('source');
    if not Assigned(p) then p := o.Get('filename');
    if not Assigned(p) then
    begin
      FParent.FReadError := 'Font item must contain at least one filename';
      Exit(False);
    end;


    if p.JsonValue is TJSONArray then
    begin
      // Corrected JSON
      if not AllowArray then
        Exit(False);

      a := p.JsonValue as TJSONArray;
      for i := 0 to a.Count - 1 do
      begin
        font := TJSONKeyboardFont.Create;
        font.Family := FParent.GetValue(o, 'family');
        font.Size := FParent.GetValue(o, 'size');
        if not (a.Items[i] is TJSONString) then
        begin
          FParent.FReadError := 'Font source/filename must be a string or array of strings';
          Exit(False);
        end;
        font.FileName := a.Items[i].Value;
        Add(font);
      end;
    end
    else if p.JsonValue is TJSONString then
    begin
      // Translated from buggy JSON, read only
      font := TJSONKeyboardFont.Create;
      font.Family := FParent.GetValue(o, 'family');
      font.Size := FParent.GetValue(o, 'size');
      font.FileName := p.JSONValue.Value;
      Add(font);
    end
    else
    begin
      FParent.FReadError := 'Font source/filename must be a string or array of strings';
      Exit(False);
    end;

    Result := True;
  end;
var
  a: TJSONArray;
  i: Integer;
begin
  Clear;
  if not Assigned(jsonPair) then Exit(True);

  if jsonPair.JsonValue is TJSONArray then
  begin
    a := jsonPair.JsonValue as TJSONArray;
    for i := 0 to a.Count - 1 do
      if not AddItem(a.Items[i], False) then
        Exit(False);

    Result := True;
  end
  else
    Result := AddItem(jsonPair.JsonValue, True);
end;

procedure TJSONKeyboardFonts.Write(jsonObject: TJSONObject);   // I4877
var
  i: Integer;
  a: TJSONArray;
begin
  if Count = 0 then Exit;

  FParent.SetValue(jsonObject, 'family', Items[0].Family);
  if Items[0].Size <> '' then FParent.SetValue(jsonObject, 'size', Items[0].Size);
  if Count = 1 then
  begin
    FParent.SetValue(jsonObject, 'source', Items[0].FileName);
  end
  else
  begin
    a := TJSONArray.Create;
    for i := 0 to Count - 1 do
      a.AddElement(TJSONString.Create(Items[i].FileName));
    jsonObject.AddPair(TJSONPair.Create('source', a));
  end;
end;

{ TJSONKeyboardLanguages }

constructor TJSONKeyboardLanguages.Create(AParent: TJSONKeyboardInfo);
begin
  inherited Create;
  FParent := AParent;
end;

function TJSONKeyboardLanguages.Read(jsonPair: TJSONPair): Boolean;
  function AddItem(v: TJSONValue): Boolean;
  var
    o: TJSONObject;
    language: TJSONKeyboardLanguage;
  begin
    if not (v is TJSONObject) then
    begin
      FParent.FReadError := 'Language item must be an object';
      Exit(False);
    end;

    o := v as TJSONObject;
    language := TJSONKeyboardLanguage.Create;
    language.ID := FParent.GetValue(o, 'id');
    language.Name := FParent.GetValue(o, 'name');
    Add(language);
    Result := True;
  end;
var
  a: TJSONArray;
  i: Integer;
begin
  Clear;
  if not Assigned(jsonPair) then Exit(True);

  if jsonPair.JsonValue is TJSONArray then
  begin
    a := jsonPair.JsonValue as TJSONArray;
    for i := 0 to a.Count - 1 do
      if not AddItem(a.Items[i]) then
        Exit(False);

    Result := True;
  end
  else
    Result := AddItem(jsonPair.JsonValue);
end;

procedure TJSONKeyboardLanguages.Write(jsonArray: TJSONArray);
var
  i: Integer;
  o: TJSONObject;
begin
  for i := 0 to Count - 1 do
  begin
    o := TJSONObject.Create;
    FParent.SetValue(o, 'id', Items[i].ID);
    FParent.SetValue(o, 'name', Items[i].Name);
    jsonArray.AddElement(o);
  end;
end;

end.
