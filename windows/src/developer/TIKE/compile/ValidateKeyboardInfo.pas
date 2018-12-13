unit ValidateKeyboardInfo;

interface

uses
  System.JSON,
  compile;

type
  TValidateKeyboardInfo = class
  private
    FJsonFile: string;
    FSilent: Boolean;
    json: TJSONObject;
    function DoFieldValidation: Boolean;
    function LoadJsonFile: Boolean;
    function SaveJsonFile: Boolean;
    constructor Create(AJsonFile: string; ASilent: Boolean);
    function MigrateLanguagesArray(alangs: TJSONArray; 
      var olangs: TJSONObject): Boolean;
    function Failed(message: string): Boolean;
  public
    class function Execute(JsonFile, JsonSchemaPath: string; FDistribution, FSilent: Boolean; FCallback: TCompilerCallback): Boolean;
  end;

implementation

uses
  System.Classes,
  System.SysUtils,
  Winapi.Windows,

  BCP47Tag,
  JsonUtil,
  Keyman.System.KeyboardInfoFile,
  Keyman.System.KMXFileLanguages;

{ TValidateKeyboardInfo }

const
  SKeyboardInfoSourceSchemaJson = 'keyboard_info.source.json';
  SKeyboardInfoDistSchemaJson = 'keyboard_info.distribution.json';

var
  GCallback: TCompilerCallback = nil;

type TValidateJsonMessageProc = function (offset: Int64; message: PAnsiChar): BOOL; stdcall;

function ValidateJsonFile(pwszSchemaFile, pwszJsonFile: PWideChar; MessageProc: TValidateJsonMessageProc): BOOL;
  stdcall; external 'kmcmpdll.dll';

function ValidateMessageProc(offset: Int64; message: PAnsiChar): BOOL; stdcall;
begin
  GCallback(-1, 0, message);
  Result := TRUE;
end;

constructor TValidateKeyboardInfo.Create(AJsonFile: string; ASilent: Boolean);
begin
  inherited Create;
  FJsonFile := AJsonFile;
  FSilent := ASilent;
end;

function TValidateKeyboardInfo.MigrateLanguagesArray(alangs: TJSONArray; 
  var olangs: TJSONObject): Boolean;
var
  i: Integer;
  msg: string;
begin
  Result := True;
  for i := 0 to alangs.Count - 1 do
    with TBCP47Tag.Create(alangs.Items[i].Value) do
    try
      if not IsValid(False, msg) then
        Result := Failed(msg);

      if not IsCanonical(msg) then
        Result := Failed(msg);
      olangs.AddPair(alangs.Items[i].Value, TJSONString.Create('something'));
    finally
      Free;
    end;
end;

function TValidateKeyboardInfo.DoFieldValidation: Boolean;
var
  alangs: TJSONArray;
  langs, lang: TJSONValue;
  i: Integer;
  olangs, olang: TJSONObject;
  id, msg: string;
  languagesMigrated, nameAdded: Boolean;
begin
  if not LoadJsonFile then
    Exit(False);

  // Test that language ids are valid and canonical
  langs := json.Values[TKeyboardInfoFile.SLanguages];
  if not Assigned(langs) then
    Exit(True);

  Result := True;
  languagesMigrated := False;
  nameAdded := False;

  // Migrate languages[] array to Object
  if langs is TJSONArray then
  begin
    alangs := langs as TJSONArray;
    olangs := TJSONObject.Create;
    Result := MigrateLanguagesArray(alangs, olangs);
    json.RemovePair(TKeyboardInfoFile.SLanguages);
    json.AddPair(TKeyboardInfoFile.SLanguages, olangs);
    langs := json.Values[TKeyboardInfoFile.SLanguages];
    languagesMigrated := True;
  end;

  olangs := langs as TJSONObject;
  for i := 0 to olangs.Count - 1 do
  begin
    id := olangs.Pairs[i].JsonString.Value;
    with TBCP47Tag.Create(id) do
    try
      if not IsValid(False, msg) then
        Result := Failed(msg);

      if not IsCanonical(msg) then
        Result := Failed(msg);

      // Validate subtag names
      lang := olangs.Values[id];
      if lang is TJSONObject then
      begin
        olang := lang as TJsonObject;
        if (olang <> nil) and (olang.GetValue(TKeyboardInfoFile.SDisplayName) = nil) then
        begin
          olang.AddPair(TKeyboardInfoFile.SDisplayName, TJSONString.Create('displayName1'));
          nameAdded := True;
        end;

        if (olang <> nil) and (olang.GetValue(TKeyboardInfoFile.SLanguageName) = nil) then
        begin
          olang.AddPair(TKeyboardInfoFile.SLanguageName, TJSONString.Create('languageName1'));
          nameAdded := True;
        end;

        if nameAdded then
        begin
          olangs.RemovePair(olangs.Pairs[i].JsonString.Value);
          olangs.AddPair(olangs.Pairs[i].JsonString.Value, olang);
        end;

      end;
    finally
      Free;
    end;

  end;

  if languagesMigrated or nameAdded then
  begin
    // Save and reload
    if not SaveJsonFile then
      Exit(Failed('Could not save updated keyboard_info file '+FJsonFile));

    if not LoadJsonFile then
      Exit(Failed('Cound not reopen updated keyboard info file'+FJsonFile));
  end;
end;

function TValidateKeyboardInfo.Failed(message: string): Boolean;
begin
  writeln(message);
  Result := False;
end;

function TValidateKeyboardInfo.LoadJsonFile: Boolean;
begin
  try
    with TStringStream.Create('', TEncoding.UTF8) do
    try
      LoadFromFile(FJsonFile);
      json := TJSONObject.ParseJsonValue(DataString) as TJSONObject;
    finally
      Free;
    end;
  except
    on E:Exception do
      Exit(Failed(E.Message));
  end;

  Result := Assigned(json);
end;

function TValidateKeyboardInfo.SaveJsonFile: Boolean;
var
  str: TStringList;
begin
  str := TStringList.Create;
  try
    PrettyPrintJSON(json, str);
    with TStringStream.Create(str.Text, TEncoding.UTF8) do
    try
      // Use TStringStream so we don't get a default BOM prolog
      SaveToFile(FJsonFile);
    finally
      Free;
    end;
  finally
    str.Free;
  end;
  Result := True;
end;

class function TValidateKeyboardInfo.Execute(JsonFile, JsonSchemaPath: string; FDistribution, FSilent: Boolean; FCallback: TCompilerCallback): Boolean;
var
  SchemaFile: string;
  t: TValidateKeyboardInfo;
begin
  GCallback := FCallback;
  if FDistribution
    then SchemaFile := JsonSchemaPath + SKeyboardInfoDistSchemaJson
    else SchemaFile := JsonSchemaPath + SKeyboardInfoSourceSchemaJson;
  Result := ValidateJsonFile(PWideChar(SchemaFile), PWideChar(JsonFile), ValidateMessageProc);

  if Result then
  begin
    t := TValidateKeyboardInfo.Create(JsonFile, FSilent);
    try
      Result := t.DoFieldValidation;
    finally
      t.Free;
    end;
  end;
  if not FSilent then
  begin
    if Result
      then writeln('File '+JsonFile+' validated successfully.')
      else writeln('File '+JsonFile+' had errors.');
  end;
end;

end.
