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
    constructor Create(AJsonFile: string; ASilent: Boolean);
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

function TValidateKeyboardInfo.DoFieldValidation: Boolean;
var
  alangs: TJSONArray;
  langs: TJSONValue;
  i: Integer;
  olangs: TJSONObject;
  msg: string;
begin
  if not LoadJsonFile then
    Exit(False);

  // Test that language ids are valid and canonical
  langs := json.Values[TKeyboardInfoFile.SLanguages];
  if not Assigned(langs) then
    Exit(True);

  Result := True;

  if langs is TJSONArray then
  begin
    alangs := langs as TJSONArray;
    for i := 0 to alangs.Count - 1 do
      with TBCP47Tag.Create(alangs.Items[i].Value) do
      try
        if not IsValid(msg) then
          Result := Failed(msg);

        if not IsCanonical(msg) then
          Result := Failed(msg);
      finally
        Free;
      end;
  end
  else
  begin
    olangs := langs as TJSONObject;
    for i := 0 to olangs.Count - 1 do
      with TBCP47Tag.Create(olangs.Pairs[i].JsonString.Value) do
      try
        if not IsValid(msg) then
          Result := Failed(msg);

        if not IsCanonical(msg) then
          Result := Failed(msg);
      finally
        Free;
      end;
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
