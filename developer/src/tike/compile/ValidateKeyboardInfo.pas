unit ValidateKeyboardInfo;

// TODO: this unit is deprecated

interface

uses
  System.JSON,
  Keyman.Developer.System.Project.ProjectLog;

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
    procedure Warning(message: string);
    procedure Info(message: string);
    procedure Hint(message: string);
  public
    class function Execute(JsonFile, JsonSchemaPath: string; FDistribution, FSilent: Boolean; FCallback: TProjectLogObjectEvent): Boolean;
  end;

implementation

uses
  System.Classes,
  System.Generics.Collections,
  System.SysUtils,
  Winapi.Windows,

  BCP47Tag,
  compile,
//  Keyman.Developer.System.Project.ProjectLogConsole,
  Keyman.System.CanonicalLanguageCodeUtils,
  Keyman.System.KeyboardInfoFile,
  Keyman.System.KMXFileLanguages;

{ TValidateKeyboardInfo }

const
  SKeyboardInfoSourceSchemaJson = 'keyboard_info.source.json';
  SKeyboardInfoDistSchemaJson = 'keyboard_info.distribution.json';

var
  GCallback: TProjectLogObjectEvent = nil;
  GFilename: string;

type TValidateJsonMessageProc = function (offset: Int64; message: PAnsiChar): BOOL; stdcall;

// TODO: dynamically load this
function ValidateJsonFile(pwszSchemaFile, pwszJsonFile: PWideChar; MessageProc: TValidateJsonMessageProc): BOOL;
  stdcall; external kmcmpdll_lib;

function ValidateMessageProc(offset: Int64; message: PAnsiChar): BOOL; stdcall;
begin
  GCallback(plsInfo, GFilename, string(AnsiString(message)), 0, 0);
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
        if not IsValid(False, msg) then
          Result := Failed(msg);

        if not TCanonicalLanguageCodeUtils.IsCanonical(Tag, msg, False, False) then
          Warning(msg);
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
        if not IsValid(False, msg) then
          Result := Failed(msg);

        if not TCanonicalLanguageCodeUtils.IsCanonical(Tag, msg, False, False) then
          Warning(msg);
      finally
        Free;
      end;
  end;
end;

function TValidateKeyboardInfo.Failed(message: string): Boolean;
begin
  GCallback(plsError, FJsonFile, Message, 0, 0);
  Result := False;
end;

procedure TValidateKeyboardInfo.Hint(message: string);
begin
  GCallback(plsHint, FJsonFile, Message, 0, 0);
end;

procedure TValidateKeyboardInfo.Info(message: string);
begin
  GCallback(plsInfo, FJsonFile, Message, 0, 0);
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

procedure TValidateKeyboardInfo.Warning(message: string);
begin
  GCallback(plsWarning, FJsonFile, Message, 0, 0);
end;

class function TValidateKeyboardInfo.Execute(JsonFile, JsonSchemaPath: string; FDistribution, FSilent: Boolean; FCallback: TProjectLogObjectEvent): Boolean;
var
  SchemaFile: string;
  t: TValidateKeyboardInfo;
begin
  GCallback := FCallback;
  GFilename := JsonFile;

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

  if Result
    then GCallback(plsSuccess, JsonFile, 'File '+JsonFile+' validated successfully.', 0, 0)
    else GCallback(plsFailure, JsonFile, 'File '+JsonFile+' has errors.', 0, 0);
end;

end.
