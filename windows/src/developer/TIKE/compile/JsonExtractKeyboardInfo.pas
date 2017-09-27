unit JsonExtractKeyboardInfo;

interface

uses
  compile;

type
  TJsonExtractKeyboardInfo = class
    class function Execute(JsonFile, Fields: string; FSilent: Boolean; FCallback: TCompilerCallback): Boolean;
  end;

implementation

uses
  System.Classes,
  System.Json,
  System.StrUtils,
  System.SysUtils,
  utilstr;

{ TJsonExtractKeyboardInfo }

class function TJsonExtractKeyboardInfo.Execute(JsonFile, Fields: string;
  FSilent: Boolean; FCallback: TCompilerCallback): Boolean;
var
  json: TJSONObject;
  s: string;
  v: TJSONValue;
begin
  try
    with TStringStream.Create('', TEncoding.UTF8) do
    try
      LoadFromFile(JsonFile);
      json := TJSONObject.ParseJsonValue(DataString) as TJSONObject;
      if not Assigned(json) then
      begin
        FCallback(-1, 0, 'Could not parse keyboard_info');
        Exit(False);
      end;

      try
        while Fields <> '' do
        begin
          s := StrToken(Fields,',');
          v := json.GetValue(s);
          if Assigned(v) then
          begin
            writeln(s+'="'+ReplaceStr(v.Value, '"', '\"')+'"');
          end;
        end;
      finally
        json.Free;
      end;
    finally
      Free;
    end;
  except
    on E:Exception do
    begin
      FCallback(-1, 0, PAnsiChar(AnsiString(E.Message)));
      Exit(False);
    end;
  end;
  Result := True;
end;

end.
