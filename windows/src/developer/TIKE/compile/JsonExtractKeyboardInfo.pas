unit JsonExtractKeyboardInfo;

interface

uses
  Keyman.Developer.System.Project.ProjectLog;

type
  TJsonExtractKeyboardInfo = class
    class function Execute(JsonFile, Fields: string; FSilent: Boolean; FCallback: TProjectLogObjectEvent): Boolean;
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
  FSilent: Boolean; FCallback: TProjectLogObjectEvent): Boolean;
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
        FCallback(plsError, JsonFile, 'Could not parse keyboard_info', 0, 0);
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
      FCallback(plsError, JsonFile, E.Message, 0, 0);
      Exit(False);
    end;
  end;
  Result := True;
end;

end.
