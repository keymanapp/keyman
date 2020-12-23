unit Keyman.System.SettingsManagerFile;

interface

uses
  Keyman.System.Settings;

type
  TKeymanSettingsManagerFile = class
    ///  <summary>Exports non-default settings to a json settings file.</summary>
    class procedure Export(Settings: TKeymanSettings; const Filename: string); static;

    ///  <summary>Imports new settings from a json settings file. Settings
    ///  not present in the file are left unmodified.</summary>
    class procedure Import(Settings: TKeymanSettings; const Filename: string);
  end;

implementation

uses
  System.Classes,
  System.JSON,
  System.SysUtils;

{ TKeymanSettingsManager }

class procedure TKeymanSettingsManagerFile.Import(Settings: TKeymanSettings;
  const Filename: string);
var
  ID: string;
  s: TKeymanSetting;
  json: TJSONObject;
  stream: TStringStream;
  p: TJSONPair;
begin
  stream := TStringStream.Create('', TEncoding.UTF8);
  try
    stream.LoadFromFile(Filename);
    json := TJSONObject.ParseJSONValue(stream.DataString, False, True) as TJSONObject;
  finally
    stream.Free;
  end;
  try
    for p in json do
    begin
      ID := p.JsonString.Value;
      s := Settings.Find(ID);
      if not Assigned(s) then
      begin
        // Custom value, do we support it?
        if ID.StartsWith(CustomKeymanSetting_TSFApp.ID, True) then
        begin
          s := TKeymanSetting.CreateCustom_TSFApp(ID);
          Settings.Add(s);
        end
        else
          // We'll silently ignore it
          Continue;
      end;

      case s.Base.ValueType of
        kstString: s.ValueStr := p.JsonValue.Value;
        kstInteger: s.ValueInt := p.JsonValue.AsType<Integer>;
      end;
    end;
  finally
    json.Free;
  end;
end;

class procedure TKeymanSettingsManagerFile.Export(Settings: TKeymanSettings; const Filename: string);
var
  s: TKeymanSetting;
  json: TJSONObject;
  txt: string;
  stream: TStringStream;
begin
  json := TJSONObject.Create;
  try
    for s in Settings do
    begin
      if not s.IsEmpty then
        case s.Base.ValueType of
          kstString: json.AddPair(s.Base.ID, s.ValueStr);
          kstInteger: json.AddPair(s.Base.ID, TJSONNumber.Create(s.ValueInt));
        end;
    end;

    txt := json.Format(2);
    stream := TStringStream.Create(txt, TEncoding.UTF8);
    try
      stream.SaveToFile(Filename);
    finally
      stream.Free;
    end;
  finally
    json.Free;
  end;
end;

end.
