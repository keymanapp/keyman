unit Keyman.System.SettingsManager;

interface

uses
  Keyman.System.Settings;

type
  TKeymanSettingsManager = class
    class procedure Load(Settings: TKeymanSettings);
    class procedure Save(Settings: TKeymanSettings; SaveAdminValues: Boolean);
  end;

implementation

uses
  System.SysUtils,
  System.Win.Registry;

{ TKeymanSettingsManager }

class procedure TKeymanSettingsManager.Load(Settings: TKeymanSettings);
var
  r: TRegistry;
  s: TKeymanSetting;
begin
  r := TRegistry.Create;
  try
    for s in Settings do
    begin
      s.Reset;  // Loads 'default' value for s

      r.RootKey := s.Base.RootKey;
      if r.OpenKeyReadOnly('\' + s.Base.Key) then
      begin
        if r.ValueExists(s.Base.Name) then
        begin
          case s.Base.ValueType of
            kstString: s.ValueStr := r.ReadString(s.Base.Name);
            kstInteger: s.ValueInt := r.ReadInteger(s.Base.Name);
          end;
        end;
      end;
    end;
  finally
    r.Free;
  end;
end;

class procedure TKeymanSettingsManager.Save(Settings: TKeymanSettings; SaveAdminValues: Boolean);
var
  r: TRegistry;
  s: TKeymanSetting;
begin
  r := TRegistry.Create;
  try
    for s in Settings do
    begin
      if s.Base.RequiresAdministrator and not SaveAdminValues then
        Continue;

      r.RootKey := s.Base.RootKey;
      if not r.KeyExists(s.Base.Key) and s.IsEmpty then
        Continue;
      if r.OpenKey('\' + s.Base.Key, True) then
      begin
        if s.IsEmpty then
        begin
          if r.ValueExists(s.Base.Name) then
            r.DeleteValue(s.Base.Name);
        end
        else
        begin
          case s.Base.ValueType of
            kstString: r.WriteString(s.Base.Name, s.ValueStr.Trim);
            kstInteger: r.WriteInteger(s.Base.Name, s.ValueInt);
          end;
        end;
      end;
    end;
  finally
    r.Free;
  end;
end;

end.
