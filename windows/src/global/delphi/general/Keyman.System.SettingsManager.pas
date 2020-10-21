unit Keyman.System.SettingsManager;

interface

uses
  Keyman.System.Settings;

type
  TKeymanSettingsManager = class
    ///  <summary>Loads settings from the registry. Missing settings are reset
    ///  to default.</summary>
    class procedure Load(Settings: TKeymanSettings); static;

    ///  <summary>Applies the settings to the registry.</summary>
    class procedure Save(Settings: TKeymanSettings; SaveAdminValues: Boolean); static;
  end;

implementation

uses
  System.Classes,
  System.SysUtils,
  System.Win.Registry,
  Winapi.Windows,

  RegistryKeys;


{ TKeymanSettingsManager }

class procedure TKeymanSettingsManager.Load(Settings: TKeymanSettings);
var
  r: TRegistry;
  s: TKeymanSetting;
  filenames: TStringList;
  filename: string;
begin
  Settings.Reset;

  r := TRegistry.Create;
  try
    //
    // Load standard keys
    //

    for s in Settings do
    begin
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

      s.ClearModified;
    end;

    //
    // Load custom keys
    //

    r.RootKey := HKEY_LOCAL_MACHINE;
    if r.OpenKeyReadOnly('\' + SRegKey_AppIntegration) then
    begin
      filenames := TStringList.Create;
      try
        r.GetValueNames(filenames);
        for filename in filenames do
        begin
          s := TKeymanSetting.CreateCustom_TSFApp(CustomKeymanSetting_TSFApp.ID + filename);
          s.ValueInt := r.ReadInteger(filename);
          s.ClearModified;
          Settings.Add(s);
        end;
      finally
        filenames.Free;
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
      if not r.KeyExists('\' + s.Base.Key) and s.IsEmpty then
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
