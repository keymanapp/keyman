unit Keyman.System.KMConfigMain;

interface

uses
  Keyman.System.Settings;

type
  TKMConfig = class
  private
    class var IsAdmin: Boolean;
    class var Settings: TKeymanSettings;
    class function Import(const Filename: string): Boolean; static;
    class function Export(const Filename: string): Boolean; static;
    class function Reset(const ID: string): Boolean; static;
    class function SetValue(const ID, Value: string): Boolean; static;
    class function Show(const P2: string): Boolean; static;
    class procedure Log(const message: string);
    class procedure LogError(const message: string);
    class procedure LogWarning(const message: string);
  public
    class constructor ClassCreate;
    class destructor ClassDestroy;
    class procedure Run; static;
  end;

implementation

uses
  System.SysUtils,
  System.Win.Registry,
  Winapi.Windows,

  Keyman.System.SettingsManager,
  Keyman.System.SettingsManagerFile,
  RegistryKeys;

type
  TANSIColor = record
    Green, Grey, Default, Yellow, Red, White: string;
  end;

var
  ANSIColor: TANSIColor;

class constructor TKMConfig.ClassCreate;
var
  r: TRegistry;
begin
  inherited;
  r := TRegistry.Create;
  try
    r.RootKey := HKEY_LOCAL_MACHINE;
    IsAdmin := r.OpenKey(SRegKey_KeymanEngine_LM, True);
  finally
    r.Free;
  end;

  Settings := TKeymanSettings.Create;
end;

class destructor TKMConfig.ClassDestroy;
begin
  Settings.Free;
  inherited;
end;

class procedure TKMConfig.Run;
var
  Command: string;
  Result: Boolean;
begin
  Settings := TKeymanSettings.Create;
  Command := ParamStr(1).ToLower;
  if Command = 'import' then Result := Import(ParamStr(2))
  else if Command = 'export' then Result := Export(ParamStr(2))
  else if Command = 'reset' then Result := Reset(ParamStr(2))
  else if Command = 'set' then Result := SetValue(ParamStr(2), ParamStr(3))
  else if Command = 'show' then Result := Show(ParamStr(2))
  else
  begin
    Result := False;
    writeln('Usage: kmconfig command [options]');
    writeln('Commands:');
    writeln('  import <settings.json>   Imports settings into local machine registry');
    writeln('  export <settings.json>   Exports settings from local machine registry to file');
    writeln('  reset [id]               Resets one or all settings to default');
    writeln('  set id value             Sets setting identified by id to value');
    writeln('  show [-a] [id]           Lists one or all settings; -a to show all, even empty settings');
    writeln('Note: registry changes may require elevation to succeed.');
  end;

  if Result
    then ExitCode := 0
    else ExitCode := 1;
end;

class procedure TKMConfig.Log(const message: string);
begin
  writeln(message);
end;

class procedure TKMConfig.LogError(const message: string);
begin
  Log(AnsiColor.Red+'ERROR: '+message+AnsiColor.Default);
end;

class procedure TKMConfig.LogWarning(const message: string);
begin
  Log(AnsiColor.Yellow+'WARNING: '+message+AnsiColor.Default);
end;

class function TKMConfig.Export(const Filename: string): Boolean;
begin
  TKeymanSettingsManager.Load(Settings);
  TKeymanSettingsManagerFile.Export(Settings, Filename);
  Log('All Keyman settings have been exported to '+Filename);
  Result := True;
end;

class function TKMConfig.Import(const Filename: string): Boolean;
begin
  TKeymanSettingsManager.Load(Settings);
  TKeymanSettingsManagerFile.Import(Settings, Filename);
  TKeymanSettingsManager.Save(Settings, IsAdmin);
  Log('Keyman settings in '+Filename+' have been imported');
  if not IsAdmin then
    LogWarning('Some settings that require Administrator permissions may not have been updated');
  Result := True;
end;

class function TKMConfig.Reset(const ID: string): Boolean;
var
  Setting: TKeymanSetting;
begin
  if ID = '' then
  begin
    Settings.Reset;
    TKeymanSettingsManager.Load(Settings);
    if Settings.Modified then
    begin
      TKeymanSettingsManager.Save(Settings, IsAdmin);
      Log('Keyman settings have been reset to default');
      if not IsAdmin then
        LogWarning('Some settings that require Administrator permissions may not have been updated');
    end
    else
    begin
      Log('All Keyman settings were already default');
    end;
  end
  else
  begin
    TKeymanSettingsManager.Load(Settings);
    Setting := Settings.Find(ID);
    if not Assigned(Setting) then
    begin
      // Custom value, do we support it?
      if ID.StartsWith(CustomKeymanSetting_TSFApp.ID, True) then
      begin
        LogWarning('Setting '+ID+' does not exist; not updating.');
        Exit(True);
      end
      else
      begin
        LogError('Setting '+ID+' is not a valid Keyman setting.');
        Exit(False);
      end;
    end;
    if Setting.IsEmpty then
    begin
      LogWarning('Setting '+ID+' is already default; not updating.');
      Exit(True);
    end;
    if (Setting.Base.RootKey = HKEY_LOCAL_MACHINE) and not IsAdmin then
    begin
      LogError('Setting '+ID+' requires Administrator permissions to update.');
      Exit(False);
    end;

    Setting.Reset;
    if Setting.Modified then
    begin
      TKeymanSettingsManager.Save(Settings, IsAdmin);
      Log('Keyman setting '+ID+' has been reset to default');
    end
    else
    begin
      LogWarning('Keyman setting '+ID+' was already default; not updating');
    end;
  end;

  Result := True;
end;

class function TKMConfig.SetValue(const ID, Value: string): Boolean;
var
  Setting: TKeymanSetting;
begin
  Setting := Settings.Find(ID);

  if not Assigned(Setting) then
  begin
    // Custom value, do we support it?
    if ID.StartsWith(CustomKeymanSetting_TSFApp.ID, True) then
    begin
      Setting := TKeymanSetting.CreateCustom_TSFApp(ID);
      Settings.Add(Setting);
    end
    else
    begin
      LogError(ID+' is not a valid Keyman setting.');
      Exit(False);
    end;
  end;

  if (Setting.Base.RootKey = HKEY_LOCAL_MACHINE) and not IsAdmin then
  begin
    LogError('Setting '+ID+' requires Administrator permissions to update.');
    Exit(False);
  end;

  case Setting.Base.ValueType of
    kstString: Setting.ValueStr := Value;
    kstInteger: Setting.ValueInt := StrToInt(Value);
  end;

  TKeymanSettingsManager.Save(Settings, IsAdmin);
  Log('Keyman setting '+ID+' has been updated to '+Value);
  Result := True;
end;

class function TKMConfig.Show(const P2: string): Boolean;

  function ShowOneSetting(const ID: string): Boolean;
  var
    Setting: TKeymanSetting;
    v: string;
  begin
    Setting := Settings.Find(ID);
    if not Assigned(Setting) then
    begin
      LogError('Setting '+ID+' is not a valid Keyman setting.');
      Exit(False);
    end;
    case Setting.Base.ValueType of
      kstString: v := Setting.ValueStr;
      kstInteger: v := Setting.ValueInt.ToString;
    end;
    if Setting.IsEmpty
      then Log(AnsiColor.Green+Setting.Base.ID+AnsiColor.Grey+'='+AnsiColor.Grey+v+AnsiColor.Default)
      else Log(AnsiColor.Green+Setting.Base.ID+AnsiColor.Grey+'='+AnsiColor.White+v+AnsiColor.Default);
    Result := True;
  end;

  procedure ShowAllSettings(ShowAll: Boolean);
  var
    Setting: TKeymanSetting;
  begin
    for Setting in Settings do
    begin
      if ShowAll or not Setting.IsEmpty then
        ShowOneSetting(Setting.Base.ID);
    end;
  end;
begin
  Result := True;
  TKeymanSettingsManager.Load(Settings);
  if SameText(P2, '-a') then
    ShowAllSettings(True)
  else if P2 = '' then
    ShowAllSettings(False)
  else
    Result := ShowOneSetting(P2);
end;

function DetectColorMode: Boolean;
var
  mode: DWORD;
  hConsole: THandle;
const
  ENABLE_VIRTUAL_TERMINAL_PROCESSING = 4;
begin
  Result := False;
  mode := 0;
  hConsole := GetStdHandle(STD_OUTPUT_HANDLE);
  if hConsole = INVALID_HANDLE_VALUE then
  begin
    //writeln(Format('GetStdHandle failed with %d %s', [GetLastError, SysErrorMessage(GetLastError)]));
    Exit;
  end;

  if GetEnvironmentVariable('MSYSTEM') = 'MINGW64' then
  begin
    // MinGW64 test
    // Use colour mode only with a non-redirected console. This test fails with pipes.
    // For pipe use, explicitly use -no-color parameter
    Result := GetFileType(hConsole) = 3;
  end
  else
  begin
    // Win32 console color mode test
    if not GetConsoleMode(hConsole, mode) then
      Exit;

    mode := mode or ENABLE_VIRTUAL_TERMINAL_PROCESSING;
    if not SetConsoleMode(hConsole, mode) then
      Exit;

    Result := True;
  end;
end;

const
  ESC=#$1b;
  ESC_BRIGHT_YELLOW=ESC+'[38;2;255;255;0m';
  ESC_RED=ESC+'[38;2;255;0;0m';
  ESC_GREEN=ESC+'[38;2;0;255;0m';
  ESC_GREY=ESC+'[38;2;128;132;128m';
  ESC_WHITE=ESC+'[38;2;255;255;255m';
  ESC_DEFAULT=ESC+'[0m';

initialization
  if DetectColorMode then
  begin
    ANSIColor.Green := ESC_GREEN;
    ANSIColor.Default := ESC_DEFAULT;
    ANSIColor.Red := ESC_RED;
    ANSIColor.Yellow := ESC_BRIGHT_YELLOW;
    ANSIColor.Grey := ESC_GREY;
    ANSIColor.White := ESC_WHITE;
  end;
end.
