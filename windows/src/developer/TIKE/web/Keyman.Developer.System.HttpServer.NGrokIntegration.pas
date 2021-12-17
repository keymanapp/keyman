unit Keyman.Developer.System.HttpServer.NgrokIntegration;

interface

type
  TNgrokIntegration = class
  private
    FUrl: string;
    FLocalPort: Integer;
    FNgrokProcessHandle: Cardinal;
    FNgrokProcessId: Cardinal;
    FNgrokPort: Integer;
    FShowNgrok: Boolean;
    FNgrokProcessOwned: Boolean;
    FRegion: string;
    FToken: string;
    function IsConfigured: Boolean;
    function IsInstalled: Boolean;
    procedure LaunchNgrok;
    procedure WriteConfigFile;
    function GetUrl: string;
    function CheckForPreviousLaunch: Boolean;
    function GetTunnelUrl: string;
    function ConfigFilename: string;
  public
    constructor Create(ALocalPort, ANgrokPort: Integer;
      const AToken, ARegion: string; AShowNgrok: Boolean);
    destructor Destroy; override;
    procedure InstantiateTunnel;
    procedure TeardownTunnel;
    function Running: Boolean;
    function Connected: Boolean;
    class function NgrokPath: string;
    property LocalPort: Integer read FLocalPort;
    property ShowNgrok: Boolean read FShowNgrok;
    property Token: string read FToken;
    property Region: string read FRegion;
    property Url: string read GetUrl;
  end;

implementation

uses
  System.Classes,
  System.Json,
  System.Net.HttpClient,
  System.SysUtils,
  Winapi.ShlObj,
  Winapi.Tlhelp32,
  Winapi.Windows,

  JsonUtil,
  KeymanPaths,
  RegistryKeys,
  utilexecute;

{ TNgrokIntegration }

const
  STunnelName = 'keyman_developer';

constructor TNgrokIntegration.Create(ALocalPort, ANgrokPort: Integer;
  const AToken, ARegion: string; AShowNgrok: Boolean);
begin
  inherited Create;
  FLocalPort := ALocalPort;
  FRegion := ARegion;
  FToken := AToken;
  FNgrokPort := ANgrokPort;
  FShowNgrok := AShowNgrok;
  if IsConfigured then
    InstantiateTunnel;
end;

destructor TNgrokIntegration.Destroy;
begin
  if Running then
    TeardownTunnel;
  inherited Destroy;
end;

class function TNgrokIntegration.NgrokPath: string;
begin
  Result := GetFolderPath(CSIDL_APPDATA) + SFolderKeymanDeveloper + '\ngrok.exe';
end;

function TNgrokIntegration.GetUrl: string;
begin
  if FUrl = '' then
  begin
    FUrl := GetTunnelUrl;
    if FUrl = '' then
      FUrl := '?';
  end;

  Result := FUrl;
end;

function TNgrokIntegration.IsConfigured: Boolean;
begin
  Result := IsInstalled and (FToken <> '');
end;

function TNgrokIntegration.IsInstalled: Boolean;
begin
  Result := FileExists(NgrokPath);
end;

function TNgrokIntegration.Running: Boolean;
var
  ec: Dword;
begin
  Result :=
    (FNgrokProcessHandle <> 0) and
    GetExitCodeProcess(FNgrokProcessHandle, ec) and
    (ec = STILL_ACTIVE);
end;

function TNgrokIntegration.Connected: Boolean;
begin
  Result := Running and (Url <> '') and (Url <> '?');
end;

procedure TNgrokIntegration.InstantiateTunnel;
begin
  if not CheckForPreviousLaunch then
  begin
    LaunchNgrok;
  end;
end;

function TNgrokIntegration.GetTunnelUrl: string;
var
  client: THttpClient;
  res: IHTTPResponse;
  NgrokControlUrl: string;
  data: string;
  o: TJSONObject;
begin
  Result := '';

  NgrokControlUrl := Format('http://127.0.0.1:%d/api/tunnels/%s', [
    FNgrokPort,
    STunnelName
  ]);

  client := THttpClient.Create;
  try
    // We are connecting to localhost so we can timeout quickly
    client.ConnectionTimeout := 100;
    try
      res := client.Get(NgrokControlUrl);
    except
      on E:ENetHTTPClientException do
        Exit;
    end;
    if res.StatusCode = 200 then
    begin
      data := res.ContentAsString(TEncoding.UTF8);
      o := System.JSON.TJSONObject.ParseJSONValue(data, True) as TJSONObject;
      if Assigned(o) then
      begin
        Result := o.GetValue<string>('public_url');
      end;
    end;
  finally
    client.Free;
  end;

end;

function TNgrokIntegration.CheckForPreviousLaunch: Boolean;
var
  hSnapshot: THandle;
  pe: TProcessEntry32;
  client: THttpClient;
  res: IHTTPResponse;
  u: string;
begin
  // First, try and connect to our normal control port and
  // see if there is life
  u := GetTunnelUrl;
  if u = '' then
    Exit(False);

  FUrl := u;

  // Look for an Ngrok process, check its command line
  hSnapshot := CreateToolhelp32Snapshot(TH32CS_SNAPPROCESS, 0);
  if hSnapshot = 0 then
  begin
    // TODO: log error?
    Exit(False);
  end;

  try
    FillChar(pe, SizeOf(pe), 0);
    if Process32First(hSnapshot, pe) then
    begin
      repeat
        if SameText(ExtractFileName(pe.szExeFile), 'Ngrok.exe') then
        begin
          FNgrokProcessId := pe.th32ProcessID;
          FNgrokProcessHandle := OpenProcess(PROCESS_ALL_ACCESS, False, FNgrokProcessId);
          Exit(FNgrokProcessHandle <> 0);
        end;
      until not Process32Next(hSnapshot, pe);
    end;
  finally
    CloseHandle(hSnapshot);
  end;

  // TODO: consider if we have multiple KMDev processes
  Result := False;
end;

procedure TNgrokIntegration.LaunchNgrok;
var
  pi: TProcessInformation;
  sw: Integer;
  cmd: string;
begin
  if Running or not IsInstalled then
    Exit;

  if FShowNgrok
    then sw := SW_SHOWNORMAL
    else sw := SW_HIDE;

  WriteConfigFile;
  cmd := Format('"%s" start %s --config "%s"', [
    NgrokPath,
    STunnelName,
    ConfigFilename
  ]);

  // launch Ngrok, capture the url it is running on
  if TUtilExecute.Execute(cmd, ExtractFilePath(ParamStr(0)), sw, pi) then
  begin
    CloseHandle(pi.hThread);
    FNgrokProcessHandle := pi.hProcess;
    FNgrokProcessId := pi.dwProcessId;
    FNgrokProcessOwned := True;
  end;
end;

function AttachConsole(dwProcessId: DWORD): BOOL; stdcall; external kernel32;

procedure TNgrokIntegration.TeardownTunnel;
begin
  if Running then
  begin
    // https://stackoverflow.com/a/15281070/1836776
    if FNgrokProcessOwned then
    begin
      if AttachConsole(FNgrokProcessId) then
      try
        SetConsoleCtrlHandler(nil, True);
        GenerateConsoleCtrlEvent(CTRL_C_EVENT, 0);
      finally
        FreeConsole;
      end;
      // Wait for up to 2 seconds for Ngrok to exit
      if WaitForSingleObject(FNgrokProcessHandle, 2000) = WAIT_TIMEOUT then
      begin
        // If it fails to close, terminate it forcefully
        TerminateProcess(FNgrokProcessHandle, 0);
      end;
    end;
    CloseHandle(FNgrokProcessHandle);
  end;

  FNgrokProcessHandle := 0;
  FNgrokProcessId := 0;
end;

function TNgrokIntegration.ConfigFilename: string;
begin
  Result := GetFolderPath(CSIDL_APPDATA) + SFolderKeymanDeveloper + '\Ngrok.yml';
end;

procedure TNgrokIntegration.WriteConfigFile;
var
  st: TStringStream;
  data: string;
  tokenLine: string;
const
  SConfigFile =
    '%s'+
    'region: %s'#13#10+
    'metadata: keyman_developer'#13#10+
    'web_addr: 127.0.0.1:%d'+#13#10+
    'tunnels:'#13#10+
    '  keyman_developer:'#13#10+
    '    proto: http'#13#10+
    '    addr: %d'#13#10+
    '    bind_tls: true'#13#10;
begin
  if FToken <> ''
    then tokenLine := 'authtoken: '+FToken+#13#10
    else tokenLine := '';

  data := Format(SConfigFile, [
    {authtoken}    tokenLine,
    {region}       FRegion,
    {control port} FNgrokPort,
    {http port}    FLocalPort
  ]);
  st := TStringStream.Create(data, TEncoding.UTF8);
  try
    st.SaveToFile(ConfigFilename);
  finally
    st.Free;
  end;
end;

end.
