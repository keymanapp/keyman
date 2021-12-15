unit Keyman.Developer.System.HttpServer.NGrokIntegration;

interface

type
  TNGrokIntegration = class
  private
    FUrl: string;
    FNGrokPath: string;
    FLocalPort: Integer;
    FNGrokProcessHandle: Cardinal;
    FNGrokProcessId: Cardinal;
    FNGrokPort: Integer;
    FShowNGrok: Boolean;
    function IsConfigured: Boolean;
    function IsInstalled: Boolean;
    procedure FindNgrok;
    procedure LaunchNGrok;
    function GetUrl: string;
  public
    constructor Create(ALocalPort: Integer; AShowNGrok: Boolean);
    destructor Destroy; override;
    procedure InstantiateTunnel;
    procedure TeardownTunnel;
    function Running: Boolean;
    function Connected: Boolean;
    property LocalPort: Integer read FLocalPort;
    property ShowNGrok: Boolean read FShowNGrok;
    property Url: string read GetUrl;
  end;

implementation

uses
  System.Json,
  System.Net.HttpClient,
  System.SysUtils,
  Winapi.ShlObj,
  Winapi.Windows,

  JsonUtil,
  KeymanPaths,
  utilexecute;

{ TNGrokIntegration }

constructor TNGrokIntegration.Create(ALocalPort: Integer; AShowNGrok: Boolean);
begin
  inherited Create;
  FLocalPort := ALocalPort;
  FShowNGrok := AShowNGrok;
  FindNgrok;
  if IsConfigured then
    InstantiateTunnel;
end;

destructor TNGrokIntegration.Destroy;
begin
  if Running then
    TeardownTunnel;
  inherited Destroy;
end;

procedure TNGrokIntegration.FindNgrok;
var
  filePart: PWideChar;
  buf: array[0..MAX_PATH] of char;
begin
  if FileExists(ExtractFilePath(ParamStr(0)) + 'ngrok.exe') then
    FNGrokPath := ExtractFilePath(ParamStr(0)) + 'ngrok.exe'
  else if SearchPath(nil, 'ngrok.exe', nil, MAX_PATH, buf, filePart) > 0 then
    FNGrokPath := buf
  else
    FNGrokPath := '';
end;

function TNGrokIntegration.GetUrl: string;
var
  client: THttpClient;
  ngrokControlUrl: string;
  res: IHTTPResponse;
  data: string;
  o: TJSONObject;
begin
  if FUrl <> '' then
  begin
    Exit(FUrl);
  end;

  FUrl := '?';
  FNGrokPort := 4040; // TODO
  ngrokControlUrl := Format('http://127.0.0.1:%d/api/tunnels/command_line', [FNGrokPort]);

  client := THttpClient.Create;
  try
    res := client.Get(ngrokControlUrl);
    if res.StatusCode = 200 then
    begin
      data := res.ContentAsString(TEncoding.UTF8);
      o := System.JSON.TJSONObject.ParseJSONValue(data, True) as TJSONObject;
      if Assigned(o) then
      begin
        FUrl := o.GetValue<string>('public_url');
        if FUrl = '' then
          FUrl := '?';
      end;
      Result := FUrl;
    end;
  finally
    client.Free;
  end;
end;

function TNGrokIntegration.IsConfigured: Boolean;
var
  path: string;
begin
  path := GetFolderPath(CSIDL_PROFILE) + '.ngrok2\ngrok.yml';
  Result := IsInstalled and FileExists(path);
end;

function TNGrokIntegration.IsInstalled: Boolean;
begin
  Result := FNGrokPath <> '';
end;

function TNGrokIntegration.Running: Boolean;
var
  ec: Dword;
begin
  Result :=
    (FNGrokProcessHandle <> 0) and
    GetExitCodeProcess(FNGrokProcessHandle, ec) and
    (ec = STILL_ACTIVE);
end;

function TNGrokIntegration.Connected: Boolean;
begin
  Result := Running and (Url <> '') and (Url <> '?');
end;

procedure TNGrokIntegration.InstantiateTunnel;
begin
  LaunchNGrok;
end;

procedure TNGrokIntegration.LaunchNGrok;
var
  pi: TProcessInformation;
  sw: Integer;
  cmd: string;
begin
  if Running then
    Exit;

  if FShowNGrok
    then sw := SW_SHOWNORMAL
    else sw := SW_HIDE;

  cmd := Format('"%s" http %d --bind-tls true', [FNGrokPath, FLocalPort]);
  // launch ngrok, capture the url it is running on
  if TUtilExecute.Execute(cmd, ExtractFilePath(ParamStr(0)), sw, pi) then
  begin
    CloseHandle(pi.hThread);
    FNGrokProcessHandle := pi.hProcess;
    FNGrokProcessId := pi.dwProcessId;
  end;
end;

function AttachConsole(dwProcessId: DWORD): BOOL; stdcall; external kernel32;

procedure TNGrokIntegration.TeardownTunnel;
begin
  if Running then
  begin
    // https://stackoverflow.com/a/15281070/1836776
    if AttachConsole(FNGrokProcessId) then
    try
      SetConsoleCtrlHandler(nil, True);
      GenerateConsoleCtrlEvent(CTRL_C_EVENT, 0);
    finally
      FreeConsole;
    end;
    // Wait for up to 2 seconds for ngrok to exit
    if WaitForSingleObject(FNGrokProcessHandle, 2000) = WAIT_TIMEOUT then
    begin
      // If it fails to close, terminate it forcefully
      TerminateProcess(FNGrokProcessHandle, 0);
    end;
    CloseHandle(FNGrokProcessHandle);
  end;

  FNGrokProcessHandle := 0;
  FNGrokProcessId := 0;
end;

end.
