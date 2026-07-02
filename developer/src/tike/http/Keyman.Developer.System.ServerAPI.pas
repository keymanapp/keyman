unit Keyman.Developer.System.ServerAPI;

interface

uses
  System.Classes,
  System.Net.HttpClient,
  System.Net.Mime;

type
  TServerDebugAPI = class
  private
    class var
      LastGetStatusTime: UInt64;
      FngrokEnabled: Boolean;
      FngrokEndpoint: string;
    class function IsDebugObjectRegistered(const objectType, id: string): Boolean; static;
    class procedure Post(const api: string; mfd: TMultipartFormData); static;
    class function HostName: string; static;
    class function LoadFileToString(const Filename: string;
      var s: string): Boolean; static;
  public
    // Server Control functions
    class function Running: Boolean; static;
    class procedure StartServer; static;
    class procedure StopServer; static;
    class procedure GetServerURLs(v: TStrings); static;
    class function ServerBinPath: string; static;
    class procedure CleanupOldNgrok; static;

    // API endpoints
    class function IsKeyboardRegistered(const Filename: string): Boolean; static;
    class procedure RegisterKeyboard(const Filename, Version, FontFace, OskFontFace: string); static;
    // Not currently used: class procedure UnregisterKeyboard(const Filename: string); static;

    class function IsPackageRegistered(const Filename: string): Boolean; static;
    class procedure RegisterPackage(const Filename, Name: string); static;
    // Not currently used: class procedure UnregisterPackage(const Filename: string); static;

    class function IsModelRegistered(const Filename: string): Boolean; static;
    class procedure RegisterModel(const Filename: string); static;
    // Not currently used: class procedure UnregisterModel(const Filename: string); static;

    class function IsFontRegistered(const Filename: string): Boolean; static;
    class procedure RegisterFont(const Filename, Facename: string); overload; static;
    class procedure RegisterFont(Filedata: TStream; const Facename: string); overload; static;
    // Not currently used: class procedure UnregisterFont(const Facename: string); static;

    class function GetStatus: Boolean; static;
    class function UpdateStatus: Boolean; static;

    class property ngrokEnabled: Boolean read FngrokEnabled;
    class property ngrokEndpoint: string read FngrokEndpoint;
  end;

implementation

uses
  IdGlobal,
  IdGlobalProtocols,
  IdStack,
  System.JSON,
  System.NetEncoding,
  System.Net.URLClient,
  System.SysUtils,
  KeymanDeveloperOptions,
  Keyman.Developer.System.KeymanDeveloperPaths,
  Keyman.System.KeyboardUtils,
  Keyman.System.LexicalModelUtils,
  RegistryKeys,
  utilexecute,
  utilsystem,
  Winapi.ShlObj,
  Winapi.Windows;

{ TServerDebugAPI }

class procedure TServerDebugAPI.StartServer;
var
  sw: Integer;
begin
  if Running then
    Exit;

  if FKeymanDeveloperOptions.ServerServerShowConsoleWindow
    then sw := SW_SHOWNORMAL
    else sw := SW_HIDE;

  // We have an explicit NODE_PATH env var so that Node can
  // find our downloaded version of ngrok in %AppData%\Server\bin\@ngrok
  // without us risking messing up the user's own node environment (if
  // they have one!)
  SetEnvironmentVariable('NODE_PATH', PChar(ServerBinPath));

  if not TUtilExecute.Execute(Format('"%s" .', [TKeymanDeveloperPaths.NodePath]),
    TKeymanDeveloperPaths.ServerPath, sw) then
  begin
    RaiseLastOSError;
  end;

  LastGetStatusTime := 0;
end;

class function TServerDebugAPI.ServerBinPath: string;
begin
  Result := TKeymanDeveloperPaths.ServerDataPath + 'bin';
  ForceDirectories(Result);
end;

function QueryFullProcessImageName(
  hProcess: THandle;
  dwFlags: DWORD;
  lpExeName: LPWSTR;
  var lpdwSize: DWORD): BOOL; stdcall; external kernel32
  name 'QueryFullProcessImageNameW';

class procedure TServerDebugAPI.StopServer;
var
  mfd: TMultipartFormData;
begin
  mfd := TMultipartFormData.Create;
  try
    Post('shutdown', mfd);
  finally
    mfd.Free;
  end;

  LastGetStatusTime := 0;
end;

class function TServerDebugAPI.LoadFileToString(const Filename: string; var s: string): Boolean;
var
  fs: TFileStream;
  ss: TStringStream;
begin
  try
    fs := TFileStream.Create(Filename, fmOpenRead);
    try
      ss := TStringStream.Create('', TEncoding.UTF8);
      try
        ss.CopyFrom(fs, 0);
        s := ss.DataString;
        Result := True;
      finally
        ss.Free;
      end;
    finally
      fs.Free;
    end;
  except
    on E:EFileStreamError do
    begin
      // TODO: Log this error?
      Result := False;
    end;
  end;
end;

class function TServerDebugAPI.Running: Boolean;
var
  s: string;
  pid: Integer;
  h: THandle;
  buf: array[0..MAX_PATH] of char;
  sz: DWord;
begin
  Result := False;

  // TODO: check a ping endpoint instead?

  if not FileExists(TKeymanDeveloperPaths.ServerDataPath + 'lock.json') or
    not FileExists(TKeymanDeveloperPaths.ServerDataPath + 'pid.json') then
  begin
    Exit;
  end;

  {if System.SysUtils.DeleteFile(TKeymanDeveloperPaths.ServerDataPath + 'lock.json') then
  begin
    // Suggests improper shutdown of node as lock.json should not be deleteable
    // if the server is running
    Exit;
  end;}

  if not LoadFileToString(TKeymanDeveloperPaths.ServerDataPath + 'pid.json', s) then
    Exit;
  if not TryStrToInt(s.Trim, pid) then
    Exit;

  // check the pid, and if it is node, we'll basically assume it's our one

  h := OpenProcess(PROCESS_ALL_ACCESS, False, pid);
  if h = 0 then
    Exit;
  try
    sz := MAX_PATH;
    if not QueryFullProcessImageName(h, 0, buf, sz) then
      Exit;
    if not SameText(ExtractFileName(buf), 'node.exe') then
      Exit;
  finally
    CloseHandle(h);
  end;

  Result := True;
end;

class procedure TServerDebugAPI.CleanupOldNgrok;
begin
  // #15625: In February 2026, we moved from npm ngrok module to @ngrok/ngrok.
  // This npm module uses a different binary version of ngrok, so we should
  // cleanup the old ngrok.exe if we find it.
  if FileExists(TServerDebugAPI.ServerBinPath + '\ngrok.exe') then
  begin
    System.SysUtils.DeleteFile(TServerDebugAPI.ServerBinPath + '\ngrok.exe');
  end;
end;

class procedure TServerDebugAPI.GetServerURLs(v: TStrings);
const
  IPv4Loopback  = '127.0.0.1';          {do not localize}

  function GetHostName(tp: TComputerNameFormat): string;
  var
    buf: array[0..260] of char;
    sz: Cardinal;
  begin
    if GetComputerNameEx(tp, buf, sz) then
      Result := buf
    else
      Result := '';
  end;
var
  port: string;
  sNetbios: string;
  sHost: string;
  sFull: string;
  i: Integer;
  FIPv4Addresses: TIdStackLocalAddressList;
begin
  if TServerDebugAPI.UpdateStatus and
    (TServerDebugAPI.ngrokEndpoint <> '') then
  begin
    v.Add(TServerDebugAPI.ngrokEndpoint);
  end;
  if FKeymanDeveloperOptions.ServerUseLocalAddresses then
  begin
    port := ':'+IntToStr(FKeymanDeveloperOptions.ServerDefaultPort);
    sFull := GetHostName(ComputerNameDnsFullyQualified);
    sHost := GetHostName(ComputerNameDnsHostname);
    sNetbios := GetHostName(ComputerNameNetBIOS);
    if SameText(sHost, sFull) then sHost := '';
    if SameText(sNetbios, sHost) or SameText(sNetbios, sFull) then sNetbios := '';

    if sFull <> '' then v.Add('http://'+sFull+port);
    if sHost <> '' then v.Add('http://'+sHost+port);
    if sNetbios <> '' then v.Add('http://'+sNetbios+port);

    FIPv4Addresses := TIdStackLocalAddressList.Create;
    try
      TIdStack.IncUsage;
      try
        GStack.GetLocalAddressList(FIPv4Addresses);
      finally
        TIdStack.DecUsage;
      end;

      for i := 0 to FIPv4Addresses.Count - 1 do
        v.Add('http://'+FIPv4Addresses[i].IPAddress+port);
    finally
      FIPv4Addresses.Free;
    end;

    v.Add('http://localhost'+port);
    v.Add('http://'+IPv4Loopback+port);
  end;
end;

class function TServerDebugAPI.UpdateStatus: Boolean;
begin
  if (GetTickCount64 - LastGetStatusTime > 1000)
    then Result := GetStatus
    else Result := True;
end;

class function TServerDebugAPI.GetStatus: Boolean;
var
  http: THttpClient;
  res: IHTTPResponse;
  o: TJSONObject;
begin
// TODO: Consider reporting this info by file to save http transaction cost?
//  if not LoadFileToString(TKeymanDeveloperPaths.ServerDataPath + 'status.json', s) then
//    Exit(False);

  FngrokEnabled := False;
  FngrokEndpoint := '';

  if not Running then
    Exit(False);

  http := THttpClient.Create;
  try
    try
      http.ConnectionTimeout := 100;
      res := http.Get(HostName+'api/status');
      Result := res.StatusCode = 200;
      if Result then
      begin
        o := TJSONObject.ParseJSONValue(res.ContentAsString) as TJSONObject;
        if not Assigned(o) then
          Exit(False);

        if not o.TryGetValue<Boolean>('ngrokEnabled', FngrokEnabled) then FngrokEnabled := False;
        if not o.TryGetValue<string>('ngrokEndpoint', FngrokEndpoint) then FngrokEndpoint := '';

        LastGetStatusTime := GetTickCount64;
      end;
    except
      Result := False;
    end;
  finally
    http.Free;
  end;
end;

class function TServerDebugAPI.HostName: string;
begin
  Result := 'http://localhost:'+FKeymanDeveloperOptions.ServerDefaultPort.ToString+'/';
end;

class function TServerDebugAPI.IsDebugObjectRegistered(const objectType, id: string): Boolean;
var
  http: THttpClient;
  uri: TURI;
begin
  // TODO: port
  // TODO: refactor address
  uri := TURI.Create(HostName+'api/'+objectType);
  uri.AddParameter('id', id);
  http := THttpClient.Create;
  try
    try
      Result := http.Get(uri.ToString).StatusCode = 200;
    except
      Result := False;
    end;
  finally
    http.Free;
  end;
end;

class procedure TServerDebugAPI.Post(const api: string; mfd: TMultipartFormData);
var
  http: THttpClient;
begin
  try
    http := THttpClient.Create;
    try
      http.ConnectionTimeout := 100;
      http.Post(HostName+'api/'+api, mfd);
    finally
      http.Free;
    end;
  except
    ;
  end;
end;

//------------------------------------------------------------------------------
// Font API wrappers
//------------------------------------------------------------------------------

class function TServerDebugAPI.IsFontRegistered(
  const Filename: string): Boolean;
begin
  Result := IsDebugObjectRegistered('font', ExtractFileName(Filename));
end;

class procedure TServerDebugAPI.RegisterFont(const Filename,
  Facename: string);
var
  mfd: TMultipartFormData;
begin
  mfd := TMultipartFormData.Create(True);
  try
    mfd.AddField('id', Facename);
    mfd.AddFile('file', Filename);
    Post('font/register', mfd);
  finally
    mfd.Free;
  end;
end;

class procedure TServerDebugAPI.RegisterFont(Filedata: TStream;
  const Facename: string);
var
  mfd: TMultipartFormData;
begin
  mfd := TMultipartFormData.Create(True);
  try
    mfd.AddField('id', Facename);
    mfd.AddStream('file', Filedata, 'myfile.ttf', 'application/octet-stream');
    Post('font/register', mfd);
  finally
    mfd.Free;
  end;
end;

//------------------------------------------------------------------------------
// Keyboard API wrappers
//------------------------------------------------------------------------------

class function TServerDebugAPI.IsKeyboardRegistered(
  const Filename: string): Boolean;
begin
  Result := IsDebugObjectRegistered('keyboard', TKeyboardUtils.KeyboardFileNameToID(Filename));
end;

class procedure TServerDebugAPI.RegisterKeyboard(const Filename, Version,
  FontFace, OskFontFace: string);
var
  mfd: TMultipartFormData;
begin
  mfd := TMultipartFormData.Create(True);
  try
    mfd.AddField('id', TKeyboardUtils.KeyboardFileNameToID(Filename));
    if FontFace <> '' then mfd.AddField('fontFace', FontFace);
    if OskFontFace <> '' then mfd.AddField('oskFontFace', OskFontFace);
    mfd.AddFile('file', Filename);
    Post('keyboard/register', mfd);
  finally
    mfd.Free;
  end;
end;

//------------------------------------------------------------------------------
// Model API wrappers
//------------------------------------------------------------------------------

class function TServerDebugAPI.IsModelRegistered(
  const Filename: string): Boolean;
begin
  Result := IsDebugObjectRegistered('model', TLexicalModelUtils.LexicalModelFileNameToID(Filename));
end;

class procedure TServerDebugAPI.RegisterModel(const Filename: string);
var
  mfd: TMultipartFormData;
begin
  mfd := TMultipartFormData.Create(True);
  try
    mfd.AddField('id', TLexicalModelUtils.LexicalModelFileNameToID(Filename));
    mfd.AddFile('file', Filename);
    Post('model/register', mfd);
  finally
    mfd.Free;
  end;
end;

//------------------------------------------------------------------------------
// Package API wrappers
//------------------------------------------------------------------------------

class function TServerDebugAPI.IsPackageRegistered(
  const Filename: string): Boolean;
begin
  Result := IsDebugObjectRegistered('package', TKeyboardUtils.KeyboardFileNameToID(Filename));
end;

class procedure TServerDebugAPI.RegisterPackage(const Filename,
  Name: string);
var
  mfd: TMultipartFormData;
begin
  mfd := TMultipartFormData.Create(True);
  try
    mfd.AddField('id', TKeyboardUtils.KeyboardFileNameToID(Filename));
    mfd.AddField('name', Name);
    mfd.AddFile('file', Filename);
    Post('package/register', mfd);
  finally
    mfd.Free;
  end;
end;

end.
