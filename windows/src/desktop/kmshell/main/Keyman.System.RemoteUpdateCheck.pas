{
  * Keyman is copyright (C) SIL International. MIT License.
  *
  * Keyman.System.RemoteUpdateCheck: Checks for keyboard package and Keyman
                                     for Windows updates.
}
unit Keyman.System.RemoteUpdateCheck;  // I3306

interface
uses
  System.Classes,
  System.SysUtils,
  KeymanPaths,
  httpuploader,
  Keyman.System.UpdateCheckResponse,
  OnlineUpdateCheck;

const
  CheckPeriod: Integer = 7; // Days between checking for updates

type
  ERemoteUpdateCheck = class(Exception);

  TRemoteUpdateCheckResult = (wucUnknown, wucSuccess, wucNoUpdates, wucFailure, wucOffline);

  TRemoteUpdateCheckDownloadParams = record
    TotalSize: Integer;
    TotalDownloads: Integer;
    StartPosition: Integer;
  end;

  TRemoteUpdateCheck = class
  private
    FForce: Boolean;
    FRemoteResult: TRemoteUpdateCheckResult;
    FErrorMessage: string;
    FShowErrors: Boolean;
    {
      Performs an online update check, including package retrieval and version
      query.

      This function checks if a week has passed since the last update check. It
      utilizes the kmcom API to retrieve the current packages. The function then
      performs an HTTP request to query the remote versions of these packages.
      The resulting information is stored in the FParams variable. Additionally,
      the function handles the main Keyman install package.

      @returns  A TBackgroundUpdateResult indicating the result of the update
      check.
    }
    function DoRun: TRemoteUpdateCheckResult;
  public

    constructor Create(AForce : Boolean);
    destructor Destroy; override;
    function Run: TRemoteUpdateCheckResult;
    property ShowErrors: Boolean read FShowErrors write FShowErrors;
  end;

procedure LogMessage(LogMessage: string);
function ConfigCheckContinue: Boolean;

implementation

uses
  System.WideStrUtils,
  Winapi.Windows,
  Winapi.WinINet,

  GlobalProxySettings,
  KLog,
  keymanapi_TLB,
  KeymanVersion,
  Keyman.System.UpdateCheckStorage,
  kmint,
  ErrorControlledRegistry,
  RegistryKeys,
  Upload_Settings,

  OnlineUpdateCheckMessages;

{ TRemoteUpdateCheck }

constructor TRemoteUpdateCheck.Create(AForce: Boolean);
begin
  inherited Create;

  FShowErrors := True;
  FRemoteResult := wucUnknown;

  FForce := AForce;

  KL.Log('TRemoteUpdateCheck.Create');
end;

destructor TRemoteUpdateCheck.Destroy;
begin
  if (FErrorMessage <> '') and FShowErrors then
    LogMessage(FErrorMessage);

  KL.Log('TRemoteUpdateCheck.Destroy: FErrorMessage = '+FErrorMessage);
  KL.Log('TRemoteUpdateCheck.Destroy: FRemoteResult = '+IntToStr(Ord(FRemoteResult)));

  inherited Destroy;
end;

function TRemoteUpdateCheck.Run: TRemoteUpdateCheckResult;
begin
  Result := DoRun;
  FRemoteResult := Result;
end;

function TRemoteUpdateCheck.DoRun: TRemoteUpdateCheckResult;
var
  flags: DWord;
  i: Integer;
  ucr: TUpdateCheckResponse;
  pkg: IKeymanPackage;
  registry: TRegistryErrorControlled;
  http: THttpUploader;
  proceed : boolean;
begin
  {FProxyHost := '';
  FProxyPort := 0;}

  { Check if user is currently online }
  if not InternetGetConnectedState(@flags, 0) then
  begin
    Result := wucOffline;
    Exit;
  end;

  proceed := ConfigCheckContinue;
  if not proceed and not FForce then
    begin
      Result :=  wucNoUpdates;
      Exit;
    end;


  try
    http := THTTPUploader.Create(nil);
    try
      http.Fields.Add('version', ansistring(CKeymanVersionInfo.Version));
      http.Fields.Add('tier', ansistring(CKeymanVersionInfo.Tier));
      if FForce
        then http.Fields.Add('manual', '1')
        else http.Fields.Add('manual', '0');

      for i := 0 to kmcom.Packages.Count - 1 do
      begin
        pkg := kmcom.Packages[i];

        // Due to limitations in PHP parsing of query string parameters names with
        // space or period, we need to split the parameters up. The legacy pattern
        // is still supported on the server side. Relates to #4886.
        http.Fields.Add(AnsiString('packageid_'+IntToStr(i)), AnsiString(pkg.ID));
        http.Fields.Add(AnsiString('packageversion_'+IntToStr(i)), AnsiString(pkg.Version));
        pkg := nil;
      end;

      http.Proxy.Server := GetProxySettings.Server;
      http.Proxy.Port := GetProxySettings.Port;
      http.Proxy.Username := GetProxySettings.Username;
      http.Proxy.Password := GetProxySettings.Password;

      http.Request.HostName := API_Server;
      http.Request.Protocol := API_Protocol;
      http.Request.UrlPath := API_Path_UpdateCheck_Windows;
      //OnStatus :=
      http.Upload;
      if http.Response.StatusCode = 200 then
      begin
        if ucr.Parse(http.Response.MessageBodyAsString, 'bundle', CKeymanVersionInfo.Version) then
        begin
          TUpdateCheckStorage.SaveUpdateCacheData(ucr);
          Result := wucSuccess;
        end
        else
        begin
          FErrorMessage := ucr.ErrorMessage;
          Result := wucFailure;
        end;
      end
      else
        raise ERemoteUpdateCheck.Create('Error '+IntToStr(http.Response.StatusCode));
    finally
      http.Free;
    end;
  except
    on E:EHTTPUploader do
    begin
      if (E.ErrorCode = 12007) or (E.ErrorCode = 12029)
        then FErrorMessage := S_OnlineUpdate_UnableToContact
        else FErrorMessage := WideFormat(S_OnlineUpdate_UnableToContact_Error, [E.Message]);
      Result := wucFailure;
    end;
    on E:Exception do
    begin
      FErrorMessage := E.Message;
      Result := wucFailure;
    end;
  end;

  registry := TRegistryErrorControlled.Create;  // I2890
  try
    if registry.OpenKey(SRegKey_KeymanDesktop_CU, True) then
      registry.WriteDateTime(SRegValue_LastUpdateCheckTime, Now);
  finally
    registry.Free;
  end;
end;

 // temp wrapper for converting showmessage to logs don't know where
 // if nt using klog
 procedure LogMessage(LogMessage: string);
 begin
   KL.Log(LogMessage);
 end;

function ConfigCheckContinue: Boolean;
var
  registry: TRegistryErrorControlled;
begin
{ Verify that it has been at least CheckPeriod days since last update check }
  Result := False;
  try
    registry := TRegistryErrorControlled.Create;   // I2890
    try
      if registry.OpenKeyReadOnly(SRegKey_KeymanDesktop_CU) then
      begin
        if registry.ValueExists(SRegValue_CheckForUpdates) and not registry.ReadBool(SRegValue_CheckForUpdates) then
        begin
          Exit;
        end;
        if registry.ValueExists(SRegValue_LastUpdateCheckTime) and (Now - registry.ReadDateTime(SRegValue_LastUpdateCheckTime) > CheckPeriod) then
        begin
          Result := True;
        end;
      end;
    finally
      registry.Free;
    end;
  except
    { we will not run the check if an error occurs reading the settings }
    on E:Exception do
    begin
      Result := False;
      LogMessage(E.Message);
    end;
  end;
end;

end.
