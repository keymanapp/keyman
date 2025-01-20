(**
  * Keyman is copyright (C) SIL International. MIT License.
  *
  * Keyman.System.RemoteUpdateCheck: Checks for keyboard package and Keyman
  * for Windows updates.
*)
unit Keyman.System.RemoteUpdateCheck; // I3306

interface

uses
  System.Classes,
  System.SysUtils,
  KeymanPaths,
  httpuploader,
  Keyman.System.UpdateCheckResponse;

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
    (**
     * Performs an online query of both the main keyman package and
     * the keyboard packages. It utilizes the kmcom API to retrieve the current
     * packages. The function then performs an HTTP request to query the remote
     * versions of these packages.
     * The resulting information is stored in the TUpdateCheckResponse
     * variable and seralized to disk.
     *
     * @returns  A TBackgroundUpdateResult indicating the result of the update
     * check.
     *)
    function DoRun: TRemoteUpdateCheckResult;
  public

    constructor Create(AForce: Boolean);
    destructor Destroy; override;
    function Run: TRemoteUpdateCheckResult;
    property ShowErrors: Boolean read FShowErrors write FShowErrors;
  end;

(**
  * This function checks if a week or CheckPeriod time has passed since the last
  * update check.
  *
  * @returns  True if it has been longer then CheckPeriod time since last update
*)
function ConfigCheckContinue: Boolean;

implementation

uses
  System.WideStrUtils,
  System.Win.Registry,
  Winapi.Windows,
  Winapi.WinINet,
  Sentry.Client,

  GlobalProxySettings,
  KLog,
  keymanapi_TLB,
  KeymanVersion,
  Keyman.System.KeymanSentryClient,
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
        TKeymanSentryClient.Client.MessageEvent(Sentry.Client.SENTRY_LEVEL_ERROR,
      '"+FErrorMessage+"');

  KL.Log('TRemoteUpdateCheck.Destroy: FErrorMessage = ' + FErrorMessage);
  KL.Log('TRemoteUpdateCheck.Destroy: FRemoteResult = ' +
    IntToStr(Ord(FRemoteResult)));

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
  Registry: TRegistryErrorControlled;
  http: THttpUploader;
  proceed: Boolean;
begin
  { FProxyHost := '';
    FProxyPort := 0; }

  { Check if user is currently online }
  if not InternetGetConnectedState(@flags, 0) then
  begin
    Result := wucOffline;
    Exit;
  end;

  proceed := ConfigCheckContinue;
  if not proceed and not FForce then
  begin
    Result := wucNoUpdates;
    Exit;
  end;

  try
    http := THttpUploader.Create(nil);
    try
      http.Fields.Add('version', ansistring(CKeymanVersionInfo.Version));
      http.Fields.Add('tier', ansistring(CKeymanVersionInfo.Tier));
      if FForce then
        http.Fields.Add('manual', '1')
      else
        http.Fields.Add('manual', '0');

      for i := 0 to kmcom.Packages.Count - 1 do
      begin
        pkg := kmcom.Packages[i];

        // Due to limitations in PHP parsing of query string parameters names with
        // space or period, we need to split the parameters up. The legacy pattern
        // is still supported on the server side. Relates to #4886.
        http.Fields.Add(ansistring('packageid_' + IntToStr(i)),
          ansistring(pkg.ID));
        http.Fields.Add(ansistring('packageversion_' + IntToStr(i)),
          ansistring(pkg.Version));
        pkg := nil;
      end;

      http.Proxy.Server := GetProxySettings.Server;
      http.Proxy.Port := GetProxySettings.Port;
      http.Proxy.Username := GetProxySettings.Username;
      http.Proxy.Password := GetProxySettings.Password;

      http.Request.HostName := API_Server;
      http.Request.Protocol := API_Protocol;
      http.Request.UrlPath := API_Path_UpdateCheck_Windows;
      // OnStatus :=
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
    on E: EHTTPUploader do
    begin
      if (E.ErrorCode = 12007) or (E.ErrorCode = 12029) then
        FErrorMessage := S_OnlineUpdate_UnableToContact
      else
        FErrorMessage := WideFormat(S_OnlineUpdate_UnableToContact_Error,
          [E.Message]);
      Result := wucFailure;
    end;
    on E: Exception do
    begin
      FErrorMessage := E.Message;
      Result := wucFailure;
    end;
  end;

  Registry := TRegistryErrorControlled.Create; // I2890
  try
    if Registry.OpenKey(SRegKey_KeymanDesktop_CU, True) then
      Registry.WriteDateTime(SRegValue_LastUpdateCheckTime, Now);
  finally
    Registry.Free;
  end;
end;

function ConfigCheckContinue: Boolean;
var
  Registry: TRegistryErrorControlled;
begin
  { Verify that it has been at least CheckPeriod days since last update check }
  Result := False;
  try
    Registry := TRegistryErrorControlled.Create; // I2890
    try
      if Registry.OpenKeyReadOnly(SRegKey_KeymanDesktop_CU) then
      begin
        if Registry.ValueExists(SRegValue_CheckForUpdates) and
          not Registry.ReadBool(SRegValue_CheckForUpdates) then
        begin
          Exit;
        end;
        if Registry.ValueExists(SRegValue_LastUpdateCheckTime) and
          (Now - Registry.ReadDateTime(SRegValue_LastUpdateCheckTime) >
          CheckPeriod) then
        begin
          Result := True;
        end;
      end;
    finally
      Registry.Free;
    end;
  except
    on E: ERegistryException do
    begin
      Result := False;
          TKeymanSentryClient.Client.MessageEvent(Sentry.Client.SENTRY_LEVEL_ERROR,
      E.Message);
    end;
  end;
end;

end.
