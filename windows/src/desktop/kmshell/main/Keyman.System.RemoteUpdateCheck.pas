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
    FDownload: TRemoteUpdateCheckDownloadParams;
    FCheckOnly: Boolean;

    function DownloadUpdates(Params: TUpdateCheckResponse) : Boolean;
    procedure DoDownloadUpdates(SavePath: string; Params: TUpdateCheckResponse;  var Result: Boolean);
    function DoRun: TRemoteUpdateCheckResult;
  public

    constructor Create(AForce : Boolean; ACheckOnly: Boolean = False);
    destructor Destroy; override;
    function Run: TRemoteUpdateCheckResult;
    property ShowErrors: Boolean read FShowErrors write FShowErrors;
  end;

procedure LogMessage(LogMessage: string);

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

constructor TRemoteUpdateCheck.Create(AForce, ACheckOnly: Boolean);
begin
  inherited Create;

  FShowErrors := True;
  FRemoteResult := wucUnknown;

  FForce := AForce;
  FCheckOnly := ACheckOnly;

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

  if Result in [ wucSuccess] then
  begin
    kmcom.Keyboards.Refresh;
    kmcom.Keyboards.Apply;
    kmcom.Packages.Refresh;
  end;

  FRemoteResult := Result;
end;


procedure TRemoteUpdateCheck.DoDownloadUpdates(SavePath: string; Params: TUpdateCheckResponse; var Result: Boolean);
var
  i, downloadCount: Integer;
  http: THttpUploader;
  fs: TFileStream;

    function DownloadFile(const url, savepath: string): Boolean;
    begin
      try
        http := THttpUploader.Create(nil);
        try
          http.Proxy.Server := GetProxySettings.Server;
          http.Proxy.Port := GetProxySettings.Port;
          http.Proxy.Username := GetProxySettings.Username;
          http.Proxy.Password := GetProxySettings.Password;
          http.Request.Agent := API_UserAgent;

          http.Request.SetURL(url);
          http.Upload;
          if http.Response.StatusCode = 200 then
          begin
            fs := TFileStream.Create(savepath, fmCreate);
            try
              fs.Write(http.Response.PMessageBody^, http.Response.MessageBodyLength);
            finally
              fs.Free;
            end;
            Result := True;
          end
          else // I2742
            // If it fails we set to false but will try the other files
            Result := False;
            Exit;
        finally
          http.Free;
        end;
      except
        on E:EHTTPUploader do
        begin
          if (E.ErrorCode = 12007) or (E.ErrorCode = 12029)
            then LogMessage(S_OnlineUpdate_UnableToContact)
            else LogMessage(WideFormat(S_OnlineUpdate_UnableToContact_Error, [E.Message]));
          Result := False;
        end;
      end;
    end;

begin
  Result := False;

  FDownload.TotalSize := 0;
  FDownload.TotalDownloads := 0;
  downloadCount := 0;

  // Keyboard Packages
  for i := 0 to High(Params.Packages) do
  begin
    Inc(FDownload.TotalDownloads);
    Inc(FDownload.TotalSize, Params.Packages[i].DownloadSize);
    Params.Packages[i].SavePath := SavePath + Params.Packages[i].FileName;
  end;

  // Add the Keyman installer
  Inc(FDownload.TotalDownloads);
  Inc(FDownload.TotalSize, Params.InstallSize);

  // Keyboard Packages
  FDownload.StartPosition := 0;
  for i := 0 to High(Params.Packages) do
    begin
      if not DownloadFile(Params.Packages[i].DownloadURL, Params.Packages[i].SavePath) then // I2742
      begin
        Params.Packages[i].Install := False; // Download failed but install other files
      end
      else
        Inc(downloadCount);
      FDownload.StartPosition := FDownload.StartPosition + Params.Packages[i].DownloadSize;
    end;

  // Keyman Installer
  if not DownloadFile(Params.InstallURL, SavePath + Params.FileName) then  // I2742
  begin
    // TODO: #10210record fail? and log  // Download failed but user wants to install other files
  end
  else
  begin
    Inc(downloadCount)
  end;

  // There needs to be at least one file successfully downloaded to return
  // True that files were downloaded
  if downloadCount > 0 then
    Result := True;
end;

function TRemoteUpdateCheck.DownloadUpdates(Params: TUpdateCheckResponse): Boolean;
var
  DownloadBackGroundSavePath : String;
  DownloadResult : Boolean;
begin
  DownloadBackGroundSavePath := IncludeTrailingPathDelimiter(TKeymanPaths.KeymanUpdateCachePath);

  DoDownloadUpdates(DownloadBackGroundSavePath, Params, DownloadResult);
  KL.Log('TRemoteUpdateCheck.DownloadUpdatesBackground: DownloadResult = '+IntToStr(Ord(DownloadResult)));
  Result := DownloadResult;

end;

function TRemoteUpdateCheck.DoRun: TRemoteUpdateCheckResult;
var
  flags: DWord;
  i: Integer;
  ucr: TUpdateCheckResponse;
  pkg: IKeymanPackage;
  downloadResult: boolean;
  registry: TRegistryErrorControlled;
  http: THttpUploader;
begin
  {FProxyHost := '';
  FProxyPort := 0;}

  { Check if user is currently online }
  if not InternetGetConnectedState(@flags, 0) then
  begin
    Result := wucOffline;
    Exit;
  end;

  { Verify that it has been at least 7 days since last update check }
  try
    registry := TRegistryErrorControlled.Create;  // I2890
    try
      if registry.OpenKeyReadOnly(SRegKey_KeymanDesktop_CU) then
      begin
        if registry.ValueExists(SRegValue_CheckForUpdates) and not registry.ReadBool(SRegValue_CheckForUpdates) and not FForce then
        begin
          Result := wucNoUpdates;
          Exit;
        end;
        if registry.ValueExists(SRegValue_LastUpdateCheckTime) and (Now - registry.ReadDateTime(SRegValue_LastUpdateCheckTime) < 7) and not FForce then
        begin
          Result := wucNoUpdates;
          // TODO: #10210 This exit is just to remove the time check for testing.
          //Exit;
        end;

        {if ValueExists(SRegValue_UpdateCheck_UseProxy) and ReadBool(SRegValue_UpdateCheck_UseProxy) then
        begin
          FProxyHost := ReadString(SRegValue_UpdateCheck_ProxyHost);
          FProxyPort := StrToIntDef(ReadString(SRegValue_UpdateCheck_ProxyPort), 80);
        end;}
      end;
    finally
      registry.Free;
    end;
  except
    { we will not run the check if an error occurs reading the settings }
    on E:Exception do
    begin
      Result := wucFailure;
      FErrorMessage := E.Message;
      Exit;
    end;
  end;

  Result := wucNoUpdates;

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
          //ResponseToParams(ucr);

          if FCheckOnly then
          begin
            TUpdateCheckStorage.SaveUpdateCacheData(ucr);
            Result := FRemoteResult;
          end
          // TODO: ##10210
          // Integerate into state machine. in the download state
          // the process can call LoadUpdateCacheData if needed to get the
          // response result.
          else if (Length(ucr.Packages) > 0) or (ucr.InstallURL <> '') then
          begin
            downloadResult := DownloadUpdates(ucr);
            if DownloadResult then
            begin
              Result := wucSuccess;
            end;
          end;
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

end.
