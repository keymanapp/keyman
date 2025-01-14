(*
 * Keyman is copyright (C) SIL Global. MIT License.
 *)
unit Keyman.System.DownloadUpdate;

interface
uses
  System.Classes,
  System.SysUtils,
  Sentry.Client,
  httpuploader,
  Keyman.System.KeymanSentryClient,
  Keyman.System.UpdateCheckResponse,
  KeymanPaths;

type
  TDownloadUpdateParams = record
    TotalSize: Integer;
    TotalDownloads: Integer;
    StartPosition: Integer;
  end;

  TDownloadUpdate = class
  private
    FShowErrors: Boolean;
    FDownload: TDownloadUpdateParams;
    (**
     *
     * Performs updates download in the background.
     * @params  SavePath  The path where the downloaded files will be saved.
     *
     *@returns  A Boolean value indicating the overall result of the
     *          download process.
     *)
    function DoDownloadUpdates(SavePath: string; Params: TUpdateCheckResponse): Boolean;

  public

    constructor Create;
    destructor Destroy; override;


    function DownloadUpdates : Boolean;
    // TODO: #12888 verify filesizes match the ucr metadata so we know we don't have partial downloads.
    //function VerifyAllFilesDownloaded : Boolean;
    property ShowErrors: Boolean read FShowErrors write FShowErrors;

  end;

implementation


uses
  System.StrUtils,
  System.Types,
  ErrorControlledRegistry,
  GlobalProxySettings,
  keymanapi_TLB,
  KeymanVersion,
  Keyman.System.UpdateCheckStorage,
  KLog,
  kmint,
  OnlineUpdateCheckMessages,
  RegistryKeys,
  Upload_Settings,
  utilkmshell;

procedure ErrorLogMessage(ErrorLogMessage: string);
begin
  KL.Log(ErrorLogMessage);
  TKeymanSentryClient.Client.MessageEvent(Sentry.Client.SENTRY_LEVEL_ERROR,
    ErrorLogMessage);
end;

constructor TDownloadUpdate.Create;
begin
  inherited Create;

  FShowErrors := True;
  KL.Log('TDownloadUpdate.Create');
end;

destructor TDownloadUpdate.Destroy;
begin
  inherited Destroy;
end;

function TDownloadUpdate.DoDownloadUpdates(SavePath: string; Params: TUpdateCheckResponse): Boolean;
var
  i : Integer;
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
          begin
            // If it fails we set to false but will try the other files
            Exit(False);
          end;
        finally
          http.Free;
        end;
      except
        on E:EHTTPUploader do
        begin
          if (E.ErrorCode = 12007) or (E.ErrorCode = 12029)
            then ErrorLogMessage(S_OnlineUpdate_UnableToContact)
            else ErrorLogMessage(WideFormat(S_OnlineUpdate_UnableToContact_Error, [E.Message]));
          Result := False;
        end;
      end;
    end;

begin
  Result := False;

  FDownload.TotalSize := 0;
  FDownload.TotalDownloads := 0;

  // Keyboard Packages
  FDownload.StartPosition := 0;
  for i := 0 to High(Params.Packages) do
  begin
    Inc(FDownload.TotalDownloads);
    Inc(FDownload.TotalSize, Params.Packages[i].DownloadSize);
    Params.Packages[i].SavePath := SavePath + Params.Packages[i].FileName;
    if not DownloadFile(Params.Packages[i].DownloadURL, Params.Packages[i].SavePath) then // I2742
    begin
      Params.Packages[i].Install := False; // Download failed but install other files
    end;
    FDownload.StartPosition := FDownload.StartPosition + Params.Packages[i].DownloadSize;
  end;

  // Keyman Installer
  Inc(FDownload.TotalDownloads);
  Inc(FDownload.TotalSize, Params.InstallSize);
  if not DownloadFile(Params.InstallURL, SavePath + Params.FileName) then  // I2742
  begin
    ErrorLogMessage('DoDownloadUpdates Failed to download' + Params.InstallURL);
  end
  else
  begin
    // If installer has downloaded that is success even
    // if zero packages were downloaded.
    Result := True;
  end;
end;

function TDownloadUpdate.DownloadUpdates: Boolean;
var
  DownloadBackGroundSavePath : String;
  ucr: TUpdateCheckResponse;
begin
  DownloadBackGroundSavePath := IncludeTrailingPathDelimiter(TKeymanPaths.KeymanUpdateCachePath);
  if TUpdateCheckStorage.LoadUpdateCacheData(ucr) then
  begin
    Result := DoDownloadUpdates(DownloadBackGroundSavePath, ucr);
    KL.Log('DownloadUpdates.DownloadUpdates: DownloadResult = '+IntToStr(Ord(Result)));
  end
  else
    Result := False;
end;

end.
