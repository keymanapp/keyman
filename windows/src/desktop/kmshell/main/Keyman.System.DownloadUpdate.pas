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


  TDownloadUpdate = class
  private
    FShowErrors: Boolean;
    FSuccessfulDownloadCount: Integer;
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
  FSuccessfulDownloadCount := 0;
  // Keyboard Packages
  for i := 0 to High(Params.Packages) do
  begin
    Params.Packages[i].SavePath := SavePath + Params.Packages[i].FileName;
    if DownloadFile(Params.Packages[i].DownloadURL, Params.Packages[i].SavePath) then
    begin
      Inc(FSuccessfulDownloadCount);
    end
    else
    begin
      Params.Packages[i].Install := False; // Download failed but install other files
    end;
  end;

  // Keyman Installer
  //
  if Params.Status = ucrsUpdateReady then
  begin
    if DownloadFile(Params.InstallURL, SavePath + Params.FileName) then
    begin
      Inc(FSuccessfulDownloadCount);
    end
    else
    begin
      ErrorLogMessage('DoDownloadUpdates Failed to download' + Params.InstallURL);
    end;
  end;
  // If there is at least one keyboard package or version of keyman downloaded then
  // the result is true
  if (FSuccessfulDownloadCount > 0) then
  begin
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
