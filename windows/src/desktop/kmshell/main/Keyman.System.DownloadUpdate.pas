(*
 * Keyman is copyright (C) SIL Global. MIT License.
 *)
unit Keyman.System.DownloadUpdate;

interface
uses
  System.Classes,
  System.SysUtils,
  KeymanPaths,
  httpuploader,
  Keyman.System.UpdateCheckResponse,
  OnlineUpdateCheck;

const
  DaysBetweenCheckingForUpdates: Integer = 7; // Days between checking for updates

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
    FErrorMessage: string;
    {
      Performs updates download in the background, without displaying a GUI
      progress bar.
      @params  SavePath  The path where the downloaded files will be saved.
               Result    A Boolean value indicating the overall result of the
               download process.
    }
    procedure DoDownloadUpdates(SavePath: string; Params: TUpdateCheckResponse;  var Result: Boolean);

  public

    constructor Create;
    destructor Destroy; override;
    {
      Performs updates download in the background, without displaying a GUI
      progress bar. This function is similar to DownloadUpdates, but it runs in
      the background.

      @returns  True  if all updates were successfully downloaded, False if any
      download failed.
    }

    function DownloadUpdates : Boolean;
    function CheckAllFilesDownloaded : Boolean;
    property ShowErrors: Boolean read FShowErrors write FShowErrors;

  end;

implementation


uses
  GlobalProxySettings,
  KLog,
  keymanapi_TLB,
  KeymanVersion,
  Keyman.System.UpdateCheckStorage,
  kmint,
  ErrorControlledRegistry,
  RegistryKeys,
  Upload_Settings,
  OnlineUpdateCheckMessages,
  utilkmshell,
  System.Types,
  System.StrUtils;

 // TODO-WINDOWS-UPDATES: temp wrapper for converting showmessage to logs don't know where
 // if not using klog
 procedure LogMessage(LogMessage: string);
 begin
   KL.Log(LogMessage);
 end;

constructor TDownloadUpdate.Create;
begin
  inherited Create;

  FShowErrors := True;
  KL.Log('TDownloadUpdate.Create');
end;

destructor TDownloadUpdate.Destroy;
begin
  if (FErrorMessage <> '') and FShowErrors then
    LogMessage(FErrorMessage);

  KL.Log('TDownloadUpdate.Destroy: FErrorMessage = '+FErrorMessage);
  inherited Destroy;
end;

procedure TDownloadUpdate.DoDownloadUpdates(SavePath: string; Params: TUpdateCheckResponse; var Result: Boolean);
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
    // TODO-WINDOWS-UPDATES: #10210  convert to error log.
    LogMessage('DoDownloadUpdates Failed to download' + Params.InstallURL);
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

function TDownloadUpdate.DownloadUpdates: Boolean;
var
  DownloadBackGroundSavePath : String;
  DownloadResult : Boolean;
  ucr: TUpdateCheckResponse;
begin
  DownloadBackGroundSavePath := IncludeTrailingPathDelimiter(TKeymanPaths.KeymanUpdateCachePath);
  if TUpdateCheckStorage.LoadUpdateCacheData(ucr) then
  begin
    DoDownloadUpdates(DownloadBackGroundSavePath, ucr, DownloadResult);
    KL.Log('DownloadUpdates.DownloadUpdatesBackground: DownloadResult = '+IntToStr(Ord(DownloadResult)));
    Result := DownloadResult;
  end
  else
    Result := False;
end;

function TDownloadUpdate.CheckAllFilesDownloaded: Boolean;
var
  i : Integer;
  SavedPath : String;
  DownloadResult : Boolean;
  Params: TUpdateCheckResponse;
  VerifyDownloads : TDownloadUpdateParams;
  FileNames : TStringDynArray;

begin
  SavedPath := IncludeTrailingPathDelimiter(TKeymanPaths.KeymanUpdateCachePath);
  GetFileNamesInDirectory(SavedPath, FileNames);
  if Length(FileNames) = 0 then
  begin
    Result := False;
    Exit;
  end;

  if TUpdateCheckStorage.LoadUpdateCacheData(Params) then
  begin
    for i := 0 to High(Params.Packages) do
    begin
      Inc(VerifyDownloads.TotalDownloads);
      Inc(VerifyDownloads.TotalSize, Params.Packages[i].DownloadSize);
      if not MatchStr(Params.Packages[i].FileName, FileNames) then
      begin
        Exit(False);
      end;
      Params.Packages[i].SavePath := SavedPath + Params.Packages[i].FileName;
    end;
    // Add the Keyman installer
    Inc(FDownload.TotalDownloads);
    Inc(FDownload.TotalSize, Params.InstallSize);
    // Check if  the Keyman installer downloaded
    if not MatchStr(Params.FileName, FileNames) then
    begin
      Exit(False);
    end;
    // TODO-WINDOWS-UPDATES: verify filesizes match so we know we don't have partial downloades.
    Result := True;
  end
  else
  begin
    Result := False;
  end;

end;

end.
