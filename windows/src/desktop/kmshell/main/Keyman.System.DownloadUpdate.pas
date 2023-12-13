(*
  Name:             WebUpdateCheck
  Copyright:        Copyright (C) SIL International.
  Documentation:
  Description:
  Create Date:      5 Dec 2023

  Modified Date:
  Authors:          rcruickshank
  Related Files:
  Dependencies:

  Bugs:
  Todo:
  Notes:
  History:
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
  CheckPeriod: Integer = 7; // Days between checking for updates

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
    FCheckOnly: Boolean;

    function DownloadUpdates(Params: TUpdateCheckResponse) : Boolean;
    procedure DoDownloadUpdates(SavePath: string; Params: TUpdateCheckResponse;  var Result: Boolean);

  public

    constructor Create(AForce : Boolean; ACheckOnly: Boolean = False);
    destructor Destroy; override;
    property ShowErrors: Boolean read FShowErrors write FShowErrors;
  end;

implementation

procedure TDownloadUpdate.DoDownloadUpdates(SavePath: string; Params: TUpdateCheckResponse; var Result: Boolean);
var
  i, downloadCount: Integer;

    function DownloadFile(const url, savepath: string): Boolean;
    begin
      with THttpUploader.Create(nil) do
      try
        Proxy.Server := GetProxySettings.Server;
        Proxy.Port := GetProxySettings.Port;
        Proxy.Username := GetProxySettings.Username;
        Proxy.Password := GetProxySettings.Password;
        Request.Agent := API_UserAgent;

        Request.SetURL(url);
        Upload;
        if Response.StatusCode = 200 then
        begin
          with TFileStream.Create(savepath, fmCreate) do
          try
            Write(Response.PMessageBody^, Response.MessageBodyLength);
          finally
            Free;
          end;
          Result := True;
        end
        else // I2742
          // If it fails we set to false but will try the other files
          Result := False;
          Exit;
      finally
        Free;
      end;
    end;


begin
  Result := False;
  try
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

    // Keyamn Installer
    if not DownloadFile(Params.InstallURL, SavePath + Params.FileName) then  // I2742
    begin
      // TODO record fail? and log  // Download failed but user wants to install other files
    end
    else
    begin
      Inc(downloadCount)
    end;

    // There needs to be at least one file successfully downloaded to return
    // TRUE that files where downloaded
    if downloadCount > 0  then
      Result := True;
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

function TDownloadUpdate.DownloadUpdates(Params: TUpdateCheckResponse): Boolean;
var
  DownloadBackGroundSavePath : String;
  DownloadResult : Boolean;
begin
  DownloadBackGroundSavePath := IncludeTrailingPathDelimiter(TKeymanPaths.KeymanUpdateCachePath);

  DoDownloadUpdates(DownloadBackGroundSavePath, Params, DownloadResult);
  KL.Log('TDownloadUpdate.DownloadUpdatesBackground: DownloadResult = '+IntToStr(Ord(DownloadResult)));
  Result := DownloadResult;

end;


end.
