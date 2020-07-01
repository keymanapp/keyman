unit Keyman.Setup.System.ResourceDownloader;

interface

uses
  Keyman.Setup.System.InstallInfo,
  UfrmDownloadProgress;

type
  TResourceDownloader = class sealed
  private
    FDownloadURL: string;
    FDownloadFilename: string;
    FInstallInfo: TInstallInfo;
    function DownloadFile(const ADownloadURL,
      ADownloadFilename: string): Boolean;
    procedure DownloadFileCallback(AOwner: TfrmDownloadProgress;
      var Result: Boolean);
    constructor Create(AInstallInfo: TInstallInfo);
  public
    class function Execute(AInstallInfo: TInstallInfo; ALocation: TInstallInfoFileLocation): Boolean;
  end;

implementation

uses
  System.Classes,
  System.SysUtils,
  Vcl.Controls,

  httpuploader,
  RunTools,
  SetupStrings,
  Upload_Settings;


{ TResourceDownloader }

{**
 * After installation, location will have its properties set to iilLocal;
 * the file is downloaded into TempPath, so will be removed post install.
 *}
class function TResourceDownloader.Execute(AInstallInfo: TInstallInfo; ALocation: TInstallInfoFileLocation): Boolean;
var
  rd: TResourceDownloader;
begin
  rd := TResourceDownloader.Create(AInstallInfo);
  try
    Result := rd.DownloadFile(ALocation.Url, AInstallInfo.TempPath + ALocation.Path);
    if Result then
    begin
      ALocation.UpgradeToLocalPath(AInstallInfo.TempPath);
    end;
  finally
    rd.Free;
  end;
end;

constructor TResourceDownloader.Create(AInstallInfo: TInstallInfo);
begin
  inherited Create;
  FInstallInfo := AInstallInfo;
end;

function TResourceDownloader.DownloadFile(const ADownloadURL, ADownloadFilename: String): Boolean;
begin
  FDownloadURL := ADownloadURL;
  FDownloadFilename := ADownloadFilename;

  { Download the redistributable }
  with TfrmDownloadProgress.Create(nil) do
  try
    Caption := 'Downloading '+ExtractFileName(ADownloadFilename); // TODO: localize
    Callback := DownloadFileCallback;
    Result := ShowModal = mrOk;
  finally
    Free;
  end;
end;

procedure TResourceDownloader.DownloadFileCallback(AOwner: TfrmDownloadProgress; var Result: Boolean);
begin
  Result := False;
  try
    with THTTPUploader.Create(AOwner) do
    try
      OnCheckCancel := AOwner.HTTPCheckCancel;
      OnStatus := AOwner.HTTPStatus;
      Request.Agent := API_UserAgent;
      //Request.Protocol := Upload_Protocol;
      //Request.HostName := Upload_Server;
      Request.SetURL(FDownloadURL);// UrlPath := URL;
      Upload;
      if Response.StatusCode = 200 then
      begin
        with TFileStream.Create(FDownloadFilename, fmCreate) do  // I3476
        try
          Write(Response.PMessageBody^, Response.MessageBodyLength);
        finally
          Free;
        end;
        Result := True;
      end
      else
        // TODO: Deal with recursive dependency
        GetRunTools.LogError(FInstallInfo.Text(ssErrorDownloadingUpdate, [Response.StatusCode]));
    finally
      Free;
    end;
  except
    on E:EHTTPUploader do
    begin
      // TODO: Deal with recursive dependency
      if (E.ErrorCode = 12007) or (E.ErrorCode = 12029)
        then GetRunTools.LogError(FInstallInfo.Text(ssErrorUnableToContactServer))
        else GetRunTools.LogError(FInstallInfo.Text(ssErrorUnableToContactServerDetailed, [E.Message]));
      Result := False;
    end;
  end;
end;

end.
