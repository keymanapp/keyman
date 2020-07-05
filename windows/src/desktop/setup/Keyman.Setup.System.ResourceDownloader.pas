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
    frmDownloadProgress: TfrmDownloadProgress;
    function DownloadFile(const ADownloadURL,
      ADownloadFilename: string): Boolean;
    procedure DownloadFileCallback(AOwner: TfrmDownloadProgress;
      var Result: Boolean);
    constructor Create(AInstallInfo: TInstallInfo);
    procedure HttpReceiveData(const Sender: TObject; AContentLength,
      AReadCount: Int64; var Abort: Boolean);
  public
    class function Execute(AInstallInfo: TInstallInfo; ALocation: TInstallInfoFileLocation): Boolean;
  end;

implementation

uses
  System.Classes,
  System.Net.HttpClient,
  System.SysUtils,
  Vcl.Controls,
  Winapi.Windows,

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
  frmDownloadProgress := TfrmDownloadProgress.Create(nil);
  try
    frmDownloadProgress.Caption := 'Downloading '+ExtractFileName(ADownloadFilename); // TODO: localize
    frmDownloadProgress.Callback := DownloadFileCallback;
    Result := frmDownloadProgress.ShowModal = mrOk;
  finally
    frmDownloadProgress.Free;
  end;
end;

procedure TResourceDownloader.HttpReceiveData(const Sender: TObject; AContentLength: Int64; AReadCount: Int64; var Abort: Boolean);
begin
  // TODO: stop using this form and report back to main form instead
  frmDownloadProgress.HTTPStatus(nil, 'Downloading '+ExtractFileName(FDownloadFilename), AReadCount, AContentLength);
  frmDownloadProgress.HTTPCheckCancel(nil, Abort);
end;

procedure TResourceDownloader.DownloadFileCallback(AOwner: TfrmDownloadProgress; var Result: Boolean);
var
  Client: THTTPClient;
  Stream: TStream;
  Response: IHTTPResponse;
  FTempFilename: string;
begin
  Result := False;
  FTempFilename := FDownloadFilename + '.download';
  Client := THTTPClient.Create;
  try
    Client.OnReceiveData := HttpReceiveData;

    Stream := TFileStream.Create(FTempFilename, fmCreate);
    try
      Response := Client.Get(FDownloadURL, Stream);
      Result := Response.StatusCode = 200;
    finally
      Stream.Free;
    end;

    if FileExists(FTempFilename)  then
    begin
      if Result then
      begin
        if FileExists(FDownloadFilename) then
          System.SysUtils.DeleteFile(FDownloadFilename);
        RenameFile(FTempFilename, FDownloadFilename);
      end
      else
        System.SysUtils.DeleteFile(FTempFilename);
    end;
  finally
    Client.Free;
  end;
end;

end.
