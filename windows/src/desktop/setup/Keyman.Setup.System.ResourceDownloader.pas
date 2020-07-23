{**
 * Wrapper for THttpClient for downloading resources with progress.
 *}
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
    FSilent: Boolean;
    function DownloadFile(const ADownloadURL, ADownloadFilename: string): Boolean;
    procedure DownloadFileCallback(AOwner: TfrmDownloadProgress; var Result: Boolean);
    constructor Create(AInstallInfo: TInstallInfo; ASilent: Boolean);
  public
    class function Execute(AInstallInfo: TInstallInfo; ALocation: TInstallInfoFileLocation; ASilent: Boolean): Boolean;
  end;

implementation

uses
  System.Classes,
  System.SysUtils,
  Vcl.Controls,
  Winapi.Windows,

  httpuploader,
  SetupStrings,
  Upload_Settings;


{ TResourceDownloader }

{**
 * After installation, location will have its properties set to iilLocal;
 * the file is downloaded into TempPath, so will be removed post install.
 *}
class function TResourceDownloader.Execute(AInstallInfo: TInstallInfo; ALocation: TInstallInfoFileLocation; ASilent: Boolean): Boolean;
var
  rd: TResourceDownloader;
begin
  rd := TResourceDownloader.Create(AInstallInfo, ASilent);
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

constructor TResourceDownloader.Create(AInstallInfo: TInstallInfo; ASilent: Boolean);
begin
  inherited Create;
  FInstallInfo := AInstallInfo;
  FSilent := ASilent;
end;

function TResourceDownloader.DownloadFile(const ADownloadURL, ADownloadFilename: String): Boolean;
var
  frmDownloadProgress: TfrmDownloadProgress;
begin
  FDownloadURL := ADownloadURL;
  FDownloadFilename := ADownloadFilename;

  if FSilent then
    DownloadFileCallback(nil, Result)
  else
  begin
    { Download the redistributable }
    frmDownloadProgress := TfrmDownloadProgress.Create(nil);
    try
      frmDownloadProgress.Caption := FInstallInfo.Text(ssDownloadingTitle, [ExtractFileName(ADownloadFilename)]);
      frmDownloadProgress.Callback := DownloadFileCallback;
      Result := frmDownloadProgress.ShowModal = mrOk;
    finally
      frmDownloadProgress.Free;
    end;
  end;
end;

procedure TResourceDownloader.DownloadFileCallback(AOwner: TfrmDownloadProgress; var Result: Boolean);
var
  http: THTTPUploader;
  Stream: TStream;
  FTempFilename: string;
begin
  Result := False;
  FTempFilename := FDownloadFilename + '.download';
  http := THTTPUploader.Create(nil);
  try
    http.ShowUI := not FSilent;
    if Assigned(AOwner) then
    begin
      http.OnStatus := AOwner.HTTPStatus;
      http.OnCheckCancel := AOwner.HTTPCheckCancel;
    end;
    http.Request.SetURL(FDownloadURL);
    http.Upload;
    Result := http.Response.StatusCode = 200;
    if Result then
    begin
      Stream := TFileStream.Create(FTempFilename, fmCreate);
      try
        Stream.Write(http.Response.PMessageBody^, http.Response.MessageBodyLength);
      finally
        Stream.Free;
      end;
    end;

    if FileExists(FTempFilename) then
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
    http.Free;
  end;
end;

end.
