unit Keyman.Configuration.UI.KeymanProtocolHandler;

interface

uses
  System.Classes,

  UfrmDownloadProgress;


type
  TKeymanProtocolHandler = class sealed
  private
    const Regex_KeymanProtocolDownload: string = '^keyman://download/keyboard/([^/\?]+)(?:\?bcp47=(.+))?$';
    var
    FDownloadFilename: string;
    FDownloadURL: string;
    frmDownloadProgress: TfrmDownloadProgress;
    function DoHandle(Owner: TComponent; const url: string; ASilent,
      ANoWelcome: Boolean; const ALogFile: string): Boolean;
    procedure DoDownload(AOwner: TfrmDownloadProgress; var Result: Boolean);
    procedure HttpReceiveData(const Sender: TObject; AContentLength,
      AReadCount: Int64; var Abort: Boolean);
  public
    class function CanHandle(const url: string): Boolean; static;
    class function Handle(Owner: TComponent; const url: string; ASilent, ANoWelcome: Boolean; const ALogFile: string): Boolean; static;
  end;

implementation

uses
  System.Net.HttpClient,
  System.RegularExpressions,
  System.SysUtils,
  Vcl.Controls,
  Winapi.Windows,

  Keyman.Configuration.UI.InstallFile, // TODO: get rid of circular dependency
  UfrmInstallKeyboard,
  Upload_Settings,
  utildir,
  utilhttp,
  utilfiletypes;

{ TKeymanProtocolHandler }

class function TKeymanProtocolHandler.CanHandle(const url: string): Boolean;
begin
  Result := TRegEx.IsMatch(url, Regex_KeymanProtocolDownload);
end;

class function TKeymanProtocolHandler.Handle(Owner: TComponent;
  const url: string; ASilent, ANoWelcome: Boolean;
  const ALogFile: string): Boolean;
var
  h: TKeymanProtocolHandler;
begin
  h := TKeymanProtocolHandler.Create;
  try
    Result := h.DoHandle(Owner, url, ASilent, ANoWelcome, ALogFile);
  finally
    h.Free;
  end;
end;

function TKeymanProtocolHandler.DoHandle(Owner: TComponent;
  const url: string; ASilent, ANoWelcome: Boolean;
  const ALogFile: string): Boolean;
var
  FTempDir: string;
  PackageID, BCP47: string;
  m: TMatch;
begin
  m := TRegEx.Match(url, Regex_KeymanProtocolDownload);
  if not m.Success then
    Exit(False);

  PackageID := URLDecode(m.Groups[1].Value);
  if m.Groups.Count > 2
    then BCP47 := URLDecode(m.Groups[2].Value)
    else BCP47 := '';

  // Download the package

  // TODO: refactor into separate unit together with code from UfrmInstallKeyboardFromWeb
  FTempDir := IncludeTrailingPathDelimiter(CreateTempPath);  // I1679
  try
    FDownloadFilename := FTempDir + PackageID + Ext_PackageFile;
    FDownloadURL := KeymanCom_Protocol_Server + URLPath_PackageDownload(PackageID, BCP47, False);

    frmDownloadProgress := TfrmDownloadProgress.Create(nil);
    try
      frmDownloadProgress.Callback := DoDownload;
      if frmDownloadProgress.ShowModal <> mrOk then
        Exit(False);
    finally
      frmDownloadProgress.Free;
    end;

    // TODO: this makes a circular dependency, refactor it out!
    Result := TInstallFile.Execute(nil, FDownloadFilename, False, False, '', BCP47);

  finally
    if FileExists(FDownloadFilename) then
      System.SysUtils.DeleteFile(FDownloadFilename);
    DeleteTempPath(FTempDir);
  end;
end;

procedure TKeymanProtocolHandler.DoDownload(AOwner: TfrmDownloadProgress; var Result: Boolean);
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
      Client.SecureProtocols := [THTTPSecureProtocol.TLS1, THTTPSecureProtocol.TLS11, THTTPSecureProtocol.TLS12];
      Client.OnReceiveData := HttpReceiveData;

      Stream := TFileStream.Create(FTempFilename, fmCreate);
      try
        try
          Response := Client.Get(FDownloadURL, Stream);
          Result := Response.StatusCode = 200;
        except
          on E:ENetHTTPClientException do
            Result := False;
          on E:ENetHTTPResponseException do
            Result := False;
        end;
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

procedure TKeymanProtocolHandler.HttpReceiveData(const Sender: TObject; AContentLength: Int64; AReadCount: Int64; var Abort: Boolean);
begin
  frmDownloadProgress.HTTPStatus(nil, 'Downloading '+ExtractFileName(FDownloadFilename), AReadCount, AContentLength);
  frmDownloadProgress.HTTPCheckCancel(nil, Abort);
end;

end.
