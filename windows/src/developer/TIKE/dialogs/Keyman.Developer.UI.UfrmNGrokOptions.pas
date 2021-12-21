unit Keyman.Developer.UI.UfrmNGrokOptions;

interface

uses
  System.Classes,
  System.SysUtils,
  System.Variants,
  Vcl.Controls,
  Vcl.Dialogs,
  Vcl.Forms,
  Vcl.Graphics,
  Vcl.StdCtrls,
  Winapi.Messages,
  Winapi.Windows,

  UfrmDownloadProgress,
  UfrmTike;

type
  TfrmNgrokOptions = class(TTikeForm)
    cmdOK: TButton;
    cmdCancel: TButton;
    gbSetup: TGroupBox;
    editAuthToken: TEdit;
    lblAuthToken: TLabel;
    cbRegion: TComboBox;
    lblRegion: TLabel;
    cmdDownload: TButton;
    gbAdvanced: TGroupBox;
    chkKeepNGrokControlWindowVisible: TCheckBox;
    editControlPort: TEdit;
    lblControlPort: TLabel;
    cmdCreateAccount: TButton;
    lblVersion: TLabel;
    Label1: TLabel;
    lblGetToken: TLabel;
    lblWebHostDefaultPort: TLabel;
    editWebHostDefaultPort: TEdit;
    chkUseNgrok: TCheckBox;
    chkLeaveServerRunning: TCheckBox;
    procedure FormCreate(Sender: TObject);
    procedure cmdOKClick(Sender: TObject);
    procedure cmdDownloadClick(Sender: TObject);
    procedure cmdCreateAccountClick(Sender: TObject);
    procedure lblGetTokenClick(Sender: TObject);
  private
    FDownloadProgress: TfrmDownloadProgress;
    procedure UpdateVersionLabel;
    function NgrokPath: string;
    procedure DownloadNgrok(AOwner: TfrmDownloadProgress; var Result: Boolean);
    procedure HttpReceiveData(const Sender: TObject; AContentLength,
      AReadCount: Int64; var Abort: Boolean);
    { Private declarations }
  public
    { Public declarations }
  end;

implementation

uses
  System.Net.HttpClient,
  System.Zip,
  Winapi.ShlObj,

  KeymanDeveloperOptions,
  Keyman.Developer.System.KMDevServerAPI,
  KeymanPaths,
  RegistryKeys,
  UmodWebHttpServer,
  utilexecute;

{$R *.dfm}

const
  // These URLs come from ngrok documentation and ngrok source. They appear
  // to be stable, as they are used in other apps also.
  SUrlNgrokAuthToken = 'https://dashboard.ngrok.com/get-started/your-authtoken';
  SUrlNgrokSignup = 'https://dashboard.ngrok.com/signup';
  SUrlNgrokDownload = 'https://bin.equinox.io/c/4VmDzA7iaHb/ngrok-stable-windows-386.zip';

procedure TfrmNgrokOptions.cmdCreateAccountClick(Sender: TObject);
begin
  TUtilExecute.URL(SUrlNgrokSignup);
end;

procedure TfrmNgrokOptions.HttpReceiveData(const Sender: TObject; AContentLength: Int64; AReadCount: Int64; var Abort: Boolean);
begin
  Abort := False;
  if Assigned(FDownloadProgress) then
  begin
    FDownloadProgress.HTTPStatus(nil, 'Downloading ngrok', AReadCount, AContentLength);
    FDownloadProgress.HTTPCheckCancel(nil, Abort);
  end;
end;

procedure TfrmNgrokOptions.DownloadNgrok(AOwner: TfrmDownloadProgress; var Result: Boolean);
var
  http: THttpClient;
  res: IHTTPResponse;
  fs: TFileStream;
  zip: TZipFile;
  ms: TStream;
  zh: TZipHeader;
begin
  Result := False;

  FDownloadProgress := AOwner;
  http := THttpClient.Create;
  try
    http.OnReceiveData := HttpReceiveData;

    try
      res := http.Get(SUrlNgrokDownload);
    except
      on E:ENetHTTPClientException do
      begin
        ShowMessage(Format('Unable to download ngrok.exe: %s', [E.Message]));
        Exit;
      end;
    end;

    if res.StatusCode <> 200 then
    begin
      ShowMessage(Format('Unable to download ngrok.exe: %d %s',
        [res.StatusCode, res.StatusText]));
      Exit;
    end;

    // Download is a zip file with a single executable, so let's extract it
    ms := nil;
    try
      zip := TZipFile.Create;
      try
        try
          zip.Open(res.ContentStream, zmRead);
          zip.Read('ngrok.exe', ms, zh);
          ms.Position := 0;
        finally
          zip.Free;
        end;
      except
        on E:Exception do
        begin
          ShowMessage(Format('Unable to extract ngrok.exe from downloaded archive: %s',
            [E.Message]));
          Exit;
        end;
      end;
      try
        fs := TFileStream.Create(NgrokPath, fmCreate);
      except
        on E:Exception do
        begin
          ShowMessage(Format('Unable to save ngrok.exe: %s', [E.Message]));
          Exit;
        end;
      end;
      try
        fs.CopyFrom(ms, 0);
      finally
        fs.Free;
      end;
    finally
      ms.Free;
    end;
  finally
    http.Free;
    FDownloadProgress := nil;
  end;
end;

procedure TfrmNgrokOptions.cmdDownloadClick(Sender: TObject);
var
  DownloadProgress: TfrmDownloadProgress;
begin
  TKMDevServerDebugAPI.StopServer;

  try
    DownloadProgress := TfrmDownloadProgress.Create(Self);
    try
      DownloadProgress.Callback := DownloadNgrok;
      DownloadProgress.ShowModal;
    finally
      DownloadProgress.Free;
    end;
  finally
    UpdateVersionLabel;
    TKMDevServerDebugAPI.StartServer;
  end;
end;

procedure TfrmNgrokOptions.cmdOKClick(Sender: TObject);
var
  DefaultPort, ngrokControlPort: Integer;
  KeepAlive, UseNgrok, ngrokKeepVisible: Boolean;
  ngrokToken, ngrokRegion: string;
  Changed: Boolean;
begin
  DefaultPort := StrToIntDef(editWebHostDefaultPort.Text, 8008);   // I4021
  KeepAlive := chkLeaveServerRunning.Checked;
  UseNgrok := chkUseNgrok.Checked;
  ngrokControlPort := StrToIntDef(editControlPort.Text, 8009);
  ngrokToken := editAuthToken.Text;
  ngrokRegion := Copy(cbRegion.Text, 1, 2);
  ngrokKeepVisible := chkKeepNGrokControlWindowVisible.Checked;

  Changed :=
    (FKeymanDeveloperOptions.WebHostDefaultPort <> DefaultPort) or
    (FKeymanDeveloperOptions.WebHostKeepAlive <> KeepAlive) or
    (FKeymanDeveloperOptions.WebHostUseNgrok <> UseNgrok) or
    (FKeymanDeveloperOptions.WebHostNGrokControlPort <> ngrokControlPort) or
    (FKeymanDeveloperOptions.WebHostNGrokToken <> ngrokToken) or
    (FKeymanDeveloperOptions.WebHostNGrokRegion <> ngrokRegion) or
    (FKeymanDeveloperOptions.WebHostKeepNGrokControlWindowVisible <> ngrokKeepVisible);

  if Changed then
  begin
    FKeymanDeveloperOptions.WebHostDefaultPort := DefaultPort;
    FKeymanDeveloperOptions.WebHostKeepAlive := KeepAlive;
    FKeymanDeveloperOptions.WebHostUseNgrok := UseNgrok;
    FKeymanDeveloperOptions.WebHostNGrokControlPort := ngrokControlPort;
    FKeymanDeveloperOptions.WebHostNGrokToken := ngrokToken;
    FKeymanDeveloperOptions.WebHostNGrokRegion := ngrokRegion;
    FKeymanDeveloperOptions.WebHostKeepNGrokControlWindowVisible := ngrokKeepVisible;
    FKeymanDeveloperOptions.Write; // TODO: Cancel button in parent dialog is a problem
    modWebHttpServer.RestartServer;
  end;
  ModalResult := mrOk;
end;

procedure TfrmNgrokOptions.FormCreate(Sender: TObject);
var
  i: Integer;
begin
  editWebHostDefaultPort.Text := IntToStr(FKeymanDeveloperOptions.WebHostDefaultPort);   // I4021
  chkUseNgrok.Checked := FKeymanDeveloperOptions.WebHostUseNgrok;
  chkLeaveServerRunning.Checked := FKeymanDeveloperOptions.WebHostKeepAlive;

  lblVersion.Caption := '';
  editAuthToken.Text := FKeymanDeveloperOptions.WebHostNGrokToken;

  cbRegion.ItemIndex := 0;
  for i := 0 to cbRegion.Items.Count - 1 do
    if cbRegion.Items[i].StartsWith(FKeymanDeveloperOptions.WebHostNGrokRegion) then
    begin
      cbRegion.ItemIndex := i;
      Break;
    end;

  chkKeepNGrokControlWindowVisible.Checked := FKeymanDeveloperOptions.WebHostKeepNGrokControlWindowVisible;
  editControlPort.Text := IntToStr(FKeymanDeveloperOptions.WebHostNGrokControlPort);

  UpdateVersionLabel;
end;

procedure TfrmNgrokOptions.lblGetTokenClick(Sender: TObject);
begin
  TUtilExecute.URL(SUrlNgrokAuthToken);
end;

function TfrmNgrokOptions.NgrokPath: string;
begin
  Result := GetFolderPath(CSIDL_APPDATA) + SFolderKeymanDeveloper + '\ngrok.exe';
end;

procedure TfrmNgrokOptions.UpdateVersionLabel;
var
  logtext: string;
  ec: Integer;
begin
  if not FileExists(NgrokPath) then
    lblVersion.Caption := 'Ngrok not installed'
  else if TUtilExecute.Console(Format('"%s" --version', [NgrokPath]), ExtractFilePath(ParamStr(0)),
    logtext, ec) then
  begin
    if ec = 0
      then lblVersion.Caption := logtext.Trim
      else lblVersion.Caption := Format('Error %d running ngrok: %s', [ec, logtext.Trim])
  end
  else
    lblVersion.Caption := Format('Unable to run ngrok: %s', [SysErrorMessage(GetLastError)]);
end;

end.
