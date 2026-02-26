unit Keyman.Developer.UI.UfrmServerOptions;

interface

uses
  System.Classes,
  System.Net.HttpClient,
  System.SysUtils,
  System.Variants,
  System.Win.Registry,
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
  TfrmServerOptions = class(TTikeForm)
    cmdOK: TButton;
    cmdCancel: TButton;
    gbSetup: TGroupBox;
    editAuthToken: TEdit;
    lblAuthToken: TLabel;
    cmdDownload: TButton;
    gbAdvanced: TGroupBox;
    chkServerShowConsoleWindow: TCheckBox;
    cmdCreateAccount: TButton;
    lblNgrokInstallState: TLabel;
    Label1: TLabel;
    lblGetToken: TLabel;
    lblDefaultPort: TLabel;
    editDefaultPort: TEdit;
    chkUseNgrok: TCheckBox;
    chkLeaveServerRunning: TCheckBox;
    cmdInstallVCRedist: TButton;
    lblVCRedistInstallState: TLabel;
    procedure FormCreate(Sender: TObject);
    procedure cmdOKClick(Sender: TObject);
    procedure cmdDownloadClick(Sender: TObject);
    procedure cmdCreateAccountClick(Sender: TObject);
    procedure lblGetTokenClick(Sender: TObject);
    procedure cmdInstallVCRedistClick(Sender: TObject);
  private
    FInstallStateChanged: Boolean;
    FDownloadProgress: TfrmDownloadProgress;
    FDownloadObject: string;
    procedure UpdateVersionLabel;
    function NgrokPath: string;
    procedure DownloadNgrok(AOwner: TfrmDownloadProgress; var Result: Boolean);
    procedure HttpReceiveData(const Sender: TObject; AContentLength,
      AReadCount: Int64; var Abort: Boolean);
    procedure DownloadVCRedist(AOwner: TfrmDownloadProgress;
      var Result: Boolean);
    function DoDownload(AOwner: TfrmDownloadProgress; const objectName, url: string): IHTTPResponse;
    function RedistInstallerPath: string;
  public
  end;

implementation

uses
  Winapi.ShlObj,

  KeymanDeveloperOptions,
  Keyman.Developer.System.ServerAPI,
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

  // Note, we manually update the version of ngrok as required in subsequent
  // releases, rather than trying to handle a potentially moving target
  SNgrokVersion = 'v1.7.0';
  SUrlNgrokDownload = 'https://github.com/ngrok/ngrok-javascript/releases/download/'+SNgrokVersion+'/ngrok.win32-ia32-msvc.node';
  SNgrokNodeModuleFilename = 'ngrok-win32-ia32-msvc.node';

  // VC++ Redistributable - presence, filename, and permanent URL
  SUrlVCRedistDownload = 'https://aka.ms/vc14/vc_redist.x86.exe';
  SVCRedistExeFilename = 'VC_redist.x86.exe';
  SRegKey_VCRedist = 'SOFTWARE\Microsoft\VisualStudio\14.0\VC\Runtimes\x86';
  SRegValue_VCRedist_Version = 'Version';

procedure TfrmServerOptions.cmdCreateAccountClick(Sender: TObject);
begin
  TUtilExecute.URL(SUrlNgrokSignup);
end;

procedure TfrmServerOptions.HttpReceiveData(const Sender: TObject; AContentLength: Int64; AReadCount: Int64; var Abort: Boolean);
begin
  Abort := False;
  if Assigned(FDownloadProgress) then
  begin
    FDownloadProgress.HTTPStatus(nil, 'Downloading '+FDownloadObject, AReadCount, AContentLength);
    FDownloadProgress.HTTPCheckCancel(nil, Abort);
  end;
end;

function TfrmServerOptions.DoDownload(AOwner: TfrmDownloadProgress; const objectName, url: string): IHTTPResponse;
var
  http: THttpClient;
  res: IHTTPResponse;
begin
  Result := nil;

  FDownloadObject := objectName;
  FDownloadProgress := AOwner;
  http := THttpClient.Create;
  try
    http.OnReceiveData := HttpReceiveData;

    try
      res := http.Get(url);
    except
      on E:ENetHTTPClientException do
      begin
        ShowMessage(Format('Unable to download %s from %s: %s', [objectName, url, E.Message]));
        Exit;
      end;
    end;

    if res.StatusCode <> 200 then
    begin
      ShowMessage(Format('Unable to download %s from %s: %d %s',
        [objectName, url, res.StatusCode, res.StatusText]));
      Exit;
    end;

    Result := res;
  finally
    http.Free;
    FDownloadProgress := nil;
    FDownloadObject := '';
  end;
end;

procedure TfrmServerOptions.DownloadVCRedist(AOwner: TfrmDownloadProgress; var Result: Boolean);
var
  res: IHTTPResponse;
  fs: TFileStream;
begin
  Result := False;

  res := DoDownload(AOwner, 'Microsoft Visual C++ Redistributable', SUrlVCRedistDownload);
  if not Assigned(res) then
    Exit;

  // Save the redistributable installer

  try
    fs := TFileStream.Create(RedistInstallerPath, fmCreate);
    try
      fs.CopyFrom(res.ContentStream, 0);
    finally
      fs.Free;
    end;
  except
    on E:Exception do
    begin
      ShowMessage(Format('Unable to save %s: %s', [RedistInstallerPath, E.Message]));
      Exit;
    end;
  end;

  // Execute the installer - user will need to follow prompts to complete installation

  Result := TUtilExecute.WaitForProcess('"'+RedistInstallerPath+'"', ExtractFileDir(RedistInstallerPath), SW_SHOWNORMAL);
  FInstallStateChanged := True;
end;

procedure TfrmServerOptions.DownloadNgrok(AOwner: TfrmDownloadProgress; var Result: Boolean);
var
  res: IHTTPResponse;
  fs: TFileStream;
begin
  Result := False;

  res := DoDownload(AOwner, 'ngrok', SUrlNgrokDownload);
  if not Assigned(res) then
    Exit;

  // Download is a single file, needs to be saved to AppData\Keyman\Keyman Developer\Server\bin\@ngrok\filename

  try
    fs := TFileStream.Create(NgrokPath, fmCreate);
  except
    on E:Exception do
    begin
      ShowMessage(Format('Unable to save %s: %s', [SNgrokNodeModuleFilename, E.Message]));
      Exit;
    end;
  end;
  try
    fs.CopyFrom(res.ContentStream, 0);
  finally
    fs.Free;
  end;

  Result := True;
  FInstallStateChanged := True;
end;

procedure TfrmServerOptions.cmdDownloadClick(Sender: TObject);
var
  DownloadProgress: TfrmDownloadProgress;
begin
  if FKeymanDeveloperOptions.ServerUseNgrok then
  begin
    // We need to stop the server if NGrok has been enabled because the module
    // may be locked and in use
    TServerDebugAPI.StopServer;
  end;

  DownloadProgress := TfrmDownloadProgress.Create(Self);
  try
    DownloadProgress.Callback := DownloadNgrok;
    DownloadProgress.ShowModal;
  finally
    DownloadProgress.Free;
    UpdateVersionLabel;
    if FKeymanDeveloperOptions.ServerUseNgrok then
    begin
      // Restart the server after ngrok module is installed (although ngrok may
      // not run if the dependency is not also installed)
      TServerDebugAPI.StartServer;
    end;
  end;
end;

procedure TfrmServerOptions.cmdInstallVCRedistClick(Sender: TObject);
var
  DownloadProgress: TfrmDownloadProgress;
begin
  DownloadProgress := TfrmDownloadProgress.Create(Self);
  try
    DownloadProgress.Callback := DownloadVCRedist;
    DownloadProgress.ShowModal;
  finally
    DownloadProgress.Free;
    UpdateVersionLabel;
  end;
end;

procedure TfrmServerOptions.cmdOKClick(Sender: TObject);
var
  DefaultPort: Integer;
  KeepAlive, UseNgrok, ngrokKeepVisible: Boolean;
  ngrokToken: string;
  Changed: Boolean;
begin
  DefaultPort := StrToIntDef(editDefaultPort.Text, 8008);   // I4021
  KeepAlive := chkLeaveServerRunning.Checked;
  UseNgrok := chkUseNgrok.Checked;
  ngrokToken := editAuthToken.Text;
  ngrokKeepVisible := chkServerShowConsoleWindow.Checked;

  Changed :=
    (FKeymanDeveloperOptions.ServerDefaultPort <> DefaultPort) or
    (FKeymanDeveloperOptions.ServerKeepAlive <> KeepAlive) or
    (FKeymanDeveloperOptions.ServerUseNgrok <> UseNgrok) or
    (FKeymanDeveloperOptions.ServerNgrokToken <> ngrokToken) or
    (FKeymanDeveloperOptions.ServerServerShowConsoleWindow <> ngrokKeepVisible) or
    FInstallStateChanged;

  if Changed then
  begin
    FKeymanDeveloperOptions.ServerDefaultPort := DefaultPort;
    FKeymanDeveloperOptions.ServerKeepAlive := KeepAlive;
    FKeymanDeveloperOptions.ServerUseNgrok := UseNgrok;
    FKeymanDeveloperOptions.ServerNgrokToken := ngrokToken;
    FKeymanDeveloperOptions.ServerServerShowConsoleWindow := ngrokKeepVisible;
    FKeymanDeveloperOptions.Write; // TODO: Cancel button in parent dialog is a problem
    modWebHttpServer.RestartServer;
  end;
  ModalResult := mrOk;
end;

procedure TfrmServerOptions.FormCreate(Sender: TObject);
begin
  editDefaultPort.Text := IntToStr(FKeymanDeveloperOptions.ServerDefaultPort);   // I4021
  chkUseNgrok.Checked := FKeymanDeveloperOptions.ServerUseNgrok;
  chkLeaveServerRunning.Checked := FKeymanDeveloperOptions.ServerKeepAlive;

  lblNgrokInstallState.Caption := '';
  lblVCRedistInstallState.Caption := '';
  editAuthToken.Text := FKeymanDeveloperOptions.ServerNgrokToken;

  chkServerShowConsoleWindow.Checked := FKeymanDeveloperOptions.ServerServerShowConsoleWindow;

  UpdateVersionLabel;
end;

procedure TfrmServerOptions.lblGetTokenClick(Sender: TObject);
begin
  TUtilExecute.URL(SUrlNgrokAuthToken);
end;


function TfrmServerOptions.NgrokPath: string;
begin
  Result := TServerDebugAPI.ServerBinPath + '\@ngrok';
  ForceDirectories(Result);
  Result := Result + '\' + SNgrokNodeModuleFilename;
end;

function TfrmServerOptions.RedistInstallerPath: string;
begin
  Result := TServerDebugAPI.ServerBinPath + SVCRedistExeFilename;
end;

procedure TfrmServerOptions.UpdateVersionLabel;
var
  reg: TRegistry;
begin
  if FileExists(NgrokPath)
    then lblNgrokInstallState.Caption := 'Installed'
    else lblNgrokInstallState.Caption := 'Not installed';

  lblVCRedistInstallState.Caption := 'Not installed';
  reg := TRegistry.Create;
  try
    reg.RootKey := HKEY_LOCAL_MACHINE;
    if reg.OpenKeyReadOnly(SRegKey_VCRedist) and reg.ValueExists(SRegValue_VCRedist_Version) then
      lblVCRedistInstallState.Caption := reg.ReadString(SRegValue_VCRedist_Version);
  finally
    reg.Free;
  end;
end;

end.
