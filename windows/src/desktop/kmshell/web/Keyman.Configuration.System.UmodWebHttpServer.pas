unit Keyman.Configuration.System.UmodWebHttpServer;

interface

uses
  System.SysUtils,
  System.Classes,

  IdBaseComponent,
  IdComponent,
  IdContext,
  IdCustomHTTPServer,
  IdCustomTCPServer,
  IdHTTPServer,

  Keyman.Configuration.System.HttpServer.App,
  Keyman.Configuration.System.HttpServer.SharedData;

type
  TmodWebHttpServer = class(TDataModule)
    http: TIdHTTPServer;
    procedure httpCommandGet(AContext: TIdContext;
      ARequestInfo: TIdHTTPRequestInfo; AResponseInfo: TIdHTTPResponseInfo);
    procedure DataModuleCreate(Sender: TObject);
    procedure DataModuleDestroy(Sender: TObject);
  private
    FSharedData: THttpServerSharedData;
    function GetPort: Integer;
    function GetHost: string;
  public
    property SharedData: THttpServerSharedData read FSharedData;
    property Port: Integer read GetPort;
    property Host: string read GetHost;
  end;

var
  modWebHttpServer: TmodWebHttpServer;

implementation

uses
  Winapi.ActiveX,

  IdException,
  IdGlobal,
  IdGlobalProtocols,
  IdSocketHandle,
  IdStack;

{%CLASSGROUP 'System.Classes.TPersistent'}

{$R *.dfm}

procedure TmodWebHttpServer.DataModuleCreate(Sender: TObject);
var
  b: TIdSocketHandle;
begin
  FSharedData := THttpServerSharedData.Create;
  b := http.Bindings.Add;
  b.Port := 8009; //0;
  b.IP := '127.0.0.1'; // only on local computer
  http.Active := True;
end;

procedure TmodWebHttpServer.DataModuleDestroy(Sender: TObject);
begin
  FreeAndNil(FSharedData);
end;

function TmodWebHttpServer.GetHost: string;
begin
  Result := 'http://'+http.Bindings[0].IP+':'+IntToStr(http.Bindings[0].Port);
end;

function TmodWebHttpServer.GetPort: Integer;
begin
  Result := http.Bindings[0].Port;
end;

procedure TmodWebHttpServer.httpCommandGet(AContext: TIdContext;
  ARequestInfo: TIdHTTPRequestInfo; AResponseInfo: TIdHTTPResponseInfo);
begin
  CoInitializeEx(nil, COINIT_APARTMENTTHREADED);
  try
    TAppHttpResponder.DoProcessRequest(SharedData, AContext, ARequestInfo, AResponseInfo);
  finally
    CoUninitialize;
  end;
end;

end.
