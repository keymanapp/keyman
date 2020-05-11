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

  Keyman.Configuration.System.HttpServer.App;

type
  TmodWebHttpServer = class(TDataModule)
    http: TIdHTTPServer;
    procedure httpCommandGet(AContext: TIdContext;
      ARequestInfo: TIdHTTPRequestInfo; AResponseInfo: TIdHTTPResponseInfo);
    procedure DataModuleCreate(Sender: TObject);
  private
    function GetPort: Integer;
    function GetHost: string;
  public
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
  b := http.Bindings.Add;
  b.Port := 8009; //0;
  b.IP := '127.0.0.1';
  http.Active := True;
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
var
  FApp: TAppHttpResponder;
begin
  CoInitializeEx(nil, COINIT_APARTMENTTHREADED);
  try
    FApp := TAppHttpResponder.Create(AContext, ARequestInfo, AResponseInfo);
    try
      FApp.ProcessRequest;
    finally
      FreeAndNil(FApp);
    end;
  finally
    CoUninitialize;
  end;
end;

end.
