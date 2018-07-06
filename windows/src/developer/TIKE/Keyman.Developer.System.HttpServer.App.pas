unit Keyman.Developer.System.HttpServer.App;

interface

uses
  IdContext,
  IdCustomHTTPServer,
  IdHTTPServer;

type
  TAppHttpServer = class
  public
    procedure ProcessRequest(AContext: TIdContext;
      ARequestInfo: TIdHTTPRequestInfo; AResponseInfo: TIdHTTPResponseInfo);
  end;

implementation

{ TAppHttpServer }

procedure TAppHttpServer.ProcessRequest(AContext: TIdContext;
  ARequestInfo: TIdHTTPRequestInfo; AResponseInfo: TIdHTTPResponseInfo);
begin

end;

end.
