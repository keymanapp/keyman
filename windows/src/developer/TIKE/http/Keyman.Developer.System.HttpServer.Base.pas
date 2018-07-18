unit Keyman.Developer.System.HttpServer.Base;

interface

uses
  IdContext,
  IdCustomHTTPServer,
  IdHTTPServer;

type
  TBaseHttpResponder = class
  protected
    procedure RespondFile(const AFileName: string; AContext: TIdContext;
      ARequestInfo: TIdHTTPRequestInfo; AResponseInfo: TIdHTTPResponseInfo);
    procedure Respond404(AContext: TIdContext;
      ARequestInfo: TIdHTTPRequestInfo; AResponseInfo: TIdHTTPResponseInfo);
  end;

implementation

uses
  IdGlobalProtocols,

  System.SysUtils;

{ TBaseHttpResponder }

procedure TBaseHttpResponder.Respond404(
  AContext: TIdContext; ARequestInfo: TIdHTTPRequestInfo;
  AResponseInfo: TIdHTTPResponseInfo);
begin
  AResponseInfo.ResponseNo := 404;
  AResponseInfo.ResponseText := 'File not found';
end;

procedure TBaseHttpResponder.RespondFile(const AFileName: string;
  AContext: TIdContext; ARequestInfo: TIdHTTPRequestInfo;
  AResponseInfo: TIdHTTPResponseInfo);
begin
  // Serve the file

  if not FileExists(AFileName) then
  begin
    AResponseInfo.ResponseNo := 404;
    AResponseInfo.ResponseText := 'File not found';
    Exit;
  end;

  AResponseInfo.ContentType :=  AResponseInfo.HTTPServer.MIMETable.GetFileMIMEType(AFileName);
  AResponseInfo.CharSet := 'UTF-8';
  AResponseInfo.ContentLength := FileSizeByName(AFileName);
//AResponseInfo.LastModified := GetFileDate(doc);
  AResponseInfo.WriteHeader;

  AContext.Connection.IOHandler.WriteFile(AFileName);
end;

end.
