unit Keyman.System.HttpServer.Base;

interface

uses
  System.Classes,

  IdContext,
  IdCustomHTTPServer,
  IdHTTPServer;

type
  TBaseHttpResponder = class
  private
  protected
    function IncludesParentFolderReference(const path: string): Boolean;
    procedure RespondFile(AFileName: string; AContext: TIdContext;
      ARequestInfo: TIdHTTPRequestInfo; AResponseInfo: TIdHTTPResponseInfo);
    procedure Respond404(AContext: TIdContext;
      ARequestInfo: TIdHTTPRequestInfo; AResponseInfo: TIdHTTPResponseInfo);
    procedure RespondStream(stream: TStream; const AFileName: string;
      AContext: TIdContext; ARequestInfo: TIdHTTPRequestInfo;
      AResponseInfo: TIdHTTPResponseInfo);
  end;

implementation

uses
  IdGlobalProtocols,

  System.SysUtils;

{ TBaseHttpResponder }

function TBaseHttpResponder.IncludesParentFolderReference(
  const path: string): Boolean;
begin
  Result := path.Contains('../') or path.Contains('..\');
end;

procedure TBaseHttpResponder.Respond404(
  AContext: TIdContext; ARequestInfo: TIdHTTPRequestInfo;
  AResponseInfo: TIdHTTPResponseInfo);
begin
  AResponseInfo.ResponseNo := 404;
  AResponseInfo.ResponseText := 'File not found';
end;

procedure TBaseHttpResponder.RespondFile(AFileName: string;
  AContext: TIdContext; ARequestInfo: TIdHTTPRequestInfo;
  AResponseInfo: TIdHTTPResponseInfo);
begin
  // Serve the file

  if DirectoryExists(AFileName) then
  begin
    AFileName := IncludeTrailingPathDelimiter(AFileName) + 'index.html';
  end;

  if not FileExists(AFileName) then
  begin
    AResponseInfo.ResponseNo := 404;
    AResponseInfo.ResponseText := 'File not found';
    Exit;
  end;

  if AResponseInfo.ContentType = '' then
  begin
    AResponseInfo.HTTPServer.MIMETable.LoadTypesFromOS := False;
    AResponseInfo.ContentType := AResponseInfo.HTTPServer.MIMETable.GetFileMIMEType(AFileName);
  end;

  AResponseInfo.CharSet := 'UTF-8';
  AResponseInfo.ContentLength := FileSizeByName(AFileName);
//AResponseInfo.LastModified := GetFileDate(doc);
  AResponseInfo.WriteHeader;

  AContext.Connection.IOHandler.WriteFile(AFileName);
end;

procedure TBaseHttpResponder.RespondStream(stream: TStream;
  const AFileName: string;
  AContext: TIdContext; ARequestInfo: TIdHTTPRequestInfo;
  AResponseInfo: TIdHTTPResponseInfo);
begin
  // Serve a memory stream

  if not Assigned(stream) then
  begin
    AResponseInfo.ResponseNo := 404;
    AResponseInfo.ResponseText := 'File not found';
    Exit;
  end;

  if AResponseInfo.ContentType = '' then
  begin
    AResponseInfo.HTTPServer.MIMETable.LoadTypesFromOS := False;
    AResponseInfo.ContentType := AResponseInfo.HTTPServer.MIMETable.GetFileMIMEType(AFileName);
  end;

  AResponseInfo.CharSet := 'UTF-8';
  AResponseInfo.ContentLength := stream.Size;
//AResponseInfo.LastModified := GetFileDate(doc);
  AResponseInfo.WriteHeader;

  AContext.Connection.IOHandler.Write(stream);
end;

end.
