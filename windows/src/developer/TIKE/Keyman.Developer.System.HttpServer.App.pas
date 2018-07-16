unit Keyman.Developer.System.HttpServer.App;

interface

uses
  IdContext,
  IdCustomHTTPServer,
  IdHTTPServer,

  Keyman.Developer.System.HttpServer.Base;

type
  TAppHttpResponder = class(TBaseHttpResponder)
  private
    FStandardTemplatePath: string;
    procedure RespondProject(doc: string; AContext: TIdContext;
      ARequestInfo: TIdHTTPRequestInfo; AResponseInfo: TIdHTTPResponseInfo);
    procedure RespondHelp(doc: string; AContext: TIdContext;
      ARequestInfo: TIdHTTPRequestInfo; AResponseInfo: TIdHTTPResponseInfo);
  public
    procedure ProcessRequest(AContext: TIdContext;
      ARequestInfo: TIdHTTPRequestInfo; AResponseInfo: TIdHTTPResponseInfo);
  end;

implementation

uses
  System.Classes,
  System.SysUtils,
  Xml.XMLDoc,
  Xml.XMLIntf,

  ProjectFile,
  ProjectSaver,
  RedistFiles;

{ TAppHttpServer }

procedure TAppHttpResponder.RespondHelp(doc: string; AContext: TIdContext;
  ARequestInfo: TIdHTTPRequestInfo; AResponseInfo: TIdHTTPResponseInfo);
var
  xml, xsl: IXMLDocument;
  s: WideString;
begin

  if FStandardTemplatePath = '' then
  begin
    FStandardTemplatePath := ExtractFilePath(ParamStr(0)) + 'locale\' + 'en';
    // I2595

    if FileExists(FStandardTemplatePath + '\xml\help\contexthelp.xml') then
      FStandardTemplatePath := FStandardTemplatePath + '\xml\help\'
    else
    begin
      FStandardTemplatePath := ExtractFilePath(ParamStr(0)) + 'locale\' + 'en';
      // I2595
      if FileExists(FStandardTemplatePath + '\xml\help\contexthelp.xml') then
        FStandardTemplatePath := FStandardTemplatePath + '\xml\help\'
      else
        FStandardTemplatePath := GetXMLTemplatePath + 'help\';
    end;
  end;

  if (doc <> 'help/') and (doc <> 'help/index') then
  begin
    Respond404(AContext, ARequestInfo, AResponseInfo);
    Exit;
  end;

  xml := TXMLDocument.Create(nil);
  xml.ParseOptions := [poResolveExternals];
  // I902 - resolve externals when loading XML files so locale.xml parses
  xml.LoadFromFile(FStandardTemplatePath + 'contexthelp.xml');

  xsl := TXMLDocument.Create(nil);
  // xml.ParseOptions := [poResolveExternals];  // I902 - resolve externals when loading XML files so locale.xml parses

  xsl.LoadFromFile(FStandardTemplatePath + 'help.xsl');
  xml.Node.transformNode(xsl.Node, s);

  AResponseInfo.ContentType := 'text/html; charset=UTF-8';
  AResponseInfo.ContentText := s;
end;

procedure TAppHttpResponder.RespondProject(doc: string; AContext: TIdContext;
  ARequestInfo: TIdHTTPRequestInfo; AResponseInfo: TIdHTTPResponseInfo);

  procedure RespondProjectFile;
  var
    path: string;
  begin
    if ARequestInfo.Params.IndexOfName('path') < 0 then
    begin
      AResponseInfo.ResponseNo := 400;
      AResponseInfo.ResponseText := 'Missing parameter path';
      Exit;
    end;

    path := ARequestInfo.Params.Values['path'];

    if not FileExists(path) or not SameText(ExtractFileExt(path), '.kpj') then
    begin
      AResponseInfo.ResponseNo := 400;
      AResponseInfo.ResponseText := 'Project file '+path+' does not exist.';
      Exit;
    end;

    // Transform the .kpj
    with TProject.Create(path) do
    try
      AResponseInfo.ContentType := 'text/html; charset=UTF-8';
      AResponseInfo.ContentText := Render;
    finally
      Free;
    end;
  end;

  procedure RespondIco;
  var
    path: string;
  begin
    if ARequestInfo.Params.IndexOfName('path') < 0 then
    begin
      AResponseInfo.ResponseNo := 400;
      AResponseInfo.ResponseText := 'Missing parameter path';
      Exit;
    end;

    path := ARequestInfo.Params.Values['path'];

    if not FileExists(path) or (
      not SameText(ExtractFileExt(path), '.ico') and
      not SameText(ExtractFileExt(path), '.bmp')
      ) then
    begin
      AResponseInfo.ResponseNo := 400;
      AResponseInfo.ResponseText := 'Image file '+path+' does not exist.';
      Exit;
    end;

    RespondFile(path, AContext, ARequestInfo, AResponseInfo);
  end;

  procedure RespondResource;
  begin
    Delete(doc, 1, 4);
    if (Pos('/', doc) > 0) or (Pos('\', doc) > 0) then
      Respond404(AContext, ARequestInfo, AResponseInfo);
    RespondFile(TProject.StandardTemplatePath + '/' + doc, AContext, ARequestInfo, AResponseInfo);
  end;

  procedure RespondState;
  var
    displayState, path: string;
    xmldoc: IXMLDocument;
    FProject: TProject;
  begin
    with TStringStream.Create('', TEncoding.UTF8) do
    try
      CopyFrom(ARequestInfo.PostStream, 0);
      displayState := DataString;
    finally
      Free;
    end;

    try
      xmldoc := LoadXMLData(displayState);
    except
      on E:Exception do
      begin
        Respond404(AContext, ARequestInfo, AResponseInfo);
        Exit;
      end;
    end;

    path := xmldoc.DocumentElement.ChildValues['path'];

    // Saving state

    if not FileExists(path) or not SameText(ExtractFileExt(path), '.kpj') then
    begin
      AResponseInfo.ResponseNo := 400;
      AResponseInfo.ResponseText := 'Project file '+path+' does not exist.';
      Exit;
    end;

    FProject := TProject.Create(path, False);
    try
      FProject.DisplayState := displayState;
      //TODO: file in-use conflicts
      with TProjectSaver.Create(FProject, path) do
      try
        SaveUser;
      finally
        Free;
      end;
    finally
      FProject.Free;
    end;
  end;
begin
  //
  // http://localhost:8008/app/project/(index)?path=<fqp-to-.kpj>
  // http://localhost:8008/app/project/<resource>
  //
  Delete(doc, 1, 8);
  if (doc = '') or (doc = 'index') then
    RespondProjectFile
  else if doc = 'ico' then
    RespondIco
  else if Copy(doc, 1, 4) = 'res/' then
    RespondResource
  else if doc = 'state' then
    RespondState;
end;

procedure TAppHttpResponder.ProcessRequest(AContext: TIdContext;
  ARequestInfo: TIdHTTPRequestInfo; AResponseInfo: TIdHTTPResponseInfo);
var
  doc: string;
begin
  // We know the request starts with /app/
  doc := ARequestInfo.Document;
  Delete(doc, 1, 5);  // /app/

  // App requests are only accepted from local computer
  // So test that the request comes from localhost
  if (AContext.Binding.PeerIP <> '127.0.0.1') and
    (AContext.Binding.PeerIP <> '0:0:0:0:0:0:0:1') then
  begin
    AResponseInfo.ResponseNo := 403;
    AResponseInfo.ResponseText := 'Access denied';
    Exit;
  end;

  if Copy(doc, 1, 8) = 'project/' then
  begin
    RespondProject(doc, AContext, ARequestInfo, AResponseInfo);
    Exit;
  end
  else if Copy(doc, 1, 5) = 'help/' then
  begin
    RespondHelp(doc, AContext, ARequestInfo, AResponseInfo);
    Exit;
  end;

  Respond404(AContext, ARequestInfo, AResponseInfo);
end;

end.
