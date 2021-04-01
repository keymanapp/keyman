unit Keyman.Configuration.System.HttpServer.App.Locale;

interface

uses
  Keyman.Configuration.System.HttpServer.App;

type
  TLocaleHttpResponder = class(TAppHttpResponder)
  public
    procedure ProcessRequest; override;
  end;

implementation

uses
  System.Classes,
  System.Contnrs,
  System.StrUtils,
  System.SysUtils,
  System.Variants,
  Xml.XMLDoc,
  Xml.XMLIntf,

  custinterfaces,
  Keyman.System.AndroidStringToKeymanLocaleString,
  Keyman.System.LocaleStrings,
  KeymanPaths,
  MessageIdentifierConsts;

const
  Path_Page = '/page/locale';

{ TOnlineUpdateHttpResponder }

procedure TLocaleHttpResponder.ProcessRequest;
var
  s: string;
  document: string;
  doc: IXMLDocument;
  messages: IKeymanCustomisationMessages;
  node: IXMLNode;
  stream: TMemoryStream;
begin
  document := Self.RequestInfo.Document.Substring(Path_Page.Length);
  if not document.StartsWith('/') then
  begin
    Respond404(Context, RequestInfo, ResponseInfo);
    Exit;
  end;

  document := document.Substring(1);
  messages := (Self.XMLRenderers.kmcom.Control as IKeymanCustomisationAccess).KeymanCustomisation.CustMessages;

  s := messages.GetLocalePathForLocale(document);
  if (s = '') or not FileExists(s) then
  begin
    s := TKeymanPaths.KeymanConfigStaticHttpFilesPath + 'strings.xml';
  end;

  // Load and transform the strings in the document

  doc := TXMLDocument.Create(nil);
  doc.ParseOptions := [poResolveExternals];  // I902 - resolve externals when loading XML files so locale.xml parses
  doc.LoadFromFile(s);

  node := doc.DocumentElement.ChildNodes.First;
  while Assigned(node) do
  begin
    if (node.NodeName = 'string') and not VarIsNull(node.NodeValue) then
    begin
      node.NodeValue := TAndroidStringToKeymanLocaleString.Transform(node.NodeValue);
    end;
    node := node.NextSibling;
  end;

  stream := TMemoryStream.Create;
  doc.Encoding := 'UTF-8';
  doc.SaveToStream(stream);
  ResponseInfo.ContentStream := stream;
  ResponseInfo.FreeContentStream := True;
  ResponseInfo.ContentStream.Position := 0;
  ResponseInfo.CharSet := 'UTF-8';
  ResponseInfo.ContentType := 'application/xml; charset=UTF-8';
end;

initialization
  TLocaleHttpResponder.Register(Path_Page, TLocaleHttpResponder);
end.
