unit Keyman.Configuration.System.HttpServer.App;

interface

uses
  IdContext,
  IdCustomHTTPServer,

  Keyman.System.HttpServer.Base,
  XMLRenderer;

type
  TAppHttpResponder = class(TBaseHttpResponder)
  private
    FContext: TIdContext;
    FRequestInfo: TIdHTTPRequestInfo;
    FResponseInfo: TIdHTTPResponseInfo;
    FXMLRenderers: TXMLRenderers;
    procedure ProcessPageMain;
    procedure ProcessPageHint;
    procedure ProcessPageSplash;
    procedure ProcessXMLPage(const s: string);
  public
    constructor Create(
      AContext: TIdContext;
      ARequestInfo: TIdHTTPRequestInfo;
      AResponseInfo: TIdHTTPResponseInfo);
    destructor Destroy; override;
    procedure ProcessRequest;
  end;

implementation

uses
  System.Classes,
  System.Contnrs,
  System.SysUtils,

  KeymanPaths,
  GenericXMLRenderer,
  HotkeysXMLRenderer,
  KeyboardListXMLRenderer,
  LanguagesXMLRenderer,
  OptionsXMLRenderer,
  SupportXMLRenderer,
  TempFileManager;

{ THttpServerApp }

procedure TAppHttpResponder.ProcessRequest;
var
  p: string;
begin
  if FRequestInfo.Document.StartsWith('/app/') then
  begin
    // Respond with static files from <desktop-install-path>/xml
    if IncludesParentFolderReference(FRequestInfo.Document) then
      Respond404(FContext, FRequestInfo, FResponseInfo)
    else
    begin
      p := TKeymanPaths.KeymanConfigStaticHttpFilesPath(FRequestInfo.Document.Substring('/app/'.Length));
      RespondFile(p, FContext, FRequestInfo, FResponseInfo);
    end;
  end
  else if FRequestInfo.Document.StartsWith('/doc/') then
  begin
    // Respond with content
  end
  else if FRequestInfo.Document.StartsWith('/page/') then
  begin
    // Retrieve data from kmcom etc
    if FRequestInfo.Document = '/page/main' then
      ProcessPageMain
    else if FRequestInfo.Document = '/page/hint' then
      ProcessPageHint
    else if FRequestInfo.Document = '/page/splash' then
      ProcessPageSplash
    else
    begin
      // Generic response
      FXMLRenderers.xRenderTemplate := FRequestInfo.Document.Substring('/page/'.Length) + '.xsl';
      FXMLRenderers.Add(TGenericXMLRenderer.Create(FXMLRenderers));
      ProcessXMLPage('');
    end;
  end;

  // Only allow one request per instance
  // This will throw on next entry

  FContext := nil;
  FRequestInfo := nil;
  FResponseInfo := nil;
end;

constructor TAppHttpResponder.Create(AContext: TIdContext;
  ARequestInfo: TIdHTTPRequestInfo; AResponseInfo: TIdHTTPResponseInfo);
begin
  inherited Create;
  FContext := AContext;
  FRequestInfo := ARequestInfo;
  FResponseInfo := AResponseInfo;
  FXMLRenderers := TXMLRenderers.Create;
end;

destructor TAppHttpResponder.Destroy;
begin
  FreeAndNil(FXMLRenderers);
  inherited Destroy;
end;

procedure TAppHttpResponder.ProcessPageHint;
begin
  FXMLRenderers.xRenderTemplate := 'Hint.xsl';
  ProcessXMLPage('');
end;

procedure TAppHttpResponder.ProcessPageMain;
var
  s: string;
begin
//  SaveState;
  s := '';
{
  s := '<state>'+XMLEncode(FState)+'</state>';
  s := s + '<basekeyboard id="'+IntToHex(Cardinal(kmcom.Options[KeymanOptionName(koBaseLayout)].Value),8)+'">'+
    XMLEncode(TBaseKeyboards.GetName(kmcom.Options[KeymanOptionName(koBaseLayout)].Value))+
    '</basekeyboard>';   // I4169
}

  FXMLRenderers.xRenderTemplate := 'Keyman.xsl';
  FXMLRenderers.Add(TKeyboardListXMLRenderer.Create(FXMLRenderers));
  FXMLRenderers.Add(THotkeysXMLRenderer.Create(FXMLRenderers));
  FXMLRenderers.Add(TOptionsXMLRenderer.Create(FXMLRenderers));
  FXMLRenderers.Add(TLanguagesXMLRenderer.Create(FXMLRenderers));
  FXMLRenderers.Add(TSupportXMLRenderer.Create(FXMLRenderers));

  ProcessXMLPage(s);
end;

procedure TAppHttpResponder.ProcessPageSplash;
begin
  FXMLRenderers.xRenderTemplate := 'Splash.xsl';
  FXMLRenderers.Clear;
  FXMLRenderers.Add(TGenericXMLRenderer.Create(FXMLRenderers));
  FXMLRenderers.Add(TKeyboardListXMLRenderer.Create(FXMLRenderers));
  ProcessXMLPage('');
end;

procedure TAppHttpResponder.ProcessXMLPage(const s: string);
var
  FXMLFileName: TTempFile;
  FFileStream: TFileStream;
begin
  FXMLFileName := FXMLRenderers.RenderToFile(False, s); // FRefreshKeyman, AdditionalData);

  FResponseInfo.ContentStream := TMemoryStream.Create;
  FResponseInfo.FreeContentStream := True;

  FFileStream := TFileStream.Create(FXMLFileName.Name, fmOpenRead or fmShareDenyWrite);
  try
    FResponseInfo.ContentStream.CopyFrom(FFileStream, 0);
  finally
    FFileStream.Free;
  end;

  FResponseInfo.ContentStream.Position := 0;
end;

end.
