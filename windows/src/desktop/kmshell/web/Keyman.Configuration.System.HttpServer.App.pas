unit Keyman.Configuration.System.HttpServer.App;

interface

uses
  System.Classes,
  System.Generics.Collections,

  IdContext,
  IdCustomHTTPServer,

  Keyman.Configuration.System.HttpServer.SharedData,
  Keyman.System.HttpServer.Base,
  XMLRenderer;

type
  TAppHttpResponderClass = class of TAppHttpResponder;

  TAppHttpResponder = class(TBaseHttpResponder)
  private
    class var FRegisteredClasses: TDictionary<string, TAppHttpResponderClass>;
  private
    FSharedData: THttpServerSharedData;
    FContext: TIdContext;
    FRequestInfo: TIdHTTPRequestInfo;
    FResponseInfo: TIdHTTPResponseInfo;
    FXMLRenderers: TXMLRenderers;
    procedure ProcessPageMain(const Params: TStrings);
    procedure ProcessPageHint;
    procedure ProcessPageSplash;
    procedure ProcessPageHelp(const Params: TStrings);
    procedure ProcessPageBaseKeyboard;
    constructor Create(
      ASharedData: THttpServerSharedData;
      AContext: TIdContext;
      ARequestInfo: TIdHTTPRequestInfo;
      AResponseInfo: TIdHTTPResponseInfo);
  protected
    procedure ProcessXMLPage(const s: string = '');
    procedure ProcessRequest; virtual;
    property SharedData: THttpServerSharedData read FSharedData;
    property Context: TIdContext read FContext;
    property RequestInfo: TIdHTTPRequestInfo read FRequestInfo;
    property ResponseInfo: TIdHTTPResponseInfo read FResponseInfo;
    property XMLRenderers: TXMLRenderers read FXMLRenderers;
    class procedure Register(const page: string; ClassType: TAppHttpResponderClass);
  public
    destructor Destroy; override;
    class procedure DoProcessRequest(
      ASharedData: THttpServerSharedData;
      AContext: TIdContext;
      ARequestInfo: TIdHTTPRequestInfo;
      AResponseInfo: TIdHTTPResponseInfo);
  end;

implementation

uses
  System.Contnrs,
  System.SysUtils,

  BaseKeyboards,
  KeymanVersion,
  MessageIdentifierConsts,
  Keyman.System.LocaleStrings,
  KeymanPaths,
  GenericXMLRenderer,
  HotkeysXMLRenderer,
  KeyboardListXMLRenderer,
  LanguagesXMLRenderer,
  OptionsXMLRenderer,
  SupportXMLRenderer,
  TempFileManager,
  utilxml;

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
    if FRequestInfo.Document = '/page/keyman' then
      ProcessPageMain(FRequestInfo.Params)
    else if FRequestInfo.Document = '/page/hint' then
      ProcessPageHint
    else if FRequestInfo.Document = '/page/help' then
      ProcessPageHelp(FRequestInfo.Params)
    else if FRequestInfo.Document = '/page/splash' then
      ProcessPageSplash
    else if FRequestInfo.Document = '/page/basekeyboard' then
      ProcessPageBaseKeyboard
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

constructor TAppHttpResponder.Create(
  ASharedData: THttpServerSharedData; AContext: TIdContext;
  ARequestInfo: TIdHTTPRequestInfo; AResponseInfo: TIdHTTPResponseInfo);
begin
  inherited Create;
  FSharedData := ASharedData;
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


class procedure TAppHttpResponder.DoProcessRequest(ASharedData: THttpServerSharedData; AContext: TIdContext;
  ARequestInfo: TIdHTTPRequestInfo; AResponseInfo: TIdHTTPResponseInfo);
var
  FClass: TAppHttpResponderClass;
  FApp: TAppHttpResponder;
begin
  if FRegisteredClasses.ContainsKey(ARequestInfo.Document) then
  begin
    FClass := FRegisteredClasses[ARequestInfo.Document]
  end
  else
    FClass := TAppHttpResponder;

  FApp := FClass.Create(ASharedData, AContext, ARequestInfo, AResponseInfo);
  try
    FApp.ProcessRequest;
  finally
    FreeAndNil(FApp);
  end;
end;

procedure TAppHttpResponder.ProcessPageHint;
begin
  FXMLRenderers.xRenderTemplate := 'Hint.xsl';
  ProcessXMLPage('');
end;

procedure TAppHttpResponder.ProcessPageHelp(const Params: TStrings);
var
  s: string;
begin
  if Params.Values['keyboard'] <> ''
    then s := Format('<Keyboard Name="%s" />', [XMLEncode(Params.Values['keyboard'])])
    else s := '';
  FXMLRenderers.xRenderTemplate := 'Help.xsl';
  ProcessXMLPage(s);
end;

procedure TAppHttpResponder.ProcessPageMain(const Params: TStrings);
var
  s: string;
begin
  s := Format('<state>%s</state><basekeyboard id="%s">%s</basekeyboard>', [
    XMLEncode(Params.Values['state']),
    XMLEncode(Params.Values['basekeyboardid']),
    XMLEncode(Params.Values['basekeyboardname'])
  ]);

  FXMLRenderers.xRenderTemplate := 'Keyman.xsl';
  FXMLRenderers.Add(TKeyboardListXMLRenderer.Create(FXMLRenderers));
  FXMLRenderers.Add(THotkeysXMLRenderer.Create(FXMLRenderers));
  FXMLRenderers.Add(TOptionsXMLRenderer.Create(FXMLRenderers));
  FXMLRenderers.Add(TLanguagesXMLRenderer.Create(FXMLRenderers));
  FXMLRenderers.Add(TSupportXMLRenderer.Create(FXMLRenderers));

  ProcessXMLPage(s);
end;

procedure TAppHttpResponder.ProcessPageSplash;
var
  xml: string;
begin
  xml :=
    '<Version>'+
    xmlencode(TLocaleStrings.MsgFromIdFormat(FXMLRenderers.kmcom, SKSplashVersion, [CKeymanVersionInfo.VersionWithTag]))+
    '</Version>';

  FXMLRenderers.xRenderTemplate := 'Splash.xsl';
  FXMLRenderers.Clear;
  FXMLRenderers.Add(TGenericXMLRenderer.Create(FXMLRenderers, xml));
  FXMLRenderers.Add(TKeyboardListXMLRenderer.Create(FXMLRenderers));
  ProcessXMLPage;
end;

procedure TAppHttpResponder.ProcessPageBaseKeyboard;
var
  xml: string;
begin
  xml := TBaseKeyboards.EnumerateXML(FXMLRenderers.kmcom.Options['koBaseLayout'].Value);
  FXMLRenderers.xRenderTemplate := 'basekeyboard.xsl';
  FXMLRenderers.Add(TGenericXMLRenderer.Create(FXMLRenderers, xml));
  ProcessXMLPage;
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

class procedure TAppHttpResponder.Register(const page: string; ClassType: TAppHttpResponderClass);
begin
  if not Assigned(FRegisteredClasses) then
    FRegisteredClasses := TDictionary<string, TAppHttpResponderClass>.Create;
  FRegisteredClasses.Add(page, ClassType);
end;

end.
