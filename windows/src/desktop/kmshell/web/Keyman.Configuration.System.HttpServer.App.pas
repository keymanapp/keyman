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
    procedure ProcessPageTextEditorFonts;
    procedure ProcessPageHint(const Params: TStrings);
    procedure ProcessPageSplash;
    procedure ProcessPageHelp(const Params: TStrings);
    procedure ProcessPageBaseKeyboard;
    constructor Create(
      ASharedData: THttpServerSharedData;
      AContext: TIdContext;
      ARequestInfo: TIdHTTPRequestInfo;
      AResponseInfo: TIdHTTPResponseInfo);
    procedure ProcessPageDownloadKeyboard;
  protected
    procedure ProcessXMLPage(const s: string = '');
    procedure ProcessRequest; virtual;
    function GetTaggedData(const IID: TGUID; out Intf): Boolean;

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
  System.RegularExpressions,
  System.StrUtils,
  System.SysUtils,

  keymanapi_tlb,
  BaseKeyboards,
  Keyman.Configuration.System.HttpServer.App.TextEditorFonts,
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
  Upload_Settings,
  utilkmshell,
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
  else if FRequestInfo.Document.StartsWith('/page/') then
  begin
    // Transform an XSL into a HTML page
    if FRequestInfo.Document = '/page/hint' then
      ProcessPageHint(FRequestInfo.Params)
    else if FRequestInfo.Document = '/page/help' then
      ProcessPageHelp(FRequestInfo.Params)
    else if FRequestInfo.Document = '/page/splash' then
      ProcessPageSplash
    else if FRequestInfo.Document = '/page/basekeyboard' then
      ProcessPageBaseKeyboard
    else if FRequestInfo.Document = '/page/downloadkeyboard' then
      ProcessPageDownloadKeyboard
    else if FRequestInfo.Document = '/page/welcome_fonts' then
      ProcessPageTextEditorFonts
    else
    begin
      // Generic response -- no special parameters needed
      FXMLRenderers.RenderTemplate := FRequestInfo.Document.Substring('/page/'.Length) + '.xsl';
      FXMLRenderers.Add(TGenericXMLRenderer.Create(FXMLRenderers));
      ProcessXMLPage;
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
  key: string;
begin
  FClass := TAppHttpResponder;

  for key in FRegisteredClasses.Keys do
  begin
    if ARequestInfo.Document.StartsWith(key) then
    begin
      FClass := FRegisteredClasses[key];
      Break;
    end;
  end;

  FApp := FClass.Create(ASharedData, AContext, ARequestInfo, AResponseInfo);
  try
    FApp.ProcessRequest;
  finally
    FreeAndNil(FApp);
  end;
end;

procedure TAppHttpResponder.ProcessPageHint(const Params: TStrings);
var
  FXML, FButtons: string;
begin
  FXML :=
    '<Hint ID="'+XMLEncode(Params.Values['id']) + '" />' +
    '<Buttons>';

  FButtons := Params.Values['buttons'];
  if FButtons.Contains('ok') then FXML := FXML + '<Button ID="OK" />';
  if FButtons.Contains('cancel') then FXML := FXML + '<Button ID="Cancel" />';

  FXML := FXML + '</Buttons>';

  FXMLRenderers.RenderTemplate := 'Hint.xsl';
  FXMLRenderers.Add(TGenericXMLRenderer.Create(FXMLRenderers, FXML));
  ProcessXMLPage;
end;

procedure TAppHttpResponder.ProcessPageHelp(const Params: TStrings);
var
  s: string;
  kbd: IKeymanKeyboardInstalled;
begin
  if Params.Values['keyboard'] <> ''
    then kbd := FXMLRenderers.kmcom.Keyboards.Items[Params.Values['keyboard']]
    else kbd := nil;

  if Assigned(kbd)
    then s := Format('<Keyboard Name="%s" />', [XMLEncode(kbd.Name)])
    else s := '';
  FXMLRenderers.RenderTemplate := 'Help.xsl';
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

  FXMLRenderers.RenderTemplate := 'Splash.xsl';
  FXMLRenderers.Clear;
  FXMLRenderers.Add(TGenericXMLRenderer.Create(FXMLRenderers, xml));
  FXMLRenderers.Add(TKeyboardListXMLRenderer.Create(FXMLRenderers));
  ProcessXMLPage;
end;

procedure TAppHttpResponder.ProcessPageTextEditorFonts;
var
  data: ITextEditorFontsSharedData;
begin
  if not GetTaggedData(ITextEditorFontsSharedData, data) then
  begin
    Respond404(Context, RequestInfo, ResponseInfo);
    Exit;
  end;
  FXMLRenderers.RenderTemplate := 'welcome_fonts.xsl';
  FXMLRenderers.Add(TGenericXMLRenderer.Create(FXMLRenderers, data.AdditionalData));
  ProcessXMLPage;
end;

procedure TAppHttpResponder.ProcessPageBaseKeyboard;
var
  xml: string;
begin
  xml := TBaseKeyboards.EnumerateXML(FXMLRenderers.kmcom.Options['koBaseLayout'].Value);
  FXMLRenderers.RenderTemplate := 'basekeyboard.xsl';
  FXMLRenderers.Add(TGenericXMLRenderer.Create(FXMLRenderers, xml));
  ProcessXMLPage;
end;

procedure TAppHttpResponder.ProcessPageDownloadKeyboard;
var
  xml: string;
begin
  xml := '<Version>'+CKeymanVersionInfo.VersionWin+'</Version>';
  FXMLRenderers.RenderTemplate := 'downloadkeyboard.xsl';
  FXMLRenderers.Add(TGenericXMLRenderer.Create(FXMLRenderers, xml));
  ProcessXMLPage;
end;

procedure TAppHttpResponder.ProcessXMLPage(const s: string);
var
  FXML: string;
  PageTag: string;
begin
  PageTag := RequestInfo.Params.Values['tag'];
  if PageTag <> '' then
    PageTag := '<PageTag>'+PageTag+'</PageTag>';

  FXML := FXMLRenderers.RenderToString(s + PageTag + DefaultServersXMLTags + DefaultVersionXMLTags);

  FResponseInfo.ContentStream := TStringStream.Create(FXML, TEncoding.UTF8);
  FResponseInfo.FreeContentStream := True;

  FResponseInfo.ContentStream.Position := 0;
  FResponseInfo.ContentType := 'text/html; charset=UTF-8';
end;

function TAppHttpResponder.GetTaggedData(const IID: TGUID; out Intf): Boolean;
var
  tag: Integer;
  u: IInterface;
  PageTag: string;
  regex: TRegEx;
  matches: TMatch;
begin
  Result := False;
  PageTag := RequestInfo.Params.Values['tag'];
  if PageTag = '' then
  begin
    // TIdUri does not have a parameter parser, and while there are other
    // classes we could use, a regex is adequate for this task
    regex := TRegEx.Create('tag=(\d+)');
    matches := regex.Match(RequestInfo.Referer);
    if matches.Success then
      PageTag := matches.Groups[1].Value;
  end;

  tag := StrToIntDef(PageTag, -1);
  if tag >= 0 then
  begin
    u := SharedData.Get(tag);
    if Assigned(u) then
      Result := Supports(u, IID, Intf);
  end;
end;

class procedure TAppHttpResponder.Register(const page: string; ClassType: TAppHttpResponderClass);
begin
  if not Assigned(FRegisteredClasses) then
    FRegisteredClasses := TDictionary<string, TAppHttpResponderClass>.Create;
  FRegisteredClasses.Add(page, ClassType);
end;

end.
