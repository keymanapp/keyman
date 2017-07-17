//****************************************************
//                     TIEParser                     *
//                     For Delphi 5 to XE            *
//                Freeware Component                 *
//                       by                          *
//                                                   *
//                Per Lindsø Larsen &                *
//              Eran Bodankin (bsalsa)               *
//                 bsalsa@gmail.com                  *
//                                                   *
// Documentation and updated versions:               *
//               http://www.bsalsa.com               *
//****************************************************

{*******************************************************************************}
{LICENSE:
THIS SOFTWARE IS PROVIDED TO YOU "AS IS" WITHOUT WARRANTY OF ANY KIND,
EITHER EXPRESSED OR IMPLIED INCLUDING BUT NOT LIMITED TO THE APPLIED
WARRANTIES OF MERCHANTABILITY AND/OR FITNESS FOR A PARTICULAR PURPOSE.
YOU ASSUME THE ENTIRE RISK AS TO THE ACCURACY AND THE USE OF THE SOFTWARE
AND ALL OTHER RISK ARISING OUT OF THE USE OR PERFORMANCE OF THIS SOFTWARE
AND DocUMENTATION. BSALSA PRODUCTIONS DOES NOT WARRANT THAT THE SOFTWARE IS ERROR-FREE
OR WILL OPERATE WITHOUT INTERRUPTION. THE SOFTWARE IS NOT DESIGNED, INTENDED
OR LICENSED FOR USE IN HAZARDOUS ENVIRONMENTS REQUIRING FAIL-SAFE CONTROLS,
INCLUDING WITHOUT LIMITATION, THE DESIGN, CONSTRUCTION, MAINTENANCE OR
OPERATION OF NUCLEAR FACILITIES, AIRCRAFT NAVIGATION OR COMMUNICATION SystemS,
AIR TRAFFIC CONTROL, AND LIFE SUPPORT OR WEAPONS SystemS. BSALSA PRODUCTIONS SPECIFICALLY
DISCLAIMS ANY EXPRES OR IMPLIED WARRANTY OF FITNESS FOR SUCH PURPOSE.

You may use, change or modify the component under 4 conditions:
1. In your website, add a Link to "http://www.bsalsa.com"
2. In your application, add credits to "Embedded Web Browser"
3. Mail me  (bsalsa@gmail.com) any code change in the unit
   for the benefit of the other users.
4. Please consider donation in our web site!
{*******************************************************************************}
//$Id: IEParser.pas,v 1.3 2009/04/05 05:55:31 bsalsa Exp $

unit IEParser;

{$I EWB.inc}

interface

uses
  Dialogs, ShlObj, ComObj, Windows, Mshtml_Ewb, ActiveX, Classes;

type
  TProxySettings = class(TPersistent)
  private
    FPort: Integer;
    FServer: string;
    FAutoLoadProxy: Boolean;
  public
    function SetProxy(const FullUserAgent, ProxyServer: string): Boolean;
  published
    property AutoLoadProxy: Boolean read FAutoLoadProxy write FAutoLoadProxy
      default False;
    property Port: Integer read FPort write FPort default 80;
    property Server: string read FServer write FServer;
  end;

type
  TElementInfo = record
    ClassName: string;
    Id: string;
    InnerHTML: string;
    InnerText: string;
    Lang: string;
    Language: string;
    OffsetHeight: Integer;
    OffsetLeft: Integer;
    OffsetTop: Integer;
    OffsetWIdth: Integer;
    OuterHTML: string;
    OuterText: string;
    RecordNumber: OleVariant;
    SourceIndex: Integer;
    TagName: string;
    Title: string;
  end;

  TDownloadControlOption = (
    DownloadImages, DownloadVideos, DownloadBGSounds, DontExecuteScripts,
    DontExecuteJava, DontExecuteActiveX, DontDownloadActiveX,
    DownloadButDontDisplay, DontDownloadFrame, CheckPageResynchronize,
    DownloadAndIgnoreCache, DontDownloadBehaviors, SuppressedMetaCharset,
    DisableUrlIfEncodingUTF8, EnableUrlIfEncodingUTF8,
    ForceOfflineMode, DontPerformClientPull, DownloadInSilentMode, WorkOffline);
  TDownloadControlOptions = set of TDownloadControlOption;

  TParserState = (psBusy, psReady, psStopped); {A state for Busy status}

  TOnParseErrorEvent = procedure(Sender: TObject; const ErrorCode: integer; const
    Url, stError: string) of object;
  TOnParseDocumentEvent = procedure(Sender: TObject; const Res: HRESULT; stMessage: string) of object;
  TOnStatusTextEvent = procedure(Sender: TObject; const Text: string) of object;
  TOnDocInfoEvent = procedure(Sender: TObject; const Text: string) of object;
  TOnParseCompleteEvent = procedure(Sender: TObject; Doc: IhtmlDocument2; All: IHtmlElementCollection) of object;
  TOnBREvent = procedure(Sender: TObject; Clear: string; Element: TElementInfo) of object;
  TOnHREvent = procedure(Sender: TObject; Align: string; Color, Width, Size:
    OleVariant; NoShade: Boolean; Element: TElementInfo) of object;
  TOnDIVEvent = procedure(Sender: TObject; Align: string; NoWrap: Boolean;
    Element: TElementInfo) of object;
  TOnScriptEvent = procedure(Sender: Tobject; Source: string; ScriptElement: IHTMLScriptElement; Element: TElementInfo) of object;
  TOnFormEvent = procedure(Sender: TObject; Action, Dir, Encoding, Method, Target, Name: string;
    Element: TElementInfo) of object;
  TOnMarqueeEvent = procedure(Sender: TObject; bgColor, Width, Height: OleVariant;
    Direction, Behavior: string; ScrollAmount, ScrollDelay, Loop, vSpace, hSpace:
    Integer; Element: TElementInfo) of object;
  TOnFontEvent = procedure(Sender: TObject; Color, Size: OleVariant; Face: string;
    Element: TElementInfo) of object;
  TOnBaseFontEvent = procedure(Sender: TObject; Color: OleVariant; Face: string;
    Size: Integer; Element: TElementInfo) of object;
  TOnBaseEvent = procedure(Sender: TObject; hRef, Target: string; Element: TElementInfo) of object;
  TOnMetaEvent = procedure(Sender: TObject; HttpEquiv, Content, Name, URL, Charset:
    string; Element: TElementInfo) of object;
  TOnBodyEvent = procedure(Sender: TObject; Background, bgProperties: string; LeftMargin,
    TopMargin, RightMargin, BottomMargin, bgColor, Text, Link, vLink, aLink: OleVariant;
    NoWrap: Boolean; Element: TElementInfo) of object;
{  TOnImageEvent = procedure(Sender: TObject; Source, LowSrc, Vrml, DynSrc, Alt, Align,
    UseMap: string; IsMap: Boolean; Border, Loop: OleVariant; vSpace, hSpace, Width,
    Height: Integer; Element: TElementInfo) of object; }

  TOnImageEvent = procedure(Sender: TObject; Source: string; ImgElement: IHTMLImgElement; Element: TElementInfo) of object;

  TOnAnchorEvent = procedure(Sender: TObject; hRef, Target, Rel, Rev, Urn, Methods, Name,
    Host, HostName, PathName, Port, Protocol, Search, Hash, AccessKey, ProtocolLong,
    MimeType, NameProp: string; Element: TElementInfo) of object;
  TOnCommentEvent = procedure(sender: TObject; Text: string; Element: TElementInfo) of object;
  TOnElementEvent = procedure(Sender: TObject; ElementInfo: TElementInfo) of object;
  TNoFramesEvent = procedure(Sender: TObject; ELement: TElementInfo) of object;
  TOnFrameEvent = procedure(Sender: TObject; Source, Name: OleVariant; Element:
    TElementInfo) of object;
  TOnFrameSetEvent = procedure(Sender: TObject; Rows, Cols, FrameBorder, Name:
    WIdeString; Border, BorderColor, FrameSpacing: OleVariant; Element:
    TelementInfo) of object;
  TStateChangeEvent = procedure(Sender: TObject; const State: TParserState) of object;
  TOnStartParsingEvent = procedure(Sender: TObject; const aUrl: WideString) of object;
  TOnQueryInfoEvent = procedure(const MimeType, Encoding, Disposition: string) of object;

  TIEParser = class(
      TComponent,
      IUnknown,
      IDispatch,
      IPropertyNotifySink,
      IOleClientSite)

  private
    BoolWorking: Boolean;
    Element: TElementInfo;
    FAbout: string;
    FAnchor: TOnAnchorEvent;
    FBase: TOnBaseEvent;
    FBaseFont: TOnBaseFontEvent;
    FBody: TOnBodyEvent;
    FBr: TOnBREvent;
    FBusy: Boolean;
    FComment: TOnCommentEvent;
    FDiv: TOnDIVEvent;
    FOnParseComplete: TOnParseCompleteEvent;
    FDownloadControlOptions: TDownloadControlOptions;
    FDownloadOnly: Boolean;
    FElement: TOnElementEvent;
    FFont: TOnFontEvent;
    FForm: TOnFormEvent;
    FHr: TOnHREvent;
    FHtml: WIdeString;
    FImage: TOnImageEvent;
    FMarquee: TOnMarqueeEvent;
    FMimeType: string;
    FDisposition: string;
    FEncoding: string;
    FMeta: TOnMetaEvent;
    FOnBusy: TNotifyEvent;
    FOnDocInfo: TOnDocInfoEvent;
    FOnFrame: TOnFrameEvent;
    FOnFrameset: TOnFrameSetEvent;
    FOnNoFrame: TNoFramesEvent;
    FOnQueryInfo: TOnQueryInfoEvent;
    FOnParseDocument: TOnParseDocumentEvent;
    FOnParseError: TOnParseErrorEvent;
    FOnStateChange: TStateChangeEvent;
    FOnStartParsing: TOnStartParsingEvent;
    FOnStatusText: TOnStatusTextEvent;
    FParseNoFrames: Boolean;
    FProxySettings: TProxySettings;
    FScript: TOnScriptEvent;
    FParserState: TParserState;
    FUrl: string;
    LoadingFromString: Boolean;
    NoFramesFound: Boolean;
    StartTick: Int64;
  private
    function UpdateDownloadControlValues: LongInt;
  protected
    function ProcessDoc(const aUrl: WideString): IHTMLDocument2;
    function GetContainer(out container: IOleContainer): HRESULT; stdcall;
    function GetMoniker(dwAssign: Longint; dwWhichMoniker: Longint; out mk: IMoniker): HRESULT; stdcall;
    function Invoke(DispId: Integer; const IId: TGUId; LocaleId: Integer; Flags: Word; var Params; VarResult, ExcepInfo, ArgErr: Pointer): HRESULT; stdcall;
    function LoadFromString: HRESULT;
    function LoadUrlFromMoniker(const aUrl: WideString): HRESULT;
    function OnChanged(dispId: TDispId): HRESULT; stdcall;
    function OnRequestEdit(dispId: TDispId): HRESULT; stdcall;
    function OnShowWindow(fShow: BOOL): HRESULT; stdcall;
    function RequestNewObjectLayout: HRESULT; stdcall;
    function SaveObject: HRESULT; stdcall;
    function ShowObject: HRESULT; stdcall;
    procedure Finalize;
    procedure GetPageProperties;
    procedure Initialize;
    procedure SetAbout(const Value: string);
    procedure DoQueryInfo(const aUrl: string);
  public
    All: IHtmlElementCollection;
    Doc: IHTMLDocument2;
    constructor Create(Owner: Tcomponent); override;
    destructor Destroy; override;
    procedure BeforeDestruction; override;
    procedure Parse(const aUrl: WideString);
    procedure Stop;
    procedure Loaded; override;
  public
    property Busy: Boolean read FBusy;
    property ParserState: TParserState read FParserState;
    property MimeType: string read FMimeType;
    property Disposition: string read FDisposition;
    property Encoding: string read FEncoding;
  published
    property About: string read FAbout write SetAbout;
    property DownloadOnly: Boolean read FDownloadOnly write FDownloadOnly default false;
    property DownloadOptions: TDownloadControlOptions read FDownloadControlOptions
      write FDownloadControlOptions default [DownloadImages, DownloadBGSounds,
      DownloadVideos, DownloadButDontDisplay, DontExecuteScripts,
      DontExecuteJava, DontExecuteActiveX, DontDownloadActiveX];
    property Html: WIdeString read FHtml write FHtml;
    property OnAnchor: TOnAnchorEvent read FAnchor write FAnchor;
    property OnBase: TOnBaseEvent read FBase write FBase;
    property OnBaseFont: TOnBaseFontEvent read FBaseFont write FBaseFont;
    property OnBody: TOnBodyEvent read FBody write FBody;
    property OnBR: TOnBREvent read FBr write FBr;
    property OnBusyStateChange: TNotifyEvent read FOnBusy write FOnBusy;
    property OnComment: TOnCommentEvent read FComment write FComment;
    property OnDiv: TOnDIVEvent read FDiv write FDiv;
    property OnDocInfo: TOnDocInfoEvent read FOnDocInfo write FOnDocInfo;
    property OnParseComplete: TOnParseCompleteEvent read FOnParseComplete write FOnParseComplete;
    property OnElement: TOnElementEvent read FElement write FElement;
    property OnFont: TOnFontEvent read FFont write FFont;
    property OnForm: TOnFormEvent read FForm write FForm;
    property OnFrame: TOnFrameEvent read FOnFrame write FOnFrame;
    property OnFrameSet: TOnFrameSetEvent read FOnFrameset write FOnFrameset;
    property OnQueryInfo: TOnQueryInfoEvent read FOnQueryInfo write
      FOnQueryInfo;
    property OnHR: TOnHREvent read FHr write FHr;
    property OnImage: TOnImageEvent read FImage write FImage;
    property OnMarquee: TOnMarqueeEvent read FMarquee write FMarquee;
    property OnMeta: TOnMetaEvent read FMeta write FMeta;
    property OnNoFrame: TNoFramesEvent read FOnNoFrame write FOnNoFrame;
    property OnParseDocument: TOnParseDocumentEvent read FOnParseDocument write FOnParseDocument;
    property OnParseError: TOnParseErrorEvent read FOnParseError write FOnParseError;
    property OnScript: TOnScriptEvent read FScript write FScript;
    property OnStateChange: TStateChangeEvent read FOnStateChange write
      FOnStateChange;
    property OnStartParsing: TOnStartParsingEvent read FOnStartParsing write FOnStartParsing;
    property OnStatusText: TOnStatusTextEvent read FOnStatusText write FOnStatusText;
    property ParseNoFrames: Boolean read FParseNoFrames write FParseNoFrames default False;
    property ProxySettings: TProxySettings read FProxySettings write FProxySettings;
    property URL: string read FUrl write FUrl;
  end;


implementation

uses
  EwbIEConst, IEDownloadStrings, IEDownloadTools, SysUtils, IeDownloadAcc, UrlMon, WinInet;


function TIEParser.GetContainer(out container: IOleContainer): HRESULT;
begin
  Result := E_NOTIMPL;
end;

function TIEParser.GetMoniker(dwAssign: Longint; dwWhichMoniker: Longint; out mk:
  IMoniker): HRESULT;
begin
  Result := E_NOTIMPL;
end;

function TIEParser.Invoke(DispId: Integer; const IId: TGUId; LocaleId: Integer;
  Flags: Word; var Params; VarResult, ExcepInfo, ArgErr: Pointer): HRESULT;
var
  I: Integer;
begin
  if DispId = DISPID_AMBIENT_DLCONTROL then
  begin
    i := UpdateDownloadControlValues;
    PVariant(VarResult)^ := I;
    Result := S_OK;
  end
  else
    Result := DISP_E_MEMBERNOTFOUND;
end;

function TIEParser.LoadFromString: HRESULT;
var
  V: OleVariant;
  vDocument: OleVariant;
  vMimeType: OleVariant;
  vHtml: OleVariant;
begin
  if FHtml = '' then
  begin
    if Assigned(FOnParseError) then
      FOnParseError(Self, E_FAIL, FUrl, Err_Load_Str + ResponseCodeToStr(E_FAIL));
    Result := E_FAIL;
  end else
  try
    if (Assigned(FOnParseDocument)) then
      FOnParseDocument(Self, S_OK, Succ_Load_Str + ResponseCodeToStr(S_OK));

    //Stop any actions - this is important!
    // FWeb.Stop;   no dealing with the Document directly
    // Grab the Document
    V := Doc;
    vDocument := V.script.Document;
    vMimeType := 'text/Html';
    vHtml := FHtml;
    vDocument.Open(vMimeType);
    vDocument.Clear;
    vDocument.Write(vHtml);
    vDocument.Close;
    Result := S_OK;
  except
    Result := E_FAIL;
  end;
end;

function TIEParser.OnChanged(dispId: TDispId): HRESULT;
var
  DP: TDispParams;
  vResult: OLEVariant;
begin
  Result := S_OK;
  if Doc = nil then Exit;
  if (DISPId_READYSTATE = DispId) then
    if SUCCEEDED((Doc as IHtmlDocument2).Invoke(DISPId_READYSTATE, GUId_null,
      LOCALE_System_DEFAULT, DISPATCH_PROPERTYGET, DP, @vResult, nil, nil)) then
      if Integer(vResult) = READYSTATE_COMPLETE then
        PostThreadMessage(GetCurrentThreadId(), WM_USER_STARTWALKING, 0, 0);
end;

function TIEParser.OnRequestEdit(dispId: TDispId): HRESULT;
begin
  Result := E_NOTIMPL;
end;

function TIEParser.OnShowWindow(fShow: BOOL): HRESULT;
begin
  Result := E_NOTIMPL;
end;

function TIEParser.RequestNewObjectLayout: HRESULT;
begin
  Result := E_NOTIMPL;
end;

function TIEParser.SaveObject: HRESULT;
begin
  Result := E_NOTIMPL;
end;

function TIEParser.ShowObject: HRESULT;
begin
  Result := E_NOTIMPL;
end;

function TIEParser.UpdateDownloadControlValues: LongInt;
const
  AcardDownloadControlValues: array[TDownloadControlOption] of Cardinal =
  ($00000010, $00000020, $00000040, $00000080,
    $00000100, $00000200, $00000400, $00000800,
    $00001000, $00002000, $00004000, $00008000,
    $00010000, $00020000, $00040000, $10000000,
    $20000000, $40000000, $80000000);
var
  i: TDownloadControlOption;
  j: Longint;
begin
  j := 0;
  if (FDownloadControlOptions <> []) then
    for i := Low(TDownloadControlOption) to High(TDownloadControlOption) do
      if (i in FDownloadControlOptions) then
        Inc(j, AcardDownloadControlValues[i]);
  Result := j;
end;

constructor TIEParser.Create(Owner: Tcomponent);
begin
  inherited Create(Owner);
  FAbout := 'TIEParser from: http://www.bsalsa.com';
  DownloadOptions := [DownloadImages, DownloadBGSounds,
    DownloadVideos, DownloadButDontDisplay, DontExecuteScripts,
    DontExecuteJava, DontExecuteActiveX, DontDownloadActiveX];
  FProxySettings := TProxySettings.Create;
  FProxySettings.FPort := 80;
  FParserState := psReady;
end;

procedure TIEParser.BeforeDestruction;
begin
  if FProxySettings.FAutoLoadProxy then
    FProxySettings.SetProxy(EmptyStr, EmptyStr); {To restore proxy settings}
  inherited BeforeDestruction;
end;

destructor TIEParser.Destroy;
begin
  if Assigned(Doc) then
    Doc := nil;
  if Assigned(All) then
    All := nil;
  FProxySettings.Free;
  inherited Destroy;
end;

procedure TIEParser.Loaded;
begin
  inherited Loaded;
  FBusy := False;
  FParserState := psReady;
  if (FProxySettings.FAutoLoadProxy) and (FProxySettings.FServer <> EmptyStr)
    then
    FProxySettings.SetProxy(USER_AGENT_IE6, FProxySettings.FServer + ':' +
      IntToStr(FProxySettings.FPort));
end;

procedure TIEParser.SetAbout(const Value: string);
begin
  Exit;
end;

procedure TIEParser.GetPageProperties;
begin
  if (Doc <> nil) and (Assigned(Doc)) and (Doc.readyState = 'complete') then
  begin
    if (Assigned(FOnDocInfo)) then
    begin
      try
        FOnDocInfo(Self, 'Title: ' + Doc.title);
        FOnDocInfo(Self, 'Design Mode: ' + Doc.designMode);
        FOnDocInfo(Self, 'State: ' + Doc.readyState);
        FOnDocInfo(Self, 'Referrer: ' + Doc.Referrer);
        FOnDocInfo(Self, 'Location: ' + Doc.location.href);
        FOnDocInfo(Self, 'Last Modified: ' + Doc.lastModified);
        FOnDocInfo(Self, 'URL: ' + Doc.url);
        if FHTML <> '' then
          FOnDocInfo(Self, 'Domain: ' + Doc.domain);
        FOnDocInfo(Self, 'Cookie: ' + Doc.cookie);
        FOnDocInfo(Self, 'Charset: ' + Doc.charset);
        FOnDocInfo(Self, 'Default Charset: ' + Doc.defaultCharset);
        FOnDocInfo(Self, 'File Updated Date: ' + Doc.fileUpdatedDate);
        FOnDocInfo(Self, 'Security: ' + Doc.security);
        FOnDocInfo(Self, 'Protocol: ' + Doc.protocol);
        FOnDocInfo(Self, 'Name Property: ' + Doc.nameProp);
        FOnDocInfo(Self, 'Path Name: ' + Doc.location.pathname);
        FOnDocInfo(Self, 'Port: ' + Doc.location.port);
        FOnDocInfo(Self, 'Protocol: ' + Doc.location.protocol);
        FOnDocInfo(Self, 'Host: ' + Doc.location.host);
        FOnDocInfo(Self, 'Hash: ' + Doc.location.hash);
        FOnDocInfo(Self, 'Search: ' + Doc.location.search);
        FOnDocInfo(Self, 'Language: ' + Doc.body.language);
        FOnDocInfo(Self, 'Lang: ' + Doc.body.lang);
        {I Disabled the following because it my cause AV on some sites}
        //FOnDocInfo(Self,'MimeType: ' + Doc.MimeType);
        //FOnDocInfo(Self,'File Size: '+ Doc.fileSize);
        //FOnDocInfo(Self,'File Created Date: '+ Doc.fileCreatedDate);
        //FOnDocInfo(Self,'File Modified Date: '+ Doc.fileModifiedDate);
      except
      end;
    end;
  end;
end;

function TIEParser.LoadUrlFromMoniker(const aUrl: WideString): HRESULT;
var
  FMoniker: IMoniker;
  FBindCtx: IBindCTX;
  HR: HRESULT;
begin
  HR := CreateURLMonikerEx(nil, PWideChar(aUrl), FMoniker, URL_MK_UNIFORM {URL_MK_LEGACY});
  if Failed(HR) and Assigned(FOnParseError) then
    FOnParseError(Self, GetLastError, FUrl, Err_URLMEx +
      ResponseCodeToStr(HR))
  else if (Assigned(FOnParseDocument)) then
    FOnParseDocument(Self, HR, CreateURLMEx + ResponseCodeToStr(HR));

  HR := CreateBindCtx(0, FBindCtx);
  if Failed(HR) and Assigned(FOnParseError) then
    FOnParseError(Self, GetLastError, FUrl, Err_AsyncBindCtx +
      ResponseCodeToStr(HR))
  else if (Assigned(FOnParseDocument)) then
    FOnParseDocument(Self, HR, CreateABindCtx + ResponseCodeToStr(HR));

  HR := (Doc as IpersistMoniker).Load(LongBool(0), FMoniker, FBindCtx, STGM_READ);
  if Failed(HR) and Assigned(FOnParseError) then
    FOnParseError(Self, GetLastError, FUrl, Err_IpersistMoniker_Load
      + ResponseCodeToStr(HR))
  else if (Assigned(FOnParseDocument)) then
    FOnParseDocument(Self, HR, Succ_IpersistMoniker_Load + ResponseCodeToStr(HR));
  Result := HR;
end;

function TIEParser.ProcessDoc(const aUrl: WideString): IHTMLDocument2;
var
  C: Integer;
  ConnectionPoint: IConnectionPoint;
  HR: HRESULT;
begin
  LoadingFromString := False;
  if Assigned(FOnStartParsing) then
      FOnStartParsing(Self, aUrl);
  HR := CoCreateInstance(CLASS_HtmlDocument, nil, CLSCTX_INPROC_SERVER,
    IHtmlDocument2, Doc);
  if Failed(HR) and Assigned(FOnParseError) then
    FOnParseError(Self, GetLastError, FUrl, Err_CoCreateInstance + ResponseCodeToStr(HR))
  else if (Assigned(FOnParseDocument)) then
    FOnParseDocument(Self, HR, Succ_CoCreateInstance + ResponseCodeToStr(HR));

  HR := (Doc as IOleObject).SetClientSite(Self as IOleClientsite);
  if Failed(HR) and Assigned(FOnParseError) then
    FOnParseError(Self, GetLastError, FUrl, Err_Doc_AsSetClientSite + ResponseCodeToStr(HR))
  else if (Assigned(FOnParseDocument)) then
    FOnParseDocument(Self, HR, Doc_AsSetClientSite + ResponseCodeToStr(HR));

  HR := (Doc as IOleControl).OnAmbientPropertyChange(DISPId_AMBIENT_DLCONTROL);
  if Failed(HR) and Assigned(FOnParseError) then
    FOnParseError(Self, GetLastError, FUrl, Err_Doc_AsAmbientPropertyChange + ResponseCodeToStr(HR))
  else if (Assigned(FOnParseDocument)) then
    FOnParseDocument(Self, HR, Doc_AsAmbientPropertyChange + ResponseCodeToStr(HR));

  HR := (Doc as IConnectionPointContainer).FindConnectionPoint(IpropertyNotifySink, ConnectionPoint);
  if Failed(HR) and Assigned(FOnParseError) then
    FOnParseError(Self, GetLastError, FUrl, Err_Doc_AsPointContainer + ResponseCodeToStr(HR))
  else if (Assigned(FOnParseDocument)) then
    FOnParseDocument(Self, HR, Doc_AsPointContainer + ResponseCodeToStr(HR));

  HR := (ConnectionPoint.Advise(Self as IPropertyNotifySink, C));
  if Failed(HR) and Assigned(FOnParseError) then
    FOnParseError(Self, GetLastError, FUrl, Err_Doc_AsAdvise + ResponseCodeToStr(HR))
  else if (Assigned(FOnParseDocument)) then
    FOnParseDocument(Self, HR, Doc_AsAdvise + ResponseCodeToStr(HR));
  DoQueryInfo(FUrl);
  Result := Doc;
end;

procedure TIEParser.DoQueryInfo(const aUrl: string);
var
  hInet: HINTERNET;
  hConnect: HINTERNET;
  infoBuffer: array[0..1024] of Char;
  dwReserved: DWORD;
  bufLen: DWORD;
  lbResult: LongBool;
begin
  hInet := InternetOpen('TDownload',
    INTERNET_OPEN_TYPE_PRECONFIG_WITH_NO_AUTOPROXY, nil, nil, 0);
  if hInet <> nil then
  begin
    try
      hConnect := InternetOpenUrl(hInet, PChar(Url), nil, 0, INTERNET_FLAG_NO_UI, 0);
      if Assigned(hConnect) then
      begin
        try
          dwReserved := 0;
          bufLen := Length(infoBuffer);

          lbResult := HttpQueryInfo(hConnect, HTTP_QUERY_CONTENT_TYPE, @infoBuffer[0], bufLen, dwReserved);
          if lbResult then
            FMimeType := infoBuffer
          else
            FMimeType := EmptyStr;

          lbResult := HttpQueryInfo(hConnect, HTTP_QUERY_CONTENT_ENCODING, @infoBuffer, bufLen, dwReserved);
          if lbResult then
            FEncoding := Encoding
          else
            FEncoding := EmptyStr;

          lbResult := HttpQueryInfo(hConnect, HTTP_QUERY_CONTENT_DISPOSITION, @infoBuffer, bufLen, dwReserved);
          if lbResult then
            FDisposition := Disposition
          else
            FDisposition := EmptyStr;

          if Assigned(FOnQueryInfo) then
            FOnQueryInfo(FMimeType, FEncoding, FDisposition);
        finally
          InternetCloseHandle(hConnect);
        end;
      end;
    finally
      InternetCloseHandle(hInet);
    end;
  end;
end;

procedure TIEParser.Initialize;
begin
  Doc := nil;
  All := nil;
  FBusy := True;
  FParserState := psBusy;
  if Assigned(FOnStateChange) then
    FOnStateChange(Self, FParserState);
  BoolWorking := True;
  StartTick := GetTickCount;
  FUrl := Url;
  LoadingFromString := False;
  NoFramesFound := False;
  CoInitialize(nil);
end;

procedure TIEParser.Stop;
begin
  if Assigned(Doc) then
    Doc := nil;
  if Assigned(All) then
    All := nil;
  FreeAndNil(Element);
  FUrl := EmptyStr;
  FBusy := False;
  Finalize;
end;

procedure TIEParser.Parse(const aUrl: WideString);
 function ContainsAboutBlank(const s: string): Boolean;
  begin
    Result := Pos('about:blank', LowerCase(s)) > 0;
  end;

var
  E: IHtmlElement;
  X: Integer;
  Msg: TMsg;
  v, u: OleVariant;
  ParseTime, NoFramesContent, Us: string;
  HR: HRESULT;
begin
  Initialize;
  FUrl := aUrl;
  ProcessDoc(aUrl);
  NoFramesContent := '';
  if FUrl <> '' then
  begin
    HR := S_OK;
    if IEDownloadTools.IsValidURL(FUrl) then
      HR := LoadUrlFromMoniker(FUrl);
    if Failed(HR) and Assigned(FOnParseError) then
      FOnParseError(Self, GetLastError, FUrl, Err_Load_Mon + ResponseCodeToStr(HR))
    else if (Assigned(FOnParseDocument)) then
      FOnParseDocument(Self, HR, Succ_Load_Mon + ResponseCodeToStr(HR));
  end
  else
    HR := LoadFromString;

  if (Doc = nil) then
  begin
    if Failed(HR) and Assigned(FOnParseError) then
      FOnParseError(Self, GetLastError, FUrl, Err_Load_Str + ResponseCodeToStr(HR));
    Exit;
  end

  else
  begin
    while (BoolWorking and GetMessage(Msg, 0, 0, 0)) do
    begin
      if ((Msg.Message = WM_USER_STARTWALKING) and (Msg.hWnd = 0)) then
      begin
        BoolWorking := False;
        All := Doc.Get_all;
        if (All <> nil) and (All.length <= 4) then
        begin
          if Assigned(FOnParseError) then
            FOnParseError(Self, E_FAIL, FUrl, Doc_Error + ResponseCodeToStr(E_FAIL));
          Exit;
        end;
        if (All <> nil) and not FDownloadOnly then
          for x := 0 to All.length - 1 do
          begin
            E := All.item(x, 0) as IHTMLElement;
            with Element do
            begin
              ClassName := E.className;
              Id := E.id;
              TagName := E.tagName;
              Title := E.title;
              Language := E.language;
              SourceIndex := E.sourceIndex;
              RecordNumber := E.recordNumber;
              Lang := E.lang;
              OffsetLeft := E.offsetLeft;
              OffsetTop := E.offsetTop;
              OffsetWidth := E.offsetWidth;
              OffsetHeight := E.offsetHeight;
              InnerHtml := E.innerHtml;
              InnerText := E.innerText;
              OuterHtml := E.outerHtml;
              OuterText := E.outerText;
            end;
            if Assigned(FElement) then
              FElement(Self, Element);

            case StrToCase(E.TagName, ['FRAMESET', 'FRAME', 'NOFRAMES', 'A', '!',
              'COMMENT', 'IMG', 'BODY', 'BASE', 'BASEFONT', 'FONT', 'META', 'MARQUEE',
                'FORM', 'SCRIPT', 'DIV', 'HR', 'BR']) of
              0: // FRAMESET
                begin
                  if Assigned(FOnFrameSet) then
                    with All.item(x, 0) as IHTMLFrameSetElement do
                      FOnFrameSet(Self, Rows, Cols, FrameBorder, Name,
                        Border, BorderColor, FrameSpacing, Element);
                end;
              1: // FRAME
                begin
                  if Assigned(FOnFrame) then
                  begin
                    v := E.GetAttribute('Name', 0);
                    u := E.GetAttribute('Src', 0); // JohnS ('Source' -> 'Src')
                    Us := u;
                    if LoadingFromString and ContainsAboutBlank(Us) then
                      Delete(Us, 1, 11);
                    FOnFrame(Self, Us, v, Element);
                  end;
                end;
              2: // NOFRAMES
                begin
                  NoFramesContent := E.InnerHtml;
                  if Assigned(FOnNoFrame) then
                    FOnNoFrame(Self, Element);
                end;
              3: // A
                begin
                  if Assigned(FAnchor) then
                    with All.item(x, 0) as IHTMLAnchorElement do
                    begin
                      Us := hRef;
                      if LoadingFromString and ContainsAboutBlank(Us) then
                        Delete(Us, 1, 11);
                      FAnchor(Self, Us, target, rel, rev, Urn, Methods,
                        name, host, hostname, pathname, port, protocol,
                        Search, Hash, AccessKey,
                        ProtocolLong, MimeType, NameProp, Element);
                    end;
                end;
              4 or 5: // !, COMMENT
                begin
                  if Assigned(FComment) then
                    with All.item(x, 0) as IHTMLCommentElement do
                      FComment(Self, Text, Element)
                  else
                    if (E.TagName = 'IMG') and Assigned(FImage) then
                      with All.item(x, 0) as IHtmlImgElement do
                      begin
                        Us := Src;
                        if LoadingFromString and ContainsAboutBlank(Us) then
                          Delete(Us, 1, 11);
                        FImage(Self, Us, All.item(x, 0) as IHTMLImgElement, Element);
                      end;
                end;

              6: // IMG
                begin
                  if Assigned(FImage) then
                     with All.item(x, 0) as IHTMLImgElement do
                    begin
                      Us := Src;
                      if LoadingFromString and ContainsAboutBlank(Us) then
                        Delete(Us, 1, 11);
                      FImage(Self, Us, All.item(x, 0) as IHTMLImgElement, Element);
                    end;
                end;
              7: // BODY
                begin
                  if Assigned(FBody) then
                    with All.item(x, 0) as IHTMLBodyElement do
                      FBody(Self, Background, bgProperties,
                        LeftMargin, TopMargin, RightMargin, BottomMargin, bgColor, Text, Link,
                        vLink, aLink, NoWrap, Element);
                end;
              8: // BASE
                begin
                  if Assigned(FBase) then
                    with All.item(x, 0) as IHTMLBaseElement do
                    begin
                      Us := hRef;
                      if LoadingFromString and ContainsAboutBlank(Us) then
                        Delete(Us, 1, 11);
                      FBase(Self, Us, Target, Element);
                    end;
                end;
              9: // BASEFONT
                begin
                  if Assigned(FBaseFont) then
                    with All.item(x, 0) as IHTMLBaseFontElement do
                      FBaseFont(Self, color, face, size, Element);
                end;
              10: // FONT
                begin
                  if Assigned(FFont) then
                    with All.item(x, 0) as IHTMLFontElement do
                      FFont(Self, color, size, face, Element);
                end;
              11: // META
                begin
                  if Assigned(FMeta) then
                    with All.item(x, 0) as IHTMLMetaElement do
                      FMeta(Self, httpEquiv, content, name, url,
                        charset, Element);
                end;
              12: // MARQUEE
                begin
                  if Assigned(FMarquee) then
                    with All.item(x, 0) as IHTMLMarqueeElement do
                      FMarquee(Self, bgColor, Width, Height, Direction, Behavior,
                        ScrollAmount, ScrollDelay, Loop, vSpace, hSpace, Element);
                end;
              13: // FORM
                begin
                  if Assigned(FForm) then
                    with All.item(x, 0) as IHTMLFormElement do
                      FForm(Self, Action, Dir, Encoding, Method,
                        Target, Name, Element);
                end;
              14: // SCRIPT
                begin
                  if Assigned(FScript) then
                    with All.item(x, 0) as IHTMLScriptElement do
                    begin
                      Us := Src;
                      if LoadingFromString and ContainsAboutBlank(Us) then
                        Delete(Us, 1, 11);
                      FScript(Self, Us, All.item(x, 0) as IHTMLScriptElement, Element);
                    end;

                end;
              15: // DIV - containers
                begin
                  if Assigned(FDiv) then
                    with All.item(x, 0) as IHTMLDivElement do
                      FDiv(Self, Align, NoWrap, Element);
                end;
              16: // HR - horizontal rule
                begin
                  if Assigned(FHr) then
                    with All.item(x, 0) as IHTMLHRElement do
                      FHr(Self, Align, Color, Width, Size, NoShade, Element);
                end;
              17: // BR - line break
                begin
                  if Assigned(FBr) then
                    with All.item(x, 0) as IHTMLBRElement do
                      FBr(Self, Clear, Element);

                end;
            end;
          end;
        GetPageProperties;
      end
      else
        DispatchMessage(Msg);
    end;
  end;
  FUrl := '';
  if (NoFramesFound) and (ParseNoFrames) then
  begin
    FHtml := NoFramesContent;
    Parse(Url);
  end;
  if Assigned(FOnParseComplete) then
    FOnParseComplete(Self, Doc, All);
  ParseTime := FormatTickToTime(GetTickCount - StartTick);
  if (Assigned(FOnParseDocument)) then
    FOnParseDocument(Self, S_OK, Done + ' Process Time: ' + ParseTime);
  Finalize;
end;

procedure TIEParser.Finalize;
begin
  FHtml := '';
  FBusy := False;
  FParserState := psStopped;
  CoUninitialize;
  if Assigned(FOnStateChange) then
    FOnStateChange(Self, FParserState);
end;

function TProxySettings.SetProxy(const FullUserAgent, ProxyServer: string):
  Boolean;
var
  intList: INTERNET_PER_CONN_OPTION_List;
  dwBufSize: DWORD;
  hInternet: Pointer;
  intOptions: array[1..3] of INTERNET_PER_CONN_OPTION;
begin
  Result := False;
  dwBufSize := SizeOf(intList);
  intList.dwSize := SizeOf(intList);
  intList.pszConnection := nil;
  intList.dwOptionCount := High(intOptions);
  // the highest index of the array (in this case 3)
  intOptions[1].dwOption := INTERNET_PER_CONN_FLAGS;
  intOptions[1].Value.dwValue := PROXY_TYPE_DIRECT or PROXY_TYPE_PROXY;
  intOptions[2].dwOption := INTERNET_PER_CONN_PROXY_SERVER;
  intOptions[2].Value.pszValue := PChar(ProxyServer);
  intOptions[3].dwOption := INTERNET_PER_CONN_PROXY_BYPASS;
  intOptions[3].Value.pszValue := '<local>';
  intList.intOptions := @intOptions;
  hInternet := InternetOpen(PChar(FullUserAgent), INTERNET_OPEN_TYPE_DIRECT,
    nil, nil, 0);
  if hInternet <> nil then
  try
    Result := InternetSetOption(hInternet,
      INTERNET_OPTION_PER_CONNECTION_OPTION,
      @intList, dwBufSize);
    Result := Result and InternetSetOption(hInternet, INTERNET_OPTION_REFRESH,
      nil, 0);
  finally
    InternetCloseHandle(hInternet)
  end;
end;
{End of Proxy Settings-----------------------------------------------------------}


initialization
  OleInitialize(nil);

finalization
  try
    OleUninitialize;
  except
  end;

end.


