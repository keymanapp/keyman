//*************************************************************
//                       EwbBehaviorsComp                     *
//                                                            *
//                     Freeware Component                     *
//                       For Delphi                           *
//                            by                              *
//                     Serge Voloshenyuk                      *
//      Developing Team:                                      *
//          Serge Voloshenyuk (SergeV@bsalsa.com)             *
//          Eran Bodankin (bsalsa) -(bsalsa@gmail.com)       *
//                                                            *
//       Documentation and updated versions:                  *
//                                                            *
//               http://www.bsalsa.com                        *
//*************************************************************
{LICENSE:
THIS SOFTWARE IS PROVIDED TO YOU "AS IS" WITHOUT WARRANTY OF ANY KIND,
EITHER EXPRESSED OR IMPLIED INCLUDING BUT NOT LIMITED TO THE APPLIED
WARRANTIES OF MERCHANTABILITY AND/OR FITNESS FOR A PARTICULAR PURPOSE.
YOU ASSUME THE ENTIRE RISK AS TO THE ACCURACY AND THE USE OF THE SOFTWARE
AND ALL OTHER RISK ARISING OUT OF THE USE OR PERFORMANCE OF THIS SOFTWARE
AND DOCUMENTATION. BSALSA PRODUCTIONS DOES NOT WARRANT THAT THE SOFTWARE IS ERROR-FREE
OR WILL OPERATE WITHOUT INTERRUPTION. THE SOFTWARE IS NOT DESIGNED, INTENDED
OR LICENSED FOR USE IN HAZARDOUS ENVIRONMENTS REQUIRING FAIL-SAFE CONTROLS,
INCLUDING WITHOUT LIMITATION, THE DESIGN, CONSTRUCTION, MAINTENANCE OR
OPERATION OF NUCLEAR FACILITIES, AIRCRAFT NAVIGATION OR COMMUNICATION SYSTEMS,
AIR TRAFFIC CONTROL, AND LIFE SUPPORT OR WEAPONS SYSTEMS. BSALSA PRODUCTIONS SPECIFICALLY
DISCLAIMS ANY EXPRESS OR IMPLIED WARRANTY OF FITNESS FOR SUCH PURPOSE.

You may use/ change/ modify the component under 3 conditions:
1. In your website, add a link to "http://www.bsalsa.com"
2. In your application, add credits to "Embedded Web Browser"
3. Mail me  (bsalsa@gmail.com) any code change in the unit  for the benefit
   of the other users.
4. Please, consider donation in our web site!
{*******************************************************************************}
//$Id: EwbBehaviorsComp.pas,v 1.1.2.1 2006/11/29 22:13:00 sergev Exp $

unit EwbBehaviorsComp;

interface

{$I EWB.inc}

uses
{$IFDEF DELPHI6_UP}Variants, {$ENDIF}
  Windows, Classes, Graphics, ActiveX, Mshtml_Ewb, EwbAcc, EwbClasses, EwbEvents,
  EwbEventsComp;

type
  TEwbBehaviorFactory = class;
  TBinBehavior = class;
  TEwbBehaviorController = class;

  TPainterProperty = (
    ppOpaque,
    ppTransparent,
    //NOIMPL     ppAlpha,
    //NOIMPL     ppComplex,
    ppOverlay,
    ppHitTest,
    ppSurface,
    pp3DSurface,
    //NOIMPL     ppNoBand,
    ppNoDC,
    ppNoPhysicalClip,
    ppNoSaveDC,
    ppSupportsXForm,
    ppExpand,
    ppNoScrollBits
    );
  TPainterProperties = set of TPainterProperty;

  TPaintZOrder = (
    pzNone,
    pzReplaceAll,
    pzReplaceContent,
    pzReplaceBackground,
    pzBelowContent,
    pzBelowFlow,
    pzAboveFlow,
    pzAboveContent,
    pzWinTop
    );

  TPaintEventFlag = (pfTarget, pfSetCursor);
  TPaintEventFlags = set of TPaintEventFlag;

  TLayoutMode = (
    lmNone,
    lmFullDelegation,
    lmModifyNatural,
    lmMapSize
    );

  {events}
  TBehaviorNameEvent = procedure(Sender: TEwbBehaviorController;
    Element: IHTMLElement2; var aName: WideString) of object;
  TCreateBehaviorEvent = function(Sender: TEwbBehaviorController;
    const bstrBehavior, bstrBehaviorUrl: WideString;
    pSite: IElementBehaviorSite): IElementBehavior of object;

  TGetIdOfNameEvent = procedure(Sender: TBinBehavior; const name: widestring; var id: Integer) of object;
  TInvokeEvent = procedure(Sender: TBinBehavior; DispID: Integer;
    VarResult: POleVariant; Params: TDispParams; var Rezult: HRESULT) of object;
  TGetPropertyEvent = procedure(Sender: TBinBehavior; DispID: Integer;
    VarResult: POleVariant; Params: TDispParams; var Rezult: HRESULT) of object;
  TPutPropertyEvent = procedure(Sender: TBinBehavior; DispID: Integer;
    Params: TDispParams; var Rezult: HRESULT) of object;


  TBehaviorNotifyEvent = procedure(Sender: TBinBehavior) of object;
  TPainterDrawEvent = procedure(Sender: TBinBehavior;
    rcBounds, rcUpdate: TRect; lDrawFlags: Integer; Canvas: TCanvas) of object;
  TPainterDirectDrawEvent = procedure(Sender: TBinBehavior;
    rcBounds, rcUpdate: TRect; lDrawFlags: Integer; DrawObject: Pointer) of
    object;
  TPainterResizeEvent = procedure(Sender: TBinBehavior; size: TSize) of object;
  TPainterInfoEvent = procedure(Sender: TBinBehavior; var pInfo:
    _HTML_PAINTER_INFO) of object;
  TPainterHitTestEvent = procedure(Sender: TBinBehavior; pt: TPoint; var pbHit:
    BOOL;
    var plPartID: Longint) of object;

  TLayoutSizeEvent = procedure(Sender: TBinBehavior; dwFlags: Integer;
    sizeContent: TSize; var pptTranslateBy: TPoint;
    var pptTopLeft: TPoint; var psizeProposed: TSize) of object;
  TLayoutPositionEvent = procedure(Sender: TBinBehavior; lFlags: Integer; var
    pptTopLeft: TPoint) of object;
  TLayoutMapSizeEvent = procedure(Sender: TBinBehavior; psizeIn: PSize; var
    prcOut: TRect) of object;
  TLayoutTextDescentEvent = procedure(Sender: TBinBehavior; var plDescent:
    Integer) of object;

  TEventTargetEvent = procedure(Sender: TBinBehavior; var ppElement:
    IHTMLElement) of object;
  TSetCursorEvent = procedure(Sender: TBinBehavior; lPartID: Integer) of object;
  TStringFromPartIDEvent = procedure(Sender: TBinBehavior; lPartID: Integer; var
    pbstrPart: WideString) of object;
  TOverlayMoveEvent = procedure(Sender: TBinBehavior; rcDevice: TRect) of
    object;
  TGetFocusRectEvent = procedure(Sender: TBinBehavior; var pRect: TRect) of
    object;
  TGetSubmitInfoEvent = procedure(Sender: TBinBehavior; pSubmitData:
    IHTMLSubmitData) of object;
  TResetSubmitEvent = procedure(Sender: TBinBehavior) of object;

  TFindBehaviorEvent = procedure(Sender: TObject;
    const bstrBehavior, bstrBehaviorUrl: WideString;
    pSite: IElementBehaviorSite; var ppBehavior: IElementBehavior) of object;

  TResolveNSEvent = procedure(Sender: TObject;
    const bstrNamespace, bstrTagName, bstrAttrs: WideString;
    pNamespace: IElementNamespace) of object;

  TCreateNamespaceEvent = procedure(Sender: TObject;
    pNamespace: IElementNamespace) of object;

  TCreateNSWithImplEvent = procedure(Sender: TObject;
    pNamespace: IElementNamespace;
    const bstrImplementation: WideString) of object;

  TEwbBehaviorFactory = class(TComponent
      , IElementBehaviorFactory
      , IElementNamespaceFactoryCallback
      , IElementNamespaceFactory
      , IElementNamespaceFactory2
      )
  private
    FOnFindBehavior: TFindBehaviorEvent;
    FOnResolveNS: TResolveNSEvent;
    FOnCreateNS: TCreateNamespaceEvent;
    FOnCreateNSWithImpl: TCreateNSWithImplEvent;
  protected
    {IElementBehaviorFactory}
    function FindBehavior(const bstrBehavior: WideString; const bstrBehaviorUrl:
      WideString;
      const pSite: IElementBehaviorSite; out ppBehavior: IElementBehavior):
      HRESULT; stdcall;
    {IElementNamespaceFactoryCallback}
    function Resolve(const bstrNamespace: WideString; const bstrTagName:
      WideString;
      const bstrAttrs: WideString; pNamespace: IElementNamespace): HRESULT;
      stdcall;
    {IElementNamespaceFactory}
    function IElementNamespaceFactory.create = FactoryCreate;
    function FactoryCreate(pNamespace: IElementNamespace): HRESULT; stdcall;
    {IElementNamespaceFactory2}
    function IElementNamespaceFactory2.create = FactoryCreate;
    function CreateWithImplementation(pNamespace: IElementNamespace;
      const bstrImplementation: WideString): HRESULT; stdcall;
  published
    property OnFindBehavior: TFindBehaviorEvent read FOnFindBehavior write
      FOnFindBehavior;
    property OnResolveNS: TResolveNSEvent read FOnResolveNS write FOnResolveNS;
    property OnCreateNS: TCreateNamespaceEvent read FOnCreateNS write
      FOnCreateNS;
    property OnCreateNSWithImpl: TCreateNSWithImplEvent read FOnCreateNSWithImpl
      write FOnCreateNSWithImpl;
  end;

  TEwbBehaviorController = class(THtmlListener, IElementBehaviorFactory)
  private
    FBehaviors: TList;
    FZOrder: TPaintZOrder;
    FPainterProperties: TPainterProperties;
    FPainterFlags: Integer;
    FOnDetach: TBehaviorNotifyEvent;
    FOnApplyStyle: TBehaviorNotifyEvent;
    FOnDocContextChange: TBehaviorNotifyEvent;
    FOnContentReady: TBehaviorNotifyEvent;
    FOnInit: TBehaviorNotifyEvent;
    FOnContentSave: TBehaviorNotifyEvent;
    FOnDocReady: TBehaviorNotifyEvent;
    FOnDraw: TPainterDrawEvent;
    FOnHitTest: TPainterHitTestEvent;
    FOnPainterInfo: TPainterInfoEvent;
    FOnResize: TPainterResizeEvent;
    FOnGetName: TBehaviorNameEvent;
    FOnCreateBehavior: TCreateBehaviorEvent;
    fHandleEvents: Boolean;
    FLayoutMode: TLayoutMode;
    fLayoutMapSize: TLayoutMapSizeEvent;
    fLayoutPosition: TLayoutPositionEvent;
    FLayoutSize: TLayoutSizeEvent;
    FLayoutTextDescent: Integer;
    FOnLayoutTextDescent: TLayoutTextDescentEvent;
    fPaintEventInfo: TPaintEventFlags;
    FOnEventTarget: TEventTargetEvent;
    FOnSetCursor: TSetCursorEvent;
    FOnStringFromPartID: TStringFromPartIDEvent;
    FOnOverlayMove: TOverlayMoveEvent;
    FOnDirectDraw: TPainterDirectDrawEvent;
    FOnGetFocusRect: TGetFocusRectEvent;
    FOnGetSubmitInfo: TGetSubmitInfoEvent;
    FOnResetSubmit: TResetSubmitEvent;
    FOnGetIdOfName: TGetIdOfNameEvent;
    FOnGetProperty: TGetPropertyEvent;
    FOnInvoke: TInvokeEvent;
    FOnPutProperty: TPutPropertyEvent;
    fAlive: Boolean;
    procedure setPainterProperties(const Value: TPainterProperties);
    procedure setZOrder(const Value: TPaintZOrder);
    procedure setLayoutMode(const Value: TLayoutMode);
    procedure setLayoutTextDescent(const Value: Integer);
  protected
    {IElementBehaviorFactory}
    function FindBehavior(const bstrBehavior: WideString; const bstrBehaviorUrl:
      WideString;
      const pSite: IElementBehaviorSite; out ppBehavior: IElementBehavior):
      HRESULT; stdcall;

  protected
    procedure Add(aBehavior: TBinBehavior);
    procedure Remove(aBehavior: TBinBehavior);

    procedure DoGetIdOfName(Sender: TBinBehavior; const name: widestring; var id: Integer); virtual;
    procedure DoInvoke(Sender: TBinBehavior; DispID: Integer;
      VarResult: POleVariant; Params: TDispParams; var Rezult: HRESULT); virtual;
    procedure DoGetProperty(Sender: TBinBehavior; DispID: Integer;
      VarResult: POleVariant; Params: TDispParams; var Rezult: HRESULT); virtual;
    procedure DoPutProperty(Sender: TBinBehavior; DispID: Integer;
      Params: TDispParams; var Rezult: HRESULT); virtual;

    procedure DoInit(Sender: TBinBehavior); virtual;
    procedure DoDetach(Sender: TBinBehavior); virtual;
    procedure DoNotify(Sender: TBinBehavior; lEvent: Integer); virtual;
    procedure DoDraw(Sender: TBinBehavior;
      rcBounds, rcUpdate: TRect; lDrawFlags: Integer; Canvas: TCanvas); virtual;
    procedure DoDirectDraw(Sender: TBinBehavior;
      rcBounds, rcUpdate: TRect; lDrawFlags: Integer; pvDrawObject: Pointer);
      virtual;
    procedure DoPainterResize(Sender: TBinBehavior; size: TSize); virtual;
    procedure GetPainterInfo(Sender: TBinBehavior; var pInfo:
      _HTML_PAINTER_INFO); virtual;
    procedure DoHitTestPoint(Sender: TBinBehavior; pt: TPoint; var pbHit: BOOL;
      var plPartID: Longint); virtual;
    procedure DoLayoutSize(Sender: TBinBehavior; dwFlags: Integer; sizeContent:
      TSize; var pptTranslateBy: TPoint;
      var pptTopLeft: TPoint; var psizeProposed: TSize); virtual;
    procedure DoLayoutPosition(Sender: TBinBehavior; lFlags: Integer; var
      pptTopLeft: TPoint); virtual;
    procedure DoLayoutMapSize(Sender: TBinBehavior; psizeIn: PSize; var prcOut:
      TRect); virtual;
    procedure DoTextDescent(Sender: TBinBehavior; var plDescent: Integer);
      virtual;

    procedure DoEventTarget(Sender: TBinBehavior; var ppElement: IHTMLElement);
      virtual;
    procedure DoSetCursor(Sender: TBinBehavior; lPartID: Integer); virtual;
    function DoStringFromPartID(Sender: TBinBehavior; lPartID: Integer; out
      pbstrPart: WideString): Boolean; virtual;
    procedure DoOverlayMove(Sender: TBinBehavior; rcDevice: TRect); virtual;
    procedure DoGetFocusRect(Sender: TBinBehavior; var pRect: TRect); virtual;
    procedure DoGetSubmitInfo(Sender: TBinBehavior; pSubmitData:
      IHTMLSubmitData); virtual;
    procedure DoResetSubmit(Sender: TBinBehavior); virtual;
  public
    destructor Destroy; override;

    procedure InvalidatePainterInfo;
    procedure InvalidateLayoutInfo;

    function Attach(Element: IHTMLElement2): Integer; overload;
    function Attach(Element: IHTMLElement): Integer; overload;
  published
    property Alive: Boolean read fAlive write fAlive;
    property ZOrder: TPaintZOrder read FZOrder write setZOrder default pzNone;
    property LayoutMode: TLayoutMode read FLayoutMode write setLayoutMode default lmNone;
    property PainterProperties: TPainterProperties read FPainterProperties write
      setPainterProperties;
    property HandleEvents: Boolean read fHandleEvents write fHandleEvents;
    property LayoutTextDescent: Integer read FLayoutTextDescent write
      setLayoutTextDescent;
    property PaintEventInfo: TPaintEventFlags read fPaintEventInfo write
      fPaintEventInfo;

    property OnGetName: TBehaviorNameEvent read FOnGetName write FOnGetName;
    property OnInvoke: TInvokeEvent read FOnInvoke write FOnInvoke;
    property OnGetProperty: TGetPropertyEvent read FOnGetProperty write FOnGetProperty;
    property OnPutProperty: TPutPropertyEvent read FOnPutProperty write FOnPutProperty;


    property OnCreateBehavior: TCreateBehaviorEvent read FOnCreateBehavior write
      FOnCreateBehavior;

    property OnGetIdOfName: TGetIdOfNameEvent read FOnGetIdOfName write FOnGetIdOfName;

    property OnInit: TBehaviorNotifyEvent read FOnInit write FOnInit;
    property OnDetach: TBehaviorNotifyEvent read FOnDetach write FOnDetach;
    property OnContentReady: TBehaviorNotifyEvent read FOnContentReady write
      FOnContentReady;
    property OnDocReady: TBehaviorNotifyEvent read FOnDocReady write
      FOnDocReady;
    property OnApplyStyle: TBehaviorNotifyEvent read FOnApplyStyle write
      FOnApplyStyle;
    property OnDocContextChange: TBehaviorNotifyEvent read FOnDocContextChange
      write FOnDocContextChange;
    property OnContentSave: TBehaviorNotifyEvent read FOnContentSave write
      FOnContentSave;

    property OnDraw: TPainterDrawEvent read FOnDraw write FOnDraw;
    property OnDirectDraw: TPainterDirectDrawEvent read FOnDirectDraw write
      FOnDirectDraw;
    property OnPainterResize: TPainterResizeEvent read FOnResize write
      FOnResize;
    property OnPainterInfo: TPainterInfoEvent read FOnPainterInfo write
      FOnPainterInfo;
    property OnPainterHitTest: TPainterHitTestEvent read FOnHitTest write
      FOnHitTest;

    property OnOverlayMove: TOverlayMoveEvent read FOnOverlayMove write
      FOnOverlayMove;

    property OnEventTarget: TEventTargetEvent read FOnEventTarget write
      FOnEventTarget;
    property OnSetCursor: TSetCursorEvent read FOnSetCursor write FOnSetCursor;
    property OnStringFromPartID: TStringFromPartIDEvent read FOnStringFromPartID
      write FOnStringFromPartID;

    property OnLayoutSize: TLayoutSizeEvent read FLayoutSize write FLayoutSize;
    property OnLayoutPosition: TLayoutPositionEvent read fLayoutPosition write
      fLayoutPosition;
    property OnLayoutMapSize: TLayoutMapSizeEvent read fLayoutMapSize write
      fLayoutMapSize;
    property OnLayoutTextDescent: TLayoutTextDescentEvent read
      FOnLayoutTextDescent write FOnLayoutTextDescent;

    property OnGetFocusRect: TGetFocusRectEvent read FOnGetFocusRect write
      FOnGetFocusRect;
    property OnGetSubmitInfo: TGetSubmitInfoEvent read FOnGetSubmitInfo write
      FOnGetSubmitInfo;
    property OnResetSubmit: TResetSubmitEvent read FOnResetSubmit write
      FOnResetSubmit;
  end;

  TBihState = set of 0..7;

  TBinBehavior = class(TInterfacedDispatchObject
      , IElementBehavior
      , IHTMLPainter
      , IHTMLPainterEventInfo
      , IHTMLPainterOverlay
      , IElementBehaviorLayout
      , IElementBehaviorLayout2
      , IElementBehaviorFocus
      , IElementBehaviorSubmit
      )
  private
    fController: TEwbBehaviorController;
    fSite: IElementBehaviorSite;
    FElement: IHTMLElement;
    FEventsLink: IHubLink;
    fState: TBihState;
    function getSiteOM: IElementBehaviorSiteOM;
    function getPaintSite: IHTMLPaintSite;
    function getDefaults: IHTMLElementDefaults;
    function getSiteLayout: IElementBehaviorSiteLayout;
    function getSiteLayout2: IElementBehaviorSiteLayout2;
  protected
    {IDispatch}
    function GetIDsOfNames(const IID: TGUID; Names: Pointer;
      NameCount, LocaleID: Integer; DispIDs: Pointer): HRESULT; override;
      stdcall;
    function Invoke(DispID: Integer; const IID: TGUID; LocaleID: Integer;
      Flags: Word; var Params; VarResult, ExcepInfo, ArgErr: Pointer): HRESULT;
      override; stdcall;
    {IElementBehavior}
    function Init(pBehaviorSite: IElementBehaviorSite): HRESULT; stdcall;
    function Notify(lEvent: Integer; var pVar: OleVariant): HRESULT; stdcall;
    function Detach: HRESULT; stdcall;
    {IHTMLPainter}
    function IHTMLPainter.Draw = PainterDraw;
    function PainterDraw(rcBounds, rcUpdate: TRect; lDrawFlags: Integer;
      hdc: hdc; pvDrawObject: Pointer): HRESULT; stdcall;
    function IHTMLPainter.onresize = PainterResize;
    function PainterResize(size: TSize): HRESULT; stdcall;
    function IHTMLPainter.GetPainterInfo = PainterInfo;
    function PainterInfo(out pInfo: _HTML_PAINTER_INFO): HRESULT; stdcall;
    function IHTMLPainter.HitTestPoint = PainterHitTestPoint;
    function PainterHitTestPoint(pt: TPoint; out pbHit: BOOL; out plPartID:
      Longint): HRESULT; stdcall;
    {IHTMLPainterEventInfo}
    function GetEventInfoFlags(out plEventInfoFlags: Integer): HRESULT; stdcall;
    function GetEventTarget(var ppElement: IHTMLElement): HRESULT; stdcall;
    function SetCursor(lPartID: Integer): HRESULT; stdcall;
    function StringFromPartID(lPartID: Integer; out pbstrPart: WideString):
      HRESULT; stdcall;
    {IHTMLPainterOverlay}
    function IHTMLPainterOverlay.onmove = onOverlayMove;
    function onOverlayMove(rcDevice: TRect): HRESULT; stdcall;
    {IElementBehaviorLayout}
    function GetSize(dwFlags: Integer; sizeContent: TSize; var pptTranslateBy:
      TPoint;
      var pptTopLeft: TPoint; var psizeProposed: TSize): HRESULT; stdcall;
    function GetLayoutInfo(out plLayoutInfo: Integer): HRESULT; stdcall;
    function GetPosition(lFlags: Integer; var pptTopLeft: TPoint): HRESULT;
      stdcall;
    function MapSize(psizeIn: PSize; out prcOut: TRect): HRESULT; stdcall;
    {IElementBehaviorLayout2}
    function GetTextDescent(out plDescent: Integer): HRESULT; stdcall;
    {IElementBehaviorFocus}
    function GetFocusRect(var pRect: TRect): HRESULT; stdcall;
    {IElementBehaviorSubmit}
    function GetSubmitInfo(pSubmitData: IHTMLSubmitData): HRESULT; stdcall;
    function IElementBehaviorSubmit.reset = ResetSubmit;
    function ResetSubmit: HRESULT; stdcall;
  protected
    function getBoolProp(const Index: Integer): Boolean;
    procedure SetBoolProp(const Index: Integer; const Value: Boolean);
  public
    constructor Create(aController: TEwbBehaviorController);
    destructor Destroy; override;

    procedure ConnectToEvents;
    procedure DisconnectFromEvents;
    property Controller: TEwbBehaviorController read fController;
    property Site: IElementBehaviorSite read fSite;
    property SiteOM: IElementBehaviorSiteOM read getSiteOM;
    property ContextReady: Boolean index 0 read getBoolProp;
    property DocumentReady: Boolean index 1 read getBoolProp;
    property Element: IHTMLElement read FElement;
    property PaintSite: IHTMLPaintSite read getPaintSite;
    property Defaults: IHTMLElementDefaults read getDefaults;
    property SiteLayout: IElementBehaviorSiteLayout read getSiteLayout;
    property SiteLayout2: IElementBehaviorSiteLayout2 read getSiteLayout2;
  end;

implementation
uses SysUtils;

const

  _zorders: array[TPaintZOrder] of Integer = (
    HTMLPAINT_ZORDER_NONE,
    HTMLPAINT_ZORDER_REPLACE_ALL,
    HTMLPAINT_ZORDER_REPLACE_CONTENT,
    HTMLPAINT_ZORDER_REPLACE_BACKGROUND,
    HTMLPAINT_ZORDER_BELOW_CONTENT,
    HTMLPAINT_ZORDER_BELOW_FLOW,
    HTMLPAINT_ZORDER_ABOVE_FLOW,
    HTMLPAINT_ZORDER_ABOVE_CONTENT,
    HTMLPAINT_ZORDER_WINDOW_TOP
    );

  _pproperties: array[TPainterProperty] of Integer = (
    HTMLPAINTER_OPAQUE,
    HTMLPAINTER_TRANSPARENT,
    //NOIMPL   HTMLPAINTER_ALPHA,
    //NOIMPL   HTMLPAINTER_COMPLEX,
    HTMLPAINTER_OVERLAY,
    HTMLPAINTER_HITTEST,
    HTMLPAINTER_SURFACE,
    HTMLPAINTER_3DSURFACE,
    //NOIMPL   HTMLPAINTER_NOBAND,
    HTMLPAINTER_NODC,
    HTMLPAINTER_NOPHYSICALCLIP,
    HTMLPAINTER_NOSAVEDC,
    HTMLPAINTER_SUPPORTS_XFORM,
    HTMLPAINTER_EXPAND,
    HTMLPAINTER_NOSCROLLBITS
    );

  _layouts: array[TLayoutMode] of Integer = (
    0,
    BEHAVIORLAYOUTINFO_FULLDELEGATION,
    BEHAVIORLAYOUTINFO_MODIFYNATURAL,
    BEHAVIORLAYOUTINFO_MAPSIZE
    );

  { TEwbBehaviorFactory }

function TEwbBehaviorFactory.CreateWithImplementation(
  pNamespace: IElementNamespace;
  const bstrImplementation: WideString): HRESULT;
begin
  Result := S_OK;
  if Assigned(FOnCreateNSWithImpl) then
    FOnCreateNSWithImpl(Self, pNamespace, bstrImplementation)
  else if Assigned(FOnCreateNS) then
    FOnCreateNS(Self, pNamespace);
end;

function TEwbBehaviorFactory.FactoryCreate(pNamespace: IElementNamespace):
  HRESULT;
begin
  Result := S_OK;
  if Assigned(FOnCreateNS) then
    FOnCreateNS(Self, pNamespace);
end;

function TEwbBehaviorFactory.FindBehavior(const bstrBehavior,
  bstrBehaviorUrl: WideString; const pSite: IElementBehaviorSite;
  out ppBehavior: IElementBehavior): HRESULT;
begin
  ppBehavior := nil;
  if Assigned(FOnFindBehavior) then
    FOnFindBehavior(Self, bstrBehavior, bstrBehaviorUrl, pSite, ppBehavior);
  if ppBehavior = nil then
    Result := E_NOTIMPL
  else
    Result := S_OK;
end;

function TEwbBehaviorFactory.Resolve(const bstrNamespace, bstrTagName,
  bstrAttrs: WideString; pNamespace: IElementNamespace): HRESULT;
begin
  Result := S_OK;
  if Assigned(FOnResolveNS) then
    FOnResolveNS(Self, bstrNamespace, bstrTagName, bstrAttrs, pNamespace);
end;

{ TBinBehavior }

constructor TBinBehavior.Create(aController: TEwbBehaviorController);
begin
  inherited Create;
  aController.Add(Self);
end;

destructor TBinBehavior.Destroy;
begin
  FController.Remove(Self);
  inherited;
end;

function TBinBehavior.getBoolProp(const Index: Integer): Boolean;
begin
  Result := Index in fState;
end;

procedure TBinBehavior.SetBoolProp(const Index: Integer;
  const Value: Boolean);
begin
  if Value then
    Include(fState, Index)
  else
    Exclude(fState, Index);
end;

function TBinBehavior.getSiteOM: IElementBehaviorSiteOM;
begin
  if not Supports(fSite, IElementBehaviorSiteOM, Result) then
    Result := nil;
end;

function TBinBehavior.getPaintSite: IHTMLPaintSite;
begin
  if not Supports(fSite, IHTMLPaintSite, Result) then
    Result := nil;
end;

function TBinBehavior.getDefaults: IHTMLElementDefaults;
var
  OM2: IElementBehaviorSiteOM2;
begin
  if Supports(fSite, IElementBehaviorSiteOM2, OM2) then
    OM2.GetDefaults(Result)
  else
    Result := nil;
end;

function TBinBehavior.getSiteLayout: IElementBehaviorSiteLayout;
begin
  if not Supports(fSite, IElementBehaviorSiteLayout, Result) then
    Result := nil;
end;

function TBinBehavior.getSiteLayout2: IElementBehaviorSiteLayout2;
begin
  if not Supports(fSite, IElementBehaviorSiteLayout2, Result) then
    Result := nil;
end;

function TBinBehavior.Detach: HRESULT;
begin
  FController.DoDetach(Self);
  DisconnectFromEvents;
  FElement := nil;
  SetBoolProp(1, False);
  fSite := nil;
  SetBoolProp(0, False);
  Result := S_OK;
end;

procedure TBinBehavior.ConnectToEvents;
begin
  if FEventsLink = nil then
    FEventsLink := FController.Connect2(Element, Self);
end;

procedure TBinBehavior.DisconnectFromEvents;
begin
  if FEventsLink <> nil then
  try
    FEventsLink.Disconnect;
  finally
    FEventsLink := nil;
  end;
end;

function TBinBehavior.Init(pBehaviorSite: IElementBehaviorSite): HRESULT;
begin
  fSite := pBehaviorSite;
  FController.DoInit(Self);
  Result := S_OK;
end;

function TBinBehavior.Notify(lEvent: Integer;
  var pVar: OleVariant): HRESULT;
begin
  case lEvent of
    BEHAVIOREVENT_CONTENTREADY:
      begin
        SetBoolProp(0, True);
        Site.GetElement(FElement);
        if FController.HandleEvents then
          Self.ConnectToEvents;
      end;
    BEHAVIOREVENT_DOCUMENTREADY:
      SetBoolProp(1, True);
  end;
  FController.DoNotify(Self, lEvent);
  Result := S_OK;
end;

function TBinBehavior.PainterDraw(rcBounds, rcUpdate: TRect;
  lDrawFlags: Integer; hdc: hdc; pvDrawObject: Pointer): HRESULT;
var
  Canvas: TCanvas;
begin
  Result := S_OK;
  if hdc <> 0 then
  begin
    Canvas := TCanvas.Create;
    Canvas.Handle := hdc;
  end else if pvDrawObject <> nil then
  begin
    FController.DoDirectDraw(Self, rcBounds, rcUpdate, lDrawFlags,
      pvDrawObject);
    Exit;
  end else
    Canvas := nil;
  try
    FController.DoDraw(Self, rcBounds, rcUpdate, lDrawFlags, Canvas);
  finally
    Canvas.Free;
  end;
end;

function TBinBehavior.PainterHitTestPoint(pt: TPoint; out pbHit: BOOL;
  out plPartID: Integer): HRESULT;
begin
  pbHit := False;
  plPartID := 0;
  FController.DoHitTestPoint(Self, pt, pbHit, plPartID);
  Result := S_OK;
end;

function TBinBehavior.PainterInfo(out pInfo: _HTML_PAINTER_INFO): HRESULT;
begin
  pInfo.lZOrder := _zorders[FController.ZOrder];
  pInfo.lFlags := FController.FPainterFlags;
  FController.GetPainterInfo(Self, pInfo);
  Result := S_OK;
end;

function TBinBehavior.PainterResize(size: TSize): HRESULT;
begin
  FController.DoPainterResize(Self, size);
  Result := S_OK;
end;

function TBinBehavior.GetLayoutInfo(out plLayoutInfo: Integer): HRESULT;
begin
  plLayoutInfo := _layouts[FController.LayoutMode];
  Result := S_OK;
end;

function TBinBehavior.GetPosition(lFlags: Integer;
  var pptTopLeft: TPoint): HRESULT;
begin
  FController.DoLayoutPosition(Self, lFlags, pptTopLeft);
  Result := S_OK;
end;

function TBinBehavior.GetSize(dwFlags: Integer; sizeContent: TSize;
  var pptTranslateBy, pptTopLeft: TPoint;
  var psizeProposed: TSize): HRESULT;
begin
  FController.DoLayoutSize(Self, dwFlags, sizeContent,
    pptTranslateBy, pptTopLeft, psizeProposed);
  Result := S_OK;
end;

function TBinBehavior.MapSize(psizeIn: PSize; out prcOut: TRect): HRESULT;
begin
  FController.DoLayoutMapSize(Self, psizeIn, prcOut);
  Result := S_OK;
end;

function TBinBehavior.GetTextDescent(out plDescent: Integer): HRESULT;
begin
  FController.DoTextDescent(Self, plDescent);
  Result := S_OK;
end;

function TBinBehavior.GetEventInfoFlags(
  out plEventInfoFlags: Integer): HRESULT;
begin
  plEventInfoFlags := 0;
  with Self.FController do
  begin
    if pfTarget in PaintEventInfo then
      plEventInfoFlags := HTMLPAINT_EVENT_TARGET;
    if pfSetCursor in PaintEventInfo then
      plEventInfoFlags := plEventInfoFlags or HTMLPAINT_EVENT_SETCURSOR;
  end;
  Result := S_OK;
end;

function TBinBehavior.GetEventTarget(var ppElement: IHTMLElement): HRESULT;
begin
  FController.DoEventTarget(Self, ppElement);
  Result := S_OK;
end;

function TBinBehavior.SetCursor(lPartID: Integer): HRESULT;
begin
  FController.DoSetCursor(Self, lPartID);
  Result := S_OK;
end;

function TBinBehavior.StringFromPartID(lPartID: Integer;
  out pbstrPart: WideString): HRESULT;
begin
  if FController.DoStringFromPartID(Self, lPartID, pbstrPart) then
    Result := S_OK
  else
    Result := E_NOTIMPL;
end;

function TBinBehavior.onOverlayMove(rcDevice: TRect): HRESULT;
begin
  FController.DoOverlayMove(Self, rcDevice);
  Result := S_OK;
end;

function TBinBehavior.GetFocusRect(var pRect: TRect): HRESULT;
begin
  FController.DoGetFocusRect(Self, pRect);
  Result := S_OK;
end;

function TBinBehavior.GetSubmitInfo(
  pSubmitData: IHTMLSubmitData): HRESULT;
begin
  FController.DoGetSubmitInfo(Self, pSubmitData);
  Result := S_OK;
end;

function TBinBehavior.ResetSubmit: HRESULT;
begin
  FController.DoResetSubmit(Self);
  Result := S_OK;
end;

function TBinBehavior.GetIDsOfNames(const IID: TGUID; Names: Pointer;
  NameCount, LocaleID: Integer; DispIDs: Pointer): HRESULT;
var
  I: Integer;
  pname: WideString;
  id: Integer;
begin
  pname := PWideChar(Names^);
  for I := 0 to NameCount - 1 do
    PDispIDList(DispIDs)^[i] := -1;
  id := -1;
  FController.DoGetIdOfName(Self, pname, id);
  if id <> -1 then
  begin
    PDispIDList(DispIDs)^[0] := TDispID(id);
    Result := S_OK
  end else Result := E_NOTIMPL;
end;

function TBinBehavior.Invoke(DispID: Integer; const IID: TGUID;
  LocaleID: Integer; Flags: Word; var Params; VarResult, ExcepInfo,
  ArgErr: Pointer): HRESULT;
begin
  try
    Result := DISP_E_MEMBERNOTFOUND;
    if integer(DispID) <> -1 then
      if Flags = DISPATCH_METHOD then
        FController.DoInvoke(Self, DispID, POleVariant(VarResult),
          TDispParams(Params), Result)
      else
        case Flags and not DISPATCH_METHOD of
          DISPATCH_PROPERTYGET:
            FController.DoGetProperty(Self, DispID, POleVariant(VarResult),
              TDispParams(Params), Result);

          DISPATCH_PROPERTYPUT,
            DISPATCH_PROPERTYPUTREF,
            DISPATCH_PROPERTYPUT + DISPATCH_PROPERTYPUTREF:
            FController.DoPutProperty(Self, DispID, TDispParams(Params), Result);
        end;
  except
    on E: Exception do
      with PExcepInfo(ExcepInfo)^ do
      begin
        Result := DISP_E_EXCEPTION;
        wCode := 9999;
        bstrDescription := E.Message;
        bstrSource := E.ClassName;
        dwHelpContext := E.HelpContext;
      end;
  end;
end;

{ TEwbBehaviorController }

destructor TEwbBehaviorController.Destroy;
begin
  FreeAndNil(FBehaviors);
  inherited;
end;

procedure TEwbBehaviorController.Add(aBehavior: TBinBehavior);
begin
  aBehavior.fController := Self;
  if Alive then
  begin
    if FBehaviors = nil then
      FBehaviors := TList.Create;
    FBehaviors.Add(aBehavior);
  end;
end;

procedure TEwbBehaviorController.Remove(aBehavior: TBinBehavior);
begin
  aBehavior.fController := nil;
  if FBehaviors <> nil then
    FBehaviors.Remove(aBehavior);
end;

procedure TEwbBehaviorController.InvalidatePainterInfo;
var I: Integer;
begin
  if FBehaviors <> nil then
    for I := 0 to FBehaviors.Count - 1 do
      with TBinBehavior(FBehaviors[I]) do
        PaintSite.InvalidatePainterInfo;
end;

procedure TEwbBehaviorController.InvalidateLayoutInfo;
var I: Integer;
begin
  if FBehaviors <> nil then
    for I := 0 to FBehaviors.Count - 1 do
      with TBinBehavior(FBehaviors[I]) do
        SiteLayout.InvalidateLayoutInfo;
end;

procedure TEwbBehaviorController.setZOrder(const Value: TPaintZOrder);
begin
  if FZOrder <> Value then
  begin
    FZOrder := Value;
    InvalidatePainterInfo;
  end;
end;

procedure TEwbBehaviorController.setPainterProperties(
  const Value: TPainterProperties);
var
  lFlags: Integer;
  I: TPainterProperty;
begin
  lFlags := 0;
  FPainterProperties := Value;
  for I := Low(TPainterProperty) to High(TPainterProperty) do
    if I in Value then
      lFlags := lFlags or _pproperties[I];
  FPainterFlags := lFlags;
  InvalidatePainterInfo;
end;

procedure TEwbBehaviorController.setLayoutMode(const Value: TLayoutMode);
begin
  if FLayoutMode <> Value then
  begin
    FLayoutMode := Value;
    InvalidateLayoutInfo;
  end;
end;

procedure TEwbBehaviorController.setLayoutTextDescent(
  const Value: Integer);
begin
  if FLayoutTextDescent <> Value then
  begin
    FLayoutTextDescent := Value;
    InvalidateLayoutInfo;
  end;
end;

function TEwbBehaviorController.FindBehavior(const bstrBehavior,
  bstrBehaviorUrl: WideString; const pSite: IElementBehaviorSite;
  out ppBehavior: IElementBehavior): HRESULT;
begin
  if Assigned(FOnCreateBehavior) then
    ppBehavior := FOnCreateBehavior(Self, bstrBehavior, bstrBehaviorUrl, pSite);
  if ppBehavior = nil then
    ppBehavior := TBinBehavior.Create(Self) as IElementBehavior;
  Result := S_OK;
end;

function TEwbBehaviorController.Attach(Element: IHTMLElement2): Integer;
var
  aName: Widestring;
{$IFDEF DELPHI5_UP}
  SelfIntf: IElementBehaviorFactory;
{$ENDIF}
begin
  aName := '';
  if Assigned(FOnGetName) then
    FOnGetName(Self, Element, aName);
{$IFDEF DELPHI5_UP}
  GetInterface(IElementBehaviorFactory, SelfIntf);
  Result := Element.addBehavior(aName, SelfIntf);
{$ELSE}
  Result := Element.addBehavior(aName, Self as IElementBehaviorFactory);
{$ENDIF}
end;

function TEwbBehaviorController.Attach(Element: IHTMLElement): Integer;
var
  E: IHTMLElement2;
begin
  if Supports(Element, IHTMLElement2, E) then
    Result := Attach(E)
  else
    Result := 0;
end;

procedure TEwbBehaviorController.DoInit(Sender: TBinBehavior);
begin
  if Assigned(FOnInit) then
    FOnInit(Sender);
end;

procedure TEwbBehaviorController.DoDetach(Sender: TBinBehavior);
begin
  if Assigned(FOnDetach) then
    FOnDetach(Sender);
end;

procedure TEwbBehaviorController.DoDraw(Sender: TBinBehavior; rcBounds,
  rcUpdate: TRect; lDrawFlags: Integer; Canvas: TCanvas);
begin
  if Assigned(FOnDraw) then
    FOnDraw(Sender, rcBounds, rcUpdate, lDrawFlags, Canvas);
end;

procedure TEwbBehaviorController.DoDirectDraw(Sender: TBinBehavior;
  rcBounds, rcUpdate: TRect; lDrawFlags: Integer; pvDrawObject: Pointer);
begin
  if Assigned(FOnDirectDraw) then
    FOnDirectDraw(Sender, rcBounds, rcUpdate, lDrawFlags, pvDrawObject);
end;

procedure TEwbBehaviorController.DoHitTestPoint(Sender: TBinBehavior;
  pt: TPoint; var pbHit: BOOL; var plPartID: Integer);
begin
  if Assigned(FOnHitTest) then
    FOnHitTest(Sender, pt, pbHit, plPartID);
end;

procedure TEwbBehaviorController.DoPainterResize(Sender: TBinBehavior;
  Size: TSize);
begin
  if Assigned(FOnResize) then
    FOnResize(Sender, size);
end;

procedure TEwbBehaviorController.GetPainterInfo(Sender: TBinBehavior;
  var pInfo: _HTML_PAINTER_INFO);
begin
  if Assigned(FOnPainterInfo) then
    FOnPainterInfo(Sender, pInfo);
end;

procedure TEwbBehaviorController.DoNotify(Sender: TBinBehavior;
  lEvent: Integer);
begin
  case lEvent of
    BEHAVIOREVENT_CONTENTREADY:
      if Assigned(FOnContentReady) then
        FOnContentReady(Sender);
    BEHAVIOREVENT_DOCUMENTREADY:
      if Assigned(FOnDocReady) then
        FOnDocReady(Sender);
    BEHAVIOREVENT_DOCUMENTCONTEXTCHANGE:
      if Assigned(FOnDocContextChange) then
        FOnDocContextChange(Sender);
    BEHAVIOREVENT_CONTENTSAVE:
      if Assigned(FOnContentSave) then
        FOnContentSave(Sender);
    BEHAVIOREVENT_APPLYSTYLE:
      if Assigned(FOnApplyStyle) then
        FOnApplyStyle(Sender);
  end;
end;

procedure TEwbBehaviorController.DoLayoutMapSize(Sender: TBinBehavior;
  psizeIn: PSize; var prcOut: TRect);
begin
  if Assigned(fLayoutMapSize) then
    fLayoutMapSize(Sender, psizeIn, prcOut);
end;

procedure TEwbBehaviorController.DoLayoutPosition(Sender: TBinBehavior;
  lFlags: Integer; var pptTopLeft: TPoint);
begin
  if Assigned(fLayoutPosition) then
    fLayoutPosition(Sender, lFlags, pptTopLeft);
end;

procedure TEwbBehaviorController.DoLayoutSize(Sender: TBinBehavior;
  dwFlags: Integer; sizeContent: TSize; var pptTranslateBy,
  pptTopLeft: TPoint; var psizeProposed: TSize);
begin
  if Assigned(FLayoutSize) then
    FLayoutSize(Sender, dwFlags, sizeContent, pptTranslateBy, pptTopLeft,
      psizeProposed);
end;

procedure TEwbBehaviorController.DoTextDescent(Sender: TBinBehavior;
  var plDescent: Integer);
begin
  plDescent := Self.LayoutTextDescent;
  if Assigned(FOnLayoutTextDescent) then
    FOnLayoutTextDescent(Sender, plDescent);
end;

procedure TEwbBehaviorController.DoEventTarget(Sender: TBinBehavior;
  var ppElement: IHTMLElement);
begin
  if Assigned(FOnEventTarget) then
    FOnEventTarget(Sender, ppElement);
end;

procedure TEwbBehaviorController.DoSetCursor(Sender: TBinBehavior;
  lPartID: Integer);
begin
  if Assigned(FOnSetCursor) then
    FOnSetCursor(Sender, lPartID);
end;

function TEwbBehaviorController.DoStringFromPartID(Sender: TBinBehavior;
  lPartID: Integer; out pbstrPart: WideString): Boolean;
begin
  Result := Assigned(FOnStringFromPartID);
  if Result then
    FOnStringFromPartID(Sender, lPartID, pbstrPart);
end;

procedure TEwbBehaviorController.DoOverlayMove(Sender: TBinBehavior;
  rcDevice: TRect);
begin
  if Assigned(FOnOverlayMove) then
    FOnOverlayMove(Sender, rcDevice);
end;

procedure TEwbBehaviorController.DoGetFocusRect(Sender: TBinBehavior;
  var pRect: TRect);
begin
  if Assigned(FOnGetFocusRect) then
    FOnGetFocusRect(Sender, pRect);
end;

procedure TEwbBehaviorController.DoGetSubmitInfo(Sender: TBinBehavior;
  pSubmitData: IHTMLSubmitData);
begin
  if Assigned(FOnGetSubmitInfo) then
    FOnGetSubmitInfo(Sender, pSubmitData);
end;

procedure TEwbBehaviorController.DoResetSubmit(Sender: TBinBehavior);
begin
  if Assigned(FOnResetSubmit) then
    FOnResetSubmit(Sender);
end;

procedure TEwbBehaviorController.DoGetIdOfName(Sender: TBinBehavior;
  const name: widestring; var id: Integer);
begin
  if Assigned(FOnGetIdOfName) then
    FOnGetIdOfName(Sender, name, id);
end;

procedure TEwbBehaviorController.DoGetProperty(Sender: TBinBehavior;
  DispID: Integer; VarResult: POleVariant; Params: TDispParams;
  var Rezult: HRESULT);
begin
  if Assigned(FOnGetProperty) then
    FOnGetProperty(Sender, DispID, VarResult, Params, Rezult);
end;

procedure TEwbBehaviorController.DoInvoke(Sender: TBinBehavior;
  DispID: Integer; VarResult: POleVariant; Params: TDispParams;
  var Rezult: HRESULT);
begin
  if Assigned(FOnInvoke) then
    FOnInvoke(Sender, DispID, VarResult, Params, Rezult);
end;

procedure TEwbBehaviorController.DoPutProperty(Sender: TBinBehavior;
  DispID: Integer; Params: TDispParams; var Rezult: HRESULT);
begin
  if Assigned(FOnPutProperty) then
    FOnPutProperty(Sender, DispID, Params, Rezult);
end;

end.
