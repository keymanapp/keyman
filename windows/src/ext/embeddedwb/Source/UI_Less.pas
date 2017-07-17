{ ***********************************************************
//                     TUILess - UI_Less parser             *
//                       For Delphi                         *
//                                                          *
//                     Freeware Component                   *
//                            by                            *
//                     Per Lindsø Larsen                    *                                                       //                   per.lindsoe@larsen.dk                  *
//                     Developing Team:                     *
//                  bsalsa - bsalsa.com                     *
//  Documentation and updated versions:                     *
//                                                          *
//               http://www.bsalsa.com                      *
{*******************************************************************************}
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

You may use, change or modify the component under 4 conditions:
1. In your website, add a link to "http://www.bsalsa.com"
2. In your application, add credits to "Embedded Web Browser"
3. Mail me  (bsalsa@gmail.com) any code change in the unit
   for the benefit of the other users.
4. Please, consider donation in our web site!
{*******************************************************************************}
//$Id: UI_Less.pas,v 1.3 2006/11/17 12:43:54 bsalsa Exp $

unit UI_Less;

interface

uses
  MsHtml_EWB, UrlMon, ActiveX, Windows, Messages, Classes;

type
  TInvokeEvent = function(DispID: Integer; const IID: TGUID; LocaleID: Integer;
    Flags: Word; var Params: TagDispParams; VarResult, ExcepInfo, ArgErr: Pointer): HRESULT of object;

  TUILess = class(TComponent, IUnknown, IDispatch, IPropertyNotifySink, IOleClientSite)
  private
    FOnInvoke: TInvokeEvent;
    FAbout: string;
    FEnabled: Boolean;
    procedure SetAbout(Value: string);
  protected
    // IDispatch: http://msdn.microsoft.com/en-us/library/ms221608.aspx
    function Invoke(DispID: Integer; const IID: TGUID; LocaleID: Integer;
      Flags: Word; var Params; VarResult, ExcepInfo, ArgErr: Pointer): HRESULT; stdcall;
    // IPropertyNotifySink: http://msdn.microsoft.com/en-us/library/ms692638(VS.85).aspx
    function OnChanged(dispid: TDispID): HRESULT; stdcall;
    function OnRequestEdit(dispid: TDispID): HRESULT; stdcall;
    // IOleClientSite: http://msdn.microsoft.com/en-us/library/ms693706.aspx
    function SaveObject: HRESULT; stdcall;
    function GetMoniker(dwAssign: Longint; dwWhichMoniker: Longint;
      out mk: IMoniker): HRESULT; stdcall;
    function GetContainer(out container: IOleContainer): HRESULT; stdcall;
    function ShowObject: HRESULT; stdcall;
    function OnShowWindow(fShow: BOOL): HRESULT; stdcall;
    function RequestNewObjectLayout: HRESULT; stdcall;
    //
    function LoadUrlFromMoniker: HRESULT;
    function LoadUrlFromFile: HRESULT;
// * We only use LoadUrlFromMoniker, but we could use LoadUrlFromFile instead.
  public
    HTMLElementCollection: IHTMLElementCollection;
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function Go(URL: WideString): IHTMLELEMENTCollection;
    procedure GetImageList(ImagesList: TStrings);
    procedure GetAnchorList(Anchorlist: TStrings); overload;
    procedure GetLinkList(LinkList: TStrings); overload;
  published
    property OnInvoke: TInvokeEvent read FOnInvoke write FOnInvoke;
    property Enabled: Boolean read FEnabled write FEnabled default True;
    property About: string read fAbout write SetAbout;
  end;

implementation

uses
  EwbIEConst;

var
  Doc: IhtmlDocument2;
  _URL: WideString;

/// CORE ---->>>>>>>>>

constructor TUILess.Create;
begin
  FAbout := 'TUILess parser  - ' + WEB_SITE;
  FEnabled := True;
  inherited;
end;

destructor TUILess.Destroy;
begin
  inherited;
end;

function TUILess.Go(URL: WideString): IHtmlElementCollection;
const
  WM_USER_STARTWALKING = WM_USER + 1;
var
  Cookie: Integer;
  CP: IConnectionPoint;
  OleObject: IOleObject;
  OleControl: IOleControl;
  CPC: IConnectionPointContainer;
  Msg: TMsg;
  hr: HRESULT;
begin
  if not FEnabled then
    Result := nil
  else
  begin
    _URL := Url;
    if Succeeded(CoCreateInstance(CLASS_HTMLDocument, nil, CLSCTX_INPROC_SERVER, IID_IHTMLDocument2, Doc)) then
    begin
      OleObject := Doc as IOleObject;
      OleObject.SetClientSite(Self);
      OleControl := Doc as IOleControl;
      OleControl.OnAmbientPropertyChange(DISPID_AMBIENT_DLCONTROL);
      CPC := Doc as IConnectionPointContainer;
      if CPC.FindConnectionPoint(IpropertyNotifySink, CP) = S_OK then
      begin
        if CP.Advise(Self, Cookie) = S_OK then
        begin
          HR := LoadUrlFromMoniker; // alternative: Hr:= LoadUrlFromFile;
          if Succeeded(HR) {or (HR = E_PENDING)} then
            while (GetMessage(msg, 0, 0, 0)) do
              if ((msg.message = WM_USER_STARTWALKING) and (msg.hwnd = 0)) then
              begin
                PostQuitMessage(0);
                HtmlElementCollection := Doc.Get_all;
                Result := HtmlElementCollection;
              end
              else
                DispatchMessage(msg);
        end;
      end;
    end;
  end;
end;

function TUILess.Invoke(DispID: Integer; const IID: TGUID; LocaleID: Integer;
  Flags: Word; var Params; VarResult, ExcepInfo, ArgErr: Pointer): HRESULT;
const
  DLCTL_NO_SCRIPTS = $00000080;
  DLCTL_NO_JAVA = $00000100;
  DLCTL_NO_RUNACTIVEXCTLS = $00000200;
  DLCTL_NO_DLACTIVEXCTLS = $00000400;
  DLCTL_DOWNLOADONLY = $00000800;
var
  I: Integer;
begin
  Result := E_Fail;
  if FEnabled then
  begin
    if DISPID_AMBIENT_DLCONTROL = DispID then
    begin
      I := DLCTL_DOWNLOADONLY + DLCTL_NO_SCRIPTS +
        DLCTL_NO_JAVA + DLCTL_NO_DLACTIVEXCTLS +
        DLCTL_NO_RUNACTIVEXCTLS;
      PVariant(VarResult)^ := I;
      Result := S_OK;
      if Assigned(FOnInvoke) then
        FOnInvoke(DispID, IID, LocaleID, Flags, TagDispParams(Params),
          VarResult, ExcepInfo, ArgErr)
    end
    else
      Result := DISP_E_MEMBERNOTFOUND;
  end;
end;

function TUILess.OnChanged(dispid: TDispID): HRESULT;
const
  WM_USER_STARTWALKING = WM_USER + 1;
  READYSTATE_COMPLETE = $00000004;
var
  dp: TDispParams;
  vResult: OleVariant;
begin
  Result := E_Fail;
  if FEnabled then
  begin
    if (DISPID_READYSTATE = Dispid) then
    begin
      if Succeeded((Doc as IHTMLDocument2).Invoke(DISPID_READYSTATE, GUID_null,
        LOCALE_SYSTEM_DEFAULT, DISPATCH_PROPERTYGET, dp, @vresult, nil, nil)) then
        if Integer(vResult) = READYSTATE_COMPLETE then
          PostThreadMessage(GetCurrentThreadId(), WM_USER_STARTWALKING, 0, 0);
    end;
  end;
end;

function TUILess.LoadUrlFromMoniker: HRESULT;
var
  Moniker: IMoniker;
  BindCtx: IBindCTX;
  PM: IPersistMoniker;
begin
  Result := E_Fail;
  if FEnabled then
  begin
    if Succeeded(CreateURLMoniker(nil, PWideChar(_URL), Moniker)) then
    begin
      CreateBindCtx(0, BindCtx);
      PM := Doc as IPersistMoniker;
      Result := PM.Load(LongBool(0), Moniker, BindCtx, STGM_READ);
    end;
  end;
end;

function TUILess.LoadUrlFromFile: HRESULT;
var
  PF: IPersistFile;
begin
  Result := E_Fail;
  if FEnabled then
  begin
    PF := Doc as IPersistFile;
    Result := PF.Load(PWideChar(_URL), 0);
  end;
end;

///  UTILITIES ---------- >>>>>>>>>>>>>>>>>>>>>

procedure TUILess.GetImageList(ImagesList: TStrings);
var
  ImageElement: IHTMLImgElement;
  Disp: IDispatch;
  i: Integer;
begin
  if HtmlElementCollection <> nil then
  begin
    for i := 0 to HTMLElementCollection.length - 1 do
    begin
      Disp := HtmlElementCollection.item(i, 0);
      if Succeeded(Disp.QueryInterface(IHTMLImgElement, ImageElement))
        then
        ImagesList.Add(ImageElement.src);
    end;
  end;
end;

procedure TUILess.GetAnchorList(Anchorlist: TStrings);
var
  Anchor: IHTMLAnchorElement;
  Disp: IDispatch;
  x: Integer;
begin
  if HTMLElementCollection <> nil then
  begin
    for x := 0 to HTMLElementCollection.length - 1 do
    begin
      Disp := HTMLElementCollection.item(x, 0);
      if Succeeded(Disp.QueryInterface(IHTMLAnchorElement, Anchor))
        and (anchor.href <> '') then
        Anchorlist.Add(Anchor.href);
    end;
  end;
end;

procedure TUILess.GetLinkList(LinkList: TStrings);
var
  Anchor: IHTMLAnchorElement;
  Link: IHTMLElement;
  Disp: IDispatch;
  i: Integer;
begin
  if HTMLElementCollection <> nil then
  begin
    for i := 0 to HTMLElementCollection.length - 1 do
    begin
      Disp := HTMLElementCollection.item(i, 0);
      if Succeeded(Disp.QueryInterface(IHTMLElement, Link)) and
        (Link.innerText <> '') then
      begin
        if Succeeded(Disp.QueryInterface(IHTMLAnchorElement, Anchor))
          and (Anchor.href <> '') then
          LinkList.Add(Link.innerText);
      end;
    end;
  end;
end;

/// Don't Care ------>>>>>>>>>>>

procedure TUILess.SetAbout(Value: string);
begin
  Exit;
end;

function TUILess.OnRequestEdit(dispid: TDispID): HRESULT;
begin
  Result := E_NOTIMPL;
end;

function TUILess.SaveObject: HRESULT;
begin
  Result := E_NOTIMPL;
end;

function TUILess.GetMoniker(dwAssign: Longint; dwWhichMoniker: Longint;
  out mk: IMoniker): HRESULT;
begin
  Result := E_NOTIMPL;
end;

function TUILess.GetContainer(out container: IOleContainer): HRESULT;
begin
  Result := E_NOTIMPL;
end;

function TUILess.ShowObject: HRESULT;
begin
  Result := E_NOTIMPL;
end;

function TUILess.OnShowWindow(fShow: BOOL): HRESULT;
begin
  Result := E_NOTIMPL;
end;

function TUILess.RequestNewObjectLayout: HRESULT;
begin
  Result := E_NOTIMPL;
end;

end.

