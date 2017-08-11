//***********************************************************
//                        TEditDesigner                     *
//                                                          *
//                       For Delphi                         *
//                     Freeware Component                   *
//                            by                            *
//                     Per Lindsø Larsen                    *
//                    per.lindsoe@larsen.dk                 *
//                 Fixed by bsalsa@gmail.com                *
//  Documentation and Updated versions:                     *
//                                                          *
//               http://www.bsalsa.com                      *
//***********************************************************
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

You may use, Change or modify the component under 4 conditions:
1. In your website, add a link to "http://www.bsalsa.com"
2. In your application, add credits to "Embedded Web Browser"
3. Mail me  (bsalsa@gmail.com) any code Change in the unit
   for the benefit of the other users.
4. Please consider donation in our web site!
{*******************************************************************************}
//$Id: EditDesigner.pas,v 1.3.2.1 2006/11/29 22:13:00 sergev Exp $
{$B-}
unit EditDesigner;

//Extras----------------------------------------------------------------------
{To remove the Extras (Events), just add a dot like "{.$DEFINE...USE_Extras" or something.
and re-compile the package.}
{$DEFINE USE_Extras}

interface

{$I EWB.inc}

uses
{$IFDEF USE_Extras}EwbAcc, Graphics, {$ENDIF}
  EmbeddedWB, ActiveX, MSHTML_EWB, Classes, Windows;

const
  S_OK = 0;
{$EXTERNALSYM S_OK}
  S_FALSE = $00000001;
{$EXTERNALSYM S_FALSE}
  E_FAIL = HRESULT($80004005);
{$EXTERNALSYM E_FAIL}
  IID_IOleContainer: TGUID = (D1: $0000011B; D2: $0000; D3: $0000;
    D4: ($C0, $00, $00, $00, $00, $00, $00, $46));
  SID_SHTMLEditServices: TGUID = (D1: $3050F7F9; D2: $98B5; D3: $11CF;
    D4: ($BB, $82, $00, $AA, $00, $BD, $CE, $0B));
  IID_IUnknown: TGUID = (D1: $00000000; D2: $0000; D3: $0000;
    D4: ($C0, $00, $00, $00, $00, $00, $00, $46));

type
  TSnapRect = function(const pIElement: IHTMLElement; var prcNew: TRECT; eHandle: _ELEMENT_CORNER): HRESULT of object;
  TPreDrag = function: HRESULT of object;
  TPreHandleEvent = function(inEvtDispId: Integer; const pIEventObj: IHTMLEventObj): HRESULT of object;
  TPostHandleEvent = function(inEvtDispId: Integer; const pIEventObj: IHTMLEventObj): HRESULT of object;
  TTranslateAccelerator = function(inEvtDispId: Integer; const pIEventObj: IHTMLEventObj): HRESULT of object;
  TPostEditorEventNotify = function(inEvtDispId: Integer; const pIEventObj: IHTMLEventObj): HRESULT of object;
  TErrorEvent = procedure(const ErrorCode: integer; ErrMessage: string) of object;
{$IFDEF USE_Extras}
  TMSHTMLEvent = procedure(Event: IHTMLEventObj) of object;
  TMousePositionEvent = procedure(const X, Y: integer) of object;
  TEvtDispIdEvent = procedure(const inEvtDispId: integer) of object;
  TMouseButtonEvent = procedure(const Button: integer) of object;
  TKeyStateEvent = procedure(const CapsLock, NumLock, InsertKey, altKey, ctrlKey, shiftKey: Boolean) of object;
  TKeyPressEvent = procedure(const Key: integer) of object;
  TtypeEvent = procedure(const type_: string) of object;
  TtagNameEvent = procedure(const tagName: string) of object;
  TtoStringEvent = procedure(const toString: string) of object;
  TinnerHtmlEvent = procedure(const innerHtml: string) of object;
  TinnerTextEvent = procedure(const innerText: string) of object;
{$ENDIF}
type
  IOleContainer = interface(IParseDisplayName)
  // The IParseDisplayName interface parses a displayable name string to convert it
  // into a moniker for custom moniker implementations.
    ['{0000011B-0000-0000-C000-000000000046}']
    function EnumObjects(grfFlags: Longint; out Enum: IEnumUnknown): HRESULT; stdcall;
    function LockContainer(fLock: BOOL): HRESULT; stdcall;
  end;

type
  TImpIOleContainer = class(TObject, IOleContainer)
  protected
    m_cRef: DWORD;
  public
    function _AddRef: Integer; overload; stdcall;
    function _Release: Integer; overload; stdcall;
    constructor Create; virtual;
    destructor Destroy; override;
    function QueryInterface(const IID: TGUID; out Obj): HRESULT; stdcall;
    function ParseDisplayName(const bc: IBindCtx; pszDisplayName: POleStr;
      out chEaten: Longint; out mkOut: IMoniker): HRESULT; stdcall;
    function EnumObjects(grfFlags: Longint; out Enum: IEnumUnknown): HRESULT; stdcall;
    function LockContainer(fLock: BOOL): HRESULT; stdcall;
  end;

  TEditDesigner = class(TComponent,
      IUnknown, // http://msdn.microsoft.com/en-us/library/ms680509.aspx
      IHtmlEditDesigner, // Provides methods that enable clients using the editor to intercept Windows Internet Explorer events
                        // so that they can change the default behavior of the editor.
                        // http://msdn.microsoft.com/en-us/library/aa704056(VS.85).aspx
      IHTMLEditHost, // Provides a method to customize the way that elements are resized and moved.
                     // http://msdn.microsoft.com/en-us/library/aa704054(VS.85).aspx
      IHTMLEditHost2 // Extends IHTMLEditHost with a method that enables you to intercept drag-and-drop operations
                     // at a stage earlier than the ondragstart event.
                     // http://msdn.microsoft.com/en-us/library/aa704052(VS.85).aspx
      )

  private
    bDesignMode: Boolean;
    bConnected: Boolean;
    FAbout: string;
    FEmbeddedWB: TEmbeddedWB;
    FEnable: Boolean;
    FOnError: TErrorEvent;
{$IFDEF USE_Extras}
    sl: TStringList;
    FOnInnerHtml: TinnerHtmlEvent;
    FOnInnerText: TinnerTextEvent;
    FOnEvtDispId: TEvtDispIdEvent;
    FOnKeyState: TKeyStateEvent;
    FOnKeyPress: TKeyPressEvent;
    FOnMousePosition: TMousePositionEvent;
    FOnMouseButton: TMouseButtonEvent;
    FonType_: TtypeEvent;
    FOnToString: TtoStringEvent;
    FOntagName: TtagNameEvent;
{$ENDIF}
    FPostEditorEventNotify: TPostEditorEventNotify;
    FPostHandleEvent: TPostHandleEvent;
    FPreHandleEvent: TPreHandleEvent;
    FPreDrag: TPreDrag;
    FSnapRect: TSnapRect;
    FTranslateAccelerator: TTranslateAccelerator;
    procedure SetAbout(Value: string);
  protected
      {IHTMLEditHost}
    function SnapRect(const pIElement: IHTMLElement; var prcNew: TRECT; eHandle: _ELEMENT_CORNER): HRESULT; stdcall;
      {IHTMLEditHost2}
    function PreDrag: HRESULT; stdcall;
      {IHtmlEditDesigner}
    function PreHandleEvent(inEvtDispId: Integer; const pIEventObj: IHTMLEventObj): HRESULT; stdcall;
    function PostHandleEvent(inEvtDispId: Integer; const pIEventObj: IHTMLEventObj): HRESULT; stdcall;
    function TranslateAccelerator(inEvtDispId: Integer; const pIEventObj: IHTMLEventObj): HRESULT; stdcall;
    function PostEditorEventNotify(inEvtDispId: Integer; const pIEventObj: IHTMLEventObj): HRESULT; stdcall;
    procedure Loaded; override;

  public
    procedure SetDesignModeOff;
    procedure SetDesignModeOn;
    function ConnectDesigner: integer;
    function RemoveDesigner: integer;
{$IFDEF USE_Extras}
    function SaveToFile(FileName: WideString): HRESULT;
    function SaveCompleted(FileName: WideString): HRESULT;
    function LoadFromFile(FileName: WideString): HRESULT;
    function IsDirty: HRESULT;
    function GetHTMLDoc2FromWB: IHTMLDocument2;
    function RGBToBGR(RGB: TColor): Integer;
    function ColorStr(RGB: TColor): string;
    procedure InsertHyperlink;
    procedure InsertImage;
    procedure InsertRadioButton;
    procedure SetFontBold;
    procedure SetFontUnderline;
    procedure SetFontItalic;
    procedure ExecCommand(Command: Widestring; ShowUI: Boolean; Value: Integer);
    procedure InsertHTML(HTML: string);
    function GetPageProperties: TStrings;
{$ENDIF}
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure BeforeDestruction; override;
    function GetContainer(out container: IOleContainer): HRESULT;
  published
    property About: string read fAbout write SetAbout;
    property Enable: Boolean read FEnable write FEnable default True;
    property EmbeddedWB: TEmbeddedWB read FEmbeddedWB write FEmbeddedWB;
    property OnPreDrag: TPreDrag read FPreDrag write FPreDrag;
    property OnPreHandle: TPreHandleEvent read FPreHandleEvent write FPreHandleEvent;
    property OnPostHandle: TPostHandleEvent read FPostHandleEvent write FPostHandleEvent;
    property OnPostEditorNotify: TPostEditorEventNotify read FPostEditorEventNotify write FPostEditorEventNotify;
    property OnError: TErrorEvent read FOnError write FOnError;
{$IFDEF USE_Extras}
    property OnInnerText: TInnerTextEvent read FOnInnerText write FOnInnerText;
    property OnInnerHtml: TInnerHtmlEvent read FOnInnerHtml write FOnInnerHtml;
    property OnKeyPress: TKeyPressEvent read FOnKeyPress write FOnKeyPress;
    property OnKeyState: TKeyStateEvent read FOnKeyState write FOnKeyState;
    property OnTranslateAccelerator: TTranslateAccelerator read FTranslateAccelerator write FTranslateAccelerator;
    property OnMousePosition: TMousePositionEvent read FOnMousePosition write FOnMousePosition;
    property OnMouseButton: TMouseButtonEvent read FOnMouseButton write FOnMouseButton;
    property OnEvtDispId: TEvtDispIdEvent read FOnEvtDispId write FOnEvtDispId;
    property OnSnapRect: TSnapRect read FSnapRect write FSnapRect;
    property OnType_: TtypeEvent read FOnType_ write FOnType_;
    property OnToString: TtoStringEvent read FOnToString write FOnToString;
    property OnTagName: TtagNameEvent read FOnTagName write FOnTagName;
{$ENDIF}
  end;

implementation

uses
  SysUtils;

var
  EditServices: IHTMLEditServices;

//--{TImpIOleContainer}---------------------------------------------------------

constructor TImpIOleContainer.Create;
begin
  inherited;
  m_cRef := 0;
end;

destructor TImpIOleContainer.Destroy;
begin
  inherited;
end;

function TImpIOleContainer.EnumObjects(grfFlags: Longint; out Enum: IEnumUnknown): HRESULT;
begin
  Result := E_NOINTERFACE;
end;

function TImpIOleContainer.LockContainer(fLock: BOOL): HRESULT;
begin
  Result := E_NOINTERFACE;
end;

function TImpIOleContainer.ParseDisplayName(const bc: IBindCtx; pszDisplayName: POleStr;
  out chEaten: Longint; out mkOut: IMoniker): HRESULT;
begin
  Result := E_NOINTERFACE;
end;

function TImpIOleContainer.QueryInterface(const IID: TGUID; out Obj): HRESULT;
begin
  Result := S_FALSE;
  if GetInterface(IID, Obj) then
  begin
    Result := S_OK;
    Exit;
  end;
{$IFDEF DELPHI7_UP}
  if (GUIDToString(IID_IUnknown) = GUIDToString(IID)) or
    (GUIDToString(IID_IOleContainer) = GUIDToString(IID)) then
  begin
    IOleContainer(Obj) := Self;
    Result := S_OK;
    self._AddRef;
  end;
{$ENDIF}
end;

function TImpIOleContainer._AddRef: Integer;
begin
  inc(m_cRef);
  Result := m_cRef;
end;

function TImpIOleContainer._Release: Integer;
begin
  Dec(m_cRef);
  Result := m_cRef;
  if m_cRef = 0 then
    Free;
end;

//--{ TEditDesigner }-----------------------------------------------------------

function TEditDesigner.GetContainer(out container: IOleContainer): HRESULT;
var
  aCntr: TImpIOleContainer;
begin
  aCntr := TImpIOleContainer.Create;
  Result := aCntr.QueryInterface(IID_IOleContainer, container);
end;

constructor TEditDesigner.Create;
begin
  FAbout := 'TEditDesigner - from http://www.bsalsa.com/';
  FEnable := True;
{$IFDEF USE_Extras}
  sl := TStringList.Create;
{$ENDIF}
  inherited;
end;

destructor TEditDesigner.Destroy;
begin
{$IFDEF USE_Extras}
  sl.Free;
{$ENDIF}
  inherited Destroy;
end;

procedure TEditDesigner.BeforeDestruction();
begin
  if bConnected then
    EditServices.RemoveDesigner(Self);
  inherited BeforeDestruction;
end;

procedure TEditDesigner.Loaded;
begin
{$IFDEF USE_Extras}
  sl.Clear;
{$ENDIF}
  bConnected := False;
  bDesignMode := False;
  if not (csDesigning in ComponentState) then
    if Assigned(FEmbeddedWB) and FEnable and not FEmbeddedWB.DocumentLoaded then
      FEmbeddedWB.AssignEmptyDocument;
  inherited;
end;

function TEditDesigner.SnapRect(const pIElement: IHTMLElement; var prcNew: TRECT;
  eHandle: _ELEMENT_CORNER): HRESULT;
begin
  Result := S_OK;
  if Assigned(FSnapRect) and FEnable then
    Result := FSnapRect(pIElement, prcNew, eHandle);
end;

function TEditDesigner.PreDrag: HRESULT;
begin
  Result := S_OK;
  if Assigned(FPreDrag) and FEnable then
    Result := FPreDrag;
end;

function TEditDesigner.PostEditorEventNotify(inEvtDispId: Integer;
  const pIEventObj: IHTMLEventObj): HRESULT;
begin
  Result := S_FALSE;
  if FEnable and not (csDesigning in ComponentState) then
  begin
{$IFDEF USE_Extras}
    if (inEvtDispId = -606) and Assigned(FOnMousePosition) then
      FOnMousePosition(pIEventObj.clientX, pIEventObj.clientY);
{$ENDIF}
    if Assigned(FPostEditorEventNotify) then
      Result := FPostEditorEventNotify(inEvtDispID, pIEventObj);
  end;
end;

function TEditDesigner.PostHandleEvent(inEvtDispId: Integer;
  const pIEventObj: IHTMLEventObj): HRESULT;
begin
  Result := S_FALSE;
  if FEnable and not (csDesigning in ComponentState) then
  begin
    if Assigned(FPostHandleEvent) and FEnable then
      Result := FPostHandleEvent(inEvtDispID, pIEventObj);
  end;
end;

function CheckCapsLock: boolean;
begin
  if Odd(GetKeyState(VK_CAPITAL)) then
    Result := True
  else
    Result := False;
end;

function CheckInsertKey: Boolean;
begin
  if Odd(GetKeyState(VK_INSERT)) then
    Result := True
  else
    Result := False;
end;

function CheckNumLock: Boolean;
begin
  if Odd(GetKeyState(VK_NUMLOCK)) then
    Result := True
  else
    Result := False;
end;

function TEditDesigner.PreHandleEvent(inEvtDispId: Integer;
  const pIEventObj: IHTMLEventObj): HRESULT;
{$IFDEF USE_Extras}
var
  srcElement: IHTMLElement;
{$ENDIF}
begin
  Result := S_FALSE;
  if FEnable and not (csDesigning in ComponentState) then
  begin
    if Assigned(FPreHandleEvent) then
      FPreHandleEvent(inEvtDispID, pIEventObj);
{$IFDEF USE_Extras}
    srcElement := pIEventObj.srcElement;
    if Assigned(srcElement) then
    begin
      if Assigned(FOnKeyState) then
        FOnKeyState(CheckCapsLock, CheckNumLock, CheckInsertKey, pIEventObj.altKey, pIEventObj.ctrlKey, pIEventObj.shiftKey);
      if Assigned(FOnEvtDispId) then
        FOnEvtDispId(inEvtDispId);
      if Assigned(FOnMouseButton) then
        FOnMouseButton(pIEventObj.button);
      if Assigned(FOnKeyPress) then
        FOnKeyPress(pIEventObj.keyCode);
      if Assigned(FOnTagName) then
        FOnTagName(pIEventObj.srcElement.tagName);
      if Assigned(FOnType_) then
        FonType_(pIEventObj.type_);
      if Assigned(FOnToString) then
        FOnToString(pIEventObj.srcElement.toString);
      if Assigned(FOnInnerText) then
        FOnInnerText(pIEventObj.srcElement.innerText);
      if Assigned(FOnInnerHTML) then
        FOnInnerHTML(pIEventObj.srcElement.innerHTML);
    end;
{$ENDIF}
  end;
end;

function TEditDesigner.TranslateAccelerator(inEvtDispId: Integer;
  const pIEventObj: IHTMLEventObj): HRESULT;
begin
  Result := S_FALSE;
  if FEnable and not (csDesigning in ComponentState) then
  begin
    if Assigned(FTranslateAccelerator) then
      Result := FTranslateAccelerator(inEvtDispID, pIEventObj);
  end;
end;

function TEditDesigner.ConnectDesigner: integer;
begin
  Result := S_FALSE;
  if FEnable and not (csDesigning in ComponentState) then
  begin
    if not Assigned(FEmbeddedWB) then
    begin
      if Assigned(FOnError) then
        FOnError(E_FAIL, 'Please assign a TEmbeddedWB!');
      Exit;
    end;
    FEmbeddedWB.Wait;
    SetDesignModeOn;
    Result := (FEmbeddedWB.Document as IServiceProvider).Queryservice(SID_SHTMLEDITSERVICES, IID_IHTMLEditServices, EditServices);
    if Result = S_OK then
      Result := EditServices.AddDesigner(Self);
    if ((Result <> S_OK) and Assigned(FOnError)) then
      FOnError(E_FAIL, 'Failed loading the designer!')
    else
      bConnected := True;
  end;
end;

function TEditDesigner.RemoveDesigner: integer;
begin
  Result := S_FALSE;
  if FEnable and not (csDesigning in ComponentState) then
  begin
    if not Assigned(FEmbeddedWB) and Assigned(FOnError) and (FEmbeddedWB.Document = nil) then
    begin
      FOnError(E_FAIL, 'Failed (Document = nil).');
      Exit;
    end
    else
      if FEmbeddedWB.Busy then
      begin
        FEmbeddedWB.Stop;
        FEmbeddedWB.Wait;
      end;
    Result := EditServices.RemoveDesigner(Self);
    if (Result <> S_OK) and (Assigned(FOnError)) then
      FOnError(Result, 'Failed to RemoveDesigner.')
    else
      bConnected := false;
    SetDesignModeOff;
  end;
end;

procedure TEditDesigner.SetDesignModeOn;
begin
  if not Assigned(FEmbeddedWB) then
  begin
    if Assigned(FOnError) then
      FOnError(E_FAIL, 'Please assign a TEmbeddedWB!');
    Exit;
  end;
  if not (csDesigning in ComponentState) then
  begin
    FEmbeddedWB.Wait;
    try
      (FEmbeddedWB.document as IHTMLDocument2).designMode := 'On';
      bDesignMode := True;
    except
      if Assigned(FOnError) then
        FOnError(E_FAIL, 'Failed to set DesignMode state On.');
    end;
  end;
end;

procedure TEditDesigner.SetDesignModeOff;
begin
  if not Assigned(FEmbeddedWB) then
  begin
    if Assigned(FOnError) then
      FOnError(E_FAIL, 'Please assign a TEmbeddedWB!');
    Exit;
  end;
  if not (csDesigning in ComponentState) then
  begin
    try
      (FEmbeddedWB.document as IHTMLDocument2).designMode := 'Off';
      bDesignMode := False;
    except
      if Assigned(FOnError) then
        FOnError(E_FAIL, 'Failed to set DedignMode state Off.');
    end;
  end;
end;

procedure TEditDesigner.SetAbout(Value: string);
begin
  Exit;
end;

{$IFDEF USE_Extras}

function TEditDesigner.GetHTMLDoc2FromWB: IHTMLDocument2;
begin
  if Assigned(FEmbeddedWB) then
    Result := FEmbeddedWB.Document as IHTMLDocument2
  else
    Result := nil;
end;

function TEditDesigner.LoadFromFile(FileName: WideString): HRESULT;
begin
  if Assigned(FEmbeddedWB) then
    Result := (GetHTMLDoc2FromWB as IPersistFile).Load(PWideChar(FileName), 0)
  else
    Result := E_FAIL;
end;

function TEditDesigner.SaveToFile(FileName: WideString): HRESULT;
begin
  if Assigned(FEmbeddedWB) then
    Result := (GetHTMLDoc2FromWB as IPersistFile).Save(PWideChar(FileName), True)
  else
    Result := E_FAIL;
end;

function TEditDesigner.SaveCompleted(FileName: WideString): HRESULT;
begin
  if Assigned(FEmbeddedWB) then
    Result := (GetHTMLDoc2FromWB as IPersistFile).SaveCompleted(PWideChar(FileName))
  else
    Result := E_FAIL;
end;

function TEditDesigner.IsDirty: HRESULT;
begin
  if Assigned(FEmbeddedWB) then
    Result := (GetHTMLDoc2FromWB as IPersistFile).IsDirty
  else
    Result := E_FAIL;
end;

function TEditDesigner.RGBToBGR(RGB: TColor): Integer;
begin
  Result := (RGB and $000000FF) shl 16 + (RGB and $0000FF00) + (RGB and $00FF0000) shr 16;
end;

function TEditDesigner.ColorStr(RGB: TColor): string;
begin
  Result := '#' + IntToHex(RGBToBGR(RGB), 6);
end;

procedure TEditDesigner.ExecCommand(Command: Widestring; ShowUI: Boolean; Value: Integer);
begin
  if Assigned(FEmbeddedWB) then
    GetHTMLDoc2FromWB.execCommand(Command, showUI, Value);
end;

procedure TEditDesigner.InsertHyperlink;
begin
  if Assigned(FEmbeddedWB) then
    ExecCommand('CreateLink', True, 0);
end;

procedure TEditDesigner.InsertImage;
begin
  if Assigned(FEmbeddedWB) then
    ExecCommand('InsertImage', True, 0);
end;

procedure TEditDesigner.InsertRadioButton;
begin
  if Assigned(FEmbeddedWB) then
    ExecCommand('InsertInputRadio', True, 0);
end;

procedure TEditDesigner.SetFontBold;
begin
  if Assigned(FEmbeddedWB) then
    ExecCommand('Bold', False, 0);
end;

procedure TEditDesigner.SetFontUnderline;
begin
  if Assigned(FEmbeddedWB) then
    ExecCommand('Underline', False, 0);
end;

procedure TEditDesigner.SetFontItalic;
begin
  if Assigned(FEmbeddedWB) then
    ExecCommand('Italic', False, 0);
end;

procedure TEditDesigner.InsertHTML(HTML: string);
var
  Sel: IHTMLSelectionObject;
  Range: IHTMLTxtRange;
  Doc: IHTMLDocument2;
begin
  if FEnable and Assigned(FEmbeddedWB) and (not (csDesigning in ComponentState)) then
  begin
    Doc := FEmbeddedWB.Doc2;
    if Assigned(Doc) then
    begin
      Sel := Doc.selection;
      if Assigned(Sel) then
      begin
        if (Sel.type_ = 'None') or (Sel.type_ = 'Text') then
        begin
          Range := Sel.createRange as IHTMLTxtRange;
          Range.pasteHTML(HTML);
        end;
      end;
    end;
  end;
end;

function TEditDesigner.GetPageProperties: TStrings;
var
  Doc: IHTMLDocument2;
begin
  sl.Clear;
  if Assigned(FEmbeddedWB) and FEnable and not (csDesigning in ComponentState) then
  begin
    FEmbeddedWB.Wait;
    Doc := FEmbeddedWB.Doc2;
    if (Assigned(Doc)) and (Doc.readyState = 'complete') and (Doc <> nil) then
      with sl do
      try
        Add('URL: ' + Doc.url);
        Add('Title: ' + Doc.title);
        Add('Protocol: ' + Doc.protocol);
        Add('Location: ' + Doc.location.href);
        Add('Path Name: ' + Doc.location.pathname);
        Add('Port: ' + Doc.location.port);
        Add('Protocol: ' + Doc.location.protocol);
        Add('Host: ' + Doc.location.host);
        Add('Hash: ' + Doc.location.hash);
        Add('Search: ' + Doc.location.search);
        Add('Last Modified: ' + Doc.lastModified);
        Add('Security: ' + Doc.security);
        Add('Name Property: ' + Doc.nameProp);
        Add('Language: ' + Doc.Body.language);
        Add('Lang: ' + Doc.Body.lang);
        Add('Design Mode: ' + Doc.designMode);
        Add('Charset: ' + Doc.charset);
        Add('Default Charset: ' + Doc.defaultCharset);
        Add('Cookie: ' + Doc.cookie);
        Add('Referrer: ' + Doc.Referrer);
        Add('Doc State: ' + Doc.readyState);
      except
        on E: Exception do
        begin
          if Assigned(FOnError) then
            FOnError(E_Fail, E.Message);
          Result := sl;
          Exit;
        end;
      end;
  end
  else
    if Assigned(FOnError) then
      FOnError(E_FAIL, 'Please assign a TEmbeddedWB and load a document.');
  Result := sl;
end;
{$ENDIF}

initialization
  OleInitialize(nil);

finalization
  OleUninitialize;

end.
