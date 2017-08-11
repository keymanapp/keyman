//*************************************************************
//                       EwbEventsComp                        *
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
//$Id: EwbEventsComp.pas,v 1.1.2.1 2006/11/29 22:13:01 sergev Exp $

unit EwbEventsComp;

interface

{$I EWB.inc}

uses
{$IFDEF DELPHI6_UP}Variants, {$ENDIF}
  Windows, Classes, ActiveX, Mshtml_Ewb, EwbAcc, EwbClasses, EwbEvents;

type
  THtmlListener = class;

  TEventEnum = (
    eiUnknown,
    eiOnAbort,
    eiOnChange,
    eiOnError,
    eiOnLoad,
    eiOnSelect,
    eiOnSubmit,
    eiOnUnload,
    eiOnBounce,
    eiOnFinish,
    eiOnStart,
    eiOnScroll,
    eiOnReset,
    eiOnresize,
    eiOnBeforeUnload,
    eiOncontextmenu,
    eiOnBeforePrint,
    eiOnAfterPrint,
    eiOnStop,
    eiOnBeforeEditFocus,
    eiOnlayoutcomplete,
    eiOnpage,
    eiOnmousewheel,
    eiOnbeforedeactivate,
    eiOnmove,
    eiOncontrolselect,
    eiOnSelectionChange,
    eiOnmoveStart,
    eiOnmoveEnd,
    eiOnresizeStart,
    eiOnresizeEnd,
    eiOnmouseEnter,
    eiOnmouseLeave,
    eiOnActivate,
    eiOnDeactivate,
    eiOnBeforeActivate,
    eiOnfocusIn,
    eiOnfocusOut,
    eiOnClick,
    eiOnDblClick,
    eiOnKeyDown,
    eiOnKeyPress,
    eiOnKeyUp,
    eiOnMouseDown,
    eiOnMouseMove,
    eiOnMouseUp,
    eiOnReadyStateChange,
    eiOnCellChange,
    eiOnRowsInserted,
    eiOnRowsDelete,
    eiOnBeforePaste,
    eiOnBeforeCopy,
    eiOnBeforeCut,
    eiOnPaste,
    eiOnCopy,
    eiOnCut,
    eiOnDrop,
    eiOnDragLeave,
    eiOnDragOver,
    eiOnDragEnter,
    eiOnDragEnd,
    eiOnDrag,
    eiOnPropertyChange,
    eiOnLoseCapture,
    eiOnFilterChange,
    eiOnDatasetComplete,
    eiOnDataAvailable,
    eiOnDatasetChanged,
    eiOnErrorUpdate,
    eiOnSelectStart,
    eiOnDragStart,
    eiOnHelp,
    eiOnMouseOut,
    eiOnMouseOver,
    eiOnRowEnter,
    eiOnRowExit,
    eiOnAfterUpdate,
    eiOnBeforeUpdate,
    eiOnFocus,
    eiOnBlur
    );

  TMSHTMLDelegate = procedure(Sender: TObject; Event: IHTMLEventObj) of object;

  TEventHandlerItem = class(TCollectionItem)
  private
    FEventID: TEventEnum;
    FEvID: TEventID;
    FOnHandle: TMSHTMLDelegate;
    procedure setEventID(const Value: TEventEnum);
  protected
    procedure AssignTo(Dest: TPersistent); override;
    function GetDisplayName: string; override;
  public
    function GetNamePath: string; override;
  published
    property EventID: TEventEnum read FEventID write setEventID;
    property OnHandle: TMSHTMLDelegate read FOnHandle write FOnHandle;
  end;

  THandlerCollection = class(TCollection)
  private
    FOwner: THtmlListener;
    function GetItem(Index: Integer): TEventHandlerItem;
  protected
    function GetOwner: TPersistent; override;
    procedure Update(Item: TCollectionItem); override;
  public
    constructor Create(AOwner: THtmlListener);
    function Add: TEventHandlerItem;
    property Items[Index: Integer]: TEventHandlerItem read GetItem; default;
  end;

  THtmlListenerLink = class;

  THtmlListener = class(TComponent, IDispatch)
  private
    FHandlers: THandlerCollection;
    FDispList: TList;
    FSinkKind: TSinkKind;
    FSinkIID: PGUID;
    Flink: THtmlListenerLink;
    procedure setHandlers(const Value: THandlerCollection);
    procedure setSinkKind(const Value: TSinkKind);
  protected
    { IInterface }
    function QueryInterface(const IID: TGUID; out Obj): HRESULT; override;
      stdcall;
    { IDispatch }
    function GetIDsOfNames(const IID: TGUID; Names: Pointer;
      NameCount, LocaleID: Integer; DispIDs: Pointer): HRESULT; stdcall;
    function GetTypeInfo(Index, LocaleID: Integer; out TypeInfo): HRESULT;
      stdcall;
    function GetTypeInfoCount(out Count: Integer): HRESULT; stdcall;
    function Invoke(DispID: Integer; const IID: TGUID; LocaleID: Integer;
      Flags: Word; var Params; VarResult, ExcepInfo, ArgErr: Pointer): HRESULT;
      stdcall;
  protected
    procedure FillDispList;
    procedure Update(Item: TEventHandlerItem);
    procedure AddDisp(Item: TEventHandlerItem);
    function Find(DispID: TEventID; var Index: Integer): Boolean;
    procedure DispatchEvent(Sender: TObject; DispID: TEventID; Event:
      IHTMLEventObj);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Connect(Source: IUnknown); overload;
    function Connect2(Source: IUnknown; aAgent: TObject = nil): IHubLink;
  published
    property Handlers: THandlerCollection read fHandlers write setHandlers;
    property SinkKind: TSinkKind read FSinkKind write setSinkKind default
      skElement;
  end;

  THtmlListenerLink = class(TInterfacedDispatchObject, IHubLink)
  private
    FHub: THtmlListener;
    FCP: IConnectionPoint;
    FAgent: TObject;
    FSinkCookies: Integer;
  protected
    function Invoke(DispID: Integer; const IID: TGUID; LocaleID: Integer;
      Flags: Word; var Params; VarResult, ExcepInfo, ArgErr: Pointer): HRESULT;
      override; stdcall;
    procedure Connect(Source: IUnknown);
  public
    constructor Create(aHub: THtmlListener; aAgent: TObject);
    procedure Disconnect;
  end;

implementation

uses
  SysUtils, EwbCoreTools;

const
  _eventids: array[TEventEnum] of TEventID = (
    TEventID(0),
    heOnAbort,
    heOnChange,
    heOnError,
    heOnLoad,
    heOnSelect,
    heOnSubmit,
    heOnUnload,
    heOnBounce,
    heOnFinish,
    heOnStart,
    heOnScroll,
    heOnReset,
    heOnresize,
    heOnBeforeUnload,
    heOncontextmenu,
    heOnBeforePrint,
    heOnAfterPrint,
    heOnStop,
    heOnBeforeEditFocus,
    heOnlayoutcomplete,
    heOnpage,
    heOnmousewheel,
    heOnbeforedeactivate,
    heOnmove,
    heOncontrolselect,
    heOnSelectionChange,
    heOnmoveStart,
    heOnmoveEnd,
    heOnresizeStart,
    heOnresizeEnd,
    heOnmouseEnter,
    heOnmouseLeave,
    heOnActivate,
    heOnDeactivate,
    heOnBeforeActivate,
    heOnfocusIn,
    heOnfocusOut,

    heOnClick,
    heOnDblClick,
    heOnKeyDown,
    heOnKeyPress,
    heOnKeyUp,
    heOnMouseDown,
    heOnMouseMove,
    heOnMouseUp,
    heOnReadyStateChange,

    heOnCellChange,
    heOnRowsInserted,
    heOnRowsDelete,
    heOnBeforePaste,
    heOnBeforeCopy,
    heOnBeforeCut,
    heOnPaste,
    heOnCopy,
    heOnCut,
    heOnDrop,
    heOnDragLeave,
    heOnDragOver,
    heOnDragEnter,
    heOnDragEnd,
    heOnDrag,
    heOnPropertyChange,
    heOnLoseCapture,
    heOnFilterChange,
    heOnDatasetComplete,
    heOnDataAvailable,
    heOnDatasetChanged,
    heOnErrorUpdate,
    heOnSelectStart,
    heOnDragStart,
    heOnHelp,
    heOnMouseOut,
    heOnMouseOver,
    heOnRowEnter,
    heOnRowExit,
    heOnAfterUpdate,
    heOnBeforeUpdate,
    heOnFocus,
    heOnBlur
    );

  _eventNames: array[TEventEnum] of string = (
    '',
    'OnAbort',
    'OnChange',
    'OnError',
    'OnLoad',
    'OnSelect',
    'OnSubmit',
    'OnUnload',
    'OnBounce',
    'OnFinish',
    'OnStart',
    'OnScroll',
    'OnReset',
    'Onresize',
    'OnBeforeUnload',
    'Oncontextmenu',
    'OnBeforePrint',
    'OnAfterPrint',
    'OnStop',
    'OnBeforeEditFocus',
    'Onlayoutcomplete',
    'Onpage',
    'Onmousewheel',
    'Onbeforedeactivate',
    'Onmove',
    'Oncontrolselect',
    'OnSelectionChange',
    'OnmoveStart',
    'OnmoveEnd',
    'OnresizeStart',
    'OnresizeEnd',
    'OnmouseEnter',
    'OnmouseLeave',
    'OnActivate',
    'OnDeactivate',
    'OnBeforeActivate',
    'OnfocusIn',
    'OnfocusOut',

    'OnClick',
    'OnDblClick',
    'OnKeyDown',
    'OnKeyPress',
    'OnKeyUp',
    'OnMouseDown',
    'OnMouseMove',
    'OnMouseUp',
    'OnReadyStateChange',

    'OnCellChange',
    'OnRowsInserted',
    'OnRowsDelete',
    'OnBeforePaste',
    'OnBeforeCopy',
    'OnBeforeCut',
    'OnPaste',
    'OnCopy',
    'OnCut',
    'OnDrop',
    'OnDragLeave',
    'OnDragOver',
    'OnDragEnter',
    'OnDragEnd',
    'OnDrag',
    'OnPropertyChange',
    'OnLoseCapture',
    'OnFilterChange',
    'OnDatasetComplete',
    'OnDataAvailable',
    'OnDatasetChanged',
    'OnErrorUpdate',
    'OnSelectStart',
    'OnDragStart',
    'OnHelp',
    'OnMouseOut',
    'OnMouseOver',
    'OnRowEnter',
    'OnRowExit',
    'OnAfterUpdate',
    'OnBeforeUpdate',
    'OnFocus',
    'OnBlur'
    );

  { TEventHandlerItem }

procedure TEventHandlerItem.AssignTo(Dest: TPersistent);
begin
  if Dest is TEventHandlerItem then
    with TEventHandlerItem(Dest) do
    begin
      EventID := Self.EventID;
      OnHandle := Self.OnHandle;
    end
  else
    inherited AssignTo(Dest);
end;

function TEventHandlerItem.GetDisplayName: string;
begin
  if FEventID = eiUnknown then
    Result := inherited GetDisplayName
  else
    Result := _eventNames[FEventID];
end;

function TEventHandlerItem.GetNamePath: string;
begin
  if Collection <> nil then
    Result := Collection.GetNamePath + GetDisplayName
  else
    Result := ClassName;
end;

procedure TEventHandlerItem.setEventID(const Value: TEventEnum);
begin
  if FEventID <> Value then
  begin
    FEventID := Value;
    FEvID := _eventids[FEventID];
    try
      Changed(False);
    except
      FEventID := eiUnknown;
      FEvID := 0;
      raise;
    end;
  end;
end;

{ THandlerCollection }

function THandlerCollection.Add: TEventHandlerItem;
begin
  Result := TEventHandlerItem(inherited Add);
end;

constructor THandlerCollection.Create(AOwner: THtmlListener);
begin
  inherited Create(TEventHandlerItem);
  FOwner := AOwner;
end;

function THandlerCollection.GetItem(Index: Integer): TEventHandlerItem;
begin
  Result := TEventHandlerItem(inherited GetItem(Index));
end;

function THandlerCollection.GetOwner: TPersistent;
begin
  Result := FOwner;
end;

procedure THandlerCollection.Update(Item: TCollectionItem);
begin
  FOwner.Update(TEventHandlerItem(Item));
end;

{ THtmlListener }

constructor THtmlListener.Create(AOwner: TComponent);
begin
  inherited;
  FHandlers := THandlerCollection.Create(Self);
  FSinkKind := skElement;
  FSinkIID := @DIID_HTMLElementEvents2;
end;

destructor THtmlListener.Destroy;
begin
  FHandlers.Free;
  FDispList.Free;
  inherited;
end;

procedure THtmlListener.Connect(Source: IUnknown);
var
  pcpc: IConnectionPointContainer;
  cp: IConnectionPoint;
  c: Integer;
{$IFDEF DELPHI5}
  SelfIntf: IDispatch;
{$ENDIF DELPHI5}
begin
  if Supports(Source, IConnectionPointContainer, pcpc) and
    (pcpc.FindConnectionPoint(FSinkIID^, cp) = S_OK) then
  begin
{$IFDEF DELPHI5}
    GetInterFace(IDispatch, SelfIntf);
    if cp.Advise(SelfIntf, c) <> S_OK then
{$ELSE}
    if cp.Advise(Self, c) <> S_OK then
{$ENDIF}
      raise Exception.Create('Error on IConnectionPoint.Advise');
  end
  else
{$IFDEF DELPHI6_UP}
    raise Exception.CreateFmt('Source don''t have connection point for [%s]',
      [GUIDToString(FSinkIID^)]);
{$ENDIF}
end;


function THtmlListener.Connect2(Source: IUnknown;
  aAgent: TObject = nil): IHubLink;
begin
  Flink := THtmlListenerLink.Create(Self, aAgent);
  Flink.Connect(Source);
  Result := Flink as IHubLink;
end;

procedure THtmlListener.setHandlers(const Value: THandlerCollection);
begin
  FHandlers.Assign(Value);
end;

procedure THtmlListener.setSinkKind(const Value: TSinkKind);
begin
  FSinkKind := Value;
  FSinkIID := mshtmlEventGUIDs[Value];
end;

procedure THtmlListener.Update(Item: TEventHandlerItem);
begin
  if csDestroying in ComponentState then
    Exit;
  if FDispList = nil then
    FDispList := TList.Create;
  if Item = nil then
    FillDispList
  else
  begin
    FDispList.Remove(Item);
    if Item.EventID <> eiUnknown then
      AddDisp(Item);
  end;
end;

procedure THtmlListener.AddDisp(Item: TEventHandlerItem);
var
  I: Integer;
begin
  if Find(Item.fEvID, I) then
    raise Exception.CreateFmt('Handler with EventID = %s already exists.',
      [_eventNames[Item.EventID]]);
  FDispList.Insert(I, Item);
end;

procedure THtmlListener.FillDispList;
var
  I: Integer;
begin
  FDispList.Clear;
  FDispList.Capacity := FHandlers.Count;
  for I := 0 to FHandlers.Count - 1 do
    if FHandlers[I].EventID <> eiUnknown then
      AddDisp(FHandlers[I]);
end;

function THtmlListener.Find(DispID: TEventID; var Index: Integer): Boolean;
var
  L, H, I: Integer;
begin
  Result := False;
  if FDispList = nil then
    Exit;

  L := 0;
  H := FDispList.Count - 1;
  while L <= H do
  begin
    I := (L + H) shr 1;
    if TEventHandlerItem(FDispList[I]).FEvID < DispID then
      L := I + 1
    else
    begin
      H := I - 1;
      if TEventHandlerItem(FDispList[I]).FEvID = DispID then
      begin
        Result := True;
        Index := I;
        Exit;
      end;
    end;
  end;
  Index := L;
end;

procedure THtmlListener.DispatchEvent(Sender: TObject; DispID: TEventID;
  Event: IHTMLEventObj);
var
  I: Integer;
begin
  if Find(DispID, I) then
    with TEventHandlerItem(FDispList[I]) do
      if Assigned(OnHandle) then
        OnHandle(Sender, Event);
end;

function THtmlListener.QueryInterface(const IID: TGUID; out Obj): HRESULT;
{$IFDEF DELPHI5}
var
  SelfIntf: IDispatch;
{$ENDIF}
begin
  Result := S_OK;
  if GetInterface(IID, Obj) then
    Exit;
  if IsEqualGuid(IID, fSinkIID^) then
{$IFDEF DELPHI5}
  begin
    GetInterface(IDispatch, SelfIntf);
    IUnknown(Obj) := SelfIntf;
  end
{$ELSE}
    IUnknown(Obj) := Self as IDispatch
{$ENDIF}
  else
    Result := E_NOINTERFACE;
end;

function THtmlListener.GetIDsOfNames(const IID: TGUID; Names: Pointer;
  NameCount, LocaleID: Integer; DispIDs: Pointer): HRESULT;
begin
  Result := E_NOTIMPL;
end;

function THtmlListener.GetTypeInfo(Index, LocaleID: Integer;
  out TypeInfo): HRESULT;
begin
  Result := DISP_E_BADINDEX;
end;

function THtmlListener.GetTypeInfoCount(out Count: Integer): HRESULT;
begin
  Count := 0;
  Result := S_OK;
end;

function THtmlListener.Invoke(DispID: Integer; const IID: TGUID;
  LocaleID: Integer; Flags: Word; var Params; VarResult, ExcepInfo,
  ArgErr: Pointer): HRESULT;
var
  Event: IHTMLEventObj;
begin
  Result := S_OK;
  try
    if Flags and DISPATCH_METHOD <> 0 then
    begin
      if (TDispParams(Params).cArgs = 0) or
        not VarSupports(POleVariant(TDispParams(Params).rgvarg)^, IHTMLEventObj,
        Event) then
        Event := nil;
      DispatchEvent(Self, TEventID(DispID), Event);
      //      if VarResult<>nil then
      //         POleVariant(VarResult)^ := False;
    end;
  except
    on E: Exception do
    begin
      Result := DISP_E_EXCEPTION;
      with PExcepInfo(ExcepInfo)^ do
      begin
        wCode := 9999;
        bstrDescription := E.Message;
        bstrSource := E.ClassName;
        dwHelpContext := E.HelpContext;
      end;
    end;
  end;
end;


{ THtmlListenerLink }

constructor THtmlListenerLink.Create(aHub: THtmlListener; aAgent: TObject);
begin
  inherited Create;
  FHub := aHub;
  FAgent := aAgent;
end;

procedure THtmlListenerLink.Disconnect;
begin
  if FCP <> nil then
  try
    FCP.Unadvise(FSinkCookies);
  finally
    FCP := nil;
  end;
end;

procedure THtmlListenerLink.Connect(Source: IUnknown);
var
  pcpc: IConnectionPointContainer;
begin
  if Supports(Source, IConnectionPointContainer, pcpc) and
    (pcpc.FindConnectionPoint(fHub.fSinkIID^, FCP) = S_OK) then
  begin
    if FCP.Advise(Self, FSinkCookies) <> S_OK then
      raise Exception.Create('Error on IConnectionPoint.Advise');
  end
  else
{$IFDEF DELPHI6_UP}
    raise Exception.CreateFmt('Source don''t have connection point for [%s]',
      [GUIDToString(FHub.FSinkIID^)]);
{$ENDIF}
end;

function THtmlListenerLink.Invoke(DispID: Integer; const IID: TGUID;
  LocaleID: Integer; Flags: Word; var Params; VarResult, ExcepInfo,
  ArgErr: Pointer): HRESULT;
var
  Event: IHTMLEventObj;
begin
  Result := S_OK;
  try
    if Flags and DISPATCH_METHOD <> 0 then
    begin
      if (TDispParams(Params).cArgs = 0) or
        not VarSupports(POleVariant(TDispParams(Params).rgvarg)^, IHTMLEventObj,
        Event) then
        Event := nil;
      FHub.DispatchEvent(FAgent, TEventID(DispID), Event);
    end;
  except
    on E: Exception do
    begin
      Result := DISP_E_EXCEPTION;
      with PExcepInfo(ExcepInfo)^ do
      begin
        wCode := 9999;
        bstrDescription := E.Message;
        bstrSource := E.ClassName;
        dwHelpContext := E.HelpContext;
      end;
    end;
  end;
end;

end.
