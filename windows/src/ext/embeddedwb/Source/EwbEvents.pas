//*************************************************************
//                        EwbEvents                           *
//                                                            *
//                     Freeware Unit                          *
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
//$Id: EwbEvents.pas,v 1.1.2.1 2006/11/29 22:13:01 sergev Exp $

unit EwbEvents;

interface

{$I EWB.inc}

uses

  Windows, Classes, ActiveX, Mshtml_Ewb, EwbClasses, wbhFixes;

type
  TSinkKind = (
    skElement,
    skLink,
    skForm,
    skControl,
    skTextContainer,
    skImg,
    skAnchor,
    skLabel,
    skSelect,
    skButton,
    skInputText,
    skOptionButton,
    skInputFile,
    skInputImage,
    skMarquee,
    skWindow,
    skDocument,
    skMap,
    skArea,
    skTable,
    skScript,
    skObject,
    skFrameSite,
    skStyle,
    skNamespace
    );

  TEventID = Integer;
const
  heOnAbort = 1000;
  heOnChange = 1001; //f
  heOnError = 1002;
  // HTMLWindowEvents2.onerror(const description: WideString; const url: WideString; line: Integer); dispid 1002;
  heOnLoad = 1003;
  heOnSelect = 1006;
  heOnSubmit = 1007; //f
  heOnUnload = 1008;
  heOnBounce = 1009;
  heOnFinish = 1010;
  heOnStart = 1011;
  heOnScroll = 1014;
  heOnReset = 1015; //f
  heOnresize = 1016;
  heOnBeforeUnload = 1017;
  heOncontextmenu = 1023; //f
  heOnBeforePrint = 1024;
  heOnAfterPrint = 1025;
  heOnStop = 1026; //f
  heOnBeforeEditFocus = 1027;
  heOnlayoutcomplete = 1030;
  heOnPage = 1031;
  heOnMousewheel = 1033; //f
  heOnBeforeDeactivate = 1034; //f
  heOnMove = 1035;
  heOnControlSelect = 1036; //f
  heOnSelectionChange = 1037;
  heOnMoveStart = 1038; //f
  heOnMoveEnd = 1039;
  heOnResizeStart = 1040; //f
  heOnResizeEnd = 1041;
  heOnMouseEnter = 1042;
  heOnMouseLeave = 1043;
  heOnActivate = 1044;
  heOnDeactivate = 1045;
  heOnBeforeActivate = 1047; //f
  heOnfocusIn = 1048;
  heOnfocusOut = 1049;

  heOnClick = -600; //f
  heOnDblClick = -601; //f
  heOnKeyDown = -602;
  heOnKeyPress = -603; //f
  heOnKeyUp = -604;
  heOnMouseDown = -605;
  heOnMouseMove = -606;
  heOnMouseUp = -607;
  heOnReadyStateChange = -609;

  heOnCellChange = -2147418078;
  heOnRowsInserted = -2147418079;
  heOnRowsDelete = -2147418080;
  heOnBeforePaste = -2147418081; //f
  heOnBeforeCopy = -2147418082; //f
  heOnBeforeCut = -2147418083; //f
  heOnPaste = -2147418084; //f
  heOnCopy = -2147418085; //f
  heOnCut = -2147418086; //f
  heOnDrop = -2147418087; //f
  heOnDragLeave = -2147418088;
  heOnDragOver = -2147418089; //f
  heOnDragEnter = -2147418090; //f
  heOnDragEnd = -2147418091;
  heOnDrag = -2147418092; //f
  //  heOnObjReadyStateChange  = -2147418092; //HTMLObjectElementEvents2
  heOnPropertyChange = -2147418093;
  //  heOnObjError        = -2147418093; //f HTMLObjectElementEvents2
  heOnLoseCapture = -2147418094;
  heOnFilterChange = -2147418095;
  heOnDatasetComplete = -2147418096;
  heOnDataAvailable = -2147418097;
  heOnDatasetChanged = -2147418098;
  heOnErrorUpdate = -2147418099; //f
  heOnSelectStart = -2147418100; //f
  heOnDragStart = -2147418101; //f
  heOnHelp = -2147418102; //f
  heOnMouseOut = -2147418103;
  heOnMouseOver = -2147418104;
  heOnRowEnter = -2147418105;
  heOnRowExit = -2147418106; //f
  heOnAfterUpdate = -2147418107;
  heOnBeforeUpdate = -2147418108; //f
  heOnFocus = -2147418111;
  heOnBlur = -2147418112;

type
  IHubLink = interface
    ['{EC22D916-CC24-4D88-A7ED-20C5E9AE0852}']
    procedure Disconnect;
  end;

  TMshtmlHandler = function(Sender: IUnknown): Boolean of object;

  TMSHTMLEventHandler = class(TInterfacedDispatchObject, IDispatchEx)
  private
    FHandler: TMshtmlHandler;
  protected
    function Invoke(DispID: Integer; const IID: TGUID; LocaleID: Integer;
      Flags: Word; var Params; VarResult, ExcepInfo, ArgErr: Pointer): HResult;
      override; stdcall;
    {IDispatchEx}
    function GetDispID(const bstrName: TBSTR; const grfdex: DWORD;
      out id: TDispID): HResult; stdcall;
    function InvokeEx(const id: TDispID; const lcid: LCID; const wflags:
      WORD; const pdp: PDispParams; out varRes: OleVariant; out pei:
      TExcepInfo; const pspCaller: PServiceProvider): HResult; stdcall;
    function DeleteMemberByName(const bstr: TBSTR;
      const grfdex: DWORD): HResult; stdcall;
    function DeleteMemberByDispID(const id: TDispID): HResult; stdcall;
    function GetMemberProperties(const id: TDispID; const grfdexFetch:
      DWORD; out grfdex: DWORD): HResult; stdcall;
    function GetMemberName(const id: TDispID; out bstrName: TBSTR):
      HResult; stdcall;
    function GetNextDispID(const grfdex: DWORD; const id: TDispID;
      out nid: TDispID): HResult; stdcall;
    function GetNameSpaceParent(out unk: IUnknown): HResult; stdcall;
  public
    constructor Create(Handler: TMshtmlHandler);
    destructor Destroy; override;
  end;

function MSHTMLEventHandler(Handler: TMshtmlHandler): IDispatchEx;
const
  mshtmlEventGUIDs: array[TSinkKind] of PGUID = (
    @DIID_HTMLElementEvents2, //IHTMLElement
    @DIID_HTMLLinkElementEvents2, //IHTMLLinkElement
    @DIID_HTMLFormElementEvents2, //IHTMLFormElement
    @DIID_HTMLControlElementEvents2, //IHTMLControlElement
    @DIID_HTMLTextContainerEvents2, //IHTMLTextContainer
    @DIID_HTMLImgEvents2, //IHTMLImgElement
    @DIID_HTMLAnchorEvents2, //IHTMLAnchorElement
    @DIID_HTMLLabelEvents2, //IHTMLLabelElement
    @DIID_HTMLSelectElementEvents2, //IHTMLSelectElement
    @DIID_HTMLButtonElementEvents2, //IHTMLButtonElement
    @DIID_HTMLInputTextElementEvents2, //IHTMLInputTextElement
    @DIID_HTMLOptionButtonElementEvents2, //IHTMLOptionButtonElement
    @DIID_HTMLInputFileElementEvents2, //IHTMLInputFileElement
    @DIID_HTMLInputImageEvents2, //IHTMLInputImage
    @DIID_HTMLMarqueeElementEvents2, //IHTMLMarqueeElement
    @DIID_HTMLWindowEvents2, //IHTMLWindow2
    @DIID_HTMLDocumentEvents2, //IHTMLDocument
    @DIID_HTMLMapEvents2, //IHTMLMapElement
    @DIID_HTMLAreaEvents2, //IHTMLAreaElement
    @DIID_HTMLTableEvents2, //IHTMLTable
    @DIID_HTMLScriptEvents2, //IHTMLScriptElement
    @DIID_HTMLObjectElementEvents2, //IHTMLObjectElement
    @DIID_HTMLFrameSiteEvents2,
    @DIID_HTMLStyleElementEvents2, //IHTMLStyleElement
    @DIID_HTMLNamespaceEvents //IHTMLNamespace
    );

implementation
uses SysUtils;

{ TMSHTMLEventHandler }

function MSHTMLEventHandler(Handler: TMshtmlHandler): IDispatchEx;
begin
  Result := TMSHTMLEventHandler.Create(Handler) as IDispatchEx;
end;

constructor TMSHTMLEventHandler.Create(Handler: TMshtmlHandler);
begin
  inherited Create;
  FHandler := Handler;
end;

destructor TMSHTMLEventHandler.Destroy;
begin
  inherited;
end;

function TMSHTMLEventHandler.DeleteMemberByDispID(const id: TDispID): HResult;
begin
  Result := S_FALSE;
end;

function TMSHTMLEventHandler.DeleteMemberByName(const bstr: TBSTR;
  const grfdex: DWORD): HResult;
begin
  Result := S_FALSE;
end;

function TMSHTMLEventHandler.GetDispID(const bstrName: TBSTR;
  const grfdex: DWORD; out id: TDispID): HResult;
begin
  Result := DISP_E_UNKNOWNNAME;
end;

function TMSHTMLEventHandler.GetMemberName(const id: TDispID;
  out bstrName: TBSTR): HResult;
begin
  Result := DISP_E_UNKNOWNNAME;
end;

function TMSHTMLEventHandler.GetMemberProperties(const id: TDispID;
  const grfdexFetch: DWORD; out grfdex: DWORD): HResult;
begin
  Result := DISP_E_UNKNOWNNAME;
end;

function TMSHTMLEventHandler.GetNameSpaceParent(
  out unk: IUnknown): HResult;
begin
  Result := E_NOTIMPL;
end;

function TMSHTMLEventHandler.GetNextDispID(const grfdex: DWORD;
  const id: TDispID; out nid: TDispID): HResult;
begin
  Result := S_FALSE;
end;

function TMSHTMLEventHandler.Invoke(DispID: Integer; const IID: TGUID;
  LocaleID: Integer; Flags: Word; var Params; VarResult, ExcepInfo,
  ArgErr: Pointer): HResult;
var
  Instance: IUnknown;
  R: Boolean;
begin
  Result := S_OK;
  if (Flags and DISPATCH_METHOD = 0) then Exit;
  try
    Instance := POleVariant(TDispParams(Params).rgvarg)^;
    R := FHandler(Instance);
    if VarResult <> nil then
      POleVariant(VarResult)^ := R;
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


function TMSHTMLEventHandler.InvokeEx(const id: TDispID; const lcid: LCID;
  const wflags: WORD; const pdp: PDispParams; out varRes: OleVariant;
  out pei: TExcepInfo; const pspCaller: PServiceProvider): HResult;
var
  Instance: IUnknown;
begin
  Result := S_OK;
  if (wflags and DISPATCH_METHOD = 0) then Exit;
  try
    Instance := POleVariant(pdp.rgvarg)^;
    varRes := FHandler(Instance);
  except
    on E: Exception do
    begin
      Result := DISP_E_EXCEPTION;
      with pei do
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
