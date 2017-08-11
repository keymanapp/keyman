(*
  Name:             EwbCore
  Copyright:        Copyright (C) 2003-2017 SIL International.
  Documentation:    
  Description:      
  Create Date:      1 May 2014

  Modified Date:    1 May 2014
  Authors:          mcdurdin
  Related Files:    
  Dependencies:     

  Bugs:             
  Todo:             
  Notes:            
  History:          01 May 2014 - mcdurdin - I4214 - V9.0 - Keyman spams message 0x0401 and frequent timer messages too
                    
*)
//*************************************************************
//                          TEwbCore                          *
//							      *
//                     Freeware Component                     *
//                       For Delphi                           *
//                       For Delphi 5 to Delphi XE            *
//                                                            *
//      Developing Team:                                      *
//          Eran Bodankin (bsalsa) -(bsalsa@gmail.com)        *
//          Serge Voloshenyuk (SergeV@bsalsa.com)             *
//          Thomas Stutz (smot777@yahoo.com                   *
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

You may use/change/modify the component under 4 conditions:
1. In your web site, add a link to "http://www.bsalsa.com"
2. In your application, add credits to "Embedded Web Browser"
3. Mail me  (bsalsa@gmail.com) any code change in the unit for the benefit
   of the other users.
4. Please, consider donation in our web site!
{*******************************************************************************}

unit EwbCore;

interface

{$I EWB.inc}

uses
{$IFDEF DELPHI6_UP}Variants, {$ENDIF}
  Dialogs, Windows, Messages, Classes, MSHTML_EWB, EWBAcc, Controls, Forms,
  ExtCtrls, ActiveX, ShlObj, SHDocVw_EWB, UrlMon, EwbIEConst;

type
  TCustomEmbeddedWB = class;

  TOleCmdArray = array of TOleCmd;
  TDragDropHandlingType = (
    ddtMS, // Microsoft implementation
    ddtMy, // TCustomEmbeddedWB event handlers
    ddtCustom, // granted by user in OnGetDropTarget event handler
    ddtNo // No drag and drop
    );
  TDocDesignMode = (ddmOn, ddmOff, ddmInherit, ddmUnknown);

  TIEPopupMenu = (
    rcmDefault,
    rcmImage,
    rcmControl,
    rcmTable,
    rcmSelText,
    rcmAnchor,
    rcmUnKnown,
    rcmImageArt,
    rcmImgDynSrc,
    rcmDebug,
    rcmAll
    );
  TIEPopupMenus = set of TIEPopupMenu;

  TIEPopupMenuItem = (
    rcsBack,
    rcsForward,
    rcsSavePageBkg,
    rcsSetAsBkg,
    rcsCopyBkg,
    rcsSetAsDeskT,
    rcsSelectAll,
    rcsPaste,
    rcsCreateSC,
    rcsAddToFav,
    rcsEncoding,
    rcsRefresh,
    rcsViewSource,
    rcsProperties,
    rcsPrint,
    rcsOpenNWindow,
    rcsOpenLink
    );
  TIEPopupMenuItems = set of TIEPopupMenuItem;

  {============================================================================}
  { Controlling Download and Execution }
  { http://msdn.microsoft.com/en-us/library/aa770041.aspx }
  { TDownloadControlOption = (
       DLCTL_DLIMAGES, DLCTL_VIDEOS, DLCTL_BGSOUNDS,
       DLCTL_NO_SCRIPTS, DLCTL_NO_JAVA,
       DLCTL_NO_RUNACTIVEXCTLS, DLCTL_NO_DLACTIVEXCTLS,
       DLCTL_DOWNLOADONLY,
       DLCTL_NO_FRAMEDOWNLOAD,
       DLCTL_RESYNCHRONIZE,
       DLCTL_PRAGMA_NO_CACHE,
       DLCTL_NO_BEHAVIORS, DLCTL_NO_METACHARSET,
       DLCTL_URL_ENCODING_DISABLE_UTF8, DLCTL_URL_ENCODING_ENABLE_UTF8,
       DLCTL_FORCEOFFLINE, DLCTL_NO_CLIENTPULL,
       DLCTL_SILENT, DLCTL_OFFLINE);
  }
  TDownloadControlOption = (
    DownloadImages, DownloadVideos, DownloadBGSounds, DontExecuteScripts,
    DontExecuteJava, DontExecuteActiveX, DontDownloadActiveX,
    DownloadButDontDisplay, DontDownloadFrame, CheckPageResynchronize,
    DownloadAndIgnoreCache, DontDownloadBehaviors, SuppressedMetaCharset,
    DisableUrlIfEncodingUTF8, EnableUrlIfEncodingUTF8,
    ForceOfflineMode, DontPerformClientPull, DownloadInSilentMode, WorkOffline);
  TDownloadControlOptions = set of TDownloadControlOption;

  { Doc Host Flags:
    http://msdn.microsoft.com/en-us/library/aa753277.aspx }
  { TUserInterfaceOption = (DIALOG, DISABLE_HELP_MENU, NO3DBORDER,
      SCROLL_NO, DISABLE_SCRIPT_INACTIVE, OPENNEWWIN, DISABLE_OFFSCREEN,
      FLAT_SCROLLBAR, DIV_BLOCKDEFAULT, ACTIVATE_CLIENTHIT_ONLY,
      OVERRIDEBEHAVIORFACTORY,
      CODEPAGELINKEDFONTS, URL_ENCODING_DISABLE_UTF8,
      URL_ENCODING_ENABLE_UTF8,
       ENABLE_FORMS_AUTOCOMPLETE, ENABLE_INPLACE_NAVIGATION,
      IME_ENABLE_RECONVERSION,
      THEME, NOTHEME, NOPICS, NO3DOUTERBORDER, DISABLE_EDIT_NS_FIXUP,
      LOCAL_MACHINE_ACCESS_CHECK, DISABLE_UNTRUSTEDPROTOCOL,
      HOST_NAVIGATES, ENABLE_REDIRECT_NOTIFICATION, USE_WINDOWLESS_SELECTCONTROL,
      USE_WINDOWED_SELECTCONTROL, ENABLE_ACTIVEX_INACTIVATE_MODE);
  }
  TUserInterfaceOption = (DisableTextSelect, DisableHelpMenu, DontUse3DBorders,
    DontUseScrollBars, PostponeScriptUntilActive, ForceOpenNewWindow,
    Reserved_OFFSCREEN,
    ForceFlatScrollBars, InsertDivTagOnEditMode, ActivateUIOnlyOnDocClick,
    ConsultBeforeRetrievingBehavior,
    CheckFontSupportsCodePage, DisableSubmitUrlInUTF8,
    EnableSubmitUrlInUTF8,
    EnablesFormsAutoComplete, ForceSameWindowNavigation,
    EmableImeLocalLanguages,
    EnableThemes, DisableThemes, DisablePicsRatings, DisableFrameSetBorder,
    DisablesAutoNameSpaceCorrection,
    DisableLocalFileAccess, DisableUntrustedProtocol,
    CheckNavigationDelegatedToHost, EnableRedirectNotification, EnableDomWindlessControls,
    EnableWindowedControls, ForceUserActivationOnActiveXJava);
  TUserInterfaceOptions = set of TUserInterfaceOption;

  {events}
  TMenuPreprocess = procedure(Sender: TObject; ID: DWORD; Menu: HMENU; const Context: IDispatch) of object;

  TEWBNotifyEvent = procedure(Sender: TObject; var Rezult: HRESULT) of object;
  TBoolQueryEvent = procedure(Sender: TObject; var Value: BOOL) of object;
  TMaskedCtrlCharEvent = procedure(Sender: TCustomEmbeddedWB; MaskedChar: Char) of object;
  TOMWindowMoveEvent = procedure(Sender: TCustomEmbeddedWB; cx, cy: Integer) of object;

  {IDocHostShowUI Interface}
  TShowHelpEvent = function(Sender: TObject; HWND: THandle; pszHelpFile: POleStr; uCommand: Integer;
    dwData: Longint; ptMouse: TPoint;
    var pDispatchObjectHit: IDispatch): HRESULT of object;
  TShowMessageEvent = function(Sender: TObject; HWND: THandle;
    lpstrText: POleStr; lpstrCaption: POleStr; dwType: Longint; lpstrHelpFile: POleStr;
    dwHelpContext: Longint; var plResult: LRESULT): HRESULT of object;
  {IDocHostUIHandler Interface}
  TEnableModelessEvent = procedure(Sender: TCustomEmbeddedWB; const fEnable: BOOL) of object;
  TFilterDataObjectEvent = procedure(Sender: TCustomEmbeddedWB; const pDO: IDataObject;
    var ppDORet: IDataObject) of object;
  TGetDropTargetEvent = procedure(Sender: TCustomEmbeddedWB; var DropTarget: IDropTarget) of object;
  TGetExternalEvent = procedure(Sender: TCustomEmbeddedWB; var ppDispatch: IDispatch) of object;
  TGetHostInfoEvent = procedure(Sender: TCustomEmbeddedWB; var pInfo: TDOCHOSTUIINFO) of object;
  TGetOptionKeyPathEvent = procedure(Sender: TCustomEmbeddedWB; var pchKey: POleStr) of object;
  TOnActivateEvent = procedure(Sender: TCustomEmbeddedWB; const fActivate: BOOL) of object;
  TResizeBorderEvent = procedure(Sender: TCustomEmbeddedWB; const prcBorder: PRect;
    const pUIWindow: IOleInPlaceUIWindow;
    const fRameWindow: BOOL) of object;
  TShowContextMenuEvent = procedure(Sender: TCustomEmbeddedWB; const dwID: DWORD; const ppt: PPOINT;
    const CommandTarget: IUnknown; const Context: IDispatch; var Result: HRESULT) of object;
  TShowUIEvent = procedure(Sender: TCustomEmbeddedWB; const dwID: DWORD; const
    pActiveObject: IOleInPlaceActiveObject;
    const pCommandTarget: IOleCommandTarget; const pFrame: IOleInPlaceFrame;
    const pDoc: IOleInPlaceUIWindow; var Rezult: HRESULT) of object;
  TTranslateAcceleratorEvent = procedure(Sender: TCustomEmbeddedWB; const lpMsg: PMSG;
    const pguidCmdGroup: PGUID;
    const nCmdID: DWORD; var Done: Boolean) of object;
  TTranslateUrlEvent = procedure(Sender: TCustomEmbeddedWB; const pchURLIn: POleStr;
    var ppchURLOut: WideString) of object;

{$IFDEF USE_IOLECOMMANDTARGET}
  TRefreshEvent = procedure(Sender: TCustomEmbeddedWB; CmdID: Integer; var Cancel: Boolean) of object;
{$ENDIF}
  {INewWindowManager Interface}
  TEvaluateNewWindowEvent = procedure(Sender: TCustomEmbeddedWB; pszUrl, pszName,
    pszUrlContext, pszFeatures: LPCWSTR;
    fReplace: BOOL; dwFlags, dwUserActionTime: DWORD; var Rezult: HRESULT) of object;
  {IDownloadManager Interface}
  TDownloadEvent = procedure(Sender: TCustomEmbeddedWB; pmk: IMoniker; pbc: IBindCtx;
    dwBindVerb: DWORD;
    grfBINDF: DWORD; pBindInfo: PBindInfo; pszHeaders: PWideChar;
    pszRedir: PWidechar; uiCP: UINT; var Rezult: HRESULT) of object;
  {IAuthenticate Interface}
  TAuthenticateEvent = procedure(Sender: TCustomEmbeddedWB; var hwnd: HWnd;
    var szUserName, szPassWord: WideString; var Rezult: HRESULT) of object;
  {IZoomEvents Interface}
  TZoomPercentChangedEvent = function(Sender: TCustomEmbeddedWB; const ulZoomPercent: uLong): HRESULT of object;
  {Script Error handling}
  TScriptErrorAction = (eaContinue, eaCancel, eaAskUser);
  TScriptErrorEvent = procedure(Sender: TObject; ErrorLine, ErrorCharacter, ErrorCode, ErrorMessage, ErrorUrl: string;
    var ScriptErrorAction: TScriptErrorAction) of object;

  {User Agent Mode Event}
  TSetUserAgentEvent = function(var UserAgent: string): HRESULT of object;

  { TCustomEmbeddedWB }
  TCustomEmbeddedWB = class(TEWB
      , IDispatch // http://msdn.microsoft.com/en-us/library/ms221608.aspx
      , IDocHostShowUI // http://msdn.microsoft.com/en-us/library/aa753269.aspx
      , IDocHostUIHandler // http://msdn.microsoft.com/en-us/library/aa753260(VS.85).aspx
      , IDocHostUIHandler2 // http://msdn.microsoft.com/en-us/library/aa753275(VS.85).aspx
      , IDropTarget // http://msdn.microsoft.com/en-us/library/ms679679.aspx
{$IFDEF USE_IOLECOMMANDTARGET}
      , IOleCommandTarget // http://msdn.microsoft.com/en-us/library/ms683797.aspx
{$ENDIF}
      , IServiceProvider // http://msdn.microsoft.com/en-us/library/cc678965(VS.85).aspx
      , INewWindowManager // http://msdn.microsoft.com/en-us/library/bb775418(VS.85).aspx
      , IProtectFocus // http://msdn2.microsoft.com/en-us/library/aa361771.aspx
      , IDownloadManager // http://msdn.microsoft.com/en-us/library/aa753613(VS.85).aspx
      , IHTMLOMWindowServices //http://msdn.microsoft.com/library/default.asp?url=/workshop/browser/hosting/reference/ifaces/IHTMLOMWindowServices/IHTMLOMWindowServices.asp
      , IHostBehaviorInit // http://msdn.microsoft.com/en-us/library/aa753687(VS.85).aspx
      , IZoomEvents // http://msdn.microsoft.com/en-us/library/aa770056(VS.85).aspx
      , IAuthenticate // http://msdn.microsoft.com/en-us/library/ms835407.aspx
      )

  private
    FOnZoomPercentChanged: TZoomPercentChangedEvent;
    FOnGetIDsOfNames: TGetIDsOfNamesEvent;
    FOnGetTypeInfo: TGetTypeInfoEvent;
    FOnGetTypeInfoCount: TGetTypeInfoCountEvent;
    FOnInvoke: TInvokeEvent;
    FDownloadControlOptions: TDownloadControlOptions;
    FOnShowMessage: TShowMessageEvent;
    FOnShowHelp: TShowHelpEvent;
    FHelpFile: string;
    fOptionKeyPath: string;
    fOverOptionKeyPath: Boolean;
    FOnFilterDataObject: TFilterDataObjectEvent;
    FOnGetExternal: TGetExternalEvent;
    FOnGetHostInfo: TGetHostInfoEvent;
    FUserInterfaceOptions: TUserInterfaceOptions;
    FOnEnableModeless: TEnableModelessEvent;
{$IFDEF GETKEYPATH_HANDLERS}
    FOnGetOptionKeyPath: TGetOptionKeyPathEvent;
    FOnGetOverrideKeyPath: TGetOptionKeyPathEvent;
{$ENDIF}
    FOnGetDropTarget: TGetDropTargetEvent;
    FOnHideUI: TEWBNotifyEvent;
    FOnOnDocWindowActivate: TOnActivateEvent;
    FOnOnFrameWindowActivate: TOnActivateEvent;
    FOnResizeBorder: TResizeBorderEvent;
    FOnShowContextmenu: TShowContextMenuEvent;
    FOnShowUI: TShowUIEvent;
    FOnTranslateAccelerator: TTranslateAcceleratorEvent;
    FOnTranslateUrL: TTranslateUrlEvent;
    FOnUpdateUI: TEWBNotifyEvent;
    FOnDragLeaveEvent: TNotifyEvent;
    FOnDragEnterEvent: TOnDragEnterEvent;
    FOnDragOverEvent: TOnDragOverEvent;
    FOnDropEvent: TOnDropEvent;
    FOnScriptError: TScriptErrorEvent;
    FScriptErrorAction: TScriptErrorAction;
{$IFDEF USE_IOLECOMMANDTARGET}
    FOnUnload: TNotifyEvent;
    FOnStop: TNotifyEvent;
    FOnRefresh: TRefreshEvent;
    FOnCommandExec: TComTargetExecEvent;
{$ENDIF}
    FOnQueryService: TQueryServiceEvent;
    FOnEvaluateNewWindow: TEvaluateNewWindowEvent;
    FCanGrabFocus: Boolean;
    FOnAllowFocusChange: TBoolQueryEvent;
    FOnDownload: TDownloadEvent;
    FDropHandlingType: TDragDropHandlingType;
    FZoomPercent: Integer;
    FDesignMode: Boolean;
    FDisabledPopupMenus: TIEPopupMenus;
    FOnFilterPopupMenu: TMenuPreprocess;
    FOnMaskedCtrlChar: TMaskedCtrlCharEvent;
    FDisableCtrlShortcuts: string;
    FOnResize: TOMWindowMoveEvent;
    FOnMoveBy: TOMWindowMoveEvent;
    FOnMove: TOMWindowMoveEvent;
    FOnResizeBy: TOMWindowMoveEvent;
    FFloatingHosting: Boolean;
    FOnPopulateNSTable: TNotifyEvent;
    FOnAuthenticate: TAuthenticateEvent;
{$IFDEF RESEARCH_MODE}
    FOnQueryInterface: OnQueryInterfaceEvent;
{$ENDIF}
    procedure SetDownloadOptions(const Value: TDownloadControlOptions);
    procedure SetUserInterfaceOptions(const Value: TUserInterfaceOptions);
    function GetDoc2: IHtmlDocument2;
    function GetDoc3: IHtmlDocument3;
    function GetDoc4: IHtmlDocument4;
    function GetDoc5: IHtmlDocument5;
    function GetElemByID(const ID: WideString): IHTMLElement;
    function GetZoom: Integer;
    procedure SetZoom(const Value: Integer);
    procedure SetOpticalZoom(const Value: Integer);
    function _getCookie: WideString;
    function GetCharSet: WideString;
    procedure SetCharSet(const Value: WideString);
    procedure SetDropHandlingType(const Value: TDragDropHandlingType);
    procedure SetDesignMode(const Value: Boolean);
    function GetDocDesignMode: TDocDesignMode;
    procedure SetDocDesignMode(const Value: TDocDesignMode);
    function GetBody: IHTMLElement;

  protected
    CurrentHandle: HWND; //jls
    procedure CreateWnd; override; //jls
    procedure DestroyWnd; override; //jls
  protected
{$IFDEF RESEARCH_MODE}
    { IInterface }
    function QueryInterface(const IID: TGUID; out Obj): HRESULT; override; stdcall;
{$ENDIF}
    {IDispatch Interface}
    function GetIDsOfNames(const IID: TGUID; Names: Pointer;
      NameCount, LocaleID: Integer; DispIDs: Pointer): HRESULT; stdcall;
    function GetTypeInfo(Index, LocaleID: Integer; out TypeInfo): HRESULT;
      stdcall;
    function GetTypeInfoCount(out Count: Integer): HRESULT; stdcall;
    function Invoke(DispID: Integer; const IID: TGUID; LocaleID: Integer;
      Flags: Word; var Params; VarResult, ExcepInfo, ArgErr: Pointer): HRESULT;
      stdcall;
    {IDocHostShowUI Interface }
    function ShowHelp(HWND: THandle; pszHelpFile: POleStr; uCommand: Integer;
      dwData: Longint; ptMouse: TPoint; var pDispatchObjectHit: IDispatch): HRESULT; stdcall;
    function ShowMessage(HWND: THandle; lpstrText: POleStr; lpstrCaption: POleStr;
      dwType: Longint; lpstrHelpFile: POleStr; dwHelpContext: Longint;
      var plResult: LRESULT): HRESULT; stdcall;
    {IDocHostUIHandler Interface}
    function EnableModeless(const fEnable: BOOL): HRESULT; stdcall;
    function FilterDataObject(const pDO: IDataObject; out ppDORet: IDataObject): HRESULT; stdcall;
    function GetDropTarget(const pDropTarget: IDropTarget;
      out ppDropTarget: IDropTarget): HRESULT; stdcall;
    function GetExternal(out ppDispatch: IDispatch): HRESULT; stdcall;
    function GetHostInfo(var pInfo: TDOCHOSTUIINFO): HRESULT; stdcall;
    function GetOptionKeyPath(out pchKey: POleStr; const dw: DWORD): HRESULT; stdcall;
    function HideUI: HRESULT; stdcall;
    function OnDocWindowActivate(const fActivate: BOOL): HRESULT; stdcall;
    function OnFrameWindowActivate(const fActivate: BOOL): HRESULT; stdcall;
    function ResizeBorder(const prcBorder: PRECT; const pUIWindow:
      IOleInPlaceUIWindow;
      const FrameWindow: BOOL): HRESULT; stdcall;
    function ShowContextMenu(const dwID: DWORD; const ppt: PPOINT;
      const CommandTarget: IUnknown; const Context: IDispatch): HRESULT; stdcall;
    function ShowUI(const dwID: DWORD; const pActiveObject:
      IOleInPlaceActiveObject;
      const pCommandTarget: IOleCommandTarget; const pFrame: IOleInPlaceFrame;
      const pDoc: IOleInPlaceUIWindow): HRESULT; stdcall;
    function TranslateAccelerator(const lpMsg: PMSG; const pguidCmdGroup: PGUID;
      const nCmdID: DWORD): HRESULT; stdcall;
    function TranslateUrl(const dwTranslate: DWORD; const pchURLIn: POleStr;
      out ppchURLOut: POleStr): HRESULT; stdcall;
    function UpdateUI: HRESULT; stdcall;
    {IDocHostUIHandler2 Interface}
    function GetOverrideKeyPath(out pchKey: POleStr; dw: DWORD): HRESULT; stdcall;
    {IDropTarget Interface}
    function DragEnter(const dataObj: IDataObject; grfKeyState: Longint;
      pt: TPoint; var dwEffect: Longint): HRESULT; stdcall;
    function IDropTarget.DragOver = DropTargetDragOver;
    function DropTargetDragOver(grfKeyState: Longint; pt: TPoint;
      var dwEffect: Longint): HRESULT; stdcall;
    function DragLeave: HRESULT; stdcall;
    function Drop(const dataObj: IDataObject; grfKeyState: Longint; pt: TPoint;
      var dwEffect: Longint): HRESULT; stdcall;
{$IFDEF USE_IOLECOMMANDTARGET}
    {IOleCommandTarget interface}
    function IOleCommandTarget.QueryStatus = CommandTarget_QueryStatus;
    function CommandTarget_QueryStatus(CmdGroup: PGUID; cCmds: Cardinal;
      prgCmds: POleCmd; CmdText: POleCmdText): HRESULT; stdcall;
    function IOleCommandTarget.Exec = CommandTarget_Exec;
    function CommandTarget_Exec(CmdGroup: PGUID; nCmdID, nCmdexecopt: DWORD;
      const vaIn: OleVariant; var vaOut: OleVariant): HRESULT; stdcall;
{$ENDIF}
    {IServiceProvider Interface}
    function QueryService(const rsid, iid: TGUID; out Obj): HRESULT; stdcall;
    {INewWindowManager Interface}
    function EvaluateNewWindow(pszUrl, pszName, pszUrlContext, pszFeatures: LPCWSTR;
      fReplace: BOOL; dwFlags, dwUserActionTime: DWORD): HRESULT; stdcall;
    {IProtectFocus IE7 interface}
    function AllowFocusChange(out pfAllow: BOOL): HRESULT; stdcall;
    {IDownloadManager Interface}
    function Download(
      pmk: IMoniker; // Identifies the object to be downloaded
      pbc: IBindCtx; // Stores information used by the moniker to bind
      dwBindVerb: DWORD; // The action to be performed during the bind
      grfBINDF: DWORD; // Determines the use of URL encoding during the bind
      pBindInfo: PBindInfo; // Used to implement IBindStatusCallback::GetBindInfo
      pszHeaders: PWidechar; // Additional headers to use with IHttpNegotiate
      pszRedir: PWidechar; // The URL that the moniker is redirected to
      uiCP: UINT // The code page of the object's display name
      ): HRESULT; stdcall;
    {IHostBehaviorInit}
    function PopulateNamespaceTable: HRESULT; stdcall;
    {IHTMLOMWindowServices Interface}
    function ResizeBy(const x, y: Integer): HRESULT; stdcall;
    function ResizeTo(const x, y: Integer): HRESULT; stdcall;
    function MoveBy(const x, y: Integer): HRESULT; stdcall;
    function MoveTo(const x, y: Integer): HRESULT; stdcall;
    {IZoomEvents  interface}
    function OnZoomPercentChanged(const ulZoomPercent: uLong): HRESULT; stdcall;
    {IAuthenticate}
    function Authenticate(var hwnd: HWnd; var szUserName, szPassWord: LPWSTR):
      HRESULT; stdcall;
  protected
    FDownloadOptionValue: Longint;
    FUserInterfaceValue: Cardinal;
    FOnSetUserAgent: TSetUserAgentEvent;
    FOnPreRefresh: TNotifyEvent;
    FOnHookChildWindow: TNotifyEvent;
    procedure UpdateDownloadControlValues;
    procedure UpdateUserInterfaceValues;
    function CopyOptionKeyPath(Overrided: Boolean): PWideChar;
    function DoFilterMsg(const lpMsg: PMSG): Boolean; virtual;
    function ScriptErrorHandler(const vaIn: OleVariant; var vaOut: OleVariant): HRESULT; virtual;
    function DoQueryService(const rsid, iid: TGUID; var Obj): Boolean; virtual;
    function FilterPopupMenu: Boolean; virtual;
    procedure DoFilterPopupMenu(Sender: TObject; ID: DWORD; Menu: HMENU; const Context:
      IDispatch); virtual;
    procedure MoveParentForm(x, y: Integer; Delta: Boolean);
    procedure ResizeParentForm(w, h: Integer; Delta: Boolean);
  public
    class function dwEffectToStr(Command: Int64): string;
    class procedure DropEffect(grfKeyState: Longint; var dwEffect: longint);
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function InvokeCommand(CmdGroup: PGUID; Cmd, nCmdexecopt: DWORD;
      var vaIn, vaOut: OleVariant): HRESULT; overload;
    function InvokeCommand(CmdGroup: PGUID; Cmd: DWORD): HRESULT; overload;
    function QueryCommandStatus(CmdGroup: PGUID; cCmds: Cardinal;
      prgCmds: POleCmd; CmdText: POleCmdText): HRESULT;
    function QueryCMDEnabled(CmdGroup: PGUID; cmdID: Cardinal): Boolean;
    function QueryCMDLatched(CmdGroup: PGUID; cmdID: Cardinal): Boolean;
    function QueryCMDStatus(CmdGroup: PGUID; cmdID: Cardinal): OLECMDF;
    function QueryCMDArrayStatus(CmdGroup: PGUID; cmds: TOleCmdArray): Boolean;

    procedure Client2HostWin(var CX, CY: Integer);
    // just call it in OnClientToHostWindow handler

    function GetIEWin(const ClassName: string): HWND;
    procedure SetFocusToDoc;
    procedure SetFocusToBody;
    procedure SetFocusToParent;

    function ZoomRangeHigh: Integer;
    function ZoomRangeLow: Integer;
    property Zoom: Integer read GetZoom write SetZoom;

    property Cookie: WideString read _getCookie;
    property DesignMode: Boolean read FDesignMode write SetDesignMode;
    {html functions}
    property Doc2: IHtmlDocument2 read GetDoc2;
    property Doc3: IHtmlDocument3 read GetDoc3;
    property Doc4: IHtmlDocument4 read GetDoc4;
    property Doc5: IHtmlDocument5 read GetDoc5;

    property Body: IHTMLElement read getBody;
    property DocDesignMode: TDocDesignMode read getDocDesignMode write
      setDocDesignMode;
    property CharactersSet: WideString read GetCharSet write SetCharSet;
    property ElementByID[const ID: WideString]: IHTMLElement read getElemByID;
    function ScrollToElement(Element: IHTMLElement): Boolean;

    function GetElementNamespaceTable(out aTable: IElementNamespaceTable):
      Boolean;

{$IFDEF RESEARCH_MODE}
    property OnQueryInterface: OnQueryInterfaceEvent read FOnQueryInterface write FOnQueryInterface;
{$ENDIF}
    property CanGrabFocus: Boolean read FCanGrabFocus write FCanGrabFocus default True;
  published
    property ZoomPercent: Integer read FZoomPercent write SetOpticalZoom default 100;
    property OnAllowFocusChange: TBoolQueryEvent read FOnAllowFocusChange write
      FOnAllowFocusChange;
    property DisableCtrlShortcuts: string read FDisableCtrlShortcuts write FDisableCtrlShortcuts;
    property DownloadOptions: TDownloadControlOptions read FDownloadControlOptions
      write SetDownloadOptions default [DownloadImages, DownloadVideos, DownloadBGSounds];
    property UserInterfaceOptions: TUserInterfaceOptions read FUserInterfaceOptions
      write SetUserInterfaceOptions default [];
    property HelpFile: string read FHelpFile write FHelpFile;
    property OptionKeyPath: string read FOptionKeyPath write FOptionKeyPath;
    property OverrideOptionKeyPath: Boolean read FOverOptionKeyPath write
      FOverOptionKeyPath default False;

    property DropHandlingType: TDragDropHandlingType read FDropHandlingType write
      setDropHandlingType default ddtMS;
    property DisabledPopupMenus: TIEPopupMenus
      read FDisabledPopupMenus write FDisabledPopupMenus default [];
    property FloatingHosting: Boolean read FFloatingHosting write
      fFloatingHosting default False;

    property OnGetIDsOfNames: TGetIDsOfNamesEvent read FOnGetIDsOfNames write
      FOnGetIdsOfNames;
    property OnGetTypeInfo: TGetTypeInfoEvent read FonGetTypeInfo write
      FOnGetTypeInfo;
    property OnGetTypeInfoCount: TGetTypeInfoCountEvent read FonGetTypeInfoCount
      write FOnGetTypeInfoCount;
    property OnInvoke: TInvokeEvent read FOnInvoke write FOnInvoke;
    property OnShowHelpRequest: TShowHelpEvent read FOnShowHelp write
      FOnShowHelp;
    property OnShowMessage: TShowMessageEvent read FOnShowMessage write
      FOnShowMessage;
    property OnFilterDataObject: TFilterDataObjectEvent read FOnFilterDataObject
      write FOnFilterDataObject;
    property OnGetExternal: TGetExternalEvent read FOnGetExternal write
      FOnGetExternal;
    property OnGetHostInfo: TGetHostInfoEvent read FOnGetHostInfo write
      FOnGetHostInfo;
    property OnEnableModeless: TEnableModelessEvent read FOnEnableModeless
      write FOnEnableModeless;
{$IFDEF GETKEYPATH_HANDLERS}
    property OnGetOptionKeyPath: TGetOptionKeyPathEvent read FOnGetOptionKeyPath
      write FOnGetOptionKeyPath;
    property OnGetOverrideKeyPath: TGetOptionKeyPathEvent read
      FOnGetOverrideKeyPath
      write FOnGetOverrideKeyPath;
{$ENDIF}
    property OnZoomPercentChange: TZoomPercentChangedEvent read FOnZoomPercentChanged write FOnZoomPercentChanged;
    property OnGetDropTarget: TGetDropTargetEvent read FOnGetDropTarget write FOnGetDropTarget;
    property OnHideUI: TEWBNotifyEvent read FOnHideUI write FOnHideUI;
    property OnOnDocWindowActivate: TOnActivateEvent read FOnOnDocWindowActivate
      write FOnOnDocWindowActivate;
    property OnOnFrameWindowActivate: TOnActivateEvent read
      FOnOnFrameWindowActivate
      write FOnOnFrameWindowActivate;
    property OnResizeBorder: TResizeBorderEvent read FOnResizeBorder write
      FOnResizeBorder;
    property OnShowContextMenu: TShowContextMenuEvent read FOnShowContextmenu
      write FOnShowContextmenu;
    property OnShowUI: TShowUIEvent read FOnShowUI write FOnShowUI;
    property OnTranslateAccelerator: TTranslateAcceleratorEvent read
      FOnTranslateAccelerator
      write FOnTranslateAccelerator;
    property OnTranslateUrl: TTranslateUrlEvent read FOnTranslateUrL
      write FOnTranslateUrL;
    property OnUpdateUI: TEWBNotifyEvent read FOnUpdateUI write FOnUpdateUI;

    property OnDragEnter: TOnDragEnterEvent read FOnDragEnterEvent write
      FOnDragEnterEvent;
    property OnDragLeave: TNotifyEvent read FOnDragLeaveEvent write
      FOnDragLeaveEvent;
    property OnDragOver2: TOnDragOverEvent read FOnDragOverEvent write
      FOnDragOverEvent;
    property OnDropEvent: TOnDropEvent read FOnDropEvent write FOnDropEvent;

    property OnScriptError: TScriptErrorEvent read FOnScriptError write FOnScriptError;
    property ScriptErrorAction: TScriptErrorAction read FScriptErrorAction
      write FScriptErrorAction default eaContinue;
{$IFDEF USE_IOLECOMMANDTARGET}
    property OnRefresh: TRefreshEvent read FOnRefresh write FOnRefresh;
    property OnStop: TNotifyEvent read FOnStop write FOnStop;
    property OnUnload: TNotifyEvent read FOnUnload write FOnUnload;
    property OnCommandExec: TComTargetExecEvent read FOnCommandExec write
      fOnCommandExec;
{$ENDIF}
    property OnQueryService: TQueryServiceEvent read FOnQueryService write
      FOnQueryService;
    property OnEvaluateNewWindow: TEvaluateNewWindowEvent read
      FOnEvaluateNewWindow
      write FOnEvaluateNewWindow;
    property OnFileDownload: TDownloadEvent read FOnDownload write FOnDownload;
    property OnFilterPopupMenu: TMenuPreprocess read FOnFilterPopupMenu write
      FOnFilterPopupMenu;
    property OnMaskedCtrlChar: TMaskedCtrlCharEvent read FOnMaskedCtrlChar write
      FOnMaskedCtrlChar;

    property OnMove: TOMWindowMoveEvent read FOnMove write FOnMove;
    property OnMoveBy: TOMWindowMoveEvent read FOnMoveBy write FOnMoveBy;
    property OnResize: TOMWindowMoveEvent read FOnResize write FOnResize;
    property OnResizeBy: TOMWindowMoveEvent read FOnResizeBy write FOnResizeBy;
    property OnPopulateNSTable: TNotifyEvent read FOnPopulateNSTable write
      FOnPopulateNSTable;
    property OnAuthenticate: TAuthenticateEvent read FOnAuthenticate write
      FOnAuthenticate;
    property OnPreRefresh: TNotifyEvent read FOnPreRefresh write FOnPreRefresh;
  end;

  TEwbCore = class(TCustomEmbeddedWB)
  private
    function IsCtrlCharMask: Boolean;
  published
    property DisableCtrlShortcuts stored IsCtrlCharMask;
  end;

  //this two functions for using in custom OnShowContextMenu handler.
function IsSeTIEPopupMenus(ID: DWORD; rcm: TIEPopupMenus): Boolean;
function ShowRightClickMenu(Sender: TObject; dwID: DWORD;
  const Target: IUnknown; const Context: IDispatch;
  const ppt: PPOINT;
  const EncodingSubMenu: OleVariant;
  preprocess: TMenuPreprocess = nil): Boolean;

implementation

uses
  SysUtils, ComObj, EwbCoreTools, Registry;

function IsSeTIEPopupMenus(ID: DWORD; rcm: TIEPopupMenus): Boolean;
begin
  Result := (rcmAll in rcm) or
    ((ID in [0..9]) and (TIEPopupMenu(ID) in rcm));
end;

function ShowRightClickMenu(Sender: TObject; dwID: DWORD; const Target: IUnknown; const Context:
  IDispatch;
  const ppt: PPOINT; const EncodingSubMenu: OleVariant;
  Preprocess: TMenuPreprocess = nil): Boolean;
var
  ShDocLcHandle: THandle;
  OleCommandTarget: IOleCommandTarget;
  OleWindow: IOleWindow;
  WindowHandle: HWND;
  ParentMenu, SubMenu: HMENU;
  SubMenuItemInfo: MENUITEMINFO;
  PopupResult: LongBool;
begin
  Result := False;
  ShDocLcHandle := GetSHDOCLCModule;

  if ShDocLcHandle = 0 then
    Exit;

  if Supports(Target, IOleCommandTarget, OleCommandTarget) and
    Supports(Target, IOleWindow, OleWindow) and
    ActiveX.Succeeded(OleWindow.GetWindow(WindowHandle)) then
  begin
    ParentMenu := Windows.LoadMenu(ShDocLcHandle,
      MAKEINTRESOURCE(CContextMenuID));
    if ParentMenu <> 0 then
      try
        SubMenu := GetSubMenu(ParentMenu, dwID);
        FillChar(SubMenuItemInfo, SizeOf(SubMenuItemInfo), 0);
        SubMenuItemInfo.cbSize := SizeOf(MENUITEMINFO);
        SubMenuItemInfo.fMask := MIIM_SUBMENU;
        SubMenuItemInfo.hSubMenu := HMENU(@EncodingSubMenu);
        SetMenuItemInfo(SubMenu, IDM_LANGUAGE, False, SubMenuItemInfo);

        if Assigned(Preprocess) then
          Preprocess(Sender, dwID, SubMenu, Context);

        PopupResult := Windows.TrackPopupMenuEx(SubMenu, TPM_LEFTALIGN
          or TPM_TOPALIGN or TPM_RETURNCMD or TPM_RIGHTBUTTON
          or TPM_HORPOSANIMATION or TPM_VERPOSANIMATION, ppt^.X, ppt^.Y,
          WindowHandle, nil);
        if PopupResult then
          SendMessage(WindowHandle, WM_COMMAND, MakeWParam(LOWORD(PopupResult), 0), 0);
        Result := True;
      finally
        DestroyMenu(ParentMenu);
      end;
  end;
end;

type
  { TnoDragDrop }
  TnoDragDrop = class(TInterfacedObject, IDropTarget)
  protected
    function DragEnter(const dataObj: IDataObject; grfKeyState: Longint;
      pt: TPoint; var dwEffect: Longint): HRESULT; stdcall;
    function DragOver(grfKeyState: Longint; pt: TPoint;
      var dwEffect: Longint): HRESULT; stdcall;
    function DragLeave: HRESULT; stdcall;
    function Drop(const dataObj: IDataObject; grfKeyState: Longint; pt: TPoint;
      var dwEffect: Longint): HRESULT; stdcall;
  end;

function TnoDragDrop.DragEnter(const dataObj: IDataObject; grfKeyState: Integer;
  pt: TPoint; var dwEffect: Integer): HRESULT;
begin
  dwEffect := DROPEFFECT_NONE;
  Result := S_OK;
end;

function TnoDragDrop.DragLeave: HRESULT;
begin
  Result := S_OK;
end;

function TnoDragDrop.DragOver(grfKeyState: Integer; pt: TPoint;
  var dwEffect: Integer): HRESULT;
begin
  dwEffect := DROPEFFECT_NONE;
  Result := S_OK;
end;

function TnoDragDrop.Drop(const dataObj: IDataObject; grfKeyState: Integer;
  pt: TPoint; var dwEffect: Integer): HRESULT;
begin
  dwEffect := DROPEFFECT_NONE;
  Result := S_OK;
end;

{ TCustomEmbeddedWB }

class function TCustomEmbeddedWB.dwEffectToStr(Command: Int64): string;
const
  E_UNSPEC = E_Fail;
begin
  case (Command) of
    DROPEFFECT_NONE: Result := 'Drop target cannot accept the data.';
    DROPEFFECT_COPY: Result := 'Drag source should copy the data.';
    DROPEFFECT_MOVE: Result := 'Drag source should remove the data.';
    DROPEFFECT_LINK: Result :=
      'Drag source should create a link to the original data.';
    DRAGDROP_S_DROP: Result := 'The drag-and-drop operation was successful.';
    DRAGDROP_S_CANCEL: Result := 'The drag-and-drop operation was canceled.';
    DRAGDROP_S_USEDEFAULTCURSORS: Result :=
      'Successful completion. Restoring defaults.';
    DRAGDROP_E_INVALIDHWND: Result :=
      'Invalid handle returned in the hwnd parameter.';
    DRAGDROP_E_NOTREGISTERED: Result :=
      'Failed to revoke a drop target that has not been registered.';
    E_UNSPEC: Result := 'Unexpected error occurred.';
    E_OUTOFMEMORY: Result := 'Out of memory.';
    7: Result := 'operation was successful.';
  else
    Result := 'Unknown.';
  end;
end;

class procedure TCustomEmbeddedWB.DropEffect(grfKeyState: Longint; var dwEffect:
  longint);
begin
  if (grfKeyState and MK_CONTROL = 0) and (grfKeyState and MK_SHIFT <> 0) and
    (dwEffect and DropEffect_Move <> 0) then
    dwEffect := DropEffect_Move
  else if (grfKeyState and MK_CONTROL <> 0) and (grfKeyState and MK_SHIFT <> 0)
    and
    (dwEffect and DropEffect_Link <> 0) then
    dwEffect := DropEffect_Link
  else if (dwEffect and DropEffect_Copy <> 0) then
    dwEffect := DropEffect_Copy
  else if (dwEffect and DropEffect_Move <> 0) then
    dwEffect := DropEffect_Move
  else if (dwEffect and DropEffect_Link <> 0) then
    dwEffect := DropEffect_Link
  else
    dwEffect := DropEffect_None;
end;

function TCustomEmbeddedWB.AllowFocusChange(out pfAllow: BOOL): HRESULT;
begin
  Result := S_OK;
  pfAllow := CanGrabFocus;
  if Assigned(OnAllowFocusChange) then
    OnAllowFocusChange(Self, pfAllow);
end;

function TCustomEmbeddedWB.CopyOptionKeyPath(Overrided: Boolean): PWideChar;
begin
  if (OptionKeyPath = '') or
    (OverrideOptionKeyPath xor Overrided) then
    Result := nil
  else
    Result := StringToLPOLESTR(OptionKeyPath);
end;

constructor TCustomEmbeddedWB.Create(AOwner: TComponent);
begin
  inherited;
  FCanGrabFocus := True;
  FScriptErrorAction := eaContinue;
  DownloadOptions := [DownloadImages, DownloadVideos, DownloadBGSounds];
  UserInterfaceOptions := [EnableThemes, EnablesFormsAutoComplete];
  FDropHandlingType := ddtMS;
  FZoomPercent := 100;
  FDisableCtrlShortcuts := 'N';
end;

destructor TCustomEmbeddedWB.Destroy();
begin
  if (CurrentHandle <> 0) and IsWindow(CurrentHandle) then
  begin
    // Allow inherited Destroy to destroy the window
    WindowHandle := CurrentHandle;
    CurrentHandle := 0;
  end;
  inherited Destroy;
end;

procedure TCustomEmbeddedWB.CreateWnd; //jls
begin
  if (CurrentHandle <> 0) and IsWindow(CurrentHandle) then
  begin
    WindowHandle := CurrentHandle;
    CurrentHandle := 0;
    Windows.SetParent(WindowHandle, TWinControl(Self).Parent.Handle);
    MoveWindow(WindowHandle, Left, Top, Width, Height, True); //Force a resize on the client window
  end
  else
    inherited CreateWnd;
end;

procedure TCustomEmbeddedWB.DestroyWnd; //jls
begin
  if (csDestroying in ComponentState) then
    inherited DestroyWnd
  else
  begin
    Windows.SetParent(WindowHandle, Forms.Application.Handle); //Parent to the Application window which is 0x0 in size
    CurrentHandle := WindowHandle; // Save the WindowHandle
    WindowHandle := 0; // Set it to 0 so Createwnd will be called again...
  end;
end;

function TCustomEmbeddedWB.EnableModeless(const fEnable: BOOL): HRESULT;
begin
  Result := S_OK;
  if Assigned(FOnEnableModeless) then
    FOnEnableModeless(Self, fEnable);
end;

function TCustomEmbeddedWB.EvaluateNewWindow(pszUrl, pszName, pszUrlContext,
  pszFeatures: LPCWSTR; fReplace: BOOL; dwFlags,
  dwUserActionTime: DWORD): HRESULT;
begin
  Result := E_FAIL;
  if Assigned(FOnEvaluateNewWindow) then
    FOnEvaluateNewWindow(Self, pszUrl, pszName, pszUrlContext, pszFeatures,
      FReplace, dwFlags, dwUserActionTime, Result);
end;

function TCustomEmbeddedWB.FilterDataObject(const pDO: IDataObject;
  out ppDORet: IDataObject): HRESULT;
begin
  ppDORet := nil;
  if Assigned(FOnFilterDataObject) then
    FOnFilterDataObject(Self, pDO, ppDORet);
  if ppDORet = nil then
    Result := S_FALSE
  else
    Result := S_OK;
end;

function TCustomEmbeddedWB.GetDoc2: IHtmlDocument2;
begin
  if not Supports(Document, IHtmlDocument2, Result) then
    Result := nil;
end;

function TCustomEmbeddedWB.GetDoc3: IHtmlDocument3;
begin
  if not Supports(Document, IHtmlDocument3, Result) then
    Result := nil;
end;

function TCustomEmbeddedWB.GetDoc4: IHtmlDocument4;
begin
  if not Supports(Document, IHtmlDocument4, Result) then
    Result := nil;
end;

function TCustomEmbeddedWB.GetDoc5: IHtmlDocument5;
begin
  if not Supports(Document, IHtmlDocument5, Result) then
    Result := nil;
end;

function TCustomEmbeddedWB.getBody: IHTMLElement;
var
  D: IHtmlDocument2;
begin
  if Supports(Document, IHtmlDocument2, D) then
    Result := D.body
  else
    Result := nil;
end;

function TCustomEmbeddedWB.GetExternal(out ppDispatch: IDispatch): HRESULT;
begin
  ppDispatch := nil;
  if Assigned(FOnGetExternal) then
    FOnGetExternal(Self, ppDispatch);
  if ppDispatch = nil then
    Result := S_FALSE
  else
    Result := S_OK;
end;

function TCustomEmbeddedWB.GetHostInfo(var pInfo: TDOCHOSTUIINFO): HRESULT;
begin
  FillChar(pInfo, SizeOf(TDOCHOSTUIINFO), #0);
  pInfo.cbSize := SizeOf(pInfo);
  pInfo.dwFlags := FUserInterfaceValue;
  pInfo.dwDoubleClick := DOCHOSTUIDBLCLK_DEFAULT;
  Result := S_OK;

  if Assigned(FOnGetHostInfo) then
    FOnGetHostInfo(Self, pInfo);
end;

function TCustomEmbeddedWB.GetIDsOfNames(const IID: TGUID; Names: Pointer; NameCount,
  LocaleID: Integer; DispIDs: Pointer): HRESULT;
begin
  Result := inherited GetIDsOfNames(IID, Names, NameCount, LocaleID, DispIDs);
  if Assigned(FOnGetIDsOfNames) then
    FOnGetIDsOfNames(Self, IID, Names, NameCount, LocaleID, DispIds, Result);
end;

function TCustomEmbeddedWB.GetIEWin(const ClassName: string): HWND;
var
  szClass: array[0..255] of char;
begin
  if HandleAllocated then
  begin
    Result := GetWindow(WindowHandle, GW_CHILD);
    repeat
      if (GetClassName(Result, szClass, SizeOf(szClass)) > 0) and
        (AnsiStrComp(PChar(ClassName), szClass) = 0) then
        Exit;
      Result := GetWindow(Result, GW_CHILD);
    until not IsWindow(Result);
  end;
  Result := 0;
end;

function TCustomEmbeddedWB.GetOptionKeyPath(out pchKey: POleStr;
  const dw: DWORD): HRESULT;
begin
  pchKey := CopyOptionKeyPath(False);
{$IFDEF GETKEYPATH_HANDLERS}
  if Assigned(FOnGetOptionKeyPath) then
    FOnGetOptionKeyPath(Self, pchKey);
{$ENDIF}
  if pchKey = nil then
    Result := S_FALSE
  else
    Result := S_OK;
end;

function TCustomEmbeddedWB.GetOverrideKeyPath(out pchKey: POleStr; dw: DWORD): HRESULT;
begin
  pchKey := CopyOptionKeyPath(True);
{$IFDEF GETKEYPATH_HANDLERS}
  if Assigned(FOnGetOverrideKeyPath) then
    FOnGetOverrideKeyPath(Self, pchKey);
{$ENDIF}
  if pchKey = nil then
    Result := S_FALSE
  else
    Result := S_OK;
end;

function TCustomEmbeddedWB.GetTypeInfo(Index, LocaleID: Integer; out TypeInfo): HRESULT;
begin
  Result := inherited GetTypeInfo(Index, LocaleID, TypeInfo);
  if Assigned(FOnGetTypeInfo) then
    FOnGetTypeInfo(Self, Index, LocaleID, ITypeInfo(TypeInfo), Result);
end;

function TCustomEmbeddedWB.GetTypeInfoCount(out Count: Integer): HRESULT;
begin
  Result := inherited GetTypeInfoCount(Count);
  if Assigned(FOnGetTypeInfoCount) then
    FOnGetTypeInfoCount(Self, Count, Result);
end;

function TCustomEmbeddedWB.GetZoom: Integer;
var
  vaIn, vaOut: OleVariant;
begin
  vaIn := NULL;
  InvokeCommand(nil, OLECMDID_ZOOM, OLECMDEXECOPT_DONTPROMPTUSER, vaIn, vaOut);
  Result := vaOut;
end;

procedure TCustomEmbeddedWB.SetZoom(const Value: Integer);
var
  vaIn, vaOut: OleVariant;
  Range: DWORD;
begin
  InvokeCommand(nil, OLECMDID_GETZOOMRANGE, OLECMDEXECOPT_DONTPROMPTUSER, vaIn,
    vaOut);
  Range := DWORD(vaOut);
  if Value < LoWord(Range) then
    vaIn := LoWord(Range)
  else if Value > HiWord(Range) then
    vaIn := HiWord(Range)
  else
    vaIn := Value;
  InvokeCommand(nil, OLECMDID_ZOOM, OLECMDEXECOPT_DONTPROMPTUSER, vaIn, vaOut);
end;

procedure TCustomEmbeddedWB.SetOpticalZoom(const Value: Integer);
var
  vaIn, vaOut: OleVariant;
  Range: DWORD;
begin
  if FZoomPercent <> Value then
  begin
    FZoomPercent := Value;
    InvokeCommand(nil, OLECMDID_OPTICAL_GETZOOMRANGE, OLECMDEXECOPT_DONTPROMPTUSER, vaIn, vaOut);
    Range := DWORD(vaOut);
    if Value < LoWord(Range) then
      vaIn := LoWord(Range)
    else if Value > HiWord(Range) then
      vaIn := HiWord(Range)
    else
      vaIn := Value;
    InvokeCommand(nil, OLECMDID_OPTICAL_ZOOM, OLECMDEXECOPT_DONTPROMPTUSER, vaIn, vaOut);
    if Assigned(FOnZoomPercentChanged) then
      FOnZoomPercentChanged(Self, vaOut);
  end;
end;

function TCustomEmbeddedWB.HideUI: HRESULT;
begin
  Result := S_FALSE;
  if Assigned(FOnHideUI) then
    FOnHideUI(Self, Result);
end;

function TCustomEmbeddedWB.InvokeCommand(CmdGroup: PGUID; Cmd, nCmdexecopt: DWORD;
  var vaIn, vaOut: OleVariant): HRESULT;
var
  CmdTarget: IOleCommandTarget;
begin
  if Supports(Document, IOleCommandTarget, CmdTarget) then
    Result := CmdTarget.Exec(CmdGroup, Cmd, nCmdexecopt, vaIn, vaOut)
  else
    Result := E_UNEXPECTED;
end;

function TCustomEmbeddedWB.InvokeCommand(CmdGroup: PGUID; Cmd: DWORD): HRESULT;
var
  CmdTarget: IOleCommandTarget;
  vaIn, vaOut: Olevariant;
begin
  if Supports(Document, IOleCommandTarget, CmdTarget) then
    Result := CmdTarget.Exec(CmdGroup, Cmd, OLECMDEXECOPT_DODEFAULT, vaIn, vaOut)
  else
    Result := E_UNEXPECTED;
end;

function TCustomEmbeddedWB.QueryCMDArrayStatus(CmdGroup: PGUID;
  cmds: TOleCmdArray): Boolean;
var
  CmdTarget: IOleCommandTarget;
begin
  if Supports(Document, IOleCommandTarget, CmdTarget) then
    Result := CmdTarget.QueryStatus(CmdGroup,
      Length(cmds), @Cmds, nil) = S_OK
  else
    Result := False;
end;

function TCustomEmbeddedWB.QueryCMDEnabled(CmdGroup: PGUID; cmdID: Cardinal): Boolean;
begin
  Result := (QueryCMDStatus(CmdGroup, cmdID) and OLECMDF_ENABLED) <> 0;
end;

function TCustomEmbeddedWB.QueryCMDLatched(CmdGroup: PGUID; cmdID: Cardinal): Boolean;
begin
  Result := (QueryCMDStatus(CmdGroup, cmdID) and OLECMDF_LATCHED) <> 0;
end;

function TCustomEmbeddedWB.QueryCMDStatus(CmdGroup: PGUID; cmdID: Cardinal): OLECMDF;
var
  CmdTarget: IOleCommandTarget;
  Cmd: TOleCmd;
begin
  Result := 0;
  if Supports(Document, IOleCommandTarget, CmdTarget) then
  begin
    Cmd.CmdID := cmdID;
    Cmd.cmdf := 0;
    if CmdTarget.QueryStatus(CmdGroup, 1, @Cmd, nil) = S_OK then
      Result := Cmd.cmdf;
  end;
end;

function TCustomEmbeddedWB.QueryCommandStatus(CmdGroup: PGUID; cCmds: Cardinal; prgCmds:
  POleCmd;
  CmdText: POleCmdText): HRESULT;
var
  CmdTarget: IOleCommandTarget;
begin
  if Supports(Document, IOleCommandTarget, CmdTarget) then
    Result := CmdTarget.QueryStatus(CmdGroup, cCmds, prgCmds, CmdText)
  else
    Result := E_UNEXPECTED;
end;

{$IFDEF RESEARCH_MODE}

function TCustomEmbeddedWB.QueryInterface(const IID: TGUID; out Obj): HRESULT;
begin
  Result := inherited QueryInterface(IID, Obj);
  if Assigned(OnQueryInterface) then
    OnQueryInterface(Self, IID, Obj, Result);
end;
{$ENDIF}

function TCustomEmbeddedWB.Invoke(DispID: Integer; const IID: TGUID; LocaleID: Integer;
  Flags: Word; var Params; VarResult, ExcepInfo, ArgErr: Pointer): HRESULT;
var
  UserAgent: string;
begin
  try
    Result := S_FALSE;
    if (Flags and DISPATCH_PROPERTYGET <> 0) and (VarResult <> nil) then
    begin
      Result := S_OK;
      case DispID of
        DISPID_AMBIENT_DLCONTROL:
          begin
            PVariant(VarResult)^ := FDownloadOptionValue;
          end;
        DISPID_AMBIENT_USERMODE:
          begin
            POleVariant(VarResult)^ := not DesignMode;
          end;
        DISPID_AMBIENT_USERAGENT:
          begin
            Result := S_FALSE;
            if Assigned(FOnSetUserAgent) then
            begin
              if FOnSetUserAgent(UserAgent) = S_OK then
                if UserAgent <> '' then
                begin
                  POleVariant(VarResult)^ := UserAgent + #13#10;
                  Result := S_OK;
                end;
            end;
          end;
      else
        Result := S_FALSE;
      end;
    end

    else if (Flags and DISPATCH_PROPERTYPUT <> 0) and
      (DispID = DISPID_AMBIENT_USERMODE) then
    begin
      Result := S_OK;
      Self.FDesignMode := POleVariant(TDispParams(Params).rgvarg)^;
    end;

    if Result = S_FALSE then
    begin
      Result := inherited Invoke(DispID, IID, LocaleID, Flags, Params,
        VarResult, ExcepInfo, ArgErr);
      if (result = DISP_E_MEMBERNOTFOUND) and Assigned(FOnInvoke) then
        FOnInvoke(Self, DispID, IID, LocaleID, Flags, TagDispParams(Params),
          VarResult, ExcepInfo, ArgErr, Result);
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

function TCustomEmbeddedWB.OnDocWindowActivate(const fActivate: BOOL): HRESULT;
begin
  if Assigned(FOnOnDocWindowActivate) then
    FOnOnDocWindowActivate(Self, FActivate);
  Result := S_OK;
end;

function TCustomEmbeddedWB.OnFrameWindowActivate(const fActivate: BOOL): HRESULT;
begin
  if Assigned(FOnOnFrameWindowActivate) then
    FOnOnFrameWindowActivate(Self, fActivate);
  Result := S_OK;
end;

function TCustomEmbeddedWB.DoQueryService(const rsid, iid: TGUID; var Obj): Boolean;
begin
  if (IsEqualGuid(rsid, IID_INewWindowManager) and Assigned(FOnEvaluateNewWindow))
    or IsEqualGuid(rsid, IID_IProtectFocus)
    or (IsEqualGuid(rsid, IID_IDownloadManager) and Assigned(FOnDownload))
    or (IsEqualGuid(rsid, IID_IHostBehaviorInit) and Assigned(OnPopulateNSTable))
    or (IsEqualGuid(rsid, IID_IHTMLOMWindowServices) and
    (FloatingHosting or Assigned(OnMove) or Assigned(Self.OnMoveBy)
    or Assigned(OnResize) or Assigned(OnResizeBy)))
    or (IsEqualGUID(iid, IID_IAuthenticate) and Assigned(OnAuthenticate)) then
    Result := QueryInterface(iid, Obj) = S_OK
  else
    Result := False;
end;

function TCustomEmbeddedWB.QueryService(const rsid, iid: TGUID; out Obj): HRESULT;
begin
  Pointer(Obj) := nil;
  if (not DoQueryService(rsid, iid, Obj)) and Assigned(FOnQueryService) then
    FOnQueryService(Self, rsid, iid, IUnknown(obj));

  if Pointer(Obj) <> nil then
    Result := S_OK
  else
    Result := E_NOINTERFACE;
end;

function TCustomEmbeddedWB.ResizeBorder(const prcBorder: PRECT;
  const pUIWindow: IOleInPlaceUIWindow; const FrameWindow: BOOL): HRESULT;
begin
  if Assigned(FOnResizeBorder) then
    FOnResizeBorder(Self, prcBorder, pUIWindow, fRameWindow);
  Result := S_OK;
end;

procedure TCustomEmbeddedWB.MoveParentForm(x, y: Integer; Delta: Boolean);
var
  F: TCustomForm;
begin
  F := GetParentForm(Self);
  if F <> nil then
  begin
    if Delta then
    begin
      x := F.Left + x;
      y := F.Top + y;
    end; //FIXME defend from moving outside of screen  (don't forget multimonitor)
    F.SetBounds(x, y, F.Width, F.Height);
  end;
end;

procedure TCustomEmbeddedWB.ResizeParentForm(w, h: Integer; Delta: Boolean);
var
  F: TCustomForm;
begin
  F := GetParentForm(Self);
  if F <> nil then
  begin
    if Delta then
    begin
      w := F.Width + w;
      h := F.Height + h;
    end;
    F.SetBounds(F.Left, F.Top, w, h);
  end;
end;

function TCustomEmbeddedWB.ResizeBy(const x, y: Integer): HRESULT;
begin
  if FloatingHosting then
    ResizeParentForm(x, y, True);
  if Assigned(OnResizeBy) then
    OnResizeBy(Self, x, y);
  Result := S_OK; // always return success to prevent script error messages
end;

function TCustomEmbeddedWB.ResizeTo(const x, y: Integer): HRESULT;
begin
  if FloatingHosting then
    ResizeParentForm(x, y, False);
  if Assigned(OnResize) then
    OnResize(self, x, y);
  Result := S_OK; // always return success to prevent script error messages
end;

function TCustomEmbeddedWB.MoveBy(const x, y: Integer): HRESULT;
begin
  if FloatingHosting then
    MoveParentForm(x, y, True);
  if Assigned(OnMoveBy) then
    OnMoveBy(self, x, y);
  Result := S_OK; // always return success to prevent script error messages
end;

function TCustomEmbeddedWB.MoveTo(const x, y: Integer): HRESULT;
begin
  if FloatingHosting then
    MoveParentForm(x, y, False);
  if Assigned(OnMove) then
    OnMove(self, x, y);
  Result := S_OK; // always return success to prevent script error messages
end;

function TCustomEmbeddedWB.OnZoomPercentChanged(const ulZoomPercent: uLong): HRESULT;
begin
  if Assigned(FOnZoomPercentChanged) then
    Result := FOnZoomPercentChanged(Self, ulZoomPercent)
  else
    Result := S_FALSE;
end;

function TCustomEmbeddedWB.GetElemByID(const ID: WideString): IHTMLElement;
var
  Doc3: IHTMLDocument3;
begin
  if Supports(Document, IHTMLDocument3, Doc3) then
    Result := Doc3.getElementById(ID)
  else
    Result := nil;
end;

function TCustomEmbeddedWB.ScrollToElement(Element: IHTMLElement): Boolean;
var
  RV: IHTMLRect;
begin
  Result := Element <> nil;
  if Result then
  begin
    RV := (Element as IHTMLElement2).getBoundingClientRect;
    Doc2.parentWindow.scrollBy(RV.left, RV.top);
  end;
end;

function TCustomEmbeddedWB.GetCharSet: WideString;
begin
  Result := Doc2.charset;
end;

procedure TCustomEmbeddedWB.SetCharSet(const Value: WideString);
var
  Level: OleVariant;
begin
  Doc2.charset := Value;
  Level := 7;
  DefaultInterface.Refresh2(Level);
end;

procedure TCustomEmbeddedWB.SetDesignMode(const Value: Boolean);
var
  Control: IOleControl;
begin
  FDesignMode := Value;
  if DefaultInterface.QueryInterface(IOleControl, Control) = 0 then
    with (Application as IOleControl) do
    begin
      OnAmbientPropertyChange(DISPID_AMBIENT_USERMODE);
      //_Release; I4214   // I4214
    end;
end;

const
  _DesignModeValues: array[TDocDesignMode] of string =
    ('On', 'Off', 'Inherit', '');

function TCustomEmbeddedWB.GetDocDesignMode: TDocDesignMode;
var
  D: IHTMLDocument2;
  I: Integer;
begin
  Result := ddmUnknown;
  if Supports(Document, IHTMLDocument2, D) then
  begin
    I := AnsiIndexStr(D.designMode, _DesignModeValues);
    if I in [0..2] then
      Result := TDocDesignMode(I);
  end;
end;

procedure TCustomEmbeddedWB.SetDocDesignMode(const Value: TDocDesignMode);
var
  D: IHTMLDocument2;
begin
  if (Value <> ddmUnknown) and Supports(Document, IHTMLDocument2, D) then
    D.designMode := _DesignModeValues[Value];
end;

procedure TCustomEmbeddedWB.SetDownloadOptions(const Value: TDownloadControlOptions);
begin
  FDownloadControlOptions := Value;
  UpdateDownloadControlValues;
  with (Application as IOleControl) do
  begin
    OnAmbientPropertyChange(DISPID_AMBIENT_DLCONTROL);
    //_Release; I4214   // I4214
  end;
end;

procedure TCustomEmbeddedWB.SetFocusToBody;
var
  bodyElement: IHTMLElement2;
  HTMLDoc2: IHTMLDocument2;
begin
  HTMLDoc2 := GetDoc2;
  if Assigned(HTMLDoc2) then
  begin
    bodyElement := HTMLDoc2.body as IHTMLElement2;
    if Assigned(bodyElement) then
      bodyElement.focus;
  end;
end;

procedure TCustomEmbeddedWB.SetFocusToDoc;
var
  bCanGrabFocus: Boolean;
  ParentForm: TCustomForm;
begin
  if Document <> nil then
  begin
    bCanGrabFocus := CanGrabFocus;
    CanGrabFocus := True;
    with (Application as IOleObject) do
    begin
      if DoVerb(OLEIVERB_UIACTIVATE, nil, Self, 0, Handle, GetClientRect) = S_OK then
      begin
        ParentForm := GetParentForm(Self);
        if Assigned(ParentForm) and Self.CanFocus then
          ParentForm.ActiveControl := Self;
      end;
    end;
    CanGrabFocus := bCanGrabFocus;
  end;
end;

procedure TCustomEmbeddedWB.SetFocusToParent;
begin
  {if IsWindow(WindowHandle) then
   begin
     Windows.SetParent(WindowHandle, Parent.Handle);
     MoveWindow(WindowHandle, 0, 0, Parent.Width, Parent.Height, True);
     Parent.SetFocus;
   end;}
  if IsWindow(WindowHandle) then
  begin
    Windows.SetParent(WindowHandle, TWinControl(Self).Parent.Handle);
    MoveWindow(WindowHandle, 0, 0, TWinControl(Self).Parent.Width,
      TWinControl(Self).Parent.Height, True);
    TWinControl(Self).Parent.SetFocus;
  end;
end;

procedure TCustomEmbeddedWB.SetUserInterfaceOptions(const Value: TUserInterfaceOptions);
begin
  FUserInterfaceOptions := Value;
  UpdateUserInterfaceValues;
  with (Application as IOleControl) do
  begin
    OnAmbientPropertyChange(DISPID_AMBIENT_DLCONTROL);
    //_Release; I4214   // I4214
  end;
end;

procedure TCustomEmbeddedWB.SetDropHandlingType(const Value: TDragDropHandlingType);
var
  innerWnd: LongWord;
  Impl: IDropTarget;
begin
  if FDropHandlingType <> Value then
  begin
    FDropHandlingType := Value;

    if HandleAllocated then
      innerWnd := GetIEWin('Internet Explorer_Server')
    else
      innerWnd := 0;
    if innerWnd <> 0 then
      RevokeDragDrop(innerWnd);

    Impl := nil;
    case Value of
      ddtMS:
        DefaultInterface.RegisterAsDropTarget := True;
      ddtMy: Impl := Self;
      ddtCustom:
        if innerWnd <> 0 then
        begin
          Impl := Self;
          if Assigned(FOnGetDropTarget) then
            FOnGetDropTarget(Self, Impl);
        end;
      ddtNo:
        DefaultInterface.RegisterAsDropTarget := False;
    end;
    if (innerWnd <> 0) and (Impl <> nil) then
      RegisterDragDrop(innerWnd, Impl);
  end;
end;

function TCustomEmbeddedWB.GetDropTarget(const pDropTarget: IDropTarget;
  out ppDropTarget: IDropTarget): HRESULT;
begin
  Result := S_OK;
  case DropHandlingType of
    ddtMS:
      begin
        DefaultInterface.RegisterAsDropTarget := True;
        Result := E_NOTIMPL;
      end;
    ddtMy:
      ppDropTarget := Self;
    ddtCustom:
      begin
        ppDropTarget := Self;
        if Assigned(FOnGetDropTarget) then
          FOnGetDropTarget(Self, ppDropTarget);
      end;
    ddtNo:
      begin
        DefaultInterface.RegisterAsDropTarget := False;
        ppDropTarget := nil;
      end;
  end;
end;

function TCustomEmbeddedWB.DragEnter(const dataObj: IDataObject; grfKeyState: Integer;
  pt: TPoint; var dwEffect: Integer): HRESULT;
begin
  Result := NOERROR;
  dwEffect := DROPEFFECT_NONE;
  if Assigned(OnDragEnter) then
    OnDragEnter(Self, dataObj, grfKeyState, pt, dwEffect, Result);
end;

function TCustomEmbeddedWB.DragLeave: HRESULT;
begin
  Result := NOERROR;
  if Assigned(OnDragLeave) then
    OnDragLeave(Self);
end;

function TCustomEmbeddedWB.Drop(const dataObj: IDataObject; grfKeyState: Integer;
  pt: TPoint; var dwEffect: Integer): HRESULT;
begin
  Result := NOERROR;
  dwEffect := DROPEFFECT_NONE;
  if Assigned(FOnDropEvent) then
    FOnDropEvent(Self, dataObj, grfKeyState, pt, dwEffect, Result);
end;

function TCustomEmbeddedWB.DropTargetDragOver(grfKeyState: Integer; pt: TPoint;
  var dwEffect: Integer): HRESULT;
begin
  Result := NOERROR;
  dwEffect := DROPEFFECT_NONE;
  if Assigned(FOnDragOverEvent) then
    FOnDragOverEvent(Self, grfKeyState, pt, dwEffect, Result);
end;

function TCustomEmbeddedWB.Download(pmk: IMoniker; pbc: IBindCtx; dwBindVerb,
  grfBINDF: DWORD; pBindInfo: PBindInfo; pszHeaders, pszRedir: PWidechar;
  uiCP: UINT): HRESULT;
begin
  Result := E_NOTIMPL;
  if Assigned(FOnDownload) then
    FOnDownload(Self, pmk, pbc, dwBindVerb, grfBINDF, pBindInfo, pszHeaders,
      pszRedir, uiCP, Result);
end;

function TCustomEmbeddedWB.FilterPopupMenu: Boolean;
begin
  Result := Assigned(OnFilterPopupMenu);
end;

procedure TCustomEmbeddedWB.DoFilterPopupMenu(Sender: TObject; ID: DWORD; Menu: HMENU; const Context:
  IDispatch);
begin
  if Assigned(OnFilterPopupMenu) then
    OnFilterPopupMenu(Sender, ID, Menu, Context);
end;

function TCustomEmbeddedWB.ShowContextMenu(const dwID: DWORD; const ppt: PPOINT;
  const CommandTarget: IUnknown; const Context: IDispatch): HRESULT;
var
  EncodingSubMenu: OleVariant;
begin
  Result := E_NOTIMPL;
  if Assigned(FOnShowContextMenu) then
    FOnShowContextMenu(Self, dwID, ppt, CommandTarget, Context, Result);

  if Result = E_NOTIMPL then
  begin
    if IsSeTIEPopupMenus(dwID, DisabledPopupMenus) then
    begin
      Result := S_OK;
      if Assigned(PopUpMenu) then // Show assigned TPopupMenu
        PopUpMenu.Popup(Mouse.CursorPos.X, Mouse.CursorPos.Y);
    end
    else if FilterPopupMenu then
    begin
      ExecWB(CGetMimeSubMenuCommandID, OLECMDEXECOPT_DODEFAULT, EncodingSubMenu);
      if ShowRightClickMenu(Self, dwID, CommandTarget, Context, ppt, EncodingSubMenu,
        DoFilterPopupMenu) then
        Result := S_OK
      else
        Result := S_FALSE;
    end
    else
      Result := S_FALSE;
  end;
end;

function TCustomEmbeddedWB.ShowHelp(HWND: THandle; pszHelpFile: POleStr; uCommand,
  dwData: Integer; ptMouse: TPoint; var pDispatchObjectHit: IDispatch): HRESULT;
begin
  if Assigned(FOnShowHelp) then
    Result := FOnShowHelp(Self, HWND, pszHelpFile, uCommand, dwData, ptMouse, pDispatchObjectHit)
  else if (pszHelpFile = nil) and (HelpFile <> '') then
  begin
    HtmlHelp(HWND, PChar(HelpFile), uCommand, dwData);
    Result := S_OK;
  end
  else
    Result := S_FALSE;
end;

function TCustomEmbeddedWB.ShowMessage(HWND: THandle; lpstrText, lpstrCaption: POleStr;
  dwType: Integer; lpstrHelpFile: POleStr; dwHelpContext: Integer;
  var plResult: LRESULT): HRESULT;
begin
  if Assigned(FOnShowMessage) then
    Result := FOnShowMessage(Self, HWND, lpstrText, lpStrCaption, dwType, lpStrHelpFile, dwHelpContext, plResult)
  else
    Result := S_FALSE;
end;

function TCustomEmbeddedWB.ShowUI(const dwID: DWORD;
  const pActiveObject: IOleInPlaceActiveObject;
  const pCommandTarget: IOleCommandTarget; const pFrame: IOleInPlaceFrame;
  const pDoc: IOleInPlaceUIWindow): HRESULT;
begin
  Result := S_FALSE;
  if Assigned(FOnShowUI) then
    FOnShowUI(Self, dwID, pActiveObject, pCommandTarget, pFrame, pDoc, Result);
end;

function TCustomEmbeddedWB.DoFilterMsg(const lpMsg: PMSG): Boolean;
type
  PWMKey = ^TWMKey;
var
  ShiftState: TShiftState;
begin
  {
   Result := (FDisableCtrlShortcuts <> '') and (lpMsg^.message = WM_KEYDOWN)
     and (((GetKeyState(VK_LCONTROL) < 0) and (GetKeyState(VK_MENU) >= 0)) or
     ((GetKeyState(VK_RCONTROL) < 0) and (GetKeyState(VK_LMENU) >= 0)))
     and (_CharPos(Char(lpMsg.wParam), FDisableCtrlShortcuts) > 0);      }

  ShiftState := KeyDataToShiftState(PWMKey(lpMsg)^.KeyData);
  Result := (FDisableCtrlShortcuts <> '') and (lpMsg^.message = WM_KEYDOWN)
    and ((ShiftState = [ssCtrl]) and (ShiftState <> [ssAlt]))
    and (_CharPos(Char(lpMsg.wParam), FDisableCtrlShortcuts) > 0);

  if Result and Assigned(OnMaskedCtrlChar) then
    OnMaskedCtrlChar(Self, Char(lpMsg.wParam));
end;

function TCustomEmbeddedWB.TranslateAccelerator(const lpMsg: PMSG;
  const pguidCmdGroup: PGUID; const nCmdID: DWORD): HRESULT;
{ Called by MSHTML when IOleInPlaceActiveObject.TranslateAccelerator or
   IOleControlSite.TranslateAccelerator is called }
var
  Filtered: Boolean;
begin
  Filtered := DoFilterMsg(lpMsg);
  if (not Filtered) and Assigned(FOnTranslateAccelerator) then
    FOnTranslateAccelerator(Self, lpMsg, pguidCmdGroup, nCmdID, Filtered);

  if Filtered then
    Result := S_OK
  else
    Result := S_FALSE;
end;

function TCustomEmbeddedWB.TranslateUrl(const dwTranslate: DWORD; const pchURLIn:
  POleStr; out ppchURLOut: POleStr): HRESULT;
var
  URLOut: WideString;
begin
  URLOut := '';
  if Assigned(FOnTranslateUrl) then
    FOnTranslateUrl(Self, pchUrlIn, URLOut);
  if URLOut <> '' then
  begin
    Result := S_OK;
    ppchURLOut := WideStringToLPOLESTR(URLOut);
  end
  else
    Result := S_FALSE;
end;

function TCustomEmbeddedWB.UpdateUI: HRESULT;
begin
  Result := S_FALSE;
  if Assigned(FOnUpdateUI) then
    FOnUpdateUI(Self, Result);
end;

procedure TCustomEmbeddedWB.UpdateUserInterfaceValues;
const
  acardUserInterfaceValues: array[TUserInterfaceOption] of Cardinal =
    ($00000001, $00000002, $00000004, $00000008,
    $00000010, $00000020, $00000040, $00000080,
    $00000100, $00000200, $00000400, $00000800,
    $00001000, $00002000, $00004000, $00010000, $00020000,
    $00040000, $00080000, $00100000, $00200000, $00400000,
    $00800000, $01000000, $02000000, $04000000, $08000000,
    $10000000, $20000000);
var
  uio: TUserInterfaceOption;
begin
  FUserInterfaceValue := 0;
  if (FUserInterfaceOptions <> []) then
    for uio := Low(TUserInterfaceOption) to High(TUserInterfaceOption) do
      if (uio in FUserInterfaceOptions) then
        Inc(FUserInterfaceValue, acardUserInterfaceValues[uio]);
end;

procedure TCustomEmbeddedWB.UpdateDownloadControlValues;
const
  acardDownloadControlValues: array[TDownloadControlOption] of Cardinal =
    ($00000010, $00000020, $00000040, $00000080,
    $00000100, $00000200, $00000400, $00000800,
    $00001000, $00002000, $00004000, $00008000,
    $00010000, $00020000, $00040000, $10000000,
    $20000000, $40000000, $80000000);
var
  dco: TDownloadControlOption;
begin
  FDownloadOptionValue := 0;
  if (FDownloadControlOptions <> []) then
    for dco := Low(TDownloadControlOption) to High(TDownloadControlOption) do
      if (dco in FDownloadControlOptions) then
        Inc(FDownloadOptionValue, acardDownloadControlValues[dco]);
end;

function TCustomEmbeddedWB.ZoomRangeHigh: Integer;
var
  vaIn, vaOut: OleVariant;
begin
  InvokeCommand(nil, OLECMDID_GETZOOMRANGE, OLECMDEXECOPT_DONTPROMPTUSER, vaIn, vaOut);
  Result := HiWord(DWORD(vaOut));
end;

function TCustomEmbeddedWB.ZoomRangeLow: Integer;
var
  vaIn, vaOut: OleVariant;
begin
  InvokeCommand(nil, OLECMDID_GETZOOMRANGE, OLECMDEXECOPT_DONTPROMPTUSER, vaIn, vaOut);
  Result := LoWord(DWORD(vaOut));
end;

function TCustomEmbeddedWB._getCookie: WideString;
var
  D: IHTMLDocument2;
begin
  if Supports(Document, IHTMLDocument2, D) then
    Result := OleObject.Document.Cookie
  else
    Result := '';
end;

procedure TCustomEmbeddedWB.Client2HostWin(var CX, CY: Integer);
var
  F: TCustomForm;
begin
  F := GetParentForm(Self);
  if F <> nil then
  begin
    Inc(CX, F.ClientWidth - Self.Width);
    Inc(CY, F.ClientHeight - Self.Height);
  end;
end;

{$IFDEF USE_IOLECOMMANDTARGET}
//======IOleCommandTarget interface ============================================

function TCustomEmbeddedWB.CommandTarget_QueryStatus(CmdGroup: PGUID; cCmds: Cardinal;
  prgCmds: POleCmd; CmdText: POleCmdText): HRESULT;
begin
  prgCmds.cmdf := OLECMDF_ENABLED;
  Result := S_OK;
end;

function TCustomEmbeddedWB.CommandTarget_Exec(CmdGroup: PGUID; nCmdID, nCmdexecopt: DWORD;
  const vaIn: OleVariant; var vaOut: OleVariant): HRESULT;
var
  tmpCancel: Boolean;
const
{$J+}
  LastTickEvent: Cardinal = 0;
{$J-}
begin
  Result := OLECMDERR_E_NOTSUPPORTED;

  if (nCmdID = OLECMDID_STOP) then
  begin
    if Assigned(FOnStop) then
      FOnStop(Self);
  end;

  if CmdGroup <> nil then
  begin
    if IsEqualGuid(cmdGroup^, CGID_EXPLORER) then
    begin
      case nCmdID of
        OLECMDID_ONUNLOAD:
          if Assigned(FOnUnload) then
          begin
            FOnUnload(Self);
            Result := S_OK;
            Exit;
          end;

        OLECMDID_PREREFRESH:
          begin
            if Assigned(FOnPreRefresh) then
            begin
              if GetTickCount - LastTickEvent > 150 then
              begin
                LastTickEvent := GetTickCount;
                FOnPreRefresh(Self);
              end;
            end;

            if Assigned(FOnHookChildWindow) then
              if (GetIEWin('Internet Explorer_Server') <> 0) or (GetIEWin('SysListView32') <> 0) then
                FOnHookChildWindow(Self);
          end;
      end
    end
    else if IsEqualGuid(cmdGroup^, CGID_DocHostCommandHandler) then
    begin
      case nCmdID of
        ID_IE_F5_REFRESH {nCmdID 6041, F5},
        ID_IE_CONTEXTMENU_REFRESH {nCmdID 6042, Refresh by ContextMenu},
        IDM_REFRESH {nCmdID 2300}:
          begin
            if Assigned(FOnRefresh) then
            begin
              tmpCancel := False;
              FOnRefresh(Self, nCmdID, tmpCancel);
              if tmpCancel then
                Result := S_OK; //FIXME is it true? Why not OLECMDERR_E_CANCELED
            end;
            Exit;
          end;
        OLECMDID_SHOWSCRIPTERROR:
          begin
            Result := ScriptErrorHandler(vaIn, vaOut);
            Exit;
          end;
      end;
    end;
  end;
  if Assigned(OnCommandExec) then
    Self.OnCommandExec(Self, CmdGroup, nCmdID, nCmdexecopt,
      vaIn, vaOut, Result);
end;
{$ENDIF}

function TCustomEmbeddedWB.ScriptErrorHandler(const vaIn: OleVariant;
  var vaOut: OleVariant): HRESULT;
var
  EventObject: IHTMLEventObj;
  CurWindow: IHTMLWindow2;
  CurDocument: IHTMLDocument2;
  CurUnknown: IUnknown;

  function GetProperty(const PropName: WideString): OleVariant;
  var
    DispParams: TDispParams;
    Disp, Status: Integer;
    ExcepInfo: TExcepInfo;
    PPropName: PWideChar;
  begin
    DispParams.rgvarg := nil;
    DispParams.rgdispidNamedArgs := nil;
    DispParams.cArgs := 0;
    DispParams.cNamedArgs := 0;
    PPropName := PWideChar(PropName);
    Status := EventObject.GetIDsOfNames(GUID_NULL, @PPropName, 1, LOCALE_SYSTEM_DEFAULT, @Disp);
    if Status = 0 then
    begin
      Status := EventObject.Invoke(disp, GUID_NULL, LOCALE_SYSTEM_DEFAULT,
        DISPATCH_PROPERTYGET, DispParams, @Result, @ExcepInfo, nil);
      if Status <> 0 then
        DispatchInvokeError(Status, ExcepInfo);
    end
    else if Status = DISP_E_UNKNOWNNAME then
      raise
        EOleError.CreateFmt('''%s'' is not supported.', [PropName])
    else
      OleCheck(Status);
  end;
begin
  Result := S_OK;
  case FScriptErrorAction of
    eaAskUser: Result := S_FALSE; //E_FAIL;
    eaContinue: vaOut := True;
    eaCancel: vaOut := False;
  end;

  if Assigned(FOnScriptError) then
  begin
    CurUnknown := IUnknown(TVarData(vaIn).VUnknown);
    if Succeeded(CurUnknown.QueryInterface(IID_IHTMLDocument2, CurDocument)) then
    begin
      CurWindow := CurDocument.Get_parentWindow;
      CurDocument := nil;
      if Assigned(CurWindow) then
      begin
        EventObject := CurWindow.Get_event;
        if EventObject <> nil then
        begin
          FOnScriptError(Self,
            GetProperty('errorline'),
            GetProperty('errorCharacter'),
            GetProperty('errorCode'),
            GetProperty('errorMessage'),
            GetProperty('errorUrl'),
            FScriptErrorAction);
        end;
      end;
    end;
  end;
end;

function TCustomEmbeddedWB.PopulateNamespaceTable: HRESULT;
begin
  Result := S_OK;
  if Assigned(fOnPopulateNSTable) then
    FOnPopulateNSTable(Self);
end;

function TCustomEmbeddedWB.GetElementNamespaceTable(
  out aTable: IElementNamespaceTable): Boolean;
var
  SP: IServiceProvider;
begin
  Result := Supports(Self.Document, IServiceProvider, SP) and
    (SP.QueryService(IID_IElementNamespaceTable, IID_IElementNamespaceTable,
    aTable) = S_OK);
end;

function WideStringToLPOLESTR(const Src: WideString): POLEStr;
begin
  Result := CoTaskMemAlloc((Length(Src) + 1) * SizeOf(WideChar));
  if Result <> nil then
    Move(PWideChar(Src)^, Result^, (Length(Src) + 1) * SizeOf(WideChar));
end;

function TCustomEmbeddedWB.Authenticate(var hwnd: HWnd; var szUserName,
  szPassWord: LPWSTR): HRESULT;
var
  aUser, aPwd: WideString;
begin
  Result := S_OK;
  hwnd := Self.Handle;
  aUser := '';
  aPwd := '';
  if Assigned(OnAuthenticate) then
    OnAuthenticate(Self, hwnd, aUser, aPwd, Result);
  if aUser <> '' then
    szUserName := WideStringToLPOLESTR(aUser)
  else
    szUserName := nil;
  if aPwd <> '' then
    szPassWord := WideStringToLPOLESTR(aPwd)
  else
    szPassWord := nil;
end;

{ TEwbCore }

function TEwbCore.IsCtrlCharMask: Boolean;
begin
  Result := FDisableCtrlShortcuts <> 'N';
end;

end.

