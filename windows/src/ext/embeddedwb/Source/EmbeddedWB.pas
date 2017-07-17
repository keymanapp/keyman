(*
  Name:             EmbeddedWB
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
//                        TEmbeddedWB                         *
//							      *
//                     Freeware Component                     *
//                       For Delphi 5 to Delphi XE            *
//                            by                              *
//                     Per Lindso Larsen                      *
//      Developing Team:                                      *
//          Eran Bodankin (bsalsa) -(bsalsa@gmail.com)        *
//          Thomas Stutz -(smot777@yahoo.com)                 *
//                                                            *
//     Contributors:                                          *
//            Neil Moss (NeilM@BuchananInternational.com)     *
//            Mathias Walter (mich@matze.tv)                  *
//            Serge Voloshenyuk (SergeV@bsalsa.com)           *
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
3. Mail me (bsalsa@gmail.com) any code change in the unit for the benefit
   of the other users.
4. Please, consider donation in our web site!
{*******************************************************************************}

unit EmbeddedWB;

interface

{$I EWB.inc}

uses
  Windows, Messages,
  Classes, EwbCore, Dialogs, MSHTML_EWB, EwbAcc, Controls, Graphics, Forms,
  ExtCtrls, ActiveX, ShlObj, SysUtils, SHDocVw_EWB, EwbCoreTools, UrlMon
{$IFDEF DELPHI6_UP}, Variants{$ENDIF}
{$IFDEF Enable_EwbMSHTMLEvents}, MSHTMLEvents{$ENDIF}
{$IFDEF AutoUse_EwbControl}, EwbControlComponent{$ENDIF};

type
  TEmbeddedWB = class;

  {Private events}
  TShowDialogEvent = procedure(Sender: TObject; h: THandle; StyleEx: Integer; OldCaption: string;
    var NewCaption: WideString; var Cancel: Boolean) of object;
  TMsgEvent = procedure(Sender: TObject; var Msg: TMessage; var Handled: Boolean) of object;
  TOnBusyWait = procedure(Sender: TEmbeddedWB; AStartTime: Cardinal; var TimeOut: Cardinal; var Cancel: Boolean) of object;
  TUserAgentMode = (uaDefault, uaInternal, uaRegistry);
  TKeyEventEx = procedure(Sender: TObject; var Key: Word; ScanCode: Word;
    Shift: TShiftState) of object;
  THookChildWindowEvent = procedure(Sender: TObject; WinHandle: HWND; WinClass: string; var Handled: Boolean) of object;
  THTMLCodeChangeEvent = procedure(Sender: TObject; HTMLCode: TStrings; var Handled: Boolean) of object;

  TScrollIntoView = (sivNoScroll, sivFirstMatch, sivLastMatch);
  TSearchFlags = set of (sfMatchWholeWord, sfMatchCase);
  TVariantArray = array of OleVariant;

{$IFDEF USE_EwbTools}
  TSearchResult = (srNotFound, srEndOf, srFound);
  TSearchResults = set of TSearchResult;
  TSearchDirections = (sdDown, sdUp);
{$ENDIF}

  {============================================================================}
  // TProxySettings
  {============================================================================}
  TProxySettings = class(TPersistent)
  private
    FPort: Integer;
    FAddress: string;
    FUserName: string;
    FPassword: string;
    FAutoLoadProxy: Boolean;
    FUserAgent: string;
    FBypass: string;
  public
{$IFDEF USE_EwbTools}
    function SetProxy(UserAgent, Address, Bypass: string): Boolean; overload;
    function SetProxy(UserAgent, Address, UserName, Password: string; Port: Integer): Boolean; overload;
    function SetProxyFromPAC(UserAgent, PACFile: string): Boolean;
{$ENDIF}
  published
    property AutoLoadProxy: Boolean read FAutoLoadProxy write FAutoLoadProxy default False;
    property Port: Integer read FPort write FPort default 80;
    property Password: string read FPassword write FPassword;
    property Address: string read FAddress write FAddress;
    property UserName: string read FUserName write FUserName;
    property UserAgent: string read FUserAgent write FUserAgent;
    property Bypass: string read FBypass write FBypass;
  end;

  {============================================================================}
  // TDialogBoxes
  {============================================================================}
  TDialogBoxes = class(TPersistent)
  private
    FDisableAll: Boolean;
    FReplaceCaption: Boolean;
    FReplaceIcon: Boolean;
    FNewCaption: WideString;
  published
    property DisableAll: Boolean read FDisableAll write FDisableAll default False;
    property ReplaceCaption: Boolean read FReplaceCaption write FReplaceCaption default True;
    property ReplaceIcon: Boolean read FReplaceIcon write FReplaceIcon default True;
    property NewCaption: WideString read FNewCaption write FNewCaption;
  end;

  {============================================================================}
  // TDisableErrors
  {============================================================================}
  TDisableErrors = class(TPersistent)
  private
    FEnableDDE: Boolean;
    FfpExceptions: Boolean;
    FScriptErrorsSuppressed: Boolean;
    procedure SetfpExceptions(const Value: Boolean);
  published
    property EnableDDE: Boolean read FEnableDDE write FEnableDDE default True;
    property fpExceptions: Boolean read FfpExceptions write SetfpExceptions default True;
    property ScriptErrorsSuppressed: Boolean read FScriptErrorsSuppressed write FScriptErrorsSuppressed default True;
  end;

  {============================================================================}
  // TVisualEffects
  {============================================================================}
  TVisualEffects = class(TPersistent)
  private
    FDisableSounds: Boolean;
    FTextSize: Integer;
  published
    property TextSize: Integer read FTextSize write FTextSize default 2;
    property DisableSounds: Boolean read FDisableSounds write FDisableSounds default False;
  end;

  {============================================================================}
  // TMargins
  {============================================================================}
  TMeasure = (mMetric, mUS, mUnknown);
  TPrintOrientationOption = (poPortrait, poLandScape);
  TMargins = class(TPersistent)
  private
    FLeft: Real;
    FRight: Real;
    FTop: Real;
    FBottom: Real;
  published
    property Left: Real read FLeft write FLeft;
    property Right: Real read FRight write FRight;
    property Top: Real read FTop write FTop;
    property Bottom: Real read FBottom write FBottom;
  end;

  {============================================================================}
  // TPrintOptions
  {============================================================================}
  TPrintOptions = class(TPersistent)
  private
    FEnabled: Boolean;
    FHTMLHeader: TStrings;
    FHeader: string;
    FFooter: string;
    FMargins: TMargins;
    FOrientation: TPrintOrientationOption;
    FMeasure: TMeasure;
    procedure SetHTMLHeader(const Value: Tstrings);
  public
    HideSetup: Boolean;
    property Measure: TMeasure read FMeasure;
  published
    property Enabled: Boolean read FEnabled write FEnabled default False;
    property Margins: TMargins read FMargins write FMargins;
    property Header: string read FHeader write FHeader;
    property HTMLHeader: TStrings read FHTMLHeader write SetHTMLHeader;
    property Footer: string read FFooter write FFooter;
    property Orientation: TPrintOrientationOption read FOrientation write FOrientation;
  end;

  {============================================================================}
  // TEmbeddedWB
  {============================================================================}
  TEmbeddedWB = class(TCustomEmbeddedWB,
      IDocHostUIHandler, // http://msdn.microsoft.com/en-us/library/aa753260(VS.85).aspx
      IInternetProtocol, // http://msdn.microsoft.com/en-us/library/aa767883(VS.85).aspx
      IInternetProtocolRoot) // http://msdn.microsoft.com/en-us/library/ms835683.aspx
  private
    FAbout: string;
    FBindInfo: IInternetBindInfo;
    FDestroying: Boolean;
    FDialogBoxes: TDialogBoxes;
    FDisabledPopupMenuItems: TIEPopupMenuItems;
    FDisableErrors: TDisableErrors;
    FEnableMessageHandler: Boolean;
    FHostCSS: string;
    FHostNS: string;
    FHTMLChar: Char;
    FHTMLCode: TStringList;
    FModified: Boolean;
    FName: string;
    FPrintOptions: TPrintOptions;
    FProtSink: IInternetProtocolSink;
    FProxySettings: TProxySettings;
    FResizing: Boolean;
    FRuntimeMeasure: TMeasure;
    FSearchText: string;
    FSilent: Boolean;
    FTextRange: IHTMLTxtRange;
    FUserAgent: string;
    FUserAgentInt: string;
    FUserAgentMode: TUserAgentMode;
    FUserAgentReg: string;
    FUserAgentRegSet: Boolean;
    FVisible: Boolean;
    FVisualEffects: TVisualEffects;
    FStopped: Boolean;
    FWinXPSP2orLater: Boolean;
    FWndProcSubClassed: Boolean;

    FOnBusyWait: TOnBusyWait;
    FOnClick: TMouseEvent;
    FOnCloseQuery: TCloseQueryEvent;
    FOnKeyDown: TKeyEventEx;
    FOnKeyUp: TKeyEventEx;
    FOnMessage: TMsgEvent;
    FOnShowDialog: TShowDialogEvent;
    FOnVisible: TEWBOnVisible;
    FOnHTMLCodeChange: THTMLCodeChangeEvent;

    {=ParentFormWndProc =============================}

{$IFDEF Enable_HookParentFormWndProc}
    FOldWindowProc: TWndMethod;
    FParentForm: TForm;
{$ENDIF}

    {=Mouse Events ==================================}
{$IFDEF Enable_MouseEnterLeaveEvents}
    FOnMouseEnter: TNotifyEvent;
    FOnMouseLeave: TNotifyEvent;
{$ENDIF}

    {=SubClass ChildWindows =========================}

{$IFDEF Enable_SubClassChildWindows}
    FDefInetExplorerServerProc: Pointer;
    FDefShellObjViewProc: Pointer;
    FDefSysListViewObjProc: Pointer;
    FShellDocObjViewHandle: THandle;
    FSysListViewHandle: THandle;
    FInetExplorerServerHandle: THandle;
    FShellDocObjInstance: Pointer;
    FInetExplorerServerInstance: Pointer;
    FSysListViewObjInstance: Pointer;
{$ENDIF}

{$IFDEF Enable_EwbMSHTMLEvents}
    FSinkComponent: TMSHTMLHTMLDocumentEvents;
{$ENDIF}

{$IFDEF USE_EwbTools}
    FLastSearchDirection: TSearchDirections;
{$ENDIF}

{$IFDEF DELPHI12_UP}
    FEncoding: TEncoding;
{$ENDIF}
    {=UserAgent stuff ===============================}
    function OnSetUserAgentEvent(var UserAgent: string): HRESULT;
    procedure RestoreUserAgentReg;
    procedure SetUserAgent(const Value: string);
    procedure SetUserAgentInt;
    procedure SetUserAgentMode(Value: TUserAgentMode);
    procedure SetUserAgentReg;
    procedure UpdateUserAgent;

    {=Misc ==========================================}
    function GetDesginMode: Boolean;
    function GetModified: Boolean;
    function GetPrintValues: Boolean;
    procedure HTMLCodeChanged(Sender: TObject);
    procedure OnHookChildWindows(Sender: TObject);
    procedure SetAbout(Value: string);
    procedure SetDesginMode(Value: Boolean);
    procedure SetHTMLCode(Value: TStringList);
    procedure SetModified(Value: Boolean);
    procedure SetActiveControl(ParentForm: TCustomForm = nil);
    {=Events ========================================}
    procedure CMVisibleChanged(var MSG: TMessage); message CM_VISIBLECHANGED;
    procedure WMSetWBFocus(var Msg: TMessage); message WM_SETWBFOCUS;
    procedure WMSetFocus(var Message: TWMSetFocus); message WM_SETFOCUS;

{$IFDEF Enable_MouseEnterLeaveEvents}
    procedure HandleMouseEnterLeaveEvents(var msg: TMessage);
{$ENDIF}

{$IFDEF Enable_HookParentFormWndProc}
    procedure HandleDialogBoxes(var AMsg: TMessage);
{$ENDIF}

{$IFDEF USE_EwbDDE}
    procedure DDEWndProc(var AMsg: TMessage);
{$ENDIF}

{$IFDEF Enable_HookParentFormWndProc}
    procedure UnHookParentFormWndProc;
    procedure FormWndProc(var AMsg: TMessage);
    procedure HookParentFormWndProc;
{$ENDIF}

{$IFDEF Enable_SubClassChildWindows}
    procedure ShellDocObjWndProc(var Msg: TMessage);
    procedure InetExplorerServerWndProc(var Msg: TMessage);
    procedure SysListViewWndProc(var Msg: TMessage);
    procedure InitEWBChildHook;
    procedure HookChildWindows;
    procedure UnHookChildWindows;
{$ENDIF}

{$IFDEF Enable_EwbMSHTMLEvents}
    procedure SetSinkComponent(Value: TMSHTMLHTMLDocumentEvents);
{$ENDIF}
  protected
    FSearchTxtRange: IHTMLTxtRange;
    {IDocHostUIHandler Interface}
    function GetHostInfo(var pInfo: TDOCHOSTUIINFO): HRESULT; stdcall;
    {IInternetProtocolRoot Interface}
    function Abort(hrReason: HRESULT; dwOptions: DWORD): HRESULT; stdcall;
    function Continue(const ProtocolData: TProtocolData): HRESULT; stdcall;
    function Resume: HRESULT; stdcall;
    function Start(szUrl: PWideChar; OIProtSink: IInternetProtocolSink;
      OIBindInfo: IInternetBindInfo; grfPI, dwReserved: DWORD): HRESULT; stdcall;
    function Suspend: HRESULT; stdcall;
    function Terminate(dwOptions: DWORD): HRESULT; stdcall;
    {IInternetProtocol Interface}
    function Read(pv: Pointer; cb: ULONG; out cbRead: ULONG): HResult; stdcall;
    function Seek(dlibMove: LARGE_INTEGER; dwOrigin: DWORD; out libNewPosition: ULARGE_INTEGER): HRESULT; stdcall;
    function LockRequest(dwOptions: DWORD): HRESULT; stdcall;
    function UnlockRequest: HRESULT; stdcall;
{$IFDEF USE_EwbDDE}
    function DDEExecute(iwParam: WPARAM; ilParam: LPARAM): LRESULT;
    function DDEInitiate(iwParam: WPARAM; ilParam: LPARAM): LRESULT;
    function DDETerminate(iwParam: WPARAM; ilParam: LPARAM): BOOL;
{$ENDIF}
    function PrintMarginStr(M: Real): string;
    function FilterPopupMenu: Boolean; override;
    procedure DoFilterPopupMenu(Sender: TObject; ID: DWORD; Menu: HMENU; const Context: IDispatch); override;
    function GetVisible: Boolean;

    procedure SetRegisterAsDropTarget(Value: Boolean); reintroduce;
    function GetRegisterAsDropTarget: Boolean; reintroduce;
    procedure SetRegisterAsBrowser(Value: Boolean); reintroduce;
    function GetRegisterAsBrowser: Boolean; reintroduce;
    procedure SetSilent(Value: Boolean); reintroduce;
    function GetSilent: Boolean; reintroduce;
    function GetName: string; reintroduce;
    procedure SetName(Value: string); reintroduce;
    function GetParent: TWinControl; reintroduce;
    procedure SetParent(Control: TWinControl); reintroduce;
    function GetHWND: Integer;
    procedure SetVisible(AValue: Boolean);
    procedure WndProc(var AMsg: TMessage); override;
  public
    SecurityManager: IInternetSecurityManager;
    ZoneManager: IInternetZoneManager;
    FResizable: Boolean;
    property Name: string read GetName write SetName;
    property HWND: Integer read GetHWND;
    property Parent: TWinControl read GetParent write SetParent;
    property Resizable: Boolean read FResizable write FResizable default True;
{$IFDEF DELPHI12_UP}
    property Encoding: TEncoding read FEncoding write FEncoding;
{$ENDIF}
    constructor Create(Owner: TComponent); override;
    destructor Destroy; override;
    procedure SetFocus; override;
    function Focused: Boolean; override;

    function LoadFrameFromStream(FrameNo: Integer; AStream: TStream): HRESULT;
    function LoadFrameFromStrings(FrameNo: Word; const AStrings: TStrings): HRESULT;
    function LoadFromStream(const AStream: TStream): HRESULT;
    function LoadFromStrings(const AStrings: TStrings): HRESULT; overload;
    function LoadFromStrings(const AStrings: TStrings; AddHtmlTags: Boolean): HRESULT; overload;
    function LoadFromString(const St: string): HRESULT; // by M.Grusha
    function LoadFromFile(const FileName: string): HRESULT; // by M.Grusha
    procedure LoadFromImage(Image: TImage);
{$IFDEF DELPHI6_UP}
    function LoadFromWideString(const WideSt: WideString): HRESULT;
{$ENDIF}
    function SaveFrameToFile(FrameNo: Word; const FName: string): HRESULT;
    function SaveFrameToStream(FrameNo: Integer; AStream: TStream): HRESULT;
    function SaveFrameToStrings(FrameNo: Integer; AStrings: TStrings): HRESULT;
    function SaveToFile(const FileName: string): HRESULT;
    function SaveToStream(AStream: TStream): HRESULT;
    function SaveToStrings(AStrings: TStrings): HRESULT;

    function DocumentLoaded: Boolean; overload;
    function DocumentLoaded(out Doc2: IHTMLDocument2): Boolean; overload;
    procedure AssignEmptyDocument(bWait: Boolean = False);
    function GetDocument: IHTMLDocument2;
    function GetFrame(FrameNo: Word): IWebbrowser2;
    procedure RefreshFrame(FrameNo: Word);
    function GetFrameFromDocument(SourceDoc: IHTMLDocument2; FrameNo: Integer): IWebBrowser2;
    function GetActiveFrame(): IHTMLDocument2; overload;
    function GetActiveFrame(Doc: IHTMLDocument2): IHTMLDocument2; overload;
    function GetActiveElement(Doc: IHTMLDocument2 = nil): IHTMLElement;
    function GetSelectedText(ReturnAsHTML: Boolean = False; bOnlyTopLevel: Boolean = False): string;
    function GetSelLength(bOnlyTopLevel: Boolean = False): Integer;

    function Copy: Boolean;
    function Cut: Boolean;
    function Paste: Boolean;
    function PasteSpecial: Boolean;
    function SelectAll: Boolean;
    function ClearSelection: Boolean;
    function Undo: Boolean;
    function Redo: Boolean;
    function Delete: Boolean;

    procedure Stop;
    procedure GoBack;
    procedure GoForward;
    function Go(Url: string; TimeOut: LongWord = 0): Boolean;
    function NavigateWait(const URL: WideString; TimeOut: LongWord = 0): Boolean;
    procedure NavigateFolder(CSIDL: Integer);
    procedure NavigatePidl(pidl: PItemIdlist);
    function NavigateToFrame(FrameList: string): IHTMLDocument2;
    function Wait(TimeOut: Longword = 0): Boolean;
    function WaitWhileBusy(TimeOut: Longword = 0): Boolean;

    class function GetIEHandle(WebBrowser: TEmbeddedWB; ClassName: string): HWND;
    function DownloadFile(SourceFile, DestFile: string): Boolean;
    function GetIEHomePage: string;
    function VariantIsObject(const value: OleVariant): Boolean;
    function IsCommandEnabled(sCmdId: WideString): Boolean;
    function QueryCommandValue(sCmdId: WideString): OleVariant;
    function InvokeCMD(InvokeIE: Boolean; Value1, Value2: Integer; var vaIn, vaOut: OleVariant): HRESULT;
    procedure InvokeIEServerCommand(Cmd: Integer);
    procedure Loaded; override;
    procedure LoadSettings;
    procedure ShowImportExportFavoritesAndCookies;
    procedure ShowAboutBox; reintroduce;

{$IFDEF USE_EwbTools}
    function AddHtmlToAboutBlank(StringToHtml: string): Boolean;
    function CheckIfInRestricredList(Host: string; SecureSite: Boolean): Boolean;
    function CheckIfInTrustedList(const Host: string; SecureSite: Boolean): Boolean;
    function CheckOnlineStatus: Boolean;
    function DecodeUrl(const InputStr: string): string;
    function DocumentSource: string; // By Bitmaker
    function DocumentSourceText: string; // By Bitmaker
    function EncodeUrl(const InputStr: string; const bQueryStr: Boolean): string;
    function ExecScriptEx(MethodName: string; ParamValues: array of const): OleVariant;
    function FillForm(FieldName: string; Value: string; ElementNr: Integer = -1): Boolean;
    function FrameCount: LongInt;
    function FrameCountFromDocument(SourceDoc: IHTMLDocument2): Integer;
    function GetBmpFromBrowser(FileName: string): Boolean;
    function GetCachedFileFromURL(ItemUrl: string): string;
    function GetCookie: string;
    function GetCookiesPath: string;
    function GetDefaultBrowserFromRegistry: string;
    function GetFavoritesPath: string;
    function GetFieldValue(FieldName: string): string;
    function GetHistoryPath: string;
    function GetIPAndHostName(var HostName, IPaddr, WSAErr: string): Boolean;
    function GetJpegFromBrowser(FileName: string; SourceHeight, SourceWidth, TargetHeight, TargetWidth: Integer): Boolean;
    function GetLastVisitedPage(var LastVisitedPage: string): Boolean;
    function GetSpecialFolderPath(CallerHandle: THandle; CSIDL: Integer): PChar;
    function GetSSLStatus(var SSLName, SSLDescription: string): Boolean;
    function GetURLSecurityZone(var ZoneName, ZoneDescription: string; var Icon: TIcon): Boolean;
    function GetWordAtCursor(const X, Y: Integer): string; // by M.Grusha
    function GetZoneAttributes(const URL: string): TZoneAttributes;
    function GetZoneIconToForm: Boolean;
    function HScrollBarPosition: Integer;
    function HScrollBarVisible: Boolean;
    function ImportCertFile(FileName, StoreType: string): Boolean;
    function IsGlobalOffline: Boolean;
    function IsValidProtocol(const URL: string): Boolean;
    function OpenClient(Client: string): Boolean;
    function OpenDialog: Boolean;

    function PageSetup(UsePrintOptions: Boolean = False): Boolean;
    function SaveDialog: Boolean;
    function SaveDialogEx(AFilter: string = ''; ATitle: string = ''): string;
    function SaveLastVisitedPage: Boolean;
    function SearchNextText(const Value: string; Direction: TSearchDirections = sdDown;
      AutoSelect: Boolean = True): TSearchResults; // by Grusha M.A.
    function SearchString(const strText: string): Boolean;
    function SearchText(const Value: string; const iPos: Integer = 1): IHTMLTxtRange; //by JJM
    function SetCharartersSet(const ACharactersSet: string; Refresh: Boolean = True): Boolean;
    function ShowFindDialog: Boolean;
    function ShowInternetOptions: Boolean;
    function ShowOrganizeFavorites: Boolean;
    function ShowPageProperties: Boolean;
    function URLFromFavorites(const dotURL: string): string;
    function UrlFromHistory(ShellFolder: IShellFolder; pidl: PItemIDList): string;
    function ViewPageSourceHtml: Boolean;
    function VScrollBarPosition: Integer;
    function VScrollBarVisible: Boolean;
    procedure AddToFavorites(URL, Title: string);
    procedure AddToRestrictedSiteList(URL: string);
    procedure AddToTrustedSiteList(URL: string);
    procedure ClearCache;
    procedure ClearHistory;
    procedure ClearTypedUrls;
    procedure CreateDesktopShortcut;
    procedure CreateNewMail;
    procedure DisableNavSound(bDisable: Boolean);
    procedure ExecScript(sExpression, sLanguage: string);
    procedure ExploreFolder(Path: string);
    procedure FillFormAndExcecute;
    procedure GetThumbnail(var Image: TImage);
    procedure GetZoneIcon(IconPath: string; var Icon: TIcon);
    procedure GoAboutBlank;
    procedure GoDownloadFile(URL: string);
    procedure GoDownloadMaskedFile(SourceFile, TargetFile: string; Notify: Boolean);
    procedure GoNoHistory(const URL: string);
    procedure GoSearchInGoogle(SearchTerm: string);
    procedure GoSearchInMSN(SearchTerm: string);
    procedure GoSearchInYahoo(SearchTerm: string);
    procedure GoWithQueryDetails(Url, Query: string);
    procedure OpenIEBrowserWithAddress;
    procedure Print;
    procedure PrintPreview;
    procedure PrintPreviewExtended(nCMDShow: Integer);
    procedure PrintPreviewFromTemplate(const TemplateFileName: string);
    procedure PrintSetup;
    procedure PrintWithOptions;
    procedure RefreshProxy;
    procedure RestoreApplicationFormSize;
    procedure SaveApplicationFormSize;
    procedure SaveImagesDialog;
    procedure SavePageTextDialog;
    procedure ScrollToBottom;
    procedure ScrollToID(ID: Integer);
    procedure ScrollToIDEx(ID: string); // by M.Grusha
    procedure ScrollToPosition(X, Y: Integer);
    procedure ScrollToTop;
    procedure SearchAndHighlight(AText: string; const ACaption, APrompt: string;
      Flags: TSearchFlags = []; cbackColor: string = 'yellow'; cForeColor: string = '';
      ScrollIntoView: TScrollIntoView = sivNoScroll); overload;
    procedure SearchAndHighlight(aText: string; Flags: TSearchFlags = [];
      cbackColor: string = 'yellow'; cForeColor: string = '';
      ScrollIntoView: TScrollIntoView = sivNoScroll); overload;
    procedure SendPageInMailAsAttachment(aOwner: TComponent; FileName, Subject, Body: string);
    procedure SendURLInMail;
    procedure SetNewHomePage(HomePage: string);
    procedure ShowIEVersionInfo;
    procedure ViewPageLinksToStrings(LinksList: TStrings);
    procedure ViewPageSourceHtmlToStrings(HtmlList: TStrings);
    procedure ViewPageSourceText;
    procedure ViewPageSourceTextToStrings(TextList: TStrings);
    procedure WorkOffline;
    procedure WorkOnline;
{$ENDIF}
{$IFDEF Enable_EwbMSHTMLEvents}
    property SinkComponent: TMSHTMLHTMLDocumentEvents read FSinkComponent write SetSinkComponent;
{$ENDIF}
    property Modified: Boolean read GetModified write SetModified;
    property DesignMode: Boolean read GetDesginMode write SetDesginMode;
  published
    property Silent: Boolean read GetSilent write SetSilent default True;
    property RegisterAsDropTarget: Boolean read GetRegisterAsDropTarget write SetRegisterAsDropTarget default True;
    property RegisterAsBrowser: Boolean read GetRegisterAsBrowser write SetRegisterAsBrowser default False;
    property About: string read FAbout write SetAbout;
    property HostCSS: string read FHostCSS write FHostCSS;
    property HostNS: string read FHostNS write FHostNS;
    property EnableMessageHandler: Boolean read FEnableMessageHandler write FEnableMessageHandler default True;
    property DisabledPopupMenuItems: TIEPopupMenuItems read FDisabledPopupMenuItems
      write FDisabledPopupMenuItems default [];
    property DisableErrors: TDisableErrors read FDisableErrors write FDisableErrors;
    property DialogBoxes: TDialogBoxes read FDialogBoxes write FDialogBoxes;
    property HTMLCode: TStringList read FHTMLCode write SetHTMLCode;
    property OnCloseQuery: TCloseQueryEvent read FOnCloseQuery write FOnCloseQuery;
    property OnMessage: TMsgEvent read FOnMessage write FOnMessage;
    property OnShowDialog: TShowDialogEvent read FOnShowDialog write FOnShowDialog;
    property PrintOptions: TPrintOptions read FPrintOptions write FPrintOptions;
    property ProxySettings: TProxySettings read FProxySettings write FProxySettings;
    property VisualEffects: TVisualEffects read FVisualEffects write FVisualEffects;
    property Visible: Boolean read GetVisible write SetVisible default True;
    property UserAgent: string read FUserAgent write SetUserAgent;
    property UserAgentMode: TUserAgentMode read FUserAgentMode write SetUserAgentMode default uaDefault;
    property OnBusyWait: TOnBusyWait read FOnBusyWait write FOnBusyWait;
    property OnVisible: TEWBOnVisible read FOnVisible write FOnVisible;
    property OnKeyDown: TKeyEventEx read FOnKeyDown write FOnKeyDown;
    property OnKeyUp: TKeyEventEx read FOnKeyUp write FOnKeyUp;
    property OnClick: TMouseEvent read FOnClick write FOnClick;
    property OnHTMLCodeChange: THTMLCodeChangeEvent read FOnHTMLCodeChange write FOnHTMLCodeChange;
{$IFDEF Enable_MouseEnterLeaveEvents}
    property OnMouseLeave: TNotifyEvent read FOnMouseLeave write FOnMouseLeave;
    property OnMouseEnter: TNotifyEvent read FOnMouseEnter write FOnMouseEnter;
{$ENDIF}
  end;

implementation

uses
{$IFDEF USE_EwbTools}
  EWBTools,
{$ENDIF}
{$IFDEF USE_EwbDDE}
  EwbDDE,
{$ENDIF}
  Registry, CommCtrl, ComObj, ShellAPI, OleServer, EwbIEConst, WinInet;

var
  // Boolean variables that must be global to restore settings for MDI cases
  // or multiple EWB instances
{$IFDEF USE_EwbDDE}
  HtmlFileApp, HtmlFileTopic: string; //All DDE variables
  FHtmlCommand, bDontRespond: Boolean;
  FoldersApp, FoldersTopic: string;
  DDEHWND: THandle = 0;
{$ENDIF}
  bPrintOptionsEnable: Boolean;
  wSaved8087CW: Word;
  bProxy: Boolean;
  bNavSound: Boolean;
  bOleInitialize: Boolean;
{$IFDEF Enable_HookParentFormWndProc}
  bWndProcHooked: Boolean;
  PrintingWithOptions: Boolean;
{$ENDIF}
  bInvokingPageSetup: Boolean;
{$IFDEF AutoUse_EwbControl}
  EwbControl: TEwbControl;
{$ENDIF}

  //=== Print Options-Persistent =================================================

procedure TPrintOptions.SetHTMLHeader(const Value: Tstrings);
begin
  FHTMLHeader.Assign(Value);
end;

//=== DisableErrors-Persistent =================================================

procedure TDisableErrors.SetfpExceptions(const Value: Boolean);
begin
  if not Value then
    Set8087CW($133F)
  else
    Set8087CW(wSaved8087CW);
  FfpExceptions := Value;
end;

//=== Accesories ===============================================================

function NextPIDL(IDList: PItemIDList): PItemIDList;
begin
  Result := IDList;
  Inc(PAnsiChar(Result), IDList^.mkid.cb);
end;

function GetPIDLSize(IDList: PItemIDList): Integer;
begin
  Result := 0;
  if Assigned(IDList) then
  begin
    Result := SizeOf(IDList^.mkid.cb);
    while IDList^.mkid.cb <> 0 do
    begin
      Result := Result + IDList^.mkid.cb;
      IDList := NextPIDL(IDList);
    end;
  end;
end;

function SaveDocToStrings(Doc: IDispatch; var AStrings: TStrings): HRESULT;
var
  IpStream: IPersistStreamInit;
  AStream: TMemoryStream;
begin
  Result := S_FALSE;
  AStream := TMemoryStream.Create;
  try
    IpStream := doc as IPersistStreamInit;
    if Assigned(IpStream) then
      if Succeeded(IpStream.Save(TStreamAdapter.Create(AStream), True)) then
      begin
        AStream.Seek(0, 0);
        AStrings.LoadFromStream(AStream);
        Result := S_OK;
      end;
  finally
    AStream.Free;
  end;
end;

function SaveDocToStream(Doc: IDispatch; var AStream: TStream): HRESULT;
var
  IpStream: IPersistStreamInit;
begin
  Result := S_FALSE;
  if Assigned(Doc) then
  begin
    IpStream := Doc as IPersistStreamInit;
    if Assigned(IpStream) then
      Result := IpStream.Save(TStreamAdapter.Create(AStream), True);
  end;
end;

function SaveDocToFile(Doc: IDispatch; const FName: string): HRESULT;
var
  PFile: IPersistFile;
begin
  Result := S_FALSE;
  if Assigned(Doc) then
  begin
    PFile := Doc as IPersistFile;
    if Assigned(PFile) then
      Result := PFile.Save(StringToOleStr(FName), False);
  end;
end;

function GetRunTimeMeasure: TMeasure;
var
  Buf: array[1..10] of Char;
begin
  Result := mUnknown;
  FillChar(Buf, SizeOf(Buf), 0);
  if GetLocaleInfo(LOCALE_USER_DEFAULT, LOCALE_IMEASURE, @Buf[1], SizeOf(Buf)) <> 0 then
  begin
    if Buf[1] = '1' then
      Result := mUS
    else
      Result := mMetric;
  end;
end;

//=== IInternetProtocolRoot Interface ==========================================

function TEmbeddedWB.Start(szUrl: PWideChar; OIProtSink: IInternetProtocolSink;
  OIBindInfo: IInternetBindInfo; grfPI, dwReserved: DWORD): HRESULT; stdcall;
var
  iNeg: IHTTPNegotiate;
  szHeaders, szAdditionalHeaders: PWideChar;
  SrvProv: IServiceProvider;
begin
  FProtSink := OIProtSink;
  FBindInfo := OIBindInfo;

  (OIProtSink as iUnknown).QueryInterface(IServiceProvider, SrvProv);
  if Assigned(SrvProv) then
  begin
    SrvProv.QueryService(IID_IHTTPNegotiate, IID_IHTTPNegotiate, iNeg);
    if Assigned(iNeg) then
    begin
      szHeaders := nil;
      szAdditionalHeaders := nil;
      iNeg.BeginningTransaction(szUrl, szHeaders, 0, szAdditionalHeaders);
    end;
  end;
  Result := INET_E_USE_DEFAULT_PROTOCOLHANDLER;
end;

function TEmbeddedWB.Terminate(dwOptions: DWORD): HRESULT; stdcall;
begin
  Result := S_OK;
end;

function TEmbeddedWB.Continue(const ProtocolData: TProtocolData): HRESULT;
begin
  Result := S_OK;
end;

function TEmbeddedWB.Abort(hrReason: HRESULT; dwOptions: DWORD): HRESULT; stdcall;
begin
  Result := E_NOTIMPL;
end;

function TEmbeddedWB.Suspend: HRESULT; stdcall;
begin
  Result := E_NOTIMPL;
end;

function TEmbeddedWB.Resume: HRESULT; stdcall;
begin
  Result := E_NOTIMPL;
end;

//=== IInternetProtocol Interface  =============================================

function TEmbeddedWB.LockRequest(dwOptions: DWORD): HRESULT; stdcall;
begin
  Result := S_OK;
end;

function TEmbeddedWB.Read(pv: Pointer; cb: ULONG; out cbRead: ULONG): HRESULT;
begin
  Result := S_OK;
end;

function TEmbeddedWB.Seek(dlibMove: LARGE_INTEGER; dwOrigin: DWORD;
  out libNewPosition: ULARGE_INTEGER): HRESULT;
begin
  Result := E_NOTIMPL;
end;

function TEmbeddedWB.UnlockRequest: HRESULT;
begin
  Result := S_OK;
end;

//=== DDE stuff ================================================================

{$IFDEF USE_EwbDDE}

function TEmbeddedwb.DDETerminate(iwParam: WPARAM; ilParam: LPARAM): BOOL;
begin
  Result := PostMessage(THandle(iwParam), WM_DDE_TERMINATE, handle, 0);
end;

function TEmbeddedWB.DDEInitiate(iwParam: WPARAM; ilParam: LPARAM): LResult;
var
  dwThreadID: DWORD;
  dwProcessID: DWORD;
  hwndClient: Integer;
  aInApp, aInTopic, aOutApp, aOutTopic: ATOM;
  szInAppName, szInAppTopic: array[0..255] of Char;
begin
  FillChar(szInAppName, SizeOf(szInAppName), 0);
  FillChar(szInAppTopic, SizeOf(szInAppTopic), 0);
  if bDontRespond then
  begin
    Result := 0;
    Exit;
  end;
  hwndClient := THandle(iwParam);
  dwThreadID := GetWindowThreadProcessId(hwndClient, @dwProcessID);
  if (GetCurrentProcessId() <> dwProcessID) or (GetCurrentThreadID() <> dwThreadID) then
  begin
    Result := 0;
    Exit;
  end;
  aInApp := LOWORD(ilParam);
  aInTopic := HIWORD(ilParam);
  GlobalGetAtomName(aInApp, szInAppName, SizeOf(szInAppName));
  GlobalGetAtomName(aInTopic, szInAppTopic, SizeOf(szInAppTopic));
  if szInAppName = HtmlFileApp then
  begin
    FHtmlCommand := True;
    aOutApp := GlobalAddAtom(PChar(HtmlFileApp));
    aOutTopic := GlobalAddAtom(PChar(HtmlFileTopic));
    if ((aOutApp <> 0) and (aOutTopic <> 0) and (aOutApp = aInApp) and (aOutTopic = aInTopic)) then
      SendMessage(hwndClient, WM_DDE_ACK, WPARAM(handle), MAKELPARAM(aOutApp, aOutTopic));
    if (aOutApp <> 0) then
      GlobalDeleteAtom(aOutApp);
    if (aOutTopic <> 0) then
      GlobalDeleteAtom(aOutTopic);
  end
  else
  begin
    FHtmlCommand := False;
    aOutApp := GlobalAddAtom(PChar(FoldersApp));
    aOutTopic := GlobalAddAtom(PChar(FoldersTopic));
    if ((aOutApp <> 0) and (aOutTopic <> 0) and (aOutApp = aInApp) and (aOutTopic = aInTopic)) then
      SendMessage(HWNDClient, WM_DDE_ACK, WPARAM(Handle), MAKELPARAM(aOutApp, aOutTopic));
    if (aOutApp <> 0) then
      GlobalDeleteAtom(aOutApp);
    if (aOutTopic <> 0) then
      GlobalDeleteAtom(aOutTopic);
  end;
  Result := 0;
end;

function TEmbeddedwb.DDEExecute(iwParam: WPARAM; ilParam: LPARAM): LResult;
var
  szFolder: string;
  szCommand: LPTSTR;
  uLo: PUINT;
  hgMem: HGLOBAL;
  ack: DDEACK;
  lpTemp: PUINT;
  uCommand: Cardinal;
  show: Integer;
  pidl: PITEMIDLIST;
  sei: TShellExecuteInfo;
  szTmp: string;
begin
  ulo := nil;
  if UnpackDDElParam(WM_DDE_EXECUTE, ilParam, uLo, @hgMem) then
  begin
    szCommand := GlobalLock(hgmem);
    ZeroMemory(@Ack, SizeOf(ddeAck));
    if Assigned(szCommand) then
    begin
      if FHtmlCommand then
      begin
        szTmp := szCommand;
        if Pos('"', szTmp) = 1 then
        begin
          System.Delete(szTmp, 1, 1);
          szTmp := System.Copy(szTmp, 1, Pos('"', szTmp) - 1);
        end;
        Go(szTmp);
        Ack.Flags := 1;
      end
      else
      begin
        uCommand := ParseDDECommand(szCommand, szFolder, pidl, Show);
        case uCommand of
          VIEW_COMMAND:
            begin
              if (szFolder <> '') then
                Go(szFolder)
              else if Assigned(pidl) then
                NavigatePidl(pidl);

              DisposePidl(pidl);
              Ack.flags := 1;
            end;
          EXPLORE_COMMAND:
            begin
              bDontRespond := True;
              ZeroMemory(@sei, SizeOf(SHELLEXECUTEINFO));
              sei.cbSize := SizeOf(SHELLEXECUTEINFO);
              if szFolder <> '' then
              begin
                sei.fMask := SEE_MASK_CLASSNAME;
                sei.lpFile := Pchar(szFolder);
              end
              else
              begin
                sei.fMask := SEE_MASK_IDLIST or SEE_MASK_CLASSNAME;
                sei.lpIDList := pidl;
              end;
              sei.lpClass := 'folder';
              sei.Wnd := 0;
              sei.nShow := Show;
              sei.lpVerb := 'explore';
              ShellExecuteEx(@sei);
              bDontRespond := False;
              DisposePidl(pidl);
              Ack.flags := 1;
            end;
          FIND_COMMAND:
            begin
              bDontRespond := True;
              ZeroMemory(@sei, SizeOf(SHELLEXECUTEINFO));
              sei.cbSize := SizeOf(SHELLEXECUTEINFO);
              if (szFolder <> '') then
              begin
                sei.fMask := 0;
                sei.lpFile := PChar(szFolder);
              end
              else
              begin
                sei.fMask := SEE_MASK_IDLIST;
                sei.lpIDList := pidl;
              end;
              sei.wnd := 0;
              sei.nShow := Show;
              sei.lpVerb := 'find';
              ShellExecuteEx(@sei);
              bDontRespond := False;
              DisposePidl(pidl);
              Ack.flags := 1;
            end;
        end;
      end;
      GlobalUnlock(hgMem);
      lpTemp := @Ack;
      PostMessage(Thandle(iwParam),
        WM_DDE_ACK,
        WPARAM(handle),
        ReuseDDElParam(ilParam, WM_DDE_EXECUTE, WM_DDE_ACK, lpTemp^, hgMem));
    end;
  end;
  Result := 0;
end;

procedure TEmbeddedWB.DDEWndProc(var AMsg: Messages.TMessage);
begin
  with AMsg do
    if (Msg = WM_DDE_INITIATE) and FDisableErrors.FEnableDDE then
    begin
      try
        DDEInitiate(WParam, LParam)
      except
      end;
    end
    else
      Result := DefWindowProc(DDEHWND, Msg, WParam, LParam);
end;
{$ENDIF} // {$IFDEF USE_EwbDDE}

//=== TEmbeddedWB ==============================================================

procedure TEmbeddedWB.CMVisibleChanged(var MSG: TMessage);
begin
  inherited;
  FVisible := MSG.WParam = 1;
  if Assigned(FOnVisible) then
    FOnVisible(Self, Self.Visible);
end;

//=== Hooks & Message Handling =================================================

{$IFDEF Enable_SubClassChildWindows}

procedure TEmbeddedWB.InitEWBChildHook;
begin
  FDefInetExplorerServerProc := nil;
  FDefShellObjViewProc := nil;
  FDefSysListViewObjProc := nil;

  FShellDocObjViewHandle := 0;
  FInetExplorerServerHandle := 0;
  FSysListViewHandle := 0;

  FShellDocObjInstance := nil;
  FInetExplorerServerInstance := nil;
  FSysListViewObjInstance := nil;
end;

procedure TEmbeddedWB.HookChildWindows;
begin
  if (csDesigning in ComponentState) or Forms.Application.Terminated then
    Exit;
  if not Assigned(Self) then
    Exit;

  if FSysListViewHandle = 0 then // SysListView32
  begin
    FSysListViewHandle := GetIEWin('SysListView32');
    if FSysListViewHandle <> 0 then
    begin
      FSysListViewObjInstance := {$IFDEF DELPHI6_UP}Classes.{$ENDIF}MakeObjectInstance(SysListViewWndProc);
      FDefSysListViewObjProc := Pointer(GetWindowLong(FSysListViewHandle, GWL_WNDPROC));
      SetWindowLongW(FSysListViewHandle, GWL_WNDPROC, Longint(FSysListViewObjInstance));
      Exit;
    end;
  end;
  // Hook child windows to catch WM_DESTROY messages
  if (FShellDocObjViewHandle = 0) and (FInetExplorerServerHandle = 0) then
  begin
    FShellDocObjViewHandle := Windows.GetWindow(Self.Handle, GW_CHILD);
    if (FShellDocObjViewHandle <> 0) then
    begin
      FInetExplorerServerInstance := {$IFDEF DELPHI6_UP}Classes.{$ENDIF}MakeObjectInstance(InetExplorerServerWndProc);
      FShellDocObjInstance := {$IFDEF DELPHI6_UP}Classes.{$ENDIF}MakeObjectInstance(ShellDocObjWndProc);
      // ShellDocObj
      FInetExplorerServerHandle := Windows.GetWindow(FShellDocObjViewHandle, GW_CHILD);
      FDefShellObjViewProc := Pointer(GetWindowLong(FShellDocObjViewHandle, GWL_WNDPROC));
      SetWindowLongW(FShellDocObjViewHandle, GWL_WNDPROC, Longint(FShellDocObjInstance));
      // Internet Explorer Server
      FDefInetExplorerServerProc := Pointer(GetWindowLong(FInetExplorerServerHandle, GWL_WNDPROC));
      SetWindowLongW(FInetExplorerServerHandle, GWL_WNDPROC, Longint(FInetExplorerServerInstance));
    end;
  end;
end;

procedure TEmbeddedWB.SysListViewWndProc(var Msg: TMessage);
begin
  with Msg do
    Result := CallWindowProcW(FDefSysListViewObjProc, FSysListViewHandle, Msg, WParam, LParam);
  case Msg.Msg of
    WM_SETFOCUS: SetActiveControl;
    WM_DESTROY: UnHookChildWindows;
  end;
end;

procedure TEmbeddedWB.InetExplorerServerWndProc(var Msg: TMessage);
type
  PWMKey = ^TWMKey;
begin
  with Msg do
    Result := CallWindowProcW(FDefInetExplorerServerProc, FInetExplorerServerHandle, Msg, WParam, LParam);
  case Msg.Msg of
    WM_SETFOCUS:
      // Catching this message allows us to set the Active control to the
      // WebBrowser itself which keeps VCL in sync with the real active control
      // which makes things like tabbing work correctly.
      SetActiveControl;
    WM_DESTROY: UnHookChildWindows;
  end;
end;

procedure TEmbeddedWB.ShellDocObjWndProc(var Msg: TMessage);
begin
  with Msg do
    Result := CallWindowProcW(FDefShellObjViewProc, FShellDocObjViewHandle, Msg, WParam, LParam);
  case Msg.Msg of
    WM_SETFOCUS: SetActiveControl;
    WM_DESTROY: UnHookChildWindows;
  end;
end;

procedure TEmbeddedWB.UnHookChildWindows;
begin
  if FSysListViewHandle <> 0 then
  begin
    SetWindowLongW(FSysListViewHandle, GWL_WNDPROC, Integer(FDefSysListViewObjProc));
{$IFDEF DELPHI6_UP}Classes.{$ENDIF}FreeObjectInstance(FSysListViewObjInstance);
    FSysListViewHandle := 0;
    FSysListViewObjInstance := nil;
  end;

  if FShellDocObjViewHandle <> 0 then
  begin
    SetWindowLongW(FShellDocObjViewHandle, GWL_WNDPROC, Integer(FDefShellObjViewProc));
{$IFDEF DELPHI6_UP}Classes.{$ENDIF}FreeObjectInstance(FShellDocObjInstance);
    FShellDocObjViewHandle := 0;
    FShellDocObjInstance := nil;
  end;

  if FInetExplorerServerHandle <> 0 then
  begin
    SetWindowLongW(FInetExplorerServerHandle, GWL_WNDPROC, Integer(FDefInetExplorerServerProc));
{$IFDEF DELPHI6_UP}Classes.{$ENDIF}FreeObjectInstance(FInetExplorerServerInstance);
    FInetExplorerServerHandle := 0;
    FInetExplorerServerInstance := nil;
  end;
end;

{$ENDIF} // Enable_SubClassChildWindows

procedure TEmbeddedWB.OnHookChildWindows(Sender: TObject);
begin
{$IFDEF Enable_SubClassChildWindows}
  if (FShellDocObjViewHandle = 0) or (FSysListViewHandle = 0) then
    HookChildWindows;
{$ENDIF};
end;

procedure TEmbeddedWB.SetActiveControl(ParentForm: TCustomForm = nil);
begin
  if ParentForm = nil then
    ParentForm := GetParentForm(Self);
  if Assigned(ParentForm) then
  begin
    if Self.CanFocus then
    begin
      ParentForm.ActiveControl := Self;
      Self.SetFocusToDoc;
    end;
  end;
end;

procedure TEmbeddedWB.WMSetWBFocus(var Msg: TMessage);
var
  ParentForm: TCustomForm;
begin
  ParentForm := GetParentForm(Self);
  if Assigned(ParentForm) then
    if ParentForm.ActiveControl is TEmbeddedWB then
      SetActiveControl(ParentForm);
end;

procedure TEmbeddedWB.SetFocus;
begin
  if Self.CanFocus then
    inherited;
end;

function TEmbeddedWB.Focused: Boolean;
begin
  Result := IsChild(Self.Handle, GetFocus);
end;

procedure TEmbeddedWB.WMSetFocus(var Message: TWMSetFocus);
begin
  inherited;
end;

{$IFDEF Enable_HookParentFormWndProc}

procedure TEmbeddedWB.HandleDialogBoxes(var AMsg: Messages.TMessage);
var
  PopHandle: Integer;
  DlgCaption, DlgClss: string;
  Msg: TWMActivate;
  WI: TWindowInfo;
begin
  Msg := TWMActivate(AMsg);
  if Msg.Active = 0 then
  begin
    PopHandle := Msg.ActiveWindow;
    FillChar(WI, SizeOf(WI), 0);
    if PopHandle <> 0 then
    begin
      WI.dwStyle := Abs(GetWindowLong(PopHandle, GWL_STYLE));
      WI.dwExStyle := Abs(GetWindowLong(PopHandle, GWL_EXSTYLE));
    end;

    DlgClss := GetWinClass(PopHandle);
    if (DlgClss = 'Internet Explorer_TridentDlgFrame') or ((DlgClss = '#32770') and
      ((GetWinClass(Windows.GetParent(PopHandle)) <> 'TApplication') and
      (FindControl(Windows.GetParent(PopHandle)) = nil))) then
    begin
      DlgCaption := GetWinText(PopHandle);
      if (PopHandle <> 0) and Assigned(FOnShowDialog) then
        FOnShowDialog(Self, PopHandle, WI.dwExStyle, DlgCaption, FDialogBoxes.FNewCaption, FDialogBoxes.FDisableAll);

      if FDialogBoxes.FDisableAll then
        SendMessage(PopHandle, WM_CLOSE, 0, 0);
      if FDialogBoxes.FReplaceIcon then
        SendMessage(PopHandle, WM_SETICON, ICON_SMALL, Forms.Application.Icon.Handle);

      if FDialogBoxes.FReplaceCaption then
      begin
        DlgCaption := StringReplace(DlgCaption, 'Microsoft ', '', []);
        DlgCaption := StringReplace(DlgCaption, 'Internet Explorer', FDialogBoxes.FNewCaption, []);
        SetWindowText(PopHandle, PChar(DlgCaption));
      end;

      if FDisableErrors.FScriptErrorsSuppressed then
      begin
        if (AnsiPos('SCRIPT', AnsiUpperCase(DlgCaption)) <> 0) then
        begin
          PostMessage(PopHandle, WM_LBUTTONDOWN, 0, 0);
          PostMessage(PopHandle, WM_LBUTTONUP, 0, 0);
          SendMessage(PopHandle, WM_CLOSE, 0, 0);
          Forms.Application.ProcessMessages;
          Exit;
        end;
        if (AnsiPos('ERROR', AnsiUpperCase(DlgCaption)) <> 0) or (WI.dwExStyle = 4260097) then
        begin
          DestroyWindow(PopHandle);
          Exit;
        end;
      end;
      if FPrintOptions.FEnabled then
      begin
        bPrintOptionsEnable := True;
        if bInvokingPageSetup then
        begin
          bInvokingPageSetup := False;
          if PrintingWithOptions then
          begin
            SetWindowPos(0, 0, -4400, 0, 0, 0, 0); //SetWindowPos(Wnd, 0, -600, 0, 0, 0, 0);
            PrintingWithOptions := False;
          end;
          if FPrintOptions.FOrientation = poPortrait then
            SendDlgItemMessage(PopHandle, $0420, BM_CLICK, 0, 0)
          else
            SendDlgItemMessage(PopHandle, $0421, BM_CLICK, 0, 0);
          SetDlgItemText(PopHandle, $1FD3, PChar(FPrintOptions.FHeader));
          SetDlgItemText(PopHandle, $1FD5, PChar(FPrintOptions.FFooter));
          SetDlgItemText(PopHandle, $0483, PChar(PrintMarginStr(FPrintOptions.FMargins.FLeft)));
          SetDlgItemText(PopHandle, $0484, PChar(PrintMarginStr(FPrintOptions.FMargins.FTop)));
          SetDlgItemText(PopHandle, $0485, PChar(PrintMarginStr(FPrintOptions.FMargins.FRight)));
          SetDlgItemText(PopHandle, $0486, PChar(PrintMarginStr(FPrintOptions.FMargins.FBottom)));
          if (Win32Platform = VER_PLATFORM_WIN32_NT) and (Win32MajorVersion > 4) then
            PostMessage(FindWindowEx(PopHandle, 0, 'Button', nil), BM_CLICK, 0, 0) //Win2000
          else
            SendDlgItemMessage(PopHandle, 1, BM_CLICK, 0, 0);
        end;
      end;
    end;
  end;
end;

procedure TEmbeddedWB.FormWndProc(var AMsg: Messages.TMessage);
{var
 s: string;}
begin
  if AMsg.Msg = WM_ACTIVATE then
  begin
    HandleDialogBoxes(AMsg);
    {
    if ((AMsg.WParamLo and WA_ACTIVE) = WA_ACTIVE) or
       ((AMsg.WParamLo and WA_CLICKACTIVE) = WA_CLICKACTIVE) then
        begin
        s := 'FormWndProc ' +'msg.Msg'+ Inttostr(Amsg.Msg) +'msg.LParam'+ Inttostr(Amsg.LParam)
        +'msg.WParam '+ Inttostr(Amsg.WParam);
        OutputDebugString(PChar(s));
        end;
    }
  end;
  FOldWindowProc(AMsg);
end;

procedure TEmbeddedWB.HookParentFormWndProc;
begin
  if not bWndProcHooked and not (csDesigning in ComponentState) then
  begin
    FParentForm := TForm(Owner);
    if Assigned(FParentForm) and (FParentForm.HandleAllocated) then
    begin
      FOldWindowProc := FParentForm.WindowProc;
      FParentForm.WindowProc := FormWndProc;
      bWndProcHooked := True;
    end;
  end;
end;

procedure TEmbeddedWB.UnHookParentFormWndProc;
begin
  if bWndProcHooked and not (csDesigning in ComponentState) then
  begin
    if Assigned(FParentForm) and (FParentForm.HandleAllocated) then
      FParentForm.WindowProc := FOldWindowProc;
    FParentForm := nil;
    bWndProcHooked := False;
  end;
end;

{$ENDIF} // Enable_HookParentFormWndProc

{$IFDEF Enable_MouseEnterLeaveEvents}

procedure TEmbeddedWB.HandleMouseEnterLeaveEvents(var msg: TMessage);
begin
  case msg.Msg of
    // Doesn't work in D2005+
    CM_MOUSELEAVE:
      if Assigned(FOnMouseLeave) then
        FOnMouseLeave(Self);
    CM_MOUSEENTER:
      if Assigned(FOnMouseEnter) then
        FOnMouseEnter(Self);
  end;
end;
{$ENDIF}

function SysListView32MoveCursor(EmbeddedWB: TEmbeddedWB; Key: Word; Shift: TShiftState): Boolean;
// Make VK_UP, VK_DOWN, VK_LEFT, VK_RIGHT work in SysListView32
// To-do: make HOME, ENDE, PG UP, PG DOWN work
var
  iNextItem, iFocusedItem: Integer;
  ListView: hWnd;
  Flags: Word;
begin
  ListView := EmbeddedWB.GetIEWin('SysListView32');
  Result := ListView <> 0;
  if Result then
  begin
    case Key of
      VK_UP: Flags := LVNI_ABOVE;
      VK_DOWN: Flags := LVNI_BELOW;
      VK_LEFT: Flags := LVNI_TOLEFT;
      VK_RIGHT: Flags := LVNI_TORIGHT;
    else
      begin
        Result := False;
        Exit;
      end;
    end;
    iFocusedItem := ListView_GetNextItem(ListView, -1, LVNI_ALL or LVNI_FOCUSED);
    iNextItem := SendMessage(ListView, LVM_GETNEXTITEM, iFocusedItem, MakeLong(Flags, 0));
    if iNextItem <> -1 then
    begin
      if not (ssShift in Shift) then
        // unselect focused items if Shift is not pressed.
        // still not perfect (like the explorer's behaviour) but works fine so far.
      begin
        ListView_SetItemState(ListView, iFocusedItem, 0, LVIS_SELECTED);
        ListView_SetItemState(ListView, iFocusedItem, 0, LVIS_FOCUSED);
      end;
      ListView_SetItemState(ListView, iNextItem, LVIS_SELECTED, LVIS_SELECTED);
      ListView_SetItemState(ListView, iNextItem, LVIS_FOCUSED, LVIS_FOCUSED);
      ListView_EnsureVisible(ListView, iNextItem, False);
      SendMessage(ListView, WM_SETFOCUS, 0, 0);
    end;
  end
  else
    Result := False;
end;

procedure TEmbeddedWB.WndProc(var AMsg: TMessage);
var
  bProcessMessage: Boolean;
  bHandled: Boolean;
begin
{$IFDEF Enable_MouseEnterLeaveEvents}
  HandleMouseEnterLeaveEvents(msg);
{$ENDIF}

  bProcessMessage := True;

  // Handle WM_CLOSE
  if AMsg.Msg = WM_CLOSE then
  begin
    if Assigned(FOnCloseQuery) then
      FOnCloseQuery(Self, bProcessMessage);
    if bProcessMessage then
      if ((not Self.Focused) and (AMsg.Msg = CM_DIALOGKEY)) then
        bProcessMessage := False;
  end;

  // Handle OnKeyDown / OnKeyUp Messages
  with AMsg do
    case Msg of
      CN_BASE + WM_KEYDOWN, CN_BASE + WM_SYSKEYDOWN:
        begin
          if SysListView32MoveCursor(Self, TWMKeyUp(AMsg).CharCode, KeyDataToShiftState(TWMKeyUp(AMsg).KeyData)) then
          begin
            bProcessMessage := False;
            AMsg.Result := 1;
          end;
          if Assigned(FOnKeyDown) then
          begin
            FOnKeyDown(Self, TWMKeyDown(AMsg).CharCode, (TWMKeyDown(AMsg).KeyData shr 16) and $FF,
              KeyDataToShiftState(TWMKeyDown(AMsg).KeyData));
            if (TWMKeyDown(AMsg).CharCode = 0) then
            begin
              bProcessMessage := False;
              AMsg.Result := 1;
            end;
          end;
        end;
      CN_BASE + WM_KEYUP, CN_BASE + WM_SYSKEYUP:
        if Assigned(FOnKeyUp) then
        begin
          FOnKeyUp(Self, TWMKeyUp(AMsg).CharCode, (TWMKeyUp(AMsg).KeyData shr 16) and $FF,
            KeyDataToShiftState(TWMKeyUp(AMsg).KeyData));
          if TWMKeyUp(AMsg).CharCode = 0 then
          begin
            bProcessMessage := False;
            AMsg.Result := 1;
          end;
        end;
    end;

  if bProcessMessage then
  begin
    if not ((AMsg.msg = CM_DOCWINDOWACTIVATE) and (Forms.Application.Terminated)) then
      inherited WndProc(AMsg);
  end;

  // Make VK_RETURN work in TextAreas etc.
  if FEnableMessageHandler and (AMsg.Msg = CN_BASE + WM_KEYDOWN) and
    (AMsg.WParam = VK_RETURN) then
    AMsg.Result := 0;

  // Our own OnMessage Handler
  if Assigned(FOnMessage) then
  begin
    bHandled := False; // not used but don't break the interface
    FOnMessage(Self, AMsg, bHandled);
  end;

  // Handle other messges
  with AMsg do
  begin
    case WParam of
      WM_LBUTTONDOWN:
        if Assigned(FOnClick) then
          FOnClick(Self, mbLeft, KeysToShiftState(TWMMouse(AMsg).Keys), TWMMouse(AMsg).XPos, TWMMouse(AMsg).YPos);
      WM_RBUTTONDOWN:
        if Assigned(FOnClick) then
          FOnClick(Self, mbRight, KeysToShiftState(TWMMouse(AMsg).Keys), TWMMouse(AMsg).XPos, TWMMouse(AMsg).YPos);
    end;

    case Msg of
      WM_GETDLGCODE: // http://msdn.microsoft.com/en-us/library/ms645425.aspx
        begin
          Result := DLGC_WANTCHARS;
        end;
      WM_SIZE:
        if (not ((not Self.Focused) and (Msg = CM_DIALOGKEY))) then
        begin
          FResizing := not FWinXPSP2orLater; // must be set in constructor
        end;
      WM_WINDOWPOSCHANGING:
        begin
          if FResizing and ((PWindowPos(LParam)^.flags and (SWP_NOMOVE or SWP_NOSIZE)) > 0) then
          begin
            if ((PWindowPos(LParam)^.flags and SWP_NOMOVE) = 0) then
            begin
              PWindowPos(LParam)^.flags := PWindowPos(LParam)^.flags or SWP_NOMOVE;
              if Assigned(OnMove) then
                OnMove(Self, PWindowPos(lParam)^.x, PWindowPos(LParam)^.y);
            end;
            if ((PWindowPos(LParam)^.flags and SWP_NOSIZE) = 0) then
            begin
              PWindowPos(LParam)^.flags := PWindowPos(LParam)^.flags or SWP_NOSIZE;
              if Assigned(OnResize) then
                OnResize(Self, PWindowPos(LParam)^.cx, PWindowPos(LParam)^.cy);
            end;
          end;
        end;
{$IFDEF USE_EwbDDE}
      WM_DDE_EXECUTE: DDEExecute(WParam, LParam);
      WM_DDE_TERMINATE: DDETerminate(WParam, LParam);
{$ENDIF}
    end;
  end;
end;

//=== EmbeddedWB Functions and procedures ======================================

//=== override Visible Property

function TEmbeddedWB.GetVisible: Boolean;
begin
  Result := FVisible;
end;

procedure TEmbeddedWB.SetName(Value: string);
begin
  if Value <> FName then
  begin
    TControl(Self).Name := Value;
    FName := Value;
  end;
end;

//=== override Name Property

function TEmbeddedWB.GetName: string;
begin
  Result := TControl(Self).Name;
  // IWebBrowser.Get_Name would return "Microsoft Browser Control"
end;

//=== override HWND Property

function TEmbeddedWB.GetHWND: Integer;
begin
  if HandleAllocated then
  begin
    Result := GetIEWin('Internet Explorer_Server');
    if not IsWindow(Result) then
      Result := GetIEWin('SysListView32')
  end
  else
    Result := 0;
end;

//=== override Visible Property

procedure TEmbeddedWB.SetVisible(AValue: Boolean);
begin
  if AValue <> FVisible then
  begin
    FVisible := AValue;
    TControl(Self).Visible := FVisible;
  end;
end;

//=== override Silent Property

procedure TEmbeddedWB.SetSilent(Value: Boolean);
begin
  // FSilent := Value;
  SetWordBoolProp(551, FSilent);
end;

function TEmbeddedWB.GetSilent: Boolean;
begin
  Result := GetWordBoolProp(551)
end;

//=== override SetRegisterAsDropTarget Property

procedure TEmbeddedWB.SetRegisterAsDropTarget(Value: Boolean);
begin
  SetWordBoolProp(553, Value)
end;

function TEmbeddedWB.GetRegisterAsDropTarget: Boolean;
begin
  Result := GetWordBoolProp(553);
end;

//=== override SetRegisterAsBrowser Property

procedure TEmbeddedWB.SetRegisterAsBrowser(Value: Boolean);
begin
  SetWordBoolProp(552, Value)
end;

function TEmbeddedWB.GetRegisterAsBrowser: Boolean;
begin
  Result := GetWordBoolProp(552);
end;

function TEmbeddedWB.GetParent: TWinControl;
begin
  Result := TWinControl(Self).Parent;
end;

procedure TEmbeddedWB.SetParent(Control: TWinControl);
begin
  //  Windows.SetParent(Self.Handle, Control.Handle)
  TWinControl(Self).Parent := Control;
end;

function TEmbeddedWB.GetHostInfo(var pInfo: TDOCHOSTUIINFO): HRESULT;
begin
  Result := inherited GetHostInfo(pInfo);
  if (pInfo.chHostCss = nil) and (HostCSS <> '') then
    pInfo.chHostCss := TaskAllocWideString(HostCSS);
  if (pInfo.chHostNS = nil) and (HostNS <> '') then
    pInfo.chHostNS := TaskAllocWideString(FHostNS);
end;

class function TEmbeddedWB.GetIEHandle(WebBrowser: TEmbeddedWB; ClassName: string): HWND;
begin
  Result := WebBrowser.GetIEWin(ClassName);
end;

function TEmbeddedWB.GetModified: Boolean;
begin
  Result := FModified;
end;

procedure TEmbeddedWB.SetHTMLCode(value: TStringList);
begin
  if Assigned(Value) then
    FHTMLCode.Text := Value.Text;
end;

procedure TEmbeddedWB.HTMLCodeChanged(Sender: TObject);
var
  Protocol, SURL: string;
  bLoad, bHandled: Boolean;
begin
  if (csDesigning in ComponentState) then
    Exit;
  if HTMLCode.Count = 1 then
  begin
    bLoad := False;
    Protocol := LowerCase(System.Copy(HTMLCode[0], 1, 4));
    if (Protocol = 'http') or (Protocol = 'www.') or (Protocol = 'ftp:') or (Protocol = 'file') or
      (LowerCase(System.Copy(HTMLCode[0], 1, 6)) = 'about:') then
    begin
      SURL := HTMLCode[0];
      Go(SURL);
    end
    else
      bLoad := True;
  end
  else
    bLoad := True;
  if bLoad then
  begin
    bHandled := False;
    if Assigned(FOnHTMLCodeChange) then
      FOnHTMLCodeChange(Self, HTMLCode, bHandled);
    if not bHandled then
      if HTMLCode.Count <> 0 then
        LoadFromStrings(HTMLCode);
  end;
end;

function TEmbeddedWB.GetDocument: IHTMLDocument2;
begin
  Wait();
  Result := nil;
  if DocumentLoaded then
    Result := Self.ControlInterface.Document as IHTMLDocument2;
end;

function TEmbeddedWB.GetFrame(FrameNo: Word): IWebbrowser2;
var
  OleContainer: IOleContainer;
  Enum: ActiveX.IEnumUnknown;
  Unk: IUnknown;
  Fetched: PLongint;
begin
  Result := nil;
  if Assigned(Document) then
  begin
    Fetched := nil;
    OleContainer := Document as IOleContainer;
    OleContainer.EnumObjects(OLECONTF_EMBEDDINGS, Enum);
    Enum.Skip(FrameNo);
    Enum.Next(1, Unk, Fetched);
    if Supports(Unk, IWebBrowser2, Result) then //perva 2008/12/10
      Result := Unk as IWebbrowser2;
  end;
end;

function TEmbeddedWB.GetFrameFromDocument(SourceDoc: IHTMLDocument2;
  FrameNo: Integer): IWebBrowser2; //By Aladin
var //by Aladin
  OleContainer: IOleContainer;
  enum: ActiveX.IEnumUnknown;
  unk: IUnknown;
  Fetched: PLongint;
begin
  Result := nil;
  Fetched := nil;
  if DocumentLoaded then
  begin
    OleContainer := SourceDoc as IOleContainer;
    OleContainer.EnumObjects(OLECONTF_EMBEDDINGS or OLECONTF_OTHERS, Enum);
    Enum.Skip(FrameNo);
    Enum.Next(1, Unk, Fetched);
    Result := Unk as IWebBrowser2;
  end;
end;

function TEmbeddedWB.GetActiveFrame: IHTMLDocument2;
var
  HtmlDoc2: IHTMLDocument2;
begin
  if Supports(Self.Document, IHTMLDocument2, HtmlDoc2) then
    Result := GetActiveFrame(HtmlDoc2);
end;

function TEmbeddedWB.GetActiveFrame(Doc: IHTMLDocument2): IHTMLDocument2;
var
  Element: IHTMLElement;
  Frame: IHTMLFrameElement;
  WB: IWebBrowser2;
  HTMLDocument: IHTMLDocument2;
  HtmlDoc2: IHTMLDocument2;
begin
  Result := nil;
  if Supports(Doc, IHTMLDocument2, HtmlDoc2) then
  begin
    Element := Doc.activeElement;
    if Assigned(Element) then
      if Element.QueryInterface(IHTMLFrameElement, Frame) = S_OK then
        if Frame.QueryInterface(IID_IWebBrowser2, WB) = S_OK then
          if Assigned(WB.Document) then
            if WB.Document.QueryInterface(IID_IHTMLDocument2, HTMLDocument) = S_OK then
              Result := GetActiveFrame(HTMLDocument);
  end;
  if Result = nil then
    result := doc;
end;

function TEmbeddedWB.GetActiveElement(Doc: IHTMLDocument2 = nil): IHTMLElement;
begin
  Result := nil;
  if Doc = nil then
    Doc := Self.GetActiveFrame;
  if Assigned(Doc) then
    Result := Doc.activeElement;
end;

function TEmbeddedWB.GetSelectedText(ReturnAsHTML: Boolean = False; bOnlyTopLevel: Boolean = False): string;
var
  doc2: IHTMLDocument2;
  selobj: IHTMLSelectionObject;
  range: IHTMLTxtRange;
  Disp: IDispatch;
begin
  Result := '';
  if DocumentLoaded then
  begin
    if (bOnlyTopLevel) then
      doc2 := Self.ControlInterface.Document as IHTMLDocument2
    else
      doc2 := Self.GetActiveFrame();
    if Assigned(doc2) and Assigned(doc2.selection) and (doc2.readyState = 'complete') then
    begin
      selobj := doc2.selection as IHTMLSelectionObject;
      if Assigned(selobj) then
      begin
        if (selobj.type_ <> 'none') and (selobj.type_ <> 'control') then
        begin
          Disp := selobj.createRange;
          if Assigned(Disp) then
          begin
            range := Disp as IHTMLTxtRange;
            if Assigned(range) then
              if (ReturnAsHTML) then
                Result := range.htmlText
              else
                Result := range.text;
          end;
        end;
      end;
    end;
  end;
end;

function TEmbeddedWB.GetSelLength(bOnlyTopLevel: Boolean = False): Integer;
begin
  Result := Length(GetSelectedText(False, bOnlyTopLevel));
end;

procedure TEmbeddedWB.AssignEmptyDocument(bWait: Boolean = False);
begin
  Navigate('about:blank');
  if bWait then
    Wait;
end;

function TEmbeddedWB.DocumentLoaded(out Doc2: IHTMLDocument2): Boolean;
var
  iDoc: IHTMLDocument2;
begin
  Result := False;
  if Assigned(Document) then
  begin
    ControlInterface.Document.QueryInterface(IHTMLDocument2, iDoc);
    Doc2 := iDoc;
    Result := Assigned(iDoc);
  end;
end;

function TEmbeddedWB.DocumentLoaded(): Boolean;
var
  iDoc: IHTMLDocument2;
begin
  Result := False;
  if Assigned(Document) then
  begin
    ControlInterface.Document.QueryInterface(IHTMLDocument2, iDoc);
    Result := Assigned(iDoc);
  end;
end;

function TEmbeddedWB.GetPrintValues: Boolean;
const
  REG_PATH_PAGESETUP = 'Software\Microsoft\Internet Explorer\PageSetup';
var
  S: string;
  Reg: TRegistry;

  {$IFDEF DELPHI7_UP}
    FS: TFormatSettings;
  {$ENDIF}

  function ReadMargin(key: string): Real;
  begin
    S := Reg.ReadString(key);
    if S = '' then
      S := '0.750000'; // <-- default margin value  by takeru_tk_81
    S := StringReplace(S, ' ', '', [rfReplaceAll]);

    if {$IFDEF DELPHI7_UP}FS.{$ENDIF}DecimalSeparator <> '.' then
      S := StringReplace(S, '.',{$IFDEF DELPHI7_UP}FS.{$ENDIF}DecimalSeparator ,[]);

    if PrintOptions.Measure = mMetric then
      Result := StrToFloat(S) * InchToMetric
    else
      Result := StrToFloat(S);
  end;

begin
  Result := False;
  Reg := TRegistry.Create;
  try
    with Reg do
    begin
      RootKey := HKEY_CURRENT_USER;
      if OpenKey(REG_PATH_PAGESETUP, False) then
      begin
        with PrintOptions do
        begin
          Header := ReadString('header');
          Footer := ReadString('footer');
          Margins.Left := ReadMargin('margin_left');
          Margins.Right := ReadMargin('margin_right');
          Margins.Top := ReadMargin('margin_top');
          Margins.Bottom := ReadMargin('margin_bottom');
        end;
        Result := True;
      end;
      Reg.Free;
    end;
  except
    // MessageDlg('Error while getting page print values from the registry.', mtError, [mbOK], 0);
  end;
end;

function TEmbeddedWB.GetIEHomePage: string;
var
  IEHomePage: string;
begin
  IEHomePage := '';
  with TRegistry.Create do
    try
      RootKey := HKEY_CURRENT_USER;
      OpenKey('\Software\Microsoft\Internet Explorer\Main', False);
      IEHomePage := ReadString('Start Page');
      CloseKey;
    finally
      Free;
    end;
  Result := IEHomePage;
end;

function TEmbeddedWB.VariantIsObject(const Value: OleVariant): Boolean;
begin
  Result := (VarType(Value) = varDispatch);
end;

// Wait procedures -------------------------------------------------------------

function TEmbeddedWB.Wait(TimeOut: Longword = 0): Boolean; //by perva: Added OnBusyWait
var
  StartTime: DWORD;
  bCancel, bDoBreak: Boolean;
begin
  bDoBreak := False;
  if HandleAllocated then
  begin
    bCancel := False;
    bDoBreak := False;
    StartTime := GetTickCount;
    while Assigned(Self) and not FDestroying and (ReadyState <> READYSTATE_COMPLETE) do
    begin
      if Assigned(FOnBusyWait) then
        FOnBusyWait(Self, StartTime, TimeOut, bCancel);
      bDoBreak := FStopped or bCancel or ((TimeOut > 0) and ((GetTickCount - StartTime) > Timeout));
      if bDoBreak or Forms.Application.Terminated then
        Break;
      Forms.Application.ProcessMessages;
      Sleep(1);
    end;
  end;
  FStopped := False;
  Result := bDoBreak = False;
end;

function TEmbeddedWB.WaitWhileBusy(TimeOut: Longword = 0): Boolean;
var
  StartTime: DWORD;
  bCancel, bDoBreak: Boolean;
begin
  bCancel := False;
  bDoBreak := False;
  StartTime := GetTickCount;
  while not FDestroying and Busy do
  begin
    if Assigned(FOnBusyWait) then
      FOnBusyWait(Self, StartTime, TimeOut, bCancel);

    bDoBreak := FStopped or bCancel or ((TimeOut > 0) and ((GetTickCount - StartTime) > Timeout));
    if bDoBreak or Forms.Application.Terminated then
      Break;
    Forms.Application.ProcessMessages;
    Sleep(1);
  end;
  FStopped := False;
  Result := bDoBreak = False;
end;

function TEmbeddedWB.DownloadFile(SourceFile, DestFile: string): Boolean;
begin
  try
    Result := UrlDownloadToFile(nil, PChar(SourceFile), PChar(DestFile), 0, nil) = 0;
  except
    Result := False;
  end;
end;

// Navigating ------------------------------------------------------------------

function TEmbeddedWB.Go(Url: string; TimeOut: LongWord = 0): Boolean;
var
  ovURL, ovFlags, ovTargetFrameName, ovPostData, ovHeaders: OleVariant;
begin
  Result := True;
  if DirectoryExists(Url) then
    Navigate('file:///' + Url)
  else if (Trim(Url) <> '') then
  begin
    ovUrl := Url;
    ovFlags := 0;
    ovTargetFrameName := 0;
    ovPostData := 0;
    ovHeaders := 0;
    Navigate2(ovUrl, ovFlags, ovTargetFrameName, ovPostData, ovHeaders);
    if TimeOut <> 0 then
      Result := Wait(TimeOut)
  end;
end;

function TEmbeddedWB.NavigateWait(const URL: WideString; TimeOut: LongWord = 0): Boolean;
begin
  Navigate(URL);
  if TimeOut = 0 then
    Result := True
  else
    Result := Wait(TimeOut)
end;

procedure TEmbeddedWB.NavigateFolder(CSIDL: Integer);
var
  sFolder: PItemIdList;
begin
  SHGetSpecialFolderLocation(0, CSIDL, SFolder);
  NavigatePidl(SFolder);
  CoTaskMemFree(SFolder);
end;

procedure TEmbeddedWB.NavigatePidl(pidl: PItemIdList);
var
  VaEmpty, vaPidl: OleVariant;
  psa: PSafeArray;
  cbData: UINT;
begin
  cbdata := GetPidlSize(pidl);
  psa := SafeArrayCreateVector(VT_UI1, 0, cbData);
  if Assigned(psa) then
  begin
    CopyMemory(psa.pvData, pidl, cbData);
    VariantInit(vaPidl);
    TVariantArg(vaPidl).vt := VT_ARRAY or VT_UI1;
    TVariantArg(vaPidl).parray := psa;
    Navigate2(vaPidl, vaEmpty, vaEmpty, vaEmpty, vaEmpty);
    VariantClear(vaPidl);
  end;
end;

function TEmbeddedWB.NavigateToFrame(FrameList: string): IHTMLDocument2;
var
  Document: IHTMLDocument2;
  FramesIndexList: TStringList;
  i: Integer;
begin
  Result := nil;
  Document := GetDocument;
  if Assigned(Document) then
  begin
    FramesIndexList := TStringList.Create;
    try
      FramesIndexList.CommaText := FrameList; //move into the last frame
      for i := 0 to FramesIndexList.Count - 1 do
      begin
        Document := GetFrameFromDocument(Document, StrToInt(FramesIndexList[i])).Document as IHTMLDocument2;
        if not DocumentLoaded then
          Exit;
      end;
      Result := Document;
    finally
      FramesIndexList.Free;
    end;
  end;
end;

// Saving & Loading ------------------------------------------------------------

function TEmbeddedWB.SaveToFile(const FileName: string): HRESULT;
begin
  Wait();
  if Assigned(Document) then
    Result := SaveDocToFile(Document, FileName)
  else
    Result := S_FALSE;
end;

function TEmbeddedWB.SaveToStream(AStream: TStream): HRESULT;
begin
  Wait();
  if Assigned(Document) then
    Result := SaveDocToStream(Document, AStream)
  else
    Result := S_FALSE;
end;

function TEmbeddedWB.SaveToStrings(AStrings: TStrings): HRESULT;
begin
  Wait();
  if Assigned(Document) then
    Result := SaveDocToStrings(Document, AStrings)
  else
    Result := S_FALSE;
end;

function TEmbeddedWB.SaveFrameToFile(FrameNo: Word; const FName: string): HRESULT;
var
  Iw: IWebbrowser2;
  PFile: IPersistFile;
begin
  iw := GetFrame(frameNo);
  if Assigned(iw) and Assigned(iw.Document) then
  begin
    PFile := iw.Document as IPersistFile;
    Result := PFile.Save(StringToOleStr(FName), False);
  end
  else
    Result := S_FALSE;
end;

function TEmbeddedWB.SaveFrameToStream(FrameNo: Integer; AStream: TStream): HRESULT;
var
  iw: IWebbrowser2;
begin
  Result := S_FALSE;
  iw := GetFrame(frameNo);
  if Assigned(iw) and Assigned(iw.Document) then
    Result := SaveDocToStream(iw.Document, AStream)
end;

function TEmbeddedWB.SaveFrameToStrings(FrameNo: Integer; AStrings: TStrings): HRESULT;
var
  iw: Iwebbrowser2;
begin
  Result := S_FALSE;
  iw := GetFrame(frameNo);
  if Assigned(iw) and Assigned(iw.Document) then
    Result := SaveDocToStrings(iw.Document, AStrings);
end;

function TEmbeddedWB.LoadFromString(const St: string): HRESULT;
var
  Stream: TStringStream;
begin
  if not Assigned(Document) then
    AssignEmptyDocument;
  Stream := TStringStream.Create(St{$IFDEF DELPHI12_UP}, FEncoding{$ENDIF});
  try
    OleCheck((Document as IPersistStreamInit).Load(TStreamAdapter.Create(Stream)));
    Result := S_OK;
  finally
    FreeAndNil(Stream);
  end;
end;

{$IFDEF DELPHI6_UP}

function TEmbeddedWB.LoadFromWideString(const WideSt: WideString): HRESULT;
var
  Strings: TStringList;
begin
  if not Assigned(Document) then
    AssignEmptyDocument;
  Strings := TStringList.Create;
  try
    Strings.Text := WideSt;
    Result := LoadFromStrings(Strings);
  finally
    FreeAndNil(Strings);
  end;
end;
{$ENDIF}

function TEmbeddedWB.LoadFromFile(const FileName: string): HRESULT;
var
  Stream: TStream;
begin
  Stream := TFileStream.Create(FileName, fmOpenRead or fmShareDenyWrite);
  try
    Result := LoadFromStream(Stream);
  finally
    Stream.Free;
  end;
end;

procedure TEmbeddedWB.LoadFromImage(Image: TImage);
var
  Stm: TMemoryStream;
begin
  Stm := TMemoryStream.Create;
  try
    Image.Picture.Bitmap.SaveToStream(Stm);
    LoadFromStream(Stm);
  finally
    Stm.Free;
  end;
end;

function TEmbeddedWB.LoadFromStrings(const AStrings: TStrings): HRESULT;
var
  M: TMemoryStream;
begin
  if not Assigned(Document) then
    AssignEmptyDocument;
  M := TMemoryStream.Create;
  try
    AStrings.SaveToStream(M{$IFDEF DELPHI12_UP}, FEncoding{$ENDIF});
    Result := LoadFromStream(M);
  except
    Result := S_FALSE;
  end;
  M.Free;
end;

function TEmbeddedWB.LoadFromStrings(const AStrings: TStrings; AddHtmlTags: Boolean): HRESULT;

  function AnsiSameStr(const S1, S2: string): Boolean;
  begin
    Result := AnsiCompareStr(S1, S2) = 0;
  end;

  function AnsiStartsStr(const ASubText, AText: string): Boolean;
  begin
    Result := AnsiSameStr(ASubText, System.Copy(AText, 1, Length(ASubText)));
  end;

var
  M: TMemoryStream;
  stn: TStrings;
begin
  if not Assigned(Document) then
    AssignEmptyDocument;
  M := TMemoryStream.Create;
  try
    if AddHtmlTags and not AnsiStartsStr('<HTML>', UpperCase(AStrings.GetText)) then
    begin
      stn := TStringList.Create;
      try
        with stn do
        begin
          Add('<html>');
          Add('<body>');
          Add('<body bgcolor="#ffffff">');
          AddStrings(AStrings);
          Add('</body>');
          Add('</html>');
          SaveToStream(M{$IFDEF DELPHI12_UP}, FEncoding{$ENDIF});
        end;
      finally
        stn.Free;
      end;
    end
    else
      AStrings.SaveToStream(M{$IFDEF DELPHI12_UP}, FEncoding{$ENDIF});
    Result := LoadFromStream(M);
  finally
    M.Free;
  end;
end;

function TEmbeddedWB.LoadFromStream(const AStream: TStream): HRESULT;
begin
  if not Assigned(Document) then
    AssignEmptyDocument;
  AStream.Seek(0, 0);
  Result := (Document as IPersistStreamInit).Load(TStreamAdapter.Create(AStream));
end;

function TEmbeddedWB.LoadFrameFromStrings(FrameNo: Word; const AStrings: TStrings): HRESULT;
var
  iw: IWebbrowser2;
  M: TMemoryStream;
begin
  Result := S_FALSE;
  iw := GetFrame(FrameNo);
  if Assigned(iw) and Assigned(iw.Document) then
  begin
    M := TMemoryStream.Create;
    try
      AStrings.SaveToStream(M{$IFDEF DELPHI12_UP}, FEncoding{$ENDIF});
      M.Seek(0, 0);
      Result := (iw.Document as IPersistStreamInit).Load(TStreamAdapter.Create(M));
    except
      Result := S_FALSE;
    end;
    M.Free;
  end;
end;

function TEmbeddedWB.LoadFrameFromStream(FrameNo: Integer; AStream: TStream): HRESULT;
var
  iw: IWebBrowser2;
begin
  Result := S_FALSE;
  iw := GetFrame(frameNo);
  if Assigned(iw) then
    if Assigned(iw.Document) then
    begin
      AStream.Seek(0, 0);
      Result := (iw.Document as IPersistStreamInit).Load(TStreamadapter.Create(AStream));
    end;
end;

// Menu Commands OLECMDID ------------------------------------------------------
// http://msdn.microsoft.com/en-us/library/ms691264(VS.85).aspx

//"Cut", "Copy", "Paste", ...

function TEmbeddedWB.IsCommandEnabled(sCmdId: WideString): Boolean;
var
  HTMLdoc2: IHTMLDocument2;
begin
  HTMLdoc2 := doc2;
  if Assigned(HTMLdoc2) then
    Result := HTMLdoc2.queryCommandEnabled(sCmdId)
  else
    Result := False;
end;

// Command Identifiers: http://msdn.microsoft.com/en-us/library/ms533049(VS.85).aspx

function TEmbeddedWB.QueryCommandValue(sCmdId: WideString): OleVariant;
var
  HTMLdoc2: IHTMLDocument2;
begin
  HTMLdoc2 := doc2;
  if Assigned(HTMLdoc2) then
    Result := HTMLdoc2.queryCommandValue(sCmdId)
  else
    Result := False;
end;

function TEmbeddedWB.Copy: Boolean;
var
  vaIn, vaOut: OleVariant;
begin
  Result := InvokeCmd(False, OLECMDID_COPY, OLECMDEXECOPT_DODEFAULT, vaIn, vaOut) = S_OK;
  // to check: http://support.microsoft.com/?scid=kb;en-us;897285&x=16&y=15
end;

function TEmbeddedWB.Paste;
var
  vaIn, vaOut: OleVariant;
begin
  Result := InvokeCmd(False, OLECMDID_PASTE, OLECMDEXECOPT_DODEFAULT, vaIn, vaOut) = S_OK;
end;

function TEmbeddedWB.PasteSpecial;
var
  vaIn, vaOut: OleVariant;
begin
  Result := InvokeCmd(False, OLECMDID_PASTESPECIAL, OLECMDEXECOPT_DODEFAULT, vaIn, vaOut) = S_OK;
end;

function TEmbeddedWB.Delete;
var
  vaIn, vaOut: OleVariant;
begin
  Result := InvokeCmd(False, OLECMDID_DELETE, OLECMDEXECOPT_DODEFAULT, vaIn, vaOut) = S_OK;
end;

function TEmbeddedWB.SelectAll;
var
  vaIn, vaOut: OleVariant;
begin
  Result := InvokeCmd(False, OLECMDID_SELECTALL, OLECMDEXECOPT_DODEFAULT, vaIn, vaOut) = S_OK;
end;

function TEmbeddedWB.ClearSelection;
var
  vaIn, vaOut: OleVariant;
begin
  Result := InvokeCmd(False, OLECMDID_CLEARSELECTION, OLECMDEXECOPT_DODEFAULT, vaIn, vaOut) = S_OK;
end;

function TEmbeddedWB.Undo;
var
  vaIn, vaOut: OleVariant;
begin
  Result := InvokeCmd(False, OLECMDID_UNDO, OLECMDEXECOPT_DODEFAULT, vaIn, vaOut) = S_OK;
end;

function TEmbeddedWB.Redo;
var
  vaIn, vaOut: OleVariant;
begin
  Result := InvokeCmd(False, OLECMDID_REDO, OLECMDEXECOPT_DODEFAULT, vaIn, vaOut) = S_OK;
end;

function TEmbeddedWB.Cut;
var
  vaIn, vaOut: OleVariant;
begin
  Result := InvokeCmd(False, OLECMDID_CUT, OLECMDEXECOPT_DODEFAULT, vaIn, vaOut) = S_OK;
end;

// Navigation ------------------------------------------------------------------

procedure TEmbeddedWB.Stop;
begin
  FStopped := True;
  try
    DefaultInterface.Stop;
  except
  end;
end;

procedure TEmbeddedWB.GoBack;
begin
  try
    DefaultInterface.GoBack;
  except
  end;
end;

procedure TEmbeddedWB.GoForward;
begin
  try
    DefaultInterface.GoForward;
  except
  end;
end;

// PopupMenu Filter ------------------------------------------------------------

function TEmbeddedWB.FilterPopupMenu: Boolean;
begin
  Result := inherited FilterPopupMenu or (DisabledPopupMenuItems <> []);
end;

procedure TEmbeddedWB.DoFilterPopupMenu(Sender: TObject; ID: DWORD; Menu: HMENU; const Context: IDispatch);
const
  cmds: array[TIEPopupMenuItem] of UINT = (
    IDM_BACK, //rcsBack
    IDM_FORWARD, //rcsForward
    IDM_SAVEBACKGROUND, //rcsSavePageBkg
    IDM_SETASBACKGROUND, //rcsSetAsBkg
    IDM_COPYBACKGROUND, //rcsCopyBkg
    IDM_SETASDESKTOPITEM, //rcsSetAsDeskT
    IDM_SELECTALL, //rcsSelectAll
    IDM_PASTE, //rcsPaste
    IDM_CREATESHORTCUT, //rcsCreateSC
    IDM_ADDTOFAVOURITES, //rcsAddToFav
    IDM_ENCODING, //rcsEncoding
    IDM_REFRESH, //rcsRefresh
    IDM_VIEWSOURCE, //rcsViewSource
    IDM_PROPERTIES, //rcsProperties
    IDM_PRINT, //rcsPrint
    IDM_OPENINNEWWINDOW, //rcsOpenNWindow
    IDM_OPENLINK //rcsOpenLink
    );
var
  IEPopupMenuItem: TIEPopupMenuItem;
begin
  inherited;
  for IEPopupMenuItem := Low(TIEPopupMenuItem) to High(TIEPopupMenuItem) do
    if IEPopupMenuItem in DisabledPopupMenuItems then
      DeleteMenu(Menu, cmds[IEPopupMenuItem], MF_BYCOMMAND);
end;

// Miscellaneous ---------------------------------------------------------------

procedure TEmbeddedWB.RefreshFrame(FrameNo: Word);
var
  OleContainer: IOleContainer;
  Enum: ActiveX.IEnumUnknown;
  Unk: IUnknown;
  Fetched: PLongint;
begin
  Wait();
  if Assigned(Document) then
  begin
    Fetched := nil;
    OleContainer := Document as IOleContainer;
    if Assigned(OleContainer) then
    begin
      OleContainer.EnumObjects(OLECONTF_EMBEDDINGS, Enum);
      Enum.Skip(FrameNo);
      Enum.Next(1, Unk, Fetched);
      (Unk as IWebbrowser2).Refresh;
    end;
  end
end;

function TEmbeddedWB.InvokeCMD(InvokeIE: Boolean; Value1, Value2: Integer; var vaIn, vaOut: OleVariant): HRESULT;
const
  PGUIDs: array[Boolean] of PGUID = (nil, @CLSID_WebBrowser);
begin
  Result := InvokeCommand(PGUIDs[InvokeIE], Value1, Value2, vaIn, vaOut);
end;

procedure TEmbeddedWB.InvokeIEServerCommand(Cmd: Integer);
begin
  SendMessage(Handle, WM_COMMAND, Cmd, 0);
end;

procedure TEmbeddedWB.ShowImportExportFavoritesAndCookies;
begin
  InvokeIEServerCommand(ID_IE_FILE_IMPORTEXPORT);
end;

procedure TEmbeddedWB.SetModified(Value: Boolean);
var
  HTMLDoc2: IHTMLDocument2;
begin
  if ((not FModified) and (READYSTATE = READYSTATE_COMPLETE)) then
  begin
    HTMLDoc2 := doc2;
    if Assigned(HTMLDoc2) then
      FModified := HTMLdoc2.QueryCommandEnabled('Undo');
  end;
end;

procedure TEmbeddedWB.SetDesginMode(Value: Boolean);
const
  SwitchOnOff: array[Boolean] of string = ('Off', 'On');
var
  HTMLDoc2: IHTMLDocument2;
begin
  HTMLDoc2 := doc2;
  if Assigned(HTMLDoc2) then
    HTMLDoc2.designMode := SwitchOnOff[Value];
end;

function TEmbeddedWB.GetDesginMode: Boolean;
const
  SwitchOnOff: array[Boolean] of string = ('Off', 'On');
var
  HTMLDoc2: IHTMLDocument2;
  Status: string;
begin
  Result := False;
  HTMLDoc2 := doc2;
  if Assigned(HTMLDoc2) then
  begin
    Status := SwitchOnOff[HTMLDoc2.get_designMode = 'On'];
    Result := Status = 'On';
  end;
end;

function TEmbeddedWB.PrintMarginStr(M: Real): string;
begin
  if PrintOptions.Measure <> FRuntimeMeasure then
  begin
    if FRuntimeMeasure = mMetric then
      Result := FloatToStr(M * InchToMetric)
    else
      Result := FloatToStr(M / InchToMetric);
  end
  else
    Result := FloatToStr(M);
end;

procedure TEmbeddedWB.SetAbout(Value: string);
begin
  Exit;
end;

{$IFDEF Enable_EwbMSHTMLEvents}

procedure TEmbeddedWB.SetSinkComponent(Value: TMSHTMLHTMLDocumentEvents);
begin
  Exit;
end;
{$ENDIF}

// User Agent Stuff ------------------------------------------------------------

procedure TEmbeddedWB.RestoreUserAgentReg;
const
  REG_KEY_USERAGENT = '\Software\Microsoft\Windows\CurrentVersion\Internet Settings\User Agent';
begin
  if (FUserAgentReg <> '') and FUserAgentRegSet then
  begin
    if (csDesigning in ComponentState) then
    begin
      FUserAgentRegSet := False;
      FUserAgentReg := '';
    end
    else
      with TRegistry.Create do
      begin
        try
          RootKey := HKEY_CURRENT_USER;
          if OpenKey(REG_KEY_USERAGENT, False) then
          begin
            DeleteKey('Post Platform');
            OpenKey(REG_KEY_USERAGENT + '\Post Platform', True);
            WriteString(USER_AGENT_IE6, '');
            FUserAgentRegSet := False;
            FUserAgentReg := '';
          end;
        finally
          CloseKey;
          Free;
        end;
      end;
  end;
end;

procedure TEmbeddedWB.SetUserAgentReg;
begin
  if (FUserAgent <> FUserAgentReg) then
  begin
    RestoreUserAgentReg;
    FUserAgentReg := FUserAgent;
    if (not (csDesigning in ComponentState)) then
      with TRegistry.Create do
      begin
        try
          FUserAgentReg := USER_AGENT_IE6 + '(' + FUserAgentReg + ')';
          begin
            RootKey := HKEY_CURRENT_USER;
            try
              if OpenKey(USER_AGENT_PATH, True) then
              begin
                WriteString(FUserAgentReg, '');
                FUserAgentRegSet := True;
              end;
            finally
              CloseKey;
            end;
          end;
        finally
          Free;
        end;
      end;
  end;
end;

procedure TEmbeddedWB.SetUserAgentInt;
var
  Control: IOleControl;
begin
  if FUserAgent <> FUserAgentInt then
  begin
    RestoreUserAgentReg;
    if DefaultInterface.QueryInterface(IOleControl, Control) = 0 then
      with (Application as IOleControl) do
      begin
        FUserAgentInt := FUserAgent;
        Control.OnAmbientPropertyChange(DISPID_AMBIENT_USERAGENT);
        //_Release;  I4214   // I4214
      end;
  end;
end;

procedure TEmbeddedWB.UpdateUserAgent;
begin
  case FUserAgentMode of
    uaInternal: SetUserAgentInt;
    uaRegistry: SetUserAgentReg;
    uaDefault: RestoreUserAgentReg;
  end;
end;

procedure TEmbeddedWB.SetUserAgent(const Value: string);
begin
  if Value <> FUserAgent then
  begin
    FUserAgent := Value;
    UpdateUserAgent;
  end;
end;

procedure TEmbeddedWB.SetUserAgentMode(Value: TUserAgentMode);
begin
  if Value <> FUserAgentMode then
  begin
    FUserAgentMode := Value;
    UpdateUserAgent;
  end;
end;

function TEmbeddedWB.OnSetUserAgentEvent(var UserAgent: string): HRESULT;
begin
  Result := S_FALSE;
  UserAgent := '';
  if (uaInternal = FUserAgentMode) and (FUserAgent <> '') then
  begin
    Result := S_OK;
    UserAgent := FUserAgent;
  end;
end;

procedure TEmbeddedWB.ShowAboutBox;
begin
  try
    TEWB(Self).ShowAboutBox;
  except
    SendMessage(Self.GetIEWin('Shell DocObject View'), WM_COMMAND, 336 {ID_IE_HELP_ABOUTIE}, 0);
  end;
end;

//=== EwbTools =================================================================

{$IFDEF USE_EwbTools}

//Document and Frame -----------------------------------------------------------

function TEmbeddedWB.SearchNextText(const Value: string; Direction: TSearchDirections = sdDown;
  AutoSelect: Boolean = True): TSearchResults;
var
  Document2: IHTMLDocument2;
const
  ADirection: array[TSearchDirections] of Shortint = (+1, -1);
begin
  Result := [srNotFound];
  if AnsiCompareText(Value, FSearchText) <> 0 then
  begin
    FTextRange := nil;
    FSearchText := Value;
    FHTMLChar := #0;
  end;
  //
  if FSearchText = '' then
    Exit;
  //
  Document2 := GetDocument;
  if Document2 = nil then
    Exit;

  if not Assigned(FTextRange) then
    FTextRange := (Document2.body as IHTMLBodyElement).createTextRange;
  //
  if FHTMLChar <> #0 then
  begin
    if FHTMLChar <> Char(Ord(Direction) + 1) then
      FTextRange.move('character', 0)
    else
      case Direction of
        sdDown: FTextRange.move('character', 1);
        sdUp: FTextRange.move('character', -1);
      end;
  end
  else
  begin
    if FLastSearchDirection <> Direction then
      case FLastSearchDirection of
        sdDown: FTextRange.move('character', -1);
        sdUp: FTextRange.move('character', 1);
      end;
  end;
  //
  if FTextRange.findText(FSearchText, ADirection[Direction], 0) then
  begin
    FTextRange.scrollIntoView(True);
    if AutoSelect then
    begin
      try
        FTextRange.select;
        // Selecting a range sometimes fails
        // Maybe someone knows of a better way to check if a range is selectable?
      except
      end;
    end;
    FHTMLChar := Char(Ord(Direction) + 1);
    Result := [srFound];
  end
  else
    FHTMLChar := #0;
  if (FLastSearchDirection = Direction) and (FHTMLChar = #0) then
    Include(Result, srEndOf);
  FLastSearchDirection := Direction;
end;

function TEmbeddedWB.GetWordAtCursor(const X, Y: Integer): string;
begin
  Result := EwbTools.GetWordAtCursor(X, Y, Self);
end;

procedure TEmbeddedWB.ScrollToIDEx(ID: string);
begin
  EwbTools.ScrollToIDEx(ID, Self);
end;

procedure TEmbeddedWB.ScrollToID(ID: Integer);
begin
  EwbTools.ScrollToID(ID, Self);
end;

function TEmbeddedWB.HScrollBarVisible: Boolean;
var
  HScroll, VScroll: Boolean;
begin
  EwbTools.GetScrollbarVisibility(Self, HScroll, VScroll);
  Result := HScroll;
end;

function TEmbeddedWB.VScrollBarVisible: Boolean;
var
  HScroll, VScroll: Boolean;
begin
  EwbTools.GetScrollbarVisibility(Self, HScroll, VScroll);
  Result := VScroll;
end;

function TEmbeddedWB.HScrollBarPosition: Integer;
var
  ScrollPos: TPoint;
begin
  if EwbTools.GetScrollBarPosition(Self, ScrollPos) then
    Result := ScrollPos.X
  else
    Result := -1;
end;

function TEmbeddedWB.VScrollBarPosition: Integer;
var
  ScrollPos: TPoint;
begin
  if EwbTools.GetScrollBarPosition(Self, ScrollPos) then
    Result := ScrollPos.Y
  else
    Result := -1;
end;

function TEmbeddedWB.DocumentSource: string;
begin
  Result := EwbTools.DocumentSource(OleObject);
end;

function TEmbeddedWB.DocumentSourceText: string;
begin
  Result := EwbTools.DocumentSourceText(OleObject, Document);
end;

function TEmbeddedWB.AddHtmlToAboutBlank(StringToHtml: string): Boolean;
begin
  Result := EwbTools.AddHtmlToAboutBlank(Self, StringToHtml);
end;

function TEmbeddedWB.FrameCount: Longint;
begin
  Wait();
  Result := EwbTools.FrameCount(Document);
end;

function TEmbeddedWB.FrameCountFromDocument(SourceDoc:
  IHTMLDocument2): Integer; //By Aladin
begin
  Wait();
  Result := EwbTools.FrameCountFromDocument(SourceDoc);
end;

function TEmbeddedWB.GetCookie: string;
begin
  Result := Cookie;
end;

//Document Operations ----------------------------------------------------------

procedure TEmbeddedWB.ScrollToTop;
begin
  Wait();
  EwbTools.ScrollToTop(OleObject);
end;

procedure TEmbeddedWB.ScrollToBottom;
begin
  Wait();
  EwbTools.ScrollToBottom(Document);
end;

procedure TEmbeddedWB.ScrollToPosition(X, Y: Integer);
begin
  Wait();
  EwbTools.ScrollToPosition(OleObject, X, Y);
end;

function TEmbeddedWB.SetCharartersSet(const ACharactersSet: string; Refresh: Boolean = True): Boolean;
begin
  Result := EwbTools.SetCharartersSet(Self, Document, ACharactersSet);
end;

procedure TEmbeddedWB.GetThumbnail(var Image: TImage);
begin
  EwbTools.GetThumbnail(Application, Image)
end;

function TEmbeddedWB.GetBmpFromBrowser(FileName: string): Boolean;
begin
  Result := EwbTools.GetBmpFromBrowser(Document, Self.Handle, Self.Width, Self.Height, FileName);
end;

function TEmbeddedWB.GetJPEGfromBrowser(FileName: string; SourceHeight, SourceWidth,
  TargetHeight, TargetWidth: Integer): Boolean;
begin
  Result := EwbTools.GetJPEGfromBrowser(Document, ControlInterface, FileName, SourceHeight,
    SourceWidth, TargetHeight, TargetWidth);
end;

//View Document Fields/Properties/Images ---------------------------------------

procedure TEmbeddedWB.ViewPageSourceText;
begin
  EwbTools.ViewPageSourceText(OleObject, Document);
end;

procedure TEmbeddedWB.ViewPageSourceTextToStrings(TextList: TStrings);
begin
  Wait();
  EwbTools.ViewPageSourceTextToStrings(OleObject, Document, TextList);
end;

procedure TEmbeddedWB.ViewPageSourceHTMLToStrings(HtmlList: TStrings);
begin
  Wait();
  EwbTools.ViewPageSourceHTMLToStrings(OleObject, Document, HtmlList);
end;

procedure TEmbeddedWB.ViewPageLinksToStrings(LinksList: TStrings);
begin
  EwbTools.ViewPageLinksToStrings(OleObject, LinksList);
end;

//Printing ---------------------------------------------------------------------

procedure TEmbeddedWB.Print;
begin
  Wait();
  if FPrintOptions.Enabled then
    EwbTools.Print(ControlInterface, FPrintOptions.HideSetup)
  else
    EwbTools.Print(ControlInterface, FPrintOptions.HideSetup, True, FPrintOptions.Header, FPrintOptions.Footer)
end;

procedure TEmbeddedWB.PrintWithOptions;
begin
  Wait();
  bPrintOptionsEnable := True;
  EwbTools.PrintWithOptions(ControlInterface, Document, True, FPrintOptions.FEnabled,
    FPrintOptions.HideSetup, bInvokingPageSetup);
end;

procedure TEmbeddedWB.PrintPreview;
begin
  EwbTools.PrintPreview(ControlInterface);
end;

procedure TEmbeddedWB.PrintPreviewExtended(nCMDShow: Integer);
begin
  EwbTools.PrintPreviewExtended(ControlInterface, nCMDShow, FPrintOptions.HideSetup);
  bPrintOptionsEnable := True;
end;

function TEmbeddedWB.PageSetup(UsePrintOptions: Boolean): Boolean;
begin
  Wait();
  Result := EwbTools.PageSetup(Document, UsePrintOptions, FPrintOptions.FEnabled, bInvokingPageSetup);
  bPrintOptionsEnable := True;
end;

procedure TEmbeddedWB.PrintSetup;
begin
  Wait();
  EwbTools.PrintSetup(ControlInterface, FPrintOptions.HideSetup);
end;

procedure TEmbeddedWB.PrintPreviewFromTemplate(const TemplateFileName: string);
begin
  EwbTools.PrintPreviewFromTemplate(TemplateFileName, Document);
end;

//Dialogs ---------------------------------------------------------------------

function TEmbeddedWB.OpenDialog: Boolean;
begin
  Result := EwbTools.OpenDialog(Self, Self);
end;

function TEmbeddedWB.SaveDialog: Boolean;
begin
  Result := EwbTools.SaveDialog(Document);
end;

function TEmbeddedWB.SaveDialogEx(AFilter: string = ''; ATitle: string = ''): string;
begin
  Result := EwbTools.SaveDialog(Self, Self, ATitle, AFilter);
end;

function TEmbeddedWB.ShowInternetOptions: Boolean;
begin
  Result := EwbTools.ShowInternetOptions(Document);
end;

function TEmbeddedWB.ShowPageProperties: Boolean;
begin
  Result := EwbTools.ShowPageProperties(Document);
end;

function TEmbeddedWB.ShowOrganizeFavorites: Boolean;
begin
  Result := EwbTools.ShowOrganizeFavorites(Handle);
end;

function TEmbeddedWB.ShowFindDialog: Boolean;
begin
  Result := EwbTools.ShowFindDialog(Document)
end;

procedure TEmbeddedWB.SaveImagesDialog;
begin
  EwbTools.SaveImagesDialog(OleObject, Document);
end;

function TEmbeddedWB.ViewPageSourceHtml: Boolean;
begin
  Result := EwbTools.ViewPageSourceHtml(Document);
end;

procedure TEmbeddedWB.SavePageTextDialog;
begin
  Wait();
  EwbTools.SavePageTextDialog(Self, OleObject, Document);
end;

//Open external programs -------------------------------------------------------

function TEmbeddedWB.OpenClient(Client: string): Boolean;
begin
  Result := EwbTools.OpenClient(Client);
end;

procedure TEmbeddedWB.ExploreFolder(Path: string);
begin
  EwbTools.DoExploreFolder(Handle, Path);
end;

procedure TEmbeddedWB.OpenIEBrowserWithAddress;
begin
  EwbTools.OpenIEBrowserWithAddress(Handle);
end;

//Open specific webpages -------------------------------------------------------

procedure TEmbeddedWB.GoSearchInGoogle(SearchTerm: string);
begin
  EwbTools.GoSearchInGoogle(Self, SearchTerm)
end;

procedure TEmbeddedWB.GoSearchInMSN(SearchTerm: string);
begin
  EwbTools.GoSearchInMSN(Self, SearchTerm)
end;

procedure TEmbeddedWB.GoSearchInYahoo(SearchTerm: string);
begin
  EwbTools.GoSearchInYahoo(Self, SearchTerm)
end;

//Navigatetml & Download ----------------------------------------------------------

procedure TEmbeddedWB.GoDownloadFile(URL: string);
begin
  EwbTools.GoDownloadFile(Self, URL);
end;

procedure TEmbeddedWB.GoDownloadMaskedFile(SourceFile, TargetFile: string; Notify: Boolean);
begin
  EwbTools.GoDownloadMaskedFile(SourceFile, TargetFile, Notify);
end;

procedure TEmbeddedWB.GoWithQueryDetails(Url, Query: string);
begin
  EwbTools.GoWithQueryDetails(Self, Url, Query);
end;

procedure TEmbeddedWB.GoNoHistory(const URL: string);
begin
  EwbTools.GoNoHistory(Self, URL);
end;

procedure TEmbeddedWB.GoAboutBlank;
begin
  EwbTools.GoAboutBlank(Self);
end;

//Get Special Folders/URL paths etc.--------------------------------------------

function TEmbeddedWB.GetCookiesPath: string;
begin
  Result := EwbTools.GetCookiesPath;
end;

function TEmbeddedWB.GetHistoryPath: string;
begin
  Result := EwbTools.GetHistoryPath;
end;

function TEmbeddedWB.GetFavoritesPath: string;
begin
  Result := EwbTools.GetFavoritesPath;
end;

function TEmbeddedWB.GetDefaultBrowserFromRegistry: string;
begin
  Result := EwbTools.GetDefaultBrowserFromRegistry;
end;

function TEmbeddedWB.GetSpecialFolderPath(CallerHandle: THandle; CSIDL: Integer): PChar;
begin
  Result := EwbTools.GetSpecialFolderPath(CallerHandle, CSIDL);
end;

function TEmbeddedWB.GetCachedFileFromURL(ItemUrl: string): string;
begin
  Result := EwbTools.GetCachedFileFromURL(ItemUrl);
end;

function TEmbeddedWB.URLFromFavorites(const dotURL: string): string;
begin
  Result := EwbTools.URLFromFavorites(dotURL);
end;

function TEmbeddedWB.UrlFromHistory(ShellFolder: IShellFolder; pidl: PItemIDList): string;
begin
  Result := EwbTools.UrlFromHistory(ShellFolder, pidl)
end;

function TEmbeddedWB.GetIPAndHostName(var HostName, IPaddr, WSAErr: string): Boolean;
begin
  Result := EwbTools.GetIPAndHostName(HostName, IPaddr, WSAErr);
end;

//E-Mail functions--------------------------------------------------------------

procedure TEmbeddedWB.SendPageInMailAsAttachment(aOwner: TComponent; FileName, Subject, Body: string);
begin
  EwbTools.SendPageInMailAsAttachment(Self, aOwner, Document, FileName, Subject, Body);
end;

procedure TEmbeddedWB.CreateNewMail;
begin
  EwbTools.CreateNewMail;
end;

procedure TEmbeddedWB.SendUrlInMail;
begin
  EwbTools.SendUrlInMail(LocationURL, LocationName);
end;

//Search in Document & Fill Forms-----------------------------------------------

function TEmbeddedWB.SearchText(const Value: string; const iPos: Integer = 1): IHTMLTxtRange;
begin //by JJM
  Result := EwbTools.SearchText(Self, Document, Value, iPos);
end;

function TEmbeddedWB.SearchString(const strText: string): Boolean;
begin
  Result := EwbTools.SearchString(Self, strText);
end;

procedure TEmbeddedWB.SearchAndHighlight(AText: string; const ACaption, APrompt: string;
  Flags: TSearchFlags = []; cbackColor: string = 'yellow'; cForeColor: string = '';
  ScrollIntoView: TScrollIntoView = sivNoScroll);
begin
  Wait();
  EwbTools.SearchAndHighlight(Document, AText, ACaption, APrompt, Flags,
    cbackColor, cForeColor, ScrollIntoView, True);
end;

procedure TEmbeddedWB.SearchAndHighlight(AText: string; Flags: TSearchFlags = [];
  cbackColor: string = 'yellow'; cForeColor: string = '';
  ScrollIntoView: TScrollIntoView = sivNoScroll);
begin
  Wait();
  EwbTools.SearchAndHighlight(Document, AText, '', '', Flags, cbackColor, cForeColor, ScrollIntoView, False);
end;

function TEmbeddedWB.FillForm(FieldName: string; Value: string; ElementNr: Integer = -1): Boolean;
begin
  Result := EwbTools.FillForm(Self, FieldName, Value, ElementNr)
end;

function TEmbeddedWB.GetFieldValue(FieldName: string): string;
begin
  Result := EwbTools.GetFieldValue(OleObject, FieldName)
end;

procedure TEmbeddedWB.FillFormAndExcecute;
begin
  EwbTools.FillIEFormAndExcecute
end;

//Clearing Cache/History/Typed ULRS---------------------------------------------

procedure TEmbeddedWB.ClearCache;
begin
  EwbTools.ClearCache;
end;

procedure TEmbeddedWB.ClearTypedUrls;
begin
  EwbTools.ClearTypedUrls;
end;

procedure TEmbeddedWB.ClearHistory;
begin
  EwbTools.ClearHistory;
end;

//Online Status-----------------------------------------------------------------

function TEmbeddedWB.CheckOnlineStatus: Boolean;
begin
  Result := EwbTools.CheckOnlineStatus;
end;

function TEmbeddedWB.IsGlobalOffline: Boolean;
begin
  Result := EwbTools.IsGlobalOffline;
end;

procedure TEmbeddedWB.WorkOnline;
begin
  EwbTools.WorkOnline();
end;

procedure TEmbeddedWB.WorkOffline;
begin
  EwbTools.WorkOffline();
end;

//Restricted & Trusted Lists----------------------------------------------------

procedure TEmbeddedWB.AddToRestrictedSiteList(URL: string);
begin
  EwbTools.AddToRestrictedSiteList(Self, URL);
end;

procedure TEmbeddedWB.AddToTrustedSiteList(URL: string);
begin
  EwbTools.AddToTrustedSiteList(Self, URL);
end;

function TEmbeddedWB.CheckIfInTrustedList(const Host: string; SecureSite: Boolean): Boolean;
begin
  Result := EwbTools.CheckIfInTrustedList(Host, SecureSite);
end;

function TEmbeddedWB.CheckIfInRestricredList(Host: string; SecureSite: Boolean): Boolean;
begin
  Result := EwbTools.CheckIfInRestricredList(Host, SecureSite);
end;

//Zone Icon, Security Zone, SSL Status  ----------------------------------------

function TEmbeddedWB.ImportCertFile(FileName, StoreType: string): Boolean;
begin
  Result := EwbTools.ImportCertFile(FileName, StoreType);
end;

procedure TEmbeddedWB.GetZoneIcon(IconPath: string; var Icon: TIcon);
begin
  EwbTools.GetZoneIcon(IconPath, Icon);
end;

function TEmbeddedWB.GetZoneAttributes(const URL: string): TZoneAttributes;
begin
  Result := EwbTools.GetZoneAttributes(LocationURL);
end;

function TEmbeddedWB.GetZoneIconToForm: Boolean;
begin
  Result := EwbTools.GetZoneIconToForm(LocationURL, Caption, Hint);
end;

function TEmbeddedWB.GetSSLStatus(var SSLName, SSLDescription: string): Boolean;
begin
  Result := EwbTools.GetSSLStatus(OleObject, LocationURL, SSLName, SSLDescription);
end;

function TEmbeddedWB.GetUrlSecurityZone(var ZoneName, ZoneDescription: string; var Icon: TIcon): Boolean;
begin
  Result := EwbTools.GetUrlSecurityZone(LocationURL, ZoneName, ZoneDescription, Icon);
end;

//Proxy ------------------------------------------------------------------------

function TProxySettings.SetProxy(UserAgent, Address, Bypass: string): Boolean;
begin
  Result := EwbTools.SetProxy(UserAgent, Address, Bypass);
  bProxy := Result;
end;

function TProxySettings.SetProxy(UserAgent, Address, UserName, Password: string; Port: Integer): Boolean;
begin
  Result := EwbTools.SetProxy(UserAgent, Address, UserName, Password, Port);
  bProxy := Result;
end;

function TProxySettings.SetProxyFromPAC(UserAgent, PACFile: string): Boolean;
begin
  Result := EwbTools.SetProxyFromPAC(UserAgent, PACFile);
  bProxy := Result;
end;

procedure TEmbeddedWB.RefreshProxy;
begin
  if FProxySettings.FUserName = '' then
    FProxySettings.SetProxy(FProxySettings.FUserAgent, FProxySettings.FAddress +
      ':' + IntToStr(FProxySettings.FPort), FProxySettings.FBypass)
  else
    FProxySettings.SetProxy(FProxySettings.FUserAgent, FProxySettings.FAddress,
      FProxySettings.FUserName, FProxySettings.FPassword, FProxySettings.FPort);
end;

//Miscellaneous ----------------------------------------------------------------

procedure TEmbeddedWB.RestoreApplicationFormSize;
begin
  EwbTools.RestoreApplicationFormSize(Self);
end;

procedure TEmbeddedWB.SaveApplicationFormSize;
begin
  EwbTools.SaveApplicationFormSize(Self);
end;

procedure TEmbeddedWB.ShowIEVersionInfo;
begin
  EwbTools.ShowIEVersionInfo(Handle);
end;

procedure TEmbeddedWB.SetNewHomePage(HomePage: string);
begin
  EwbTools.SetNewHomePage(HomePage);
end;

function TEmbeddedWB.GetLastVisitedPage(var LastVisitedPage: string): Boolean;
begin
  Result := EwbTools.GetLastVisitedPage(LastVisitedPage);
end;

function TEmbeddedWB.SaveLastVisitedPage: Boolean;
begin
  Result := EwbTools.SaveLastVisitedPage(Self, LocationURL);
end;

procedure TEmbeddedWB.AddToFavorites(URL, Title: string);
begin
  inherited;
  EwbTools.AddToFavorites(URL, Title);
end;

procedure TEmbeddedWB.CreateDesktopShortcut;
begin
  EwbTools.CreateDesktopShortcut(Handle);
end;

procedure TEmbeddedWB.DisableNavSound(bDisable: Boolean);
begin
  if GetIEVersionMajor >= 7 then
    CoInternetSetFeatureEnabled(FEATURE_DISABLE_NAVIGATION_SOUNDS, FEATURE_FROM_PROCESS, bDisable)
  else
    EwbTools.DisableNavSound(bDisable);
end;

procedure TEmbeddedWB.ExecScript(sExpression, sLanguage: string);
// e.g. sLanguage = 'JavaScript';
begin
  EwbTools.ExecScript(Self, sExpression, sLanguage);
end;

function TEmbeddedWB.ExecScriptEx(MethodName: string; ParamValues: array of const): OleVariant;
begin
  Result := EwbTools.ExecScriptEx(Self, MethodName, ParamValues);
end;

function TEmbeddedWB.IsValidProtocol(const URL: string): Boolean;
begin
  Result := EwbTools.IsValidProtocol(URL);
end;

function TEmbeddedWB.DecodeUrl(const InputStr: string): string;
begin
  Result := EwbTools.DecodeUrl(InputStr);
end;

function TEmbeddedWB.EncodeUrl(const InputStr: string; const bQueryStr: Boolean): string;
begin
  Result := EwbTools.EncodeUrl(InputStr, bQueryStr);
end;

{$ENDIF} // USE_EwbTools

//=== OnCreate / OnDestroy / Loaded / LoadSettings  ============================

constructor TEmbeddedWB.Create(Owner: TComponent);
begin
  inherited;
  {---------------------------------------------}
{$IFDEF Enable_EwbMSHTMLEvents}
  FSinkComponent := TMSHTMLHTMLDocumentEvents.Create(Self);
{$ENDIF}
  {---------------------------------------------}
  FAbout := EWB_INFO;
  FVisible := True;
  FSilent := True;
  {---------------------------------------------}
  FPrintOptions := TPrintOptions.Create;
  FPrintOptions.Margins := TMargins.Create;
  FPrintOptions.FHTMLHeader := TStringlist.Create;
  FPrintOptions.FHTMLHeader.Add('<HTML></HTML>');
  {---------------------------------------------}
  FProxySettings := TProxySettings.Create;
  FProxySettings.FPort := 80;
  {---------------------------------------------}
  FDisableErrors := TDisableErrors.Create;
  FDisableErrors.FEnableDDE := True;
  FDisableErrors.FfpExceptions := True;
  FDisableErrors.FScriptErrorsSuppressed := True;
  {---------------------------------------------}
  FDialogBoxes := TDialogBoxes.Create;
  FDialogBoxes.FReplaceCaption := True;
  FDialogBoxes.FReplaceIcon := True;
  {---------------------------------------------}
  FVisualEffects := TVisualEffects.Create;
  FVisualEffects.FTextSize := 2;
  {---------------------------------------------}
  FWinXPSP2orLater := IsWinXPSP2OrLater;
  FResizing := not FWinXPSP2orLater;
  FSearchText := '';
  FHTMLChar := #0;
  FStopped := False;
  {---------------------------------------------}
  FUserAgent := '';
  FUserAgentInt := '';
  FUserAgentReg := '';
  {---------------------------------------------}
{$IFDEF DELPHI12_UP}
  Encoding := TEncoding.Default;
{$ENDIF}
  FTextRange := nil;
  FHTMLCode := TStringList.Create;
  FEnableMessageHandler := True;
  FWndProcSubClassed := False;
  FOnSetUserAgent := OnSetUserAgentEvent;
  FOnHookChildWindow := OnHookChildWindows;
{$IFDEF Enable_SubClassChildWindows}
  InitEWBChildHook;
{$ENDIF}
  FDestroying := False;
end;

destructor TEmbeddedWB.Destroy;
begin
  FDestroying := True;
  FPrintOptions.HTMLHeader.Free;
  FPrintOptions.Margins.Free;
  FreeAndNil(FPrintOptions);
  FreeAndNil(FDialogBoxes);
  FreeAndNil(FDisableErrors);
  FreeAndNil(FProxySettings);
  FreeAndNil(FHTMLCode);
{$IFDEF USE_EwbMSHTMLEvents}
  FSinkComponent.OnMouseDown := nil;
  FreeAndNil(FSinkComponent);
{$ENDIF}
{$IFDEF USE_EwbTools}
  if (FVisualEffects.FTextSize <> 2) and (not (csDesigning in ComponentState)) then
    Zoom := 2;
{$ENDIF}
  FTextRange := nil;
  FreeAndNil(FVisualEffects);
  RestoreUserAgentReg;
{$IFDEF Enable_HookParentFormWndProc}
  UnHookParentFormWndProc;
{$ENDIF}
  inherited;
end;

procedure TEmbeddedWB.LoadSettings;
begin
  if not (csDesigning in ComponentState) then
  begin
    Modified := False;
    FRuntimeMeasure := GetRunTimeMeasure;
    FEnableMessageHandler := True;
{$IFDEF USE_EwbDDE}
{$IFDEF DELPHI6_UP}
{$WARN SYMBOL_DEPRECATED OFF}
{$ENDIF}
    if DDEHWND = 0 then
    begin
      GetDDEVariables;
      DDEHWND := AllocateHWnd(DDEWndProc);
    end;
{$IFDEF DELPHI6_UP}
{$WARN SYMBOL_DEPRECATED ON}
{$ENDIF}{$ENDIF}
    if not FWndProcSubClassed then
    begin
      WindowProc := WndProc;
      FWndProcSubClassed := True;
    end;
    if not Assigned(SecurityManager) then
    begin
      CoInternetCreateSecurityManager(Self, SecurityManager, 0);
      CoInternetCreateZoneManager(Self, ZoneManager, 0);
    end;
    if FDisableErrors.FScriptErrorsSuppressed then
      ScriptErrorAction := eaContinue;
    FDisableErrors.SetfpExceptions(FDisableErrors.FfpExceptions);
{$IFDEF Enable_HookParentFormWndProc}
    HookParentFormWndProc;
{$ENDIF}

{$IFDEF USE_EwbTools}
    if (FProxySettings.FAutoLoadProxy) then
      RefreshProxy;
    if FVisualEffects.FDisableSounds then
    begin
      DisableNavSound(True);
      bNavSound := True;
    end;
{$ENDIF}
  end
  else
  begin
    FPrintOptions.FMeasure := FRunTimeMeasure;
    GetPrintValues;
  end;
  HTMLCodeChanged(Self);
  FHTMLCode.OnChange := HTMLCodeChanged;
end;

procedure TEmbeddedWB.Loaded;
begin
  inherited Loaded;
  LoadSettings;
end;

//=== Initialization & Finalization ============================================

procedure DoInitialization;
begin
{$WARN SYMBOL_PLATFORM OFF}
  wSaved8087CW := Default8087CW;
{$WARN SYMBOL_PLATFORM DEFAULT}
  bPrintOptionsEnable := False;
  bProxy := False;
  bNavSound := False;
  bOleInitialize := OleInitialize(nil) = S_OK;

{$IFDEF AutoUse_EwbControl}
  EwbControl := TEwbControl.Create(nil);
{$IFDEF Enable_AutoFocusControl}
  EwbControl.FocusControl.Active := True;
{$ENDIF Enable_AutoFocusControl}

{$IFDEF Enable_AutoMouseWheelFix}
  EwbControl.MouseWheelFix.Active := True;
{$ENDIF Enable_AutoMouseWheelFix}

{$ENDIF AutoUse_EwbControl}
end;

procedure DoFinalization;
begin
{$IFDEF USE_EwbDDE}
{$IFDEF DELPHI6_UP}
{$WARN SYMBOL_DEPRECATED OFF}
  if DDEHWND <> 0 then
    DeAllocateHWND(DDEHWND);
{$WARN SYMBOL_DEPRECATED ON}
{$ENDIF}
{$ENDIF}

{$IFDEF USE_EwbTools}
  if bProxy then
    RemoveProxy;
  if bNavSound then
    DisableNavSound(False);
{$ENDIF}

  Set8087CW(wSaved8087CW);
  if bOleInitialize then
    try
      OleUninitialize;
    except
    end;

{$IFDEF Enable_AutoFocusControl}
  if Assigned(EwbControl) then
    EwbControl.Free;
{$ENDIF}
end;

initialization
  DoInitialization;
finalization
  DoFinalization;
end.

