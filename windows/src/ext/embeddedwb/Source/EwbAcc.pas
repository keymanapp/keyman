//**************************************************************
//                                                             *
//                             EwbAcc                          *                                                      *
//                           For Delphi                        *
//                            by bsalsa                        *
//                        bsalsa@gmail.com                     *
//  Updated versions:                                          *
//               http://www.bsalsa.com                         *
//**************************************************************

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
4. Please,  consider donation in our web site!
{*******************************************************************************}


unit EwbAcc;

interface

{$I EWB.inc}

uses
  ActiveX, SysUtils, ShlObj, Windows, UrlMon, EwbIEConst;

type
  IObjectIdentity = interface
    ['{CA04B7E6-0D21-11d1-8CC5-00C04FC2B085}']
    function IsEqualObject(unk: IUnknown): HRESULT; stdcall;
  end;

  type
  PInternetPerConnOption = ^INTERNET_PER_CONN_OPTION;
  INTERNET_PER_CONN_OPTION = record
    dwOption: DWORD;
    case Integer of
      0: (dwValue: DWORD);
      1: (pszValue:LPTSTR);
      2: (ftValue: FILETIME);
    end;

  PInternetPerConnOptionList = ^INTERNET_PER_CONN_OPTION_LIST;
  INTERNET_PER_CONN_OPTION_LIST = record
    dwSize        :DWORD;
    pszConnection :LPTSTR;
    dwOptionCount :DWORD;
    dwOptionError :DWORD;
    pOptions      :PInternetPerConnOption;
  end;

  LPINTERNET_PER_CONN_OPTION = ^INTERNET_PER_CONN_OPTION;
  LPINTERNET_PER_CONN_OPTION_LIST = ^INTERNET_PER_CONN_OPTION_LIST;

type
  BrowserNavConstants = TOleEnum;
  STATURL = record
    cbSize: DWORD;
    pwcsUrl: DWORD;
    pwcsTitle: DWORD;
    ftLastVisited: FILETIME;
    ftLastUpdated: FILETIME;
    ftExpires: FILETIME;
    dwFlags: DWORD;
  end;

  OSVERSIONINFOEX = packed record
    dwOSVersionInfoSize: DWORD;
    dwMajorVersion: DWORD;
    dwMinorVersion: DWORD;
    dwBuildNumber: DWORD;
    dwPlatformId: DWORD;
    szCSDVersion: array[0..127] of Char;
    wServicePackMajor: WORD;
    wServicePackMinor: WORD;
    wSuiteMask: WORD;
    wProductType: Byte;
    wReserved: Byte;
  end;
  TOSVersionInfoEx = OSVERSIONINFOEX;
  POSVersionInfoEx = ^TOSVersionInfoEx;

  IZoomEvents = interface(IUnknown)
    ['{41B68150-904C-4E17-A0BA-A438182E359D}']
    function OnZoomPercentChanged(const ulZoomPercent: ulong): HRESULT; stdcall;
  end;

  IHLinkFrame = interface(IUnknown)
    ['{79EAC9C5-BAF9-11CE-8C82-00AA004BA90B}']
    {Some functions are not supported any more?}
    //function SetBrowseContext(ppihlbc: IHlinkBrowseContext): HRESULT; stdcall;
    //function GetBrowseContext(out ppihlbc: IHlinkBrowseContext): HRESULT; stdcall;
    //function Navigate(grfHLNF: DWORD; pbc: IBindCtx; pibsc: IBindStatusCallback; pihlNavigate: IHLink): HRESULT; stdcall;
    function OnNavigate(grfHLNF: DWORD; pimkTarget: IMoniker; pwzLocation,
      pwzFriendlyName: PWideChar; dwreserved: DWORD): HRESULT; stdcall;
    function UpdateHlink(uHLID: ULONG; pimkTarget: IMoniker; pwzLocation,
      pwzFriendlyName: PWideChar): HRESULT; stdcall;
  end;

  IDownloadManager = interface(IUnknown)
    ['{988934A4-064B-11D3-BB80-00104B35E7F9}']
    function Download(
      pmk: IMoniker; // Identifies the object to be downloaded
      pbc: IBindCtx; // Stores information used by the moniker to bind
      dwBindVerb: DWORD; // The action to be performed during the bind
      grfBINDF: DWORD; // Determines the use of URL encoding during the bind
      pBindInfo: PBindInfo; // Used to implement IBindStatusCallback::GetBindInfo
      pszHeaders: PWideChar; // Additional headers to use with IHttpNegotiate
      pszRedir: PWideChar; // The URL that the moniker is redirected to
      uiCP: UINT // The code page of the object's display name
      ): HRESULT; stdcall;
  end;

  IEnumSTATURL = interface(IUnknown)
    ['{3C374A42-BAE4-11CF-BF7D-00AA006946EE}']
    function Next(celt: Integer; out elt; pceltFetched: PLongint): HRESULT;
      stdcall;
    function Skip(celt: Longint): HRESULT; stdcall;
    function Reset: HRESULT; stdcall;
    function Clone(out ppenum: IEnumSTATURL): HRESULT; stdcall;
    function SetFilter(poszFilter: PWideChar; dwFlags: DWORD): HRESULT; stdcall;
  end;

type
  TSTATURL = record
    cbSize: DWORD;
    pwcsUrl: DWORD;
    pwcsTitle: DWORD;
    ftLastVisited: FILETIME;
    ftLastUpdated: FILETIME;
    ftExpires: FILETIME;
    dwFlags: DWORD;
  end;

  PEntry = ^TEntry;
  TEntry = record
    Url: string;
    Title: string;
    LastVisited,
    LastUpdated,
    Expires: TDateTime;
  end;

  IUrlHistoryStg = interface(IUnknown)
    ['{3C374A41-BAE4-11CF-BF7D-00AA006946EE}']
    function AddUrl(pocsUrl: PWideChar; pocsTitle: PWideChar; dwFlags: Integer): HRESULT; stdcall;
    function DeleteUrl(pocsUrl: PWideChar; dwFlags: Integer): HRESULT; stdcall;
    function QueryUrl(pocsUrl: PWideChar; dwFlags: Integer; var lpSTATURL: TSTATURL): HRESULT; stdcall;
    function BindToObject(pocsUrl: PWideChar; var riid: TIID; out ppvOut: Pointer): HRESULT; stdcall;
    function EnumUrls(out ppenum: IEnumSTATURL): HRESULT; stdcall;
  end;


  IUrlHistoryStg2 = interface(IUrlHistoryStg)
    ['{AFA0DC11-C313-11D0-831A-00C04FD5AE38}']
    function AddUrlAndNotify(pocsUrl: PWideChar; pocsTitle: PWideChar; dwFlags: Integer;
      FWriteHistory: Integer; var poctNotify: Pointer;
      const punkISFolder: IUnknown): HRESULT; stdcall;
    function ClearHistory: HRESULT; stdcall;
  end;

  IUrlHistoryNotify = interface(IOleCommandTarget)
    ['{BC40BEC1-C493-11d0-831B-00C04FD5AE38}']
  end;

  PDOCHOSTUIINFO = ^TDOCHOSTUIINFO;
  TDOCHOSTUIINFO = record
    cbSize: ULONG;
    dwFlags: DWORD;
    dwDoubleClick: DWORD;
    chHostCss: POleStr;
    chHostNS: POleStr;
  end;

type
  INewWindowManager = interface(IUnknown)
    ['{D2BC4C84-3F72-4a52-A604-7BCBF3982CBB}']
    function EvaluateNewWindow(pszUrl, pszName, pszUrlContext, pszFeatures:
      LPCWSTR;
      fReplace: BOOL; dwFlags, dwUserActionTime: DWORD): HRESULT; stdcall;
  end;

  IDocHostShowUI = interface(IUnknown)
    ['{c4d244b0-d43e-11cf-893b-00aa00bdce1a}']
    function ShowMessage(hwnd: THandle; lpstrText: POleStr; lpstrCaption: POleStr;
      dwType: longint; lpstrHelpFile: POleStr; dwHelpContext: longint;
      var plResult: LRESULT): HRESULT; stdcall;
    function ShowHelp(hwnd: THandle; pszHelpFile: POleStr; uCommand: integer;
      dwData: longint; ptMouse: TPoint;
      var pDispatchObjectHit: IDispatch): HRESULT; stdcall;
  end;

  IDocHostUIHandler = interface(IUnknown)
    ['{bd3f23c0-d43e-11cf-893b-00aa00bdce1a}']
    function ShowContextMenu(const dwID: DWORD; const ppt: PPOINT;
      const CommandTarget: IUnknown; const Context: IDispatch): HRESULT;
      stdcall;
    function GetHostInfo(var pInfo: TDOCHOSTUIINFO): HRESULT; stdcall;
    function ShowUI(const dwID: DWORD; const pActiveObject:
      IOleInPlaceActiveObject;
      const pCommandTarget: IOleCommandTarget; const pFrame: IOleInPlaceFrame;
      const pDoc: IOleInPlaceUIWindow): HRESULT; stdcall;
    function HideUI: HRESULT; stdcall;
    function UpdateUI: HRESULT; stdcall;
    function EnableModeless(const fEnable: BOOL): HRESULT; stdcall;
    function OnDocWindowActivate(const fActivate: BOOL): HRESULT; stdcall;
    function OnFrameWindowActivate(const fActivate: BOOL): HRESULT; stdcall;
    function ResizeBorder(const prcBorder: PRECT;
      const pUIWindow: IOleInPlaceUIWindow;
      const fRameWindow: BOOL): HRESULT; stdcall;
    function TranslateAccelerator(const lpMsg: PMSG; const pguidCmdGroup: PGUID;
      const nCmdID: DWORD): HRESULT; stdcall;
    function GetOptionKeyPath(out pchKey: POleStr; const dw: DWORD): HRESULT;
      stdcall;
    function GetDropTarget(const pDropTarget: IDropTarget;
      out ppDropTarget: IDropTarget): HRESULT; stdcall;
    function GetExternal(out ppDispatch: IDispatch): HRESULT; stdcall;
    function TranslateUrl(const dwTranslate: DWORD; const pchURLIn: POleStr;
      out ppchURLOut: POleStr): HRESULT; stdcall;
    function FilterDataObject(const pDO: IDataObject;
      out ppDORet: IDataObject): HRESULT; stdcall;
  end;

  IDocHostUIHandler2 = interface(IDocHostUIHandler)
    ['{3050f6d0-98b5-11cf-bb82-00aa00bdce0b}']
    function GetOverrideKeyPath(out pchKey: POleStr; dw: DWORD): HRESULT;
      stdcall;
  end;

  IProtectFocus = interface
    ['{D81F90A3-8156-44F7-AD28-5ABB87003274}']
    function AllowFocusChange(out pfAllow: BOOL): HRESULT; stdcall;
  end;

  ICustomDoc = interface(IUnknown)
    ['{3050f3f0-98b5-11cf-bb82-00aa00bdce0b}']
    function SetUIHandler(const pUIHandler: IDocHostUIHandler): HRESULT;
      stdcall;
  end;

  IHTMLOMWindowServices = interface(IUnknown)
    ['{3050f5fc-98b5-11cf-bb82-00aa00bdce0b}']
    function moveTo(const x, y: Integer): HRESULT; stdcall;
    function moveBy(const x, y: Integer): HRESULT; stdcall;
    function resizeTo(const x, y: Integer): HRESULT; stdcall;
    function resizeBy(const x, y: Integer): HRESULT; stdcall;
  end;

type
  ITravelLogEntry = interface(IUnknown)
    ['{7EBFDD87-AD18-11d3-A4C5-00C04F72D6B8}']
    function GetTitle(var ppszTitle: POleStr): HRESULT; stdcall;
    function GetUrl(var ppszURL: POleStr): HRESULT; stdcall;
  end;

  IEnumTravelLogEntry = interface(IUnknown)
    ['{7EBFDD85-AD18-11d3-A4C5-00C04F72D6B8}']
    function Next(cElt: ULONG; out rgElt: ITravelLogEntry; out pcEltFetched: ULONG): HRESULT; stdcall;
    function Skip(cElt: ULONG): HRESULT; stdcall;
    function Reset: HRESULT; stdcall;
    function Clone(out ppEnum: IEnumTravelLogEntry): HRESULT; stdcall;
  end;

  ITravelLogStg = interface(IUnknown)
    ['{7EBFDD80-AD18-11d3-A4C5-00C04F72D6B8}']
    function CreateEntry(pszURL, pszTitle: POleStr; ptleRelativeTo: ITravelLogEntry;
      fPrepend: BOOL; out pptle: ITravelLogEntry): HRESULT; stdcall;
    function TravelTo(ptle: ITravelLogEntry): HRESULT; stdcall;
    function EnumEntries(Flags: DWORD; out ppenum: IEnumTravellogEntry): HRESULT; stdcall;
    function FindEntries(Flags: DWORD; pszURL: POleStr; out ppenum: IEnumTravelLogEntry): HRESULT; stdcall;
    function GetCount(Flags: DWORD; out pcEntries: DWORD): HRESULT; stdcall;
    function RemoveEntry(ptle: ITravelLogEntry): HRESULT; stdcall;
    function GetRelativeEntry(iOffset: Integer; out ptle: ITravelLogEntry): HRESULT; stdcall;
  end;

  PUrlInvokeCommandInfoA = ^TUrlInvokeCommandInfoA;
  TUrlInvokeCommandInfoA = record
    dwcbSize: DWORD; // Size of structure
    dwFlags: DWORD; // Bit field of IURL_INVOKECOMMAND_FLAGS
    hwndParent: HWND;
      // Parent window.  Valid only if IURL_INVOKECOMMAND_FL_ALLOW_UI is set.
    pcszVerb: LPCSTR;
      // Verb to invoke.  Ignored if IURL_INVOKECOMMAND_FL_USE_DEFAULT_VERB is set.
  end;

  PUrlInvokeCommandInfoW = ^TUrlInvokeCommandInfoW;
  TUrlInvokeCommandInfoW = record
    dwcbSize: DWORD; // Size of structure
    dwFlags: DWORD; // Bit field of IURL_INVOKECOMMAND_FLAGS
    hwndParent: HWND;
      // Parent window.  Valid only if IURL_INVOKECOMMAND_FL_ALLOW_UI is set.
    pcszVerb: LPCWSTR;
      // Verb to invoke.  Ignored if IURL_INVOKECOMMAND_FL_USE_DEFAULT_VERB is set.
  end;

{$IFDEF UNICODE}
  PURLInvokeCommandInfo = ^TUrlInvokeCommandInfoW;
  TUrlInvokeCommandInfo = TUrlInvokeCOmmandInfoW;
{$ELSE}
  PURLInvokeCommandInfo = ^TUrlInvokeCommandInfoA;
  TUrlInvokeCommandInfo = TUrlInvokeCOmmandInfoA;
{$ENDIF}

  IUniformResourceLocatorA = interface(IUnknown)
    [SID_IUniformResourceLocatorA]
    function SetURL(pcszURL: LpcStr; dwInFlags: DWORD): HRESULT; stdcall;
    function GetURL(ppszURL: LpStr): HRESULT; stdcall;
    function InvokeCommand(purlici: PURLINVOKECOMMANDINFOA): HRESULT; stdcall;

  end;

  IUniformResourceLocatorW = interface(IUnknown)
    [SID_IUniformResourceLocatorW]
    function SetURL(pcszURL: LpcWStr; dwInFlags: DWORD): HRESULT; stdcall;
    function GetURL(ppszURL: LpWStr): HRESULT; stdcall;
    function InvokeCommand(purlici: PURLINVOKECOMMANDINFOW): HRESULT; stdcall;
  end;

{$IFDEF UNICODE}
  IUniformResourceLocator = IUniformResourceLocatorW;
{$ELSE}
  IUniformResourceLocator = IUniformResourceLocatorA;
{$ENDIF}

function TranslateURLA(pcszURL: LPCSTR;
  dwInFlags: DWORD;
  ppszTranslatedURL: LPSTR): HRESULT; stdcall;

function TranslateURLW(pcszURL: LPCWSTR;
  dwInFlags: DWORD;
  ppszTranslatedURL: LPWSTR): HRESULT; stdcall;

{$IFDEF UNICODE}
function TranslateURL(pcszURL: LPCWSTR;
  dwInFlags: DWORD;
  ppszTranslatedURL: LPWSTR): HRESULT; stdcall;
{$ELSE}
function TranslateURL(pcszURL: LPCSTR;
  dwInFlags: DWORD;
  ppszTranslatedURL: LPSTR): HRESULT; stdcall;
{$ENDIF}

function URLAssociationDialogA(hwndParent: HWND;
  dwInFlags: DWORD;
  pcszFile: LPCSTR;
  pcszURL: LPCSTR;
  pszAppBuf: LPSTR;
  ucAppBufLen: UINT): HRESULT; stdcall;

function URLAssociationDialogW(hwndParent: HWND;
  dwInFlags: DWORD;
  pcszFile: LPCWSTR;
  pcszURL: LPCWSTR;
  pszAppBuf: LPWSTR;
  ucAppBufLen: UINT): HRESULT; stdcall;

{$IFDEF UNICODE}
function URLAssociationDialog(hwndParent: HWND;
  dwInFlags: DWORD;
  pcszFile: LPCWSTR;
  pcszURL: LPCWSTR;
  pszAppBuf: LPWSTR;
  ucAppBufLen: UINT): HRESULT; stdcall;
{$ELSE}
function URLAssociationDialog(hwndParent: HWND;
  dwInFlags: DWORD;
  pcszFile: LPCSTR;
  pcszURL: LPCSTR;
  pszAppBuf: LPSTR;
  ucAppBufLen: UINT): HRESULT; stdcall;
{$ENDIF}

function MIMEAssociationDialogA(hwndParent: HWND;
  dwInFlags: DWORD;
  pcszFile: LPCSTR;
  pcszMIMEContentType: LPCSTR;
  pszAppBuf: LPSTR;
  ucAppBufLen: UINT): HRESULT; stdcall;

function MIMEAssociationDialogW(hwndParent: HWND;
  dwInFlags: DWORD;
  pcszFile: LPCWSTR;
  pcszMIMEContentType: LPCWSTR;
  pszAppBuf: LPWSTR;
  ucAppBufLen: UINT): HRESULT; stdcall;

{$IFDEF UNICODE}
function MIMEAssociationDialog(hwndParent: HWND;
  dwInFlags: DWORD;
  pcszFile: LPCWSTR;
  pcszMIMEContentType: LPCWSTR;
  pszAppBuf: LPWSTR;
  ucAppBufLen: UINT): HRESULT; stdcall;
{$ELSE}
function MIMEAssociationDialog(hwndParent: HWND;
  dwInFlags: DWORD;
  pcszFile: LPCSTR;
  pcszMIMEContentType: LPCSTR;
  pszAppBuf: LPSTR;
  ucAppBufLen: UINT): HRESULT; stdcall;
{$ENDIF}

function InetIsOffline(dwFlags: DWORD): BOOL; stdcall;

//Serge
{$IFDEF DELPHI6_UP}
type
{$EXTERNALSYM _tagINTERNETFEATURELIST}
  _tagINTERNETFEATURELIST = (
    FEATURE_OBJECT_CACHING = 0,
    FEATURE_ZONE_ELEVATION = 1,
    FEATURE_MIME_HANDLING = 2,
    FEATURE_MIME_SNIFFING = 3,
    FEATURE_WINDOW_RESTRICTIONS = 4,
    FEATURE_WEBOC_POPUPMANAGEMENT = 5,
    FEATURE_BEHAVIORS = 6,
    FEATURE_DISABLE_MK_PROTOCOL = 7,
    FEATURE_LOCALMACHINE_LOCKDOWN = 8,
    FEATURE_SECURITYBAND = 9,
    FEATURE_RESTRICT_ACTIVEXINSTALL = 10,
    FEATURE_VALIDATE_NAVIGATE_URL = 11,
    FEATURE_RESTRICT_FILEDOWNLOAD = 12,
    FEATURE_ADDON_MANAGEMENT = 13,
    FEATURE_PROTOCOL_LOCKDOWN = 14,
    FEATURE_HTTP_USERNAME_PASSWORD_DISABLE = 15,
    FEATURE_SAFE_BINDTOOBJECT = 16,
    FEATURE_UNC_SAVEDFILECHECK = 17,
    FEATURE_GET_URL_DOM_FILEPATH_UNENCODED = 18,
    FEATURE_TABBED_BROWSING = 19,
    FEATURE_SSLUX = 20,
    FEATURE_DISABLE_NAVIGATION_SOUNDS = 21,
    FEATURE_DISABLE_LEGACY_COMPRESSION = 22,
    FEATURE_FORCE_ADDR_AND_STATUS = 23,
    FEATURE_XMLHTTP = 24,
    FEATURE_DISABLE_TELNET_PROTOCOL = 25,
    FEATURE_FEEDS = 26,
    FEATURE_BLOCK_INPUT_PROMPTS = 27,
    FEATURE_ENTRY_COUNT = 28
    );
  TInternetFeature = _tagINTERNETFEATURELIST;
{$ELSE}
type
  _tagINTERNETFEATURELIST = (
    FEATURE_OBJECT_CACHING,
    FEATURE_ZONE_ELEVATION,
    FEATURE_MIME_HANDLING,
    FEATURE_MIME_SNIFFING,
    FEATURE_WINDOW_RESTRICTIONS,
    FEATURE_WEBOC_POPUPMANAGEMEN,
    FEATURE_BEHAVIORS,
    FEATURE_DISABLE_MK_PROTOCOL,
    FEATURE_LOCALMACHINE_LOCKDOWN,
    FEATURE_SECURITYBAND,
    FEATURE_RESTRICT_ACTIVEXINSTALL,
    FEATURE_VALIDATE_NAVIGATE_URL,
    FEATURE_RESTRICT_FILEDOWNLOAD,
    FEATURE_ADDON_MANAGEMENT,
    FEATURE_PROTOCOL_LOCKDOWN,
    FEATURE_HTTP_USERNAME_PASSWORD_DISABLE,
    FEATURE_SAFE_BINDTOOBJECT,
    FEATURE_UNC_SAVEDFILECHECK,
    FEATURE_GET_URL_DOM_FILEPATH_UNENCODED,
    FEATURE_TABBED_BROWSING,
    FEATURE_SSLUX,
    FEATURE_DISABLE_NAVIGATION_SOUNDS,
    FEATURE_DISABLE_LEGACY_COMPRESSION,
    FEATURE_FORCE_ADDR_AND_STATUS,
    FEATURE_XMLHTTP,
    FEATURE_DISABLE_TELNET_PROTOCOL,
    FEATURE_FEEDS,
    FEATURE_BLOCK_INPUT_PROMPTS,
    FEATURE_ENTRY_COUNT);
  TInternetFeature = _tagINTERNETFEATURELIST;
{$ENDIF}


function CoInternetSetFeatureEnabled(aFeature: TInternetFeature;
  dwFlags: DWORD; fEnable: Boolean): HRESULT;
function CoInternetIsFeatureEnabled(aFeature: TInternetFeature;
  dwFlags: DWORD): HRESULT;
function CoInternetIsFeatureEnabledForUrl(aFeature: TInternetFeature;
  dwFlags: DWORD; szUrl: LPCWSTR; pSecMgr: IInternetSecurityManager): HRESULT;

function CoInternetIsFeatureZoneElevationEnabled(
  szFromURL, szToURL: LPCWSTR; pSecMgr: IInternetSecurityManager;
  dwFlags: DWORD): HRESULT;

var
  ShellModule: THandle;
  ComCtlModule: THandle;
  HHCtrlModule: THandle;
  URLMonModule: THandle;

function SHLockShared(Handle: THandle; DW: DWORD): Pointer; stdcall;
function SHUnlockShared(Pnt: Pointer): BOOL; stdcall;
function SHFreeShared(Handle: THandle; DW: DWORD): Pointer; stdcall;
function _Free(Pidl: PItemIDList): BOOL; stdcall;
function HtmlHelp(hwndCaller: HWND; pszFile: PChar; uCommand: Integer;
  dwData: DWORD): HWND; stdcall;

function GetSHDOCLCModule: THandle;

{events}
type
  OnQueryInterfaceEvent = procedure(Sender: TObject; const IID: TGUID; var Obj; Rezult: HRESULT) of object;

  {IDispatch Interface}
  TGetTypeInfoCountEvent = procedure(Sender: TObject; var Count: Integer; var
    Rezult: HRESULT) of object;
  TGetTypeInfoEvent = procedure(Sender: TObject; Index, LocaleID: Integer; var
    TypeInfo: ITypeInfo; var Rezult: HRESULT) of object;
  TGetIDsOfNamesEvent = procedure(Sender: TObject; const IID: TGUID; Names:
    Pointer;
    NameCount, LocaleID: Integer; DispIDs: Pointer; var Rezult: HRESULT) of
    object;
  TInvokeEvent = procedure(Sender: TObject; DispID: Integer; const IID: TGUID;
    LocaleID: Integer;
    Flags: Word; var Params: TDispParams; VarResult, ExcepInfo, ArgErr: Pointer;
    var Rezult: HRESULT) of object;
  {IDropTarget Interface}
  TOnDropEvent = procedure(Sender: TObject; const dataObj: IDataObject;
    grfKeyState: Longint; pt: TPoint;
    var dwEffect: Longint; var Rezult: HRESULT) of object;
  TOnDragEnterEvent = procedure(Sender: TObject; const dataObj: IDataObject;
    grfKeyState: Longint;
    pt: TPoint; var dwEffect: Longint; var Rezult: HRESULT) of object;
  TOnDragOverEvent = procedure(Sender: TObject; grfKeyState: Longint; pt:
    TPoint; var dwEffect: Longint;
    var Rezult: HRESULT) of object;
  {IOLECommandTarget Interface}
  TComTargetExecEvent = procedure(Sender: TObject; CmdGroup: PGUID; nCmdID,
    nCmdexecopt: DWORD;
    const vaIn: OleVariant; var vaOut: OleVariant; var Rezult: HRESULT) of
    object;
  {IServiceProvider Interface}
  TQueryServiceEvent = procedure(Sender: TObject; const rsid, iid: TGUID; var
    Obj: IUnknown) of object;

implementation

function InetIsOffline; external urldll name 'InetIsOffline';

function MIMEAssociationDialogW; external urldll name 'MIMEAssociationDialogW';
function MIMEAssociationDialogA; external urldll name 'MIMEAssociationDialogA';
{$IFDEF UNICODE}
function MIMEAssociationDialog; external urldll name 'MIMEAssociationDialogW';
{$ELSE}
function MIMEAssociationDialog; external urldll name 'MIMEAssociationDialogA';
{$ENDIF}

function URLAssociationDialogW; external urldll name 'URLAssociationDialogW';
function URLAssociationDialogA; external urldll name 'URLAssociationDialogA';
{$IFDEF UNICODE}
function URLAssociationDialog; external urldll name 'URLAssociationDialogW';
{$ELSE}
function URLAssociationDialog; external urldll name 'URLAssociationDialogA';
{$ENDIF}

function TranslateURLA; external urldll name 'TranslateURLA';
function TranslateURLW; external urldll name 'TranslateURLW';
{$IFDEF UNICODE}
function TranslateURL; external urldll name 'TranslateURLW';
{$ELSE}
function TranslateURL; external urldll name 'TranslateURLA';
{$ENDIF}


function GetShellModule: THandle;
begin
  if ShellModule = 0 then
  begin
    ShellModule := SafeLoadLibrary(Shell32);
    if ShellModule <= HINSTANCE_ERROR then
      ShellModule := 0;
  end;
  Result := ShellModule;
end;

function GetHHctrlModule: THandle;
begin
  if HHCtrlModule = 0 then
  begin
    HHCtrlModule := SafeLoadLibrary(HHCtrl);
    if HHCtrlModule <= HINSTANCE_ERROR then
      HHCtrlModule := 0;
  end;
  Result := HHCtrlModule;
end;

function GetComctlModule: THandle;
begin
  if ComctlModule = 0 then
  begin
    ComctlModule := SafeLoadLibrary(comctl32);
    if ComctlModule <= HINSTANCE_ERROR then
      ComctlModule := 0;
  end;
  Result := ComctlModule;
end;

function HtmlHelp(hwndCaller: HWND; pszFile: PChar; uCommand: Integer;
  dwData: DWORD): HWND; stdcall;
type
  TheFunctionType = function(hwndCaller: HWND; pszFile: PChar; uCommand: Integer;
    dwData: DWORD): HWND; stdcall;
var
  TheFunction: TheFunctionType;
begin
  Result := 0;
  if HHCtrlModule = 0 then
    HHCtrlModule := GetHHCtrlModule;
  if HHCtrlModule <> 0 then
  begin
{$IFDEF UNICODE}
    TheFunction := GetProcAddress(HHCtrlModule, 'HtlmHelpW');
{$ELSE}
    TheFunction := GetProcAddress(HHCtrlModule, 'HtlmHelpA');
{$ENDIF UNICODE}
    if (Assigned(TheFunction)) then
      Result := TheFunction(hwndCaller, pszFile, uCommand, dwdata);
  end;
end;

function _Free(Pidl: PItemIDList): BOOL; stdcall;
type
  TheFunctionType = function(Pidl: PItemIDList): BOOL; stdcall;
var
  TheFunction: TheFunctionType;
begin
  Result := False;
  if ComctlModule = 0 then
    ComctlModule := GetComctlModule;
  if ComctlModule <> 0 then
  begin
    TheFunction := GetProcAddress(ComctlModule, PChar(Free_Index));
    if (Assigned(TheFunction)) then
      Result := TheFunction(Pidl);
  end;
end;

function SHLockShared(Handle: THandle; DW: DWORD): Pointer; stdcall;
type
  TheFunctionType = function(Handle: THandle; DW: DWORD): Pointer; stdcall;
var
  TheFunction: TheFunctionType;
begin
  Result := nil;
  if ShellModule = 0 then
    ShellModule := GetShellModule;
  if ShellModule <> 0 then
  begin
    TheFunction := GetProcAddress(ShellModule, PChar(SHLockShared_Index));
    if (Assigned(TheFunction)) then
      Result := TheFunction(Handle, DW);
  end;
end;

function SHUnLockShared(pnt: Pointer): BOOL; stdcall;
type
  TheFunctionType = function(pnt: Pointer): BOOL; stdcall;
var
  TheFunction: TheFunctionType;
begin
  Result := FALSE;
  if ShellModule = 0 then
    ShellModule := GetShellModule;
  if ShellModule <> 0 then
  begin
    TheFunction := GetProcAddress(ShellModule, PChar(SHUnLockShared_Index));
    if (Assigned(TheFunction)) then
      Result := TheFunction(pnt);
  end;
end;

function SHFreeShared(Handle: THandle; DW: DWORD): Pointer; stdcall;
type
  TheFunctionType = function(Handle: THandle; DW: DWORD): Pointer; stdcall;
var
  TheFunction: TheFunctionType;
begin
  Result := nil;
  if ShellModule = 0 then
    ShellModule := GetShellModule;
  if ShellModule <> 0 then
  begin
    TheFunction := GetProcAddress(ShellModule, PChar(SHFreeShared_Index));
    if (Assigned(TheFunction)) then
      Result := TheFunction(Handle, DW);
  end;
end;

function CheckURLMonModule: Boolean;
begin
  if URLMonModule = 0 then
    URLMonModule := SafeLoadLibrary(UrlMonLib);
  Result := URLMonModule > HINSTANCE_ERROR;
end;

function CheckURLMonModuleFunc(const Name: string; var ptr: Pointer): Boolean;
begin
  Result := CheckURLMonModule;
  if Result then
  begin
    ptr := GetProcAddress(URLMonModule, PChar(Name));
    Result := Assigned(ptr);
    if not Result then
      ptr := Pointer(1); // ???
  end;
end;

type
  TCoInternetSetFeatureEnabled = function(aFeature: DWORD;
    dwFlags: DWORD; fEnable: BOOL): HRESULT; stdcall;
  TCoInternetIsFeatureEnabled = function(aFeature: DWORD;
    dwFlags: DWORD): HRESULT; stdcall;
  TCoInternetIsFeatureEnabledForUrl = function(aFeature: DWORD;
    dwFlags: DWORD; szUrl: LPCWSTR; pSecMgr: IInternetSecurityManager): HRESULT;
  stdcall;
  TCoInternetIsFeatureZoneElevationEnabled = function(szFromURL, szToURL:
    LPCWSTR; pSecMgr: IInternetSecurityManager;
    dwFlags: DWORD): HRESULT; stdcall;

var
  CoInternetIsFeatureZoneElevationEnabledPtr: Pointer;

  CoInternetSetFeatureEnabledPtr: Pointer;
  CoInternetIsFeatureEnablePtr: Pointer;
  CoInternetIsFeatureEnabledForUrlPtr: Pointer;

function CoInternetSetFeatureEnabled(aFeature: TInternetFeature;
  dwFlags: DWORD; fEnable: Boolean): HRESULT;
begin
  if (Integer(CoInternetSetFeatureEnabledPtr) > 1) or
    CheckURLMonModuleFunc('CoInternetSetFeatureEnabled',
    CoInternetSetFeatureEnabledPtr) then
    Result := TCoInternetSetFeatureEnabled(CoInternetSetFeatureEnabledPtr)(
      DWORD(aFeature), dwFlags, fEnable)
  else
    Result := E_NOTIMPL;
end;

function CoInternetIsFeatureEnabled(aFeature: TInternetFeature;
  dwFlags: DWORD): HRESULT;
begin
  if (Integer(CoInternetIsFeatureEnablePtr) > 1) or
    CheckURLMonModuleFunc('CoInternetIsFeatureEnabled',
    CoInternetIsFeatureEnablePtr) then
    Result := TCoInternetIsFeatureEnabled(CoInternetIsFeatureEnablePtr)(
      DWORD(aFeature), dwFlags)
  else
    Result := E_NOTIMPL;
end;

function CoInternetIsFeatureEnabledForUrl(aFeature: TInternetFeature;
  dwFlags: DWORD; szUrl: LPCWSTR; pSecMgr: IInternetSecurityManager): HRESULT;
begin
  if (Integer(CoInternetIsFeatureEnabledForUrlPtr) > 1) or
    CheckURLMonModuleFunc('CoInternetIsFeatureEnabledForUrl',
    CoInternetIsFeatureEnabledForUrlPtr) then
    Result :=
      TCoInternetIsFeatureEnabledForUrl(CoInternetIsFeatureEnabledForUrlPtr)(
      DWORD(aFeature), dwFlags, szUrl, pSecMgr)
  else
    Result := E_NOTIMPL;
end;


function CoInternetIsFeatureZoneElevationEnabled(
  szFromURL, szToURL: LPCWSTR; pSecMgr: IInternetSecurityManager;
  dwFlags: DWORD): HRESULT;
begin
  if (Integer(CoInternetIsFeatureZoneElevationEnabledPtr) > 1) or
    CheckURLMonModuleFunc('CoInternetIsFeatureZoneElevationEnabled',
    CoInternetIsFeatureZoneElevationEnabledPtr) then
    Result :=
      TCoInternetIsFeatureZoneElevationEnabled(CoInternetIsFeatureZoneElevationEnabledPtr)(
      szFromURL, szToURL, pSecMgr, dwFlags)
  else
    Result := E_NOTIMPL;
end;

var
  HSHDOCLCModule: THandle;

function GetSHDOCLCModule: THandle;
begin
  if HSHDOCLCModule = 0 then
  begin
    HSHDOCLCModule := GetModuleHandle('SHDOCLC.DLL');
    if HSHDOCLCModule = 0 then
      HSHDOCLCModule := SafeLoadLibrary('SHDOCLC.DLL');
    if HSHDOCLCModule <= HINSTANCE_ERROR then
      HSHDOCLCModule := 0;
  end;
  Result := HSHDOCLCModule;
end;


initialization

finalization
  if ShellModule <> 0 then
    FreeLibrary(ShellModule);
  if ComctlModule <> 0 then
    FreeLibrary(ComctlModule);
  if HHCtrlModule <> 0 then
    FreeLibrary(HHCtrlModule);
  if URLMonModule <> 0 then
    FreeLibrary(URLMonModule);
end.
