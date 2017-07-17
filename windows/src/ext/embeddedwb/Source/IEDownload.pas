//*************************************************************************
//                                                                        *
//                    IEDownload 2010                                     *
//     IEDownload is a UrlMon wrapper with a build-in Callback            *
//                                                                        *
//                     Freeware Component                                 *
//                       for Delphi by                                    *
//                      Eran Bodankin                                     *
//                   and Per Lindsø Larsen                                *
//                                                                        *
//                                                                        *
//  Updated versions:                                                     *
//               http://www.bsalsa.com                                    *
//*************************************************************************
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
//$Id: IEDownload.pas,v 1.6 2009/02/25 11:56:31 bsalsa Exp $

unit IEDownload;

{To use the MSHTML, just remove the dot in the line below like {$DEFINE USE_MSHTML}{
and re-compile the package.}
{$DEFINE USE_MSHTML}

interface

{$I EWB.inc}

{$IFDEF DELPHI6_UP}
{$WARN SYMBOL_PLATFORM OFF}
{$ENDIF}

{$IFDEF DELPHI7_UP}
{$WARN SYMBOL_DEPRECATED OFF}
{$ENDIF}

uses
  Dialogs, IEDownloadAcc, Controls, Shellapi, EwbIEConst, ActiveX,
  Contnrs, ExtCtrls, Windows, WinInet, UrlMon, Classes, SysUtils
{$IFDEF DELPHI5}, FileCtrl{$ENDIF}{$IFDEF USE_MSHTML}, MSHTML_EWB{$ENDIF};

const
  WAIT_BSCB = WAIT_OBJECT_0 + 1;

{$IFNDEF UNICODE}
type
  RawByteString = AnsiString;
{$ENDIF UNICODE}

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

  TCustomIEDownload = class;

  TInfoData = class(TList)
  public
    infAdditionalHeader: TStrings;
    infBindF_Value: Cardinal;
    infBindF2_Value: Cardinal;
    infBindInfoF_Value: Cardinal;
    infBindInfoOptions_Value: Cardinal;
    infBindVerb_Value: Cardinal;
    infCodePage_Value: Cardinal;
    infCustomVerb: string;
    infDescriptor: RawByteString;
    infDownloadFolder: string;
    infExtraInfo: string;
    infFileExt: string;
    infFileName: string;
    infFileSize: Cardinal;
    infHost: string;
    infIndex: Integer;
    infInheritHandle: Boolean;
    infPassword: string;
    infPostData: string;
    infPutFileName: string;
    infRangeBegin: Cardinal;
    infRangeEnd: Integer;
    infSender: TCustomIEDownload;
    infTimeOut: Integer;
    infUrl: PWideChar;
    infUserAgent: string;
    infUserName: string;
    Sender: TObject;
  public
    constructor Create;
    destructor Destroy; override;
  end;

  TThreadStatus = (tsRunning, tsSuspended, tsWaiting, tsTerminated);
  TState = (sBusy, sReady, sStopped);

  TBSCB = class(TThread,
      IAuthenticate,
{$IFDEF DELPHI6_UP}
      IAuthenticateEx,
      IMonikerProp,
{$ENDIF}
      IBindHost,
      IWindowForBindingUI,
      IBindStatusCallback,
      IBindStatusCallbackEx,
      ICodeInstall,
      IHttpNegotiate,
      IHttpNegotiate2,
      IHttpNegotiate3,
      IHTTPSecurity,
{$IFDEF USE_MSHTML}
      IPropertyNotifySink,
{$ENDIF}
      IServiceProvider,
      IUnknown)

  private
    Frequency: Int64;
    TimeStarted: Int64;
    TimeNow: Int64;
    FSender: TCustomIEDownload;
    FBindCtx: IBindCtx;
    FBSCBTimer: TTimer;
    FDataSize: Integer;
    FGlobalData: HGLOBAL;
    FMoniker: IMoniker;
    FRedirect: Boolean;
    fOutStream: IStream;
    FTimedOut: Boolean;
    FTotalRead: Cardinal;
    m_pPrevBSCB: IBindStatusCallback;
    fsOutputFile: TFileStream;

    function GetSerializedClientCertContext(out ppbCert: Byte; var pcbCert:
      DWORD): HResult; stdcall;
{$IFDEF DELPHI6_UP}
    function AuthenticateEx(out phwnd: HWND; out pszUsername,
      pszPassword: LPWSTR; var pauthinfo: AUTHENTICATEINFO): HResult; stdcall;
    {IMonikerProp Interface}
    function PutProperty(mkp: MONIKERPROPERTY; val: LPCWSTR): HResult;
      stdcall;
{$ENDIF}

    {IBindStatusCallbackEx}
    function GetBindInfoEx(out grfBINDF: DWORD; var pbindinfo: BINDINFO;
      out grfBINDF2: DWORD; out pdwReserved: DWORD): HResult; stdcall;

{$IFDEF USE_MSHTML}
    {IPropertyNotifySink Interface}
    function OnChanged(dispId: TDispId): HRESULT; stdcall;
    function OnRequestEdit(dispId: TDispId): HRESULT; stdcall;
{$ENDIF}

    {IHttpNegotiate2 Interface}
    function GetRootSecurityId(var SecurityIdBuffer: TByteArray; var
      BufferSize: DWord; dwReserved: DWORD): HResult; stdcall;

    {IBindStatusCallback Interface}
    function GetBindInfo(out grfBINDF: DWORD; var BindInfo: TBindInfo): HRESULT;
      stdcall;
    function GetPriority(out nPriority): HRESULT; stdcall;
    function OnDataAvailable(grfBSCF: DWORD; dwSize: DWORD; FormatEtc:
      PFormatEtc; stgmed: PStgMedium): HRESULT; stdcall;
    function OnLowResource(Reserved: DWORD): HRESULT; stdcall;
    function OnProgress(ulProgress, ulProgressMax, ulStatusCode: ULONG;
      szStatusText: LPCWSTR): HRESULT; stdcall;
    function OnObjectAvailable(const IID: TGUID; punk: IUnknown): HRESULT;
      stdcall;
    function OnStartBinding(dwReserved: DWORD; pib: IBinding): HRESULT; stdcall;
    function OnStopBinding(HRESULT: HRESULT; szError: LPCWSTR): HRESULT;
      stdcall;
    function OnSecurityProblem(dwProblem: DWORD): HRESULT; stdcall;

    {IHTTPNegotiate methods}
    function OnResponse(dwResponseCode: DWORD; szResponseHeaders,
      szRequestHeaders: LPCWSTR;
      out szAdditionalRequestHeaders: LPWSTR): HRESULT; stdcall;
    function BeginningTransaction(szURL, szHeaders: LPCWSTR; dwReserved: DWORD;
      out szAdditionalHeaders: LPWSTR): HRESULT; stdcall;

    {IUnknown Interface}
    function QueryInterface(const IID: TGUID; out Obj): HRESULT; stdcall;
    function _AddRef: Integer; stdcall;
    function _Release: Integer; stdcall;

    {IWindowForBindingUI methods}
    function GetWindow(const GUIDReason: TGUID; out hwnd): HRESULT; stdcall;

    {IAuthenticate Interface}
    function Authenticate(var hwnd: HWnd; var szUserName, szPassWord: LPWSTR):
      HResult; stdcall;

    {ICodeInstall Interface}
    function OnCodeInstallProblem(ulStatusCode: ULONG; szDestination, szSource:
      LPCWSTR;
      dwReserved: DWORD): HResult; stdcall;

    {IBindHost Interface}
    function CreateMoniker(szName: POLEStr; BC: IBindCtx; out mk: IMoniker;
      dwReserved: DWORD): HResult; stdcall;
    function MonikerBindToStorage(Mk: IMoniker; BC: IBindCtx; BSC:
      IBindStatusCallback;
      const iid: TGUID; out pvObj{$IFNDEF DELPHI8_UP}: Pointer{$ENDIF}): HResult;
        stdcall;
    function MonikerBindToObject(Mk: IMoniker; BC: IBindCtx; BSC:
      IBindStatusCallback;
      const iid: TGUID; out pvObj{$IFNDEF DELPHI8_UP}: Pointer{$ENDIF}): HResult;
        stdcall;

    {IServiceProvider Interface}
    function QueryService(const rsid, iid: TGUID; out Obj): HRESULT; stdcall;

    function GetBindResult(out clsidProtocol: TCLSID; out dwResult: DWORD;
      out szResult: POLEStr): HRESULT;
  private
    function CheckCancelState: Integer;
    procedure ClearAll;
    procedure TimerExpired(Sender: TObject);
    procedure DoConnect;
    procedure DoDownloadToFile;
    procedure DoDownloadToCache;
    procedure ReceiveData;
    procedure ReturnData;
    procedure GetData(aSender: TCustomIEDownload);
    procedure SetComponents;
  protected
    procedure Execute; override;
    procedure Suspend;
    procedure Terminate;
    procedure Resume;

  public
    Stream: TStream;
    Binding: IBinding;
    BscbInfo: TInfoData;
    ThreadStatus: TThreadStatus;
    constructor Create(aSender: TCustomIEDownload; const pmk: IMoniker;
      const pbc: IBindCtx; CreateSuspended: boolean);
    destructor Destroy; override;
    function QueryInfoFileName: HRESULT;
    function DoSaveFileAs: string;
    function QueryInfo(dwOption: DWORD; var Info: Cardinal): Boolean; overload;
    function QueryInfo(dwOption: DWORD; var Info: string): Boolean; overload;
    function QueryInfo(dwOption: DWORD; var Info: TDateTime): Boolean; overload;
    function IsRunning: Boolean;
    function GetDisplayName: PWideChar;
    function GetFileNameFromUrl(Url: string): string;
    function AbortBinding: Hresult;
    function MkParseDisplayName(var DisplayName: PWideChar): IMoniker;
  end;

  TBSCBList = class(TObjectList) {by Jury Gerasimov}
  private
    function GetItem(Index: Integer): TBSCB;
    procedure SetItem(Index: Integer; Value: TBSCB);
  public
    SessionList: TStrings;
    constructor Create;
    destructor Destroy; override;
    property Items[Index: Integer]: TBSCB read GetItem write SetItem; default;
    function byURL(Url: string): TBSCB;
  end;

  TSecurity = class(TPersistent)
  private
    FInheritHandle: Boolean;
    FDescriptor: RawByteString;
  published
    property InheritHandle: boolean read FInheritHandle write FInheritHandle
      default False;
    property Descriptor: RawByteString read FDescriptor write FDescriptor;
  end;

  TRange = class(TPersistent)
  private
    FRangeBegin: Integer;
    FRangeEnd: Integer;
  published
    property RangeBegin: Integer read FRangeBegin write FRangeBegin default 0;
    property RangeEnd: Integer read FRangeEnd write FRangeEnd default 0;
  end;
  {http://msdn.microsoft.com/en-us/library/ms775130(VS.85).aspx}
  TBindF = (Asynchronous, AsyncStorage, NoProgressiveRendering,
    OfflineOperation, GetNewestVersion, NoWriteCache, NeedFile, PullData,
    IgnoreSecurityProblem, Resynchronize, AllowHyperlink, No_UI,
    SilentOperation, Pragma_No_Cache, GetClassObject, Reserved_1,
    Free_Threaded, DirectReadIgnoreSize, HandleAsFormsSubmit,
    GetFromCacheIfNetFail, FromUrlmon, FisrtTryCache, PreferDefaultHandler,
    RestrictedSitesZone);
  TBindF_Options = set of TBindF;
  TBindF2 = (DisableBasicAuth, DisableAutoCookie, DisableRedirectUnlessSID,
    ReadDataOver4GB, Reserved_2, Reserved_11);
  TBindF2_Options = set of TBindF2;
  TBindInfoF = (PostData, ExtraInfo);
  TBindInfoF_Options = set of TBindInfoF;
  TBindInfoOption = (UseBindInfoOptions, EnableUtf8, DisableUtf8,
    UseIE_Encoding,
    BindToObject, SecurityOptOut, IgnoreMimeTextPlain, UseBindStrCredentials,
    IgnoreHttp2HttpsRedirect, IgnoreSslErrOnce, WpcDownloadBlocked,
      WpcLoggingEnabled,
    DisableAutoRedirect, ShDocVw_Reserved, AllowConnectMessages);
  TBindInfoOptions_Options = set of TBindInfoOption;
  TBindVerb = (Get, Post, Put, Custom);
  TCodePageOption = (
    Ansi, {default to ANSI code page}
    OEM, {default to OEM  code page}
    Mac, {default to MAC  code page}
    ThreadsAnsi, {Current thread's ANSI code page}
    Symbol, {Symbol code page (42)}
    UTF7, {Translate using UTF-7}
    UTF8); {Translate using UTF-8}

  TDownloadTo = (dtNormal, dtDownloadToFile, dtDownloadToCache, dtMoniker);
  TDownloadMethod = (dmStream, dmFile); {Set download to a file or astream}
  TFileExistsOption = (feOverWrite, feSkip, feRename); {If file exsits then..}

  TQueryInterfaceEvent = function(const IID: TGUID; out Obj): HRESULT of object;
  TAuthenticateEvent = procedure(Sender: TBSCB; var tmpHWND: HWnd;
    var szUserName, szPassWord: WideString; var Rezult: HRESULT) of object;
{$IFDEF DELPHI6_UP}
  TAuthenticateExEvent = procedure(Sender: TBSCB; var tmpHWND: HWnd;
    var szUserName, szPassWord: WideString; pauthinfo: AUTHENTICATEINFO;
    var Rezult: HRESULT) of object;
  TOnPutPropertyEvent = function(Sender: TBSCB; mkp: MONIKERPROPERTY; val:
    LPCWSTR): HResult of object;
{$ENDIF}

  TOnCodeInstallProblemEvent = function(Sender: TBSCB; ulStatusCode: ULONG;
    szDestination, szSource: LPCWSTR;
    dwReserved: DWORD; stResult: string): HRESULT of object;
  TStateChangeEvent = procedure(const State: TState) of object;
  TErrorEvent = procedure(const ErrorCode: integer; const
    stError: string) of object;
  TOnConnectEvent = procedure(Sender: TBSCB; Res: HRESULT; stMessage: string) of
    object;
  TOnGetBindInfoEvent = function(Sender: TBSCB; out grfBINDF: DWORD; var
    BindInfo: TBindInfo): HRESULT of object;
  TOnGetBindInfoExEvent = function(Sender: TBSCB; out grfBINDF: DWORD;
    pbindinfo: BINDINFO;
    out grfBINDF2: DWORD): HRESULT of object;
  TRedirect = procedure(Sender: TBSCB; var AbortRedirect: boolean; const
    FromUrl: string; const DestUrl: string) of object;
  TBeginningTransactionEvent = function(Sender: TBSCB; szURL, szHeaders:
    LPCWSTR; dwReserved: DWORD;
    out szAdditionalHeaders: LPWSTR): HRESULT of object;
  TOnResponseEvent = function(Sender: TBSCB; dwResponseCode: DWORD;
    szResponseHeaders, szRequestHeaders: LPCWSTR;
    out szAdditionalRequestHeaders: LPWSTR): HRESULT of object;
  TOnSecurityProblemEvent = function(Sender: TBSCB; dwProblem: DWORD; Problem:
    string): HRESULT of object;
  TFileExistsEvent = procedure(var Action: TFileExistsOption; const aFileName:
    WideString; var NewFileName: WideString) of object;
  TOnProgressEvent = procedure(Sender: TBSCB; ulProgress, ulProgressMax,
    ulStatusCode, FileSize: ULONG; szStatusText: LPCWSTR; Downloaded,
    ElapsedTime, Speed, RemainingTime, Status, Percent: string) of object;
  TOnDataAvailableEvent = procedure(Sender: TBSCB; var Buffer: PByte; var
    BufLength: Cardinal) of object;
  TOnDataAvailableInfoEvent = procedure(Sender: TBSCB; grfBSCF: DWORD;
    Status: string {; FormatEtc: PFormatEtc}) of object;
  TOnCompleteEvent = procedure(Sender: TCustomIEDownload; aFileNameAndPath,
    aFileName,
    aFolderName, aExtension: WideString; const ActiveConnections: Integer) of
      object;
  TOnStreamCompleteEvent = procedure(Sender: TBSCB; Stream: TStream; Result:
    HRESULT) of object;
  TOnResumeEvent = procedure(Sender: TBSCB; FileName: string; var Action:
    Cardinal) of object;
  TGetWindowEvent = function(Sender: TBSCB; const GUIDReason: TGUID; out hwnd:
    LongWord): HRESULT of object;
  TOnStartBindingEvent = procedure(Sender: TBSCB; var Cancel: Boolean; pib:
    IBinding; const FileName: WideString; const FileSize: integer) of object;
  TOnStopBindingEvent = procedure(Sender: TBSCB; HRESULT: HRESULT;
    szError: LPCWSTR) of object;
  TOnGetBindResultsEvent = procedure(var Sender: TBSCB; out clsidProtocol:
    TCLSID; out dwResult: DWORD; out szResult: POLEStr;
    const stResult: string) of object;
  TOnGetClientCertEvent = function(var Sender: TBSCB; out ppbCert: Byte; var
    pcbCert: DWORD): HResult of object;
  TTerminateEvent = procedure(const Sender: TBSCB; const ThreadId: Integer;
    const aFileName: Widestring; var bCancel: Boolean) of object;
  TOnGetRootSecurityIdEvent = function(var SecurityIdBuffer: TByteArray; var
    BufferSize: DWord): HRESULT of object;
  {IServiceProvider Interface}
  TQueryServiceEvent = procedure(Sender: TObject; const rsid, iid: TGUID; var
    Obj: IUnknown) of object;
  TOnBeforeDownloadEvent = procedure(Sender: TInfoData; const Url, FileName,
    FileExtension, Host, DownloadFolder: string; const FileSize: Integer; var
      Cancel: Boolean) of object;

  TCustomIEDownload = class(TComponent)

  private
    FAbout: string;
{$IFNDEF DELPHI7_UP}
    FOldTimeSep: Char;
{$ENDIF}
    bCancelAll: boolean;
    bDone: boolean;
    bRenamed: boolean;
    BS: TBSCB;
    FActiveConnections: integer;
    FAdditionalHeader: TStrings;
    FBeginningTransaction: TBeginningTransactionEvent;
    FBindF: TBindF_Options;
    FBindF_Value: Cardinal;
    FBindF2: TBindF2_Options;
    FBindF2_Value: Cardinal;
    FBindInfoF: TBindInfoF_Options;
    FBindInfoF_Value: Cardinal;
    FBindInfoOption_: TBindInfoOptions_Options;
    FBindInfoOption_Value: Cardinal;
    FBindVerb: TBindVerb;
    FBindVerb_Value: Cardinal;
    FBusy: Boolean;
    FCancel: Boolean;
    FCodePageOption: TCodePageOption;
    FCodePageValue: Cardinal;
    FCustomVerb: string;
    FDefaultProtocol: string;
    FDefaultUrlFileName: string;
    FDisplayName: PWideChar;
    FdlCounter: integer;
    FDownloadedFile: string;
    FDownloadFolder: string;
    FDownloadMethod: TDownloadMethod;
    FDownloadTo: TDownloadTo;
    FExtraInfo: string;
    FFileExistsOption: TFileExistsOption;
    FFileExtension: string;
    FFileName: string;
    FFileSize: ULong;
    FFullUserAgent: string;
    FGetWindow: TGetWindowEvent;
    FHWnd: HWND;
    FMimeType: string;
    FOnAuthenticate: TAuthenticateEvent;
{$IFDEF DELPHI6_UP}
    FOnAuthenticateEx: TAuthenticateExEvent;
    FOnPutProperty: TOnPutPropertyEvent;
{$ENDIF}
{$IFDEF DELPHI7_UP}
    FFormatSettings: TFormatSettings;
{$ENDIF}
    FOnCodeInstallProblem: TOnCodeInstallProblemEvent;
    FOnComplete: TOnCompleteEvent;
    FOnConnect: TOnConnectEvent;
    FOnBeforeDownload: TOnBeforeDownloadEvent;
    FOnDataAvailable: TOnDataAvailableEvent;
    FOnDataAvailableInfo: TOnDataAvailableInfoEvent;
    FOnError: TErrorEvent;
    FOnFileExists: TFileExistsEvent;
    FOnGetBindInfo: TOnGetBindInfoEvent;
    FOnGetBindInfoEx: TOnGetBindInfoExEvent;
    FOnGetBindResults: TOnGetBindResultsEvent;
    FOnGetClientCert: TOnGetClientCertEvent;
    FOnGetRootSecurityId: TOnGetRootSecurityIdEvent;
    FOnProgress: TOnProgressEvent;
    FOnQueryInterface: TQueryInterfaceEvent;
    FOnQueryService: TQueryServiceEvent;
    FOnRedirect: TRedirect;
    FOnResponse: TOnResponseEvent;
    FOnResume: TOnResumeEvent;
    FOnSecurityProblem: TOnSecurityProblemEvent;
    FOnStartBinding: TOnStartBindingEvent;
    FOnStateChange: TStateChangeEvent;
    FOnStopBinding: TOnStopBindingEvent;
    FOnStreamComplete: TOnStreamCompleteEvent;
    FOnTerminate: TTerminateEvent;
    FOpenDownloadFolder: Boolean;
    FPassword: string;
    FPostData: string;
    FProxySettings: TProxySettings;
    FPutFileName: string;
    FRange: TRange;
    FRefCount: Integer;
    FSecurity: TSecurity;
    FServerAddress: string;
    FServerIP: string;
    FStartTick: Integer;
    FState: TState;
    FTimeOut: Integer;
    FUrl: string;
    FUserAgent: string;
    FUserName: string;
    FUseSystemDownloadFolder: boolean;
    FValidateUrl: boolean;
    hProcess: THandle;
    hStop: THandle;

  private
    function GoAction(const actUrl, actFileName, actDownloadFolder: string;
      pmk: IMoniker; pbc: IBindCtx): boolean;
    function GoInit(const inUrl: string; const inFileName: string;
      const inDownloadFolder: string): boolean;
    function SetDownloadFolder(const aDownloadFolder: string): string;
    function SetHttpProtocol(const aUrl: string): string;
    procedure DoUpdate;
    procedure ExtractDataFromFile(const aFileName: string);
    procedure PrepareForExit;
    procedure PrepareForStart;
    procedure SetAbout(Value: string);
    procedure SetAdditionalHeader(const Value: TStrings);
    procedure SetBeforeExit;
    procedure SetBindF(const Value: TBindF_Options);
    procedure SetBindF2(const Value: TBindF2_Options);
    procedure SetBindInfoF(const Value: TBindInfoF_Options);
    procedure SetBindInfoOption(const Value: TBindInfoOptions_Options);
    procedure SetBindVerb(const Value: TBindVerb);
    procedure SetCodePage(const Value: TCodePageOption);
    procedure SetDefaultProtocol(const Value: string);
    procedure SetDownloadMethod(const Value: TDownloadMethod);
    procedure SetFileName(const Value: string);
    procedure SetUserAgent;
    procedure Update_BindF_Value;
    procedure Update_BindF2_Value;
    procedure Update_BindInfoF_Value;
    procedure Update_BindInfoOptions_Value;

  public
    ItemsManager: TBSCBList;
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function CheckFileExists(const aFileName: string): boolean;
    function CodeInstallProblemToStr(const ulStatusCode: Integer): string;
    function FormatSize(const Byte: Double): string;
    function FormatTickToTime(const TickCount: Cardinal): string;
    function IsAsyncMoniker(const pmk: IMoniker): HRESULT;
    function IsSynchronous(iedInfo: TInfoData): boolean;
    function IsUrlValid(const isUrl: string): Boolean;
    function OpenFolder(const aFolderName: string): Boolean;
    function ResponseCodeToStr(const dwResponse: Integer): string;
    function SetFileNameFromUrl(const aUrl: string): string;
    function URLDownloadToCacheFile(const aUrl: string): string;
    function UrlDownloadToFile(const aUrl: string): HRESULT;
    function WaitForProcess(var EventName: THandle; var aStartTick,
      aTimeOut: Integer): Boolean;
    function WideStringToLPOLESTR(const Source: string): POleStr;
    procedure BeforeDestruction; override;
    procedure Cancel(const Item: TBSCB); overload;
    procedure Cancel; overload;
    procedure Reset;
    procedure CancelAll;
    procedure Download(const pmk: IMoniker; const pbc: IBindCtx); overload;
    procedure Go(const aUrl: string); overload;
    procedure Go(const aUrl: string; const aFileName: string); overload;
    procedure Go(const aUrl: string; const aFileName: string; const
      aDownloadFolder: string); overload;
    procedure GoList(const UrlsList: TStrings); overload;
    procedure GoList(const UrlsList: TStrings; const FileNameList: TStrings);
      overload;
    procedure GoList(const UrlsList: TStrings; const FileNameList: TStrings;
      const DownloadFolderList: TStrings); overload;
    procedure Loaded; override;
    procedure Resume;
    procedure Suspend;

  public
    property ActiveConnections: integer read FActiveConnections;
    property Busy: Boolean read FBusy;
    property DisplayName: PWideChar read FDisplayName;
    property DownloadedFile: string read FDownloadedFile;
    property DownloadsCounter: integer read FdlCounter;
    property FileExtension: string read FFileExtension;
    property FileSize: ULong read FFileSize;
    property MimeType: string read FMimeType;
    property ServerAddress: string read FServerAddress;
    property ServerIP: string read FServerIP;
    property State: TState read FState;

  published
    property About: string read FAbout write SetAbout;
    property AdditionalHeader: TStrings read FAdditionalHeader write
      SetAdditionalHeader;
    property BindF: TBindF_Options read FBindF write
      SetBindF default [Asynchronous, AsyncStorage, PullData,
      NoWriteCache, GetNewestVersion];
    property BindF2: TBindF2_Options read FBindF2 write
      SetBindF2 default [ReadDataOver4GB];
    property BindInfoF: TBindInfoF_Options read FBindInfoF write
      SetBindInfoF default [];
    property BindVerb: TBindVerb read FBindVerb write
      SetBindVerb default Get;
    property BindInfoOptions: TBindInfoOptions_Options read FBindInfoOption_
      write SetBindInfoOption default [UseBindInfoOptions,
        AllowConnectMessages];
    property CodePage: TCodePageOption read FCodePageOption write
      SetCodePage default Ansi;
    property CustomVerb: string read FCustomVerb write FCustomVerb;
    property DefaultProtocol: string read FDefaultProtocol write
      SetDefaultProtocol;
    property DefaultUrlFileName: string read FDefaultUrlFileName write
      FDefaultUrlFileName;
    property DownloadFolder: string read FDownloadFolder write
      FDownloadFolder;
    property DownloadMethod: TDownloadMethod read FDownloadMethod write
      SetDownloadMethod default dmFile;
    property ExtraInfo: string read FExtraInfo write FExtraInfo;
    property FileExistsOption: TFileExistsOption read FFileExistsOption write
      FFileExistsOption default feOverwrite;
    property FileName: string read FFileName write SetFileName;
    property OnAuthenticate: TAuthenticateEvent read FOnAuthenticate
      write FOnAuthenticate;
{$IFDEF DELPHI6_UP}
    property OnAuthenticateEx: TAuthenticateExEvent read FOnAuthenticateEx
      write FOnAuthenticateEx;
    property OnPutProperty: TOnPutPropertyEvent read FOnPutProperty write
      FOnPutProperty;
{$ENDIF}
    property OnBeforeDownload: TOnBeforeDownloadEvent read FOnBeforeDownload
      write FOnBeforeDownload;
    property OnBeginningTransaction: TBeginningTransactionEvent read
      FBeginningTransaction write FBeginningTransaction;
    property OnCodeInstallProblem: TOnCodeInstallProblemEvent read
      FOnCodeInstallProblem write FOnCodeInstallProblem;
    property OnDataAvailable: TOnDataAvailableEvent read
      FOnDataAvailable write FOnDataAvailable;
    property OnDataAvailableInfo: TOnDataAvailableInfoEvent read
      FOnDataAvailableInfo write FOnDataAvailableInfo;
    property OnConnect: TOnConnectEvent read FOnConnect write FOnConnect;
    property OnComplete: TOnCompleteEvent read FOnComplete write FOnComplete;
    property OnStreamComplete: TOnStreamCompleteEvent read
      FOnStreamComplete write FOnStreamComplete;
    property OnError: TErrorEvent read FOnError write FOnError;
    property OnGetBindResults: TOnGetBindResultsEvent read
      FOnGetBindResults write FOnGetBindResults;
    property OnGetBindInfo: TOnGetBindInfoEvent read
      FOnGetBindInfo write FOnGetBindInfo;
    property OnGetBindInfoEx: TOnGetBindInfoExEvent read
      FOnGetBindInfoEx write FOnGetBindInfoEx;
    property OnGetSerializedClientCertContext: TOnGetClientCertEvent read
      FOnGetClientCert
      write FOnGetClientCert;
    property OnGetRootSecurityId: TOnGetRootSecurityIdEvent
      read FOnGetRootSecurityId write FOnGetRootSecurityId;
    property OnGetWindow: TGetWindowEvent read FGetWindow write
      FGetWindow;
    property OnFileExists: TFileExistsEvent read FOnFileExists write
      FOnFileExists;
    property OnProgress: TOnProgressEvent read FOnProgress write
      FOnProgress;
    property OnQueryInterface: TQueryInterfaceEvent read
      FOnQueryInterface write FOnQueryInterface;
    property OnQueryService: TQueryServiceEvent read FOnQueryService write
      FOnQueryService;
    property OnRedirect: TRedirect read FOnRedirect write FOnRedirect;
    property OnResponse: TOnResponseEvent read FOnResponse
      write FOnResponse;
    property OnResume: TOnResumeEvent read FOnResume write FOnResume;
    property OnSecurityProblem: TOnSecurityProblemEvent read FOnSecurityProblem
      write FOnSecurityProblem;
    property OnStartBinding: TOnStartBindingEvent read FOnStartBinding write
      FOnStartBinding;
    property OnStateChange: TStateChangeEvent read FOnStateChange write
      FOnStateChange;
    property OnTerminate: TTerminateEvent read FOnTerminate write FOnTerminate;
    property OnStopBinding: TOnStopBindingEvent read FOnStopBinding
      write FOnStopBinding;
    property OpenDownloadFolder: Boolean read FOpenDownloadFolder write
      FOpenDownloadFolder default False;
    property Password: string read FPassword write FPassword;
    property PostData: string read FPostData write FPostData;
    property ProxySettings: TProxySettings read FProxySettings write
      FProxySettings;
    property PutFileName: string read FPutFileName write FPutFileName;
    property Range: TRange read FRange write FRange;
    property Security: TSecurity read FSecurity write FSecurity;
    property TimeOut: Integer read FTimeOut write FTimeOut default 0;
    property Url: string read FUrl write FUrl;
    property UserAgent: string read FUserAgent write FUserAgent;
    property UserName: string read FUserName write FUserName;
    property UseSystemDownloadFolder: boolean read FUseSystemDownloadFolder write
      FUseSystemDownloadFolder default False;
    property ValidateUrl: boolean read FValidateUrl write FValidateUrl default
      False;
  end;

  TIEDownload = class(TCustomIEDownload)
  published
  end;

var
  ThreadStatusDesc: array[TThreadStatus] of string = ('Running', 'Suspended',
    'Waiting', 'Terminated');

implementation

uses
  IEDownloadStrings, EwbUrl, IEDownloadTools, Forms
{$IFDEF DELPHI6_UP}, StrUtils{$ENDIF};

{TInfoData---------------------------------------------------------------------}

constructor TInfoData.Create;
begin
  inherited Create;
  InfAdditionalHeader := TStringList.Create;
end;

destructor TInfoData.Destroy;
begin {Cleaning out and free our resources}
  Clear;
  Remove(Sender);
  Extract(Self);
  {Its just to make sure we cleanly remove the IEDownload as an object}
  Remove(infSender);
  Extract(Self);
  if Assigned(infAdditionalHeader) then
    FreeAndNil(infAdditionalHeader);
  inherited;
end;
{End of TInfoData--------------------------------------------------------------}

{Proxy Settings-----------------------------------------------------------------}

function TProxySettings.SetProxy(const FullUserAgent, ProxyServer: string):
  Boolean; //mladen
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

{$IFDEF USE_MSHTML}

function TBSCB.OnChanged(dispId: TDispId): HRESULT;
var
  DP: TDispParams;
  vResult: OLEVariant;
  Doc: IHTMLDocument2;
begin
  if (DISPID_READYSTATE = DispId) then
    if Succeeded((Doc as IHTMLDocument2).Invoke(DISPId_READYSTATE, GUId_null,
      LOCALE_System_DEFAULT, DISPATCH_PROPERTYGET, DP, @vResult, nil, nil)) then
      if Integer(vResult) = READYSTATE_COMPLETE then
        PostThreadMessage(GetCurrentThreadId, WM_USER_STARTWALKING, 0, 0);
  Result := S_OK;
end;

function TBSCB.OnRequestEdit(dispId: TDispId): HRESULT;
begin
  Result := E_NOTIMPL;
end;
{$ENDIF}
{Callback procedure--------------------------------------------------------------}
{IAuthenticate Interface
Provides the URL moniker with information to authenticate the user}

function TBSCB.Authenticate(var hwnd: HWnd; var szUserName, szPassWord: LPWSTR):
  HResult;
{Provides the URL moniker with information to authenticate the user.
S_OK Authentication was successful.
E_ACCESSDENIED Authentication failed.
E_INVALIDARG One or more parameters are invalid. }
var
  aUser, aPwd: WideString;
begin
  Result := S_OK;
  hwnd := FSender.FHWnd;
  aUser := EmptyStr;
  aPwd := EmptyStr;
  if Assigned(FSender.FOnAuthenticate) then
    FSender.FOnAuthenticate(Self, hwnd, aUser, aPwd, Result);
  if aUser <> EmptyStr then
    szUserName := WidestringToLPOLESTR(aUser)
  else
    szUserName := nil;
  if aPwd <> EmptyStr then
    szPassWord := WidestringToLPOLESTR(aPwd)
  else
    szPassWord := nil;
end;

{IHttpNegotiate Interface
Implemented by a client application to provide support for HTTP negotiations}

function TBSCB.BeginningTransaction(szURL, szHeaders: LPCWSTR; dwReserved:
  DWORD; out szAdditionalHeaders: LPWSTR): HRESULT;
{IHttpNegotiate::BeginningTransaction Method
Notifies the client of the URL that is being bound to at the beginning of an HTTP transaction.
S_OK The HTTP transaction completed successfully and any additional headers specified have been appended.
E_ABORT The HTTP transaction has been terminated.
E_INVALIDARG A parameter is invalid.}
var
  sr: TSearchRec;
  Action: Cardinal;
  tmpNewName: WideString;
  NewHeaders: string;
  Size: Longint;
  x, Len: Integer;
  ActExists: TFileExistsOption;
begin
  ActExists := FSender.FFileExistsOption;
  tmpNewName := '';
  dwReserved := 0;
  if (FSender.FCancel) and (Binding <> nil) then
  begin
    Result := E_ABORT;
    binding.Abort;
    Exit;
  end;
  NewHeaders := FSender.FFullUserAgent + #13 + #10;
  if (BscbInfo.infFileName <> EmptyStr) then
  begin
    if FindFirst(BscbInfo.infFileName, faAnyFile, sr) = 0 then
    begin
      Size := sr.Size;
      FindClose(sr);
      BscbInfo.infRangeEnd := 0;
      Action := 0;

      {IBinding still do not support resume (By MS 4.2009)}
      if Assigned(FSender.FOnResume) then
      begin
        FSender.FOnResume(Self, BscbInfo.infFileName, Action);
        BscbInfo.infRangeBegin := Size;
      end;

      if Assigned(FSender.FOnFileExists) then
        FSender.FOnFileExists(ActExists, BscbInfo.infFileName, tmpNewName);

      if tmpNewName = EmptyStr then
        tmpNewName := TimeToStr(now) + '_' + BscbInfo.infFileName;
      case ActExists of
        feOverwrite:
          begin
            Binding.Resume;
            if Assigned(FSender.FOnResume) then
              FSender.FOnResume(Self, BscbInfo.infFileName, Action);
            BscbInfo.infRangeBegin := 0
          end;
        feSkip:
          begin
            Result := E_ABORT;
            Binding.Abort;
            Exit;
          end;
        feRename: BscbInfo.infFileName := tmpNewName;
      end
    end;
  end
  else {Download is starting}
  begin {Set the range to 0 which means start download from scratch}
    BscbInfo.infRangeBegin := 0;
    BscbInfo.infRangeEnd := 0;
  end;

  if ((BscbInfo.infRangeBegin <> 0) or (BscbInfo.infRangeEnd <> 0)) then
  begin {We set the new headers to send to the server}
    NewHeaders := NewHeaders + 'Range: bytes=' +
      IntToStr(BscbInfo.infRangeBegin) + '-';
    if BscbInfo.infRangeEnd <> 0 then
      NewHeaders := NewHeaders + IntToStr(BscbInfo.infRangeEnd) + #13#10
    else
      NewHeaders := NewHeaders + #13#10;
  end;
  if (BscbInfo.infAdditionalHeader.Text <> EmptyStr) then
    for x := 0 to BscbInfo.infAdditionalHeader.Count - 1 do
      NewHeaders := NewHeaders + BscbInfo.infAdditionalHeader[x] + #13#10;
  Len := Length(NewHeaders);
  szAdditionalHeaders := CoTaskMemAlloc((Len + 1) * SizeOf(WideChar));
  StringToWideChar(NewHeaders, szAdditionalHeaders, Len + 1);
  {We will post the event}
  if Assigned(FSender.FBeginningTransaction) then
    Result := FSender.FBeginningTransaction(Self, szURL, szHeaders,
      dwReserved, szAdditionalHeaders)
  else
    Result := S_OK;
  FBSCBTimer.Enabled := True; {Timeout timer}
  FTimedOut := False;
  Self._Release;
end;

function TBSCB.OnResponse(dwResponseCode: DWORD; szResponseHeaders,
  szRequestHeaders: LPCWSTR; out szAdditionalRequestHeaders: LPWSTR): HRESULT;
{Enables the client of a bind operation to examine the response headers,
 optionally terminate the bind operation, and add HTTP headers to a
 request before resending the request.
Returns one of the following values.
S_OK The operation completed successfully.
E_ABORT Terminate the HTTP transaction.
E_INVALIDARG The parameter is invalid.}
var
  Len: Cardinal;
  S: string;
  tmpName: string;
begin
  if (FSender.FCancel) and (Binding <> nil) then
  begin
    Result := E_ABORT;
    binding.Abort;
    Exit;
  end;
  Result := S_OK;
  if (QueryInfo(HTTP_QUERY_CUSTOM, Len) and (Len = 0)) {file size = 0}
  or (QueryInfo(HTTP_QUERY_CONTENT_LENGTH, Len) and (Len = 0)) {file size = 0}
  or (dwResponseCode >= 400) then {An Error}
  begin
    Result := E_ABORT;
    if Assigned(FSender.FOnError) then
      FSender.FOnError(dwResponseCode,
        ResponseCodeToStr(dwResponseCode));
  end;
  begin {Publish the event}
    if Assigned(FSender.FOnResponse) then
      Result := FSender.FOnResponse(Self, dwResponseCode,
        szResponseHeaders, szRequestHeaders, szAdditionalRequestHeaders);
    if (FSender.FDownloadTo = dtDownloadToFile)
      or (FSender.FDownloadTo = dtDownloadToCache) then
    begin
      Result := S_OK;
      Exit;
    end;

    if (BscbInfo.infRangeBegin <> 0) and (BscbInfo.infFileName <> EmptyStr) then
    begin {Retrieves the types of range requests that are accepted for a resource.}
      QueryInfo(HTTP_QUERY_ACCEPT_RANGES, S);
      {'Partial Content'}
      if (S = 'bytes') or (dwResponseCode = 206) then
      begin {Create an output file as a stream back from where we finished}
        tmpName := DoSaveFileAs;
        if tmpName <> EmptyStr then
        begin
          fsOutputFile := TFileStream.Create(tmpName, fmOpenReadWrite);
          fsOutputFile.Seek(0, soFromEnd);
        end;
      end
      else
      begin {'Create an output file as a stream from range begin 0'}
        // not needed
        tmpName := DoSaveFileAs;
        if tmpName <> EmptyStr then
        begin
          fsOutputFile := TFileStream.Create(tmpName, fmCreate);
          BscbInfo.infRangeBegin := 0;
        end;
      end;
    end
    else
    begin {Here we create the file}
      if (FSender.FDownloadMethod = dmFile) then
      begin
        tmpName := DoSaveFileAs;
        if tmpName <> EmptyStr then
        begin
          fsOutputFile := TFileStream.Create(tmpName, fmCreate);
          fsOutputFile.Seek(0, soFromBeginning);
        end;
      end;
    end
  end;
end;

{IHttpNegotiate2 Interface}

function TBSCB.GetRootSecurityId(var SecurityIdBuffer: TByteArray; var
  BufferSize: DWord; dwReserved: DWORD): HResult;
begin {Gets a root security ID.}
  if Assigned(FSender.FOnGetRootSecurityId) then
    Result := FSender.FOnGetRootSecurityId(SecurityIdBuffer, BufferSize)
  else
    Result := E_NOTIMPL;
end;

function TBSCB.GetBindInfoEx(out grfBINDF: DWORD; var pbindinfo: BINDINFO;
  out grfBINDF2: DWORD; out pdwReserved: DWORD): HResult;
var
  PutFile: TFileStream;
  Len: Integer;
begin
  pdwReserved := 0;
  if Assigned(FSender.FOnGetBindInfoEx) then
    FSender.FOnGetBindInfoEx(Self, grfBINDF, pbindinfo, grfBINDF2);
  grfBINDF := BscbInfo.infBindF_Value; {Insert our options.}
  grfBINDF2 := BscbInfo.infBindF2_Value; {Insert our options 2.}
  with pbindinfo do {Lets play with our options.}
  begin
    cbSize := SizeOf(TBindInfo);
    if FRedirect then
    begin {Set method to get in case of redirect}
      dwBindVerb := BINDVERB_GET;
    end
    else {Insert the options}
      dwBindVerb := BscbInfo.infBindVerb_Value;
    grfBindInfoF := BscbInfo.infBindInfoF_Value;
    dwCodePage := BscbInfo.infCodePage_Value;
    {Insert security arguments}
    with SecurityAttributes do
    begin
      nLength := SizeOf(TSecurityAttributes);
      bInheritHandle := BscbInfo.infInheritHandle;
      if BscbInfo.infDescriptor <> '' then
        lpSecurityDescriptor := PAnsiChar(BscbInfo.infDescriptor)
      else
        lpSecurityDescriptor := nil;
    end;
    {Insert Extra Info}
    if BscbInfo.infExtraInfo <> EmptyStr then
    begin
      Len := Length(BscbInfo.infExtraInfo);
      szExtraInfo := CoTaskMemAlloc((Len + 1) * SizeOf(WideChar));
      StringToWideChar(BscbInfo.infExtraInfo, szExtraInfo, Len + 1);
    end
    else
      szExtraInfo := nil;
    case BscbInfo.infBindVerb_Value of
      {Now we will set by our BindVerbOption}
      BINDVERB_PUT: {Perform an HTTP PUT operation. The data to put should be
        specified in the stgmedData member of the BINDINFO structure.}
        if BscbInfo.infPutFileName <> EmptyStr then
        begin
          PutFile := TFileStream.Create(BscbInfo.infPutFileName,
            fmOpenRead);
          try
            PutFile.Seek(0, 0);
            FGlobalData := GlobalAlloc(GPTR, PutFile.Size);
            FDataSize := PutFile.Size;
            PutFile.ReadBuffer(Pointer(FGlobalData)^, PutFile.Size);
          finally
            PutFile.Free;
          end;
        end;
      BINDVERB_POST: {Perform an HTTP POST operation.
        The data to be posted should be specified in the stgmedData
        member of the BINDINFO structure.}
        if BscbInfo.infPostData <> EmptyStr then
        begin
          FGlobalData := GlobalAlloc(GPTR, Length(BscbInfo.infPostData)
            + 1);
          FDataSize := Length(BscbInfo.infPostData) + 1;
          Move(BscbInfo.infPostData[1], Pointer(FGlobalData)^,
            Length(BscbInfo.infPostData));
        end;
      BINDVERB_CUSTOM: {Perform a custom operation that is protocol-specific
        See the szCustomVerb member of the BINDINFO structure.
        The data to be used in the custom operation should be specified
        in the stgmedData structure.}
        if (BscbInfo.infCustomVerb <> EmptyStr) then
        begin
          Len := Length(BscbInfo.infCustomVerb);
          szCustomVerb := CoTaskMemAlloc((Len + 1) * SizeOf(WideChar));
          StringToWideChar(BscbInfo.infCustomVerb, szCustomVerb, Len + 1);
        end
        else {BINDVERB_GET so no need to play arround.}
          szCustomVerb := nil;
    end;
    FillChar(stgmedData, 0, SizeOf(STGMEDIUM));
    cbStgmedData := FDataSize;
    with StgmedData do
    begin
      if dwBindVerb = BINDVERB_GET then
        {The stgmedData member of the BINDINFO
        structure should be set to TYMED_NULL for the GET operation}
        Tymed := TYMED_NULL
      else
        Tymed := TYMED_HGLOBAL;
      {this is the only medium urlmon supports right now}
      hGlobal := FGlobalData;
      IUnknown(unkForRelease) := Self; {Set the IUnknown interface}
    end;
  end;
  Result := S_OK;
end;

{IBindStatusCallback Interface}
{Accepts information on an asynchronous bind operation.}

function TBSCB.GetBindInfo(out grfBINDF: DWORD; var BindInfo: TBindInfo):
  HRESULT;
{Provides information about how the bind operation is handled when
 it is called by an asynchronous moniker.
Returns S_OK if this is successful or E_INVALIDARG if one or more parameters are invalid.}
var
  PutFile: TFileStream;
  Len: Integer;
begin
  grfBINDF := BscbInfo.infBindF_Value; {Insert our options.}
  with BindInfo do {Lets play with our options.}
  begin
    cbSize := SizeOf(TBindInfo);
    if FRedirect then
    begin {Set method to get in case of redirect}
      dwBindVerb := BINDVERB_GET;
    end
    else {Insert the options}
      dwBindVerb := BscbInfo.infBindVerb_Value;
    grfBindInfoF := BscbInfo.infBindInfoF_Value;
    dwCodePage := BscbInfo.infCodePage_Value;
    {Insert security arguments}
    with SecurityAttributes do
    begin
      nLength := SizeOf(TSecurityAttributes);
      bInheritHandle := BscbInfo.infInheritHandle;
      if BscbInfo.infDescriptor <> '' then
        lpSecurityDescriptor := PAnsiChar(BscbInfo.infDescriptor)
      else
        lpSecurityDescriptor := nil;
    end;
    {Insert Extra Info}
    if BscbInfo.infExtraInfo <> EmptyStr then
    begin
      Len := Length(BscbInfo.infExtraInfo);
      szExtraInfo := CoTaskMemAlloc((Len + 1) * SizeOf(WideChar));
      StringToWideChar(BscbInfo.infExtraInfo, szExtraInfo, Len + 1);
    end
    else
      szExtraInfo := nil;
    case BscbInfo.infBindVerb_Value of
      {Now we will set by our BindVerbOption}
      BINDVERB_PUT: {Perform an HTTP PUT operation. The data to put should be
        specified in the stgmedData member of the BINDINFO structure.}
        if BscbInfo.infPutFileName <> EmptyStr then
        begin {Create a process to put a file}
          PutFile := TFileStream.Create(BscbInfo.infPutFileName,
            fmOpenRead);
          try
            PutFile.Seek(0, 0);
            FGlobalData := GlobalAlloc(GPTR, PutFile.Size);
            FDataSize := PutFile.Size;
            PutFile.ReadBuffer(Pointer(FGlobalData)^, PutFile.Size);
          finally
            PutFile.Free;
          end;
        end;

      BINDVERB_POST: {Perform an HTTP POST operation.
        The data to be posted should be specified in the stgmedData
        member of the BINDINFO structure.}
        if BscbInfo.infPostData <> EmptyStr then
        begin
          FGlobalData := GlobalAlloc(GPTR, Length(BscbInfo.infPostData)
            + 1);
          FDataSize := Length(BscbInfo.infPostData) + 1;
          Move(BscbInfo.infPostData[1], Pointer(FGlobalData)^,
            Length(BscbInfo.infPostData));
        end;
      BINDVERB_CUSTOM: {Perform a custom operation that is protocol-specific
        See the szCustomVerb member of the BINDINFO structure.
        The data to be used in the custom operation should be specified
        in the stgmedData structure.}
        if (BscbInfo.infCustomVerb <> EmptyStr) then
        begin
          Len := Length(BscbInfo.infCustomVerb);
          szCustomVerb := CoTaskMemAlloc((Len + 1) * SizeOf(WideChar));
          StringToWideChar(BscbInfo.infCustomVerb, szCustomVerb, Len + 1);
        end
        else {BINDVERB_GET so no need to play arround.}
          szCustomVerb := nil;
    end;
    FillChar(stgmedData, 0, SizeOf(STGMEDIUM));
    cbStgmedData := FDataSize;
    with StgmedData do
    begin
      if dwBindVerb = BINDVERB_GET then
        {The stgmedData member of the BINDINFO
        structure should be set to TYMED_NULL for the GET operation}
        Tymed := TYMED_NULL
      else
        Tymed := TYMED_HGLOBAL;
      {this is the only medium urlmon supports right now}
      hGlobal := FGlobalData;
      IUnknown(unkForRelease) := Self; {Set the IUnknown interface}
    end;
  end;
  if Assigned(FSender.FOnGetBindInfo) then
    FSender.FOnGetBindInfo(Self, grfBINDF, BindInfo);
  Result := S_OK;
end;

function TBSCB.GetPriority(out nPriority): HRESULT;
{Gets the priority for the bind operation when it is called by an asynchronous moniker.}
{Returns S_OK if this is successful or E_INVALIDARG if the pnPriority parameter is invalid.}
begin {if you want to set priority you should implement SetPriority in your application}
  Result := S_OK;
  if (FSender.FCancel) and (Binding <> nil) then
    binding.Abort
end;

function TBSCB.OnDataAvailable(grfBSCF, dwSize: DWORD; FormatEtc: PFormatEtc;
  stgmed: PStgMedium): HRESULT;
{Provides data to the client as it becomes available during
asynchronous bind operations.OnDataAvailable return E_PENDING
when they reference data not yet available through their read
methods, rather than blocking until the data becomes available.
 This flag applies only to ASYNCHRONOUS operations}
{Returns S_OK if this is successful or E_INVALIDARG if one or more parameters are invalid.}
var
  Data: PByte;
  BufL, dwRead, dwActuallyRead: Cardinal;
begin
  if (FSender.FCancel) and (Binding <> nil) then
    binding.Abort
  else
  begin

    if Assigned(FSender.FOnDataAvailableInfo) then
      FSender.FOnDataAvailableInfo(Self, grfBSCF,
        DataAvalibleToStr(grfBSCF) {, FormatEtc});

    if Assigned(FBSCBTimer) then {reset our timer.}
    begin
      FBSCBTimer.Enabled := False;
      FBSCBTimer.Enabled := True;
    end;

    if (grfBSCF = grfBSCF or BSCF_FIRSTDATANOTIFICATION) then
    begin

      if (fOutStream = nil) and (stgmed.tymed = TYMED_ISTREAM) then
        fOutStream := IStream(stgmed.stm);
      if Assigned(m_pPrevBSCB) and not Assigned(fsOutputFile)
        and (BscbInfo.infFileName <> '') then
        try
          //TODO: check for resume
          fsOutputFile := TFileStream.Create(DoSaveFileAs, fmCreate);
          BscbInfo.infRangeBegin := 0;
        except on EFCreateError do
          begin
            Binding.Abort;
            Result := E_FAIL;
            Exit;
            fsOutputFile.Free;
          end;
        end;
    end;
    dwRead := dwSize - FTotalRead;
    dwActuallyRead := 0;
    if (dwRead > 0) then
      repeat
        Data := AllocMem(dwRead + 1);
        fOutStream.Read(Data, dwRead, @dwActuallyRead);
        BufL := dwActuallyRead;
        if Assigned(FSender.FOnDataAvailable) then
        begin
          FSender.FOnDataAvailable(self, Data, BufL);
        end;
        if (BscbInfo.infFileName <> '') and Assigned(fsOutputFile) then
        begin
          fsOutputFile.WriteBuffer(Data^, BufL);
        end
        else if Assigned(Stream) then
          Stream.WriteBuffer(Data^, BufL);
        Inc(FTotalRead, dwActuallyRead);
        FreeMem(Data);
      until dwActuallyRead = 0;
  end;
  Result := S_OK;
  {if (grfBSCF = grfBSCF or BSCF_FIRSTDATANOTIFICATION) then
  begin
    if (fOutStream = nil) and (stgmed.tymed = TYMED_ISTREAM) then
      fOutStream := IStream(stgmed.stm);
    if Assigned(m_pPrevBSCB) and not Assigned(fsOutputFile)
     //and (BscbInfo.infFileName <> '')
      then
     // and (FSender.FDownloadMethod = dmFile) then
    try
     fsOutputFile := TFileStream.Create(DoSaveFileAs, fmCreate);
      BscbInfo.infRangeBegin := 0;
    except on EFCreateError do
      begin
        Binding.Abort;
        Result := E_INVALIDARG;
        if Assigned(FSender.FOnError) then
          FSender.FOnError(GetLastError, SysErrorMessage(GetLastError));
        fsOutputFile.Free;
        Exit;
      end;
    end;
  end;
  dwRead := dwSize - FTotalRead;
  dwActuallyRead := 0;
  if (dwRead > 0) then
    repeat
      Data := AllocMem(dwRead + 1); //to fix stack overflow
      fOutStream.Read(Data, dwRead, @dwActuallyRead);
      BufL := dwActuallyRead;
      if Assigned(FSender.FOnDataAvailable) then
        FSender.FOnDataAvailable(Self, Data, Bufl);
      try
        Stream.WriteBuffer(Data^, Bufl);
      except
        on EWriteError do
        begin
          Binding.Abort;
          Result := E_INVALIDARG;
          if Assigned(FSender.FOnError) then
            FSender.FOnError(GetLastError, SysErrorMessage(GetLastError));
          fsOutputFile.Free;
          Exit;
        end;
      end;

      if (FSender.FDownloadMethod = dmFile) and Assigned(fsOutputFile) then
      begin
        try
          fsOutputFile.WriteBuffer(Data^, bufl);
        except
          on EWriteError do
          begin
            Binding.Abort;
            Result := E_INVALIDARG;
            if Assigned(FSender.FOnError) then
              FSender.FOnError(GetLastError, SysErrorMessage(GetLastError));
            fsOutputFile.Free;
            Exit;
          end
        end;
      end;
      Inc(FTotalRead, dwActuallyRead);
      FreeMem(Data);
    until dwActuallyRead = 0;
end;
Result := S_OK;}
end;

function TBSCB.OnLowResource(Reserved: DWORD): HRESULT;
{Not implemented by MS.}
begin
  Result := E_NOTIMPL;
end;

function TBSCB.OnObjectAvailable(const IID: TGUID; punk: IUnknown): HRESULT;
{Passes the requested object interface pointer to the client.}
{Returns S_OK if this is successful or E_INVALIDARG if one or more parameters are invalid.}
begin
  Self._AddRef;
  if (FSender.FCancel) and (Binding <> nil) then
    binding.Abort;
  Result := S_OK;
end;

function TBSCB.OnProgress(ulProgress, ulProgressMax, ulStatusCode: ULONG;
  szStatusText: LPCWSTR): HRESULT;
{Indicates the progress and the status of the bind operation.}
{Returns S_OK if this is successful or E_INVALIDARG if one or more parameters are invalid.}
{Avalible flags: http://msdn.microsoft.com/en-us/library/ms775133(VS.85).aspx}
var
  Percent, Speed, Elapsed, Downloaded, RemainingTime, Status: string;
  _Speed: Single;
  bAbort: Boolean;
  tmpElapsed, iFileSize: integer;
begin
  if (FSender.FCancel) and (Binding <> nil) then
    Binding.Abort
  else
  begin
    tmpElapsed := 0;
    bAbort := False;
    Status := ResponseCodeToStr(ulStatusCode);
    if (ulProgress > ulProgressMax) then
      ulProgressMax := ulProgress;
    iFileSize := ulProgressMax;
    FSender.FFileSize := ulProgressMax;
    {For a download manager}
    if Assigned(m_pPrevBSCB) then
    begin
      {Weed to do this otherwise a filedownload dlg will be displayed
      as we are downloading the file.}
      if (ulStatusCode = BINDSTATUS_CONTENTDISPOSITIONATTACH) then
      begin
        Result := S_OK;
        Exit; {We must exit so no DLG will be displayed}
      end;
      m_pPrevBSCB.OnProgress(ulProgress, ulProgressMax, ulStatusCode,
        szStatusText);
    end;
    case ulStatusCode of
      BINDSTATUS_REDIRECTING: {redirecting}
        begin
          FRedirect := True;
          FSender.FServerAddress := szStatusText;
          if (Assigned(FSender.FOnRedirect)) and
            (FSender.FUrl <> szStatusText) then
            FSender.FOnRedirect(Self, bAbort, FSender.FUrl, szStatusText);
          if bAbort then {If we do not wish to be redirect}
          begin
            FSender.FCancel := True;
            Result := E_INVALIDARG;
            Exit;
          end;
          {Get the new addreess after redirecing}
          if (FSender.FDownloadMethod = dmFile) then
            FSender.SetFileNameFromUrl(szStatusText);
        end;
      BINDSTATUS_CONNECTING: FSender.FServerIP := szStatusText;
      BINDSTATUS_MIMETYPEAVAILABLE: FSender.FMimeType := szStatusText;
      BINDSTATUS_BEGINDOWNLOADDATA: FSender.FServerAddress := szStatusText;
      BINDSTATUS_DOWNLOADINGDATA: {We are downloading so here we will calculate download variables}
        if Assigned(FSender.FOnProgress) then
        begin
          if (ulProgress {+ BscbInfo.infRangeBegin} > 0) then
            Downloaded := FormatSize(ulProgress {+ BscbInfo.infRangeBegin});
          if (ulProgressMax > 0) and (ulProgress > 0) then
            Percent := Format('%.1f %%', [ulProgress / ulProgressMax * 100]);
          QueryPerformanceCounter(TimeNow);
          if (TimeNow > TimeStarted)
            {and (Round((TimeNow-TimeStarted)/Frequency) <= tmpElapsed)}then
          begin
            tmpElapsed := Round((TimeNow - TimeStarted) / Frequency);
            Elapsed := SecToStr(tmpElapsed);
          end;
          try
            if (ulProgress > 0) and (tmpElapsed > 0) then
              _Speed := ulProgress / 1024 / tmpElapsed
            else
              _Speed := 0;
            Speed := Format('%.1f ' + kb_sec, [_Speed]);
            if (ulProgressMax > 0) and ((_Speed) > 0) and (ulProgressMax >
              ulProgress) then
              RemainingTime := SecToStr(Round(ulProgressMax / _speed / 1000)
                - Round(ulProgress / _speed / 1000))
            else
              RemainingTime := TimeToStr(0);
          except
            on EZeroDivide do
              RemainingTime := TimeToStr(0);
          end;
        end;

      BINDSTATUS_ENDDOWNLOADDATA: {You are joking right? NO MORE DATA TO DOWNLOAD}
        begin
          Downloaded := done;
          ulProgress := 0;
          ulProgressMax := 0;
          Speed := '0/00' + kb_sec;
          RemainingTime := '00.00.00';
          Status := done;
          Percent := '100%';
        end;
      {Here you can add more handlers to any BINDSTATUS_ you like}
    end;

    if Assigned(FSender.FOnProgress) then {Publish the event}
      FSender.FOnProgress(Self, ulProgress {+ BscbInfo.infRangeBegin},
        ulProgressMax {+ BscbInfo.infRangeBegin}, ulStatusCode, iFileSize,
          szStatusText,
        Downloaded, Elapsed, Speed, RemainingTime, Status, Percent);
  end;
  Result := S_OK;
end;

function TBSCB.GetFileNameFromUrl(Url: string): string;
var
  Ut: TUrl;
begin
  Ut := TUrl.Create(Url);
  try
    Ut.CrackUrl(Url, ICU_ESCAPE);
    if AnsiPos('.', Ut.ExtraInfo) = 0 then
      Result := FSender.FDefaultUrlFileName
    else
      Result := Ut.ExtraInfo;
  finally
    Ut.Free;
  end;
end;

function TBSCB.OnStartBinding(dwReserved: DWORD; pib: IBinding): HRESULT;
{Notifies the client about the callback methods that it is registered to receive.}
{Returns S_OK if this is successful or
 E_INVALIDARG if the pib parameter is invalid.
To abort the binding we should return E_FAIL.}
var
  bAbort: Boolean;
begin
  //dwReserved:= 0; // A demand by ms that is not needed.}
  if FSender.FCancel then
    Result := E_FAIL
  else
  begin

    Result := S_OK;
    bAbort := False;
    Binding := pib; {A pointer to the IBinding interface}
    Binding._AddRef; {To be released on StopBinding}
    {We will try to get the file size using query info}
    QueryInfo(HTTP_QUERY_CONTENT_LENGTH, BscbInfo.infFileSize);
    QueryInfoFileName;
    if Assigned(FSender.FOnBeforeDownload) then
      FSender.FOnBeforeDownload(BscbInfo, BscbInfo.infUrl, BscbInfo.infFileName,
        BscbInfo.infFileExt, BscbInfo.infHost, BscbInfo.infDownloadFolder,
        BscbInfo.infFileSize, bAbort);

    {For the download manager}
    FSender.FFileName := BscbInfo.infFileName;
    FSender.FDownloadFolder := BscbInfo.infDownloadFolder;
    if Assigned(m_pPrevBSCB) then
      m_pPrevBSCB.OnStopBinding(HTTP_STATUS_OK, nil);

    {Remove file name which is not needed for stream}
    case FSender.FDownloadMethod of
      dmStream: BscbInfo.infFileName := EmptyStr;
      dmFile:
        begin {Try # 2}
          if (BscbInfo.infFileName = EmptyStr) and (FSender.FDownloadTo =
            dtMoniker) then
            BscbInfo.infFileName := GetFileNameFromUrl(FSender.FUrl)
          else
          begin
            if (BscbInfo.infFileName = EmptyStr) and (not FSender.bRenamed) and
              (BscbInfo.infFileName <> GetFileNameFromUrl(BscbInfo.infUrl)) then
              BscbInfo.infFileName := GetFileNameFromUrl(BscbInfo.infUrl);
          end;
        end;
    end;
    if Assigned(FSender.FOnStartBinding) then
      FSender.FOnStartBinding(Self, bAbort, Binding, BscbInfo.infFileName,
        BscbInfo.infFileSize);
    if bAbort then
    begin {Note: We are still in busy state until OnStopBinding!!}
      Result := E_FAIL; {Do not use Binding.Abort! Just send E_FAIL}
      FSender.FCancel := True;
    end;
  end;
end;

function TBSCB.OnStopBinding(HRESULT: HRESULT; szError: LPCWSTR): HRESULT;
{This method indicates the end of the bind operation.
Returns S_OK if this is successful or an error value otherwise.}
var
  clsidProtocol: TCLSID;
  dwResult: DWORD;
  szResult: POLEStr;
  HR: System.HRESULT;
begin //OK
  if (FSender.FDownloadTo = dtDownloadToFile)
    or (FSender.FDownloadTo = dtDownloadToCache) then
  begin
    Result := S_OK;
    Exit;
  end;

  if (Assigned(m_pPrevBSCB) and Assigned(FBindCtx)) then
    {Stores an IUnknown pointer on the specified object }
  begin {To be used with a download manager}
    HR := FBindCtx.RegisterObjectParam('_BSCB_Holder_', m_pPrevBSCB);
    if Failed(HR) and Assigned(FSender.FOnError) then
      FSender.FOnError(GetLastError, SysErrorMessage(GetLastError))
    else if (Assigned(FSender.FOnConnect)) then
      FSender.FOnConnect(Self, HR, Registering_new_moniker +
        ResponseCodeToStr(HR));
    m_pPrevBSCB._Release;
    m_pPrevBSCB := nil;
    FBindCtx._Release;
    FBindCtx := nil;
    Dec(FSender.FRefCount);
  end;

  GetBindResult(clsidProtocol, dwResult, szResult);
  if FTimedOut then
  begin {If we reached TimeOut them we will post the event}
    HRESULT := INET_E_CONNECTION_TIMEOUT;
    if Assigned(FSender.FOnError) then
      FSender.FOnError(HRESULT, ResponseCodeToStr(HRESULT));
  end;
  if Assigned(FSender.FOnStopBinding) then
    FSender.FOnStopBinding(Self, HRESULT, szError);
  Result := HRESULT;
  FSender.FState := sStopped;
  if Assigned(FSender.FOnStateChange) then
    FSender.FOnStateChange(FSender.FState);

  if Assigned(FSender.FOnStreamComplete) then
    FSender.FOnStreamComplete(Self, Stream, HRESULT);
  FSender.bDone := True;
  SetEvent(FSender.hStop);
  Terminate;
  Dec(FSender.FActiveConnections);
  if FSender.FActiveConnections = 0 then
    SetEvent(FSender.hProcess);
end;

{IServiceProvider Interface}

function TBSCB.QueryService(const rsid, iid: TGUID; out Obj): HRESULT;
begin
  Pointer(Obj) := nil;
  if Assigned(FSender.FOnQueryService) then
    FSender.FOnQueryService(Self, rsid, iid, IUnknown(obj));
  if Pointer(Obj) <> nil then
    Result := S_OK
  else
    Result := E_NOINTERFACE;
end;

{ICodeInstall Interface}

function TBSCB.OnCodeInstallProblem(ulStatusCode: ULONG; szDestination,
  szSource: LPCWSTR; dwReserved: DWORD): HResult; stdcall;
{Returns a value based on the status passed in, which indicates
whether to abort the application installation or file download.
S_OK Indicates that the installation or download should continue.
E_ABORT Indicates that the installation or download should abort.}
begin
  dwReserved := 0;
  if Assigned(FSender.FOnCodeInstallProblem) then
    Result := FSender.FOnCodeInstallProblem(Self, ulStatusCode, szDestination,
      szSource, dwReserved, ResponseCodeToStr(ulStatusCode))
  else
    Result := S_OK;
end;

{IUnknown Interface}

function TBSCB.QueryInterface(const IID: TGUID; out Obj): HRESULT;
{S_OK if the interface is supported, E_NOINTERFACE if not.}
begin
  Self._AddRef;
  if Assigned(FSender.OnQueryInterface) then
    FSender.OnQueryInterface(IID, Obj);
  if GetInterface(IID, Obj) then
    Result := 0
  else
    Result := E_NOINTERFACE;
end;

function TBSCB._AddRef: Integer;
{The IUnknown::AddRef method increments the reference count for
 an interface on an object.}
begin
  Result := InterlockedIncrement(FSender.FRefCount);
end;

function TBSCB._Release: Integer;
{Decrements the reference count for the calling interface on a object. }
begin
  Result := InterlockedDecrement(FSender.FRefCount);
  if Result = 0 then
    Destroy;
end;

{IWindowForBindingUI Interface}

function TBSCB.GetWindow(const GUIDReason: TGUID; out hwnd): HRESULT;
{Returns S_OK if the window handle was successfully returned,
 or E_INVALIDARG if the phwnd parameter is invalid.
 If you implement this interface, you can return S_FALSE
 for this method to indicate that no window is available for
 to display user interface information.}
begin
  if Assigned(FSender.FGetWindow) then
    Result := FSender.FGetWindow(Self, GUIDReason, LongWord(hwnd))
  else
    Result := S_OK;
end;

{IHttpSecurity}

function TBSCB.OnSecurityProblem(dwProblem: DWORD): HResult;
{RPC_E_RETRY The calling application should continue or retry the download.
S_FALSE The calling application should open a dialog box to warn the user.
E_ABORT The calling application should abort the download.}
begin
  if Assigned(FSender.FOnSecurityProblem) then
    Result := FSender.FOnSecurityProblem(Self, dwProblem,
      ResponseCodeToStr(dwProblem))
  else
    Result := S_FALSE;
end;

function TBSCB.GetSerializedClientCertContext(out ppbCert: Byte; var pcbCert:
  DWORD): HResult; stdcall;
begin
  if Assigned(FSender.FOnGetClientCert) then
    Result := FSender.FOnGetClientCert(Self, ppbCert, pcbCert)
  else
    Result := S_FALSE;
end;
{$IFDEF DELPHI6_UP}

function TBSCB.AuthenticateEx(out phwnd: HWND; out pszUsername,
  pszPassword: LPWSTR; var pauthinfo: AUTHENTICATEINFO): HResult; stdcall;
var
  aUser, aPwd: WideString;
  tmpHWND: HWND;
begin
  Result := S_OK;
  phwnd := FSender.FHWnd;
  aUser := EmptyStr;
  aPwd := EmptyStr;
  if Assigned(FSender.FOnAuthenticateEx) then
    FSender.FOnAuthenticateEx(Self, tmpHWND, aUser, aPwd,
      pauthinfo, Result);
  if aUser <> EmptyStr then
    pszUserName := WidestringToLPOLESTR(aUser)
  else
    pszUserName := nil;
  if aPwd <> EmptyStr then
    pszPassWord := WidestringToLPOLESTR(aPwd)
  else
    pszPassWord := nil;
end;

function TBSCB.PutProperty(mkp: MONIKERPROPERTY; val: LPCWSTR): HResult;
{This interface is implemented by persistent monikers,
 such as a MIME handler, to get properties about the moniker
 being handled.}
begin
  if Assigned(FSender.FOnPutProperty) then
    Result := FSender.FOnPutProperty(Self, mkp, val)
  else
    Result := E_NOTIMPL;
end;
{$ENDIF}

function TBSCB.GetBindResult(out clsidProtocol: TCLSID; out dwResult: DWORD;
  out szResult: POLEStr): HRESULT;
{Gets the protocol-specific outcome of a bind operation.}
var
  dwReserved: DWORD;
begin
  dwReserved := 0;
  if (Binding <> nil) then
    Result := Binding.GetBindResult(clsidProtocol, dwResult, szResult,
      dwReserved)
  else
    Result := E_FAIL;
  if Assigned(FSender.FOnGetBindResults) then
    FSender.FOnGetBindResults(Self, clsidProtocol, dwResult,
      szResult, ResponseCodeToStr(dwResult));
  if (Result <> S_OK) and (Assigned(FSender.FOnError)) then
    FSender.FOnError(Result, ResponseCodeToStr(Result));
end;

function TBSCB.CheckCancelState: Integer;
begin
  if (FSender.FCancel = True) then
    Result := E_ABORT
  else
    Result := S_OK;
end;

procedure TBSCB.TimerExpired(Sender: TObject);
begin
  FTimedOut := True;
end;

procedure TBSCB.ClearAll;
begin {Reset our resources}
  if Assigned(Binding) then
    Binding.Abort;
  FGlobalData := 0;
  FTotalRead := 0;
  if m_pPrevBSCB <> nil then
    m_pPrevBSCB := nil;
end;

function TBSCB.QueryInfo(dwOption: DWORD; var Info: Cardinal): Boolean;
var
  HttpInfo: IWinInetHttpInfo;
  C: Cardinal;
  BufferLength: Cardinal;
  Reserved, dwFlags: Cardinal;
begin
  if (Assigned(Binding) and (Binding.QueryInterface(IWinInetHttpInfo, HttpInfo)
    = S_OK)) then
  begin
    Info := 0;
    Reserved := 0;
    dwFlags := 0;
    BufferLength := SizeOf(Cardinal);
    Result := not Boolean(HttpInfo.QueryInfo(dwOption or HTTP_QUERY_FLAG_NUMBER,
      @C, BufferLength, dwFlags, Reserved));
    HttpInfo := nil;
    if Result then
      Info := C;
  end
  else
    Result := False;
end;

function TBSCB.QueryInfo(dwOption: DWORD; var Info: string): Boolean;
var
  Buf: array[0..INTERNET_MAX_PATH_LENGTH] of AnsiChar;
  HttpInfo: IWinInetHttpInfo;
  BufLength, dwReserved, dwFlags: Cardinal;
begin
  if (Assigned(Binding) and (Binding.QueryInterface(IWinInetHttpInfo, HttpInfo)
    = S_OK)) then
  begin
    Info := '';
    dwReserved := 0;
    dwFlags := 0;
    BufLength := INTERNET_MAX_PATH_LENGTH + 1;
    Result := not Boolean(HttpInfo.QueryInfo(dwOption, @Buf, BufLength, dwFlags,
      dwReserved));
    HttpInfo := nil;
    if Result then
      Info := string(Buf);
  end
  else
    Result := False;
end;

function TBSCB.QueryInfo(dwOption: DWORD; var Info: TDateTime): Boolean;
var
  HttpInfo: IWinInetHttpInfo;
  SysTime: TSystemtime;
  BufferLength: Cardinal;
  Reserved, dwFlags: Cardinal;
begin
  if (Assigned(Binding) and (Binding.QueryInterface(IWinInetHttpInfo, HttpInfo)
    = S_OK)) then
  begin
    Info := 0;
    Reserved := 0;
    dwFlags := 0;
    BufferLength := SizeOf(TSystemTime);
    Result := not Boolean(HttpInfo.QueryInfo(dwOption or
      HTTP_QUERY_FLAG_SYSTEMTIME,
      @SysTime, BufferLength, dwFlags, Reserved));
    HttpInfo := nil;
    if Result then
      Info := SystemTimeToDateTime(SysTime);
  end
  else
    Result := False;
end;

function TBSCB.DoSaveFileAs: string;
begin
  if FSender.FDownloadMethod = dmFile then
  begin
    with BscbInfo do
    begin
      if (infFileName = EmptyStr) then
        infFileName := FSender.SetFileNameFromUrl(infUrl);
      if (infFileName = EmptyStr) then
        infFileName := FSender.FDefaultUrlFileName;
    end;
    with FSender do
    begin
      FDownloadedFile := BscbInfo.infDownloadFolder + BscbInfo.infFileName;
      FFileExtension := ExtractFileExt(FSender.FDownloadedFile);
      BscbInfo.infFileExt := FFileExtension;
      FFileName := BscbInfo.infFileName;
      FDownloadFolder := BscbInfo.infDownloadFolder;
    end;
    Result := CharReplace(FSender.FDownloadedFile, '?', '_');
    ;
  end;
end;

function TBSCB.QueryInfoFileName: HRESULT;
const
  CD_FILE_PARAM = 'filename=';
var
  i: Integer;
  st, sTmp: string;
  res: Boolean;
begin
  Result := E_FAIL;
  sTmp := '';
  res := QueryInfo(HTTP_QUERY_CONTENT_DISPOSITION, sTmp);
  if not res then
    Exit;
  i := Pos(CD_FILE_PARAM, sTmp);
  if (i > 0) then
  begin
    sTmp := Copy(sTmp, i + Length(CD_FILE_PARAM), Length(sTmp) - i);
    if (sTmp[1] = '"') then
      i := Pos('";', sTmp)
    else
      i := Pos(';', sTmp);
    //TODO: what's happen, if the filename contains a quotion mark?
    if (i > 0) then
      sTmp := Copy(sTmp, 1, i);
    if (sTmp[1] = '"') then
    begin
      st := (Copy(sTmp, 2, Length(sTmp) - 2));
      BscbInfo.infFileName := Copy(sTmp, 2, Length(sTmp) - 2);
    end
    else
      BscbInfo.infFileName := sTmp;
    if (Length(sTmp) > 0) then
      Result := S_OK;
  end;
  FSender.FFileName := BscbInfo.infFileName; {Return Data}
end;

function TBSCB.IsRunning: Boolean;
begin
  if (Succeeded(FMoniker.IsRunning(FBindCtx, FMoniker, nil))) then
    Result := True
  else
    Result := False;
end;

function TBSCB.GetDisplayName: PWideChar;
begin {Expensive operation so I'll do it only once.
  For extra info use MkParseDisplayName }
  if IsRunning then
    FMoniker.GetDisplayName(FBindCtx, nil, Result);
end;

function TBSCB.MkParseDisplayName(var DisplayName: PWideChar): IMoniker;
var
  i: cardinal;
begin
  UrlMon.MkParseDisplayNameEx(FBindCtx, DisplayName, i, Result);
end;

function TBSCB.CreateMoniker(szName: POLEStr; BC: IBindCtx; out mk: IMoniker;
  dwReserved: DWORD): HResult;
begin
  szName := StringToOleStr(BscbInfo.infUrl);
  Result := CreateURLMonikerEx(nil, szName, FMoniker, URL_MK_UNIFORM
    {URL_MK_LEGACY});
end;

function TBSCB.MonikerBindToStorage(Mk: IMoniker; BC: IBindCtx; BSC:
  IBindStatusCallback;
  const iid: TGUID; out pvObj{$IFNDEF DELPHI8_UP}: Pointer{$ENDIF}): HResult;
begin
  Mk := FMoniker;
  BC := FBindCtx;
  BSC := Self;
  Result := Mk.BindToStorage(BC, nil, IStream, fOutStream);
end;

function TBSCB.MonikerBindToObject(Mk: IMoniker; BC: IBindCtx; BSC:
  IBindStatusCallback;
  const iid: TGUID; out pvObj{$IFNDEF DELPHI8_UP}: Pointer{$ENDIF}): HResult;
begin
  Mk := FMoniker;
  BC := FBindCtx;
  BSC := Self;
  Result := Mk.BindToObject(BC, nil, IStream, fOutStream);
end;

function TBSCB.AbortBinding: HRESULT;
begin
  Result := E_Fail;
  if Assigned(Binding) then
    Result := Binding.Abort;
end;

destructor TBSCB.Destroy;
begin {Cleaning out and free our resources}
  ClearAll;
  if Assigned(Stream) then
    FreeAndNil(Stream);
  if Assigned(FBSCBTimer) then
    FreeAndNil(FBSCBTimer);
  if Assigned(fsOutputFile) then
    FreeAndNil(fsOutputFile);
  if (FGlobalData <> 0) then
    GlobalFree(FGlobalData);
  inherited;
end;

constructor TBSCB.Create(aSender: TCustomIEDownload; const pmk: IMoniker;
  const pbc: IBindCtx; CreateSuspended: boolean);
var
  tmp: PWideChar;
begin
  inherited Create(CreateSuspended);
  FreeOnTerminate := False;
  if CreateSuspended then
    ThreadStatus := tsSuspended
  else
    ThreadStatus := tsWaiting;
  Stream := TMemoryStream.Create; {A stream to contain the download}
  FSender := aSender;
  FMoniker := pmk;
  FBindCtx := pbc;
  if FSender.FDownloadTo = dtMoniker then
  begin
    FSender.FBindF := FSender.FBindF + [GetNewestVersion];
    FSender.DoUpdate;
    FMoniker.GetDisplayName(FBindCtx, nil, tmp);
    FSender.FUrl := tmp;
    FSender.ItemsManager.SessionList.Add(tmp);
  end;
end;

procedure TBSCB.SetComponents;
begin {Initial all internals before the process}
  QueryPerformanceFrequency(Frequency);
  QueryPerformanceCounter(TimeStarted);
  ClearAll; {Clearing Internals}
  FBSCBTimer := TTimer.Create(nil); {Creating Timer for a TimeOut option}
  FBSCBTimer.OnTimer := TimerExpired;
  FBSCBTimer.Interval := BscbInfo.infTimeOut;
  FTimedOut := False;
  if not FSender.IsSynchronous(BscbInfo) then {We are on Asynchronous mode}
  begin
    FSender.ItemsManager.Add(Self); {Adding asynchronous items}
    Inc(BscbInfo.infIndex); {Pass the index}
    Inc(FSender.FdlCounter);
  end;
end;

procedure TBSCB.Resume;
begin
  inherited;
  ThreadStatus := tsRunning;
end;

procedure TBSCB.Suspend;
begin
  inherited;
  ThreadStatus := tsSuspended;
end;

procedure TBSCB.Terminate;
var
  bCanc: Boolean;
begin
  if FSender.ActiveConnections = 0 then
    FSender.FBusy := False;
  ThreadStatus := tsTerminated;
  bCanc := False;
  if Assigned(FSender.FOnTerminate) then
    FSender.FOnTerminate(Self, ThreadID, BscbInfo.infFileName, bCanc);
  if bCanc then
    FSender.CancelAll;
  inherited;
end;

procedure TBSCB.Execute;
begin
  if Terminated then
    Exit;
  try
    {Dont be in shock, as a tread it sometimes fail so we should succeed now}
    OleInitialize(nil);
  except
  end;
  ThreadStatus := tsRunning;
  Synchronize(ReceiveData);
  Synchronize(SetComponents);
  case FSender.FDownloadTo of
    dtNormal:
      begin
        Synchronize(DoConnect);
        Synchronize(ReturnData);
      end;
    dtMoniker:
      begin
        Synchronize(DoConnect);
        Synchronize(ReturnData);
      end;
    dtDownloadToCache: Synchronize(DoDownloadToCache);
    dtDownloadToFile: Synchronize(DoDownloadToFile);
  end;
  try
    if (Assigned(BscbInfo)) then
  finally
    BscbInfo.Clear;
    FreeAndNil(BscbInfo);
  end;
  OleUninitialize;
end;

procedure TBSCB.ReceiveData;
begin
  BscbInfo := TInfoData.Create;
  GetData(FSender); {Pass Data to the TObject}
  FSender := TCustomIEDownload(BscbInfo.Sender);
end;

procedure TBSCB.ReturnData;
begin
  with FSender do
  begin
    FDownloadedFile := BscbInfo.infDownloadFolder + BscbInfo.infFileName;
    FFileExtension := ExtractFileExt(FSender.FDownloadedFile);
    BscbInfo.infFileExt := FFileExtension;
    FFileName := BscbInfo.infFileName;
    FDownloadFolder := BscbInfo.infDownloadFolder;
  end;
end;

procedure TBSCB.DoDownloadToCache;
var
  Buf: array[0..INTERNET_MAX_PATH_LENGTH] of char;
begin
  if Succeeded(UrlMon.URLDownloadToCacheFile(nil, Pchar(BscbInfo.infUrl),
    Buf, SizeOf(Buf), 0, Self)) then
    FSender.ExtractDataFromFile(Buf);
  FSender.ItemsManager.Extract(Self);
end;

procedure TBSCB.DoDownloadToFile;
var
  HR: integer;
  tmp: string;
begin
  tmp := BscbInfo.infDownloadFolder + BscbInfo.infFileName;
  HR := UrlMon.URLDownloadToFile(nil, Pchar(BscbInfo.infUrl),
    PChar(tmp), 0, Self);
  if Failed(HR) and Assigned(FSender.FOnError) then
    FSender.FOnError(GetLastError, Err_ToFile + SysErrorMessage(GetLastError))
  else if (Assigned(FSender.FOnConnect)) then
    FSender.FOnConnect(Self, HR, DL_ToFile + ResponseCodeToStr(HR));
  FSender.ExtractDataFromFile(tmp);
  FSender.ItemsManager.Extract(Self);
end;

procedure TBSCB.DoConnect;
var
  Ut: TUrl;
  HR: HRESULT;
  pPrevBSCB, tmpBSC: IBindStatusCallback;
begin
  FSender.bDone := False;
  FSender.hStop := 0;
  if FSender.FDownloadTo <> dtMoniker then
  begin
    HR := CreateURLMonikerEx(nil, BscbInfo.infUrl, FMoniker, URL_MK_UNIFORM
      {URL_MK_LEGACY});
    if Failed(HR) and Assigned(FSender.FOnError) then
    begin
      FSender.FOnError(GetLastError, Err_URLMEx +
        ResponseCodeToStr(HR));
      Exit;
    end
    else if (Assigned(FSender.FOnConnect)) then
      FSender.FOnConnect(Self, HR, CreateURLMEx + ResponseCodeToStr(HR));

    HR := CreateAsyncBindCtx(0, Self, nil, FBindCtx);
    if Failed(HR) and Assigned(FSender.FOnError) then
    begin
      FSender.FOnError(GetLastError, Err_AsyncBindCtx +
        ResponseCodeToStr(HR));
      Exit;
    end
    else if (Assigned(FSender.FOnConnect)) then
      FSender.FOnConnect(Self, HR, CreateABindCtx + ResponseCodeToStr(HR));
  end;

  FSender.FDisplayName := GetDisplayName;
  begin
    if FSender.FDisplayName <> EmptyStr then
    begin
      BscbInfo.infUrl := FSender.FDisplayName;
      FSender.FUrl := FSender.FDisplayName;
    end;
    Ut := TUrl.Create(BscbInfo.infUrl);
    try
      Ut.QueryUrl(BscbInfo.infUrl);
      BscbInfo.infFileName := Ut.Document;
      BscbInfo.infHost := Ut.HostName;
    finally
      Ut.Free;
    end;
  end;

  HR := RegisterBindStatusCallback(FBindCtx, Self, pPrevBSCB, 0);
  if Failed(HR) and Assigned(pPrevBSCB) then
  begin
    HR := FBindCtx.RevokeObjectParam('_BSCB_Holder_');
    if (Succeeded(HR)) then
    begin {Attempt register again, should succeed now}
      HR := RegisterBindStatusCallback(FBindCtx, Self, tmpBSC, 0);
      if (SUCCEEDED(HR)) then
      begin //Need to pass a pointer for BindCtx and previous BSCB to our implementation
        m_pPrevBSCB := pPrevBSCB;
        Self._AddRef;
        m_pPrevBSCB._AddRef;
        FBindCtx._AddRef;
        if (Assigned(FSender.FOnConnect)) then
          FSender.FOnConnect(Self, HR, Reg_BSCB + ResponseCodeToStr(HR));
      end
      else if Assigned(FSender.FOnError) then
        FSender.FOnError(GetLastError, Err_RegBSCB
          + ResponseCodeToStr(HR));
    end;
  end
  else if (Assigned(FSender.FOnConnect)) then
    FSender.FOnConnect(Self, HR, Reg_BSCB +
      ResponseCodeToStr(HR));
  FSender.hStop := CreateEvent(nil, True, False, nil);
  HR := FMoniker.BindToStorage(FBindCtx, nil, IStream, fOutStream);
  if Failed(HR) and Assigned(FSender.FOnError) then
  begin
    FSender.FOnError(GetLastError, Err_BindToSt +
      ResponseCodeToStr(HR));
    Exit;
  end
  else if (Assigned(FSender.FOnConnect)) then
    FSender.FOnConnect(Self, HR, Bind_To_St + ResponseCodeToStr(HR));
  repeat
    try
      if FSender.WaitForProcess(FSender.hStop, FSender.FStartTick,
        FSender.FTimeOut) then
    except
      if Assigned(FSender.FOnError) then
        FSender.FOnError(E_FAIL, Err_Proc_Ev);
      raise;
    end;
  until (FSender.FCancel) or (FSender.bDone)
    {and (stream = nil)}{or (BscbInfo.infIndex = 0)};
  HR := RevokeBindStatusCallback(FBindCtx, pPrevBSCB);
  if Failed(HR) then
    HR := RevokeBindStatusCallback(FBindCtx, tmpBSC);
  if Failed(HR) and Assigned(FSender.FOnError) then
    FSender.FOnError(HR, Err_Revoke + ResponseCodeToStr(HR))
  else if (Assigned(FSender.FOnConnect)) then
    FSender.FOnConnect(Self, HR, Revoke_BSCB + ResponseCodeToStr(S_OK));

  if FSender.FActiveConnections = 0 then
    FSender.FBusy := False;
  if not FSender.IsSynchronous(BscbInfo) then {We are on asynchronous mode}
  begin
    FSender.ItemsManager.Extract(Self);
    {Remove the item from our list because we finished}
    Dec(BscbInfo.infIndex); {Pass the new index}
  end;
end;

procedure TBSCB.GetData(aSender: TCustomIEDownload);
begin {Get data from IEDownload to the iedInfo}
  with BscbInfo do
  begin
    infAdditionalHeader.AddStrings(aSender.FAdditionalHeader);
    infBindF_Value := aSender.FBindF_Value;
    infBindF2_Value := aSender.FBindF2_Value;
    infBindInfoF_Value := aSender.FBindInfoF_Value;
    infBindVerb_Value := aSender.FBindVerb_Value;
    infBindInfoOptions_Value := aSender.FBindVerb_Value;
    infCodePage_Value := aSender.FCodePageValue;
    infCustomVerb := aSender.FCustomVerb;
    infDescriptor := aSender.Security.FDescriptor;
    infDownloadFolder := aSender.FDownloadFolder;
    infExtraInfo := aSender.FExtraInfo;
    infFileName := aSender.FFileName;
    inFFileSize := 0;
    infInheritHandle := aSender.Security.FInheritHandle;
    infPassword := aSender.FPassword;
    infPostData := aSender.FPostData;
    infPutFileName := aSender.FPutFileName;
    infRangeBegin := aSender.Range.FRangeBegin;
    infRangeEnd := aSender.Range.FRangeEnd;
    infTimeOut := aSender.FTimeOut;
    infUrl := StringToOleStr(aSender.FUrl);
    infUserName := aSender.FUserName;
    Sender := aSender;
  end;
end;

{Enf of Callback procedure------------------------------------------------------}

{BSCBList----------------------------------------------------------------------}

function TBSCBList.byURL(Url: string): TBSCB; //by Jury Gerasimov
var
  i: integer;
begin
  Result := nil;
  for i := 0 to Count - 1 do
    if Items[i].BscbInfo.infUrl = Url then
    begin
      Result := Items[i];
      Break;
    end;
end;

function TBSCBList.GetItem(Index: Integer): TBSCB;
begin
  Result := TBSCB(inherited GetItem(Index));
end;

procedure TBSCBList.SetItem(Index: Integer; Value: TBSCB);
begin
  inherited SetItem(Index, Value);
end;

constructor TBSCBList.Create;
begin
  inherited Create;
  SessionList := TStringList.Create;
end;

destructor TBSCBList.Destroy;
begin
  FreeAndNil(SessionList);
  inherited Destroy;
end;

{End of BSCBList---------------------------------------------------------------}

{IEDownload--------------------------------------------------------------------}

constructor TCustomIEDownload.Create(AOwner: TComponent);
begin
  inherited;
{$IFDEF DELPHI7_UP}
  // TO Do:  W1000 Symbol 'GetLocaleFormatSettings' is deprecated: 'Use TFormatSettings.Create(Locale)'
  GetLocaleFormatSettings(SysLocale.DefaultLCID, FFormatSettings);
  FFormatSettings.LongTimeFormat := Frmt_Time;
  // FFormatSettings:= TFormatSettings.Create('');  //use default locale
  FFormatSettings.TimeSeparator := '_'; {For the feRename}
{$ELSE}
  FOldTimeSep := TimeSeparator;
  LongTimeFormat := Frmt_Time;
  TimeSeparator := '_'; {For the feRename}
{$ENDIF}
  FAbout := IED_INFO;
  hProcess := 0;
  bDone := False;
  bCancelAll := False;
  FAdditionalHeader := TStringlist.Create;
  FAdditionalHeader.Add('Content-Type: application/x-www-form-urlencoded ');
  FBindF := [Asynchronous, AsyncStorage, PullData, NoWriteCache,
    GetNewestVersion];
  FBindF2 := [ReadDataOver4GB];
  FBindVerb := Get;
  FCodePageOption := Ansi;
  FBindInfoOption_ := [UseBindInfoOptions, AllowConnectMessages];
  FDefaultProtocol := 'http://';
  FDefaultUrlFileName := 'index.html';
  FdlCounter := 0;
  FActiveConnections := 0;
  FDownloadMethod := dmFile;
  FProxySettings := TProxySettings.Create;
  FProxySettings.FPort := 80;
  FRange := TRange.Create;
  FRefCount := 0;
  FSecurity := TSecurity.Create;
  FState := sReady;
  FBindInfoF := [];
  ItemsManager := TBSCBList.Create;
  SetUserAgent;
end;

procedure TCustomIEDownload.Loaded;
begin
  inherited Loaded;
  if FTimeOut = 0 then
    FTimeOut := MaxInt;
  if (FProxySettings.FAutoLoadProxy) and (FProxySettings.FServer <> EmptyStr)
    then
    FProxySettings.SetProxy(FFullUserAgent, FProxySettings.FServer + ':' +
      IntToStr(FProxySettings.FPort));
end;

procedure TCustomIEDownload.Resume;
begin
  if BS <> nil then
    BS.Resume;
end;

procedure TCustomIEDownload.Suspend;
begin
  if BS <> nil then
    BS.Suspend;
end;

destructor TCustomIEDownload.Destroy;
begin
{$IFNDEF DELPHI7_UP}
  TimeSeparator := FOldTimeSep;
{$ENDIF}
  FTimeOut := 0;
  FRange.Free;
  FSecurity.Free;
  FProxySettings.Free;
  ItemsManager.Free;
  if Assigned(FAdditionalHeader) then
    FreeAndNil(FAdditionalHeader);
  inherited;
end;

procedure TCustomIEDownload.BeforeDestruction;
begin
  if FProxySettings.FAutoLoadProxy then
    FProxySettings.SetProxy(EmptyStr, EmptyStr); {To restore proxy settings}
  inherited BeforeDestruction;
end;

procedure TCustomIEDownload.Cancel;
begin
  if (not FBusy) or (FState <> sBusy) then
    Exit;
  FCancel := True;
  Application.ProcessMessages;
end;

procedure TCustomIEDownload.Reset;
begin
  if (FState = sBusy) then
    Exit;
  FCancel := False;
  bCancelAll := False;
  Application.ProcessMessages;
end;

procedure TCustomIEDownload.CancelAll;
begin
  if (not FBusy) or (FState <> sBusy) then
    Exit;
  bCancelAll := True;
  FCancel := True;
  Application.ProcessMessages;
end;

procedure TCustomIEDownload.Cancel(const Item: TBSCB);
begin
  Item.CheckCancelState;
  FCancel := True;
end;

procedure TCustomIEDownload.Update_BindInfoF_Value;
const
  Acard_BindInfoF_Values: array[TBindInfoF] of Cardinal = (
    $00000001, $00000002);
var
  i: TBindInfoF;
begin
  FBindInfoF_Value := 0;
  if (FBindInfoF <> []) then
    for i := Low(TBindInfoF) to High(TBindInfoF) do
      if (i in FBindInfoF) then
        Inc(FBindInfoF_Value, Acard_BindInfoF_Values[i]);
end;

procedure TCustomIEDownload.Update_BindF_Value;
const
  Acard_BindF_Values: array[TBindF] of Cardinal = (
    $00000001, $00000002, $00000004, $00000008, $00000010, $00000020,
    $00000040, $00000080, $00000100, $00000200, $00000400, $00000800,
    $00001000, $00002000, $00004000, $00008000, $00010000, $00020000,
    $00040000, $00080000, $00100000, $00200000, $00400000, $00800000);
var
  i: TBindF;
begin
  FBindF_Value := 0;
  if (FBindF <> []) then
    for i := Low(TBindF) to High(TBindF) do
      if (i in FBindF) then
        Inc(FBindF_Value, Acard_BindF_Values[i]);
end;

procedure TCustomIEDownload.Update_BindInfoOptions_Value;
const
  AcardBindInfoOption_Values: array[TBindInfoOption] of Cardinal = (
    $00010000, $00020000, $00040000, $00080000, $00100000, $00200000,
    $00400000, $00800000, $01000000, $02000000, $08000000, $10000000,
    $40000000, $80000000, $20000000);
var
  i: TBindInfoOption;
begin
  FBindInfoOption_Value := 0;
  if (FBindInfoOption_ <> []) then
    for i := Low(TBindInfoOption) to High(TBindInfoOption) do
      if (i in FBindInfoOption_) then
        Inc(FBindInfoOption_Value, AcardBindInfoOption_Values[i]);
end;

procedure TCustomIEDownload.Update_BindF2_Value;
const
  AcardBindF2_Values: array[TBindF2] of Cardinal = ($00000001,
    $00000002, $00000004, $00000008, $40000000, $80000000);
var
  i: TBindF2;
begin
  FBindF2_Value := 0;
  if (FBindF2 <> []) then
    for i := Low(TBindF2) to High(TBindF2) do
      if (i in FBindF2) then
        Inc(FBindF2_Value, AcardBindF2_Values[i]);
end;

function TCustomIEDownload.OpenFolder(const aFolderName: string): Boolean;
var
  Int: integer;
begin
  Result := False;
  if (FDownloadMethod = dmFile) then
  begin
    Int := ShellExecute(Forms.Application.Handle, PChar('explore'),
      PChar(aFolderName), nil, nil, SW_SHOWNORMAL);
    Result := (Int > 32);
    if not Result and Assigned(FOnError) then
      FOnError(Int, Err_Folder);
  end;
end;

procedure TCustomIEDownload.DoUpdate;
begin
  Update_BindF_Value;
  Update_BindF2_Value;
  Update_BindInfoF_Value;
  Update_BindInfoOptions_Value;
end;

function TCustomIEDownload.CodeInstallProblemToStr(const ulStatusCode: Integer):
  string;
begin
  Result := IEDownloadTools.CodeInstallProblemToStr(ulStatusCode);
end;

function TCustomIEDownload.CheckFileExists(const aFileName: string): boolean;
begin
  Result := FileExists(aFileName);
end;

procedure TCustomIEDownload.Go(const aUrl: string);
begin
  GoAction(aUrl, EmptyStr, EmptyStr, nil, nil);
  if FOpenDownloadFolder then
    OpenFolder(FDownloadFolder);
end;

procedure TCustomIEDownload.Go(const aUrl: string; const aFileName: string);
begin
  GoAction(aUrl, aFileName, EmptyStr, nil, nil);
  if FOpenDownloadFolder then
    OpenFolder(FDownloadFolder);
end;

procedure TCustomIEDownload.Go(const aUrl: string; const aFileName: string;
  const aDownloadFolder: string);
begin
  GoAction(aUrl, aFileName, aDownloadFolder, nil, nil);
  if FOpenDownloadFolder then
    OpenFolder(FDownloadFolder);
end;

procedure TCustomIEDownload.GoList(const UrlsList: TStrings);
var
  Idx: integer;
begin
  for Idx := 0 to UrlsList.Count - 1 do
    if (UrlsList[Idx] <> EmptyStr) and (not bCancelAll) then
      GoAction(UrlsList[Idx], EmptyStr, EmptyStr, nil, nil);
  if FOpenDownloadFolder then
    OpenFolder(FDownloadFolder);
end;

procedure TCustomIEDownload.GoList(const UrlsList: TStrings; const FileNameList:
  TStrings);
var
  Idx: integer;
begin
  for Idx := 0 to UrlsList.Count - 1 do
    if (UrlsList[Idx] <> EmptyStr) and (not bCancelAll) then
      GoAction(UrlsList[Idx], FileNameList[Idx], EmptyStr, nil, nil);
  if FOpenDownloadFolder then
    OpenFolder(FDownloadFolder);
end;

procedure TCustomIEDownload.GoList(const UrlsList: TStrings; const FileNameList:
  TStrings;
  const DownloadFolderList: TStrings);
var
  Idx: integer;
begin
  for Idx := 0 to UrlsList.Count - 1 do
    if (UrlsList[Idx] <> EmptyStr) and (not bCancelAll) then
      GoAction(UrlsList[Idx], FileNameList[Idx], DownloadFolderList[Idx], nil,
        nil);
  if FOpenDownloadFolder then
    OpenFolder(FDownloadFolder);
end;

procedure TCustomIEDownload.Download(const pmk: IMoniker; const pbc: IBindCtx);
begin
  FDownloadTo := dtMoniker;
  PrepareForStart;
  hProcess := CreateEvent(nil, True, False, nil);
  if (not GoInit('', FFileName, FDownloadFolder)) then
  begin
    PrepareForExit;
    Exit;
  end;
  BS := TBSCB.Create(Self, pmk, pbc, True);
  try
    BS.Execute;
    repeat
      try
        if WaitForProcess(hProcess, FStartTick, FTimeOut) then
      except
        if Assigned(FOnError) then
          FOnError(E_FAIL, Err_Proc_Ev);
        raise;
      end;
    until (FCancel) or (FActiveConnections = 0);
  finally
    FreeAndNil(BS);
  end;
  PrepareForExit;
end;

function TCustomIEDownload.GoAction(const actUrl, actFileName,
  actDownloadFolder: string;
  pmk: IMoniker; pbc: IBindCtx): boolean;
begin
  Result := False;
  PrepareForStart;
  hProcess := CreateEvent(nil, True, False, nil);
  if (not GoInit(actUrl, actFileName, actDownloadFolder)) then
  begin
    PrepareForExit;
    Exit;
  end;
  BS := TBSCB.Create(Self, pmk, pbc, True); {Creating Download Callback}
  try //Fix Deadlock?
    BS.Execute;
    repeat
      try
        if WaitForProcess(hProcess, FStartTick, FTimeOut) then
      except
        if Assigned(FOnError) then
          FOnError(E_FAIL, Err_Proc_Ev);
        raise;
      end;
    until (FCancel) or (FActiveConnections = 0);
  finally
    FreeAndNil(BS);
  end;
  PrepareForExit;
  Result := True;
end;

function TCustomIEDownload.URLDownloadToCacheFile(const aUrl: string): string;
begin
  Result := EmptyStr;
  PrepareForStart;
  if not GoInit(aUrl, '', '') then
    Exit;
  FDownloadTo := dtDownloadToCache;
  BS := TBSCB.Create(Self, nil, nil, True);
  try
    BS.Execute;
    BS.Terminate;
    Dec(FActiveConnections);
  finally
    FreeAndNil(BS);
  end;
  SetBeforeExit;
  PrepareForExit;
  Result := FDownloadFolder;
end;

function TCustomIEDownload.UrlDownloadToFile(const aUrl: string): HRESULT;
begin
  Result := E_FAIL;
  PrepareForStart;
  if not GoInit(aUrl, '', '') then
    Exit;
  FDownloadTo := dtDownloadToFile;
  BS := TBSCB.Create(Self, nil, nil, True);
  try
    BS.Execute;
    BS.Terminate;
    Dec(FActiveConnections);
  finally
    FreeAndNil(BS);
  end;
  SetBeforeExit;
  PrepareForExit;
  Result := S_OK;
end;

procedure TCustomIEDownload.SetBeforeExit;
begin
  if FOpenDownloadFolder then
    OpenFolder(FDownloadFolder);
  if FActiveConnections = 0 then
    FBusy := False;
  FState := sStopped;
  if Assigned(FOnStateChange) then
    FOnStateChange(FState);
end;

function TCustomIEDownload.GoInit(const inUrl: string; const inFileName:
  string; const inDownloadFolder: string): boolean;
var
  tmpNewName: WideString;
  Act: TFileExistsOption;
begin
  act := FFileExistsOption;
  tmpNewName := '';
  Result := False;
  if FDownloadTo <> dtMoniker then
  begin
    if inUrl = EmptyStr then
    begin
      PrepareForExit;
      Exit;
    end;
    FUrl := SetHttpProtocol(inUrl); {We pass the Address we got to the component}
    if (FValidateUrl) and not (IsUrlValid(FUrl)) then
    begin
      PrepareForExit;
      Exit;
    end;
    ItemsManager.SessionList.Add(FUrl);
    if FDownloadMethod = dmFile then
    begin
      FDownloadFolder := SetDownloadFolder(inDownloadFolder);
      if FDownloadFolder = EmptyStr then
        Exit;
      FFileName := inFileName;
      if (FFileName = EmptyStr) then
        FFileName := SetFileNameFromUrl(FUrl); {First try}
      if (CheckFileExists(FDownloadFolder + FFileName)) then
      begin
        if Assigned(FOnFileExists) then
          FOnFileExists(Act, FDownloadFolder + FFileName, tmpNewName);
        case Act of
          feSkip:
            begin
              PrepareForExit;
              Exit;
            end;
          feRename:
            begin
{$IFDEF DELPHI7_UP}
              if tmpNewName = EmptyStr then
                tmpNewName := TimeToStr(now, FFormatSettings) + '_' + FFileName;
{$ELSE}
              if tmpNewName = EmptyStr then
                tmpNewName := TimeToStr(now) + '_' + FFileName;
{$ENDIF}
              FFileName := tmpNewName;
              bRenamed := True;
            end;
          feOverwrite: FBindF := FBindF + [GetNewestVersion];
        end;
      end;
    end
    else
      FBindF := FBindF + [GetNewestVersion];
  end;
  DoUpdate;
  Result := True;
end;

function TCustomIEDownload.WaitForProcess(var EventName: THandle;
  var aStartTick, aTimeOut: Integer): Boolean;
var
  dwResult: DWORD;
  Msg: TMsg;
  EventList: array[0..0] of THandle;
begin
  EventList[0] := EventName;
  dwResult := MsgWaitForMultipleObjects(1, EventList, False, DWORD(ATimeOut),
    QS_ALLEVENTS);

  case dwResult of
    WAIT_FAILED: {Waiting failed}
      begin
        if Assigned(FOnError) then
          FOnError(GetLastError, SysErrorMessage(GetLastError));
      end;
    WAIT_TIMEOUT: {Waiting Timo out}
      begin
        if Assigned(FOnError) then
          FOnError(GetLastError, SysErrorMessage(GetLastError));
      end;
    WAIT_BSCB: {Our state to process messages}
      begin
        while PeekMessage(Msg, 0, 0, 0, PM_REMOVE) do
        begin
          TranslateMessage(Msg);
          DispatchMessage(Msg);
          if (Integer(GetTickCount) - aStartTick > aTimeOut) then
          begin
            if Assigned(FOnError) then
              FOnError(GetLastError, Err_TimeOut);
          end;
        end;
        if (Integer(GetTickCount) - aStartTick > aTimeOut) then
        begin
          if Assigned(FOnError) then
            FOnError(GetLastError, Err_TimeOut);
        end;
      end;
  end;
  Result := (dwResult = WAIT_OBJECT_0); {We are done waiting}
end;

function TCustomIEDownload.IsSynchronous(iedInfo: TInfoData): boolean;
begin {Return True if mode is Synchronous}
  if iedInfo.infBindF_Value <> (iedInfo.infBindF_Value or
    BINDF_ASYNCHRONOUS) then
    Result := True
  else
    Result := False;
end;

function TCustomIEDownload.IsAsyncMoniker(const pmk: IMoniker): HRESULT;
begin
  Result := UrlMon.IsAsyncMoniker(pmk);
end;

function TCustomIEDownload.FormatSize(const Byte: Double): string;
begin
  Result := IEDownloadTools.FormatSize(Byte);
end;

function TCustomIEDownload.FormatTickToTime(const TickCount: Cardinal): string;
begin
  Result := IEDownloadTools.FormatTickToTime(TickCount);
end;

function TCustomIEDownload.IsUrlValid(const isUrl: string): Boolean;
var
  U: TUrl;
begin
  U := TUrl.Create(isUrl);
  try
    Result := U.IsUrlValid(isUrl);
    if not Result and Assigned(FOnError) then
      FOnError(GetLastError, SysErrorMessage(GetLastError) + isUrl);
  finally
    U.Free;
  end;
end;

procedure TCustomIEDownload.PrepareForExit;
begin
  if Assigned(FOnComplete) then
    FOnComplete(Self, FDownloadedFile, FFileName, FDownloadFolder,
      FFileExtension, ActiveConnections);
  FState := sReady;
  if Assigned(FOnStateChange) then
    FOnStateChange(FState);
end;

procedure TCustomIEDownload.PrepareForStart;
begin
  FBusy := True;
  bRenamed := False;
  FCancel := False;
  FDownloadedFile := EmptyStr;
  FDownloadFolder := EmptyStr;
  FFileExtension := EmptyStr;
  FFileName := EmptyStr;
  FFileSize := 0;
  FMimeType := EmptyStr;
  FServerAddress := EmptyStr;
  FServerIP := EmptyStr;
  FUrl := EmptyStr;
  FState := sBusy;
  if Assigned(FOnStateChange) then
    FOnStateChange(FState);
  FStartTick := GetTickCount;
  Inc(FRefCount);
  Inc(FActiveConnections);
end;

procedure TCustomIEDownload.SetCodePage(const Value: TCodePageOption);
begin
  FCodePageOption := Value;
  case FCodePageOption of
    Ansi: FCodePageValue := CP_ACP;
    Mac: FCodePageValue := CP_MACCP;
    OEM: FCodePageValue := CP_OEMCP;
    Symbol: FCodePageValue := CP_SYMBOL;
    ThreadsAnsi: FCodePageValue := CP_THREAD_ACP;
    UTF7: FCodePageValue := CP_UTF7;
    UTF8: FCodePageValue := CP_UTF8;
  end;
end;

procedure TCustomIEDownload.SetBindVerb(const Value: TBindVerb);
begin {Contains values that specify an action, such as an HTTP request, to be performed during the binding operation.}
  FBindVerb := Value;
  case FBindVerb of
    Get: FBindVerb_Value := BINDVERB_GET;
    Put: FBindVerb_Value := BINDVERB_PUT;
    Post: FBindVerb_Value := BINDVERB_POST;
    Custom: FBindVerb_Value := BINDVERB_CUSTOM;
  end;
end;

procedure TCustomIEDownload.SetFileName(const Value: string);
begin
  FFileName := Value;
end;

function TCustomIEDownload.SetFileNameFromUrl(const aUrl: string): string;
var
  Ut: TUrl;
  sTmp1, sTmp2: string;
begin
  if FDownloadMethod = dmFile then
  begin
    Ut := TUrl.Create(aUrl);
    try
      Ut.CrackUrl(aUrl, ICU_ESCAPE);
      if AnsiPos('.', Ut.ExtraInfo) <> 0 then
        sTmp1 := Ut.ExtraInfo;
      Ut.QueryUrl(aUrl);
      sTmp2 := Ut.Document;
    finally
      Ut.Free;
    end;
    if sTmp1 <> EmptyStr then
    begin
      Result := sTmp1;
      Exit;
    end
    else
      Result := sTmp2;
  end;
end;

procedure TCustomIEDownload.ExtractDataFromFile(const aFileName: string);
begin
  FDownloadedFile := aFileName;
  FFileName := ExtractFileName(aFileName);
  FDownloadFolder := ExtractFilePath(aFileName);
  FFileExtension := ExtractFileExt(aFileName);
end;

procedure TCustomIEDownload.SetAdditionalHeader(const Value: TStrings);
begin {Sets additional headers to append to the HTTP request.}
  FAdditionalHeader.Assign(Value);
end;

procedure TCustomIEDownload.SetAbout(Value: string);
begin
  Exit;
end;

procedure TCustomIEDownload.SetDefaultProtocol(const Value: string);
begin
  FDefaultProtocol := (Value);
  if FDefaultProtocol = EmptyStr then
    FDefaultProtocol := 'http://';
end;

procedure TCustomIEDownload.SetUserAgent;
begin
  FFullUserAgent := USER_AGENT_IE6 + '(' + FUserAgent + ')' + #13#10;
end;

procedure TCustomIEDownload.SetBindInfoF(const Value: TBindInfoF_Options);
begin
  FBindInfoF := Value;
  Update_BindInfoF_Value;
end;

procedure TCustomIEDownload.SetBindF2(const Value: TBindF2_Options);
begin
  FBindF2 := Value;
  Update_BindF2_Value;
end;

procedure TCustomIEDownload.SetBindInfoOption(const Value:
  TBindInfoOptions_Options);
begin
  FBindInfoOption_ := Value;
  Update_BindInfoOptions_Value;
end;

procedure TCustomIEDownload.SetBindF(const Value: TBindF_Options);
begin
  if FFileExistsOption = feOverWrite then
    FBindF := FBindF + [GetNewestVersion];
  FBindF := Value;
  Update_BindF_Value;
end;

procedure TCustomIEDownload.SetDownloadMethod(const Value: TDownloadMethod);
begin
  FDownloadMethod := Value;
end;

function TCustomIEDownload.SetHttpProtocol(const aUrl: string): string;
type {Insert http to an address like bsalsa.com }
  TProtocols = array[1..23] of string;
const
  Protocols: TProtocols = (
    'about', 'cdl', 'dvd', 'file', 'ftp', 'gopher', 'http', 'ipp', 'its',
    'javascript', 'local', 'mailto', 'mk', 'msdaipp', 'ms-help', 'ms-its',
    'mso', 'res', 'sysimage', 'tv', 'vbscript', 'via', 'https');
var
  i: Integer;
begin
  for i := 1 to 23 do
  begin
    if (AnsiPos(AnsiUpperCase(Protocols[i]), AnsiUpperCase(aUrl)) <> 0) then
    begin
      Result := aUrl;
      Exit;
    end;
  end;
  Result := 'http://' + aUrl;
end;

function TCustomIEDownload.SetDownloadFolder(const aDownloadFolder: string):
  string;
begin
  if (FDownloadMethod = dmFile) then
  begin
    Result := aDownloadFolder;
    if (Result = EmptyStr) then
      Result := ExtractFilePath(Application.ExeName) + DL_DIR;
    if Result <> EmptyStr then
      try
        ForceDirectories(Result);
      except
        if Assigned(FOnError) then
          FOnError(GetLastError, SysErrorMessage(GetLastError) +
            Err_Creating_Dir);
      end;
  end;
end;

function TCustomIEDownload.ResponseCodeToStr(const dwResponse: Integer): string;
begin
  Result := IEDownloadTools.ResponseCodeToStr(dwResponse);
end;

function TCustomIEDownload.WideStringToLPOLESTR(const Source: string): POleStr;
begin
  Result := IEDownloadTools.WidestringToLPOLESTR(Source);
end;

initialization
  coInitialize(nil);
finalization
  coUninitialize;
end.

