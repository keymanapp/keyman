unit SHDocVw_TLB;

{ This file contains pascal declarations imported from a type library.
  This file will be written during each import or refresh of the type
  library editor.  Changes to this file will be discarded during the
  refresh process. }

{ Microsoft Internet Controls }
{ Version 1.1 }

{ Conversion log:
  Warning: 'Type' is a reserved word. IWebBrowser.Type changed to Type_
  Warning: 'Property' is a reserved word. Parameter 'Property' in DWebBrowserEvents.PropertyChange changed to 'Property_'
  Warning: 'Property' is a reserved word. Parameter 'Property' in IWebBrowserApp.PutProperty changed to 'Property_'
  Warning: 'Property' is a reserved word. Parameter 'Property' in IWebBrowserApp.GetProperty changed to 'Property_'
  Warning: 'Type' is a reserved word. FolderItem.Type changed to Type_
  Warning: 'Type' is a reserved word. Parameter 'Type' in IShellUIHelper.AddDesktopComponent changed to 'Type_'
 }

interface

uses Windows, ActiveX, Classes, Graphics, OleCtrls, StdVCL;

const
  LIBID_SHDocVw: TGUID = '{EAB22AC0-30C1-11CF-A7EB-0000C05BAE0B}';

const

{ Constants for WebBrowser CommandStateChange }

{ CommandStateChangeConstants }

  CSC_UPDATECOMMANDS = -1;
  CSC_NAVIGATEFORWARD = 1;
  CSC_NAVIGATEBACK = 2;

{ OLECMDID }

  OLECMDID_OPEN = 1;
  OLECMDID_NEW = 2;
  OLECMDID_SAVE = 3;
  OLECMDID_SAVEAS = 4;
  OLECMDID_SAVECOPYAS = 5;
  OLECMDID_PRINT = 6;
  OLECMDID_PRINTPREVIEW = 7;
  OLECMDID_PAGESETUP = 8;
  OLECMDID_SPELL = 9;
  OLECMDID_PROPERTIES = 10;
  OLECMDID_CUT = 11;
  OLECMDID_COPY = 12;
  OLECMDID_PASTE = 13;
  OLECMDID_PASTESPECIAL = 14;
  OLECMDID_UNDO = 15;
  OLECMDID_REDO = 16;
  OLECMDID_SELECTALL = 17;
  OLECMDID_CLEARSELECTION = 18;
  OLECMDID_ZOOM = 19;
  OLECMDID_GETZOOMRANGE = 20;
  OLECMDID_UPDATECOMMANDS = 21;
  OLECMDID_REFRESH = 22;
  OLECMDID_STOP = 23;
  OLECMDID_HIDETOOLBARS = 24;
  OLECMDID_SETPROGRESSMAX = 25;
  OLECMDID_SETPROGRESSPOS = 26;
  OLECMDID_SETPROGRESSTEXT = 27;
  OLECMDID_SETTITLE = 28;
  OLECMDID_SETDOWNLOADSTATE = 29;
  OLECMDID_STOPDOWNLOAD = 30;
  OLECMDID_ONTOOLBARACTIVATED = 31;
  OLECMDID_FIND = 32;
  OLECMDID_DELETE = 33;
  OLECMDID_HTTPEQUIV = 34;
  OLECMDID_HTTPEQUIV_DONE = 35;
  OLECMDID_ENABLE_INTERACTION = 36;
  OLECMDID_ONUNLOAD = 37;
  OLECMDID_PROPERTYBAG2 = 38;
  OLECMDID_PREREFRESH = 39;

{ OLECMDF }

  OLECMDF_SUPPORTED = 1;
  OLECMDF_ENABLED = 2;
  OLECMDF_LATCHED = 4;
  OLECMDF_NINCHED = 8;

{ OLECMDEXECOPT }

  OLECMDEXECOPT_DODEFAULT = 0;
  OLECMDEXECOPT_PROMPTUSER = 1;
  OLECMDEXECOPT_DONTPROMPTUSER = 2;
  OLECMDEXECOPT_SHOWHELP = 3;

{ tagREADYSTATE }

  READYSTATE_UNINITIALIZED = 0;
  READYSTATE_LOADING = 1;
  READYSTATE_LOADED = 2;
  READYSTATE_INTERACTIVE = 3;
  READYSTATE_COMPLETE = 4;

{ Constants for ShellWindows registration }

{ ShellWindowTypeConstants }

  SWC_EXPLORER = 0;
  SWC_BROWSER = 1;
  SWC_3RDPARTY = 2;
  SWC_CALLBACK = 4;

{ Options for ShellWindows FindWindow }

{ ShellWindowFindWindowOptions }

  SWFO_NEEDDISPATCH = 1;
  SWFO_INCLUDEPENDING = 2;
  SWFO_COOKIEPASSED = 4;

{ Constants for ViewOptions }

{ ShellFolderViewOptions }

  SFVVO_SHOWALLOBJECTS = 1;
  SFVVO_SHOWEXTENSIONS = 2;
  SFVVO_SHOWCOMPCOLOR = 8;
  SFVVO_SHOWSYSFILES = 32;
  SFVVO_WIN95CLASSIC = 64;
  SFVVO_DOUBLECLICKINWEBVIEW = 128;
  SFVVO_DESKTOPHTML = 512;

{ Constants for Special Folders for open/Explore }

{ ShellSpecialFolderConstants }

  ssfDESKTOP = 0;
  ssfPROGRAMS = 2;
  ssfCONTROLS = 3;
  ssfPRINTERS = 4;
  ssfPERSONAL = 5;
  ssfFAVORITES = 6;
  ssfSTARTUP = 7;
  ssfRECENT = 8;
  ssfSENDTO = 9;
  ssfBITBUCKET = 10;
  ssfSTARTMENU = 11;
  ssfDESKTOPDIRECTORY = 16;
  ssfDRIVES = 17;
  ssfNETWORK = 18;
  ssfNETHOOD = 19;
  ssfFONTS = 20;
  ssfTEMPLATES = 21;

const

{ Component class GUIDs }
  Class_WebBrowser_V1: TGUID = '{EAB22AC3-30C1-11CF-A7EB-0000C05BAE0B}';
//  Class_WebBrowser: TGUID = '{8856F961-340A-11D0-A96B-00C04FD705A2}';
  Class_InternetExplorer: TGUID = '{0002DF01-0000-0000-C000-000000000046}';
//  Class_ShellFolderViewOC: TGUID = '{9BA05971-F6A8-11CF-A442-00A0C90A8F39}';
//  Class_ShellWindows: TGUID = '{9BA05972-F6A8-11CF-A442-00A0C90A8F39}';
//  Class_ShellLinkObject: TGUID = '{11219420-1768-11D1-95BE-00609797EA4F}';
//  Class_ShellFolderView: TGUID = '{62112AA1-EBE4-11CF-A5FB-0020AFE7292D}';
//  Class_Shell: TGUID = '{13709620-C279-11CE-A49E-444553540000}';
//  Class_ShellDispatchInproc: TGUID = '{0A89A860-D7B1-11CE-8350-444553540000}';
//  Class_WebViewFolderContents: TGUID = '{1820FED0-473E-11D0-A96C-00C04FD705A2}';
//  Class_ShellUIHelper: TGUID = '{64AB4BB7-111E-11D1-8F79-00C04FC2FBE1}';

type

{ Forward declarations: Interfaces }
  IWebBrowser = interface;
  IWebBrowserDisp = dispinterface;
  DWebBrowserEvents = dispinterface;
(*  IWebBrowserApp = interface;
  IWebBrowserAppDisp = dispinterface;
  IWebBrowser2 = interface;
  IWebBrowser2Disp = dispinterface;
  DWebBrowserEvents2 = dispinterface;
  IFolderViewOC = interface;
  IFolderViewOCDisp = dispinterface;
  DShellFolderViewEvents = dispinterface;
  DShellWindowsEvents = dispinterface;
  IShellWindows = interface;
  IShellWindowsDisp = dispinterface;
  IShellLinkDual = interface;
  IShellLinkDualDisp = dispinterface;
  FolderItemVerb = interface;
  FolderItemVerbDisp = dispinterface;
  FolderItemVerbs = interface;
  FolderItemVerbsDisp = dispinterface;
  Folder = interface;
  FolderDisp = dispinterface;
  FolderItems = interface;
  FolderItemsDisp = dispinterface;
  FolderItem = interface;
  FolderItemDisp = dispinterface;
  IShellFolderViewDual = interface;
  IShellFolderViewDualDisp = dispinterface;
  IShellDispatch = interface;
  IShellDispatchDisp = dispinterface;
  IShellUIHelper = interface;
  IShellUIHelperDisp = dispinterface;*)

{ Forward declarations: CoClasses }
  WebBrowser_V1 = IWebBrowser;
(*  WebBrowser = IWebBrowser2;
  InternetExplorer = IWebBrowser2;
  ShellFolderViewOC = IFolderViewOC;
  ShellWindows = IShellWindows;
  ShellLinkObject = IShellLinkDual;
  ShellFolderView = IShellFolderViewDual;
  Shell = IShellDispatch;
  ShellDispatchInproc = IUnknown;
  WebViewFolderContents = IShellFolderViewDual;
  ShellUIHelper = IShellUIHelper;*)

{ Forward declarations: Enums }
  CommandStateChangeConstants = TOleEnum;
  OLECMDID = TOleEnum;
  OLECMDF = TOleEnum;
  OLECMDEXECOPT = TOleEnum;
  tagREADYSTATE = TOleEnum;
  ShellWindowTypeConstants = TOleEnum;
  ShellWindowFindWindowOptions = TOleEnum;
  ShellFolderViewOptions = TOleEnum;
  ShellSpecialFolderConstants = TOleEnum;

{ Web Browser interface }

  IWebBrowser = interface(IDispatch)
    ['{EAB22AC1-30C1-11CF-A7EB-0000C05BAE0B}']
    procedure GoBack; safecall;
    procedure GoForward; safecall;
    procedure GoHome; safecall;
    procedure GoSearch; safecall;
    procedure Navigate(const URL: WideString; var Flags, TargetFrameName, PostData, Headers: OleVariant); safecall;
    procedure Refresh; safecall;
    procedure Refresh2(var Level: OleVariant); safecall;
    procedure Stop; safecall;
    function Get_Application: IDispatch; safecall;
    function Get_Parent: IDispatch; safecall;
    function Get_Container: IDispatch; safecall;
    function Get_Document: IDispatch; safecall;
    function Get_TopLevelContainer: WordBool; safecall;
    function Get_Type_: WideString; safecall;
    function Get_Left: Integer; safecall;
    procedure Set_Left(Value: Integer); safecall;
    function Get_Top: Integer; safecall;
    procedure Set_Top(Value: Integer); safecall;
    function Get_Width: Integer; safecall;
    procedure Set_Width(Value: Integer); safecall;
    function Get_Height: Integer; safecall;
    procedure Set_Height(Value: Integer); safecall;
    function Get_LocationName: WideString; safecall;
    function Get_LocationURL: WideString; safecall;
    function Get_Busy: WordBool; safecall;
    property Application: IDispatch read Get_Application;
    property Parent: IDispatch read Get_Parent;
    property Container: IDispatch read Get_Container;
    property Document: IDispatch read Get_Document;
    property TopLevelContainer: WordBool read Get_TopLevelContainer;
    property Type_: WideString read Get_Type_;
    property Left: Integer read Get_Left write Set_Left;
    property Top: Integer read Get_Top write Set_Top;
    property Width: Integer read Get_Width write Set_Width;
    property Height: Integer read Get_Height write Set_Height;
    property LocationName: WideString read Get_LocationName;
    property LocationURL: WideString read Get_LocationURL;
    property Busy: WordBool read Get_Busy;
  end;

{ DispInterface declaration for Dual Interface IWebBrowser }

  IWebBrowserDisp = dispinterface
    ['{EAB22AC1-30C1-11CF-A7EB-0000C05BAE0B}']
    procedure GoBack; dispid 100;
    procedure GoForward; dispid 101;
    procedure GoHome; dispid 102;
    procedure GoSearch; dispid 103;
    procedure Navigate(const URL: WideString; var Flags, TargetFrameName, PostData, Headers: OleVariant); dispid 104;
    procedure Refresh; dispid -550;
    procedure Refresh2(var Level: OleVariant); dispid 105;
    procedure Stop; dispid 106;
    property Application: IDispatch readonly dispid 200;
    property Parent: IDispatch readonly dispid 201;
    property Container: IDispatch readonly dispid 202;
    property Document: IDispatch readonly dispid 203;
    property TopLevelContainer: WordBool readonly dispid 204;
    property Type_: WideString readonly dispid 205;
    property Left: Integer dispid 206;
    property Top: Integer dispid 207;
    property Width: Integer dispid 208;
    property Height: Integer dispid 209;
    property LocationName: WideString readonly dispid 210;
    property LocationURL: WideString readonly dispid 211;
    property Busy: WordBool readonly dispid 212;
  end;

{ Web Browser Control Events (old) }

  DWebBrowserEvents = dispinterface
    ['{EAB22AC2-30C1-11CF-A7EB-0000C05BAE0B}']
    procedure BeforeNavigate(const URL: WideString; Flags: Integer; const TargetFrameName: WideString; var PostData: OleVariant; const Headers: WideString; var Cancel: WordBool); dispid 100;
    procedure NavigateComplete(const URL: WideString); dispid 101;
    procedure StatusTextChange(const Text: WideString); dispid 102;
    procedure ProgressChange(Progress, ProgressMax: Integer); dispid 108;
    procedure DownloadComplete; dispid 104;
    procedure CommandStateChange(Command: Integer; Enable: WordBool); dispid 105;
    procedure DownloadBegin; dispid 106;
    procedure NewWindow(const URL: WideString; Flags: Integer; const TargetFrameName: WideString; var PostData: OleVariant; const Headers: WideString; var Processed: WordBool); dispid 107;
    procedure TitleChange(const Text: WideString); dispid 113;
    procedure FrameBeforeNavigate(const URL: WideString; Flags: Integer; const TargetFrameName: WideString; var PostData: OleVariant; const Headers: WideString; var Cancel: WordBool); dispid 200;
    procedure FrameNavigateComplete(const URL: WideString); dispid 201;
    procedure FrameNewWindow(const URL: WideString; Flags: Integer; const TargetFrameName: WideString; var PostData: OleVariant; const Headers: WideString; var Processed: WordBool); dispid 204;
    procedure Quit(var Cancel: WordBool); dispid 103;
    procedure WindowMove; dispid 109;
    procedure WindowResize; dispid 110;
    procedure WindowActivate; dispid 111;
    procedure PropertyChange(const Property_: WideString); dispid 112;
  end;

{ Web Browser Application Interface. }

(*
  IWebBrowserApp = interface(IWebBrowser)
    ['{0002DF05-0000-0000-C000-000000000046}']
    procedure Quit; safecall;
    procedure ClientToWindow(var pcx, pcy: SYSINT); safecall;
    procedure PutProperty(const Property_: WideString; vtValue: OleVariant); safecall;
    function GetProperty(const Property_: WideString): OleVariant; safecall;
    function Get_Name: WideString; safecall;
    function Get_HWND: Integer; safecall;
    function Get_FullName: WideString; safecall;
    function Get_Path: WideString; safecall;
    function Get_Visible: WordBool; safecall;
    procedure Set_Visible(Value: WordBool); safecall;
    function Get_StatusBar: WordBool; safecall;
    procedure Set_StatusBar(Value: WordBool); safecall;
    function Get_StatusText: WideString; safecall;
    procedure Set_StatusText(const Value: WideString); safecall;
    function Get_ToolBar: SYSINT; safecall;
    procedure Set_ToolBar(Value: SYSINT); safecall;
    function Get_MenuBar: WordBool; safecall;
    procedure Set_MenuBar(Value: WordBool); safecall;
    function Get_FullScreen: WordBool; safecall;
    procedure Set_FullScreen(Value: WordBool); safecall;
    property Name: WideString read Get_Name;
    property HWND: Integer read Get_HWND;
    property FullName: WideString read Get_FullName;
    property Path: WideString read Get_Path;
    property Visible: WordBool read Get_Visible write Set_Visible;
    property StatusBar: WordBool read Get_StatusBar write Set_StatusBar;
    property StatusText: WideString read Get_StatusText write Set_StatusText;
    property ToolBar: SYSINT read Get_ToolBar write Set_ToolBar;
    property MenuBar: WordBool read Get_MenuBar write Set_MenuBar;
    property FullScreen: WordBool read Get_FullScreen write Set_FullScreen;
  end;


{ DispInterface declaration for Dual Interface IWebBrowserApp }


  IWebBrowserAppDisp = dispinterface
    ['{0002DF05-0000-0000-C000-000000000046}']
    procedure GoBack; dispid 100;
    procedure GoForward; dispid 101;
    procedure GoHome; dispid 102;
    procedure GoSearch; dispid 103;
    procedure Navigate(const URL: WideString; var Flags, TargetFrameName, PostData, Headers: OleVariant); dispid 104;
    procedure Refresh; dispid -550;
    procedure Refresh2(var Level: OleVariant); dispid 105;
    procedure Stop; dispid 106;
    property Application: IDispatch readonly dispid 200;
    property Parent: IDispatch readonly dispid 201;
    property Container: IDispatch readonly dispid 202;
    property Document: IDispatch readonly dispid 203;
    property TopLevelContainer: WordBool readonly dispid 204;
    property Type_: WideString readonly dispid 205;
    property Left: Integer dispid 206;
    property Top: Integer dispid 207;
    property Width: Integer dispid 208;
    property Height: Integer dispid 209;
    property LocationName: WideString readonly dispid 210;
    property LocationURL: WideString readonly dispid 211;
    property Busy: WordBool readonly dispid 212;
    procedure Quit; dispid 300;
    procedure ClientToWindow(var pcx, pcy: SYSINT); dispid 301;
    procedure PutProperty(const Property_: WideString; vtValue: OleVariant); dispid 302;
    function GetProperty(const Property_: WideString): OleVariant; dispid 303;
    property Name: WideString readonly dispid 0;
    property HWND: Integer readonly dispid -515;
    property FullName: WideString readonly dispid 400;
    property Path: WideString readonly dispid 401;
    property Visible: WordBool dispid 402;
    property StatusBar: WordBool dispid 403;
    property StatusText: WideString dispid 404;
    property ToolBar: SYSINT dispid 405;
    property MenuBar: WordBool dispid 406;
    property FullScreen: WordBool dispid 407;
  end;


{ Web Browser Interface for IE4. }


  IWebBrowser2 = interface(IWebBrowserApp)
    ['{D30C1661-CDAF-11D0-8A3E-00C04FC9E26E}']
    procedure Navigate2(var URL, Flags, TargetFrameName, PostData, Headers: OleVariant); safecall;
    function QueryStatusWB(cmdID: OLECMDID): OLECMDF; safecall;
    procedure ExecWB(cmdID: OLECMDID; cmdexecopt: OLECMDEXECOPT; var pvaIn, pvaOut: OleVariant); safecall;
    procedure ShowBrowserBar(var pvaClsid, pvarShow, pvarSize: OleVariant); safecall;
    function Get_ReadyState: tagREADYSTATE; safecall;
    function Get_Offline: WordBool; safecall;
    procedure Set_Offline(Value: WordBool); safecall;
    function Get_Silent: WordBool; safecall;
    procedure Set_Silent(Value: WordBool); safecall;
    function Get_RegisterAsBrowser: WordBool; safecall;
    procedure Set_RegisterAsBrowser(Value: WordBool); safecall;
    function Get_RegisterAsDropTarget: WordBool; safecall;
    procedure Set_RegisterAsDropTarget(Value: WordBool); safecall;
    function Get_TheaterMode: WordBool; safecall;
    procedure Set_TheaterMode(Value: WordBool); safecall;
    function Get_AddressBar: WordBool; safecall;
    procedure Set_AddressBar(Value: WordBool); safecall;
    function Get_Resizable: WordBool; safecall;
    procedure Set_Resizable(Value: WordBool); safecall;
    property ReadyState: tagREADYSTATE read Get_ReadyState;
    property Offline: WordBool read Get_Offline write Set_Offline;
    property Silent: WordBool read Get_Silent write Set_Silent;
    property RegisterAsBrowser: WordBool read Get_RegisterAsBrowser write Set_RegisterAsBrowser;
    property RegisterAsDropTarget: WordBool read Get_RegisterAsDropTarget write Set_RegisterAsDropTarget;
    property TheaterMode: WordBool read Get_TheaterMode write Set_TheaterMode;
    property AddressBar: WordBool read Get_AddressBar write Set_AddressBar;
    property Resizable: WordBool read Get_Resizable write Set_Resizable;
  end;


{ DispInterface declaration for Dual Interface IWebBrowser2 }


  IWebBrowser2Disp = dispinterface
    ['{D30C1661-CDAF-11D0-8A3E-00C04FC9E26E}']
    procedure GoBack; dispid 100;
    procedure GoForward; dispid 101;
    procedure GoHome; dispid 102;
    procedure GoSearch; dispid 103;
    procedure Navigate(const URL: WideString; var Flags, TargetFrameName, PostData, Headers: OleVariant); dispid 104;
    procedure Refresh; dispid -550;
    procedure Refresh2(var Level: OleVariant); dispid 105;
    procedure Stop; dispid 106;
    property Application: IDispatch readonly dispid 200;
    property Parent: IDispatch readonly dispid 201;
    property Container: IDispatch readonly dispid 202;
    property Document: IDispatch readonly dispid 203;
    property TopLevelContainer: WordBool readonly dispid 204;
    property Type_: WideString readonly dispid 205;
    property Left: Integer dispid 206;
    property Top: Integer dispid 207;
    property Width: Integer dispid 208;
    property Height: Integer dispid 209;
    property LocationName: WideString readonly dispid 210;
    property LocationURL: WideString readonly dispid 211;
    property Busy: WordBool readonly dispid 212;
    procedure Quit; dispid 300;
    procedure ClientToWindow(var pcx, pcy: SYSINT); dispid 301;
    procedure PutProperty(const Property_: WideString; vtValue: OleVariant); dispid 302;
    function GetProperty(const Property_: WideString): OleVariant; dispid 303;
    property Name: WideString readonly dispid 0;
    property HWND: Integer readonly dispid -515;
    property FullName: WideString readonly dispid 400;
    property Path: WideString readonly dispid 401;
    property Visible: WordBool dispid 402;
    property StatusBar: WordBool dispid 403;
    property StatusText: WideString dispid 404;
    property ToolBar: SYSINT dispid 405;
    property MenuBar: WordBool dispid 406;
    property FullScreen: WordBool dispid 407;
    procedure Navigate2(var URL, Flags, TargetFrameName, PostData, Headers: OleVariant); dispid 500;
    function QueryStatusWB(cmdID: OLECMDID): OLECMDF; dispid 501;
    procedure ExecWB(cmdID: OLECMDID; cmdexecopt: OLECMDEXECOPT; var pvaIn, pvaOut: OleVariant); dispid 502;
    procedure ShowBrowserBar(var pvaClsid, pvarShow, pvarSize: OleVariant); dispid 503;
    property ReadyState: tagREADYSTATE readonly dispid -525;
    property Offline: WordBool dispid 550;
    property Silent: WordBool dispid 551;
    property RegisterAsBrowser: WordBool dispid 552;
    property RegisterAsDropTarget: WordBool dispid 553;
    property TheaterMode: WordBool dispid 554;
    property AddressBar: WordBool dispid 555;
    property Resizable: WordBool dispid 556;
  end;

{ Web Browser Control events interface }

  DWebBrowserEvents2 = dispinterface
    ['{34A715A0-6587-11D0-924A-0020AFC7AC4D}']
    procedure StatusTextChange(const Text: WideString); dispid 102;
    procedure ProgressChange(Progress, ProgressMax: Integer); dispid 108;
    procedure CommandStateChange(Command: Integer; Enable: WordBool); dispid 105;
    procedure DownloadBegin; dispid 106;
    procedure DownloadComplete; dispid 104;
    procedure TitleChange(const Text: WideString); dispid 113;
    procedure PropertyChange(const szProperty: WideString); dispid 112;
    procedure BeforeNavigate2(pDisp: IDispatch; var URL, Flags, TargetFrameName, PostData, Headers: OleVariant; var Cancel: WordBool); dispid 250;
    procedure NewWindow2(var ppDisp: IDispatch; var Cancel: WordBool); dispid 251;
    procedure NavigateComplete2(pDisp: IDispatch; var URL: OleVariant); dispid 252;
    procedure DocumentComplete(pDisp: IDispatch; var URL: OleVariant); dispid 259;
    procedure OnQuit; dispid 253;
    procedure OnVisible(Visible: WordBool); dispid 254;
    procedure OnToolBar(ToolBar: WordBool); dispid 255;
    procedure OnMenuBar(MenuBar: WordBool); dispid 256;
    procedure OnStatusBar(StatusBar: WordBool); dispid 257;
    procedure OnFullScreen(FullScreen: WordBool); dispid 258;
    procedure OnTheaterMode(TheaterMode: WordBool); dispid 260;
  end;


{ Folder View Events Forwarder Object }

  IFolderViewOC = interface(IDispatch)
    ['{9BA05970-F6A8-11CF-A442-00A0C90A8F39}']
    procedure SetFolderView(pDisp: IDispatch); safecall;
  end;

{ DispInterface declaration for Dual Interface IFolderViewOC }

  IFolderViewOCDisp = dispinterface
    ['{9BA05970-F6A8-11CF-A442-00A0C90A8F39}']
    procedure SetFolderView(pDisp: IDispatch); dispid 1610743808;
  end;

{ Event interface for ShellFolderView }

  DShellFolderViewEvents = dispinterface
    ['{62112AA2-EBE4-11CF-A5FB-0020AFE7292D}']
    procedure SelectionChanged; dispid 200;
  end;

{ Event interface for IShellWindows }

  DShellWindowsEvents = dispinterface
    ['{FE4106E0-399A-11D0-A48C-00A0C90A8F39}']
    procedure WindowRegistered(lCookie: Integer); dispid 200;
    procedure WindowRevoked(lCookie: Integer); dispid 201;
  end;

{ Definition of interface IShellWindows }

  IShellWindows = interface(IDispatch)
    ['{85CB6900-4D95-11CF-960C-0080C7F4EE85}']
    function Get_Count: Integer; safecall;
    function Item(index: OleVariant): IDispatch; safecall;
    function _NewEnum: IUnknown; safecall;
    procedure Register(pid: IDispatch; HWND: Integer; swClass: SYSINT; out plCookie: Integer); safecall;
    procedure RegisterPending(lThreadId: Integer; var pvarloc, pvarlocRoot: OleVariant; swClass: SYSINT; out plCookie: Integer); safecall;
    procedure Revoke(lCookie: Integer); safecall;
    procedure OnNavigate(lCookie: Integer; var pvarloc: OleVariant); safecall;
    procedure OnActivated(lCookie: Integer; fActive: WordBool); safecall;
    function FindWindow(var pvarloc, pvarlocRoot: OleVariant; swClass: SYSINT; out pHWND: Integer; swfwOptions: SYSINT): IDispatch; safecall;
    procedure OnCreated(lCookie: Integer; punk: IUnknown); safecall;
    procedure ProcessAttachDetach(fAttach: WordBool); safecall;
    property Count: Integer read Get_Count;
  end;

{ DispInterface declaration for Dual Interface IShellWindows }

  IShellWindowsDisp = dispinterface
    ['{85CB6900-4D95-11CF-960C-0080C7F4EE85}']
    property Count: Integer readonly dispid 1610743808;
    function Item(index: OleVariant): IDispatch; dispid 0;
    function _NewEnum: IUnknown; dispid -4;
    procedure Register(pid: IDispatch; HWND: Integer; swClass: SYSINT; out plCookie: Integer); dispid 1610743811;
    procedure RegisterPending(lThreadId: Integer; var pvarloc, pvarlocRoot: OleVariant; swClass: SYSINT; out plCookie: Integer); dispid 1610743812;
    procedure Revoke(lCookie: Integer); dispid 1610743813;
    procedure OnNavigate(lCookie: Integer; var pvarloc: OleVariant); dispid 1610743814;
    procedure OnActivated(lCookie: Integer; fActive: WordBool); dispid 1610743815;
    function FindWindow(var pvarloc, pvarlocRoot: OleVariant; swClass: SYSINT; out pHWND: Integer; swfwOptions: SYSINT): IDispatch; dispid 1610743816;
    procedure OnCreated(lCookie: Integer; punk: IUnknown); dispid 1610743817;
    procedure ProcessAttachDetach(fAttach: WordBool); dispid 1610743818;
  end;

{ Definition of Shell Link IDispatch interface }

  IShellLinkDual = interface(IDispatch)
    ['{88A05C00-F000-11CE-8350-444553540000}']
    function Get_Path: WideString; safecall;
    procedure Set_Path(const Value: WideString); safecall;
    function Get_Description: WideString; safecall;
    procedure Set_Description(const Value: WideString); safecall;
    function Get_WorkingDirectory: WideString; safecall;
    procedure Set_WorkingDirectory(const Value: WideString); safecall;
    function Get_Arguments: WideString; safecall;
    procedure Set_Arguments(const Value: WideString); safecall;
    function Get_Hotkey: SYSINT; safecall;
    procedure Set_Hotkey(Value: SYSINT); safecall;
    function Get_ShowCommand: SYSINT; safecall;
    procedure Set_ShowCommand(Value: SYSINT); safecall;
    procedure Resolve(fFlags: SYSINT); safecall;
    function GetIconLocation(out pbs: WideString): SYSINT; safecall;
    procedure SetIconLocation(const bs: WideString; iIcon: SYSINT); safecall;
    procedure Save(vWhere: OleVariant); safecall;
    property Path: WideString read Get_Path write Set_Path;
    property Description: WideString read Get_Description write Set_Description;
    property WorkingDirectory: WideString read Get_WorkingDirectory write Set_WorkingDirectory;
    property Arguments: WideString read Get_Arguments write Set_Arguments;
    property Hotkey: SYSINT read Get_Hotkey write Set_Hotkey;
    property ShowCommand: SYSINT read Get_ShowCommand write Set_ShowCommand;
  end;

{ DispInterface declaration for Dual Interface IShellLinkDual }

  IShellLinkDualDisp = dispinterface
    ['{88A05C00-F000-11CE-8350-444553540000}']
    property Path: WideString dispid 1610743808;
    property Description: WideString dispid 1610743810;
    property WorkingDirectory: WideString dispid 1610743812;
    property Arguments: WideString dispid 1610743814;
    property Hotkey: SYSINT dispid 1610743816;
    property ShowCommand: SYSINT dispid 1610743818;
    procedure Resolve(fFlags: SYSINT); dispid 1610743820;
    function GetIconLocation(out pbs: WideString): SYSINT; dispid 1610743821;
    procedure SetIconLocation(const bs: WideString; iIcon: SYSINT); dispid 1610743822;
    procedure Save(vWhere: OleVariant); dispid 1610743823;
  end;

{ Definition of interface FolderItemVerb }

  FolderItemVerb = interface(IDispatch)
    ['{08EC3E00-50B0-11CF-960C-0080C7F4EE85}']
    function Get_Application: IDispatch; safecall;
    function Get_Parent: IDispatch; safecall;
    function Get_Name: WideString; safecall;
    procedure DoIt; safecall;
    property Application: IDispatch read Get_Application;
    property Parent: IDispatch read Get_Parent;
    property Name: WideString read Get_Name;
  end;

{ DispInterface declaration for Dual Interface FolderItemVerb }

  FolderItemVerbDisp = dispinterface
    ['{08EC3E00-50B0-11CF-960C-0080C7F4EE85}']
    property Application: IDispatch readonly dispid 1610743808;
    property Parent: IDispatch readonly dispid 1610743809;
    property Name: WideString readonly dispid 0;
    procedure DoIt; dispid 1610743811;
  end;

{ Definition of interface FolderItemVerbs }

  FolderItemVerbs = interface(IDispatch)
    ['{1F8352C0-50B0-11CF-960C-0080C7F4EE85}']
    function Get_Count: Integer; safecall;
    function Get_Application: IDispatch; safecall;
    function Get_Parent: IDispatch; safecall;
    function Item(index: OleVariant): FolderItemVerb; safecall;
    function _NewEnum: IUnknown; safecall;
    property Count: Integer read Get_Count;
    property Application: IDispatch read Get_Application;
    property Parent: IDispatch read Get_Parent;
  end;

{ DispInterface declaration for Dual Interface FolderItemVerbs }

  FolderItemVerbsDisp = dispinterface
    ['{1F8352C0-50B0-11CF-960C-0080C7F4EE85}']
    property Count: Integer readonly dispid 1610743808;
    property Application: IDispatch readonly dispid 1610743809;
    property Parent: IDispatch readonly dispid 1610743810;
    function Item(index: OleVariant): FolderItemVerb; dispid 1610743811;
    function _NewEnum: IUnknown; dispid -4;
  end;

{ Definition of interface Folder }

  Folder = interface(IDispatch)
    ['{BBCBDE60-C3FF-11CE-8350-444553540000}']
    function Get_Title: WideString; safecall;
    function Get_Application: IDispatch; safecall;
    function Get_Parent: IDispatch; safecall;
    function Get_ParentFolder: Folder; safecall;
    function Items: FolderItems; safecall;
    function ParseName(const bName: WideString): FolderItem; safecall;
    procedure NewFolder(const bName: WideString; vOptions: OleVariant); safecall;
    procedure MoveHere(vItem, vOptions: OleVariant); safecall;
    procedure CopyHere(vItem, vOptions: OleVariant); safecall;
    function GetDetailsOf(vItem: OleVariant; iColumn: SYSINT): WideString; safecall;
    property Title: WideString read Get_Title;
    property Application: IDispatch read Get_Application;
    property Parent: IDispatch read Get_Parent;
    property ParentFolder: Folder read Get_ParentFolder;
  end;

{ DispInterface declaration for Dual Interface Folder }

  FolderDisp = dispinterface
    ['{BBCBDE60-C3FF-11CE-8350-444553540000}']
    property Title: WideString readonly dispid 0;
    property Application: IDispatch readonly dispid 1610743809;
    property Parent: IDispatch readonly dispid 1610743810;
    property ParentFolder: Folder readonly dispid 1610743811;
    function Items: FolderItems; dispid 1610743812;
    function ParseName(const bName: WideString): FolderItem; dispid 1610743813;
    procedure NewFolder(const bName: WideString; vOptions: OleVariant); dispid 1610743814;
    procedure MoveHere(vItem, vOptions: OleVariant); dispid 1610743815;
    procedure CopyHere(vItem, vOptions: OleVariant); dispid 1610743816;
    function GetDetailsOf(vItem: OleVariant; iColumn: SYSINT): WideString; dispid 1610743817;
  end;

{ Definition of interface FolderItems }

  FolderItems = interface(IDispatch)
    ['{744129E0-CBE5-11CE-8350-444553540000}']
    function Get_Count: Integer; safecall;
    function Get_Application: IDispatch; safecall;
    function Get_Parent: IDispatch; safecall;
    function Item(index: OleVariant): FolderItem; safecall;
    function _NewEnum: IUnknown; safecall;
    property Count: Integer read Get_Count;
    property Application: IDispatch read Get_Application;
    property Parent: IDispatch read Get_Parent;
  end;

{ DispInterface declaration for Dual Interface FolderItems }

  FolderItemsDisp = dispinterface
    ['{744129E0-CBE5-11CE-8350-444553540000}']
    property Count: Integer readonly dispid 1610743808;
    property Application: IDispatch readonly dispid 1610743809;
    property Parent: IDispatch readonly dispid 1610743810;
    function Item(index: OleVariant): FolderItem; dispid 1610743811;
    function _NewEnum: IUnknown; dispid -4;
  end;

{ Definition of interface FolderItem }

  FolderItem = interface(IDispatch)
    ['{FAC32C80-CBE4-11CE-8350-444553540000}']
    function Get_Application: IDispatch; safecall;
    function Get_Parent: IDispatch; safecall;
    function Get_Name: WideString; safecall;
    procedure Set_Name(const Value: WideString); safecall;
    function Get_Path: WideString; safecall;
    function Get_GetLink: IDispatch; safecall;
    function Get_GetFolder: IDispatch; safecall;
    function Get_IsLink: WordBool; safecall;
    function Get_IsFolder: WordBool; safecall;
    function Get_IsFileSystem: WordBool; safecall;
    function Get_IsBrowsable: WordBool; safecall;
    function Get_ModifyDate: TDateTime; safecall;
    procedure Set_ModifyDate(Value: TDateTime); safecall;
    function Get_Size: Integer; safecall;
    function Get_Type_: WideString; safecall;
    function Verbs: FolderItemVerbs; safecall;
    procedure InvokeVerb(vVerb: OleVariant); safecall;
    property Application: IDispatch read Get_Application;
    property Parent: IDispatch read Get_Parent;
    property Name: WideString read Get_Name write Set_Name;
    property Path: WideString read Get_Path;
    property GetLink: IDispatch read Get_GetLink;
    property GetFolder: IDispatch read Get_GetFolder;
    property IsLink: WordBool read Get_IsLink;
    property IsFolder: WordBool read Get_IsFolder;
    property IsFileSystem: WordBool read Get_IsFileSystem;
    property IsBrowsable: WordBool read Get_IsBrowsable;
    property ModifyDate: TDateTime read Get_ModifyDate write Set_ModifyDate;
    property Size: Integer read Get_Size;
    property Type_: WideString read Get_Type_;
  end;

{ DispInterface declaration for Dual Interface FolderItem }

  FolderItemDisp = dispinterface
    ['{FAC32C80-CBE4-11CE-8350-444553540000}']
    property Application: IDispatch readonly dispid 1610743808;
    property Parent: IDispatch readonly dispid 1610743809;
    property Name: WideString dispid 0;
    property Path: WideString readonly dispid 1610743812;
    property GetLink: IDispatch readonly dispid 1610743813;
    property GetFolder: IDispatch readonly dispid 1610743814;
    property IsLink: WordBool readonly dispid 1610743815;
    property IsFolder: WordBool readonly dispid 1610743816;
    property IsFileSystem: WordBool readonly dispid 1610743817;
    property IsBrowsable: WordBool readonly dispid 1610743818;
    property ModifyDate: TDateTime dispid 1610743819;
    property Size: Integer readonly dispid 1610743821;
    property Type_: WideString readonly dispid 1610743822;
    function Verbs: FolderItemVerbs; dispid 1610743823;
    procedure InvokeVerb(vVerb: OleVariant); dispid 1610743824;
  end;

{ definition of interface IShellFolderViewDual }

  IShellFolderViewDual = interface(IDispatch)
    ['{E7A1AF80-4D96-11CF-960C-0080C7F4EE85}']
    function Get_Application: IDispatch; safecall;
    function Get_Parent: IDispatch; safecall;
    function Get_Folder: Folder; safecall;
    function SelectedItems: FolderItems; safecall;
    function Get_FocusedItem: FolderItem; safecall;
    procedure SelectItem(var pvfi: OleVariant; dwFlags: SYSINT); safecall;
    function PopupItemMenu(const pfi: FolderItem; vx, vy: OleVariant): WideString; safecall;
    function Get_Script: IDispatch; safecall;
    function Get_ViewOptions: Integer; safecall;
    property Application: IDispatch read Get_Application;
    property Parent: IDispatch read Get_Parent;
    property Folder: Folder read Get_Folder;
    property FocusedItem: FolderItem read Get_FocusedItem;
    property Script: IDispatch read Get_Script;
    property ViewOptions: Integer read Get_ViewOptions;
  end;

{ DispInterface declaration for Dual Interface IShellFolderViewDual }

  IShellFolderViewDualDisp = dispinterface
    ['{E7A1AF80-4D96-11CF-960C-0080C7F4EE85}']
    property Application: IDispatch readonly dispid 1610743808;
    property Parent: IDispatch readonly dispid 1610743809;
    property Folder: Folder readonly dispid 1610743810;
    function SelectedItems: FolderItems; dispid 1610743811;
    property FocusedItem: FolderItem readonly dispid 1610743812;
    procedure SelectItem(var pvfi: OleVariant; dwFlags: SYSINT); dispid 1610743813;
    function PopupItemMenu(const pfi: FolderItem; vx, vy: OleVariant): WideString; dispid 1610743814;
    property Script: IDispatch readonly dispid 1610743815;
    property ViewOptions: Integer readonly dispid 1610743816;
  end;

{ Definition of interface IShellDispatch }

  IShellDispatch = interface(IDispatch)
    ['{D8F015C0-C278-11CE-A49E-444553540000}']
    function Get_Application: IDispatch; safecall;
    function Get_Parent: IDispatch; safecall;
    function NameSpace(vDir: OleVariant): Folder; safecall;
    function BrowseForFolder(HWND: Integer; const Title: WideString; Options: Integer; RootFolder: OleVariant): Folder; safecall;
    function Windows: IDispatch; safecall;
    procedure Open(vDir: OleVariant); safecall;
    procedure Explore(vDir: OleVariant); safecall;
    procedure MinimizeAll; safecall;
    procedure UndoMinimizeALL; safecall;
    procedure FileRun; safecall;
    procedure CascadeWindows; safecall;
    procedure TileVertically; safecall;
    procedure TileHorizontally; safecall;
    procedure ShutdownWindows; safecall;
    procedure Suspend; safecall;
    procedure EjectPC; safecall;
    procedure SetTime; safecall;
    procedure TrayProperties; safecall;
    procedure Help; safecall;
    procedure FindFiles; safecall;
    procedure FindComputer; safecall;
    procedure RefreshMenu; safecall;
    procedure ControlPanelItem(const szDir: WideString); safecall;
    property Application: IDispatch read Get_Application;
    property Parent: IDispatch read Get_Parent;
  end;

{ DispInterface declaration for Dual Interface IShellDispatch }

  IShellDispatchDisp = dispinterface
    ['{D8F015C0-C278-11CE-A49E-444553540000}']
    property Application: IDispatch readonly dispid 1610743808;
    property Parent: IDispatch readonly dispid 1610743809;
    function NameSpace(vDir: OleVariant): Folder; dispid 1610743810;
    function BrowseForFolder(HWND: Integer; const Title: WideString; Options: Integer; RootFolder: OleVariant): Folder; dispid 1610743811;
    function Windows: IDispatch; dispid 1610743812;
    procedure Open(vDir: OleVariant); dispid 1610743813;
    procedure Explore(vDir: OleVariant); dispid 1610743814;
    procedure MinimizeAll; dispid 1610743815;
    procedure UndoMinimizeALL; dispid 1610743816;
    procedure FileRun; dispid 1610743817;
    procedure CascadeWindows; dispid 1610743818;
    procedure TileVertically; dispid 1610743819;
    procedure TileHorizontally; dispid 1610743820;
    procedure ShutdownWindows; dispid 1610743821;
    procedure Suspend; dispid 1610743822;
    procedure EjectPC; dispid 1610743823;
    procedure SetTime; dispid 1610743824;
    procedure TrayProperties; dispid 1610743825;
    procedure Help; dispid 1610743826;
    procedure FindFiles; dispid 1610743827;
    procedure FindComputer; dispid 1610743828;
    procedure RefreshMenu; dispid 1610743829;
    procedure ControlPanelItem(const szDir: WideString); dispid 1610743830;
  end;

{ Shell UI Helper Control Interface }

  IShellUIHelper = interface(IDispatch)
    ['{729FE2F8-1EA8-11D1-8F85-00C04FC2FBE1}']
    procedure ResetFirstBootMode; safecall;
    procedure ResetSafeMode; safecall;
    procedure RefreshOfflineDesktop; safecall;
    procedure AddFavorite(const URL: WideString; var Title: OleVariant); safecall;
    procedure AddChannel(const URL: WideString); safecall;
    procedure AddDesktopComponent(const URL, Type_: WideString; var Left, Top, Width, Height: OleVariant); safecall;
    function IsSubscribed(const URL: WideString): WordBool; safecall;
  end;

{ DispInterface declaration for Dual Interface IShellUIHelper }

  IShellUIHelperDisp = dispinterface
    ['{729FE2F8-1EA8-11D1-8F85-00C04FC2FBE1}']
    procedure ResetFirstBootMode; dispid 1;
    procedure ResetSafeMode; dispid 2;
    procedure RefreshOfflineDesktop; dispid 3;
    procedure AddFavorite(const URL: WideString; var Title: OleVariant); dispid 4;
    procedure AddChannel(const URL: WideString); dispid 5;
    procedure AddDesktopComponent(const URL, Type_: WideString; var Left, Top, Width, Height: OleVariant); dispid 6;
    function IsSubscribed(const URL: WideString): WordBool; dispid 7;
  end;
*)

{ WebBrowser Control }

  TWebBrowser_V1BeforeNavigate = procedure(Sender: TObject; const URL: WideString; Flags: Integer; const TargetFrameName: WideString; var PostData: OleVariant; const Headers: WideString; var Cancel: WordBool) of object;
  TWebBrowser_V1NavigateComplete = procedure(Sender: TObject; const URL: WideString) of object;
  TWebBrowser_V1StatusTextChange = procedure(Sender: TObject; const Text: WideString) of object;
  TWebBrowser_V1ProgressChange = procedure(Sender: TObject; Progress, ProgressMax: Integer) of object;
  TWebBrowser_V1CommandStateChange = procedure(Sender: TObject; Command: Integer; Enable: WordBool) of object;
  TWebBrowser_V1NewWindow = procedure(Sender: TObject; const URL: WideString; Flags: Integer; const TargetFrameName: WideString; var PostData: OleVariant; const Headers: WideString; var Processed: WordBool) of object;
  TWebBrowser_V1TitleChange = procedure(Sender: TObject; const Text: WideString) of object;
  TWebBrowser_V1FrameBeforeNavigate = procedure(Sender: TObject; const URL: WideString; Flags: Integer; const TargetFrameName: WideString; var PostData: OleVariant; const Headers: WideString; var Cancel: WordBool) of object;
  TWebBrowser_V1FrameNavigateComplete = procedure(Sender: TObject; const URL: WideString) of object;
  TWebBrowser_V1FrameNewWindow = procedure(Sender: TObject; const URL: WideString; Flags: Integer; const TargetFrameName: WideString; var PostData: OleVariant; const Headers: WideString; var Processed: WordBool) of object;
  TWebBrowser_V1Quit = procedure(Sender: TObject; var Cancel: WordBool) of object;
  TWebBrowser_V1PropertyChange = procedure(Sender: TObject; const Property_: WideString) of object;

  TWebBrowser_V1 = class(TOleControl)
  private
    FOnBeforeNavigate: TWebBrowser_V1BeforeNavigate;
    FOnNavigateComplete: TWebBrowser_V1NavigateComplete;
    FOnStatusTextChange: TWebBrowser_V1StatusTextChange;
    FOnProgressChange: TWebBrowser_V1ProgressChange;
    FOnDownloadComplete: TNotifyEvent;
    FOnCommandStateChange: TWebBrowser_V1CommandStateChange;
    FOnDownloadBegin: TNotifyEvent;
    FOnNewWindow: TWebBrowser_V1NewWindow;
    FOnTitleChange: TWebBrowser_V1TitleChange;
    FOnFrameBeforeNavigate: TWebBrowser_V1FrameBeforeNavigate;
    FOnFrameNavigateComplete: TWebBrowser_V1FrameNavigateComplete;
    FOnFrameNewWindow: TWebBrowser_V1FrameNewWindow;
    FOnQuit: TWebBrowser_V1Quit;
    FOnWindowMove: TNotifyEvent;
    FOnWindowResize: TNotifyEvent;
    FOnWindowActivate: TNotifyEvent;
    FOnPropertyChange: TWebBrowser_V1PropertyChange;
    FIntf: IWebBrowser;
    function GetControlInterface: IWebBrowser;
  protected
    procedure CreateControl;
    procedure InitControlData; override;
    function GetTOleEnumProp(Index: Integer): TOleEnum;
    procedure SetTOleEnumProp(Index: Integer; Value: TOleEnum);
  public
    procedure GoBack;
    procedure GoForward;
    procedure GoHome;
    procedure GoSearch;
    procedure Navigate(const URL: WideString; var Flags, TargetFrameName, PostData, Headers: OleVariant);
    procedure Refresh;
    procedure Refresh2(var Level: OleVariant);
    procedure Stop;
    property ControlInterface: IWebBrowser read GetControlInterface;
    property Application: IDispatch index 200 read GetIDispatchProp;
    property Parent: IDispatch index 201 read GetIDispatchProp;
    property Container: IDispatch index 202 read GetIDispatchProp;
    property Document: IDispatch index 203 read GetIDispatchProp;
    property TopLevelContainer: WordBool index 204 read GetWordBoolProp;
    property Type_: WideString index 205 read GetWideStringProp;
    property LocationName: WideString index 210 read GetWideStringProp;
    property LocationURL: WideString index 211 read GetWideStringProp;
    property Busy: WordBool index 212 read GetWordBoolProp;
  published
    property TabStop;
    property Align;
    property DragCursor;
    property DragMode;
    property ParentShowHint;
    property PopupMenu;
    property ShowHint;
    property TabOrder;
    property Visible;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDrag;
    property OnEnter;
    property OnExit;
    property OnStartDrag;
    property OnBeforeNavigate: TWebBrowser_V1BeforeNavigate read FOnBeforeNavigate write FOnBeforeNavigate;
    property OnNavigateComplete: TWebBrowser_V1NavigateComplete read FOnNavigateComplete write FOnNavigateComplete;
    property OnStatusTextChange: TWebBrowser_V1StatusTextChange read FOnStatusTextChange write FOnStatusTextChange;
    property OnProgressChange: TWebBrowser_V1ProgressChange read FOnProgressChange write FOnProgressChange;
    property OnDownloadComplete: TNotifyEvent read FOnDownloadComplete write FOnDownloadComplete;
    property OnCommandStateChange: TWebBrowser_V1CommandStateChange read FOnCommandStateChange write FOnCommandStateChange;
    property OnDownloadBegin: TNotifyEvent read FOnDownloadBegin write FOnDownloadBegin;
    property OnNewWindow: TWebBrowser_V1NewWindow read FOnNewWindow write FOnNewWindow;
    property OnTitleChange: TWebBrowser_V1TitleChange read FOnTitleChange write FOnTitleChange;
    property OnFrameBeforeNavigate: TWebBrowser_V1FrameBeforeNavigate read FOnFrameBeforeNavigate write FOnFrameBeforeNavigate;
    property OnFrameNavigateComplete: TWebBrowser_V1FrameNavigateComplete read FOnFrameNavigateComplete write FOnFrameNavigateComplete;
    property OnFrameNewWindow: TWebBrowser_V1FrameNewWindow read FOnFrameNewWindow write FOnFrameNewWindow;
    property OnQuit: TWebBrowser_V1Quit read FOnQuit write FOnQuit;
    property OnWindowMove: TNotifyEvent read FOnWindowMove write FOnWindowMove;
    property OnWindowResize: TNotifyEvent read FOnWindowResize write FOnWindowResize;
    property OnWindowActivate: TNotifyEvent read FOnWindowActivate write FOnWindowActivate;
    property OnPropertyChange: TWebBrowser_V1PropertyChange read FOnPropertyChange write FOnPropertyChange;
  end;

{ WebBrowser Control }

(*
  TWebBrowserStatusTextChange = procedure(Sender: TObject; const Text: WideString) of object;
  TWebBrowserProgressChange = procedure(Sender: TObject; Progress, ProgressMax: Integer) of object;
  TWebBrowserCommandStateChange = procedure(Sender: TObject; Command: Integer; Enable: WordBool) of object;
  TWebBrowserTitleChange = procedure(Sender: TObject; const Text: WideString) of object;
  TWebBrowserPropertyChange = procedure(Sender: TObject; const szProperty: WideString) of object;
  TWebBrowserBeforeNavigate2 = procedure(Sender: TObject; pDisp: IDispatch; var URL, Flags, TargetFrameName, PostData, Headers: OleVariant; var Cancel: WordBool) of object;
  TWebBrowserNewWindow2 = procedure(Sender: TObject; var ppDisp: IDispatch; var Cancel: WordBool) of object;
  TWebBrowserNavigateComplete2 = procedure(Sender: TObject; pDisp: IDispatch; var URL: OleVariant) of object;
  TWebBrowserDocumentComplete = procedure(Sender: TObject; pDisp: IDispatch; var URL: OleVariant) of object;
  TWebBrowserOnVisible = procedure(Sender: TObject; Visible: WordBool) of object;
  TWebBrowserOnToolBar = procedure(Sender: TObject; ToolBar: WordBool) of object;
  TWebBrowserOnMenuBar = procedure(Sender: TObject; MenuBar: WordBool) of object;
  TWebBrowserOnStatusBar = procedure(Sender: TObject; StatusBar: WordBool) of object;
  TWebBrowserOnFullScreen = procedure(Sender: TObject; FullScreen: WordBool) of object;
  TWebBrowserOnTheaterMode = procedure(Sender: TObject; TheaterMode: WordBool) of object;

  TWebBrowser = class(TOleControl)
  private
    FOnStatusTextChange: TWebBrowserStatusTextChange;
    FOnProgressChange: TWebBrowserProgressChange;
    FOnCommandStateChange: TWebBrowserCommandStateChange;
    FOnDownloadBegin: TNotifyEvent;
    FOnDownloadComplete: TNotifyEvent;
    FOnTitleChange: TWebBrowserTitleChange;
    FOnPropertyChange: TWebBrowserPropertyChange;
    FOnBeforeNavigate2: TWebBrowserBeforeNavigate2;
    FOnNewWindow2: TWebBrowserNewWindow2;
    FOnNavigateComplete2: TWebBrowserNavigateComplete2;
    FOnDocumentComplete: TWebBrowserDocumentComplete;
    FOnQuit: TNotifyEvent;
    FOnVisible: TWebBrowserOnVisible;
    FOnToolBar: TWebBrowserOnToolBar;
    FOnMenuBar: TWebBrowserOnMenuBar;
    FOnStatusBar: TWebBrowserOnStatusBar;
    FOnFullScreen: TWebBrowserOnFullScreen;
    FOnTheaterMode: TWebBrowserOnTheaterMode;
    FIntf: IWebBrowser2;
    function GetControlInterface: IWebBrowser2;
  protected
    procedure CreateControl;
    procedure InitControlData; override;
    function GetTOleEnumProp(Index: Integer): TOleEnum;
    procedure SetTOleEnumProp(Index: Integer; Value: TOleEnum);
  public
    procedure GoBack;
    procedure GoForward;
    procedure GoHome;
    procedure GoSearch;
    procedure Navigate(const URL: WideString; var Flags, TargetFrameName, PostData, Headers: OleVariant);
    procedure Refresh;
    procedure Refresh2(var Level: OleVariant);
    procedure Stop;
    procedure Quit;
    procedure ClientToWindow(var pcx, pcy: SYSINT);
    procedure PutProperty(const Property_: WideString; vtValue: OleVariant);
    function GetProperty(const Property_: WideString): OleVariant;
    procedure Navigate2(var URL, Flags, TargetFrameName, PostData, Headers: OleVariant);
    function QueryStatusWB(cmdID: OLECMDID): OLECMDF;
    procedure ExecWB(cmdID: OLECMDID; cmdexecopt: OLECMDEXECOPT; var pvaIn, pvaOut: OleVariant);
    procedure ShowBrowserBar(var pvaClsid, pvarShow, pvarSize: OleVariant);
    property ControlInterface: IWebBrowser2 read GetControlInterface;
    property Application: IDispatch index 200 read GetIDispatchProp;
    property Parent: IDispatch index 201 read GetIDispatchProp;
    property Container: IDispatch index 202 read GetIDispatchProp;
    property Document: IDispatch index 203 read GetIDispatchProp;
    property TopLevelContainer: WordBool index 204 read GetWordBoolProp;
    property Type_: WideString index 205 read GetWideStringProp;
    property LocationName: WideString index 210 read GetWideStringProp;
    property LocationURL: WideString index 211 read GetWideStringProp;
    property Busy: WordBool index 212 read GetWordBoolProp;
    property Name: WideString index 0 read GetWideStringProp;
    property HWND: Integer index -515 read GetIntegerProp;
    property FullName: WideString index 400 read GetWideStringProp;
    property Path: WideString index 401 read GetWideStringProp;
    property ReadyState: tagREADYSTATE index -525 read GetTOleEnumProp;
  published
    property TabStop;
    property Align;
    property DragCursor;
    property DragMode;
    property ParentShowHint;
    property PopupMenu;
    property ShowHint;
    property TabOrder;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDrag;
    property OnEnter;
    property OnExit;
    property OnStartDrag;
    property Visible: WordBool index 402 read GetWordBoolProp write SetWordBoolProp stored False;
    property StatusBar: WordBool index 403 read GetWordBoolProp write SetWordBoolProp stored False;
    property StatusText: WideString index 404 read GetWideStringProp write SetWideStringProp stored False;
    property ToolBar: SYSINT index 405 read GetIntegerProp write SetIntegerProp stored False;
    property MenuBar: WordBool index 406 read GetWordBoolProp write SetWordBoolProp stored False;
    property FullScreen: WordBool index 407 read GetWordBoolProp write SetWordBoolProp stored False;
    property Offline: WordBool index 550 read GetWordBoolProp write SetWordBoolProp stored False;
    property Silent: WordBool index 551 read GetWordBoolProp write SetWordBoolProp stored False;
    property RegisterAsBrowser: WordBool index 552 read GetWordBoolProp write SetWordBoolProp stored False;
    property RegisterAsDropTarget: WordBool index 553 read GetWordBoolProp write SetWordBoolProp stored False;
    property TheaterMode: WordBool index 554 read GetWordBoolProp write SetWordBoolProp stored False;
    property AddressBar: WordBool index 555 read GetWordBoolProp write SetWordBoolProp stored False;
    property Resizable: WordBool index 556 read GetWordBoolProp write SetWordBoolProp stored False;
    property OnStatusTextChange: TWebBrowserStatusTextChange read FOnStatusTextChange write FOnStatusTextChange;
    property OnProgressChange: TWebBrowserProgressChange read FOnProgressChange write FOnProgressChange;
    property OnCommandStateChange: TWebBrowserCommandStateChange read FOnCommandStateChange write FOnCommandStateChange;
    property OnDownloadBegin: TNotifyEvent read FOnDownloadBegin write FOnDownloadBegin;
    property OnDownloadComplete: TNotifyEvent read FOnDownloadComplete write FOnDownloadComplete;
    property OnTitleChange: TWebBrowserTitleChange read FOnTitleChange write FOnTitleChange;
    property OnPropertyChange: TWebBrowserPropertyChange read FOnPropertyChange write FOnPropertyChange;
    property OnBeforeNavigate2: TWebBrowserBeforeNavigate2 read FOnBeforeNavigate2 write FOnBeforeNavigate2;
    property OnNewWindow2: TWebBrowserNewWindow2 read FOnNewWindow2 write FOnNewWindow2;
    property OnNavigateComplete2: TWebBrowserNavigateComplete2 read FOnNavigateComplete2 write FOnNavigateComplete2;
    property OnDocumentComplete: TWebBrowserDocumentComplete read FOnDocumentComplete write FOnDocumentComplete;
    property OnQuit: TNotifyEvent read FOnQuit write FOnQuit;
    property OnVisible: TWebBrowserOnVisible read FOnVisible write FOnVisible;
    property OnToolBar: TWebBrowserOnToolBar read FOnToolBar write FOnToolBar;
    property OnMenuBar: TWebBrowserOnMenuBar read FOnMenuBar write FOnMenuBar;
    property OnStatusBar: TWebBrowserOnStatusBar read FOnStatusBar write FOnStatusBar;
    property OnFullScreen: TWebBrowserOnFullScreen read FOnFullScreen write FOnFullScreen;
    property OnTheaterMode: TWebBrowserOnTheaterMode read FOnTheaterMode write FOnTheaterMode;
  end;

{ Shell Folder View Events Router. }

  TShellFolderViewOC = class(TOleControl)
  private
    FOnSelectionChanged: TNotifyEvent;
    FIntf: IFolderViewOC;
    function GetControlInterface: IFolderViewOC;
  protected
    procedure CreateControl;
    procedure InitControlData; override;
    function GetTOleEnumProp(Index: Integer): TOleEnum;
    procedure SetTOleEnumProp(Index: Integer; Value: TOleEnum);
  public
    procedure SetFolderView(pDisp: IDispatch);
    property ControlInterface: IFolderViewOC read GetControlInterface;
  published
    property OnSelectionChanged: TNotifyEvent read FOnSelectionChanged write FOnSelectionChanged;
  end;
*)

procedure Register;

implementation

uses ComObj;

procedure TWebBrowser_V1.InitControlData;
const
  CEventDispIDs: array[0..16] of Integer = (
    $00000064, $00000065, $00000066, $0000006C, $00000068, $00000069,
    $0000006A, $0000006B, $00000071, $000000C8, $000000C9, $000000CC,
    $00000067, $0000006D, $0000006E, $0000006F, $00000070);
  CControlData: TControlData = (
    ClassID: '{EAB22AC3-30C1-11CF-A7EB-0000C05BAE0B}';
    EventIID: '{EAB22AC2-30C1-11CF-A7EB-0000C05BAE0B}';
    EventCount: 17;
    EventDispIDs: @CEventDispIDs;
    LicenseKey: nil;
    Flags: $00000000;
    Version: 300);
begin
  ControlData := @CControlData;
end;

procedure TWebBrowser_V1.CreateControl;

  procedure DoCreate;
  begin
    FIntf := IUnknown(OleObject) as IWebBrowser;
  end;

begin
  if FIntf = nil then DoCreate;
end;

function TWebBrowser_V1.GetControlInterface: IWebBrowser;
begin
  CreateControl;
  Result := FIntf;
end;

function TWebBrowser_V1.GetTOleEnumProp(Index: Integer): TOleEnum;
begin
  Result := GetIntegerProp(Index);
end;

procedure TWebBrowser_V1.SetTOleEnumProp(Index: Integer; Value: TOleEnum);
begin
  SetIntegerProp(Index, Value);
end;

procedure TWebBrowser_V1.GoBack;
begin
  CreateControl;
  FIntf.GoBack;
end;

procedure TWebBrowser_V1.GoForward;
begin
  CreateControl;
  FIntf.GoForward;
end;

procedure TWebBrowser_V1.GoHome;
begin
  CreateControl;
  FIntf.GoHome;
end;

procedure TWebBrowser_V1.GoSearch;
begin
  CreateControl;
  FIntf.GoSearch;
end;

procedure TWebBrowser_V1.Navigate(const URL: WideString; var Flags, TargetFrameName, PostData, Headers: OleVariant);
begin
  CreateControl;
  FIntf.Navigate(URL, Flags, TargetFrameName, PostData, Headers);
end;

procedure TWebBrowser_V1.Refresh;
begin
  CreateControl;
  FIntf.Refresh;
end;

procedure TWebBrowser_V1.Refresh2(var Level: OleVariant);
begin
  CreateControl;
  FIntf.Refresh2(Level);
end;

procedure TWebBrowser_V1.Stop;
begin
  CreateControl;
  FIntf.Stop;
end;

(*
procedure TWebBrowser.InitControlData;
const
  CEventDispIDs: array[0..17] of Integer = (
    $00000066, $0000006C, $00000069, $0000006A, $00000068, $00000071,
    $00000070, $000000FA, $000000FB, $000000FC, $00000103, $000000FD,
    $000000FE, $000000FF, $00000100, $00000101, $00000102, $00000104);
  CControlData: TControlData = (
    ClassID: '{8856F961-340A-11D0-A96B-00C04FD705A2}';
    EventIID: '{34A715A0-6587-11D0-924A-0020AFC7AC4D}';
    EventCount: 18;
    EventDispIDs: @CEventDispIDs;
    LicenseKey: nil;
    Flags: $00000000;
    Version: 300);
begin
  ControlData := @CControlData;
end;

procedure TWebBrowser.CreateControl;

  procedure DoCreate;
  begin
    FIntf := IUnknown(OleObject) as IWebBrowser2;
  end;

begin
  if FIntf = nil then DoCreate;
end;

function TWebBrowser.GetControlInterface: IWebBrowser2;
begin
  CreateControl;
  Result := FIntf;
end;

function TWebBrowser.GetTOleEnumProp(Index: Integer): TOleEnum;
begin
  Result := GetIntegerProp(Index);
end;

procedure TWebBrowser.SetTOleEnumProp(Index: Integer; Value: TOleEnum);
begin
  SetIntegerProp(Index, Value);
end;

procedure TWebBrowser.GoBack;
begin
  CreateControl;
  FIntf.GoBack;
end;

procedure TWebBrowser.GoForward;
begin
  CreateControl;
  FIntf.GoForward;
end;

procedure TWebBrowser.GoHome;
begin
  CreateControl;
  FIntf.GoHome;
end;

procedure TWebBrowser.GoSearch;
begin
  CreateControl;
  FIntf.GoSearch;
end;

procedure TWebBrowser.Navigate(const URL: WideString; var Flags, TargetFrameName, PostData, Headers: OleVariant);
begin
  CreateControl;
  FIntf.Navigate(URL, Flags, TargetFrameName, PostData, Headers);
end;

procedure TWebBrowser.Refresh;
begin
  CreateControl;
  FIntf.Refresh;
end;

procedure TWebBrowser.Refresh2(var Level: OleVariant);
begin
  CreateControl;
  FIntf.Refresh2(Level);
end;

procedure TWebBrowser.Stop;
begin
  CreateControl;
  FIntf.Stop;
end;

procedure TWebBrowser.Quit;
begin
  CreateControl;
  FIntf.Quit;
end;

procedure TWebBrowser.ClientToWindow(var pcx, pcy: SYSINT);
begin
  CreateControl;
  FIntf.ClientToWindow(pcx, pcy);
end;

procedure TWebBrowser.PutProperty(const Property_: WideString; vtValue: OleVariant);
begin
  CreateControl;
  FIntf.PutProperty(Property_, vtValue);
end;

function TWebBrowser.GetProperty(const Property_: WideString): OleVariant;
begin
  CreateControl;
  Result := FIntf.GetProperty(Property_);
end;

procedure TWebBrowser.Navigate2(var URL, Flags, TargetFrameName, PostData, Headers: OleVariant);
begin
  CreateControl;
  FIntf.Navigate2(URL, Flags, TargetFrameName, PostData, Headers);
end;

function TWebBrowser.QueryStatusWB(cmdID: OLECMDID): OLECMDF;
begin
  CreateControl;
  Result := FIntf.QueryStatusWB(cmdID);
end;

procedure TWebBrowser.ExecWB(cmdID: OLECMDID; cmdexecopt: OLECMDEXECOPT; var pvaIn, pvaOut: OleVariant);
begin
  CreateControl;
  FIntf.ExecWB(cmdID, cmdexecopt, pvaIn, pvaOut);
end;

procedure TWebBrowser.ShowBrowserBar(var pvaClsid, pvarShow, pvarSize: OleVariant);
begin
  CreateControl;
  FIntf.ShowBrowserBar(pvaClsid, pvarShow, pvarSize);
end;


procedure TShellFolderViewOC.InitControlData;
const
  CEventDispIDs: array[0..0] of Integer = (
    $000000C8);
  CControlData: TControlData = (
    ClassID: '{9BA05971-F6A8-11CF-A442-00A0C90A8F39}';
    EventIID: '{62112AA2-EBE4-11CF-A5FB-0020AFE7292D}';
    EventCount: 1;
    EventDispIDs: @CEventDispIDs;
    LicenseKey: nil;
    Flags: $00000000;
    Version: 300);
begin
  ControlData := @CControlData;
end;

procedure TShellFolderViewOC.CreateControl;

  procedure DoCreate;
  begin
    FIntf := IUnknown(OleObject) as IFolderViewOC;
  end;

begin
  if FIntf = nil then DoCreate;
end;

function TShellFolderViewOC.GetControlInterface: IFolderViewOC;
begin
  CreateControl;
  Result := FIntf;
end;

function TShellFolderViewOC.GetTOleEnumProp(Index: Integer): TOleEnum;
begin
  Result := GetIntegerProp(Index);
end;

procedure TShellFolderViewOC.SetTOleEnumProp(Index: Integer; Value: TOleEnum);
begin
  SetIntegerProp(Index, Value);
end;

procedure TShellFolderViewOC.SetFolderView(pDisp: IDispatch);
begin
  CreateControl;
  FIntf.SetFolderView(pDisp);
end;
*)


procedure Register;
begin
  RegisterComponents('ActiveX', [TWebBrowser_V1]); //, TWebBrowser, TShellFolderViewOC]);
end;


end.
