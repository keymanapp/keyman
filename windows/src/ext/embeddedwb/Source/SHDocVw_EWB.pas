//**************************************************************
//                                                             *
//                SHDocVw_EWB                                  *
//                                                             *
//                     For Delphi 5 to XE                      *
//                            by                               *
//       bsalsa - Eran Bodankin  - bsalsa@gmail.com            *
//                                                             *
//                                                             *
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
4. Please consider donation in our web site!
{*******************************************************************************}
//$Id: SHDocVw_EWB.pas,v 1.3 2006/12/14 12:57:55 bsalsa Exp $

unit SHDocVw_EWB;
{$I EWB.inc}
// ************************************************************************  //
// Type Lib: shdocvw.dll (1) EWB
// LIBID: {EAB22AC0-30C1-11CF-A7EB-0000C05BAE0B}
// LCID: 0
// Created by Eran Bodankin (bsalsa)
// Errors:
//   Hint: Symbol 'Type' renamed to 'type_'
//   Hint: Parameter 'Type' of IWebBrowser.Type changed to 'Type_'
//   Hint: Parameter 'Property' of DWebBrowserEvents.PropertyChange changed to 'Property_'
//   Hint: Parameter 'Property' of IWebBrowserApp.PutProperty changed to 'Property_'
//   Hint: Parameter 'Property' of IWebBrowserApp.GetProperty changed to 'Property_'
//   Hint: Parameter 'Type' of IShellUIHelper.AddDesktopComponent changed to 'Type_'
//   Hint: Parameter 'var' of IShellNameSpace.Expand changed to 'var_'
// ************************************************************************ //
{$TYPEDADDRESS OFF} // Unit must be compiled without type-checked pointers.
{$IFDEF DELPHI6_UP}
{$WARN SYMBOL_PLATFORM OFF}
{$ALIGN 4}
{$VARPROPSETTER ON} // You need to download and install the second D6 patch in order for this to compile.
{$ENDIF}
{$WRITEABLECONST ON}

interface

{$I EWB.inc}

uses
  EwbAcc, Windows, ActiveX, Classes{$IFDEF USE_OleCtrlsFix}, OleCtrlsFix{$ENDIF}, OleCtrls, OleServer, StdVCL{$IFDEF DELPHI6_UP}, Variants{$ENDIF};

// *********************************************************************//
// GUIDS declared in the TypeLibrary. Following prefixes are used:
//   Type Libraries     : LIBID_xxxx
//   CoClasses          : CLASS_xxxx
//   DISPInterfaces     : DIID_xxxx
//   Non-DISP interfaces: IID_xxxx
// *********************************************************************//
const
  // TypeLibrary Major and minor versions
  SHDocVwMajorVersion = 1;
  SHDocVwMinorVersion = 1;

  LIBID_SHDocVw: TGUID = '{EAB22AC0-30C1-11CF-A7EB-0000C05BAE0B}';

  IID_IWebBrowser: TGUID = '{EAB22AC1-30C1-11CF-A7EB-0000C05BAE0B}';
  DIID_DWebBrowserEvents: TGUID = '{EAB22AC2-30C1-11CF-A7EB-0000C05BAE0B}';
  IID_IWebBrowserApp: TGUID = '{0002DF05-0000-0000-C000-000000000046}';
  IID_IWebBrowser2: TGUID = '{D30C1661-CDAF-11D0-8A3E-00C04FC9E26E}';
  DIID_DWebBrowserEvents2: TGUID = '{34A715A0-6587-11D0-924A-0020AFC7AC4D}';
  CLASS_WebBrowser_V1: TGUID = '{EAB22AC3-30C1-11CF-A7EB-0000C05BAE0B}';
  CLASS_WebBrowser: TGUID = '{8856F961-340A-11D0-A96B-00C04FD705A2}';
  CLASS_InternetExplorer: TGUID = '{0002DF01-0000-0000-C000-000000000046}';
  CLASS_ShellBrowserWindow: TGUID = '{C08AFD90-F2A1-11D1-8455-00A0C91F3880}';
  DIID_DShellWindowsEvents: TGUID = '{FE4106E0-399A-11D0-A48C-00A0C90A8F39}';
  IID_IShellWindows: TGUID = '{85CB6900-4D95-11CF-960C-0080C7F4EE85}';
  CLASS_ShellWindows: TGUID = '{9BA05972-F6A8-11CF-A442-00A0C90A8F39}';
  IID_IShellUIHelper: TGUID = '{729FE2F8-1EA8-11D1-8F85-00C04FC2FBE1}';
  IID_IShellUIHelper2: TGUID = '{A7FE6EDA-1932-4281-B881-87B31B8BC52C}';
  IID_IShellUIHelper3: TGUID = '{B7FCA399-7F15-42DF-A51C-E39C1A4F32DE}';
  CLASS_ShellUIHelper: TGUID = '{64AB4BB7-111E-11D1-8F79-00C04FC2FBE1}';
  DIID_DShellNameSpaceEvents: TGUID = '{55136806-B2DE-11D1-B9F2-00A0C98BC547}';
  IID_IShellFavoritesNameSpace: TGUID = '{55136804-B2DE-11D1-B9F2-00A0C98BC547}';
  IID_IShellNameSpace: TGUID = '{E572D3C9-37BE-4AE2-825D-D521763E3108}';
  CLASS_ShellNameSpace: TGUID = '{55136805-B2DE-11D1-B9F2-00A0C98BC547}';
  CLASS_ShellShellNameSpace: TGUID = '{2F2F1F96-2BC1-4B1C-BE28-EA3774F4676A}';
  IID_IScriptErrorList: TGUID = '{F3470F24-15FD-11D2-BB2E-00805FF7EFCA}';
  CLASS_CScriptErrorList: TGUID = '{EFD01300-160F-11D2-BB2E-00805FF7EFCA}';

{$IFDEF USE_OleCtrlsFix}
type
  TOleControl = class(OleCtrlsFix.TOleControlFix);
{$ENDIF}

// *********************************************************************//
// Declaration of Enumerations defined in Type Library
// *********************************************************************//
// Constants for enum CommandStateChangeConstants
type
  CommandStateChangeConstants = TOleEnum;
const
  CSC_UPDATECOMMANDS = $FFFFFFFF;
  CSC_NAVIGATEFORWARD = $00000001;
  CSC_NAVIGATEBACK = $00000002;

// Constants for enum OLECMDID
type
  OLECMDID = TOleEnum;
const
  OLECMDID_OPEN = $00000001;
  OLECMDID_NEW = $00000002;
  OLECMDID_SAVE = $00000003;
  OLECMDID_SAVEAS = $00000004;
  OLECMDID_SAVECOPYAS = $00000005;
  OLECMDID_PRINT = $00000006;
  OLECMDID_PRINTPREVIEW = $00000007;
  OLECMDID_PAGESETUP = $00000008;
  OLECMDID_SPELL = $00000009;
  OLECMDID_PROPERTIES = $0000000A;
  OLECMDID_CUT = $0000000B;
  OLECMDID_COPY = $0000000C;
  OLECMDID_PASTE = $0000000D;
  OLECMDID_PASTESPECIAL = $0000000E;
  OLECMDID_UNDO = $0000000F;
  OLECMDID_REDO = $00000010;
  OLECMDID_SELECTALL = $00000011;
  OLECMDID_CLEARSELECTION = $00000012;
  OLECMDID_ZOOM = $00000013;
  OLECMDID_GETZOOMRANGE = $00000014;
  OLECMDID_UPDATECOMMANDS = $00000015;
  OLECMDID_REFRESH = $00000016;
  OLECMDID_STOP = $00000017;
  OLECMDID_HIDETOOLBARS = $00000018;
  OLECMDID_SETPROGRESSMAX = $00000019;
  OLECMDID_SETPROGRESSPOS = $0000001A;
  OLECMDID_SETPROGRESSTEXT = $0000001B;
  OLECMDID_SETTITLE = $0000001C;
  OLECMDID_SETDOWNLOADSTATE = $0000001D;
  OLECMDID_STOPDOWNLOAD = $0000001E;
  OLECMDID_ONTOOLBARACTIVATED = $0000001F;
  OLECMDID_FIND = $00000020;
  OLECMDID_DELETE = $00000021;
  OLECMDID_HTTPEQUIV = $00000022;
  OLECMDID_HTTPEQUIV_DONE = $00000023;
  OLECMDID_ENABLE_INTERACTION = $00000024;
  OLECMDID_ONUNLOAD = $00000025;
  OLECMDID_PROPERTYBAG2 = $00000026;
  OLECMDID_PREREFRESH = $00000027;
  OLECMDID_SHOWSCRIPTERROR = $00000028;
  OLECMDID_SHOWMESSAGE = $00000029;
  OLECMDID_SHOWFIND = $0000002A;
  OLECMDID_SHOWPAGESETUP = $0000002B;
  OLECMDID_SHOWPRINT = $0000002C;
  OLECMDID_CLOSE = $0000002D;
  OLECMDID_ALLOWUILESSSAVEAS = $0000002E;
  OLECMDID_DONTDOWNLOADCSS = $0000002F;
  OLECMDID_UPDATEPAGESTATUS = $00000030;
  OLECMDID_PRINT2 = $00000031;
  OLECMDID_PRINTPREVIEW2 = $00000032;
  OLECMDID_SETPRINTTEMPLATE = $00000033;
  OLECMDID_GETPRINTTEMPLATE = $00000034;
  OLECMDID_PAGEACTIONBLOCKED = $00000037;
  OLECMDID_PAGEACTIONUIQUERY = $00000038;
  OLECMDID_FOCUSVIEWCONTROLS = $00000039;
  OLECMDID_FOCUSVIEWCONTROLSQUERY = $0000003A;
  OLECMDID_SHOWPAGEACTIONMENU = $0000003B;
   //New IE7 Values
  OLECMDID_ADDTRAVELENTRY = $0000003C;
  OLECMDID_UPDATETRAVELENTRY = $0000003D;
  OLECMDID_UPDATEBACKFORWARDSTATE = $0000003E;
  OLECMDID_OPTICAL_ZOOM = $0000003F;
  OLECMDID_OPTICAL_GETZOOMRANGE = $00000040;
  OLECMDID_WINDOWSTATECHANGED = $00000041;
   //New IE8 Values
  OLECMDID_ACTIVEXINSTALLSCOPE = $00000042;
  OLECMDID_UPDATETRAVELENTRY_DATARECOVERY = $00000043;

// Constants for enum OLECMDF
type
  OLECMDF = TOleEnum;
const
  OLECMDF_SUPPORTED = $00000001;
  OLECMDF_ENABLED = $00000002;
  OLECMDF_LATCHED = $00000004;
  OLECMDF_NINCHED = $00000008;
  OLECMDF_INVISIBLE = $00000010;
  OLECMDF_DEFHIDEONCTXTMENU = $00000020;
  OLECMDIDF_WINDOWSTATE_USERVISIBLE = $00000001;
  OLECMDIDF_WINDOWSTATE_ENABLED = $00000002;
  OLECMDIDF_WINDOWSTATE_USERVISIBLE_VALID = $00010000;
  OLECMDIDF_WINDOWSTATE_ENABLED_VALID = $00020000;

// Constants for enum OLECMDEXECOPT
type
  OLECMDEXECOPT = TOleEnum;
const
  OLECMDEXECOPT_DODEFAULT = $00000000;
  OLECMDEXECOPT_PROMPTUSER = $00000001;
  OLECMDEXECOPT_DONTPROMPTUSER = $00000002;
  OLECMDEXECOPT_SHOWHELP = $00000003;

// Constants for enum tagREADYSTATE
type
  tagREADYSTATE = TOleEnum;
const
  READYSTATE_UNINITIALIZED = $00000000;
  READYSTATE_LOADING = $00000001;
  READYSTATE_LOADED = $00000002;
  READYSTATE_INTERACTIVE = $00000003;
  READYSTATE_COMPLETE = $00000004;

// Constants for enum SecureLockIconConstants
type
  SecureLockIconConstants = TOleEnum;
const
  secureLockIconUnsecure = $00000000;
  secureLockIconMixed = $00000001;
  secureLockIconSecureUnknownBits = $00000002;
  secureLockIconSecure40Bit = $00000003;
  secureLockIconSecure56Bit = $00000004;
  secureLockIconSecureFortezza = $00000005;
  secureLockIconSecure128Bit = $00000006;

// Constants for enum NewProcessCauseConstants
type
  NewProcessCauseConstants = TOleEnum;
const
  ProtectedModeRedirect = $00000001;

// Constants for enum ShellWindowTypeConstants
type
  ShellWindowTypeConstants = TOleEnum;
const
  SWC_EXPLORER = $00000000;
  SWC_BROWSER = $00000001;
  SWC_3RDPARTY = $00000002;
  SWC_CALLBACK = $00000004;
  SWC_DESKTOP = $00000008;

// Constants for enum ShellWindowFindWindowOptions
type
  ShellWindowFindWindowOptions = TOleEnum;
const
  SWFO_NEEDDISPATCH = $00000001;
  SWFO_INCLUDEPENDING = $00000002;
  SWFO_COOKIEPASSED = $00000004;

type
  RefreshConstants = TOleEnum;
const
  REFRESH_NORMAL = 0;
  REFRESH_IFEXPIRED = 1;
  REFRESH_CONTINUE = 2;
  REFRESH_COMPLETELY = 3;

type

// *********************************************************************//
// Forward declaration of types defined in TypeLibrary
// *********************************************************************//
  IWebBrowser = interface;
  IWebBrowserDisp = dispinterface;
  DWebBrowserEvents = dispinterface;
  IWebBrowserApp = interface;
  IWebBrowserAppDisp = dispinterface;
  IWebBrowser2 = interface;
  IWebBrowser2Disp = dispinterface;
  DWebBrowserEvents2 = dispinterface;
  DShellWindowsEvents = dispinterface;
  IShellWindows = interface;
  IShellWindowsDisp = dispinterface;
  IShellUIHelper = interface;
  IShellUIHelperDisp = dispinterface;
  IShellUIHelper2 = interface;
  IShellUIHelper2Disp = dispinterface;
  IShellUIHelper3 = interface;
  IShellUIHelper3Disp = dispinterface;
  DShellNameSpaceEvents = dispinterface;
  IShellFavoritesNameSpace = interface;
  IShellFavoritesNameSpaceDisp = dispinterface;
  IShellNameSpace = interface;
  IShellNameSpaceDisp = dispinterface;
  IScriptErrorList = interface;
  IScriptErrorListDisp = dispinterface;

// *********************************************************************//
// Declaration of CoClasses defined in Type Library
// (NOTE: Here we map each CoClass to its Default Interface)
// *********************************************************************//
  WebBrowser_V1 = IWebBrowser;
  WebBrowser = IWebBrowser2;
  InternetExplorer = IWebBrowser2;
  ShellBrowserWindow = IWebBrowser2;
  ShellWindows = IShellWindows;
  ShellUIHelper = IShellUIHelper3;
  ShellNameSpace = IShellNameSpace;
  ShellShellNameSpace = IShellNameSpace;
  CScriptErrorList = IScriptErrorList;

// *********************************************************************//
// Declaration of structures, unions and aliases.
// *********************************************************************//
  POleVariant1 = ^OleVariant; {*}

// *********************************************************************//
// Interface: IWebBrowser
// Flags:     (4432) Hidden Dual OleAutomation Dispatchable
// GUID:      {EAB22AC1-30C1-11CF-A7EB-0000C05BAE0B}
// *********************************************************************//
  IWebBrowser = interface(IDispatch)
    ['{EAB22AC1-30C1-11CF-A7EB-0000C05BAE0B}']
    procedure GoBack; safecall;
    procedure GoForward; safecall;
    procedure GoHome; safecall;
    procedure GoSearch; safecall;
    {  procedure Navigate(const URL: WideString; var Flags: OleVariant;
         var TargetFrameName: OleVariant; var PostData: OleVariant;
         var Headers: OleVariant); safecall;   }

      // D2009:
    procedure Navigate(const URL: WideString; const Flags: OleVariant;
      const TargetFrameName: OleVariant; var PostData: OleVariant;
      const Headers: OleVariant); safecall;

    procedure Refresh; safecall;
    procedure Refresh2(var Level: OleVariant); safecall;
    procedure Stop; safecall;
    function Get_Application: IDispatch; safecall;
    function Get_Parent: IDispatch; safecall;
    function Get_Container: IDispatch; safecall;
    function Get_Document: IDispatch; safecall;
    function Get_TopLevelContainer: WordBool; safecall;
    function Get_type_: WideString; safecall;
    function Get_Left: Integer; safecall;
    procedure Set_Left(pl: Integer); safecall;
    function Get_Top: Integer; safecall;
    procedure Set_Top(pl: Integer); safecall;
    function Get_Width: Integer; safecall;
    procedure Set_Width(pl: Integer); safecall;
    function Get_Height: Integer; safecall;
    procedure Set_Height(pl: Integer); safecall;
    function Get_LocationName: WideString; safecall;
    function Get_LocationURL: WideString; safecall;
    function Get_Busy: WordBool; safecall;
    property Application: IDispatch read Get_Application;
    property Parent: IDispatch read Get_Parent;
    property Container: IDispatch read Get_Container;
    property Document: IDispatch read Get_Document;
    property TopLevelContainer: WordBool read Get_TopLevelContainer;
    property type_: WideString read Get_type_;
    property Left: Integer read Get_Left write Set_Left;
    property Top: Integer read Get_Top write Set_Top;
    property Width: Integer read Get_Width write Set_Width;
    property Height: Integer read Get_Height write Set_Height;
    property LocationName: WideString read Get_LocationName;
    property LocationURL: WideString read Get_LocationURL;
    property Busy: WordBool read Get_Busy;
  end;

// *********************************************************************//
// DispIntf:  IWebBrowserDisp
// Flags:     (4432) Hidden Dual OleAutomation Dispatchable
// GUID:      {EAB22AC1-30C1-11CF-A7EB-0000C05BAE0B}
// *********************************************************************//
  IWebBrowserDisp = dispinterface
    ['{EAB22AC1-30C1-11CF-A7EB-0000C05BAE0B}']
    procedure GoBack; dispid 100;
    procedure GoForward; dispid 101;
    procedure GoHome; dispid 102;
    procedure GoSearch; dispid 103;
    procedure Navigate(const URL: WideString; var Flags: OleVariant;
      var TargetFrameName: OleVariant; var PostData: OleVariant;
      var Headers: OleVariant); dispid 104;
    procedure Refresh; dispid - 550;
    procedure Refresh2(var Level: OleVariant); dispid 105;
    procedure Stop; dispid 106;
    property Application: IDispatch readonly dispid 200;
    property Parent: IDispatch readonly dispid 201;
    property Container: IDispatch readonly dispid 202;
    property Document: IDispatch readonly dispid 203;
    property TopLevelContainer: WordBool readonly dispid 204;
    property type_: WideString readonly dispid 205;
    property Left: Integer dispid 206;
    property Top: Integer dispid 207;
    property Width: Integer dispid 208;
    property Height: Integer dispid 209;
    property LocationName: WideString readonly dispid 210;
    property LocationURL: WideString readonly dispid 211;
    property Busy: WordBool readonly dispid 212;
  end;

// *********************************************************************//
// DispIntf:  DWebBrowserEvents
// Flags:     (4112) Hidden Dispatchable
// GUID:      {EAB22AC2-30C1-11CF-A7EB-0000C05BAE0B}
// *********************************************************************//
  DWebBrowserEvents = dispinterface
    ['{EAB22AC2-30C1-11CF-A7EB-0000C05BAE0B}']
    procedure BeforeNavigate(const URL: WideString; Flags: Integer;
      const TargetFrameName: WideString; var PostData: OleVariant;
      const Headers: WideString; var Cancel: WordBool); dispid 100;
    procedure NavigateComplete(const URL: WideString); dispid 101;
    procedure StatusTextChange(const Text: WideString); dispid 102;
    procedure ProgressChange(Progress: Integer; ProgressMax: Integer); dispid 108;
    procedure DownloadComplete; dispid 104;
    procedure CommandStateChange(Command: Integer; Enable: WordBool); dispid 105;
    procedure DownloadBegin; dispid 106;
    procedure NewWindow(const URL: WideString; Flags: Integer; const TargetFrameName: WideString;
      var PostData: OleVariant; const Headers: WideString; var Processed: WordBool); dispid 107;
    procedure TitleChange(const Text: WideString); dispid 113;
    procedure FrameBeforeNavigate(const URL: WideString; Flags: Integer;
      const TargetFrameName: WideString; var PostData: OleVariant;
      const Headers: WideString; var Cancel: WordBool); dispid 200;
    procedure FrameNavigateComplete(const URL: WideString); dispid 201;
    procedure FrameNewWindow(const URL: WideString; Flags: Integer;
      const TargetFrameName: WideString; var PostData: OleVariant;
      const Headers: WideString; var Processed: WordBool); dispid 204;
    procedure Quit(var Cancel: WordBool); dispid 103;
    procedure WindowMove; dispid 109;
    procedure WindowResize; dispid 110;
    procedure WindowActivate; dispid 111;
    procedure PropertyChange(const Property_: WideString); dispid 112;
  end;

// *********************************************************************//
// Interface: IWebBrowserApp
// Flags:     (4432) Hidden Dual OleAutomation Dispatchable
// GUID:      {0002DF05-0000-0000-C000-000000000046}
// *********************************************************************//
  IWebBrowserApp = interface(IWebBrowser)
    ['{0002DF05-0000-0000-C000-000000000046}']
    procedure Quit; safecall;
    procedure ClientToWindow(var pcx: SYSINT; var pcy: SYSINT); safecall;
    procedure PutProperty(const Property_: WideString; vtValue: OleVariant); safecall;
    function GetProperty(const Property_: WideString): OleVariant; safecall;
    function Get_Name: WideString; safecall;
    function Get_HWND: Integer; safecall;
    function Get_FullName: WideString; safecall;
    function Get_Path: WideString; safecall;
    function Get_Visible: WordBool; safecall;
    procedure Set_Visible(pBool: WordBool); safecall;
    function Get_StatusBar: WordBool; safecall;
    procedure Set_StatusBar(pBool: WordBool); safecall;
    function Get_StatusText: WideString; safecall;
    procedure Set_StatusText(const StatusText: WideString); safecall;
    function Get_ToolBar: SYSINT; safecall;
    procedure Set_ToolBar(Value: SYSINT); safecall;
    function Get_MenuBar: WordBool; safecall;
    procedure Set_MenuBar(Value: WordBool); safecall;
    function Get_FullScreen: WordBool; safecall;
    procedure Set_FullScreen(pbFullScreen: WordBool); safecall;
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

// *********************************************************************//
// DispIntf:  IWebBrowserAppDisp
// Flags:     (4432) Hidden Dual OleAutomation Dispatchable
// GUID:      {0002DF05-0000-0000-C000-000000000046}
// *********************************************************************//
  IWebBrowserAppDisp = dispinterface
    ['{0002DF05-0000-0000-C000-000000000046}']
    procedure Quit; dispid 300;
    procedure ClientToWindow(var pcx: SYSINT; var pcy: SYSINT); dispid 301;
    procedure PutProperty(const Property_: WideString; vtValue: OleVariant); dispid 302;
    function GetProperty(const Property_: WideString): OleVariant; dispid 303;
    property Name: WideString readonly dispid 0;
    property HWND: Integer readonly dispid - 515;
    property FullName: WideString readonly dispid 400;
    property Path: WideString readonly dispid 401;
    property Visible: WordBool dispid 402;
    property StatusBar: WordBool dispid 403;
    property StatusText: WideString dispid 404;
    property ToolBar: SYSINT dispid 405;
    property MenuBar: WordBool dispid 406;
    property FullScreen: WordBool dispid 407;
    procedure GoBack; dispid 100;
    procedure GoForward; dispid 101;
    procedure GoHome; dispid 102;
    procedure GoSearch; dispid 103;
    procedure Navigate(const URL: WideString; var Flags: OleVariant;
      var TargetFrameName: OleVariant; var PostData: OleVariant;
      var Headers: OleVariant); dispid 104;
    procedure Refresh; dispid - 550;
    procedure Refresh2(var Level: OleVariant); dispid 105;
    procedure Stop; dispid 106;
    property Application: IDispatch readonly dispid 200;
    property Parent: IDispatch readonly dispid 201;
    property Container: IDispatch readonly dispid 202;
    property Document: IDispatch readonly dispid 203;
    property TopLevelContainer: WordBool readonly dispid 204;
    property type_: WideString readonly dispid 205;
    property Left: Integer dispid 206;
    property Top: Integer dispid 207;
    property Width: Integer dispid 208;
    property Height: Integer dispid 209;
    property LocationName: WideString readonly dispid 210;
    property LocationURL: WideString readonly dispid 211;
    property Busy: WordBool readonly dispid 212;
  end;

// *********************************************************************//
// Interface: IWebBrowser2
// Flags:     (4432) Hidden Dual OleAutomation Dispatchable
// GUID:      {D30C1661-CDAF-11D0-8A3E-00C04FC9E26E}
// *********************************************************************//
  IWebBrowser2 = interface(IWebBrowserApp)
    ['{D30C1661-CDAF-11D0-8A3E-00C04FC9E26E}']
    procedure Navigate2(var URL: OleVariant; var Flags: OleVariant;
      var TargetFrameName: OleVariant; var PostData: OleVariant;
      var Headers: OleVariant); safecall;
    function QueryStatusWB(cmdID: OLECMDID): OLECMDF; safecall;
    procedure ExecWB(cmdID: OLECMDID; cmdexecopt: OLECMDEXECOPT; var pvaIn: OleVariant;
      var pvaOut: OleVariant); safecall;
    procedure ShowBrowserBar(var pvaClsid: OleVariant; var pvarShow: OleVariant;
      var pvarSize: OleVariant); safecall;
    function Get_ReadyState: tagREADYSTATE; safecall;
    function Get_Offline: WordBool; safecall;
    procedure Set_Offline(pbOffline: WordBool); safecall;
    function Get_Silent: WordBool; safecall;
    procedure Set_Silent(pbSilent: WordBool); safecall;
    function Get_RegisterAsBrowser: WordBool; safecall;
    procedure Set_RegisterAsBrowser(pbRegister: WordBool); safecall;
    function Get_RegisterAsDropTarget: WordBool; safecall;
    procedure Set_RegisterAsDropTarget(pbRegister: WordBool); safecall;
    function Get_TheaterMode: WordBool; safecall;
    procedure Set_TheaterMode(pbRegister: WordBool); safecall;
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

// *********************************************************************//
// DispIntf:  IWebBrowser2Disp
// Flags:     (4432) Hidden Dual OleAutomation Dispatchable
// GUID:      {D30C1661-CDAF-11D0-8A3E-00C04FC9E26E}
// *********************************************************************//
  IWebBrowser2Disp = dispinterface
    ['{D30C1661-CDAF-11D0-8A3E-00C04FC9E26E}']
    procedure Navigate2(var URL: OleVariant; var Flags: OleVariant;
      var TargetFrameName: OleVariant; var PostData: OleVariant;
      var Headers: OleVariant); dispid 500;
    function QueryStatusWB(cmdID: OLECMDID): OLECMDF; dispid 501;
    procedure ExecWB(cmdID: OLECMDID; cmdexecopt: OLECMDEXECOPT; var pvaIn: OleVariant;
      var pvaOut: OleVariant); dispid 502;
    procedure ShowBrowserBar(var pvaClsid: OleVariant; var pvarShow: OleVariant;
      var pvarSize: OleVariant); dispid 503;
    property ReadyState: tagREADYSTATE readonly dispid - 525;
    property Offline: WordBool dispid 550;
    property Silent: WordBool dispid 551;
    property RegisterAsBrowser: WordBool dispid 552;
    property RegisterAsDropTarget: WordBool dispid 553;
    property TheaterMode: WordBool dispid 554;
    property AddressBar: WordBool dispid 555;
    property Resizable: WordBool dispid 556;
    procedure Quit; dispid 300;
    procedure ClientToWindow(var pcx: SYSINT; var pcy: SYSINT); dispid 301;
    procedure PutProperty(const Property_: WideString; vtValue: OleVariant); dispid 302;
    function GetProperty(const Property_: WideString): OleVariant; dispid 303;
    property Name: WideString readonly dispid 0;
    property HWND: Integer readonly dispid - 515;
    property FullName: WideString readonly dispid 400;
    property Path: WideString readonly dispid 401;
    property Visible: WordBool dispid 402;
    property StatusBar: WordBool dispid 403;
    property StatusText: WideString dispid 404;
    property ToolBar: SYSINT dispid 405;
    property MenuBar: WordBool dispid 406;
    property FullScreen: WordBool dispid 407;
    procedure GoBack; dispid 100;
    procedure GoForward; dispid 101;
    procedure GoHome; dispid 102;
    procedure GoSearch; dispid 103;
    procedure Navigate(const URL: WideString; var Flags: OleVariant;
      var TargetFrameName: OleVariant; var PostData: OleVariant;
      var Headers: OleVariant); dispid 104;
    procedure Refresh; dispid - 550;
    procedure Refresh2(var Level: OleVariant); dispid 105;
    procedure Stop; dispid 106;
    property Application: IDispatch readonly dispid 200;
    property Parent: IDispatch readonly dispid 201;
    property Container: IDispatch readonly dispid 202;
    property Document: IDispatch readonly dispid 203;
    property TopLevelContainer: WordBool readonly dispid 204;
    property type_: WideString readonly dispid 205;
    property Left: Integer dispid 206;
    property Top: Integer dispid 207;
    property Width: Integer dispid 208;
    property Height: Integer dispid 209;
    property LocationName: WideString readonly dispid 210;
    property LocationURL: WideString readonly dispid 211;
    property Busy: WordBool readonly dispid 212;
  end;

// *********************************************************************//
// DispIntf:  DWebBrowserEvents2
// Flags:     (4112) Hidden Dispatchable
// GUID:      {34A715A0-6587-11D0-924A-0020AFC7AC4D}
// *********************************************************************//
  DWebBrowserEvents2 = dispinterface
    ['{34A715A0-6587-11D0-924A-0020AFC7AC4D}']
    procedure StatusTextChange(const Text: WideString); dispid 102;
    procedure ProgressChange(Progress: Integer; ProgressMax: Integer); dispid 108;
    procedure CommandStateChange(Command: Integer; Enable: WordBool); dispid 105;
    procedure DownloadBegin; dispid 106;
    procedure DownloadComplete; dispid 104;
    procedure TitleChange(const Text: WideString); dispid 113;
    procedure PropertyChange(const szProperty: WideString); dispid 112;
    procedure BeforeNavigate2(const pDisp: IDispatch; var URL: OleVariant; var Flags: OleVariant;
      var TargetFrameName: OleVariant; var PostData: OleVariant;
      var Headers: OleVariant; var Cancel: WordBool); dispid 250;
    procedure NewWindow2(var ppDisp: IDispatch; var Cancel: WordBool); dispid 251;
    procedure NavigateComplete2(const pDisp: IDispatch; var URL: OleVariant); dispid 252;
    procedure DocumentComplete(const pDisp: IDispatch; var URL: OleVariant); dispid 259;
    procedure OnQuit; dispid 253;
    procedure OnVisible(Visible: WordBool); dispid 254;
    procedure OnToolBar(ToolBar: WordBool); dispid 255;
    procedure OnMenuBar(MenuBar: WordBool); dispid 256;
    procedure OnStatusBar(StatusBar: WordBool); dispid 257;
    procedure OnFullScreen(FullScreen: WordBool); dispid 258;
    procedure OnTheaterMode(TheaterMode: WordBool); dispid 260;
    procedure OnAddressBar(AddressBar: WordBool); dispid 261;
    procedure WindowSetResizable(Resizable: WordBool); dispid 262;
    procedure WindowSetLeft(Left: Integer); dispid 264;
    procedure WindowSetTop(Top: Integer); dispid 265;
    procedure WindowSetWidth(Width: Integer); dispid 266;
    procedure WindowSetHeight(Height: Integer); dispid 267;
    procedure WindowClosing(IsChildWindow: WordBool; var Cancel: WordBool); dispid 263;
    procedure ClientToHostWindow(var CX: Integer; var CY: Integer); dispid 268;
    procedure SetSecureLockIcon(SecureLockIcon: Integer); dispid 269;
     // procedure FileDownload(var Cancel: WordBool); dispid 270;
    procedure FileDownload(ActiveDocument: WordBool; var Cancel: WordBool); dispid 270;
    procedure NavigateError(const pDisp: IDispatch; var URL: OleVariant; var Frame: OleVariant;
      var StatusCode: OleVariant; var Cancel: WordBool); dispid 271;
    procedure PrintTemplateInstantiation(const pDisp: IDispatch); dispid 225;
    procedure PrintTemplateTeardown(const pDisp: IDispatch); dispid 226;
    procedure UpdatePageStatus(const pDisp: IDispatch; var nPage: OleVariant; var fDone: OleVariant); dispid 227;
    procedure PrivacyImpactedStateChange(bImpacted: WordBool); dispid 272;
    procedure NewWindow3(var ppDisp: IDispatch; var Cancel: WordBool; dwFlags: LongWord;
      const bstrUrlContext: WideString; const bstrUrl: WideString); dispid 273;
    procedure SetPhishingFilterStatus(PhishingFilterStatus: Integer); dispid 282;
//     procedure WindowStateChanged(dwWindowStateFlags: LongWord; dwValidFlagsMask: LongWord); dispid 283;
    procedure NewProcess(lCauseFlag: Integer; const pWB2: IDispatch; var Cancel: WordBool); dispid 284;
    procedure ThirdPartyUrlBlocked(var URL: OleVariant; dwCount: LongWord); dispid 285;
  end;

// *********************************************************************//
// DispIntf:  DShellWindowsEvents
// Flags:     (4096) Dispatchable
// GUID:      {FE4106E0-399A-11D0-A48C-00A0C90A8F39}
// *********************************************************************//
  DShellWindowsEvents = dispinterface
    ['{FE4106E0-399A-11D0-A48C-00A0C90A8F39}']
    procedure WindowRegistered(lCookie: Integer); dispid 200;
    procedure WindowRevoked(lCookie: Integer); dispid 201;
  end;

// *********************************************************************//
// Interface: IShellWindows
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {85CB6900-4D95-11CF-960C-0080C7F4EE85}
// *********************************************************************//
  IShellWindows = interface(IDispatch)
    ['{85CB6900-4D95-11CF-960C-0080C7F4EE85}']
    function Get_Count: Integer; safecall;
    function Item(index: OleVariant): IDispatch; safecall;
    function _NewEnum: IUnknown; safecall;
    procedure Register(const pid: IDispatch; HWND: Integer; swClass: SYSINT; out plCookie: Integer); safecall;
    procedure RegisterPending(lThreadId: Integer; var pvarloc: OleVariant;
      var pvarlocRoot: OleVariant; swClass: SYSINT; out plCookie: Integer); safecall;
    procedure Revoke(lCookie: Integer); safecall;
    procedure OnNavigate(lCookie: Integer; var pvarloc: OleVariant); safecall;
    procedure OnActivated(lCookie: Integer; fActive: WordBool); safecall;
    function FindWindowSW(var pvarloc: OleVariant; var pvarlocRoot: OleVariant; swClass: SYSINT;
      out pHWND: Integer; swfwOptions: SYSINT): IDispatch; safecall;
    procedure OnCreated(lCookie: Integer; const punk: IUnknown); safecall;
    procedure ProcessAttachDetach(fAttach: WordBool); safecall;
    property Count: Integer read Get_Count;
  end;

// *********************************************************************//
// DispIntf:  IShellWindowsDisp
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {85CB6900-4D95-11CF-960C-0080C7F4EE85}
// *********************************************************************//
  IShellWindowsDisp = dispinterface
    ['{85CB6900-4D95-11CF-960C-0080C7F4EE85}']
    property Count: Integer readonly dispid 1610743808;
    function Item(index: OleVariant): IDispatch; dispid 0;
    function _NewEnum: IUnknown; dispid - 4;
    procedure Register(const pid: IDispatch; HWND: Integer; swClass: SYSINT; out plCookie: Integer); dispid 1610743811;
    procedure RegisterPending(lThreadId: Integer; var pvarloc: OleVariant;
      var pvarlocRoot: OleVariant; swClass: SYSINT; out plCookie: Integer); dispid 1610743812;
    procedure Revoke(lCookie: Integer); dispid 1610743813;
    procedure OnNavigate(lCookie: Integer; var pvarloc: OleVariant); dispid 1610743814;
    procedure OnActivated(lCookie: Integer; fActive: WordBool); dispid 1610743815;
    function FindWindowSW(var pvarloc: OleVariant; var pvarlocRoot: OleVariant; swClass: SYSINT;
      out pHWND: Integer; swfwOptions: SYSINT): IDispatch; dispid 1610743816;
    procedure OnCreated(lCookie: Integer; const punk: IUnknown); dispid 1610743817;
    procedure ProcessAttachDetach(fAttach: WordBool); dispid 1610743818;
  end;

// *********************************************************************//
// Interface: IShellUIHelper
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {729FE2F8-1EA8-11D1-8F85-00C04FC2FBE1}
// *********************************************************************//
  IShellUIHelper = interface(IDispatch)
    ['{729FE2F8-1EA8-11D1-8F85-00C04FC2FBE1}']
    procedure ResetFirstBootMode; safecall;
    procedure ResetSafeMode; safecall;
    procedure RefreshOfflineDesktop; safecall;
    procedure AddFavorite(const URL: WideString; var Title: OleVariant); safecall;
    procedure AddChannel(const URL: WideString); safecall;
    procedure AddDesktopComponent(const URL: WideString; const Type_: WideString;
      var Left: OleVariant; var Top: OleVariant; var Width: OleVariant;
      var Height: OleVariant); safecall;
    function IsSubscribed(const URL: WideString): WordBool; safecall;
    procedure NavigateAndFind(const URL: WideString; const strQuery: WideString;
      var varTargetFrame: OleVariant); safecall;
    procedure ImportExportFavorites(fImport: WordBool; const strImpExpPath: WideString); safecall;
    procedure AutoCompleteSaveForm(var Form: OleVariant); safecall;
    procedure AutoScan(const strSearch: WideString; const strFailureUrl: WideString;
      var pvarTargetFrame: OleVariant); safecall;
    procedure AutoCompleteAttach(var Reserved: OleVariant); safecall;
    function ShowBrowserUI(const bstrName: WideString; var pvarIn: OleVariant): OleVariant; safecall;
  end;

// *********************************************************************//
// DispIntf:  IShellUIHelperDisp
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {729FE2F8-1EA8-11D1-8F85-00C04FC2FBE1}
// *********************************************************************//
  IShellUIHelperDisp = dispinterface
    ['{729FE2F8-1EA8-11D1-8F85-00C04FC2FBE1}']
    procedure ResetFirstBootMode; dispid 1;
    procedure ResetSafeMode; dispid 2;
    procedure RefreshOfflineDesktop; dispid 3;
    procedure AddFavorite(const URL: WideString; var Title: OleVariant); dispid 4;
    procedure AddChannel(const URL: WideString); dispid 5;
    procedure AddDesktopComponent(const URL: WideString; const Type_: WideString;
      var Left: OleVariant; var Top: OleVariant; var Width: OleVariant;
      var Height: OleVariant); dispid 6;
    function IsSubscribed(const URL: WideString): WordBool; dispid 7;
    procedure NavigateAndFind(const URL: WideString; const strQuery: WideString;
      var varTargetFrame: OleVariant); dispid 8;
    procedure ImportExportFavorites(fImport: WordBool; const strImpExpPath: WideString); dispid 9;
    procedure AutoCompleteSaveForm(var Form: OleVariant); dispid 10;
    procedure AutoScan(const strSearch: WideString; const strFailureUrl: WideString;
      var pvarTargetFrame: OleVariant); dispid 11;
    procedure AutoCompleteAttach(var Reserved: OleVariant); dispid 12;
    function ShowBrowserUI(const bstrName: WideString; var pvarIn: OleVariant): OleVariant; dispid 13;
  end;

//*********************************************************************//
// Interface: IShellUIHelper2
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {A7FE6EDA-1932-4281-B881-87B31B8BC52C}
// *********************************************************************//
  IShellUIHelper2 = interface(IShellUIHelper)
    ['{A7FE6EDA-1932-4281-B881-87B31B8BC52C}']
    procedure AddSearchProvider(const URL: WideString); safecall;
    procedure RunOnceShown; safecall;
    procedure SkipRunOnce; safecall;
    procedure CustomizeSettings(fSQM: WordBool; fPhishing: WordBool; const bstrLocale: WideString); safecall;
    function SqmEnabled: WordBool; safecall;
    function PhishingEnabled: WordBool; safecall;
    function BrandImageUri: WideString; safecall;
    procedure SkipTabsWelcome; safecall;
    procedure DiagnoseConnection; safecall;
    procedure CustomizeClearType(fSet: WordBool); safecall;
    function IsSearchProviderInstalled(const URL: WideString): LongWord; safecall;
    function IsSearchMigrated: WordBool; safecall;
    function DefaultSearchProvider: WideString; safecall;
    procedure RunOnceRequiredSettingsComplete(fComplete: WordBool); safecall;
    function RunOnceHasShown: WordBool; safecall;
    function SearchGuideUrl: WideString; safecall;
  end;

// *********************************************************************//
// DispIntf:  IShellUIHelper2Disp
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {A7FE6EDA-1932-4281-B881-87B31B8BC52C}
// *********************************************************************//
  IShellUIHelper2Disp = dispinterface
    ['{A7FE6EDA-1932-4281-B881-87B31B8BC52C}']
    procedure AddSearchProvider(const URL: WideString); dispid 14;
    procedure RunOnceShown; dispid 15;
    procedure SkipRunOnce; dispid 16;
    procedure CustomizeSettings(fSQM: WordBool; fPhishing: WordBool; const bstrLocale: WideString); dispid 17;
    function SqmEnabled: WordBool; dispid 18;
    function PhishingEnabled: WordBool; dispid 19;
    function BrandImageUri: WideString; dispid 20;
    procedure SkipTabsWelcome; dispid 21;
    procedure DiagnoseConnection; dispid 22;
    procedure CustomizeClearType(fSet: WordBool); dispid 23;
    function IsSearchProviderInstalled(const URL: WideString): LongWord; dispid 24;
    function IsSearchMigrated: WordBool; dispid 25;
    function DefaultSearchProvider: WideString; dispid 26;
    procedure RunOnceRequiredSettingsComplete(fComplete: WordBool); dispid 27;
    function RunOnceHasShown: WordBool; dispid 28;
    function SearchGuideUrl: WideString; dispid 29;
    procedure ResetFirstBootMode; dispid 1;
    procedure ResetSafeMode; dispid 2;
    procedure RefreshOfflineDesktop; dispid 3;
    procedure AddFavorite(const URL: WideString; var Title: OleVariant); dispid 4;
    procedure AddChannel(const URL: WideString); dispid 5;
    procedure AddDesktopComponent(const URL: WideString; const Type_: WideString;
      var Left: OleVariant; var Top: OleVariant; var Width: OleVariant;
      var Height: OleVariant); dispid 6;
    function IsSubscribed(const URL: WideString): WordBool; dispid 7;
    procedure NavigateAndFind(const URL: WideString; const strQuery: WideString;
      var varTargetFrame: OleVariant); dispid 8;
    procedure ImportExportFavorites(fImport: WordBool; const strImpExpPath: WideString); dispid 9;
    procedure AutoCompleteSaveForm(var Form: OleVariant); dispid 10;
    procedure AutoScan(const strSearch: WideString; const strFailureUrl: WideString;
      var pvarTargetFrame: OleVariant); dispid 11;
    procedure AutoCompleteAttach(var Reserved: OleVariant); dispid 12;
    function ShowBrowserUI(const bstrName: WideString; var pvarIn: OleVariant): OleVariant; dispid 13;
  end;

// *********************************************************************//
// Interface: IShellUIHelper3
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {B7FCA399-7F15-42DF-A51C-E39C1A4F32DE}
// *********************************************************************//
  IShellUIHelper3 = interface(IShellUIHelper2)
    ['{B7FCA399-7F15-42DF-A51C-E39C1A4F32DE}']
    procedure AddService(const URL: WideString); safecall;
    procedure AddInPrivateSubscription(const URL: WideString; const bstrFilterName: WideString); safecall;
    function IsServiceInstalled(const URL: WideString; const Verb: WideString): LongWord; safecall;
    procedure AddToFavoritesBar(const URL: WideString; const Title: WideString;
      var Type_: OleVariant); safecall;
    procedure BuildNewTabPage; safecall;
    procedure SetRecentlyClosedVisible(fVisible: WordBool); safecall;
    procedure SetActivitiesVisible(fVisible: WordBool); safecall;
    procedure ContentDiscoveryReset; safecall;
    function IsSuggestedSitesEnabled: WordBool; safecall;
    procedure EnableSuggestedSites(fEnable: WordBool); safecall;
    procedure NavigateToSuggestedSites(const bstrRelativeUrl: WideString); safecall;
    procedure ShowTabsHelp; safecall;
    procedure ShowInPrivateHelp; safecall;
  end;

// *********************************************************************//
// DispIntf:  IShellUIHelper3Disp
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {B7FCA399-7F15-42DF-A51C-E39C1A4F32DE}
// *********************************************************************//
  IShellUIHelper3Disp = dispinterface
    ['{B7FCA399-7F15-42DF-A51C-E39C1A4F32DE}']
    procedure AddService(const URL: WideString); dispid 30;
    procedure AddInPrivateSubscription(const URL: WideString; const bstrFilterName: WideString); dispid 37;
    function IsServiceInstalled(const URL: WideString; const Verb: WideString): LongWord; dispid 31;
    procedure AddToFavoritesBar(const URL: WideString; const Title: WideString;
      var Type_: OleVariant); dispid 32;
    procedure BuildNewTabPage; dispid 33;
    procedure SetRecentlyClosedVisible(fVisible: WordBool); dispid 34;
    procedure SetActivitiesVisible(fVisible: WordBool); dispid 35;
    procedure ContentDiscoveryReset; dispid 36;
    function IsSuggestedSitesEnabled: WordBool; dispid 38;
    procedure EnableSuggestedSites(fEnable: WordBool); dispid 39;
    procedure NavigateToSuggestedSites(const bstrRelativeUrl: WideString); dispid 40;
    procedure ShowTabsHelp; dispid 41;
    procedure ShowInPrivateHelp; dispid 42;
    procedure AddSearchProvider(const URL: WideString); dispid 14;
    procedure RunOnceShown; dispid 15;
    procedure SkipRunOnce; dispid 16;
    procedure CustomizeSettings(fSQM: WordBool; fPhishing: WordBool; const bstrLocale: WideString); dispid 17;
    function SqmEnabled: WordBool; dispid 18;
    function PhishingEnabled: WordBool; dispid 19;
    function BrandImageUri: WideString; dispid 20;
    procedure SkipTabsWelcome; dispid 21;
    procedure DiagnoseConnection; dispid 22;
    procedure CustomizeClearType(fSet: WordBool); dispid 23;
    function IsSearchProviderInstalled(const URL: WideString): LongWord; dispid 24;
    function IsSearchMigrated: WordBool; dispid 25;
    function DefaultSearchProvider: WideString; dispid 26;
    procedure RunOnceRequiredSettingsComplete(fComplete: WordBool); dispid 27;
    function RunOnceHasShown: WordBool; dispid 28;
    function SearchGuideUrl: WideString; dispid 29;
    procedure ResetFirstBootMode; dispid 1;
    procedure ResetSafeMode; dispid 2;
    procedure RefreshOfflineDesktop; dispid 3;
    procedure AddFavorite(const URL: WideString; var Title: OleVariant); dispid 4;
    procedure AddChannel(const URL: WideString); dispid 5;
    procedure AddDesktopComponent(const URL: WideString; const Type_: WideString;
      var Left: OleVariant; var Top: OleVariant; var Width: OleVariant;
      var Height: OleVariant); dispid 6;
    function IsSubscribed(const URL: WideString): WordBool; dispid 7;
    procedure NavigateAndFind(const URL: WideString; const strQuery: WideString;
      var varTargetFrame: OleVariant); dispid 8;
    procedure ImportExportFavorites(fImport: WordBool; const strImpExpPath: WideString); dispid 9;
    procedure AutoCompleteSaveForm(var Form: OleVariant); dispid 10;
    procedure AutoScan(const strSearch: WideString; const strFailureUrl: WideString;
      var pvarTargetFrame: OleVariant); dispid 11;
    procedure AutoCompleteAttach(var Reserved: OleVariant); dispid 12;
    function ShowBrowserUI(const bstrName: WideString; var pvarIn: OleVariant): OleVariant; dispid 13;
  end;

// *********************************************************************//
// DispIntf:  DShellNameSpaceEvents
// Flags:     (4096) Dispatchable
// GUID:      {55136806-B2DE-11D1-B9F2-00A0C98BC547}
// *********************************************************************//
  DShellNameSpaceEvents = dispinterface
    ['{55136806-B2DE-11D1-B9F2-00A0C98BC547}']
    procedure FavoritesSelectionChange(cItems: Integer; hItem: Integer; const strName: WideString;
      const strUrl: WideString; cVisits: Integer;
      const strDate: WideString; fAvailableOffline: Integer); dispid 1;
    procedure SelectionChange; dispid 2;
    procedure DoubleClick; dispid 3;
    procedure Initialized; dispid 4;
  end;

// *********************************************************************//
// Interface: IShellFavoritesNameSpace
// Flags:     (4432) Hidden Dual OleAutomation Dispatchable
// GUID:      {55136804-B2DE-11D1-B9F2-00A0C98BC547}
// *********************************************************************//
  IShellFavoritesNameSpace = interface(IDispatch)
    ['{55136804-B2DE-11D1-B9F2-00A0C98BC547}']
    procedure MoveSelectionUp; safecall;
    procedure MoveSelectionDown; safecall;
    procedure ResetSort; safecall;
    procedure NewFolder; safecall;
    procedure Synchronize; safecall;
    procedure Import; safecall;
    procedure Export; safecall;
    procedure InvokeContextMenuCommand(const strCommand: WideString); safecall;
    procedure MoveSelectionTo; safecall;
    function Get_SubscriptionsEnabled: WordBool; safecall;
    function CreateSubscriptionForSelection: WordBool; safecall;
    function DeleteSubscriptionForSelection: WordBool; safecall;
    procedure SetRoot(const bstrFullPath: WideString); safecall;
    property SubscriptionsEnabled: WordBool read Get_SubscriptionsEnabled;
  end;

// *********************************************************************//
// DispIntf:  IShellFavoritesNameSpaceDisp
// Flags:     (4432) Hidden Dual OleAutomation Dispatchable
// GUID:      {55136804-B2DE-11D1-B9F2-00A0C98BC547}
// *********************************************************************//
  IShellFavoritesNameSpaceDisp = dispinterface
    ['{55136804-B2DE-11D1-B9F2-00A0C98BC547}']
    procedure MoveSelectionUp; dispid 1;
    procedure MoveSelectionDown; dispid 2;
    procedure ResetSort; dispid 3;
    procedure NewFolder; dispid 4;
    procedure Synchronize; dispid 5;
    procedure Import; dispid 6;
    procedure Export; dispid 7;
    procedure InvokeContextMenuCommand(const strCommand: WideString); dispid 8;
    procedure MoveSelectionTo; dispid 9;
    property SubscriptionsEnabled: WordBool readonly dispid 10;
    function CreateSubscriptionForSelection: WordBool; dispid 11;
    function DeleteSubscriptionForSelection: WordBool; dispid 12;
    procedure SetRoot(const bstrFullPath: WideString); dispid 13;
  end;

// *********************************************************************//
// Interface: IShellNameSpace
// Flags:     (4432) Hidden Dual OleAutomation Dispatchable
// GUID:      {E572D3C9-37BE-4AE2-825D-D521763E3108}
// *********************************************************************//
  IShellNameSpace = interface(IShellFavoritesNameSpace)
    ['{E572D3C9-37BE-4AE2-825D-D521763E3108}']
    function Get_EnumOptions: Integer; safecall;
    procedure Set_EnumOptions(pgrfEnumFlags: Integer); safecall;
    function Get_SelectedItem: IDispatch; safecall;
    procedure Set_SelectedItem(const pItem: IDispatch); safecall;
    function Get_Root: OleVariant; safecall;
    procedure Set_Root(pvar: OleVariant); safecall;
    function Get_Depth: SYSINT; safecall;
    procedure Set_Depth(piDepth: SYSINT); safecall;
    function Get_Mode: SYSUINT; safecall;
    procedure Set_Mode(puMode: SYSUINT); safecall;
    function Get_Flags: LongWord; safecall;
    procedure Set_Flags(pdwFlags: LongWord); safecall;
    procedure Set_TVFlags(dwFlags: LongWord); safecall;
    function Get_TVFlags: LongWord; safecall;
    function Get_Columns: WideString; safecall;
    procedure Set_Columns(const bstrColumns: WideString); safecall;
    function Get_CountViewTypes: SYSINT; safecall;
    procedure SetViewType(iType: SYSINT); safecall;
    function SelectedItems: IDispatch; safecall;
    procedure Expand(var_: OleVariant; iDepth: SYSINT); safecall;
    procedure UnselectAll; safecall;
    property EnumOptions: Integer read Get_EnumOptions write Set_EnumOptions;
    property SelectedItem: IDispatch read Get_SelectedItem write Set_SelectedItem;
    property Root: OleVariant read Get_Root write Set_Root;
    property Depth: SYSINT read Get_Depth write Set_Depth;
    property Mode: SYSUINT read Get_Mode write Set_Mode;
    property Flags: LongWord read Get_Flags write Set_Flags;
    property TVFlags: LongWord read Get_TVFlags write Set_TVFlags;
    property Columns: WideString read Get_Columns write Set_Columns;
    property CountViewTypes: SYSINT read Get_CountViewTypes;
  end;

// *********************************************************************//
// DispIntf:  IShellNameSpaceDisp
// Flags:     (4432) Hidden Dual OleAutomation Dispatchable
// GUID:      {E572D3C9-37BE-4AE2-825D-D521763E3108}
// *********************************************************************//
  IShellNameSpaceDisp = dispinterface
    ['{E572D3C9-37BE-4AE2-825D-D521763E3108}']
    property EnumOptions: Integer dispid 14;
    property SelectedItem: IDispatch dispid 15;
    property Root: OleVariant dispid 16;
    property Depth: SYSINT dispid 17;
    property Mode: SYSUINT dispid 18;
    property Flags: LongWord dispid 19;
    property TVFlags: LongWord dispid 20;
    property Columns: WideString dispid 21;
    property CountViewTypes: SYSINT readonly dispid 22;
    procedure SetViewType(iType: SYSINT); dispid 23;
    function SelectedItems: IDispatch; dispid 24;
    procedure Expand(var_: OleVariant; iDepth: SYSINT); dispid 25;
    procedure UnselectAll; dispid 26;
    procedure MoveSelectionUp; dispid 1;
    procedure MoveSelectionDown; dispid 2;
    procedure ResetSort; dispid 3;
    procedure NewFolder; dispid 4;
    procedure Synchronize; dispid 5;
    procedure Import; dispid 6;
    procedure Export; dispid 7;
    procedure InvokeContextMenuCommand(const strCommand: WideString); dispid 8;
    procedure MoveSelectionTo; dispid 9;
    property SubscriptionsEnabled: WordBool readonly dispid 10;
    function CreateSubscriptionForSelection: WordBool; dispid 11;
    function DeleteSubscriptionForSelection: WordBool; dispid 12;
    procedure SetRoot(const bstrFullPath: WideString); dispid 13;
  end;

// *********************************************************************//
// Interface: IScriptErrorList
// Flags:     (4432) Hidden Dual OleAutomation Dispatchable
// GUID:      {F3470F24-15FD-11D2-BB2E-00805FF7EFCA}
// *********************************************************************//
  IScriptErrorList = interface(IDispatch)
    ['{F3470F24-15FD-11D2-BB2E-00805FF7EFCA}']
    procedure advanceError; safecall;
    procedure retreatError; safecall;
    function canAdvanceError: Integer; safecall;
    function canRetreatError: Integer; safecall;
    function getErrorLine: Integer; safecall;
    function getErrorChar: Integer; safecall;
    function getErrorCode: Integer; safecall;
    function getErrorMsg: WideString; safecall;
    function getErrorUrl: WideString; safecall;
    function getAlwaysShowLockState: Integer; safecall;
    function getDetailsPaneOpen: Integer; safecall;
    procedure setDetailsPaneOpen(fDetailsPaneOpen: Integer); safecall;
    function getPerErrorDisplay: Integer; safecall;
    procedure setPerErrorDisplay(fPerErrorDisplay: Integer); safecall;
  end;

// *********************************************************************//
// DispIntf:  IScriptErrorListDisp
// Flags:     (4432) Hidden Dual OleAutomation Dispatchable
// GUID:      {F3470F24-15FD-11D2-BB2E-00805FF7EFCA}
// *********************************************************************//
  IScriptErrorListDisp = dispinterface
    ['{F3470F24-15FD-11D2-BB2E-00805FF7EFCA}']
    procedure advanceError; dispid 10;
    procedure retreatError; dispid 11;
    function canAdvanceError: Integer; dispid 12;
    function canRetreatError: Integer; dispid 13;
    function getErrorLine: Integer; dispid 14;
    function getErrorChar: Integer; dispid 15;
    function getErrorCode: Integer; dispid 16;
    function getErrorMsg: WideString; dispid 17;
    function getErrorUrl: WideString; dispid 18;
    function getAlwaysShowLockState: Integer; dispid 23;
    function getDetailsPaneOpen: Integer; dispid 19;
    procedure setDetailsPaneOpen(fDetailsPaneOpen: Integer); dispid 20;
    function getPerErrorDisplay: Integer; dispid 21;
    procedure setPerErrorDisplay(fPerErrorDisplay: Integer); dispid 22;
  end;

// *********************************************************************//
// OLE Control Proxy class declaration
// Control Name     : TEWB_V1
// Help String      : WebBrowser Control
// Default Interface: IWebBrowser
// Def. Intf. DISP? : No
// Event   Interface: DWebBrowserEvents
// TypeFlags        : (34) CanCreate Control
// *********************************************************************//
  TEWB_V1BeforeNavigate = procedure(ASender: TObject; const URL: WideString; Flags: Integer;
    const TargetFrameName: WideString;
    var PostData: OleVariant;
    const Headers: WideString;
    var Cancel: WordBool) of object;
  TEWB_V1NavigateComplete = procedure(ASender: TObject; const URL: WideString) of object;
  TEWB_V1StatusTextChange = procedure(ASender: TObject; const Text: WideString) of object;
  TEWB_V1ProgressChange = procedure(ASender: TObject; Progress: Integer; ProgressMax: Integer) of object;
  TEWB_V1CommandStateChange = procedure(ASender: TObject; Command: Integer; Enable: WordBool) of object;
  TEWB_V1NewWindow = procedure(ASender: TObject; const URL: WideString; Flags: Integer;
    const TargetFrameName: WideString;
    var PostData: OleVariant;
    const Headers: WideString;
    var Processed: WordBool) of object;
  TEWB_V1TitleChange = procedure(ASender: TObject; const Text: WideString) of object;
  TEWB_V1FrameBeforeNavigate = procedure(ASender: TObject; const URL: WideString;
    Flags: Integer;
    const TargetFrameName: WideString;
    var PostData: OleVariant;
    const Headers: WideString;
    var Cancel: WordBool) of object;
  TEWB_V1FrameNavigateComplete = procedure(ASender: TObject; const URL: WideString) of object;
  TEWB_V1FrameNewWindow = procedure(ASender: TObject; const URL: WideString; Flags: Integer;
    const TargetFrameName: WideString;
    var PostData: OleVariant;
    const Headers: WideString;
    var Processed: WordBool) of object;
  TEWB_V1Quit = procedure(ASender: TObject; var Cancel: WordBool) of object;
  TEWB_V1PropertyChange = procedure(ASender: TObject; const Property_: WideString) of object;

  TEWB_V1 = class(TOleControl)
  private
    FOnBeforeNavigate: TEWB_V1BeforeNavigate;
    FOnNavigateComplete: TEWB_V1NavigateComplete;
    FOnStatusTextChange: TEWB_V1StatusTextChange;
    FOnProgressChange: TEWB_V1ProgressChange;
    FOnDownloadComplete: TNotifyEvent;
    FOnCommandStateChange: TEWB_V1CommandStateChange;
    FOnDownloadBegin: TNotifyEvent;
    FOnNewWindow: TEWB_V1NewWindow;
    FOnTitleChange: TEWB_V1TitleChange;
    FOnFrameBeforeNavigate: TEWB_V1FrameBeforeNavigate;
    FOnFrameNavigateComplete: TEWB_V1FrameNavigateComplete;
    FOnFrameNewWindow: TEWB_V1FrameNewWindow;
    FOnQuit: TEWB_V1Quit;
    FOnWindowMove: TNotifyEvent;
    FOnWindowResize: TNotifyEvent;
    FOnWindowActivate: TNotifyEvent;
    FOnPropertyChange: TEWB_V1PropertyChange;
    FIntf: IWebBrowser;
    function GetControlInterface: IWebBrowser;
  protected
    procedure CreateControl;
    procedure InitControlData; override;
    function Get_Application: IDispatch;
    function Get_Parent: IDispatch;
    function Get_Container: IDispatch;
    function Get_Document: IDispatch;
  public
    procedure GoBack;
    procedure GoForward;
    procedure GoHome;
    procedure GoSearch;
    procedure Navigate(const URL: WideString); overload;
    procedure Navigate(const URL: WideString; var Flags: OleVariant); overload;
    procedure Navigate(const URL: WideString; var Flags: OleVariant; var TargetFrameName: OleVariant); overload;
    procedure Navigate(const URL: WideString; var Flags: OleVariant;
      var TargetFrameName: OleVariant; var PostData: OleVariant); overload;
    procedure Navigate(const URL: WideString; var Flags: OleVariant;
      var TargetFrameName: OleVariant; var PostData: OleVariant;
      var Headers: OleVariant); overload;
    procedure Refresh;
    procedure Refresh2; overload;
    procedure Refresh2(var Level: OleVariant); overload;
    procedure Stop;
    property ControlInterface: IWebBrowser read GetControlInterface;
    property DefaultInterface: IWebBrowser read GetControlInterface;
    property Application: IDispatch index 200 read GetIDispatchProp;
    property Parent: IDispatch index 201 read GetIDispatchProp;
    property Container: IDispatch index 202 read GetIDispatchProp;
    property Document: IDispatch index 203 read GetIDispatchProp;
    property TopLevelContainer: WordBool index 204 read GetWordBoolProp;
    property type_: WideString index 205 read GetWideStringProp;
    property LocationName: WideString index 210 read GetWideStringProp;
    property LocationURL: WideString index 211 read GetWideStringProp;
    property Busy: WordBool index 212 read GetWordBoolProp;
  published
    property Anchors;
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
    property OnBeforeNavigate: TEWB_V1BeforeNavigate read FOnBeforeNavigate write FOnBeforeNavigate;
    property OnNavigateComplete: TEWB_V1NavigateComplete read FOnNavigateComplete write FOnNavigateComplete;
    property OnStatusTextChange: TEWB_V1StatusTextChange read FOnStatusTextChange write FOnStatusTextChange;
    property OnProgressChange: TEWB_V1ProgressChange read FOnProgressChange write FOnProgressChange;
    property OnDownloadComplete: TNotifyEvent read FOnDownloadComplete write FOnDownloadComplete;
    property OnCommandStateChange: TEWB_V1CommandStateChange read FOnCommandStateChange write FOnCommandStateChange;
    property OnDownloadBegin: TNotifyEvent read FOnDownloadBegin write FOnDownloadBegin;
    property OnNewWindow: TEWB_V1NewWindow read FOnNewWindow write FOnNewWindow;
    property OnTitleChange: TEWB_V1TitleChange read FOnTitleChange write FOnTitleChange;
    property OnFrameBeforeNavigate: TEWB_V1FrameBeforeNavigate read FOnFrameBeforeNavigate write FOnFrameBeforeNavigate;
    property OnFrameNavigateComplete: TEWB_V1FrameNavigateComplete read FOnFrameNavigateComplete write FOnFrameNavigateComplete;
    property OnFrameNewWindow: TEWB_V1FrameNewWindow read FOnFrameNewWindow write FOnFrameNewWindow;
    property OnQuit: TEWB_V1Quit read FOnQuit write FOnQuit;
    property OnWindowMove: TNotifyEvent read FOnWindowMove write FOnWindowMove;
    property OnWindowResize: TNotifyEvent read FOnWindowResize write FOnWindowResize;
    property OnWindowActivate: TNotifyEvent read FOnWindowActivate write FOnWindowActivate;
    property OnPropertyChange: TEWB_V1PropertyChange read FOnPropertyChange write FOnPropertyChange;
  end;

// *********************************************************************//
// OLE Control Proxy class declaration
// Control Name     : TEWB
// Help String      : WebBrowser Control
// Default Interface: IWebBrowser2
// Def. Intf. DISP? : No
// Event   Interface: DWebBrowserEvents2
// TypeFlags        : (34) CanCreate Control
// *********************************************************************//
  TEWBStatusTextChange = procedure(ASender: TObject; const Text: WideString) of object;
  TEWBProgressChange = procedure(ASender: TObject; Progress: Integer; ProgressMax: Integer) of object;
  TEWBCommandStateChange = procedure(ASender: TObject; Command: Integer; Enable: WordBool) of object;
  TEWBTitleChange = procedure(ASender: TObject; const Text: WideString) of object;
  TEWBPropertyChange = procedure(ASender: TObject; const szProperty: WideString) of object;
  TEWBBeforeNavigate2 = procedure(ASender: TObject; const pDisp: IDispatch;
    var URL: OleVariant;
    var Flags: OleVariant;
    var TargetFrameName: OleVariant;
    var PostData: OleVariant;
    var Headers: OleVariant;
    var Cancel: WordBool) of object;
  TEWBNewWindow2 = procedure(ASender: TObject; var ppDisp: IDispatch; var Cancel: WordBool) of object;
  TEWBNavigateComplete2 = procedure(ASender: TObject; const pDisp: IDispatch;
    var URL: OleVariant) of object;
  TEWBDocumentComplete = procedure(ASender: TObject; const pDisp: IDispatch;
    var URL: OleVariant) of object;
  TEWBOnVisible = procedure(ASender: TObject; Visible: WordBool) of object;
  TEWBOnToolBar = procedure(ASender: TObject; ToolBar: WordBool) of object;
  TEWBOnMenuBar = procedure(ASender: TObject; MenuBar: WordBool) of object;
  TEWBOnStatusBar = procedure(ASender: TObject; StatusBar: WordBool) of object;
  TEWBOnFullScreen = procedure(ASender: TObject; FullScreen: WordBool) of object;
  TEWBOnTheaterMode = procedure(ASender: TObject; TheaterMode: WordBool) of object;
  TEWBOnAddressBar = procedure(ASender: TObject; AddressBar: WordBool) of object;
  TEWBWindowSetResizable = procedure(ASender: TObject; Resizable: WordBool) of object;
  TEWBWindowSetLeft = procedure(ASender: TObject; Left: Integer) of object;
  TEWBWindowSetTop = procedure(ASender: TObject; Top: Integer) of object;
  TEWBWindowSetWidth = procedure(ASender: TObject; Width: Integer) of object;
  TEWBWindowSetHeight = procedure(ASender: TObject; Height: Integer) of object;
  TEWBWindowClosing = procedure(ASender: TObject; IsChildWindow: WordBool;
    var Cancel: WordBool) of object;
  TEWBClientToHostWindow = procedure(ASender: TObject; var CX: Integer; var CY: Integer) of object;
  TEWBSetSecureLockIcon = procedure(ASender: TObject; SecureLockIcon: Integer) of object;
  TEWBFileDownload = procedure(ASender: TObject; var Cancel: WordBool) of object;
  TEWBNavigateError = procedure(ASender: TObject; const pDisp: IDispatch;
    var URL: OleVariant;
    var Frame: OleVariant;
    var StatusCode: OleVariant;
    var Cancel: WordBool) of object;
  TEWBPrintTemplateInstantiation = procedure(ASender: TObject; const pDisp: IDispatch) of object;
  TEWBPrintTemplateTeardown = procedure(ASender: TObject; const pDisp: IDispatch) of object;
  TEWBUpdatePageStatus = procedure(ASender: TObject; const pDisp: IDispatch;
    var nPage: OleVariant;
    var fDone: OleVariant) of object;
  TEWBPrivacyImpactedStateChange = procedure(ASender: TObject; bImpacted: WordBool) of object;
  TEWBNewWindow3 = procedure(ASender: TObject; var ppDisp: IDispatch; var Cancel: WordBool;
    dwFlags: LongWord;
    const bstrUrlContext: WideString;
    const bstrUrl: WideString) of object;

  TEWB = class(TOleControl)
  private
    FOnStatusTextChange: TEWBStatusTextChange;
    FOnProgressChange: TEWBProgressChange;
    FOnCommandStateChange: TEWBCommandStateChange;
    FOnDownloadBegin: TNotifyEvent;
    FOnDownloadComplete: TNotifyEvent;
    FOnTitleChange: TEWBTitleChange;
    FOnPropertyChange: TEWBPropertyChange;
    FOnBeforeNavigate2: TEWBBeforeNavigate2;
    FOnNewWindow2: TEWBNewWindow2;
    FOnNavigateComplete2: TEWBNavigateComplete2;
    FOnDocumentComplete: TEWBDocumentComplete;
    FOnQuit: TNotifyEvent;
    FOnVisible: TEWBOnVisible;
    FOnToolBar: TEWBOnToolBar;
    FOnMenuBar: TEWBOnMenuBar;
    FOnStatusBar: TEWBOnStatusBar;
    FOnFullScreen: TEWBOnFullScreen;
    FOnTheaterMode: TEWBOnTheaterMode;
    FOnAddressBar: TEWBOnAddressBar;
    FOnWindowSetResizable: TEWBWindowSetResizable;
    FOnWindowSetLeft: TEWBWindowSetLeft;
    FOnWindowSetTop: TEWBWindowSetTop;
    FOnWindowSetWidth: TEWBWindowSetWidth;
    FOnWindowSetHeight: TEWBWindowSetHeight;
    FOnWindowClosing: TEWBWindowClosing;
    FOnClientToHostWindow: TEWBClientToHostWindow;
    FOnSetSecureLockIcon: TEWBSetSecureLockIcon;
    FOnFileDownload: TEWBFileDownload;
    FOnNavigateError: TEWBNavigateError;
    FOnPrintTemplateInstantiation: TEWBPrintTemplateInstantiation;
    FOnPrintTemplateTeardown: TEWBPrintTemplateTeardown;
    FOnUpdatePageStatus: TEWBUpdatePageStatus;
    FOnPrivacyImpactedStateChange: TEWBPrivacyImpactedStateChange;
    FOnNewWindow3: TEWBNewWindow3;
    FIntf: IWebBrowser2;
    function GetControlInterface: IWebBrowser2;
  protected
    procedure CreateControl;
    procedure InitControlData; override;
    function Get_Application: IDispatch;
    function Get_Parent: IDispatch;
    function Get_Container: IDispatch;
    function Get_Document: IDispatch;
  public
    procedure GoBack;
    procedure GoForward;
    procedure GoHome;
    procedure GoSearch;
    procedure Navigate(const URL: WideString); overload;
    procedure Navigate(const URL: WideString; var Flags: OleVariant); overload;
    procedure Navigate(const URL: WideString; var Flags: OleVariant; var TargetFrameName: OleVariant); overload;
    procedure Navigate(const URL: WideString; var Flags: OleVariant;
      var TargetFrameName: OleVariant; var PostData: OleVariant); overload;
    procedure Navigate(const URL: WideString; var Flags: OleVariant;
      var TargetFrameName: OleVariant; var PostData: OleVariant;
      var Headers: OleVariant); overload;
    procedure Refresh;
    procedure Refresh2; overload;
    procedure Refresh2(var Level: OleVariant); overload;
    procedure Stop;
    procedure Quit;
    procedure ClientToWindow(var pcx: SYSINT; var pcy: SYSINT);
    procedure PutProperty(const Property_: WideString; vtValue: OleVariant);
    function GetProperty(const Property_: WideString): OleVariant;
    procedure Navigate2(var URL: OleVariant); overload;
    procedure Navigate2(var URL: OleVariant; var Flags: OleVariant); overload;
    procedure Navigate2(var URL: OleVariant; var Flags: OleVariant; var TargetFrameName: OleVariant); overload;
    procedure Navigate2(var URL: OleVariant; var Flags: OleVariant;
      var TargetFrameName: OleVariant; var PostData: OleVariant); overload;
    procedure Navigate2(var URL: OleVariant; var Flags: OleVariant;
      var TargetFrameName: OleVariant; var PostData: OleVariant;
      var Headers: OleVariant); overload;
    function QueryStatusWB(cmdID: OLECMDID): OLECMDF;
    procedure ExecWB(cmdID: OLECMDID; cmdexecopt: OLECMDEXECOPT); overload;
    procedure ExecWB(cmdID: OLECMDID; cmdexecopt: OLECMDEXECOPT; var pvaIn: OleVariant); overload;
    procedure ExecWB(cmdID: OLECMDID; cmdexecopt: OLECMDEXECOPT; var pvaIn: OleVariant;
      var pvaOut: OleVariant); overload;
    procedure ShowBrowserBar(var pvaClsid: OleVariant); overload;
    procedure ShowBrowserBar(var pvaClsid: OleVariant; var pvarShow: OleVariant); overload;
    procedure ShowBrowserBar(var pvaClsid: OleVariant; var pvarShow: OleVariant;
      var pvarSize: OleVariant); overload;
    property ControlInterface: IWebBrowser2 read GetControlInterface;
    property DefaultInterface: IWebBrowser2 read GetControlInterface;
    property Application: IDispatch index 200 read GetIDispatchProp;
    property Parent: IDispatch index 201 read GetIDispatchProp;
    property Container: IDispatch index 202 read GetIDispatchProp;
    property Document: IDispatch index 203 read GetIDispatchProp;
    property TopLevelContainer: WordBool index 204 read GetWordBoolProp;
    property type_: WideString index 205 read GetWideStringProp;
    property LocationName: WideString index 210 read GetWideStringProp;
    property LocationURL: WideString index 211 read GetWideStringProp;
    property Busy: WordBool index 212 read GetWordBoolProp;
    property Name: WideString index 0 read GetWideStringProp;
    property HWND: Integer index - 515 read GetIntegerProp;
    property FullName: WideString index 400 read GetWideStringProp;
    property Path: WideString index 401 read GetWideStringProp;
    property ReadyState: TOleEnum index - 525 read GetTOleEnumProp;
  published
    property Anchors;
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
    property ToolBar: Integer index 405 read GetIntegerProp write SetIntegerProp stored False;
    property MenuBar: WordBool index 406 read GetWordBoolProp write SetWordBoolProp stored False;
    property FullScreen: WordBool index 407 read GetWordBoolProp write SetWordBoolProp stored False;
    property Offline: WordBool index 550 read GetWordBoolProp write SetWordBoolProp stored False;
    property Silent: WordBool index 551 read GetWordBoolProp write SetWordBoolProp stored False;
    property RegisterAsBrowser: WordBool index 552 read GetWordBoolProp write SetWordBoolProp stored False;
    property RegisterAsDropTarget: WordBool index 553 read GetWordBoolProp write SetWordBoolProp stored False;
    property TheaterMode: WordBool index 554 read GetWordBoolProp write SetWordBoolProp stored False;
    property AddressBar: WordBool index 555 read GetWordBoolProp write SetWordBoolProp stored False;
    property Resizable: WordBool index 556 read GetWordBoolProp write SetWordBoolProp stored False;
    property OnStatusTextChange: TEWBStatusTextChange read FOnStatusTextChange write FOnStatusTextChange;
    property OnProgressChange: TEWBProgressChange read FOnProgressChange write FOnProgressChange;
    property OnCommandStateChange: TEWBCommandStateChange read FOnCommandStateChange write FOnCommandStateChange;
    property OnDownloadBegin: TNotifyEvent read FOnDownloadBegin write FOnDownloadBegin;
    property OnDownloadComplete: TNotifyEvent read FOnDownloadComplete write FOnDownloadComplete;
    property OnTitleChange: TEWBTitleChange read FOnTitleChange write FOnTitleChange;
    property OnPropertyChange: TEWBPropertyChange read FOnPropertyChange write FOnPropertyChange;
    property OnBeforeNavigate2: TEWBBeforeNavigate2 read FOnBeforeNavigate2 write FOnBeforeNavigate2;
    property OnNewWindow2: TEWBNewWindow2 read FOnNewWindow2 write FOnNewWindow2;
    property OnNavigateComplete2: TEWBNavigateComplete2 read FOnNavigateComplete2 write FOnNavigateComplete2;
    property OnDocumentComplete: TEWBDocumentComplete read FOnDocumentComplete write FOnDocumentComplete;
    property OnQuit: TNotifyEvent read FOnQuit write FOnQuit;
    property OnVisible: TEWBOnVisible read FOnVisible write FOnVisible;
    property OnToolBar: TEWBOnToolBar read FOnToolBar write FOnToolBar;
    property OnMenuBar: TEWBOnMenuBar read FOnMenuBar write FOnMenuBar;
    property OnStatusBar: TEWBOnStatusBar read FOnStatusBar write FOnStatusBar;
    property OnFullScreen: TEWBOnFullScreen read FOnFullScreen write FOnFullScreen;
    property OnTheaterMode: TEWBOnTheaterMode read FOnTheaterMode write FOnTheaterMode;
    property OnAddressBar: TEWBOnAddressBar read FOnAddressBar write FOnAddressBar;
    property OnWindowSetResizable: TEWBWindowSetResizable read FOnWindowSetResizable write FOnWindowSetResizable;
    property OnWindowSetLeft: TEWBWindowSetLeft read FOnWindowSetLeft write FOnWindowSetLeft;
    property OnWindowSetTop: TEWBWindowSetTop read FOnWindowSetTop write FOnWindowSetTop;
    property OnWindowSetWidth: TEWBWindowSetWidth read FOnWindowSetWidth write FOnWindowSetWidth;
    property OnWindowSetHeight: TEWBWindowSetHeight read FOnWindowSetHeight write FOnWindowSetHeight;
    property OnWindowClosing: TEWBWindowClosing read FOnWindowClosing write FOnWindowClosing;
    property OnClientToHostWindow: TEWBClientToHostWindow read FOnClientToHostWindow write FOnClientToHostWindow;
    property OnSetSecureLockIcon: TEWBSetSecureLockIcon read FOnSetSecureLockIcon write FOnSetSecureLockIcon;
    property OnFileDownload: TEWBFileDownload read FOnFileDownload write FOnFileDownload;
    property OnNavigateError: TEWBNavigateError read FOnNavigateError write FOnNavigateError;
    property OnPrintTemplateInstantiation: TEWBPrintTemplateInstantiation read FOnPrintTemplateInstantiation write FOnPrintTemplateInstantiation;
    property OnPrintTemplateTeardown: TEWBPrintTemplateTeardown read FOnPrintTemplateTeardown write FOnPrintTemplateTeardown;
    property OnUpdatePageStatus: TEWBUpdatePageStatus read FOnUpdatePageStatus write FOnUpdatePageStatus;
    property OnPrivacyImpactedStateChange: TEWBPrivacyImpactedStateChange read FOnPrivacyImpactedStateChange write FOnPrivacyImpactedStateChange;
    property OnNewWindow3: TEWBNewWindow3 read FOnNewWindow3 write FOnNewWindow3;
  end;

// *********************************************************************//
// The Class CoInternetExplorer provides a Create and CreateRemote method to
// create instances of the default interface IWebBrowser2 exposed by
// the CoClass InternetExplorer. The functions are intended to be used by
// clients wishing to automate the CoClass objects exposed by the
// server of this typelibrary.
// *********************************************************************//
  CoInternetExplorer = class
    class function Create: IWebBrowser2;
    class function CreateRemote(const MachineName: string): IWebBrowser2;
  end;

// *********************************************************************//
// The Class CoShellBrowserWindow provides a Create and CreateRemote method to
// create instances of the default interface IWebBrowser2 exposed by
// the CoClass ShellBrowserWindow. The functions are intended to be used by
// clients wishing to automate the CoClass objects exposed by the
// server of this typelibrary.
// *********************************************************************//
  CoShellBrowserWindow = class
    class function Create: IWebBrowser2;
    class function CreateRemote(const MachineName: string): IWebBrowser2;
  end;

// *********************************************************************//
// The Class CoShellWindows provides a Create and CreateRemote method to
// create instances of the default interface IShellWindows exposed by
// the CoClass ShellWindows. The functions are intended to be used by
// clients wishing to automate the CoClass objects exposed by the
// server of this typelibrary.
// *********************************************************************//
  CoShellWindows = class
    class function Create: IShellWindows;
    class function CreateRemote(const MachineName: string): IShellWindows;
  end;

// *********************************************************************//
// The Class CoShellUIHelper provides a Create and CreateRemote method to
// create instances of the default interface IShellUIHelper3 exposed by
// the CoClass ShellUIHelper. The functions are intended to be used by
// clients wishing to automate the CoClass objects exposed by the
// server of this typelibrary.
// *********************************************************************//
  CoShellUIHelper = class
    class function Create: IShellUIHelper3;
    class function CreateRemote(const MachineName: string): IShellUIHelper3;
  end;

// *********************************************************************//
// The Class CoShellNameSpace provides a Create and CreateRemote method to
// create instances of the default interface IShellNameSpace exposed by
// the CoClass ShellNameSpace. The functions are intended to be used by
// clients wishing to automate the CoClass objects exposed by the
// server of this typelibrary.
// *********************************************************************//
  CoShellNameSpace = class
    class function Create: IShellNameSpace;
    class function CreateRemote(const MachineName: string): IShellNameSpace;
  end;

// *********************************************************************//
// The Class CoShellShellNameSpace provides a Create and CreateRemote method to
// create instances of the default interface IShellNameSpace exposed by
// the CoClass ShellShellNameSpace. The functions are intended to be used by
// clients wishing to automate the CoClass objects exposed by the
// server of this typelibrary.
// *********************************************************************//
  CoShellShellNameSpace = class
    class function Create: IShellNameSpace;
    class function CreateRemote(const MachineName: string): IShellNameSpace;
  end;

// *********************************************************************//
// The Class CoCScriptErrorList provides a Create and CreateRemote method to
// create instances of the default interface IScriptErrorList exposed by
// the CoClass CScriptErrorList. The functions are intended to be used by
// clients wishing to automate the CoClass objects exposed by the
// server of this typelibrary.
// *********************************************************************//
  CoCScriptErrorList = class
    class function Create: IScriptErrorList;
    class function CreateRemote(const MachineName: string): IScriptErrorList;
  end;


implementation

uses ComObj;

var
  VarEmptyParam: OleVariant;

procedure TEWB_V1.InitControlData;
const
  CEventDispIDs: array[0..16] of DWORD = (
    $00000064, $00000065, $00000066, $0000006C, $00000068, $00000069,
    $0000006A, $0000006B, $00000071, $000000C8, $000000C9, $000000CC,
    $00000067, $0000006D, $0000006E, $0000006F, $00000070);
  CControlData: TControlData2 = (
    ClassID: '{EAB22AC3-30C1-11CF-A7EB-0000C05BAE0B}';
    EventIID: '{EAB22AC2-30C1-11CF-A7EB-0000C05BAE0B}';
    EventCount: 17;
    EventDispIDs: @CEventDispIDs;
    LicenseKey: nil (*HR:$80040111*);
    Flags: $00000000;
    Version: 401);
begin
  ControlData := @CControlData;
  TControlData2(CControlData).FirstEventOfs := Cardinal(@@FOnBeforeNavigate) - Cardinal(Self);
end;

procedure TEWB_V1.CreateControl;

  procedure DoCreate;
  begin
    FIntf := IUnknown(OleObject) as IWebBrowser;
  end;

begin
  if FIntf = nil then
    DoCreate;
end;

function TEWB_V1.GetControlInterface: IWebBrowser;
begin
  CreateControl;
  Result := FIntf;
end;

function TEWB_V1.Get_Application: IDispatch;
begin
  Result := DefaultInterface.Application;
end;

function TEWB_V1.Get_Parent: IDispatch;
begin
  Result := DefaultInterface.Parent;
end;

function TEWB_V1.Get_Container: IDispatch;
begin
  Result := DefaultInterface.Container;
end;

function TEWB_V1.Get_Document: IDispatch;
begin
  Result := DefaultInterface.Document;
end;

procedure TEWB_V1.GoBack;
begin
  DefaultInterface.GoBack;
end;

procedure TEWB_V1.GoForward;
begin
  DefaultInterface.GoForward;
end;

procedure TEWB_V1.GoHome;
begin
  DefaultInterface.GoHome;
end;

procedure TEWB_V1.GoSearch;
begin
  DefaultInterface.GoSearch;
end;

procedure TEWB_V1.Navigate(const URL: WideString);
begin
  DefaultInterface.Navigate(URL, EmptyParam, EmptyParam, VarEmptyParam, EmptyParam);
end;

procedure TEWB_V1.Navigate(const URL: WideString; var Flags: OleVariant);
begin
  DefaultInterface.Navigate(URL, Flags, EmptyParam, VarEmptyParam, EmptyParam);
end;

procedure TEWB_V1.Navigate(const URL: WideString; var Flags: OleVariant;
  var TargetFrameName: OleVariant);
begin
  DefaultInterface.Navigate(URL, Flags, TargetFrameName, VarEmptyParam, EmptyParam);
end;

procedure TEWB_V1.Navigate(const URL: WideString; var Flags: OleVariant;
  var TargetFrameName: OleVariant; var PostData: OleVariant);
begin
  DefaultInterface.Navigate(URL, Flags, TargetFrameName, VarEmptyParam, EmptyParam);
end;

procedure TEWB_V1.Navigate(const URL: WideString; var Flags: OleVariant;
  var TargetFrameName: OleVariant; var PostData: OleVariant;
  var Headers: OleVariant);
begin
  DefaultInterface.Navigate(URL, Flags, TargetFrameName, PostData, Headers);
end;

procedure TEWB_V1.Refresh;
begin
  DefaultInterface.Refresh;
end;

procedure TEWB_V1.Refresh2;
var
  Level: OleVariant;
begin
  Level := EmptyParam;
  DefaultInterface.Refresh2(Level);
end;

procedure TEWB_V1.Refresh2(var Level: OleVariant);
begin
  DefaultInterface.Refresh2(Level);
end;

procedure TEWB_V1.Stop;
begin
  DefaultInterface.Stop;
end;

procedure TEWB.InitControlData;
const
  CEventDispIDs: array[0..33] of DWORD = (
    $00000066, $0000006C, $00000069, $0000006A, $00000068, $00000071,
    $00000070, $000000FA, $000000FB, $000000FC, $00000103, $000000FD,
    $000000FE, $000000FF, $00000100, $00000101, $00000102, $00000104, $00000105,
    $00000106, $00000108, $00000109, $0000010A, $0000010B, $00000107,
    $0000010C, $0000010D, $0000010E, $0000010F, $000000E1, $000000E2,
    $000000E3, $00000110, $00000111);
  CControlData: TControlData2 = (
    ClassID: '{8856F961-340A-11D0-A96B-00C04FD705A2}';
    EventIID: '{34A715A0-6587-11D0-924A-0020AFC7AC4D}';
    EventCount: 34;
    EventDispIDs: @CEventDispIDs;
    LicenseKey: nil (*HR:$80040111*);
    Flags: $00000000;
    Version: 401);
begin
  ControlData := @CControlData;
  TControlData2(CControlData).FirstEventOfs := Cardinal(@@FOnStatusTextChange) - Cardinal(Self);
end;

procedure TEWB.CreateControl;

  procedure DoCreate;
  begin
    FIntf := IUnknown(OleObject) as IWebBrowser2;
  end;

begin
  if not Assigned(FIntf) then
    DoCreate;
end;

function TEWB.GetControlInterface: IWebBrowser2;
begin
  CreateControl;
  Result := FIntf;
end;

function TEWB.Get_Application: IDispatch;
begin
  Result := DefaultInterface.Application;
end;

function TEWB.Get_Parent: IDispatch;
begin
  Result := DefaultInterface.Parent;
end;

function TEWB.Get_Container: IDispatch;
begin
  Result := DefaultInterface.Container;
end;

function TEWB.Get_Document: IDispatch;
begin
  Result := DefaultInterface.Document;
end;

procedure TEWB.GoBack;
begin
  DefaultInterface.GoBack;
end;

procedure TEWB.GoForward;
begin
  DefaultInterface.GoForward;
end;

procedure TEWB.GoHome;
begin
  DefaultInterface.GoHome;
end;

procedure TEWB.GoSearch;
begin
  DefaultInterface.GoSearch;
end;

procedure TEWB.Navigate(const URL: WideString);
begin
  DefaultInterface.Navigate(URL, EmptyParam, EmptyParam, VarEmptyParam, EmptyParam);
end;

procedure TEWB.Navigate(const URL: WideString; var Flags: OleVariant);
begin
  DefaultInterface.Navigate(URL, Flags, EmptyParam, VarEmptyParam, EmptyParam);
end;

procedure TEWB.Navigate(const URL: WideString; var Flags: OleVariant;
  var TargetFrameName: OleVariant);
begin
  DefaultInterface.Navigate(URL, Flags, TargetFrameName, VarEmptyParam, EmptyParam);
end;

procedure TEWB.Navigate(const URL: WideString; var Flags: OleVariant;
  var TargetFrameName: OleVariant; var PostData: OleVariant);
begin
  DefaultInterface.Navigate(URL, Flags, TargetFrameName, PostData, EmptyParam);
end;

procedure TEWB.Navigate(const URL: WideString; var Flags: OleVariant;
  var TargetFrameName: OleVariant; var PostData: OleVariant;
  var Headers: OleVariant);
begin
  DefaultInterface.Navigate(URL, Flags, TargetFrameName, PostData, Headers);
end;

procedure TEWB.Refresh;
begin
  DefaultInterface.Refresh;
end;

procedure TEWB.Refresh2;
begin
  DefaultInterface.Refresh2(VarEmptyParam);
end;

procedure TEWB.Refresh2(var Level: OleVariant);
begin
  DefaultInterface.Refresh2(Level);
end;

procedure TEWB.Stop;
begin
  DefaultInterface.Stop;
end;

procedure TEWB.Quit;
begin
  DefaultInterface.Quit;
end;

procedure TEWB.ClientToWindow(var pcx: SYSINT; var pcy: SYSINT);
begin
  DefaultInterface.ClientToWindow(pcx, pcy);
end;

procedure TEWB.PutProperty(const Property_: WideString; vtValue: OleVariant);
begin
  DefaultInterface.PutProperty(Property_, vtValue);
end;

function TEWB.GetProperty(const Property_: WideString): OleVariant;
begin
  Result := DefaultInterface.GetProperty(Property_);
end;

procedure TEWB.Navigate2(var URL: OleVariant);
begin
  DefaultInterface.Navigate2(URL, VarEmptyParam, VarEmptyParam, VarEmptyParam, VarEmptyParam);
end;

procedure TEWB.Navigate2(var URL: OleVariant; var Flags: OleVariant);
begin
  DefaultInterface.Navigate2(URL, Flags, VarEmptyParam, VarEmptyParam, VarEmptyParam);
end;

procedure TEWB.Navigate2(var URL: OleVariant; var Flags: OleVariant;
  var TargetFrameName: OleVariant);
begin
  DefaultInterface.Navigate2(URL, Flags, TargetFrameName, VarEmptyParam, VarEmptyParam);
end;

procedure TEWB.Navigate2(var URL: OleVariant; var Flags: OleVariant;
  var TargetFrameName: OleVariant; var PostData: OleVariant);
begin
  DefaultInterface.Navigate2(URL, Flags, TargetFrameName, PostData, VarEmptyParam);
end;

procedure TEWB.Navigate2(var URL: OleVariant; var Flags: OleVariant;
  var TargetFrameName: OleVariant; var PostData: OleVariant;
  var Headers: OleVariant);
begin
  DefaultInterface.Navigate2(URL, Flags, TargetFrameName, PostData, Headers);
end;

function TEWB.QueryStatusWB(cmdID: OLECMDID): OLECMDF;
begin
  Result := DefaultInterface.QueryStatusWB(cmdID);
end;

procedure TEWB.ExecWB(cmdID: OLECMDID; cmdexecopt: OLECMDEXECOPT);
begin
  DefaultInterface.ExecWB(cmdID, cmdexecopt, VarEmptyParam, VarEmptyParam);
end;

procedure TEWB.ExecWB(cmdID: OLECMDID; cmdexecopt: OLECMDEXECOPT; var pvaIn: OleVariant);
begin
  DefaultInterface.ExecWB(cmdID, cmdexecopt, pvaIn, VarEmptyParam);
end;

procedure TEWB.ExecWB(cmdID: OLECMDID; cmdexecopt: OLECMDEXECOPT; var pvaIn: OleVariant;
  var pvaOut: OleVariant);
begin
  DefaultInterface.ExecWB(cmdID, cmdexecopt, pvaIn, pvaOut);
end;

procedure TEWB.ShowBrowserBar(var pvaClsid: OleVariant);
begin
  DefaultInterface.ShowBrowserBar(pvaClsid, VarEmptyParam, VarEmptyParam);
end;

procedure TEWB.ShowBrowserBar(var pvaClsid: OleVariant; var pvarShow: OleVariant);
begin
  DefaultInterface.ShowBrowserBar(pvaClsid, pvarShow, VarEmptyParam);
end;

procedure TEWB.ShowBrowserBar(var pvaClsid: OleVariant; var pvarShow: OleVariant;
  var pvarSize: OleVariant);
begin
  DefaultInterface.ShowBrowserBar(pvaClsid, pvarShow, pvarSize);
end;

class function CoInternetExplorer.Create: IWebBrowser2;
begin
  Result := CreateComObject(CLASS_InternetExplorer) as IWebBrowser2;
end;

class function CoInternetExplorer.CreateRemote(const MachineName: string): IWebBrowser2;
begin
  Result := CreateRemoteComObject(MachineName, CLASS_InternetExplorer) as IWebBrowser2;
end;

class function CoShellBrowserWindow.Create: IWebBrowser2;
begin
  Result := CreateComObject(CLASS_ShellBrowserWindow) as IWebBrowser2;
end;

class function CoShellBrowserWindow.CreateRemote(const MachineName: string): IWebBrowser2;
begin
  Result := CreateRemoteComObject(MachineName, CLASS_ShellBrowserWindow) as IWebBrowser2;
end;

class function CoShellWindows.Create: IShellWindows;
begin
  Result := CreateComObject(CLASS_ShellWindows) as IShellWindows;
end;

class function CoShellWindows.CreateRemote(const MachineName: string): IShellWindows;
begin
  Result := CreateRemoteComObject(MachineName, CLASS_ShellWindows) as IShellWindows;
end;

class function CoShellUIHelper.Create: IShellUIHelper3;
begin
  Result := CreateComObject(CLASS_ShellUIHelper) as IShellUIHelper3;
end;

class function CoShellUIHelper.CreateRemote(const MachineName: string): IShellUIHelper3;
begin
  Result := CreateRemoteComObject(MachineName, CLASS_ShellUIHelper) as IShellUIHelper3;
end;

class function CoShellNameSpace.Create: IShellNameSpace;
begin
  Result := CreateComObject(CLASS_ShellNameSpace) as IShellNameSpace;
end;

class function CoShellNameSpace.CreateRemote(const MachineName: string): IShellNameSpace;
begin
  Result := CreateRemoteComObject(MachineName, CLASS_ShellNameSpace) as IShellNameSpace;
end;

class function CoShellShellNameSpace.Create: IShellNameSpace;
begin
  Result := CreateComObject(CLASS_ShellShellNameSpace) as IShellNameSpace;
end;

class function CoShellShellNameSpace.CreateRemote(const MachineName: string): IShellNameSpace;
begin
  Result := CreateRemoteComObject(MachineName, CLASS_ShellShellNameSpace) as IShellNameSpace;
end;

class function CoCScriptErrorList.Create: IScriptErrorList;
begin
  Result := CreateComObject(CLASS_CScriptErrorList) as IScriptErrorList;
end;

class function CoCScriptErrorList.CreateRemote(const MachineName: string): IScriptErrorList;
begin
  Result := CreateRemoteComObject(MachineName, CLASS_CScriptErrorList) as IScriptErrorList;
end;



end.
