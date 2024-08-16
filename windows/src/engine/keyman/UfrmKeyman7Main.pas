(*
  Name:             UfrmKeyman7Main
  Copyright:        Copyright (C) SIL International.
  Documentation:
  Description:
  Create Date:      20 Jun 2006

  Modified Date:    25 Oct 2016
  Authors:          mcdurdin
  Related Files:
  Dependencies:

  Bugs:
  Todo:
  Notes:
  History:          20 Jun 2006 - mcdurdin - Initial version
                    01 Aug 2006 - mcdurdin - Use TKeymanTrayIcon, rework for Keyman 7
                    14 Sep 2006 - mcdurdin - Remove debug controls
                    14 Sep 2006 - mcdurdin - Rework menu items as public for calling from visual keyboard
                    14 Sep 2006 - mcdurdin - Support icons for keyboards as well as bitmaps
                    14 Sep 2006 - mcdurdin - Refactor menu rendering and construction
                    04 Dec 2006 - mcdurdin - Load icons appropriately
                    04 Dec 2006 - mcdurdin - Support Vista
                    04 Dec 2006 - mcdurdin - Lots of UI tweak and polish
                    04 Dec 2006 - mcdurdin - Support KMC_GETLOADED for wm_keyman_control
                    04 Jan 2007 - mcdurdin - Add vista alt fix
                    04 Jan 2007 - mcdurdin - ShutdownKeyman32Engine  when closing down
                    04 Jan 2007 - mcdurdin - Add OpenKeyboardHelp function
                    04 Jan 2007 - mcdurdin - Add help support
                    15 Jan 2007 - mcdurdin - Don't show the OSK on first startup
                    22 Jan 2007 - mcdurdin - Implemented run a program with the RunProgram action
                    19 Mar 2007 - mcdurdin - I701 - running products not freed
                    16 May 2007 - mcdurdin - I819 - Test if Keyman is functioning when keyboard menu opened
                    19 Jun 2007 - mcdurdin - I815 - Support current-user font install
                    19 Jun 2007 - mcdurdin - I819 - Improve Keyman stability
                    23 Aug 2007 - mcdurdin - Refactor package welcome
                    12 Oct 2007 - mcdurdin - I984, I1077, I1078 - Handle crash when starting Keyman with Windows and the tray is unresponsive
                    12 Oct 2007 - mcdurdin - I1096 - Avoid silent exception when OPSK is not visible and Keyman exits
                    12 Oct 2007 - mcdurdin - I1019, I1020, I1028, I1047, I1086 - Avoid crash when installing user fonts and starting Keyman
                    12 Oct 2007 - mcdurdin - I958 - Handle OSK disappearing when it should be appearing
                    19 Nov 2007 - mcdurdin - I1148 - Fix crash starting Windows (show error code)
                    27 Mar 2008 - mcdurdin - I1248 - Integrated Welcome process
                    27 Mar 2008 - mcdurdin - I1288 - OSK auto-popup
                    27 Mar 2008 - mcdurdin - I1375 - Switch to OSK page automatically when keyboard selected
                    27 Mar 2008 - mcdurdin - I1376 - Sometimes OSK shows wrong keyboard when it starts
                    27 Mar 2008 - mcdurdin - I1374 - Support font helper
                    27 Mar 2008 - mcdurdin - I1377 - Show menu near tray icon when using hotkey
                    27 Mar 2008 - mcdurdin - I1287 - Support switching languages with keyboards
                    27 Mar 2008 - mcdurdin - I1292 - InstallUserFonts using ansi filepath
                    27 Mar 2008 - mcdurdin - I1123, I1136 - Fix InstallUserFonts crashes
                    27 Mar 2008 - mcdurdin - I1236 - Integrate keyboard help with Keyman help
                    27 Mar 2008 - mcdurdin - I1289 - Focus not returned to active app when OSK opened
                    27 Mar 2008 - mcdurdin - I1274 - Avoid crash if OSK already closed when closing OSK
                    14 Jun 2008 - mcdurdin - Improve performance of OSK
                    16 Jun 2008 - mcdurdin - I1082 - Avoid nasty menu flicker
                    20 Jul 2008 - mcdurdin - I1412 - Performance when starting On Screen Keyboard in tutorial
                    28 Jul 2008 - mcdurdin - I1510 - context help
                    10 Sep 2008 - mcdurdin - I1627 - Switching languages with keyboard should be in keyman32.dll
                    19 Sep 2008 - mcdurdin - I1649 - Tray icon robustness
                    29 Sep 2008 - mcdurdin - I1649 - Additional tray icon robustness - ask message after 120 seconds only
                    16 Jan 2009 - mcdurdin - I1649 - Additional trayicon robust work
                    16 Jan 2009 - mcdurdin - I1486 - Restart Keyman Engine if it was previously locked and didn't start correctly
                    16 Jan 2009 - mcdurdin - I1656 - Fix crash when OSK does not exist (e.g. fails to create)
                    27 Jan 2009 - mcdurdin - I1826 - Improve performance of fontchange
                    25 May 2009 - mcdurdin - I1951 - Fix crash when starting 2 instances of Keyman Engine at same time
                    11 Dec 2009 - mcdurdin - I934 - x64 support - platform comms
                    11 Dec 2009 - mcdurdin - I1455 - Keyboard switching not working correctly in multi-process/multi-thread apps
                    11 Dec 2009 - mcdurdin - I823 - Use GetGUIThreadInfo
                    12 Mar 2010 - mcdurdin - I2226 - Add text editor, charmap, keyboard usage, font helper to menu
                    29 Mar 2010 - mcdurdin - I1089 - Switch keyboard for all applications support
                    06 Apr 2010 - mcdurdin - I2271 - Select Keyboard tidy up
                    09 Apr 2010 - mcdurdin - I2287 - Menu links not opening correct OSK tool
                    19 Apr 2010 - mcdurdin - I2297 - Switching languages automatically was not quite stable
                    04 May 2010 - mcdurdin - I2349 - Debug log capture hotkey
                    04 May 2010 - mcdurdin - I2348 - Rework of debug log format
                    04 May 2010 - mcdurdin - I2350 - Support debugging from keyman.exe
                    04 May 2010 - mcdurdin - I2297 - Window and keyboard activation work
                    14 May 2010 - mcdurdin - I2226 - Improve Keyman menu
                    25 May 2010 - mcdurdin - I1694 - Select Keyman UI language rework
                    28 Jun 2010 - mcdurdin - I2444 - Keyman tries to re-init because OSK was not unregistering until after Keyman_Exit
                    22 Oct 2010 - mcdurdin - I2522 - Language switch window - control Windows' hotkeys
                    30 Nov 2010 - mcdurdin - I2543 - Support switching to TSF addins in Language Switch window
                    11 Jan 2011 - mcdurdin - I1867 - Sort keyboards alphabetically - means we now refer to keyboards by KeymanID instead of index
                    11 Jan 2011 - mcdurdin - I2287 - Open correct tool in OSK when selecting from menu
                    25 Jan 2011 - mcdurdin - I2569 - Keyboard welcome needs to run from kmshell
                    31 Jan 2011 - mcdurdin - I2692 - Ensure OSK closes before Keyman does
                    28 Feb 2011 - mcdurdin - I2398 - After reloading configuration, OSK does not show active keyboard in toolbar
                    03 May 2011 - mcdurdin - I2890 - Record diagnostic data when encountering registry errors
                    19 Aug 2011 - mcdurdin - I3010 - Hint balloon on Keyman tray icon has hardcoded message
                    22 Aug 2011 - mcdurdin - I3042 - Null dereference showing balloon hint
                    03 Oct 2011 - mcdurdin - I3092 - Keyman Engine does not restart nicely when shutdown uncleanly
                    18 May 2012 - mcdurdin - I3306 - V9.0 - Remove TntControls + Win9x support
                    02 Dec 2011 - mcdurdin - I3160 - When multiple OEM products are running, hotkeys can fail to activate keyboards
                    24 Jan 2012 - mcdurdin - I3139 - Crash at shutdown when keyman32.dll unloaded early
                    02 Feb 2012 - mcdurdin - I2975 - VistaAltFixUnit causes exception on shutdown
                    03 Nov 2012 - mcdurdin - I3515 - V9.0 - Merge of I3139 - Crash at shutdown when keyman32.dll unloaded early
                    03 Nov 2012 - mcdurdin - I3516 - V9.0 - Merge of I3160 - When multiple OEM products are running, hotkeys can fail to activate keyboards
                    17 Jan 2013 - mcdurdin - I3758 - V9.0 - keymanx64 can find incorrect window handle for keyman.exe
                    24 Oct 2013 - mcdurdin - I3933 - V9.0 - Keyman tray icon menu is not showing installed keyboards
                    07 Nov 2013 - mcdurdin - I3949 - V9.0 - Keyboard selection and notification needs consolidation
                    11 Nov 2013 - mcdurdin - I3961 - V9.0 - Clean up communication between keyman32 and keyman
                    29 Nov 2013 - mcdurdin - I3990 - V9.0 - Keyman keyboards menu should be used on OSK toolbar
                    29 Nov 2013 - mcdurdin - I3991 - V9.0 - Eliminate some now unnecessary interference checks when showing menu
                    17 Dec 2013 - mcdurdin - I4004 - V9.0 - Remove old Keyman keyboard code from LangSwitchManager
                    06 Mar 2014 - mcdurdin - I4124 - V9.0 - Language switch dialog is not working in v9
                    24 Apr 2014 - mcdurdin - I4191 - V9.0 - Lang switch window shows wrong selection with Alt+LeftShift when TIP is active
                    02 May 2014 - mcdurdin - I4225 - V9.0- Opening font helper or keyboard usage from Keyman menu on Win 8 still shows HTML outside window
                    28 May 2014 - mcdurdin - I4243 - V9.0 - Remove kmuspmgr and keymanuc
                    16 Jun 2014 - mcdurdin - I4271 - V9.0 - Switch language for all applications is not working
                    23 Jun 2014 - mcdurdin - I4285 - V9.0 - Profile change needs debug data for tracing source profile change thread
                    23 Jun 2014 - mcdurdin - I4286 - V9.0 - If an atom is deleted before profile change is received, then we must ignore it
                    03 Aug 2014 - mcdurdin - I4359 - V9.0 - OSK shows wrong base keyboard and doesn't refresh
                    03 Aug 2014 - mcdurdin - I4326 - V9.0 - Switch-off hotkey not working, then keyboard hotkey stopped working (win 8.1 jeremy) [High]
                    12 Aug 2014 - mcdurdin - I4374 - V9.0 - Refactor wow64 tests
                    28 Aug 2014 - mcdurdin - I4390 - V9.0 - Free vs Pro
                    28 Aug 2014 - mcdurdin - I4390 - V9.0 - Free vs Pro
                    13 Oct 2014 - mcdurdin - I4451 - V9.0 - Language hotkeys are not working
                    23 Oct 2014 - mcdurdin - I4458 - Crash showing keyboard menu when product missing [CrashID:keyman.exe_9.0.472.0_2C5933D2_EAccessViolation]
                    03 Feb 2015 - mcdurdin - I4576 - V9.0 - Switch for all languages not disabled on Win8 upgrades
                    06 Feb 2015 - mcdurdin - I4576 - V9.0 - Switch for all languages not disabled on Win8 upgrades
                    04 Mar 2015 - mcdurdin - I4606 - V9.0 - Support single keyboard buttons on OSK toolbar
                    22 Apr 2015 - mcdurdin - I4674 - V9.0 - Hotkeys do not always work consistently
                    02 Jun 2015 - mcdurdin - I4715 - Language profile change notification while Keyman menu is visible sometimes causes a crash [CrashID:keyman.exe_9.0.492.0_00000000_EAccessViolation]
                    02 Jun 2015 - mcdurdin - I4731 - Keyman loses focus sometimes when switching keyboards using the menu
                    02 Jun 2015 - mcdurdin - I4731 - Keyman loses focus sometimes when switching keyboards using the menu
                    12 Sep 2016 - mcdurdin - I5086 - Keyboard hotkey toggles are not working in 9.0
                    25 Oct 2016 - mcdurdin - I5132 - keyman.exe crashes on start with access violation
                    25 Oct 2016 - mcdurdin - I5133 - Keyman.exe starts and exits because Control.StartKeyman32Engiene is passed a zero ProductID
                    25 Oct 2016 - mcdurdin - I5136 - Remove additional product references from Keyman Engine
*)
unit UfrmKeyman7Main;  // I3306   // I4004   // I5136

interface

uses
  Winapi.Windows,
  Winapi.Messages,
  System.Classes,
  System.Contnrs,
  System.SysUtils,
  System.UITypes,
  Vcl.Graphics,
  Vcl.Controls,
  Vcl.Forms,
  Vcl.Dialogs,
  Vcl.ExtCtrls,
  Vcl.Menus,
  Vcl.StdCtrls,
  Vcl.ImgList,

  Keyman.System.DebugLogClient,
  Keyman.System.DebugLogManager,
  Keyman.System.FrameworkInputPane,

  keymanapi_TLB,
//TOUCH    UfrmTouchKeyboard,
  GlobalKeyboardChangeManager,
  UfrmVisualKeyboard,
  IntegerList,
  KeymanTrayIcon,
  KeymanMenuItem,
  custinterfaces,
  Menu_KeyboardItems,
  UserMessages,
  UfrmKeymanMenu,
  UfrmLanguageSwitch,
  LangSwitchManager;

const
  TKF_PING = 0;
  TKF_RESPONSE = 1;

type
  TMouseButtons = set of TMouseButton;

  TRunningProduct = class
  private
    FOwner: TComponent;
  public
    Name: WideString;
    HIcon: THandle;
    FTrayIcon: TKeymanTrayIcon;
    FLangSwitchConfiguration: TLangSwitchConfiguration;
    constructor Create(AOwner: TComponent);
    destructor Destroy; override;
  end;

  TTestKeymanFunctionType = (tkfPopupMenu, tkfKeyboard);

  TTestKeymanFunctioning = record
    FunctionType: TTestKeymanFunctionType;
    RunOnSuccess: Boolean;
    Count: Integer;
  end;

  THandleArray = array of THandle;   // I4731

  TfrmKeyman7Main = class(TForm)
    mnu: TPopupMenu;
    tmrTestKeymanFunctioning: TTimer;
    tmrOnlineUpdateCheck: TTimer;
    tmrCheckInputPane: TTimer;
    tmrRefresh: TTimer;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure tmrTestKeymanFunctioningTimer(Sender: TObject);
    procedure tmrOnlineUpdateCheckTimer(Sender: TObject);
    procedure tmrCheckInputPaneTimer(Sender: TObject);
    procedure tmrRefreshTimer(Sender: TObject);
  private
    InMenuLoop: Integer;  // I1082 - Avoid menu nasty flicker with rapid click
    FClosingApp: Boolean;
    FRunningProduct: TRunningProduct;
    sMsg_TaskbarRestart: Cardinal;
    FTrayButtonDown: TMouseButtons;
    FTestKeymanFunctioning: TTestKeymanFunctioning;
    FWelcomeWindow: THandle;
    FInUpdateOSKVisibility: Boolean;
    FOSKManuallyClosedThisSession: Boolean;

    FLangSwitchRefreshWatcher: TThread;
    FLastFocus: THandle;
    FLastActive: THandle;
    //FLastKeymanID: Integer;
    FLastHKL: Integer;
    FLangSwitchManager: TLangSwitchManager;   // I3933
    FGlobalKeyboardChangeManager: TGlobalKeyboardChangeManager;   // I4271
    FActiveHKL: Integer;
    FTrayIcon: TIcon;   // I4359

    FHotkeyWindow: HWND;
    FHotkeys: TIntegerList;

    FInputPane: TFrameworkInputPane;
    FIsInputPaneVisible: Boolean;

    //TOUCH      FCurrentContext: string;

    function AddTaskbarIcon: Boolean;

    function IsProductLoaded: Boolean;

    function LoadProduct: Boolean;
    procedure UnloadProduct;

    procedure ShowVisualKeyboard(Page: TOSKActivePage = apUndefined);
    procedure HideVisualKeyboard;

    procedure TrayIconUnresponsive(Sender: TObject; RetryCount: Integer; var ShouldCancel: Boolean);
    procedure TrayIconMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure ShowBalloon(Value: Integer);

    procedure WMUserStart(var Message: TMessage); message WM_USER_Start;
    procedure WMUserParameterPass(var Message: TMessage); message WM_USER_ParameterPass;
    procedure WMUserSendFontChange(var Message: TMessage); message WM_USER_SendFontChange;
    procedure WMUserVisualKeyboardClosed(var Message: TMessage); message WM_USER_VisualKeyboardClosed;   // I4243
    procedure SetTrayIcon(rp: TRunningProduct; kbd: IKeymanKeyboardInstalled);
    procedure TrayIconMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure TrayIconDblClick(Sender: TObject);
    //function CreateKeymanMenuItem(Owner: TPopupMenu; rp: TRunningProduct; cmi: IKeymanCustomisationMenuItem): TKeymanMenuItem;
    function AppMessage(var Message: TMessage): Boolean;
    procedure TestKeymanFunctioning(FunctionType: TTestKeymanFunctionType; RunOnSuccess, ResetCounter: Boolean);
    function ShouldTestKeymanFunctioning: Boolean;
    procedure TrayIconDebugMessage(Sender: TObject; msg: Integer);
    procedure TrayIconBalloonClick(Sender: TObject);

    procedure UpdateOSKVisibility;
    function GetOption_AutoOpenOSK: Boolean;
    function GetOption_AutoSwitchOSKPages: Boolean;
    //procedure SnapToolHelp;
    procedure RecreateTaskbarIcons;
    function StartKeymanEngine: Boolean;
    procedure StartKeymanX64;
    procedure OpenTextEditor;
    //function GetCachedKeymanID(hkl: DWORD): DWORD;
    procedure ShowLanguageSwitchForm;
    procedure LanguageSwitchFormHidden(Sender: TObject);
    procedure RegisterControllerWindows;  // I3092
    procedure TrayIconMouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer);
    procedure ProcessProfileChange(CommandAndAtom: DWORD);   // I3933   // I3949
    function GetActiveKeymanID: Integer;   // I3949
    procedure RequestCurrentActiveKeyboard(Command: WORD);   // I3961
    function ProcessWMKeymanControl(Command, WParam: Word; LParam: DWord): LResult;   // I3961
//TOUCH    procedure HideTouchKeyboard;
//TOUCH    procedure ShowTouchKeyboard;
//TOUCH    function UseTouchKeyboard: Boolean;
//TOUCH    function TouchKeyboardVisible: Boolean;
//TOUCH    procedure ProcessContextChange(Atom: DWORD);

    procedure PostGlobalKeyboardChange(FActiveKeyboard: TLangSwitchKeyboard);   // I4271
    function IsControllerWindow(AHandle: THandle): Boolean;   // I4731
    procedure UpdateFocusInfo;   // I4731
    procedure UnregisterControllerWindows;   // I4731
    function IsSysTrayWindow(AHandle: THandle): Boolean;
    procedure GetTrayIconHandle;   // I4731

    procedure RegisterHotkeys;
    procedure UnregisterHotkeys;
    procedure HotkeyWndProc(var Message: TMessage);

    procedure DoLanguageHotkey(Index: Integer);
  protected
    procedure DoInterfaceHotkey(Target: Integer);

  protected
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    procedure WndProc(var Message: TMessage); override;
    procedure CreateWnd; override;
  public
    frmKeymanMenu: TfrmKeymanMenu;
    frmVisualKeyboard: TfrmVisualKeyboard;
    frmLanguageSwitch: TfrmLanguageSwitch;

    procedure SetLastFocus;
    procedure ShowMenu(Sender: TObject; Location: TCustomisationMenuItemLocation; TriggeredByKeyboard: Boolean);   // I3961   // I3990
    procedure ActivateKeyboard(Keyboard: TLangSwitchKeyboard);   // I4326

    function ActiveKeyboard: IKeymanKeyboardInstalled;

    { ICustMenuActions_Main }
    function VisualKeyboardVisible(Page: TOSKActivePage = apUndefined): Boolean;
    procedure MnuKeyboardClick(Sender: TObject);
    procedure MnuExitKeyman(Sender: TObject);
    procedure MnuOpenKeymanConfiguration(Sender: TObject);
    procedure MnuOpenProductHelp(Sender: TObject);
    procedure MnuOpenKeyboardHelp(Sender: TObject);
    procedure MnuOpenProductAbout(Sender: TObject);

    procedure MnuVisualKeyboard(Sender: TObject);
    procedure MnuCharacterMap(Sender: TObject);
    procedure MnuFontHelper(Sender: TObject);

    procedure MnuRunProgram(Sender: TObject);
    procedure MnuSelectKeyboard(Sender: TObject);   // I4606
    procedure MnuError(Sender: TObject);

    procedure MnuOpenTextEditor(Sender: TObject);

    function GetLangSwitchKeyboard(const name: string; fallback: Boolean): TLangSwitchKeyboard;   // I4606
    procedure SwitchToFirstWindowsKeyboard;  // I1867

    property RunningProduct: TRunningProduct read FRunningProduct;
    property ActiveKeymanID: Integer read GetActiveKeymanID;   // I3949
    property ActiveHKL: Integer read FActiveHKL;   // I4359

    property LangSwitchManager: TLangSwitchManager read FLangSwitchManager;   // I3933

    property Option_AutoOpenOSK: Boolean read GetOption_AutoOpenOSK;
    property Option_AutoSwitchOSKPages: Boolean read GetOption_AutoSwitchOSKPages;
  end;

var
  frmKeyman7Main: TfrmKeyman7Main;
  hProgramMutex: THandle;

  wm_keyman_globalswitch, wm_keyman_globalswitch_process, wm_keyman_control, wm_keyman_control_internal, wm_test_keyman_functioning: Cardinal;

  FEnableCrashTest: Boolean = False;

const
  KMC_StartProduct = 0;
  KMC_StopProduct = 1;
  KMC_ShowVisualKeyboard = 2;
  KMC_HideVisualKeyboard = 3;

  KMCI_SELECTKEYBOARD = 3;   // I3933
  KMCI_SELECTKEYBOARD_TSF = 4;   // I3933
  KMCI_GETACTIVEKEYBOARD = 5;   // I3933
  KMCI_SETFOREGROUND = 6;   // I3933
  KMCI_SELECTKEYBOARD_BACKGROUND = 7;   // I4271
  KMCI_SELECTKEYBOARD_BACKGROUND_TSF = 8;   // I4271

const
  MSGFLT_ADD = 1;

const
  KEYMANID_NONKEYMAN = -1;
  KEYMANID_INVALID: Cardinal = $FFFFFFFD;

const
  skHKL = 0;       // A windows language has been selected so select the most appropriate Keyman keyboard
  skKeymanID = 1;  // Select the requested Keyman keyboard
  skSelectHKL = 2; // Select the requested Windows language (and therefore the most appropriate Keyman keyboard)
  skSelectHKLOnly = 3; // Select the requested Windows language and turn Keyman keyboard off
  skTSF = 4;       // A TSF TIP

implementation

uses
  DebugPaths,
  UILanguages,
  UfrmOSKCharacterMap,
  utilstr,
  utilwow64,
  Keyman.System.KeymanSentryClient,
  KeymanEngineControl,
  KeymanControlMessages,
  KeymanDesktopShell,
  KeymanPaths,
  KeymanVersion,
  kmint,
  Winapi.ShellApi,
  BitmapIPicture,
  Winapi.ActiveX,
  ErrorControlledRegistry,
  RegistryKeys,
  klog,
  GetOsVersion,
  System.StrUtils,
  System.Win.ComObj, {tlhelp32,}
  VistaMessages,
  Vcl.AxCtrls,
  Vcl.Buttons,
  Vcl.ComCtrls,
  InterfaceHotkeys,
  utilhotkey,
  MessageIdentifiers,
  MessageIdentifierConsts,
  utilsystem,
  System.Types,
  KeymanHints,
  HintConsts;

{$R *.DFM}

function TfrmKeyman7Main.AddTaskbarIcon: Boolean;
var
  rp: TRunningProduct;
  cust: IKeymanCustomisation;
  //icofilename: WideString;
  istrm: IStream;
  olestrm: TOLEStream;
begin
  Result := True;

  RegisterClasses([TImage, TButton, TBitBtn, TTabSheet, TPageControl, TBevel, TShape, TLabel]);

  if not Assigned(FRunningProduct) then
  begin
    Result := False;
    Exit;
  end;

  rp := FRunningProduct;
  cust := kmint.KeymanCustomisation;

  istrm := cust.CustFile['appicon.ico'];
  if istrm <> nil then
  begin
    olestrm := TOLEStream.Create(istrm);
    try
      // In some situations, launching the app multiple times rapidly can
      // cause the icon to be loaded multiple times. Make sure we reset the
      // stream position before we try and read.
      olestrm.Position := 0;
      Application.Icon.LoadFromStream(olestrm);
    finally
      olestrm.Free;
      istrm := nil;
    end;
  end;

  if rp.FTrayIcon.Visible then Exit;

  GetTrayIconHandle;

  rp.FTrayIcon.OnMessage := TrayIconDebugMessage;
  rp.FTrayIcon.OnBalloonClick := TrayIconBalloonClick;  // I1248 - Integrate with new welcome
  rp.FTrayIcon.OnMouseMove := TrayIconMouseMove;
  rp.FTrayIcon.OnMouseDown := TrayIconMouseDown;
  rp.FTrayIcon.OnMouseUp := TrayIconMouseUp;
  rp.FTrayIcon.OnDblClick := TrayIconDblClick;
  rp.FTrayIcon.OnUnresponsive := TrayIconUnresponsive;
  //rp.FTrayIcon.OnClick := TrayIconClick;
//  rp.FTrayIcon.Icon.ReleaseHandle;
  rp.FTrayIcon.Icon.Assign(FTrayIcon);
  rp.FTrayIcon.Hint := MsgFromId(SKApplicationTitle);
  rp.FTrayIcon.Visible := True;
end;

procedure TfrmKeyman7Main.GetTrayIconHandle;
var
  istrm: IStream;
  olestrm: TOLEStream;
begin
  if Assigned(FTrayIcon) then
    Exit;
  FTrayIcon := TIcon.Create;

  istrm := kmint.KeymanCustomisation.CustFile['trayicon.ico'];
  if istrm <> nil then
  begin
    olestrm := TOLEStream.Create(istrm);
    try
      olestrm.Seek(0, soFromBeginning);
      FTrayIcon.LoadFromStream(olestrm);
    finally
      olestrm.Free;
    end;
  end;
end;

procedure TfrmKeyman7Main.FormCreate(Sender: TObject);
begin
  TKeymanSentryClient.Breadcrumb('trace', 'TfrmKeyman7Main.FormCreate');

  if GetOs in [osVista, osWin7] then   // I4576
    FGlobalKeyboardChangeManager := TGlobalKeyboardChangeManager.Create;   // I4271

  frmKeymanMenu := TfrmKeymanMenu.Create(Self);

  FLangSwitchManager := TLangSwitchManager.Create;   // I3933

  //FLastKeymanID := -1;
  FLastHKL := GetKeyboardLayout(0);

  try
    GetDebugManager(Handle);
  except
    on E:Exception do
      KL.LogError('GetDebugManager failed: '+E.Message);
  end;

  Application.HookMainWindow(AppMessage);
//  FRunningProduct := TRunningProduct.Create;

  sMsg_TaskbarRestart := RegisterWindowMessage('TaskbarCreated');
  ChangeWindowMessageFilter(sMsg_TaskbarRestart, MSGFLT_ADD);

  FInputPane := TFrameworkInputPane.Create;

  PostMessage(Handle, WM_USER_Start, 0, 0);
end;

{procedure TfrmKeyman7Main.AppDebug(var Msg: TMsg; var Handled: Boolean);
begin
  KL.Log('MESSAGE %8.8x %x [%x, %x]', [Msg.hwnd, Msg.message, Msg.wParam, Msg.lParam]);
end;}

procedure TfrmKeyman7Main.FormDestroy(Sender: TObject);
begin
  TKeymanSentryClient.Breadcrumb('trace', 'TfrmKeyman7Main.FormDestroy');
  if Assigned(FLangSwitchRefreshWatcher) then
  begin
    FLangSwitchRefreshWatcher.Terminate;
    FreeAndNil(FLangSwitchRefreshWatcher);
  end;

  FreeAndNil(FInputPane);

  UnregisterHotkeys;

  with TRegistryErrorControlled.Create do  // I2890
  try
    if OpenKey(SRegKey_KeymanOSK_CU, True) then
      WriteBool(SRegValue_OSK_ShowVisualKeyboard, VisualKeyboardVisible);
  finally
    Free;
  end;
  if Assigned(frmVisualKeyboard) then // I1096 - silent exception when Keyman closes
  begin
    //frmVisualKeyboard.Hide;
    //frmVisualKeyboard.Release;
    FreeAndNil(frmVisualKeyboard);  // I2692
  end;

  Application.UnhookMainWindow(AppMessage);

  UnregisterControllerWindows;   // I4731

  if Assigned(FRunningProduct) then
    UnloadProduct;

  FreeAndNil(frmKeymanMenu);  // I3139 - attempt to avoid crash because keyman32.dll is unloaded and then additional messages sent?   // I3515
  FreeAndNil(frmLanguageSwitch);  // I3139 - attempt to avoid crash because keyman32.dll is unloaded and then additional messages sent?   // I3515

  if kmint.KeymanEngineControl <> nil then
    kmint.KeymanEngineControl.ShutdownKeyman32Engine;
  FreeAndNil(FLangSwitchManager);   // I3933

  //Windows.MessageBox(Handle, PChar(IntToStr(kmcom._AddRef)), 'RefCount+1', MB_OK);
  kmint.kmcom := nil;   // I5132

  FreeAndNil(FGlobalKeyboardChangeManager);   // I4271
end;

function TfrmKeyman7Main.GetOption_AutoOpenOSK: Boolean;
begin
  Result := kmcom.Options.Items['koAutoOpenOSK'].Value;
end;

function TfrmKeyman7Main.GetOption_AutoSwitchOSKPages: Boolean;
begin
  Result := kmcom.Options.Items['koAutoSwitchOSKPages'].Value;
end;

function TfrmKeyman7Main.IsControllerWindow(AHandle: THandle): Boolean;   // I4731
begin
  Result := GetWindowThreadProcessId(AHandle, nil) = GetCurrentThreadId;
end;

function TfrmKeyman7Main.IsSysTrayWindow(AHandle: THandle): Boolean;   // I4731
var
  buf: array[0..64] of char;
begin
	GetClassName(AHandle, buf, 63);
	Result := SameText(buf, 'SysTabControl32') or
		SameText(buf, 'TrayNotifyWnd') or
		SameText(buf, 'Shell_TrayWnd') or
    SameText(buf, 'NotifyIconOverflowWindow');
end;

procedure TfrmKeyman7Main.RegisterControllerWindows;  // I3092
begin
  if kmint.KeymanEngineControl = nil then
  begin
    TKeymanSentryClient.ReportMessage('RegisterControllerWindows: KeymanEngineControl was unexpectedly nil');
    Exit;
  end;
  kmint.KeymanEngineControl.RegisterMasterController(Application.Handle);
  kmint.KeymanEngineControl.RegisterControllerThread(GetCurrentThreadId);
end;

procedure TfrmKeyman7Main.UnregisterControllerWindows;   // I4731
begin
  if kmint.KeymanEngineControl = nil then
  begin
    TKeymanSentryClient.ReportMessage('UnregisterControllerWindows: KeymanEngineControl was unexpectedly nil');
    Exit;
  end;
  kmint.KeymanEngineControl.UnregisterMasterController(Application.Handle);
  kmint.KeymanEngineControl.UnregisterControllerThread(GetCurrentThreadId);
end;

function TfrmKeyman7Main.LoadProduct: Boolean;
begin
  Result := False;

  if not Assigned(FRunningProduct) then
  begin
    if not Assigned(kmcom) and not StartKeymanEngine then Exit;  // I1951

    if kmint.KeymanEngineControl = nil then
    begin
      TKeymanSentryClient.ReportMessage('LoadProduct: KeymanEngineControl was unexpectedly nil');
      Exit;
    end;

    kmint.KeymanEngineControl.ResetKeyman32Engine; // pre-initialise  // I3092   // I5133
    RegisterControllerWindows;  // I3092
    kmint.KeymanEngineControl.StartKeyman32Engine;   // I5133

    FRunningProduct := TRunningProduct.Create(Self);
    FRunningProduct.Name := MsgFromId(SKApplicationTitle);
    AddTaskbarIcon;
    CreateUILanguages;
  end;

  Result := True;
end;

procedure TfrmKeyman7Main.UnloadProduct;
begin
  if Assigned(FRunningProduct) then
  begin
    if not Assigned(kmcom) and not StartKeymanEngine then Exit;  // I1951

    if kmint.KeymanEngineControl = nil then
    begin
      TKeymanSentryClient.ReportMessage('UnloadProduct: KeymanEngineControl was unexpectedly nil');
    end
    else
    begin
      kmint.KeymanEngineControl.StopKeyman32Engine;   // I5133
    end;

    FreeAndNil(FRunningProduct);

    FClosingApp := True;
    ReleaseMutex(hProgramMutex); // We are not controlling Keyman any more - another copy of Keyman.exe may start
    Close;
  end;
end;

{ TRunningProduct }

constructor TRunningProduct.Create(AOwner: TComponent);
begin
  inherited Create;
  FOwner := AOwner;
  FLangSwitchConfiguration := TLangSwitchConfiguration.Create;
  FTrayIcon := TKeymanTrayIcon.Create(FOwner);
end;

destructor TRunningProduct.Destroy;
begin
  FreeAndNil(FTrayIcon);
  FreeAndNil(FLangSwitchConfiguration);
  inherited;
end;

function TfrmKeyman7Main.AppMessage(var Message: TMessage): Boolean;
begin
  if Message.Msg = WM_ACTIVATEAPP then
  begin
    { Stop Delphi from de-topmosting Visual Keyboard!}
    with Message do
      Result := DefWindowProc(Handle, Msg, WParam, LParam);
    Result := True;
  end
  else if Message.Msg = sMsg_TaskbarRestart then
  begin
    // I1649 - Increase robustness of icon creation
    RecreateTaskbarIcons;
    Result := False;
  end
  else if Message.Msg = WM_FONTCHANGE then  // 1374 - Support font helper
  begin
    if Assigned(frmVisualKeyboard) then
      frmVisualKeyboard.Dispatch(Message);
    Result := False;
  end
  else if Message.Msg = wm_keyman_globalswitch then
  begin
    TDebugLogClient.Instance.WriteMessage('wm_keyman_globalswitch for Application Handle: %x %x', [Message.wParam, Message.lParam]);

    case Message.wParam of
      skHKL,      // A windows language has been selected so select the most appropriate Keyman keyboard
      skSelectHKL: // Select the requested Windows language (and therefore the most appropriate Keyman keyboard)
        FLastHKL := Message.lParam;
    end;
    Result := True;
  end
  else if Message.Msg = wm_keyman_control then
  begin
    Result := True;
    if FClosingApp then
    begin
      Message.Result := 0;
      Exit;
    end;

    TDebugLogClient.Instance.WriteMessage('wm_keyman_control for Application Handle: %x %x', [Message.wParam, Message.lParam]);
    Message.Result := ProcessWMKeymanControl(LoWord(Message.WParam), HiWord(Message.WParam), Message.LParam);   // I3961
    TDebugLogClient.Instance.WriteMessage('wm_keyman_control-exit for Application Handle: %x %x -> %x', [Message.wParam, Message.lParam, Message.Result]);   // I3961
  end
  else
    Result := False;
end;

procedure TfrmKeyman7Main.UpdateFocusInfo;   // I4731
var
  gti: TGUIThreadInfo;
begin
  gti.cbSize := SizeOf(gti);
  if not GetGUIThreadInfo(0, gti) then
  begin
    TDebugLogClient.Instance.WriteLastError('UpdateFocusInfo', 'GetGUIThreadInfo');
    Exit;
  end;

  TDebugLogClient.Instance.WriteMessage('UpdateFocusInfo: last focus=%x last active=%x', [FLastFocus, FLastActive]);
  TDebugLogClient.Instance.WriteMessage('UpdateFocusInfo: new focus=%x new active=%x', [gti.hwndFocus, gti.hwndActive]);

  if IsControllerWindow(gti.hwndFocus) then  // I4731
    TDebugLogClient.Instance.WriteMessage('UpdateFocusInfo: new focus is controller window, not updating', [])
  else if IsSysTrayWindow(gti.hwndFocus) then   // I4731
    TDebugLogClient.Instance.WriteMessage('UpdateFocusInfo: new focus is systray window, not updating', [])
  else if IsControllerWindow(gti.hwndActive) then  // I4731
    TDebugLogClient.Instance.WriteMessage('UpdateFocusInfo: new active is controller window, not updating', [])
  else if IsSysTrayWindow(gti.hwndActive) then   // I4731
    TDebugLogClient.Instance.WriteMessage('UpdateFocusInfo: new active is systray window, not updating', [])
  else
  begin
    FLastFocus := gti.hwndFocus;
    FLastActive := gti.hwndActive;
  end;
end;

function TfrmKeyman7Main.ProcessWMKeymanControl(Command, WParam: Word; LParam: DWord): LResult;   // I3961
begin
  Result := 0;

  case Command of
    KMC_GETLASTHKL:
      begin
        Result := FLastHKL;
      end;

    KMC_SETFOCUSINFO:
      begin
        UpdateFocusInfo;   // I4731
        RequestCurrentActiveKeyboard(PC_UPDATE);   // I3961
      end;

    KMC_INTERFACEHOTKEY:
      begin
        DoInterfaceHotkey(wParam);
      end;
    KMC_ONSCREENKEYBOARD:
      begin
        if LParam <> 0
          then ShowVisualKeyboard
          else HideVisualKeyboard;
      end;
    KMC_GETLOADED:
      begin
        if IsProductLoaded then Result := 1 else Result := 0;
      end;
    KMC_REFRESH:
      begin
        if wParam = 1 then
        begin
          // We're going to keep checking for changes for
          // the next few seconds because Windows is in the
          // process of updating its keyboard list
          tmrRefresh.Tag := 0;
          tmrRefresh.Enabled := False;
          tmrRefresh.Enabled := True;
        end;

        if VisualKeyboardVisible then
          frmVisualKeyboard.PreRefreshKeyman;
        kmcom.Refresh;
        FLangSwitchManager.Refresh;

        if VisualKeyboardVisible then
        begin
          frmVisualKeyboard.RefreshKeyman;
          frmVisualKeyboard.RefreshSelectedKeyboard;  // I2398
        end;
        FOSKManuallyClosedThisSession := False;
        FRunningProduct.FLangSwitchConfiguration.Refresh;
        RegisterHotkeys;
      end;
    KMC_NOTIFYWELCOME:    // I1248 - Redesigned welcome
      begin
        case wParam of
          NW_NOTIFYHANDLE: FWelcomeWindow := LParam;
          NW_SENDBALLOON: ShowBalloon(LParam);
        end;
      end;
    KMC_GETLASTFOCUS:
      begin
        Result := FLastFocus;
      end;
    KMC_GETLASTACTIVE:
      begin
        Result := FLastActive;
      end;
    KMC_PROFILECHANGED:   // I3933
      begin
        // Record the active profile
        ProcessProfileChange(LParam);
      end;
    KMC_HINTRESPONSE:
      begin
        ResetHintState;
        if (TKeymanHint(lParam) = KH_EXITPRODUCT) and (wParam = mrOk) then
          UnloadProduct;
      end;
//TOUCH    KMC_CONTEXT:
//TOUCH      begin
//TOUCH        if LParam <> 0 then
//TOUCH          ProcessContextChange(LParam);
//TOUCH      end;
  end;
end;

procedure TfrmKeyman7Main.DoLanguageHotkey(Index: Integer);
var
  FKeyboard: TLangSwitchKeyboard;
begin
  if (Index >= 0) and (Index < kmcom.Languages.Count) then
  begin
    FKeyboard := FLangSwitchManager.FindKeyboard(kmcom.Languages[Index].HKL, kmcom.Languages[Index].ProfileGUID);
    if not Assigned(FKeyboard) then Exit;

    // Handle toggle hotkey
    if (FKeyboard = FLangSwitchManager.ActiveKeyboard) and (kmcom.Options['koKeyboardHotkeysAreToggle'].Value) then
        FKeyboard := FLangSwitchManager.Languages[0].Keyboards[0];

    ActivateKeyboard(FKeyboard);
  end;
end;

procedure TfrmKeyman7Main.DoInterfaceHotkey(Target: Integer);
begin
  if not Assigned(FRunningProduct) then
    Exit;

  case Target of
    khKeymanOff: SwitchToFirstWindowsKeyboard;  // I1867
    khKeyboardMenu: ShowMenu(FRunningProduct.FTrayIcon, milLeft, True); // I1377 - Show the menu down near the tray when hotkey is pressed   // I3990
    khVisualKeyboard: MnuVisualKeyboard(nil);
    khKeymanConfiguration: TKeymanDesktopShell.RunKeymanConfiguration('-c 0');
    khFontHelper: MnuFontHelper(nil);
    khCharacterMap: MnuCharacterMap(nil);
    khTextEditor: MnuOpenTextEditor(nil);
    khLanguageSwitch:
      RequestCurrentActiveKeyboard(PC_UPDATE_LANGUAGESWITCH);   // I4124

  end;
end;

procedure TfrmKeyman7Main.ShowBalloon(Value: Integer);
var
  FMessage: WideString;
begin
  if not Assigned(FRunningProduct) then Exit;

  case Value of
    NWB_IDENTIFYICON: FMessage := MsgFromId(SKBalloonClickToSelectKeyboard);  // I3010  // I3042
    NWB_TUTORIALFINISHED: FMessage := MsgFromId(SKBalloonOSKClosed);   // I3010  // I3042
    NWB_KEYMANRUNNING:
      begin
        FMessage := MsgFromId(SKBalloonKeymanIsRunning);
        if FMessage = '' then
          FMessage := MsgFromId(SKBalloonClickToSelectKeyboard);
      end;
    else Exit;
  end;

  if FMessage <> '' then
  begin
    FRunningProduct.FTrayIcon.BalloonTitle := FRunningProduct.Name; // I3010
    FRunningProduct.FTrayIcon.BalloonFlags := bfInfo;
    FRunningProduct.FTrayIcon.BalloonTimeout := 30000;
    FRunningProduct.FTrayIcon.BalloonHint := FMessage;  // I3042
    FRunningProduct.FTrayIcon.ShowBalloonHint;
  end;
end;

procedure TfrmKeyman7Main.ShowLanguageSwitchForm;
var
  Keyboard: TLangSwitchKeyboard;
begin
  FLangSwitchManager.Refresh;   // ?? is this really needed. It seems in some situations that the refresh
                                // occurs too soon after keyboard installation and background processing
                                // has not completed, so this makes sure it's all okay. Long term, if we
                                // keep this pattern, we should not have a global FLangSwitchManager but
                                // instead always build an instance of the lang switch manager when the
                                // form is shown. Performance may be a concern.

  if not Assigned(frmLanguageSwitch) then
  begin
    frmLanguageSwitch := TfrmLanguageSwitch.Create(Self);
    frmLanguageSwitch.OnHidden := LanguageSwitchFormHidden;
  end;

  Keyboard := FLangSwitchManager.ActiveKeyboard;   // I3949
  if Assigned(Keyboard) then   // I3949
  begin
    ///GetDebugManager(Handle).WriteMessage('TfrmKeyman7Main.ShowLanguageSwitchForm: Keyboard '+Keyboard.Caption+' is active before lang switch window shows', []);
    frmLanguageSwitch.SelectItem(Keyboard);   // I4191
  end;
  ///else
    ///GetDebugManager(Handle).WriteMessage('TfrmKeyman7Main.ShowLanguageSwitchForm: No keyboard is active before lang switch window shows', []);

  frmLanguageSwitch.Show;
  SetWindowPos(frmLanguageSwitch.Handle, HWND_TOPMOST, 0, 0, 0, 0, SWP_NOSIZE or SWP_NOMOVE or SWP_NOREDRAW or SWP_NOACTIVATE);
end;

procedure TfrmKeyman7Main.LanguageSwitchFormHidden(Sender: TObject);
var
  hwnd: THandle;
  kbd: TLangSwitchKeyboard;
begin
  hwnd := FLastFocus;

  kbd := frmLanguageSwitch.GetSelection;
  if Assigned(kbd) then
  begin
    case kbd.ItemType of
      lsitWinKeyboard:    TDebugLogClient.Instance.WriteMessage('LanguageSwitchFormHidden: kbd assigned, type = lsitWinKeyboard, value = %x', [(kbd as TLangSwitchKeyboard_WinKeyboard).HKL]);
      lsitTIP:            TDebugLogClient.Instance.WriteMessage('LanguageSwitchFormHidden: kbd assigned, type = lsitTIP, value = %s', [(kbd as TLangSwitchKeyboard_TIP).Caption]);
      else                TDebugLogClient.Instance.WriteMessage('LanguageSwitchFormHidden: kbd assigned, type = somthing else', []);
    end;
    kbd.Activate(hwnd);
  end
  else
    TDebugLogClient.Instance.WriteMessage('LanguageSwitchFormHidden: kbd NOT assigned',[]);
end;

procedure TfrmKeyman7Main.ShowMenu(Sender: TObject; Location: TCustomisationMenuItemLocation; TriggeredByKeyboard: Boolean);   // I3990   // I3991
var
  hwndTray, hwndSysTray: THandle;

  function FindSysTrayLocation: TAlign;
  var
    r: TRect;
    m: TMonitor;
  begin
    // Note: we don't use SHAppBarMessage because it is re-entrant
    // It also does not honour the DPI awareness of the caller

    // If we can't find the tray window, assume it is at the bottom of the
    // primary screen

    if (hwndTray = 0) or not GetWindowRect(hwndTray, r) then
      Exit(alBottom);

    m := Screen.MonitorFromPoint(r.CenterPoint, mdNearest);
    if not Assigned(m) then
      // Could not find monitor, perhaps a race condition, see I2693, I2820
      // This may never happen as it may have been a VCL bug, but I'm including
      // it just for safety given it can return nil.
      Exit(alBottom);

    if r.Left = m.Left then
    begin
      if r.Top = m.Top then
      begin
        if r.Right = m.BoundsRect.Right then
          Exit(alTop);
        Exit(alLeft);
      end;
      Exit(alBottom)
    end;
    Exit(alRight);
  end;

  function GetMenuAnchorPoint(systraylocation: TAlign): TPoint;
  type
    TGetDpiForWindow = function(hwnd: HWND): UINT; stdcall;
  var
    pt: TPoint;
    r: TRect;
    ident: NOTIFYICONIDENTIFIER;
    dpi: Cardinal;
    m: TMonitor;
    FGetDpiForWindow: TGetDpiForWindow;
  begin
    // We'll try and get the actual location of our icon
    ident.cbSize := SizeOf(ident);
    ident.hWnd := FRunningProduct.FTrayIcon.NotificationWindow;
    ident.uID := 1;
    ident.guidItem := GUID_NULL;
    if TriggeredByKeyboard or (Shell_NotifyIconGetRect(ident, r) <> S_OK) then
    begin
      // User has requested the menu with a hotkey, so show it near the
      // SystemNotificationArea, but not actually where our icon is, because
      // the icon may be hidden (or if we fail to get the location of our icon,
      // for any reason)
      if not GetWindowRect(hwndSysTray, r) then
        r := TRect.Empty;
    end
    else
    begin
      // Shell_NotifyIconGetRect does not honour the DPI awareness of the caller
      // and as we do not yet support per-monitor DPI awareness, we have to
      // scale the rectangle by its actual DPI to our fixed 96 DPI value to
      // get the location of the rect in our 96 DPI world. As a monitor may have
      // a non-zero origin, we have to deal with that too!

      // Don't use Delphi's GetDpiForWindow declaration as we have to verify
      // Windows version first, which cascades into potential updates to the
      // manifest, which we don't want to do just now.
      FGetDpiForWindow := GetProcAddress(GetModuleHandle(user32), 'GetDpiForWindow');
      if Assigned(FGetDpiForWindow) then
      begin
        dpi := FGetDpiForWindow(hwndTray);
        m := Screen.MonitorFromWindow(hwndTray);
        if Assigned(m) then
        begin
          r.Right := MulDiv(r.Right-m.Left, 96, dpi) + m.Left;
          r.Bottom := MulDiv(r.Bottom-m.Top, 96, dpi) + m.Top;
          r.Left := MulDiv(r.Left-m.Left, 96, dpi) + m.Left;
          r.Top := MulDiv(r.Top-m.Top, 96, dpi) + m.Top;
        end
        else if not GetWindowRect(hwndSysTray, r) then
          r := TRect.Empty;
      end;
    end;

    // Align the menu according to the location of the taskbar
    case systraylocation of
      alTop:    begin pt.X := (r.Right + r.Left) div 2; pt.Y := r.Bottom; end;
      alBottom: begin pt.X := (r.Right + r.Left) div 2; pt.Y := r.Top; end;
      alLeft:   begin pt.X := r.Right; pt.Y := r.Top; end;
      alRight:  begin pt.X := r.Left; pt.Y := r.Top; end;
    end;

    Result := pt;
  end;

var
  systraylocation: TAlign;
  hwnd: THandle;
  pt: TPoint;
begin   // I3933
  if not Assigned(Sender) and not Assigned(FRunningProduct) then
    Exit;

  if InMenuLoop > 0 then Exit;  // I1082 - Avoid menu nasty flicker with rapid click

  hwndTray := FindWindow('Shell_TrayWnd', nil);
  hwndSysTray := FindWindowEx(hwndTray, 0, 'TrayNotifyWnd', nil);

  BuildCustMenu(kmcom, mnu, Location);   // I3933

  GetCursorPos(pt);

  mnu.Alignment := paCenter;

  systraylocation := FindSysTrayLocation;
  pt := GetMenuAnchorPoint(systraylocation);

  case Location of
    milLeft:  mnu.TrackButton := tbLeftButton;
    milRight: mnu.TrackButton := tbRightButton;
  end;

  hwnd := FLastFocus;

  if not IsDebuggerPresent then
  begin
    // We don't risk attaching to the debugger thread because that can cause a deadlock
    AttachThreadInput(GetCurrentThreadId, GetWindowThreadProcessId(hwnd, nil), TRUE);
    Winapi.Windows.SetForegroundWindow(frmKeymanMenu.Handle);
    AttachThreadInput(GetCurrentThreadId, GetwindowThreadProcessId(hwnd, nil), FALSE);
  end;

  frmKeymanMenu.PopupEx(mnu, pt.x, pt.y, systraylocation);   // I3990
end;

procedure TfrmKeyman7Main.PostGlobalKeyboardChange(FActiveKeyboard: TLangSwitchKeyboard);   // I4271
begin
  if Assigned(FActiveKeyboard) and
      Assigned(kmcom) and
      (GetOs in [osVista, osWin7]) and    // I4576   // I4576   // I4576
      kmcom.Options.Items['koSwitchLanguageForAllApplications'].Value and
      Assigned(FGlobalKeyboardChangeManager) then
    FGlobalKeyboardChangeManager.PostChange(FActiveKeyboard);
end;

procedure TfrmKeyman7Main.TrayIconDblClick(Sender: TObject);
begin
  SetLastFocus;
  if VisualKeyboardVisible
    then HideVisualKeyboard
    else ShowVisualKeyboard;
end;

procedure TfrmKeyman7Main.TrayIconMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
begin
  Exit;
  if FTrayButtonDown <> [] then Exit;
  UpdateFocusInfo;   // I4731
end;

procedure TfrmKeyman7Main.TrayIconMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  Include(FTrayButtonDown, Button);
end;

procedure TfrmKeyman7Main.TrayIconBalloonClick(Sender: TObject);
var
  rp: TRunningProduct;
begin
  FTrayButtonDown := [mbLeft,mbRight,mbMiddle]; // balloon click evil flag
  rp := FRunningProduct;
  if rp = nil then Exit;
  TestKeymanFunctioning(tkfPopupMenu, True, True);
end;

procedure TfrmKeyman7Main.TrayIconMouseUp(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  rp: TRunningProduct;
begin
  if Button in FTrayButtonDown then
  begin
    rp := FRunningProduct;
    if rp = nil then Exit;

    if FTrayButtonDown = [mbLeft,mbRight,mbMiddle]
      then begin FTrayButtonDown := []; end // balloon click
      else Exclude(FTrayButtonDown, Button);
    if Button = mbLeft
      then TestKeymanFunctioning(tkfPopupMenu, True, True)
      else ShowMenu(Sender, milRight, False);   // I3990
  end;
end;

procedure TfrmKeyman7Main.TrayIconUnresponsive(Sender: TObject;
  RetryCount: Integer; var ShouldCancel: Boolean);
var
  rp: TRunningProduct;
begin
  ShouldCancel := False;
  KL.Log('TrayIconUnresponsive: %d', [RetryCount]);
  if ((RetryCount mod 120) = 0) and (RetryCount > 0) then
  begin
    KL.Log('TrayIconUnresponsive: asking about cancel');
    // I1649 - display this message only after 120 seconds!
    // I1148 - display the error code for the end user
    if MessageDlg('Keyman Engine is unable to create its taskbar notification icon after 2 minutes.  The error code returned was '+
      IntToStr((Sender as TKeymanTrayIcon).LastError)+', '+SysErrorMessage((Sender as TKeymanTrayIcon).LastError)+#13#10#13#10+
      '  Do you want to continue trying?  If you click No, Keyman Engine will exit.',
      mtWarning, [mbYes, mbNo], 0, mbYes) = mrNo then
    begin
      ShouldCancel := True;

      rp := FRunningProduct;
      if rp = nil then Close;
      UnloadProduct;
    end;
  end;
end;

procedure TfrmKeyman7Main.MnuError(Sender: TObject);
begin
  ShowMessage('Invalid menu item');
end;

procedure TfrmKeyman7Main.MnuExitKeyman(Sender: TObject);
begin
  if not ShowKeymanHint(KH_EXITPRODUCT) then Exit;

  UnloadProduct;
end;

procedure TfrmKeyman7Main.SetTrayIcon(rp: TRunningProduct; kbd: IKeymanKeyboardInstalled);
var
  cust: IKeymanCustomisation;
  bb: IPicture;
  h: OLE_HANDLE;
begin
  if Assigned(rp) then
  begin
    bb := kbd.Bitmap;
    if Assigned(bb) then
    begin
      bb.get_Handle(h);
      rp.FTrayIcon.Icon.Handle := CopyIcon(h);
    end;

    if VisualKeyboardVisible then
    begin
      frmVisualKeyboard.Caption := rp.Name + ' - ' + kbd.Name;
    end;
  end
  else
  begin
    if VisualKeyboardVisible then
      if Assigned(FRunningProduct) then
        frmVisualKeyboard.Caption := FRunningProduct.Name
      else
        frmVisualKeyboard.Caption := SKeymanDesktopName;
  end;

  if not Assigned(rp) then
  begin
    FRunningProduct.FTrayIcon.Hint := FRunningProduct.Name;
    cust := kmint.KeymanCustomisation as IKeymanCustomisation;
    if not Assigned(cust) then
      raise Exception.Create('Product '+FRunningProduct.Name+' has unexpectedly disappeared!');
    FRunningProduct.FTrayIcon.Icon.Assign(FTrayIcon);
  end;
end;

procedure TfrmKeyman7Main.WMUserParameterPass(var Message: TMessage);
begin
  case Message.wParam of
    KMC_StartProduct: LoadProduct;
    KMC_StopProduct:  UnloadProduct;
    KMC_ShowVisualKeyboard: ShowVisualKeyboard;
    KMC_HideVisualKeyboard: HideVisualKeyboard;
  end;
end;

procedure TfrmKeyman7Main.WMUserSendFontChange(var Message: TMessage);
begin
  PostMessage(HWND_BROADCAST, WM_FONTCHANGE, 0, 0);
    // I1826 - post instead of send for improved performance and no blocking
end;

procedure TfrmKeyman7Main.WMUserStart(var Message: TMessage);
begin
  StartKeymanEngine; // I1951
end;

procedure TfrmKeyman7Main.WMUserVisualKeyboardClosed(var Message: TMessage);   // I4243
begin
  ShowKeymanHint(KH_CLOSEOSK);     // I1232 - Show hint on how to get OSK back when closing
                                   // This only happens if hint is already visibler
  FOSKManuallyClosedThisSession := True;
end;

function TfrmKeyman7Main.StartKeymanEngine: Boolean;  // I1951
begin
  Result := True;

  TKeymanSentryClient.Breadcrumb('trace', 'TfrmKeyman7Main.StartKeymanEngine:kmcom='+IfThen(Assigned(kmcom),'assigned','nil'));

  if Assigned(kmcom) then Exit;

  try
    kmcom := CoKeyman.Create;
  except
    on E:Exception do
    begin
      TKeymanSentryClient.ReportHandledException(E, 'StartKeymanEngine: Instantiating kmcom');
      ShowMessage(E.Message);
      Application.Terminate;
      Result := False;
      Exit;
    end;
  end;

  //Windows.MessageBox(Handle, PChar(IntToStr(kmcom._AddRef)), 'RefCount+1', MB_OK);

  try
    kmcom.AutoApply := False;
    kmcom.Keyboards.Apply;
    RegisterControllerWindows;  // I3092
  except
    on E:Exception do
    begin
      TKeymanSentryClient.ReportHandledException(E, 'StartKeymanEngine: Registering controller windows');
      Application.ShowException(E);
      Application.Terminate;
      Result := False;
      Exit;
    end;
  end;

  if not LoadProduct then
  begin
    TKeymanSentryClient.ReportMessage('StartKeymanEngine: Unable to load product');
    ShowMessage('Unable to load product.');
    Application.Terminate;
    Application.ShowMainForm := False;
    Result := False;
    Exit;
  end;

  with TRegistryErrorControlled.Create do  // I2890
  try
    if OpenKeyReadOnly(SRegKey_KeymanOSK_CU) and ValueExists(SRegValue_OSK_ShowVisualKeyboard) and ReadBool(SRegValue_OSK_ShowVisualKeyboard) then
      ShowVisualKeyboard;
  finally
    Free;
  end;

  if kmint.KeymanEngineControl = nil then
  begin
    TKeymanSentryClient.ReportMessage('StartKeymanEngine: KeymanEngineControl was unexpectedly nil');
  end
  else
    kmint.KeymanEngineControl.RestartEngine; // I1486

  StartKeymanX64;

  RegisterHotkeys;
end;

procedure TfrmKeyman7Main.RequestCurrentActiveKeyboard(Command: WORD);   // I3961
begin
  if FLastFocus <> 0 then
    PostMessage(FLastFocus, wm_keyman_control_internal, KMCI_GETACTIVEKEYBOARD, Command);   // I3949
end;

procedure TfrmKeyman7Main.WndProc(var Message: TMessage);
begin
  inherited;
  if Message.Msg = wm_keyman_control then
  begin
    // While keyman32 will never send messages to this window,
    // kmcomapi currently can.
    Message.Result := ProcessWMKeymanControl(LoWord(Message.WParam), HiWord(Message.WParam), Message.LParam);   // I3961
  end
  else if Message.Msg = wm_test_keyman_functioning then
  begin
    if Message.wParam = TKF_RESPONSE then
    begin
      tmrTestKeymanFunctioning.Enabled := False;
    end;
  end;
end;

procedure TfrmKeyman7Main.ProcessProfileChange(CommandAndAtom: DWORD);   // I3933   // I3949

  procedure ProcessHotkeyChange(hkl: DWORD; guidProfile: TGUID);
  var
    kbd: TLangSwitchKeyboard;
  begin
    kbd := FLangSwitchManager.FindKeyboard(hkl, guidProfile);
    if Assigned(kbd) then
    begin
      // Handle toggle hotkey
      if (kbd = FLangSwitchManager.ActiveKeyboard) and (kmcom.Options['koKeyboardHotkeysAreToggle'].Value) then
          kbd := FLangSwitchManager.Languages[0].Keyboards[0];

      ActivateKeyboard(kbd);
    end;
  end;

var
  val, FLangID: Integer;
  param2, param3: string;
  FClsid, FProfileGuid: TGUID;
  buftext: array[0..128] of char;
  buf: string;
  FActiveKeyboard: TLangSwitchKeyboard;
  i: Integer;
  wCommand: Word;
begin
  if GlobalGetAtomName(HiWord(CommandAndAtom), buftext, 128) = 0 then   // I3949
    Exit;   // I4286

  wCommand := LoWord(CommandAndAtom);

  GlobalDeleteAtom(HiWord(CommandAndAtom));   // I3949
  buf := buftext;

  //OutputDebugString(PChar('ProcessProfileChange("'+buf+'")'#13#10));

  {debug := } StrToken(buf, '|');   // I4285

  if not TryStrToInt(StrToken(buf, '|'), FLangID) then Exit;
  param2 := StrToken(buf, '|');
  if buf = '' then
  begin
    if not TryStrToInt(param2, val) then
    begin
      OutputDebugString(PChar('Could not process profile change'#13#10));
      Exit;
    end;

    if (wCommand = PC_UPDATE) or (wCommand = PC_UPDATE_LANGUAGESWITCH) then
    begin
      FLangSwitchManager.UpdateActive(FLangID, lsitWinKeyboard, val);   // I3949
      if FLangSwitchManager.ActiveKeyboard = nil then   // I4715
      begin
        FLangSwitchManager.Refresh;
        FLangSwitchManager.UpdateActive(FLangID, lsitWinKeyboard, val);   // I3949
      end;
      FActiveHKL := val;   // I4359
    end
    else if wCommand = PC_HOTKEYCHANGE then
    begin
      ProcessHotkeyChange(val, GUID_NULL);
      Exit;
    end;
  end
  else
  begin
    param3 := StrToken(buf, '|');
    try
      FClsid := StringToGUID(param2);
      FProfileGuid := StringToGUID(param3);
    except
      on E:EOleException do
      begin
        OutputDebugString(PChar(E.Message+#13#10));
        Exit;
      end;
    end;

    if (wCommand = PC_UPDATE) or (wCommand = PC_UPDATE_LANGUAGESWITCH) then
    begin
      FLangSwitchManager.UpdateActive(FLangID, lsitTIP, FClsid, FProfileGuid);

      if FLangSwitchManager.ActiveKeyboard = nil then   // I4715
      begin
        FLangSwitchManager.Refresh;
        FLangSwitchManager.UpdateActive(FLangID, lsitTIP, FClsid, FProfileGuid);
      end;
    end
    else if wCommand = PC_HOTKEYCHANGE then
    begin
      ProcessHotkeyChange(0, FProfileGuid);
      Exit;
    end;
  end;

  // Update the icon and the visual keyboard

  FActiveKeyboard := FLangSwitchManager.ActiveKeyboard;   // I3949
  if not Assigned(FActiveKeyboard) or (FActiveKeyboard.KeymanID = KEYMANID_NONKEYMAN) then
  begin
    SetTrayIcon(nil, nil);
  end
  else
  begin
    for i := 0 to kmcom.Keyboards.Count - 1 do
      if (kmcom.Keyboards[i].KeymanID = FActiveKeyboard.KeymanID) then
      begin
        SetTrayIcon(FRunningProduct, kmcom.Keyboards[i]);
        Break;
      end;
    UpdateOSKVisibility;
  end;

  PostGlobalKeyboardChange(FActiveKeyboard);   // I4271

  if Assigned(frmVisualKeyboard) then   // I3949
  begin
    frmVisualKeyboard.RefreshSelectedKeyboard;  // I2398
  end;

//TOUCH    if Assigned(frmTouchKeyboard) then
//TOUCH    begin
//TOUCH      frmTouchKeyboard.RefreshSelectedKeyboard;
//TOUCH    end;

  if wCommand = PC_UPDATE_LANGUAGESWITCH then   // I4124
  begin                                                           // TODO fixup product id
    //OutputDebugString(PChar('Showing language switch form'#13#10));
    ShowLanguageSwitchForm;
  end;
end;

function AllowSetForegroundWindow(dwProcessId: DWORD): BOOL; stdcall; external 'user32.dll';

const ASFW_ANY: Cardinal = $FFFFFFFF;

procedure TfrmKeyman7Main.MnuKeyboardClick(Sender: TObject);
begin
  if Sender is TKeymanMenuItem then
    with Sender as TKeymanMenuItem do
    begin
      if Assigned(Keyboard) then
        ActivateKeyboard(Keyboard);   // I4326
    end;
end;

procedure TfrmKeyman7Main.ActivateKeyboard(Keyboard: TLangSwitchKeyboard);   // I4326
var
  hwnd: THandle;
begin
  TDebugLogClient.Instance.WriteMessage('ActivateKeyboard: LastFocus=%x LastActive=%x Keyboard=%s KeymanID=%d', [FLastFocus, FLastActive, Keyboard.Caption, Keyboard.KeymanID]);   // I4674
  hwnd := FLastFocus;
  AllowSetForegroundWindow(ASFW_ANY);   // I3933
  PostMessage(FLastActive, wm_keyman_control_internal, KMCI_SETFOREGROUND, FLastFocus);   // I3933
  Keyboard.Activate(hwnd);   // I3933
end;

procedure TfrmKeyman7Main.SwitchToFirstWindowsKeyboard;   // I4326
var
  i: Integer;
  j: Integer;
begin
  for i := 0 to FLangSwitchManager.LanguageCount - 1 do
    for j := 0 to FLangSwitchManager.Languages[i].KeyboardCount - 1 do
      if FLangSwitchManager.Languages[i].Keyboards[j].KeymanID = KEYMANID_NONKEYMAN then
      begin
        ActivateKeyboard(FLangSwitchManager.Languages[i].Keyboards[j]);
        Exit;
      end;
end;

procedure TfrmKeyman7Main.SetLastFocus;
var
  tid: DWORD;
begin
  tid := GetWindowThreadProcessId(FLastFocus, nil);
  if tid = 0 then
  begin
    TDebugLogClient.Instance.WriteLastError('SetLastFocus', 'GetWindowThreadProcessId');
    Exit;
  end;

  if not AttachThreadInput(tid, GetCurrentThreadId, TRUE) then
  begin
    TDebugLogClient.Instance.WriteLastError('SetLastFocus', 'AttachThreadInput:1');
    Exit;
  end;

  //Windows.SetForegroundWindow(FLastFocus); //FLastActive);
  if Winapi.Windows.SetFocus(FLastFocus) = 0 then
    TDebugLogClient.Instance.WriteLastError('SetLastFocus', 'SetFocus');

  if not AttachThreadInput(tid, GetCurrentThreadId, FALSE) then
  begin
    TDebugLogClient.Instance.WriteLastError('SetLastFocus', 'AttachThreadInput:2');
    Exit;
  end;
end;

function TfrmKeyman7Main.IsProductLoaded: Boolean;
begin
  Result := Assigned(FRunningProduct);
end;

procedure TfrmKeyman7Main.RecreateTaskbarIcons;
begin
  if Assigned(FRunningProduct) then
  begin
    FRunningProduct.FTrayIcon.Visible := False;
    FRunningProduct.FTrayIcon.Visible := True;
  end;
end;

procedure TfrmKeyman7Main.MnuOpenKeyboardHelp(Sender: TObject);
var
  kbd: IKeymanKeyboardInstalled;
begin
  kbd := ActiveKeyboard;
  if not Assigned(kbd) then Exit;
  kmcom.Control.ShowKeyboardWelcome(kbd);
end;

function TfrmKeyman7Main.ActiveKeyboard: IKeymanKeyboardInstalled;
var
  i: Integer;
  FActiveKeyboard: TLangSwitchKeyboard;
begin
  FActiveKeyboard := FLangSwitchManager.ActiveKeyboard;   // I3949
  if not Assigned(FActiveKeyboard) or (FActiveKeyboard.KeymanID = KEYMANID_NONKEYMAN) then
    Exit(nil);   // I3949

  for i := 0 to kmcom.Keyboards.Count - 1 do
    if kmcom.Keyboards[i].KeymanID = FActiveKeyboard.KeymanID then
      Exit(kmcom.Keyboards[i]);   // I3949
end;

procedure TfrmKeyman7Main.MnuOpenKeymanConfiguration(Sender: TObject);
begin
  TKeymanDesktopShell.RunKeymanConfiguration('-c');
end;

procedure TfrmKeyman7Main.MnuOpenProductAbout(Sender: TObject);
begin
  TKeymanDesktopShell.RunKeymanConfiguration('-a');
end;

procedure TfrmKeyman7Main.MnuOpenProductHelp(Sender: TObject);
begin
  // Ctrl+Shift+Help command, with command line -sentry-client-test-exception
  if FEnableCrashTest and (GetKeyState(VK_CONTROL) < 0) and (GetKeyState(VK_SHIFT) < 0) then
    TKeymanSentryClient.Validate(True);

  TKeymanDesktopShell.OpenHelpJump('index', Self.ActiveKeyboard);
end;

procedure TfrmKeyman7Main.MnuRunProgram(Sender: TObject);   // I4606
var
  CmdLine: string;
  errmsg: string;
begin
  if Sender is TKeymanMenuItem then
    CmdLine := ((Sender as TKeymanMenuItem).CustomisationMenuItem).CmdLine
  else if Sender is TToolButton then
    CmdLine := (Sender as TKeymanToolButton).CmdLine;

  if not ExecuteProgram(CmdLine, TKeymanPaths.KeymanDesktopInstallDir, errmsg) then
    ShowMessage(errmsg);
  // todo: complete mnurunprogram
end;

procedure TfrmKeyman7Main.MnuSelectKeyboard(Sender: TObject);   // I4606
var
  kbd: TLangSwitchKeyboard;
begin
  if Sender is TKeymanMenuItem then
    kbd := (Sender as TKeymanMenuItem).Keyboard
  else if Sender is TToolButton then
    kbd := GetLangSwitchKeyboard((Sender as TKeymanToolButton).KeyboardName, True)
  else
    kbd := nil;

  if Assigned(kbd) then
    ActivateKeyboard(kbd);
end;

function TfrmKeyman7Main.GetLangSwitchKeyboard(const name: string; fallback: Boolean): TLangSwitchKeyboard;   // I4606
var
  i: Integer;
  j: Integer;
begin
  Result := nil;
  for i := 0 to FLangSwitchManager.LanguageCount - 1 do
  begin
    for j := 0 to FLangSwitchManager.Languages[i].KeyboardCount - 1 do
    begin
      if SameText(FLangSwitchManager.Languages[i].Keyboards[j].Caption, name) then
        Exit(FLangSwitchManager.Languages[i].Keyboards[j])
      else if (Result = nil) and fallback then
        Result := FLangSwitchManager.Languages[i].Keyboards[j];
    end;
  end;
end;

procedure TfrmKeyman7Main.MnuVisualKeyboard(Sender: TObject);
begin
  SetLastFocus;     // I1289 - Focus not returned to active app when OSK opened

//TOUCH    if UseTouchKeyboard then
//TOUCH    begin
//TOUCH      if TouchKeyboardVisible
//TOUCH        then HideTouchKeyboard
//TOUCH        else ShowTouchKeyboard;
//TOUCH    end
//TOUCH    else
//TOUCH    begin
    if VisualKeyboardVisible(apKeyboard)
      then HideVisualKeyboard
      else ShowVisualKeyboard(apKeyboard);
//TOUCH    end;
end;

procedure TfrmKeyman7Main.MnuFontHelper(Sender: TObject);
begin
  SetLastFocus;     // I1289 - Focus not returned to active app when OSK opened
  if VisualKeyboardVisible(apFontHelper)
    then HideVisualKeyboard
    else ShowVisualKeyboard(apFontHelper);
end;

procedure TfrmKeyman7Main.MnuCharacterMap(Sender: TObject);
begin
  SetLastFocus;     // I1289 - Focus not returned to active app when OSK opened
  if VisualKeyboardVisible(apCharacterMap)
    then HideVisualKeyboard
    else ShowVisualKeyboard(apCharacterMap);
end;

procedure TfrmKeyman7Main.MnuOpenTextEditor(Sender: TObject);
begin
  OpenTextEditor;
end;

procedure TfrmKeyman7Main.OpenTextEditor;
begin
  TKeymanDesktopShell.RunKeymanConfiguration('-t');
end;

procedure TfrmKeyman7Main.Notification(AComponent: TComponent; Operation: TOperation);
begin
  if (AComponent = frmVisualKeyboard) and (Operation = opRemove) then
    frmVisualKeyboard := nil;
  inherited Notification(AComponent, Operation);
end;

procedure TfrmKeyman7Main.HideVisualKeyboard;
begin
  if not Assigned(kmcom) and not StartKeymanEngine then Exit;

//TOUCH    if UseTouchKeyboard then
//TOUCH    begin
//TOUCH      HideTouchKeyboard;
//TOUCH      Exit;
//TOUCH    end;

  if Assigned(frmVisualKeyboard) then // I1274 - Avoid crash when closing OSK when already closed!
    frmVisualKeyboard.Release;
end;

procedure TfrmKeyman7Main.ShowVisualKeyboard(Page: TOSKActivePage);
begin
  if not Assigned(kmcom) and not StartKeymanEngine then Exit;

//TOUCH    if UseTouchKeyboard then
//TOUCH    begin
//TOUCH      ShowTouchKeyboard;
//TOUCH      Exit;
//TOUCH    end;

//TOUCH    HideTouchKeyboard;

  if not Assigned(frmVisualKeyboard) then  // I1274 - Related, don't recreate OSK if already exists
  begin
    frmVisualKeyboard := TfrmVisualKeyboard.Create(nil);
    frmVisualKeyboard.FreeNotification(Self);
    frmVisualKeyboard.Show;
    frmVisualKeyboard.ActivePage := Page; // I2287   // I4225
  end
  else if Page <> apUndefined then
    frmVisualKeyboard.ActivePage := Page;
end;

function TfrmKeyman7Main.VisualKeyboardVisible(Page: TOSKActivePage): Boolean;
begin
  Result := Assigned(frmVisualKeyboard) and ((Page = apUndefined) or (frmVisualKeyboard.ActivePage = Page));
end;

//TOUCH  function TfrmKeyman7Main.TouchKeyboardVisible: Boolean;
//TOUCH  begin
//TOUCH    Result := Assigned(frmTouchKeyboard);
//TOUCH  end;

procedure TfrmKeyman7Main.TestKeymanFunctioning(FunctionType: TTestKeymanFunctionType; RunOnSuccess, ResetCounter: Boolean);
begin
  tmrTestKeymanFunctioning.Enabled := False;

  if FunctionType = tkfPopupMenu then
  begin
    // We are only going to do it on SetActiveKeyboard for now
    ShowMenu(FRunningProduct.FTrayIcon, milLeft, False);   // I3990
    Exit;
  end;

  FTestKeymanFunctioning.FunctionType := FunctionType;
  FTestKeymanFunctioning.RunOnSuccess := RunOnSuccess;

	if ResetCounter then FTestKeymanFunctioning.Count := 0;

	if ShouldTestKeymanFunctioning then
  begin
    PostMessage(Handle, wm_test_keyman_functioning, TKF_PING, 0);
    tmrTestKeymanFunctioning.Enabled := True;
  end
	else if RunOnSuccess then
  begin
    case FunctionType of
      tkfPopupMenu: ShowMenu(FRunningProduct.FTrayIcon, milLeft, False);   // I3990
                      		// we know this is from a mouse click as kbd method tells us we know Keyman is working
    end;
  end;
end;

//TOUCH  procedure TfrmKeyman7Main.ShowTouchKeyboard;
//TOUCH  var
//TOUCH    FProduct: IKeymanProduct;
//TOUCH  begin
//TOUCH    if not Assigned(kmcom) and not StartKeymanEngine then Exit;
//TOUCH
//TOUCH    if Assigned(frmVisualKeyboard) then // I1274 - Avoid crash when closing OSK when already closed!
//TOUCH      frmVisualKeyboard.Release;
//TOUCH
//TOUCH    if not Assigned(frmTouchKeyboard) then
//TOUCH    begin
//TOUCH      FProduct := ProductFromMenuItem(nil);
//TOUCH      if not Assigned(FProduct) then Exit;
//TOUCH
//TOUCH      frmTouchKeyboard := TfrmTouchKeyboard.Create(nil, FProduct);
//TOUCH      frmTouchKeyboard.FreeNotification(Self);
//TOUCH      frmTouchKeyboard.SetContext(FCurrentContext);
//TOUCH      //frmTouchKeyboard.Show;
//TOUCH    end;
//TOUCH  end;

//TOUCH  procedure TfrmKeyman7Main.HideTouchKeyboard;
//TOUCH  begin
//TOUCH    if not Assigned(kmcom) and not StartKeymanEngine then Exit;
//TOUCH
//TOUCH    FreeAndNil(frmTouchKeyboard);
//TOUCH  end;

//TOUCH  function TfrmKeyman7Main.UseTouchKeyboard: Boolean;
//TOUCH  begin
//TOUCH    if not Assigned(kmcom)
//TOUCH      then Result := False
//TOUCH      else Result := kmcom.Options['koUseTouchLayout'].Value;
//TOUCH  end;

procedure TfrmKeyman7Main.tmrCheckInputPaneTimer(Sender: TObject);
var
  r: TRect;
  isVisible: Boolean;
begin
  if Assigned(FInputPane) and FInputPane.GetLocation(r) then
  begin
    isVisible := not r.IsEmpty;
    if FIsInputPaneVisible <> isVisible then
    begin
      FIsInputPaneVisible := isVisible;
      if kmint.KeymanEngineControl <> nil then
        kmint.KeymanEngineControl.UpdateTouchPanelVisibility(isVisible);
    end;
    //TDebugLogClient.Instance.WriteMessage('InputPane Location: %d, %d, %d, %d', [r.Left, r.Top, r.Right, r.Bottom]);
  end;
end;

procedure TfrmKeyman7Main.tmrOnlineUpdateCheckTimer(Sender: TObject);
begin
  with TRegistryErrorControlled.Create do  // I2890
  try
    if OpenKeyReadOnly(SRegKey_KeymanDesktop_CU) then
    begin
      if ValueExists(SRegValue_CheckForUpdates) and not ReadBool(SRegValue_CheckForUpdates) then Exit;
      if ValueExists(SRegValue_LastUpdateCheckTime) and (Now - ReadDateTime(SRegValue_LastUpdateCheckTime) < 7) then Exit;
      TKeymanDesktopShell.RunKeymanConfiguration('-ouc');
    end;
  finally
    Free;
  end;
end;

procedure TfrmKeyman7Main.tmrRefreshTimer(Sender: TObject);
const
  SECONDS_TO_CHECK = 5;
begin
  if tmrRefresh.Tag > SECONDS_TO_CHECK * (1000 div Integer(tmrRefresh.Interval)) then
  begin
    tmrRefresh.Enabled := False;
    tmrRefresh.Tag := 0;
  end
  else
    tmrRefresh.Tag := tmrRefresh.Tag + 1;

  PostMessage(Application.Handle, wm_keyman_control, KMC_REFRESH, 0);
end;

procedure TfrmKeyman7Main.tmrTestKeymanFunctioningTimer(Sender: TObject);
begin
  tmrTestKeymanFunctioning.Enabled := False;
  if FTestKeymanFunctioning.Count < 3 then
  begin
    if kmint.KeymanEngineControl <> nil then
      kmint.KeymanEngineControl.RestartEngine;
    Inc(FTestKeymanFunctioning.Count);
    TestKeymanFunctioning(FTestKeymanFunctioning.FunctionType, FTestKeymanFunctioning.RunOnSuccess, False);
  end
  else
  begin
    if Winapi.Windows.MessageBox(Handle,
      'Keyman Engine does not appear to be receiving messages from Windows normally.  You may have a conflicting application.  '+
      'Do you want to diagnose this issue now?',
      PChar('Keyman Engine'), MB_ICONHAND or MB_TOPMOST or MB_OKCANCEL) = IDCANCEL then Exit;

    kmcom.Control.OpenDiagnostics;
  end;
end;

function TfrmKeyman7Main.ShouldTestKeymanFunctioning: Boolean;
begin
  Result := kmcom.Options.Items['koTestKeymanFunctioning'].Value;
end;

procedure TfrmKeyman7Main.TrayIconDebugMessage(Sender: TObject; msg: Integer);
var
  Point: TPoint;
begin
  if FWelcomeWindow <> 0 then
  begin
    GetCursorPos(Point);
    PostMessage(FWelcomeWindow, wm_keyman_control, msg, Integer(PointToSmallPoint(Point)));
  end;
end;

function TfrmKeyman7Main.GetActiveKeymanID: Integer;   // I3949
var
  FActiveKeyboard: TLangSwitchKeyboard;
begin
  FActiveKeyboard := FLangSwitchManager.ActiveKeyboard;
  if not Assigned(FActiveKeyboard) then
    Exit(KEYMANID_NONKEYMAN);
  Exit(FActiveKeyboard.KeymanID);
end;

procedure TfrmKeyman7Main.UpdateOSKVisibility;  // I1288 - Show OSK when keyboard is selected
begin
  if not Option_AutoOpenOSK or FOSKManuallyClosedThisSession then
    Exit;

  if FInUpdateOSKVisibility then Exit;
  FInUpdateOSKVisibility := True;

  if (ActiveKeymanID  <> KEYMANID_NONKEYMAN) and not Assigned(frmVisualKeyboard) then   // I3949
    ShowVisualKeyboard;

  FInUpdateOSKVisibility := False;
end;

type
  TLangSwitchRefreshWatcher = class(TThread)
  private
    FOwnerHandle: THandle;
    hTerminatedEvent: THandle;
  protected
    procedure Execute; override;
    procedure TerminatedSet; override;
  public
    constructor Create(AOwnerHandle: THandle); reintroduce;
    destructor Destroy; override;
  end;

procedure TfrmKeyman7Main.CreateWnd;
begin
  inherited;
  if Assigned(FLangSwitchRefreshWatcher) then
  begin
    FLangSwitchRefreshWatcher.Terminate;
    FreeAndNil(FLangSwitchRefreshWatcher);
  end;
  FLangSwitchRefreshWatcher := TLangSwitchRefreshWatcher.Create(Application.Handle);
  FLangSwitchRefreshWatcher.Start;
end;

procedure TfrmKeyman7Main.StartKeymanX64;
var
  cmd, dir, params: string;
  sei: TShellExecuteInfoW;
begin
  if not IsWow64 then Exit;   // I4374

  try
    // TODO: use TKeymanPaths to find keymanx64?
    dir := ExtractFilePath(ParamStr(0));
    cmd := dir + 'keymanx64.exe';
    params := Format('%d %d', [GetCurrentProcessId, Application.Handle]);

    if not FileExists(cmd) then
      // We'll get notification of the issue but it won't
      // crash the process
      raise Exception.Create(cmd+' could not be found');

    FillChar(sei, SizeOf(sei), 0);
    sei.cbSize := SizeOf(sei);
    sei.Wnd := Handle;
    sei.lpVerb := 'open';
    sei.lpFile := PWideChar(cmd);
    sei.lpParameters := PChar(params);
    sei.lpDirectory := PWideChar(dir);
    sei.nShow := SW_SHOW;

    if not ShellExecuteExW(@sei) then
      RaiseLastOSError;
  except
    on E:Exception do
    begin
      // We're going to handle any exceptions here but we'd like to know that
      // they happened
      TKeymanSentryClient.ReportHandledException(E, 'Error starting keymanx64', True);
    end;
  end;
end;

procedure TfrmKeyman7Main.HotkeyWndProc(var Message: TMessage);
begin
  if Message.Msg = WM_HOTKEY then
  begin
    KL.Log('Hotkey %d', [Message.WParam]);
    if Message.WParam > kh__High
      then DoLanguageHotkey(Message.WParam - kh__High - 1)
      else DoInterfaceHotkey(Message.WParam);
  end;
  Message.Result := DefWindowProc(FHotkeyWindow, Message.Msg, Message.WParam, Message.LParam);
end;

function KeymanHotkeyModifiersToWindowsHotkeyModifiers(v: KeymanHotkeyModifiers): Integer;
begin
  Result := 0;
  if (v and HK_SHIFT) = HK_SHIFT then Result := Result or MOD_SHIFT;
  if (v and HK_CTRL) = HK_CTRL then Result := Result or MOD_CONTROL;
  if (v and HK_ALT) = HK_ALT then Result := Result or MOD_ALT;
end;

procedure TfrmKeyman7Main.RegisterHotkeys;
var
  hk: IKeymanHotkey;
  i: Integer;
  language: IKeymanLanguage;
  id: Integer;
begin
  if not Reg_GetDebugFlag(SRegValue_Flag_UseRightModifierHotKey) then Exit;

  TDebugLogClient.Instance.WriteMessage('Enter RegisterHotkeys', []);

  if FHotkeyWindow = 0 then
    FHotkeyWindow := AllocateHWnd(HotkeyWndProc);

  if not Assigned(FHotkeys) then
    FHotkeys := TIntegerList.Create;

  UnregisterHotkeys;

  for i := 0 to kmcom.Hotkeys.Count - 1 do
  begin
    hk := kmcom.Hotkeys[i];
    if not hk.IsEmpty and (hk.VirtualKey <> 0) then
    begin
      // Note, if hk.VirtualKey is 0, this indicates a modifier-only hotkey such
      // as Alt+Left Shift. These are handled in keyman32 k32_lowlevelkeyboardhook
      // because RegisterHotkey cannot handle modifier-only hotkeys.
      if RegisterHotkey(FHotkeyWindow, hk.Target, KeymanHotkeyModifiersToWindowsHotkeyModifiers(hk.Modifiers), hk.VirtualKey) then
      begin
        TDebugLogClient.Instance.WriteMessage('Added hotkey %d -> %x %x', [hk.Target,
           KeymanHotkeyModifiersToWindowsHotkeyModifiers(hk.Modifiers), hk.VirtualKey]);
        FHotkeys.Add(hk.Target)
      end
      else
        TDebugLogClient.Instance.WriteLastError('RegisterHotkeys', 'RegisterHotkey', 'Failed to register hotkey '+IntToStr(hk.Target));
    end;
  end;

  for i := 0 to kmcom.Languages.Count - 1 do
  begin
    language := kmcom.Languages[i];
    hk := language.Hotkey;
    if Assigned(hk) and not hk.IsEmpty and (hk.VirtualKey <> 0) then
    begin
      id := kh__High + 1 + i;
      if RegisterHotkey(FHotkeyWindow, id, KeymanHotkeyModifiersToWindowsHotkeyModifiers(hk.Modifiers), hk.VirtualKey) then
      begin
        TDebugLogClient.Instance.WriteMessage('Added hotkey for language %s [%d] -> %x %x', [language.LocaleName, id,
           KeymanHotkeyModifiersToWindowsHotkeyModifiers(hk.Modifiers), hk.VirtualKey]);
        FHotkeys.Add(id);
      end
      else
        TDebugLogClient.Instance.WriteLastError('RegisterHotkeys', 'RegisterHotkey', 'Failed to register hotkey '+IntToStr(id));
    end;
  end;
end;

procedure TfrmKeyman7Main.UnregisterHotkeys;
var
  i, hk: Integer;
begin
  if not Reg_GetDebugFlag(SRegValue_Flag_UseRightModifierHotKey) then Exit;

  TDebugLogClient.Instance.WriteMessage('Enter UnregisterHotkeys', []);

  if not Assigned(FHotkeys) then
    Exit;

  for i := 0 to FHotkeys.Count - 1 do
  begin
    hk := FHotkeys[i];
    if not UnregisterHotKey(FHotkeyWindow, hk) then
      TDebugLogClient.Instance.WriteLastError('UnregisterHotkeys', 'UnregisterHotkey', 'Failed to unregister hotkey '+IntToStr(hk));
  end;
  FHotkeys.Clear;
end;

{ TLangSwitchRefreshWatcher }

constructor TLangSwitchRefreshWatcher.Create(AOwnerHandle: THandle);
begin
  inherited Create(True);
  FOwnerHandle := AOwnerHandle;
  hTerminatedEvent := CreateEvent(nil, True, False, nil);
end;

destructor TLangSwitchRefreshWatcher.Destroy;
begin
  CloseHandle(hTerminatedEvent);
  inherited Destroy;
end;

procedure TLangSwitchRefreshWatcher.Execute;
var
  hk: HKEY;
  hEvent: THandle;
  hEvents: array[0..1] of THandle;
const
  // We don't really know how long the changes are going to take
  // so we'll wait 500 msec and then ask the main thread to reload
  // the changes from the registry
  ARBITRARY_WAIT_FOR_CHANGES = 1000;
begin
  if RegOpenKeyEx(HKEY_CURRENT_USER, PChar(SRegKey_ControlPanelInternationalUserProfile), 0,
      KEY_NOTIFY, hk) <> ERROR_SUCCESS then
    Exit;
  try
    hEvent := CreateEvent(nil, True, False, nil);
    if hEvent = 0 then
      Exit;
    try
      // Watch the registry key for any changes
      if RegNotifyChangeKeyValue(hk, True, REG_NOTIFY_CHANGE_NAME or
          REG_NOTIFY_CHANGE_LAST_SET, hEvent, True) <> ERROR_SUCCESS then
        Exit;
      repeat
        // Wait for either the registry to change or for us to be terminated
        hEvents[0] := hEvent;
        hEvents[1] := hTerminatedEvent;
        if WaitForMultipleObjects(2, @hEvents[0], False, INFINITE) <> WAIT_OBJECT_0 then
          Exit;

        ResetEvent(hEvent);

        // Start watching for changes again before requesting a refresh, so we
        // don't miss any additional changes that come through
        if RegNotifyChangeKeyValue(hk, True, REG_NOTIFY_CHANGE_NAME or
            REG_NOTIFY_CHANGE_LAST_SET, hEvent, True) <> ERROR_SUCCESS then
          Exit;

        // Finally, tell the master controller to get ready to process the refresh
        PostMessage(FOwnerHandle, wm_keyman_control, MAKELONG(KMC_REFRESH, 1), 0);
      until False;

    finally
      CloseHandle(hEvent);
    end;
  finally
    RegCloseKey(hk);
  end;

end;

procedure TLangSwitchRefreshWatcher.TerminatedSet;
begin
  inherited;
  SetEvent(hTerminatedEvent);
end;

initialization
  wm_test_keyman_functioning := RegisterWindowMessage('wm_test_keyman_functioning');
  wm_keyman_globalswitch := RegisterWindowMessage('WM_KEYMAN_GLOBALSWITCH');
  wm_keyman_globalswitch_process := RegisterWindowMessage('WM_KEYMAN_GLOBALSWITCH_PROCESS');
  wm_keyman_control := RegisterWindowMessage('WM_KEYMAN_CONTROL');
  wm_keyman_control_internal := RegisterWindowMessage('WM_KEYMAN_CONTROL_INTERNAL');   // I3933

  ChangeWindowMessageFilter(wm_keyman_control, MSGFLT_ADD);
  ChangeWindowMessageFilter(wm_keyman_globalswitch, MSGFLT_ADD);
  ChangeWindowMessageFilter(wm_keyman_globalswitch_process, MSGFLT_ADD);
  ChangeWindowMessageFilter(wm_keyman_control_internal, MSGFLT_ADD);   // I3933
end.
