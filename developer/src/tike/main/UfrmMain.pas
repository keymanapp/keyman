(*
  Name:             UfrmMain
  Copyright:        Copyright (C) SIL International.
  Documentation:
  Description:
  Create Date:      20 Jun 2006

  Modified Date:    24 Aug 2015
  Authors:          mcdurdin
  Related Files:
  Dependencies:

  Bugs:
  Todo:
  Notes:
  History:          20 Jun 2006 - mcdurdin - Initial version
                    01 Aug 2006 - mcdurdin - Rework ui using sp-TBX
                    02 Aug 2006 - mcdurdin - Remove old menu
                    23 Aug 2006 - mcdurdin - Remove Window menu
                    23 Aug 2006 - mcdurdin - Refactor menus as actions
                    23 Aug 2006 - mcdurdin - Implement encoding combobox
                    23 Aug 2006 - mcdurdin - Support WM_InputLangChange for visual keyboard dynamic refresh
                    23 Aug 2006 - mcdurdin - Add help form
                    23 Aug 2006 - mcdurdin - Force tab orders to active child or active docked form only (not through all forms)
                    30 Aug 2006 - mcdurdin - Add reformat XML edit menu item
                    14 Sep 2006 - mcdurdin - Add debug toolbar
                    14 Sep 2006 - mcdurdin - Add CRM links
                    14 Sep 2006 - mcdurdin - Add UnicodeData build callbacks
                    14 Sep 2006 - mcdurdin - Add new Character Map events
                    28 Sep 2006 - mcdurdin - Add check for updates, debug manager
                    28 Sep 2006 - mcdurdin - Remove file association check (now done only by installer)
                    04 Dec 2006 - mcdurdin - Add CRM button
                    04 Dec 2006 - mcdurdin - Add file to project MRU when opened
                    04 Dec 2006 - mcdurdin - Support standalone OSK editor for KVK files
                    12 Dec 2006 - mcdurdin - Remove Project Settings menu item
                    04 Jan 2007 - mcdurdin - Add help support
                    04 Jan 2007 - mcdurdin - Add proxy support
                    04 Jan 2007 - mcdurdin - Fix scroll bug for page control
                    04 Jan 2007 - mcdurdin - Check if branding pack is installed
                    15 Jan 2007 - mcdurdin - Disable KPP editor if no branding pack installed
                    25 Jan 2007 - mcdurdin - Remove ActiveChild.Setfocus in WMActivate (resets focus to wrong control)
                    16 May 2007 - mcdurdin - I806 - File/Close menu item
                    30 May 2007 - mcdurdin - I825 - Added proxy username and password
                    23 Aug 2007 - mcdurdin - I1010 - Persist untitled project
                    23 Aug 2007 - mcdurdin - I1004 - Fix crash when starting up on Vista with Matrox driver?
                    12 Oct 2007 - mcdurdin - I944 - Fix crash when FChildWindows is nil when closing Keyman Developer
                    19 Nov 2007 - mcdurdin - I1157 - const string parameters
                    16 Jan 2009 - mcdurdin - Add font helper menu item
                    25 May 2010 - mcdurdin - I2392 - Activation Client integration
                    26 Jul 2010 - mcdurdin - I2467 - 8.0 renumber
                    17 Dec 2010 - mcdurdin - U2595 - Remove gnugettext
                    31 Jan 2011 - mcdurdin - I2655 - No prompt for unsaved untitled project on exit
                    03 May 2011 - mcdurdin - I2890 - Record diagnostic data when encountering registry errors
                    18 May 2012 - mcdurdin - I3306 - V9.0 - Remove TntControls + Win9x support
                    08 Jun 2012 - mcdurdin - I3350 - V9.0 - Register control classes for Keyman Developer to start
                    17 Aug 2012 - mcdurdin - I3377 - KM9 - Update code references from 8.0 to 9.0
                    08 Oct 2012 - mcdurdin - I3463 - V9.0 - Look up charmap root path from registry when starting Developer
                    24 Oct 2012 - mcdurdin - I3484 - V9.0 - Tidy up look and feel of Keyman Developer skins
                    24 Jan 2012 - mcdurdin - I3184 - Missing manifest for tike.exe
                    02 Feb 2012 - mcdurdin - I2975 - VistaAltFixUnit causes exception on shutdown
                    06 Feb 2012 - mcdurdin - I3082 - Reload text file with specific encoding support
                    03 Nov 2012 - mcdurdin - I3502 - V9.0 - Merge of I3082 - Reload text file with specific encoding support
                    03 Nov 2012 - mcdurdin - I3501 - V9.0 - Merge of I3184 - Missing manifest for tike.exe
                    11 Aug 2013 - mcdurdin - I3887 - V9.0 - Use strict most standard compliant edge compatibility for embedded IE in TIKE
                    13 Feb 2014 - mcdurdin - I4045 - V9.0 - Developer should use a TIKE user agent for the layout editor
                    27 Feb 2014 - mcdurdin - I4081 - V9.0 - Trace compile errors in subfiles in Keyman Developer
                    10 Jun 2014 - mcdurdin - I4257 - V9.0 - Add ethnologue language codes to unicodedata.mdb
                    10 Jun 2014 - mcdurdin - I3733 - V9.0 - File format dropdown should be disabled in project view
                    10 Oct 2014 - mcdurdin - I4436 - V9.0 - browser emulation control for kmshell breaks downlevel versions of Keyman
                    04 Nov 2014 - mcdurdin - I4507 - V9.0 - Move tab close button onto tabs
                    30 Apr 2015 - mcdurdin - I4677 - V9.0 - Move Developer help to online only
                    04 May 2015 - mcdurdin - I4687 - V9.0 - Split project UI actions into separate classes
                    04 May 2015 - mcdurdin - I4691 - V9.0 - Always save project automatically
                    27 May 2015 - mcdurdin - I4352 - Developer crashes on exit occasionally [CrashID:tike.exe_9.0.453.0_0058805F_EInvalidOperation]
                    27 May 2015 - mcdurdin - I4404 - Developer crashes on exit occasionally [CrashID:tike.exe_9.0.466.0_0058805F_EInvalidOperation]
                    27 May 2015 - mcdurdin - I4485 - Developer crashes on exit occasionally [CrashID:tike.exe_9.0.473.0_005880FB_EInvalidOperation]
                    23 Jun 2015 - mcdurdin - I4749 - Opening a keyboard externally can open it multiple times
                    24 Jul 2015 - mcdurdin - I4796 - Refresh Keyman Developer look and feel for release
                    03 Aug 2015 - mcdurdin - I4807 - Add Character Identifier to Keyman Developer
                    09 Aug 2015 - mcdurdin - I4841 - Restructure version 9 developer help
                    09 Aug 2015 - mcdurdin - I4847 - Code editor does not get focus automatically when file is loaded
                    09 Aug 2015 - mcdurdin - I2986 - Ctrl+N, Ctrl+O not working in Developer Project view
                    24 Aug 2015 - mcdurdin - I4874 - Keyman Configuration test windows do not always display correctly
*)
unit UfrmMain;  // I3306   // I3502   // I3501   // I4796

interface

uses
  System.UITypes,
  System.Generics.Collections,
  Windows,
  Messages,
  SysUtils,
  Classes,
  Graphics,
  Controls,
  Forms,
  ComCtrls,
  ImgList,
  Menus,
  StdCtrls,
  ExtCtrls,
  ToolWin,
  Buttons,
  KeymanDeveloperUtils,
  UfrmMDIChild,
  UfrmMDIEditor,
  MenuImgList,
  Keyman.Developer.System.Project.Project,
  Keyman.Developer.System.TikeMultiProcess,
  Keyman.Developer.UI.Project.UfrmProject,
  CharacterMapSettings,
  mrulist,
  UfrmUnicodeDataStatus,
  CharacterDragObject,
  Keyman.Developer.UI.dmActionsModelEditor,
  UnicodeData,
  UserMessages,
  webhelp,
  dmActionsDebugger,
  dmActionsKeyboardEditor,
  dmActionsMain,
  Dialogs,
  UfrmTike,
  AppEvnts,
  DropTarget,
  System.ImageList,
  Winapi.ActiveX,
  CloseButtonPageControl,
  JvComponentBase,
  JvDockControlForm,
  JvDockTree,
  JvDockVIDStyle,
  JvDockVSNetStyle,
  JvAppRegistryStorage,
  Vcl.ActnMan,
  Vcl.ActnCtrls;

type
  TfrmKeymanDeveloper = class(TTikeForm, IUnicodeDataUIManager, IDragDrop)
    barStatus: TStatusBar;
    ilPages: TImageList;
    lstImages: TMenuImgList;
    ApplicationEvents1: TApplicationEvents;
    pages: TCloseButtonPageControl;
    dockServer: TJvDockServer;
    JvDockVSNetStyle1: TJvDockVSNetStyle;
    mnuReloadAsFormat: TPopupMenu;
    ANSI1: TMenuItem;
    UTF81: TMenuItem;
    UTF161: TMenuItem;
    MainMenu1: TMainMenu;
    barTools: TControlBar;
    panDebugToolbar: TPanel;
    cbDebugSystemKeyboard: TComboBox;
    ToolBar2: TToolBar;
    ToolButton20: TToolButton;
    ToolButton21: TToolButton;
    ToolButton23: TToolButton;
    ToolButton22: TToolButton;
    ToolButton24: TToolButton;
    ToolButton25: TToolButton;
    ToolButton26: TToolButton;
    ToolButton27: TToolButton;
    ToolButton28: TToolButton;
    ToolButton29: TToolButton;
    panToolbar: TPanel;
    ToolBar1: TToolBar;
    ToolButton1: TToolButton;
    ToolButton2: TToolButton;
    ToolButton3: TToolButton;
    ToolButton4: TToolButton;
    ToolButton5: TToolButton;
    ToolButton6: TToolButton;
    ToolButton7: TToolButton;
    ToolButton8: TToolButton;
    ToolButton9: TToolButton;
    ToolButton10: TToolButton;
    ToolButton11: TToolButton;
    ToolButton12: TToolButton;
    ToolButton19: TToolButton;
    ToolButton14: TToolButton;
    ToolButton15: TToolButton;
    cbTextFileFormat: TComboBox;
    SpeedButton1: TSpeedButton;
    SpeedButton2: TSpeedButton;
    File1: TMenuItem;
    Edit1: TMenuItem;
    View1: TMenuItem;
    Project1: TMenuItem;
    mnuKeyboard: TMenuItem;
    mnuDebug: TMenuItem;
    mnuTools: TMenuItem;
    Help1: TMenuItem;
    New1: TMenuItem;
    Open1: TMenuItem;
    Save1: TMenuItem;
    SaveAs1: TMenuItem;
    Savecopyas1: TMenuItem;
    Revert1: TMenuItem;
    N5: TMenuItem;
    Close1: TMenuItem;
    N6: TMenuItem;
    PageSetup1: TMenuItem;
    PrintPreview1: TMenuItem;
    Print1: TMenuItem;
    N8: TMenuItem;
    Exit1: TMenuItem;
    N9: TMenuItem;
    mnuFileRecent: TMenuItem;
    Undo1: TMenuItem;
    Redo1: TMenuItem;
    N10: TMenuItem;
    Cut1: TMenuItem;
    Copy1: TMenuItem;
    Paste1: TMenuItem;
    N18: TMenuItem;
    Delete1: TMenuItem;
    N19: TMenuItem;
    SelectAll1: TMenuItem;
    N20: TMenuItem;
    Find1: TMenuItem;
    FindNext1: TMenuItem;
    Replace1: TMenuItem;
    N21: TMenuItem;
    ReformatXMLDocument1: TMenuItem;
    Project2: TMenuItem;
    N22: TMenuItem;
    CharacterIdentifier1: TMenuItem;
    CharacterMap1: TMenuItem;
    Messages1: TMenuItem;
    ContextHelp1: TMenuItem;
    oolbar1: TMenuItem;
    StatusBar1: TMenuItem;
    N23: TMenuItem;
    ExpandEditor1: TMenuItem;
    N24: TMenuItem;
    CharacterFont1: TMenuItem;
    CodeFont1: TMenuItem;
    NewProject1: TMenuItem;
    OpenProject1: TMenuItem;
    N25: TMenuItem;
    mnuProjectsRecent: TMenuItem;
    N26: TMenuItem;
    N27: TMenuItem;
    mnuProjectAddToProject: TMenuItem;
    CurrentEditorFile1: TMenuItem;
    OtherFiles1: TMenuItem;
    ProjectSettings1: TMenuItem;
    CompileKeyboard1: TMenuItem;
    N28: TMenuItem;
    N29: TMenuItem;
    IncludeDebugInformation1: TMenuItem;
    estKeyboard1: TMenuItem;
    Install1: TMenuItem;
    Uninstall1: TMenuItem;
    N31: TMenuItem;
    Fonts1: TMenuItem;
    FontHelper1: TMenuItem;
    SetBreakpoint1: TMenuItem;
    N32: TMenuItem;
    N33: TMenuItem;
    N34: TMenuItem;
    N35: TMenuItem;
    N36: TMenuItem;
    StartDebugging1: TMenuItem;
    StopDebugger1: TMenuItem;
    ANSITestMode1: TMenuItem;
    SingleStepMode1: TMenuItem;
    StepForward1: TMenuItem;
    Run1: TMenuItem;
    Pause1: TMenuItem;
    View2: TMenuItem;
    N37: TMenuItem;
    SelectSystemKeyboard1: TMenuItem;
    State1: TMenuItem;
    Elements1: TMenuItem;
    CallStack1: TMenuItem;
    Deadkeys1: TMenuItem;
    RegressionTesting1: TMenuItem;
    N38: TMenuItem;
    Font1: TMenuItem;
    UseEditorCharacterFont1: TMenuItem;
    VirtualKeyIdentifier1: TMenuItem;
    N39: TMenuItem;
    Options1: TMenuItem;
    Contents1: TMenuItem;
    N40: TMenuItem;
    CheckforUpdates1: TMenuItem;
    N41: TMenuItem;
    About1: TMenuItem;
    N1: TMenuItem;
    Reloadwithencoding1: TMenuItem;
    ReloadasANSI1: TMenuItem;
    ReloadasUTF81: TMenuItem;
    ReloadasUTF161: TMenuItem;
    mnuToolsDebugTests: TMenuItem;
    mnuToolsDebugTestsExceptionTest: TMenuItem;
    CloseProject1: TMenuItem;
    mnuModel: TMenuItem;
    CompileModel1: TMenuItem;
    N2: TMenuItem;
    estLexicalModel1: TMenuItem;
    mnuToolsDebugTestsShowDebuggerEventsPanel: TMenuItem;
    N3: TMenuItem;
    N4: TMenuItem;
    mnuToolsWebDebugger: TMenuItem;
    mnuToolsWebDebuggerOpenInBrowser: TMenuItem;
    mnuToolsWebDebuggerCopyToClipboard: TMenuItem;
    N7: TMenuItem;
    mnuToolsWebDebuggerConfigure: TMenuItem;
    N11: TMenuItem;
    Startserver1: TMenuItem;
    Stopserver1: TMenuItem;
    ToolButton13: TToolButton;
    ToolButton16: TToolButton;
    mnuWindow: TMenuItem;
    Newwindow1: TMenuItem;
    N12: TMenuItem;
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure mnuFileClick(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure mnuProjectClick(Sender: TObject);
    procedure mnuToolsDebugTestsExceptionTestClick(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure cbTextFileFormatItemClick(Sender: TObject);
    procedure cbDebugSystemKeyboard_DropDown(Sender: TObject);
    procedure cbDebugSystemKeyboardItemClick(Sender: TObject);
    procedure mnuDebugI374Click(Sender: TObject);
    function ApplicationEvents1Help(Command: Word; Data: NativeInt;
      var CallHelp: Boolean): Boolean;
    procedure mnuDebugSetWindowSizeForScreenshotsClick(Sender: TObject);
    procedure pagesChange(Sender: TObject);
    procedure pagesCloseTab(Sender: TObject; Index: Integer);
    procedure mnuToolsClick(Sender: TObject);
    procedure mnuToolsDebugTestsShowDebuggerEventsPanelClick(Sender: TObject);
    procedure ApplicationEvents1Message(var Msg: tagMSG; var Handled: Boolean);
    procedure mnuWindowClick(Sender: TObject);

  private
    AppStorage: TJvAppRegistryStorage;

    FControlDown: Boolean;

    FCharMapSettings: TCharMapSettings;
    FDropTarget: TDropTarget;
    FChildWindows: TChildWindowList;

    FProjectMRU: TMRUList;
    hInputLangChangeHook: Cardinal;
    FUnicodeDataStatusForm: TfrmUnicodeDataStatus;
    FInOnHelp: Boolean;
    mHHelp: TWebHookHelpSystem;   // I4677
    FFirstShow: Boolean;
    FIsClosing: Boolean;
    FCanClose: Boolean;
    FHasDoneCloseCleanup: Boolean;

    FFilesToOpen: TStringList;

    FWindowProcesses: TObjectList<TTikeProcess>;

    //procedure ChildWindowsChange(Sender: TObject);
    procedure WMUserFormShown(var Message: TMessage); message WM_USER_FORMSHOWN;
    procedure WMUserInputLangChange(var Message: TMessage); message WM_USER_INPUTLANGCHANGE;

    procedure WMCopyData(var Message: TWMCopyData); message WM_COPYDATA;
    procedure WMUserOpenFiles(var Message: TMessage); message WM_USER_OpenFiles;

    procedure UpdateFileMRU;
    procedure ProjectMRUChange(Sender: TObject);

    procedure AppOnActivate(Sender: TObject);
    function GetActiveEditor: TfrmTikeEditor;

    function OpenKMNEditor(FFileName: string): TfrmTikeEditor;
    function OpenKPSEditor(FFileName: string): TfrmTikeEditor;
    procedure SetActiveChild(const Value: TfrmTikeChild);
    function OpenKVKEditor(FFileName: string): TfrmTikeEditor;
    function OpenTSVEditor(FFileName: string): TfrmTikeEditor;

    procedure ShowChild(Window: TfrmTikeChild);
    function GetActiveChild: TfrmTikeChild;
    function GetActiveChildIndex: Integer;
    procedure SetActiveChildIndex(const Value: Integer);
    procedure WMActivate(var Message: TWMActivate); message WM_ACTIVATE;
    procedure mnuProjectRecentFileClick(Sender: TObject);
    procedure mnuFileRecentFileClick(Sender: TObject);

    function GetDropTargetIntf: IDropTarget;

    { IDropTarget }

    procedure Drop(const FileNames: TArray<string>);
    function DropAllowed(const FileNames: TArray<string>): Boolean;
    procedure InitDock;
    procedure LoadDockLayout;
    procedure SaveDockLayout;
    procedure CEFShutdownComplete(Sender: TObject);
    procedure ActivateActiveChild;
    function OpenModelEditor(FFileName: string): TfrmTikeEditor;
    procedure DoCloseCleanup;

    procedure RefreshProjectMRU;
    procedure mnuWindowSelectClick(Sender: TObject);

  protected
    procedure WndProc(var Message: TMessage); override;
    procedure CreateWnd; override;
    procedure DestroyWnd; override;

  public
    procedure AddMRU(FileName: string);
    procedure GetTabOrderList(List: TList); override;

    procedure CharMapCancelFocus(Sender: TObject);
    procedure CharMapCanInsertCode(Sender: TObject; Control: TWinControl;
      var Result: Boolean);
    procedure CharMapInsertCode(Sender: TObject; Control: TWinControl;
      DragObject: TCharacterDragObject);

    procedure UpdateCaption;

    procedure DefaultDockLayout;

    procedure UDUI_Error(Sender: TUnicodeData; Error: TUnicodeDataError; const Details: WideString);
    function UDUI_ShouldStartRebuildOnError(const Msg: WideString): Boolean;
    function UDUI_StartRebuild(Callback: TNotifyEvent; AskFirst: Boolean): Boolean;
    procedure UDUI_UpdateStatus(const Msg: WideString; Pos: Integer; Max: Integer);

    property ActiveChildIndex: Integer read GetActiveChildIndex write SetActiveChildIndex;

    function BeforeOpenProject: Boolean;
    procedure ShowProject;
    function ProjectForm: TfrmProject;
    procedure ToggleProject;

    procedure ShowDebug(AShow: Boolean);

    function SaveAndCloseAllFiles: Boolean;

    property ActiveChild: TfrmTikeChild read GetActiveChild write SetActiveChild;
    property ActiveEditor: TfrmTikeEditor read GetActiveEditor;
    procedure FocusActiveChild;

    procedure NotifyChildWindowChange(Window: TfrmTikeChild; Action: TListNotification);
    procedure UpdateChildCaption(Window: TfrmTikeChild);

    function FindEditorByFileName(AFileName: string): TfrmTikeEditor;

    procedure RefreshOptions;

    procedure OpenProjectInCurrentProcess(FileName: WideString);
    function OpenEditor(FFileName: string; frmClass: TfrmTikeEditorClass): TfrmTikeEditor;
    procedure OpenProject(const filename: string);
    function OpenFile(FFileName: string; FCloseNewFile: Boolean): TfrmTikeChild;
    procedure OpenFilesInProject(FFileNames: TArray<string>);
    procedure OpenNewWindow;

    procedure HelpTopic(s: string); overload;
    procedure HelpTopic(Sender: TTIKEForm); overload;

    property ChildWindows: TChildWindowList read FChildWindows;
    property CharMapSettings: TCharMapSettings read FCharMapSettings;

    property ProjectMRU: TMRUList read FProjectMRU;

    property DropTargetIntf: IDropTarget read GetDropTargetIntf;
  end;

var
  frmKeymanDeveloper: TfrmKeymanDeveloper;

implementation

uses
  System.Math,
  Winapi.WinInet,
  Winapi.ShlObj,
  Winapi.UxTheme,
  System.Win.ComObj,
  Vcl.Themes,

  Keyman.System.CEFManager,

  CharMapDropTool,
  HTMLHelpViewer,
  KLog,
  KeymanVersion,
  Keyman.System.CopyDataHelper,
  Keyman.System.KeymanSentryClient,
  Keyman.Developer.UI.TikeOnlineUpdateCheck,
  GlobalProxySettings,
  Keyman.Developer.System.LaunchProjects,
  Keyman.Developer.System.Project.ProjectFile,
  Keyman.Developer.System.Project.ProjectFileType,
  Keyman.Developer.System.Project.WelcomeRenderer,
  Keyman.Developer.System.Project.ProjectLoader,
  Keyman.Developer.System.Project.ProjectLog,
  Keyman.Developer.System.Project.XmlLdmlProjectFile,
  Keyman.Developer.System.TikeCommandLine,
  Keyman.Developer.UI.Project.ProjectFileUI,
  Keyman.Developer.UI.Project.ProjectUI,
  Keyman.Developer.UI.UfrmLdmlKeyboardEditor,
  Keyman.Developer.UI.UfrmWordlistEditor,
  Keyman.Developer.UI.UfrmModelEditor,
  TextFileFormat,
  RedistFiles,
  ErrorControlledRegistry,
  RegistryKeys,
  Sentry.Client,
  TikeUnicodeData,
  UfrmCharacterMapDock,
  UfrmMessages,
  UfrmNew,
  UfrmAboutTike,
  UfrmOSKEditor,
  UfrmSelectSystemKeyboard,
  UfrmOptions,
  UfrmKeyTest, UfrmKeymanWizard,
  UfrmPackageEditor, UfrmEditor, UfrmBitmapEditor,
  UfrmDebug, KeymanDeveloperOptions, utilfiletypes,
  UfrmHelp, dmActionsTextEditor, UfrmDebugStatus,
  UfrmCharacterIdentifier, UfrmCharacterMapNew,
  UmodWebHttpServer;

{$R *.DFM}

{-------------------------------------------------------------------------------
 - Form events and messages                                                    -
 -------------------------------------------------------------------------------}

function CallWndProc_InputLangChange(nCode: Integer; wParam: WPARAM; lParam: LPARAM): LRESULT; stdcall;
begin
  if nCode >= 0 then
    with PCWPSTRUCT(lParam)^ do
      if message = WM_INPUTLANGCHANGE then
        PostMessage(frmKeymanDeveloper.Handle, WM_USER_INPUTLANGCHANGE, wParam, lParam);
  if frmKeymanDeveloper <> nil then
    Result := CallNextHookEx(frmKeymanDeveloper.hInputLangChangeHook, nCode, wParam, lParam)
  else
    Result := 0;
end;


procedure TfrmKeymanDeveloper.FormCreate(Sender: TObject);
begin
  inherited;

  FFilesToOpen := TStringList.Create;

  try
    if not ForceDirectories(FKeymanDeveloperOptions.DefaultProjectPath) then
    begin
      // Fall back to Documents folder if we cannot create the default project path
      // Documents folder should always exist
      FKeymanDeveloperOptions.DefaultProjectPath := GetFolderPath(CSIDL_PERSONAL);
    end;
  except
    on E:EInOutError do
    begin
      // If the DefaultProjectPath is a relative, invalid path, then it may
      // cause EInOutError (#11554). Note that this exception should no longer
      // be possible because the path is sanitized when loaded in
      // KeymanDeveloperOptions.pas, but keeping this fallback just in case.
      FKeymanDeveloperOptions.DefaultProjectPath := GetFolderPath(CSIDL_PERSONAL);
    end;
  end;

  FFirstShow := True;

  hInputLangChangeHook := SetWindowsHookEx(WH_CALLWNDPROC, CallWndProc_InputLangChange, 0, GetCurrentThreadId);

  modActionsTextEditor := TmodActionsTextEditor.Create(Self);
  modActionsKeyboardEditor := TmodActionsKeyboardEditor.Create(Self);
  modActionsDebugger := TmodActionsDebugger.Create(Self);
  modActionsMain := TmodActionsMain.Create(Self);
  modActionsModelEditor := TmodActionsModelEditor.Create(Self);

  FChildWindows := TChildWindowList.Create;

  FProjectMRU := TMRUList.Create('Project');
  FProjectMRU.OnChange := ProjectMRUChange;
  FProjectMRU.Load;

  ShowDebug(False);

  CreateTikeUnicodeData(Self);   // I4257

  //CreateKeymanDeveloperOptions;
  FCharMapSettings := TCharMapSettings.Create;

  Application.HelpFile := GetHelpURL;   // I4677   // I4841
  mHHelp := TWebHookHelpSystem.Create(Application.HelpFile);   // I4677   // I4841

  if not FKeymanDeveloperOptions.ToolbarVisible then
    barTools.Visible := False;

  RemoveOldestTikeEditFonts(False);
  RemoveOldestTikeTestFonts(False);

  if TikeCommandLine.StartupProjectPath <> '' then
  begin
    try
      LoadGlobalProjectUI(ptUnknown, TikeCommandLine.StartupProjectPath);
    except
      on E:EProjectLoader do
      begin
        // Message will be displayed by LoadGlobalProjectUI
        FreeGlobalProjectUI;
      end;
    end;
  end;

  UpdateCaption;

  InitDock;

  frmMessages := TfrmMessages.Create(Self);

  frmCharacterMapDock := TfrmCharacterMapDock.Create(Self);

  frmCharacterIdentifier := TfrmCharacterIdentifier.Create(Self);   // I4807
  frmCharacterIdentifier.OnCancelFocus := CharMapCancelFocus;

  frmHelp := TfrmHelp.Create(Application);

  Application.OnActivate := AppOnActivate;

  LoadDockLayout;
  Invalidate;

  UpdateCaption;
end;

procedure TfrmKeymanDeveloper.CreateWnd;
begin
  inherited;
  FDropTarget := TDropTarget.Create(WindowHandle, Self);
end;

procedure TfrmKeymanDeveloper.DestroyWnd;
begin
  FreeAndNil(FDropTarget);
  inherited;
end;

procedure TfrmKeymanDeveloper.CharMapInsertCode(Sender: TObject; Control: TWinControl; DragObject: TCharacterDragObject);
begin
  GetCharMapDropTool.InsertToControl(Control, DragObject);
end;

procedure TfrmKeymanDeveloper.CharMapCanInsertCode(Sender: TObject; Control: TWinControl; var Result: Boolean);
begin
  Result := Assigned(Control) and (GetCharMapDropTool.Controls.IndexOfControl(Control) >= 0);
end;

procedure TfrmKeymanDeveloper.CharMapCancelFocus(Sender: TObject);
begin
  if Assigned(ActiveChild) then ActiveChild.SetFocus;
end;

procedure TfrmKeymanDeveloper.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  DoCloseCleanup;
  Action := caFree;
end;

procedure TfrmKeymanDeveloper.DoCloseCleanup;
var
  i: Integer;
begin
  if FHasDoneCloseCleanup then
    Exit;
  FHasDoneCloseCleanup := True;

  for i := FChildWindows.Count - 1 downto 0 do
  begin
    FChildWindows[i].Visible := False;
    FChildWindows[i].Parent := nil;
    FChildWindows[i].Release;   // I2595, probably not necessary
  end;

  if IsGlobalProjectUIReady then
  begin
    FGlobalProject.Save;   // I4691
    FKeymanDeveloperOptions.StartupProjectPath := FGlobalProject.FileName;
  end
  else
  begin
    FKeymanDeveloperOptions.StartupProjectPath := '';
  end;
  FKeymanDeveloperOptions.Write;

  FreeAndNil(frmCharacterMapDock);
  FreeAndNil(frmCharacterIdentifier);   // I4807
  FreeAndNil(frmMessages);
  FreeAndNil(frmHelp);
//  FreeAndNil(frmDebugStatus);
end;

procedure TfrmKeymanDeveloper.FormCloseQuery(Sender: TObject;
  var CanClose: Boolean);
var
  i: Integer;
begin
//  OutputDebugString(PChar('TfrmKeymanDeveloper.FormCloseQuery: FIsClosing='+BoolToStr(FIsClosing)+' FCanClose='+BoolToStr(FCanClose)));
  if not FIsClosing then
  begin
    // I944 - Fix crash when FChildWindows is nil on closing Keyman Developer
    if Assigned(FChildWindows) then
    begin
      for i := 0 to FChildWindows.Count - 1 do
        if not FChildWindows[i].CloseQuery then
        begin
          CanClose := False;
          Exit;
        end;
    end;

    FIsClosing := True;
    SaveDockLayout;

    CanClose := FInitializeCEF.StartShutdown(CEFShutdownComplete);
    // TODO: complete exit after StartClose is successful
  end
  else
    CanClose := FCanClose;
end;

procedure TfrmKeymanDeveloper.CEFShutdownComplete(Sender: TObject);
begin
//  OutputDebugString(PChar('TfrmKeymanDeveloper.CEFShutdownComplete'));
  FCanClose := True;
  Close;
end;

procedure TfrmKeymanDeveloper.FormDestroy(Sender: TObject);
begin
  DoCloseCleanup;

  //  OutputDebugString(PChar('TfrmKeymanDeveloper.FormDestroy'));
  UnhookWindowsHookEx(hInputLangChangeHook);

  FreeAndNil(FCharMapSettings);
  Application.OnActivate := nil;

  FreeGlobalProjectUI;
  FreeAndNil(FChildWindows);
  FreeAndNil(FProjectMRU);

  ClearSystemKeyboardList(cbDebugSystemKeyboard.Items);

  FreeAndNil(mHHelp);   // I4677

  FreeUnicodeData;

  FreeAndNil(AppStorage);

  FreeAndNil(FFilesToOpen);
  FreeAndNil(FWindowProcesses);
end;

procedure TfrmKeymanDeveloper.FormShow(Sender: TObject);
begin
  if FFirstShow then PostMessage(Handle, WM_USER_FORMSHOWN, 0, 0);
  FFirstShow := False;
end;

procedure TfrmKeymanDeveloper.WMActivate(var Message: TWMActivate);
begin
  try   // I4352   // I4404   // I4485
    inherited;
  except
    on E:EInvalidOperation do
      ; // This happens on shutdown sometimes due to
        // annoying sequence of operations in Delphi's focus management.
        // It's easier to do this, and relatively side-effect-free, so
        // happy to live with this...  I4352, I4404, I4485
  end;
end;

procedure TfrmKeymanDeveloper.WMCopyData(var Message: TWMCopyData);
var
  id: TCopyDataCommand;
  filename: string;
begin
  if not TCopyDataHelper.ReceiveData(Message, id, filename) then
  begin
    // Invalid data, we'll ignore the message
    Message.Result := ERROR_INVALID_FUNCTION;
    Exit;
  end;

  if id <> TCopyDataCommand.CD_OPENFILE then
  begin
    Message.Result := ERROR_INVALID_FUNCTION;
  end
  else if not FileExists(filename) then
  begin
    Message.Result := ERROR_FILE_NOT_FOUND;
  end
  else
  begin
    FFilesToOpen.Add(filename);
    PostMessage(Handle, WM_USER_OpenFiles, 0, 0);
    Message.Result := 0;
  end;
end;

procedure TfrmKeymanDeveloper.WMUserFormShown(var Message: TMessage);
var
  filename: string;
begin
  if Length(TikeCommandLine.StartupFilenames) = 0 then
  begin
    ShowProject;
  end
  else
  begin
    for filename in TikeCommandLine.StartupFilenames do
      if FileExists(filename) then
        OpenFile(filename, False);
  end;

  if True then //FKeymanDeveloperOptions.AutoCheckForUpdates then
  begin
    with TOnlineUpdateCheck.Create(SRegKey_KeymanDeveloper_CU, False, True, True, GetProxySettings.Server, GetProxySettings.Port, GetProxySettings.Username, GetProxySettings.Password) do  // I3377
      Run;
  end;
end;

procedure TfrmKeymanDeveloper.WMUserInputLangChange(var Message: TMessage);
var
  i: Integer;
begin
  for i := 0 to FChildWindows.Count - 1 do
    PostMessage(FChildWindows[i].Handle, WM_USER_INPUTLANGCHANGE, Message.wParam, Message.LParam);
end;

procedure TfrmKeymanDeveloper.WMUserOpenFiles(var Message: TMessage);
var
  filename: string;
begin
  for filename in FFilesToOpen do
  begin
    OpenFile(filename, False);
  end;
  FFilesToOpen.Clear;
end;

const
  SPI_GETKEYBOARDCUES = $100A;
var
  FAlwaysDrawUnderline: Boolean = False;
  FWndProcInit: Boolean = False;

procedure TfrmKeymanDeveloper.WndProc(var Message: TMessage);
begin
  case Message.Msg of
    WM_DRAWITEM:
      begin
        if not FWndProcInit then
        begin
        	SystemParametersInfo(SPI_GETKEYBOARDCUES, 0, @FAlwaysDrawUnderline, 0);
          FWndProcInit := True;
        end;

        if FAlwaysDrawUnderline then
          with PDrawItemStruct(Message.lParam)^ do
            if (CtlType = ODT_MENU) and Assigned(Menu) then
              itemState := itemState and $FEFF;
      end;
  end;
  inherited;
end;

function TfrmKeymanDeveloper.ApplicationEvents1Help(Command: Word;
  Data: NativeInt; var CallHelp: Boolean): Boolean;   // I4677
var
  s: string;
  frm: TCustomForm;
begin
  CallHelp := False;
  if Command = HELP_COMMAND then   // I4677
  begin
    if not FInOnHelp then
    begin
      FInOnHelp := True;
      try
        s := PChar(Data);
        if s = '' then
        begin
          frm := GetParentForm(Screen.ActiveControl, False);
          if frm is TTikeForm then
            s := (frm as TTikeForm).HelpTopic;
        end;
        if s <> '' then
          mHHelp.HelpTopic(s);
      finally
        FInOnHelp := False;
      end;
    end
    else
      mHHelp.HelpTopic(s);
  end;
  Result := True;
end;

procedure TfrmKeymanDeveloper.ApplicationEvents1Message(var Msg: tagMSG;
  var Handled: Boolean);
var
  state: Boolean;
begin
  Handled := False;
  if (Msg.message = WM_KEYDOWN) or (Msg.message = WM_SYSKEYDOWN) or
    (Msg.message = WM_KEYUP) or (Msg.message = WM_SYSKEYUP) then
  begin
    state := (Msg.message = WM_KEYDOWN) and (Msg.wParam = VK_CONTROL) and
      (GetKeyState(VK_SHIFT) >= 0) and (GetKeyState(VK_MENU) >= 0);
    if not state and FControlDown and Assigned(ActiveEditor) and (Msg.wParam = VK_CONTROL) then
    begin
      ActiveEditor.ControlKeyPressedAndReleased;
    end;
    FControlDown := state;
  end;
end;

procedure TfrmKeymanDeveloper.AppOnActivate(Sender: TObject);
var
  i: Integer;
begin
  FWndProcInit := False;

  RefreshProjectMRU;

  for i := 0 to FChildWindows.Count - 1 do
    if FChildWindows[i] is TfrmTikeEditor then
      with FChildWindows[i] as TfrmTikeEditor do
        CheckForReload;

  try
    FocusActiveChild;   // I4847   // I2986
  except
    on E:EInvalidOperation do ; // We don't want to crash if focus fails, e.g. on shutdown
  end;
end;

{-------------------------------------------------------------------------------
 - File menu                                                                   -
 -------------------------------------------------------------------------------}

procedure TfrmKeymanDeveloper.mnuDebugI374Click(Sender: TObject);
begin
  if Assigned(ActiveEditor)
    then frmMessages.Add(plsError, ActiveEditor.FileName, '1: bug with multi-line messages'#13#10'Double-click on the second line of this message', 0, 0)
    else frmMessages.Add(plsError, 'test.txt', '1: bug with multi-line messages'#13#10'Double-click on the second line of this message', 0, 0);
end;

procedure TfrmKeymanDeveloper.mnuDebugSetWindowSizeForScreenshotsClick(Sender: TObject);
begin
  Width := 960;
  Height := 600;
end;

procedure TfrmKeymanDeveloper.mnuFileClick(Sender: TObject);
begin
  UpdateFileMRU;
  mnuFileRecent.Enabled := mnuFileRecent.Count > 0;
end;

{-------------------------------------------------------------------------------
 - View menu                                                                   -
 -------------------------------------------------------------------------------}

procedure TfrmKeymanDeveloper.ShowChild(Window: TfrmTikeChild);
var
  i, j: Integer;
begin
  for i := 0 to FChildWindows.Count - 1 do
    if FChildWindows[i] = Window then
    begin
      //Window.SetFocus;
      for j := 0 to pages.PageCount - 1 do
        if pages.Pages[j].Tag = Integer(Window) then
        begin
          pages.ActivePageIndex := j;
          Break;
        end;
    end;
    //else
      //FChildWindows[i].Visible := False;
end;

procedure TfrmKeymanDeveloper.ShowDebug(AShow: Boolean);
begin
  if ActiveEditor is TfrmKeymanWizard then
  begin
    (ActiveEditor as TfrmKeymanWizard).panDebugHost.Visible := AShow;
    if AShow then
      // Move the form below the splitter
      (ActiveEditor as TfrmKeymanWizard).panDebugHost.Top := ActiveEditor.ClientHeight;
  end
  else if ActiveEditor is TfrmLdmlKeyboardEditor then
  begin
    (ActiveEditor as TfrmLdmlKeyboardEditor).panDebugHost.Visible := AShow;
    if AShow then
      // Move the form below the splitter
      (ActiveEditor as TfrmLdmlKeyboardEditor).panDebugHost.Top := ActiveEditor.ClientHeight;
  end;
//  if Assigned(frmDebugStatus) then frmDebugStatus.Visible := AShow;
  panDebugToolbar.Visible := AShow;
end;

procedure TfrmKeymanDeveloper.ShowProject;
begin
  if ProjectForm = nil
    then ShowChild(TfrmProject.Create(Self))
    else ShowChild(ProjectForm);
  ProjectForm.SetGlobalProject;
end;

procedure TfrmKeymanDeveloper.ToggleProject;
var
  frmProject: TfrmProject;
begin
  frmProject := ProjectForm;
  if Assigned(frmProject) and (ActiveChild = frmProject)
    then PostMessage(frmProject.Handle, WM_CLOSE, 0, 0)
    else ShowProject;
end;

procedure TfrmKeymanDeveloper.InitDock;
begin
  AppStorage := TJvAppRegistryStorage.Create(self);
  AppStorage.Path := SRegKey_IDEDock_CU;
  AppStorage.AutoFlush := True;
  AppStorage.AutoReload := True;
end;

procedure TfrmKeymanDeveloper.SaveDockLayout;
begin
  //save
  AppStorage.BeginUpdate;
  try
    SaveDockTreeToAppStorage(AppStorage);
    { SaveDockTreeToAppStorage clears the storage, so we save the forms after the
      SaveDockTreeToAppStorage call }
  finally
    AppStorage.EndUpdate;
  end;
end;

procedure TfrmKeymanDeveloper.DefaultDockLayout;
begin
  // Undock the forms first
  frmCharacterMapDock.ManualFloat(Rect(0, 0, Screen.Width div 6, Screen.Height div 2));
  frmCharacterIdentifier.ManualFloat(Rect(0, 0, Screen.Width div 7, Screen.Height div 3));
  frmHelp.ManualFloat(Rect(0, 0, Screen.Width div 6, Screen.Height div 6));
  frmMessages.ManualFloat(Rect(0, 0, Screen.Width * 2 div 3, Screen.Height div 6));

  // Character Map on Right, Top half
  frmCharacterMapDock.ManualDock(dockServer.RightDockPanel);

  // Character Identifier on Left
  frmCharacterIdentifier.ManualDock(dockServer.LeftDockPanel);

  // Help on Right, Bottom half
  frmHelp.ManualDock(dockServer.RightDockPanel, dockServer.RightDockPanel.Controls[0], alBottom);

  // Messages across bottom
  frmMessages.ManualDock(dockServer.BottomDockPanel);

  frmCharacterMapDock.Show;
  frmCharacterIdentifier.Show;
  frmHelp.Show;
  frmMessages.Show;

  frmHelp.Height := frmHelp.TBDockHeight;

  dockServer.RightDockPanel.ShowDockPanel(True, frmCharacterMapDock);
  dockServer.RightDockPanel.Width := Screen.Width div 6;

  dockServer.LeftDockPanel.ShowDockPanel(True, frmCharacterIdentifier);
  dockServer.LeftDockPanel.Width := Screen.Width div 7;

  dockServer.BottomDockPanel.ShowDockPanel(True, frmMessages);
  dockServer.BottomDockPanel.Height := Screen.Height div 6;

  // In future, we could consider resizing the Help window down, but
  // this appears non-trivial to accomplish with JvDocking. Not important
  // enough to chase right now.
end;

procedure TfrmKeymanDeveloper.LoadDockLayout;
begin
  DefaultDockLayout;

  with TRegistryErrorControlled.Create do
  try
    if OpenKeyReadOnly(SRegKey_IDEDock_CU) then
    begin
      BeginDockLoading;
      try
        AppStorage.BeginUpdate;
        try
          LoadDockTreeFromAppStorage(AppStorage);
        finally
          AppStorage.EndUpdate;
        end;
      finally
        EndDockLoading;
      end;
    end;
  finally
    Free;
  end;
end;

procedure TfrmKeymanDeveloper.cbDebugSystemKeyboardItemClick(Sender: TObject);
begin
  if cbDebugSystemKeyboard.ItemIndex < 0 then
    cbDebugSystemKeyboard.ItemIndex := 0;
  modActionsDebugger.SelectDebugSystemKeyboard(cbDebugSystemKeyboard.Items.Objects[cbDebugSystemKeyboard.ItemIndex] as TSystemKeyboardItem);
  //actDebugSystemKeyboard
end;

procedure TfrmKeymanDeveloper.cbDebugSystemKeyboard_DropDown(Sender: TObject);
begin
{  if SelectSystemKeyboard(frmKeymanDeveloper, FLoadedSystemKeyboard, FSystemKeyboardName) then
    UpdateSystemKeyboardCaption;
  if FDebugVisible then
    memo.SetFocus;
}
  if cbDebugSystemKeyboard.Items.Count < 2 then
  begin
    //modActionsKeyboardEditor.SelectedSystemKeyboard;
    FillSystemKeyboardList(cbDebugSystemKeyboard.Items);
  end;
  //modActionsKeyboardEditor.SetupDebugSystemKeyboard(cbDebugSystemKeyboard.Strings);
end;

{-------------------------------------------------------------------------------
 - Utility functions                                                           -
 -------------------------------------------------------------------------------}

function TfrmKeymanDeveloper.GetActiveChild: TfrmTikeChild;
var
  i: Integer;
begin
  i := ActiveChildIndex;
  if (i < 0) or not Assigned(FChildWindows) then Result := nil
  else Result := FChildWindows[i];
end;

function TfrmKeymanDeveloper.GetActiveChildIndex: Integer;
begin
  if Assigned(pages)                      // I1004: Crash when starting up on Vista with Matrox.PowerDesk.Hooks.dll (assumed)
    then Result := pages.ActivePageIndex
    else Result := -1;
end;

function TfrmKeymanDeveloper.GetActiveEditor: TfrmTikeEditor;
begin
  if Assigned(ActiveChild) and (ActiveChild is TfrmTikeEditor)
    then Result := ActiveChild as TfrmTikeEditor
    else Result := nil;
end;

function TfrmKeymanDeveloper.GetDropTargetIntf: IDropTarget;
begin
  if Assigned(FDropTarget)
    then Result := FDropTarget as IDropTarget
    else Result := nil;
end;

procedure TfrmKeymanDeveloper.GetTabOrderList(List: TList);
  function DoForm(Form: TForm): Boolean;
  begin
    if Assigned(Form) and Form.ContainsControl(Screen.ActiveControl) then
    begin
      Form.GetTabOrderList(List);
      Result := True;
    end
    else
      Result := False;
  end;
begin
  if not DoForm(ActiveChild) and
    not DoForm(frmMessages) and
    not DoForm(frmHelp) and
    not DoForm(frmCharacterMapDock) then
    DoForm(frmCharacterIdentifier);   // I4807
end;

procedure TfrmKeymanDeveloper.HelpTopic(Sender: TTIKEForm);
begin
  HelpTopic(Sender.HelpTopic);
end;

procedure TfrmKeymanDeveloper.ActivateActiveChild;
begin
  if Assigned(ActiveChild) then
    SendMessage(ActiveChild.Handle, CM_ACTIVATE, 0, 0);
end;

procedure TfrmKeymanDeveloper.pagesChange(Sender: TObject);
begin
  inherited;
  //CharacterMapFormChanged(ActiveChild);
  ActivateActiveChild;

  cbTextFileFormat.Enabled := Assigned(ActiveChild) and (ActiveChild is TfrmTikeEditor);   // I3733
  FocusActiveChild;
end;

procedure TfrmKeymanDeveloper.pagesCloseTab(Sender: TObject; Index: Integer);
begin
  PostMessage(TfrmTikeChild(pages.Pages[Index].Tag).Handle, WM_CLOSE, 0, 0);
end;

procedure TfrmKeymanDeveloper.OpenFilesInProject(FFileNames: TArray<string>);
var
  filename: string;
  filenames: TStringList;
  projects: TLaunchProjects;
  p: TLaunchProject;
  FProcesses: TTikeProcessList;
begin
  if Length(FFileNames) = 0 then
    Exit;

  // Uses the TTikeCommandLine pattern to open the resulting file either locally
  // or in a remote process
  FProcesses := TikeMultiProcess.Enumerate;
  projects := TLaunchProjects.Create(False);
  filenames := TStringList.Create;
  try
    filenames.AddStrings(FFileNames);
    FFileNames := [];

    // Sort filenames into project groupings
    projects.GroupFilenamesIntoProjects(filenames);

    // Look for the current instance's project in the list of projects, and
    // capture its list of filenames separately
    if IsGlobalProjectUIReady then
    begin
      for p in projects do
      begin
        if SameFileName(p.ProjectFilename, FGlobalProject.FileName) then
        begin
          FFileNames := p.Filenames.ToStringArray;
          projects.Remove(p);
          Break;
        end;
      end;
    end;

    // Launch new processes or load files into existing processes
    projects.LaunchAll(FProcesses);
  finally
    filenames.Free;
    projects.Free;
    FProcesses.Free;
  end;

  // Finally, open the remaining files which belong to this instance's project
  for filename in FFileNames do
    OpenFile(filename, True);
end;

function TfrmKeymanDeveloper.OpenFile(FFileName: string; FCloseNewFile: Boolean): TfrmTikeChild;
  function FileHasModelTsExt(Filename: string): Boolean;
  begin
    // We cannot use ExtractFileExt because of the two-part extension
    Result := Filename.ToLower.EndsWith('.model.ts');
  end;
var
  ext: string;
begin
  Result := nil;

  if not IsProjectFile(FFileName) then
  begin
    if not IsGlobalProjectUIReady then
    begin
      // We need to create a temporary project to host this file
      // This can happen if we get a file opened via Explorer without a
      // corresonding project file
      CreateTempGlobalProjectUI(ptUnknown);
      UpdateCaption;
    end;

    if GetGlobalProjectUI.IsTemporary then
    begin
      if FGlobalProject.Files.IndexOfFileName(FFileName) < 0 then
      begin
        CreateProjectFile(FGlobalProject, FFileName, nil);
      end;
    end;
  end;

  Screen.Cursor := crHourglass;
  try
    try
      ext := LowerCase(ExtractFileExt(FFileName));

      if ext = Ext_ProjectSource then
      begin
        if IsGlobalProjectUIReady and SameFileName(GetGlobalProjectUI.FileName, FFileName) then
        begin
          ShowProject;
          Exit;
        end;

        if BeforeOpenProject then
          OpenProjectInCurrentProcess(FFileName);
      end
      else
      begin
        FFileName := ExpandUNCFileName(FFileName);

        if FCloseNewFile then
          if Assigned(ActiveEditor) then
            if not ActiveEditor.Modified and ActiveEditor.Untitled then
              ActiveEditor.Free;

        if ext = '.kmn' then Result := OpenKMNEditor(FFileName)
        else if ext = '.kps'  then Result := OpenKPSEditor(FFileName)
        else if ext = '.kvk'  then Result := OpenKVKEditor(FFileName)
        else if ext = '.kvks' then Result := OpenKVKEditor(FFileName)
        else if ext = '.bmp'  then Result := OpenEditor(FFileName, TfrmBitmapEditor)
        else if ext = '.tsv'  then Result := OpenTSVEditor(FFileName)
        else if (ext = '.xml') and TxmlLdmlProjectFile.IsFileTypeSupported(FFileName) then
          Result := OpenEditor(FFileName, TfrmLdmlKeyboardEditor)
        else if FileHasModelTsExt(FFileName) then Result := OpenModelEditor(FFileName)
        else                       Result := OpenEditor(FFileName, TfrmEditor);

        if Assigned(Result) then
          AddMRU(FFileName);
      end;

    except
      on E:EFOpenError do
        ShowMessage('Unable to open the file '''+FFileName+'''.');
    end;

    pagesChange(pages);
  finally
    Screen.Cursor := crDefault;
  end;
end;

procedure TfrmKeymanDeveloper.OpenProjectInCurrentProcess(FileName: WideString);
begin
  FileName := ExpandUNCFileName(FileName);
  if (FileName <> '') and not FileExists(FileName) then
  begin
    ShowMessage('The project '+FileName+' does not exist.');
    Exit;
  end;

  if IsGlobalProjectUIReady then
  begin
    if not SaveAndCloseAllFiles then Exit;
    FreeGlobalProjectUI;
  end;

  try
    LoadGlobalProjectUI(ptUnknown, FileName);   // I4687
  except
    on E:EProjectLoader do
    begin
      // Message will be displayed by LoadGlobalProjectUI
      FreeGlobalProjectUI;
      ShowProject;
      UpdateCaption;
      Exit;
    end;
  end;
  ProjectMRU.Add(FGlobalProject.FileName);
  ShowProject;
  UpdateCaption;
end;

function TfrmKeymanDeveloper.OpenKMNEditor(FFileName: string): TfrmTikeEditor;
begin
  Result := OpenEditor(FFileName, TfrmKeymanWizard);
end;

function TfrmKeymanDeveloper.OpenTSVEditor(FFileName: string): TfrmTikeEditor;
begin
  Result := OpenEditor(FFileName, TfrmWordlistEditor);
end;

function TfrmKeymanDeveloper.OpenModelEditor(FFileName: string): TfrmTikeEditor;
begin
  Result := OpenEditor(FFileName, TfrmModelEditor);
end;

procedure TfrmKeymanDeveloper.OpenNewWindow;
begin
  if not TLaunchProjects.LaunchNewEmptyInstance then
    ShowMessage('Unable to launch new instance: '+SysErrorMessage(GetLastError));
end;

function TfrmKeymanDeveloper.OpenKPSEditor(FFileName: string): TfrmTikeEditor;
begin
  Result := OpenEditor(FFileName, TfrmPackageEditor);
end;

function TfrmKeymanDeveloper.OpenKVKEditor(FFileName: string): TfrmTikeEditor;
begin
  Result := OpenEditor(FFileName, TfrmOSKEditor);
end;

function TfrmKeymanDeveloper.SaveAndCloseAllFiles: Boolean;
var
  i: Integer;
begin
  FGlobalProject.Save;
  for i := 0 to FChildWindows.Count - 1 do
  begin
    if FChildWindows[i] is TfrmProject then
      Continue;

    if not FChildWindows[i].CloseQuery then
      Exit(False);
  end;

  for i := 0 to FChildWindows.Count - 1 do
  begin
    if FChildWindows[i] is TfrmProject then
      Continue;

    FChildWindows[i].Visible := False;
    FChildWindows[i].Parent := nil;
    FChildWindows[i].Release;
  end;

  Result := True;
end;

function TfrmKeymanDeveloper.BeforeOpenProject: Boolean;
var
  i: Integer;
begin
  for i := 0 to FChildWindows.Count - 1 do
    if not FChildWindows[i].CloseQuery then
      Exit(False);

  Result := True;
end;

function TfrmKeymanDeveloper.OpenEditor(FFileName: string; frmClass: TfrmTikeEditorClass): TfrmTikeEditor;
var
  i, n: Integer;
begin
  for i := 0 to FChildWindows.Count - 1 do
    if FChildWindows[i] is TfrmTikeEditor then
      with FChildWindows[i] as TfrmTikeEditor do
        if SameFileName(FileName, FFileName) or HasSubFilename(FFileName) then   // I4749
        begin
          Result := Self.FChildWindows[i] as TfrmTikeEditor;
          ShowChild(Result);
          Exit;
        end;

  LockWindowUpdate(pages.Handle);
  Result := frmClass.Create(Self);

  n := FGlobalProject.Files.IndexOfFileName(FFileName);
  if n >= 0 then
    Result.ProjectFile := FGlobalProject.Files[n];

  LockWindowUpdate(0);

  if not (Result as frmClass).OpenFile(FFileName) then
  begin
    Result.Release;
  end;
end;

procedure TfrmKeymanDeveloper.HelpTopic(s: string);
begin
  Application.HelpJump(s);
end;

{-------------------------------------------------------------------------------
 - Some functions                                                              -
 -------------------------------------------------------------------------------}

function TfrmKeymanDeveloper.FindEditorByFileName(AFileName: string): TfrmTikeEditor;
var
  i: Integer;
begin
  for i := 0 to FChildWindows.Count - 1 do
  begin
    if (FChildWindows[i] is TfrmTikeEditor) then   // I4081
    begin
      if ((FChildWindows[i] as TfrmTikeEditor).FileName = AFileName) or
        (FChildWindows[i] as TfrmTikeEditor).HasSubFilename(AFilename) then
      begin
        Result := FChildWindows[i] as TfrmTikeEditor;
        Exit;
      end;
    end;
  end;

  Result := nil;
end;

{-------------------------------------------------------------------------------
 - MRU File List                                                               -
 -------------------------------------------------------------------------------}

procedure TfrmKeymanDeveloper.AddMRU(FileName: string);
begin
  // Add the filename to the top of the MRU.

  if (FileName = '') or not IsGlobalProjectUIReady then Exit;

  if LowerCase(ExtractFileExt(FileName)) <> '.kpj' then
    // Don't add project files to project MRU
    FGlobalProject.MRU.Add(FileName);
end;

procedure TfrmKeymanDeveloper.UpdateFileMRU;
var
  i: Integer;
  m: TMenuItem;
begin
  for i := 0 to mnuFileRecent.Count - 1 do
    mnuFileRecent.Items[0].Free;

  if IsGlobalProjectUIReady then
  begin
    for i := 0 to FGlobalProject.MRU.FileCount - 1 do
    begin
      m := TMenuItem.Create(Self);
      m.Caption := '&'+IntToStr(i+1)+' '+FGlobalProject.MRU.EllipsisFile(i);
      m.Tag := i;
      m.OnClick := mnuFileRecentFileClick;
      mnuFileRecent.Add(m);
    end;
  end;
end;

procedure TfrmKeymanDeveloper.mnuFileRecentFileClick(Sender: TObject);
var
  filename: string;
begin
  filename := FGlobalProject.MRU.Files[(Sender as TMenuItem).Tag];
  OpenFilesInProject([filename]);
end;

{-------------------------------------------------------------------------------
 - Drag and Drop functionality                                                 -
 -------------------------------------------------------------------------------}

procedure TfrmKeymanDeveloper.Drop(const FileNames: TArray<string>);
begin
  OpenFilesInProject(FileNames);
end;

function TfrmKeymanDeveloper.DropAllowed(const FileNames: TArray<string>): Boolean;
begin
  Result := True;
end;

{-------------------------------------------------------------------------------
 - TIKE projects                                                               -
 -------------------------------------------------------------------------------}

procedure TfrmKeymanDeveloper.NotifyChildWindowChange(Window: TfrmTikeChild; Action: TListNotification);
var
  item: TTabSheet;
  i: Integer;
begin
  case Action of
    lnAdded:
      begin
        item := TTabSheet.Create(Self);
        item.Caption := Window.Caption+'      '; //enough space for [x] close button
        if Window is TfrmTikeEditor then
        begin
          item.Hint := (Window as TfrmTikeEditor).FileName;
        end;
        item.Tag := Integer(Window);
        item.PageControl := pages;
        Window.Parent := item;
      end;
    lnDeleted,lnExtracted:
      begin
        for i := 0 to pages.PageCount - 1 do
          if pages.Pages[i].Tag = Integer(Window) then
          begin
            //CharacterMapFormChanged(nil);
            Window.Parent := nil;
            pages.Pages[i].Free;
            ActivateActiveChild;
            FocusActiveChild;

            if FIsClosing then
              if pages.PageCount = 0 then
              begin
                Close;
              end;
            Exit;
          end;
      end;
  end;
end;

procedure TfrmKeymanDeveloper.mnuToolsClick(Sender: TObject);
begin
  mnuToolsDebugTests.Visible := (GetKeyState(VK_CONTROL) < 0) and (GetKeyState(VK_SHIFT) < 0);
  mnuToolsDebugTestsShowDebuggerEventsPanel.Checked := TfrmDebugStatus.ShowDebuggerEventsPanel;
  mnuToolsWebDebugger.Visible := FKeymanDeveloperOptions.ServerUseNgrok;
end;

procedure TfrmKeymanDeveloper.mnuProjectClick(Sender: TObject);
var
  i: Integer;
  m: TMenuItem;
begin
  for i := 0 to mnuProjectsRecent.Count - 1 do
    mnuProjectsRecent.Items[0].Free;

  for i := 0 to FProjectMRU.FileCount - 1 do
  begin
    m := TMenuItem.Create(Self);
    m.Caption := '&'+IntToStr(i+1)+' '+FProjectMRU.EllipsisFile(i);
    m.Tag := i;
    m.OnClick := mnuProjectRecentFileClick;
    mnuProjectsRecent.Add(m);
  end;

  mnuProjectsRecent.Enabled := FProjectMRU.FileCount > 0;

  mnuProjectAddToProject.Visible := not IsGlobalProjectUIReady or
    (FGlobalProject.Options.Version = pv10);
end;

procedure TfrmKeymanDeveloper.mnuProjectRecentFileClick(Sender: TObject);
var
  filename: string;
begin
  filename := FProjectMRU.Files[(Sender as TMenuItem).Tag];
  OpenProject(filename);
end;

procedure TfrmKeymanDeveloper.OpenProject(const filename: string);
begin
  if IsGlobalProjectUIReady then
  begin
    // If we already have a project open, then we'll start a new instance
    OpenFilesInProject([filename]);
  end
  else if BeforeOpenProject then
  begin
    // No project is open, so we'll reuse the current instance
    OpenProjectInCurrentProcess(filename);
  end;
end;

procedure TfrmKeymanDeveloper.cbTextFileFormatItemClick(Sender: TObject);
begin
  modActionsMain.actToolsFileFormat.Execute;
end;

procedure TfrmKeymanDeveloper.mnuToolsDebugTestsExceptionTestClick(Sender: TObject);
begin
  TKeymanSentryClient.Validate(True);
end;

procedure TfrmKeymanDeveloper.mnuToolsDebugTestsShowDebuggerEventsPanelClick(
  Sender: TObject);
begin
  TfrmDebugStatus.ShowDebuggerEventsPanel := not TfrmDebugStatus.ShowDebuggerEventsPanel;
end;

procedure TfrmKeymanDeveloper.mnuWindowClick(Sender: TObject);
var
  m: TMenuItem;
  i: Integer;
  p: TTikeProcess;
begin
  // Remove existing items from the menu
  for i := mnuWindow.Count - 1 downto 2 do
    mnuWindow.Items[i].Free;

  FreeAndNil(FWindowProcesses);

  FWindowProcesses := TikeMultiProcess.Enumerate;
  i := 0;
  for p in FWindowProcesses do
  begin
    m := TMenuItem.Create(Self);
    m.Caption := '&'+IntToStr(i+1)+' '+EllipsisFile(p.Title);
    m.Tag := i;
    m.OnClick := mnuWindowSelectClick;
    m.Checked := p.ThreadId = GetCurrentThreadId;
    mnuWindow.Add(m);
    Inc(i);
  end;
end;

procedure TfrmKeymanDeveloper.mnuWindowSelectClick(Sender: TObject);
begin
  FWindowProcesses[(Sender as TMenuItem).Tag].FocusProcess;
end;

procedure TfrmKeymanDeveloper.SetActiveChild(const Value: TfrmTikeChild);
begin
  ShowChild(Value);
end;

procedure TfrmKeymanDeveloper.SetActiveChildIndex(const Value: Integer);
begin
  ShowChild(ChildWindows[Value]);
end;

procedure TfrmKeymanDeveloper.FocusActiveChild;
var
  PrevFocus: TfrmTikeChild;
begin
  PrevFocus := ActiveChild;
  if Assigned(PrevFocus) then
  begin
    ShowChild(PrevFocus);
    if PrevFocus.Visible then
      PrevFocus.SetFocus;
  end;

  modActionsKeyboardEditor.actKeyboardCompile.Update;
  modActionsModelEditor.actModelCompile.Update;
end;

procedure TfrmKeymanDeveloper.UDUI_Error(Sender: TUnicodeData;
  Error: TUnicodeDataError; const Details: WideString);
begin
  case Error of
    udeCouldNotDeleteDatabaseForRebuild: ShowMessage('Could not delete the character map database prior to rebuild.  The error was: '+Details);
    udeCouldNotCreateDatabase: ShowMessage('Could not create the character map database. The error was: '+Details);
  end;
end;

function TfrmKeymanDeveloper.UDUI_ShouldStartRebuildOnError(
  const Msg: WideString): Boolean;
begin
  Result := MessageDlg('The Unicode character database did not load successfully ('+Msg+').  Rebuild it now?', mtConfirmation, mbOkCancel, 0) = mrOk;
end;

function TfrmKeymanDeveloper.UDUI_StartRebuild(Callback: TNotifyEvent; AskFirst: Boolean): Boolean;
begin
  Result := False;

  if AskFirst and (MessageDlg('The Character Map has a database of characters that needs to be built before it can be used.  Build it now?',
    mtConfirmation, mbOkCancel, 0) = mrCancel) then Exit;
  FUnicodeDataStatusForm := TfrmUnicodeDataStatus.Create(Self);
  try
    FUnicodeDataStatusForm.Callback := Callback;
    Result := FUnicodeDataStatusForm.ShowModal = mrOk;
  finally
    FreeAndNil(FUnicodeDataStatusForm);
  end;
end;

procedure TfrmKeymanDeveloper.UDUI_UpdateStatus(const Msg: WideString; Pos,
  Max: Integer);
begin
  if Assigned(FUnicodeDataStatusForm) then
    FUnicodeDataStatusForm.UpdateStatus(Msg, Pos, Max);
end;

procedure TfrmKeymanDeveloper.UpdateCaption;
const
  C_StandardCaption = 'Keyman Developer';
  C_TemporaryProject = 'Temporary Project';
begin
  if not IsGlobalProjectUIReady then
    Caption := C_StandardCaption
  else if GetGlobalProjectUI.IsTemporary then
    Caption := C_TemporaryProject+' - '+C_StandardCaption
  else
    Caption := ChangeFileExt(ExtractFileName(FGlobalProject.FileName), '')+' - '+C_StandardCaption;
end;

procedure TfrmKeymanDeveloper.UpdateChildCaption(Window: TfrmTikeChild);
var
  i: Integer;
begin
  for i := 0 to pages.PageCount - 1 do
    if pages.Pages[i].Tag = Integer(Window) then
      with pages.Pages[i] do
      begin
        Caption := Window.Caption+'      '; //enough space for [x] close button;
        Hint := Window.Hint;
        Exit;
      end;
end;

procedure TfrmKeymanDeveloper.RefreshOptions;
var
  i: Integer;
begin
  Screen.Cursor := crHourglass;
  try
    FUnicodeData.RefreshOptions;
    for i := 0 to Self.FChildWindows.Count - 1 do
      if Self.FChildWindows[i] is TfrmEditor then (Self.FChildWindows[i] as TfrmEditor).RefreshOptions;
    if ProjectForm <> nil then ProjectForm.RefreshOptions;
    if Assigned(frmMessages) then frmMessages.RefreshOptions;
    if Assigned(frmCharacterMapDock) then frmCharacterMapDock.RefreshOptions; // Recalculates grid after unicode data refresh
//    if Assigned(modWebHttpServer) then modWebHttpServer.RestartServer;
  finally
    Screen.Cursor := crDefault;
  end;
end;

procedure TfrmKeymanDeveloper.RefreshProjectMRU;
begin
  FProjectMRU.Load;
end;

function TfrmKeymanDeveloper.ProjectForm: TfrmProject;
var
  i: Integer;
begin
  for i := 0 to FChildWindows.Count - 1 do
    if (FChildWindows[i] is TfrmProject) then
    begin
      Result := FChildWindows[i] as TfrmProject;
      Exit;
    end;

  Result := nil;
end;

procedure TfrmKeymanDeveloper.ProjectMRUChange(Sender: TObject);
begin
  FProjectMRU.SaveListToXML(TWelcomeRenderer.ProjectMRUFilename);

  // Tell project window to refresh!
  if ProjectForm <> nil then
    ProjectForm.SetGlobalProject;
end;

end.

