(*
  Name:             UfrmKeymanWizard
  Copyright:        Copyright (C) SIL International.
  Documentation:
  Description:
  Create Date:      20 Jun 2006

  Modified Date:    23 Feb 2016
  Authors:          mcdurdin
  Related Files:
  Dependencies:

  Bugs:
  Todo:
  Notes:
  History:          20 Jun 2006 - mcdurdin - Initial version
                    01 Aug 2006 - mcdurdin - Rework for Keyman 7 part 1
                    02 Aug 2006 - mcdurdin - Rework menus as sp-TBX
                    23 Aug 2006 - mcdurdin - Rework to integrate source editor
                    23 Aug 2006 - mcdurdin - Redesign pages with new Unicode UI
                    23 Aug 2006 - mcdurdin - Integrate visual keyboard editor
                    23 Aug 2006 - mcdurdin - Remember format and display settings
                    23 Aug 2006 - mcdurdin - Integrate debugger
                    23 Aug 2006 - mcdurdin - Get file encoding combo working
                    30 Aug 2006 - mcdurdin - Fix bug where icon and visual keyboard changes were lost on moving out of source tab
                    14 Sep 2006 - mcdurdin - Polish icon editor frame
                    14 Sep 2006 - mcdurdin - Tweak system keyboard selection
                    28 Sep 2006 - mcdurdin - Added insert copyright button
                    28 Sep 2006 - mcdurdin - Added Open Containing Folder button
                    28 Sep 2006 - mcdurdin - Fixed character drag and drop
                    06 Oct 2006 - mcdurdin - Add test KeymanWeb button
                    06 Oct 2006 - mcdurdin - Warn if keyboard is being upgraded to version 7.0
                    04 Dec 2006 - mcdurdin - Move OSK to separate frame
                    04 Dec 2006 - mcdurdin - Warn if upgrading; center layout complex panel;
                    04 Dec 2006 - mcdurdin - Polish import of KMX to KVK
                    12 Dec 2006 - mcdurdin - Support Print Preview and Print
                    04 Jan 2007 - mcdurdin - Add help support
                    04 Jan 2007 - mcdurdin - Fix error line references
                    15 Jan 2007 - mcdurdin - Use correct key cap when no character on selected shift state
                    22 Jan 2007 - mcdurdin - Fix default font for debugger
                    25 Jan 2007 - mcdurdin - Fix crash when control is not enabled in wizard page change
                    19 Mar 2007 - mcdurdin - I712 - Character map should follow text in OSK and keyboard editors
                    19 Mar 2007 - mcdurdin - I710 - Include OSK checkbox now sets filename correctly always
                    19 Mar 2007 - mcdurdin - I693 - Keyboard parser appends .bmp to bitmap if no extension given
                    30 May 2007 - mcdurdin - Handle crashes when saving files
                    30 May 2007 - mcdurdin - Fix on screen keyboard not setting modify state correctly
                    13 Jul 2007 - mcdurdin - I905 - Stop keys moving around on the keyboard designer
                    23 Aug 2007 - mcdurdin - I1012 - Fix for loading UTF-16 files without BOM
                    23 Aug 2007 - mcdurdin - I1012 - Add a BOM when saving UTF-16 files
                    14 Sep 2007 - mcdurdin - I1059, I1012 - Fix crash when reloading a file and in Source tab
                    19 Nov 2007 - mcdurdin - I1157 - const string parameters
                    14 Jun 2008 - mcdurdin - Add Languages tab
                    14 Jun 2008 - mcdurdin - I1467 - Fixup source -> Layout tab differentiation of L/R alt
                    14 Jun 2008 - mcdurdin - I993 - Fix crash when pressing Ctrl+PgUp/Pgdn
                    28 Jul 2008 - mcdurdin - I1557 - Lookup language via tav.com
                    16 Jan 2009 - mcdurdin - Fix crash trying to focus KMW tab
                    16 Jan 2009 - mcdurdin - Support widestring names for keyboard icon and OSK
                    26 Jul 2010 - mcdurdin - I2468 - Eliminate KeymanWeb Pack
                    18 Mar 2011 - mcdurdin - I2584 - Pressing Ctrl to select a key no longer works
                    18 Mar 2011 - mcdurdin - I2532 - Show L/R alt checkbox is not working in Keyman Developer
                    18 Mar 2011 - mcdurdin - I1766 - Changing KVK filename does not appear to stick
                    25 Mar 2011 - mcdurdin - I2843 - Charmap search does not work for supp plane chars stored as chars
                    03 May 2011 - mcdurdin - I2890 - Record diagnostic data when encountering registry errors
                    18 May 2012 - mcdurdin - I3306 - V9.0 - Remove TntControls + Win9x support
                    18 May 2012 - mcdurdin - I3323 - V9.0 - Change from Plus-MemoU to Plus-Memo
                    08 Jun 2012 - mcdurdin - I3337 - V9.0 - Review of input/output for Unicode
                    08 Jun 2012 - mcdurdin - I3349 - V9.0 - Consolidate all process creation into TUtilExecute
                    26 Jun 2012 - mcdurdin - I3377 - KM9 - Update code references from 8.0 to 9.0
                    13 Dec 2012 - mcdurdin - I3637 - V9.0 - I3502 Fail - Reload as Format button is disabled in some contexts
                    11 Aug 2013 - mcdurdin - I3885 - V9.0 - Touch Layout Editor
                    15 Oct 2013 - mcdurdin - I3909 - Keyman Developer crashes when saving keyboard if touch layout filename is not specified
                    07 Nov 2013 - mcdurdin - I3945 - V9.0 - Touch Layout Editor should allow import from existing On Screen Keyboard
                    10 Jan 2014 - mcdurdin - I4021 - V9.0 - Redesign Keyboard Wizard to integrate V9 features
                    07 Feb 2014 - mcdurdin - I4034 - V9.0 - Restructure keyboard wizard for source views and features
                    21 Feb 2014 - mcdurdin - I4057 - V9.0 - Keyman Developer Keyboard Font dialog helpful to reduce font confusion
                    21 Feb 2014 - mcdurdin - I4058 - V9.0 - Touch Layout Import OSK fails with weird error if OSK is missing
                    21 Feb 2014 - mcdurdin - I4059 - V9.0 - If OSK is not saved, then changes do not flow through in Touch Layout Import
                    27 Feb 2014 - mcdurdin - I4081 - V9.0 - Trace compile errors in subfiles in Keyman Developer
                    27 Feb 2014 - mcdurdin - I4082 - V9.0 - Make details tab scrollable with mouse wheel
                    27 Feb 2014 - mcdurdin - I4083 - V9.0 - When errors encountered in JSON layout file, locate the error in the source view
                    27 Feb 2014 - mcdurdin - I4085 - V9.0 - Remove old font selection controls from wizard
                    19 Mar 2014 - mcdurdin - I4137 - V9.0 - Don't prompt to sort out L/R shift on save of keyboard
                    19 Mar 2014 - mcdurdin - I4138 - V9.0 - If the KVK is removed from the keyboard, it can still be imported into the touch layout until the file is closed
                    21 Mar 2014 - mcdurdin - I4157 - V9.0 - Need keyboard version control in Keyman Developer Wizard
                    01 May 2014 - mcdurdin - I4181 - V9.0 - Stop using DeleteFileAlways, MOVEFILE_DELAY_UNTIL_REBOOT
                    10 Jun 2014 - mcdurdin - I4258 - V9.0 - Keyboard font dialog crashes if in text editor view
                    16 Jun 2014 - mcdurdin - I4247 - Clicking Remove Feature when no feature selected crashes [CrashID:tike.exe_9.0.449.0_00987F63_EAccessViolation]
                    23 Oct 2014 - mcdurdin - I4369 - V9.0 - Add kmw_embedcss to feature support in Developer
                    04 Nov 2014 - mcdurdin - I4505 - V9.0 - Add JSON metadata editor to keyboard wizard
                    04 Nov 2014 - mcdurdin - I4504 - V9.0 - Consolidate the compile action into single command
                    04 Nov 2014 - mcdurdin - I4506 - V9.0 - Add command to send email with targets
                    14 Nov 2014 - mcdurdin - I4508 - V9.0 - JSON search needs to do cleanname on the keyboard filename to find candidates
                    31 Dec 2014 - mcdurdin - I4557 - V9.0 - The text editor in the keyboard wizard can be blanked in some circumstances when switching tabs
                    31 Dec 2014 - mcdurdin - I4558 - V9.0 - Keyboard parser should not force keyboard version to 9.0
                    30 Apr 2015 - mcdurdin - I4679 - V9.0 - Remember focus for active editor when swapping editors
                    30 Apr 2015 - mcdurdin - I4678 - V9.0 - Fixup Ctrl+PgUp, Ctrl+PgDn, Alt+Left, Alt+Right hotkeys
                    30 Apr 2015 - mcdurdin - I4680 - V9.0 - Ctrl+S often activates the "select key" dialog in editor
                    04 May 2015 - mcdurdin - I4687 - V9.0 - Split project UI actions into separate classes
                    04 May 2015 - mcdurdin - I4695 - V9.0 - Debugger needs to use project file filename not internal name
                    06 May 2015 - mcdurdin - I4702 - Package actions crash if package not part of a project [CrashID:tike.exe_9.0.496.0_008033BD_EAccessViolation]
                    27 May 2015 - mcdurdin - I4587 - Developer crashes if Edit Feature clicked when no features are present for a keyboard [CrashID:tike.exe_9.0.476.0_009B32CF_EAccessViolation]
                    27 May 2015 - mcdurdin - I4427 - Developer crashes if Edit Feature clicked when no features are present for a keyboard [CrashID:tike.exe_9.0.463.0_0098CDA7_EAccessViolation]
                    27 May 2015 - mcdurdin - I4533 - Developer crashes when touch layout keys are added to a keyboard and the visual desktop layout is then refreshed [CrashID:tike.exe_9.0.474.0_0095D1C1_EAssertionFailed]
                    27 May 2015 - mcdurdin - I4723 - Switching from source tab to design tab in layout fails to refresh the layout
                    30 May 2015 - mcdurdin - I4728 - Saving a keyboard with a locked feature file causes Developer to crash [CrashID:tike.exe_9.0.504.0_0045876A_EFCreateError]
                    22 Jun 2015 - mcdurdin - I4751 - Add "open in code view" default option for keyboards
                    22 Jun 2015 - mcdurdin - I4765 - Double-click on message does not find source line since build 500
                    22 Jun 2015 - mcdurdin - I4766 - EthnologueCode system store does not correlate to control on first page of wizard for messages
                    23 Jun 2015 - mcdurdin - I4751 - Add "open in code view" default option for keyboards
                    24 Jul 2015 - mcdurdin - I4796 - Refresh Keyman Developer look and feel for release
                    03 Aug 2015 - mcdurdin - I4810 - Targets box overlaps name box in keyboard editor
                    03 Aug 2015 - mcdurdin - I4811 - Hide JSON metadata tab for desktop keyboards
                    03 Aug 2015 - mcdurdin - I4807 - Add Character Identifier to Keyman Developer
                    09 Aug 2015 - mcdurdin - I4847 - Code editor does not get focus automatically when file is loaded
                    24 Aug 2015 - mcdurdin - I4872 - OSK font and Touch Layout font should be the same in Developer
                    24 Aug 2015 - mcdurdin - I4865 - Add treat hints and warnings as errors into project
                    24 Aug 2015 - mcdurdin - I4866 - Add warn on deprecated features to project and compile
                    23 Feb 2016 - mcdurdin - I4979 - Importing a KMN into a KVK does not work if includecodes are in the keyboard
*)
unit UfrmKeymanWizard;  // I3306  // I3323   // I4021   // I4085   // I4505   // I4796

interface

uses
  System.Types,
  System.UITypes,
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  UfrmMDIChild, Buttons, StdCtrls, KeyBtn, ComCtrls, ExtCtrls,
  Menus, ExtDlgs,
  Keyman.Developer.System.Project.ProjectFile, UfrmMDIEditor, CharMapInsertMode,
  Contnrs, Unicode, UframeBitmapEditor, OnScreenKeyboardData,
  UframeTextEditor, UfrmDebug, ExtShiftState,
  ImgList, MenuImgList,
  KeyboardParser, TextFileFormat, dmActionsKeyboardEditor,
  dmActionsDebugger,
  VisualKeyboard, UframeOnScreenKeyboardEditor,
  KeymanDeveloperUtils,
  OnScreenKeyboard, KMDActionInterfaces,


  AppEvnts,
  kmxfileconsts,
  Keyman.Developer.System.Project.kmnProjectFile,
  Keyman.Developer.UI.Project.kmnProjectFileUI,
  UserMessages,
  UframeTouchLayoutBuilder,
  CheckboxGridHelper,
  Vcl.Grids, KeyboardFonts,
  TempFileManager,
  UfrmDebugStatus,
  UKeymanTargets, Winapi.UxTheme, Vcl.Themes,
  System.Generics.Collections, LeftTabbedPageControl, Vcl.CheckLst;

type
  TWizardFeature = record
    Frame: TframeTextEditor;
    Modified: Boolean;
    OpenFileAge: TDateTime;
    Filename: string;
  end;

  TfrmKeymanWizard = class(TfrmTikeEditor, IKMDPrintActions {TODO:, IKMDPrintPreviewActions})
    dlgBrowseBitmap: TOpenPictureDialog;
    dlgSaveExport: TSaveDialog;
    dlgSaveBitmap: TSavePictureDialog;
    panCharSetAndCopyright: TPanel;
    panMessage: TPanel;
    panComments: TPanel;
    lblCopyright: TLabel;
    Label2: TLabel;
    editCopyright: TEdit;
    lblMessage: TLabel;
    editMessage: TEdit;
    lblComments: TLabel;
    memoComments: TMemo;
    lblWinLangTitle: TLabel;
    cmdWinLang_Add: TButton;
    cmdWinLang_Remove: TButton;
    panTargets: TPanel;
    pages: TLeftTabbedPageControl;
    pageDetails: TTabSheet;
    pageLayout: TTabSheet;
    pageIcon: TTabSheet;
    pageOnScreenKeyboard: TTabSheet;
    pageTouchLayout: TTabSheet;
    pageCompile: TTabSheet;
    panKeyboardVersion: TPanel;
    frameBitmap: TframeBitmapEditor;
    Label9: TLabel;
    panLayoutSimple: TPanel;
    kbdLayout: TOnScreenKeyboard;
    panLayoutSimple_Edit: TPanel;
    panDesignDetails: TPanel;
    Label5: TLabel;
    keySample: TKeyBtn;
    keySampleCtrl: TKeyBtn;
    keySampleShift: TKeyBtn;
    keySampleAlt: TKeyBtn;
    editKeyOutputText: TEdit;
    editKeyOutputCode: TEdit;
    lblLayoutOutputCodeCaption: TLabel;
    lblOutputCharacterCaption: TLabel;
    chkSplitCtrlAlt: TCheckBox;
    chkLayoutDisplay102Key: TCheckBox;
    panWindowsLanguages: TPanel;
    panFeatures: TPanel;
    sbDetails: TScrollBox;
    sbCompile: TScrollBox;
    gridFeatures: TStringGrid;
    cmdAddFeature: TButton;
    cmdRemoveFeature: TButton;
    cmdEditFeature: TButton;
    gridWinLanguages: TStringGrid;
    pageKMWEmbedJS: TTabSheet;
    pageIncludeCodes: TTabSheet;
    pageKMWHelp: TTabSheet;
    panDetailsLeft: TPanel;
    tmrUpdateCharacterMap: TTimer;
    cmdInsertCopyright: TButton;
    pagesLayout: TPageControl;
    pageLayoutDesign: TTabSheet;
    pageLayoutCode: TTabSheet;
    pagesTouchLayout: TPageControl;
    pageTouchLayoutDesign: TTabSheet;
    pageTouchLayoutCode: TTabSheet;
    panWebHelp: TPanel;
    editKMWHelpText: TEdit;
    chkKMWRTL: TCheckBox;
    lblKMWHelpText: TLabel;
    editKeyboardVersion: TEdit;
    lblKeyboardVersion: TLabel;
    lblTarget: TLabel;
    clbTargets: TCheckListBox;
    panISOLanguages: TPanel;
    lblISOLangTitle: TLabel;
    lblISOLang: TLabel;
    editEthnologueCode: TEdit;
    cmdISOLang_Lookup: TButton;
    lblCongrats: TLabel;
    panBuildWindows: TPanel;
    lblInstallHint: TLabel;
    lblCompileTargetHeader: TLabel;
    cmdInstall: TButton;
    cmdUninstall: TButton;
    panBuildKMW: TPanel;
    lblDebugHostCaption: TLabel;
    lblCrossPlatform: TLabel;
    cmdTestKeymanWeb: TButton;
    cmdOpenDebugHost: TButton;
    lbDebugHosts: TListBox;
    cmdSendURLsToEmail: TButton;
    panName: TPanel;   // I4810
    lblName: TLabel;   // I4810
    Label3: TLabel;   // I4810
    lblType: TLabel;   // I4810
    editName: TEdit;
    panDebugHost: TPanel;
    panDebugWindowHost: TPanel;
    panDebugStatusHost: TPanel;
    Splitter1: TSplitter;
    Splitter2: TSplitter;   // I4810
    panLanguageKeyman10: TPanel;
    lblLanguageKeyman10Note: TLabel;
    lblLanguageKeyman10Title: TLabel;
    imgQRCode: TImage;
    panOpenInExplorer: TPanel;
    lblOpenInExplorer: TLabel;
    cmdOpenSourceFolder: TButton;
    cmdOpenBuildFolder: TButton;
    cmdOpenProjectFolder: TButton;
    panFileActions: TPanel;
    lblFileActions: TLabel;
    cmdAddToProject: TButton;
    cmdStartDebugging: TButton;
    cmdCompile: TButton;
    panWarnMixedShiftStates: TPanel;
    cmdFixupShiftStates: TButton;
    lblWarnMixedShiftStates: TLabel;
    cmdCopyDebuggerLink: TButton;
    cmdConfigureWebDebugger: TButton;
    procedure FormCreate(Sender: TObject);
    procedure editNameChange(Sender: TObject);
    procedure editCopyrightChange(Sender: TObject);
    procedure editMessageChange(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure cmdRunTutorialClick(Sender: TObject);
    procedure mnuViewUnderlyingLayoutClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure chkSplitCtrlAltClick(Sender: TObject);
    procedure editKMWHelpTextChange(Sender: TObject);
    procedure chkKMWRTLClick(Sender: TObject);
    procedure editKeyOutputCodeChange(Sender: TObject);
    procedure editKeyOutputTextChange(Sender: TObject);
    procedure memoCommentsChange(Sender: TObject);
    procedure kbdLayoutSelectionChange(Sender: TObject);
    procedure kbdLayoutShiftChange(Sender: TObject);
    procedure kbdLayoutDragOver(Sender, Source: TObject; X, Y: Integer;
      State: TDragState; var Accept: Boolean);
    procedure kbdLayoutDragDrop(Sender, Source: TObject; X, Y: Integer);
    procedure editKeyOutputTextKeyUp(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure chkLayoutDisplay102KeyClick(Sender: TObject);
    procedure cmdInsertCopyrightClick(Sender: TObject);
    procedure cmdOpenSourceFolderClick(Sender: TObject);
    procedure editKeyOutputCodeClick(Sender: TObject);
    procedure editKeyOutputTextClick(Sender: TObject);
    procedure tmrUpdateCharacterMapTimer(Sender: TObject);
    procedure cmdWinLang_AddClick(Sender: TObject);
    procedure cmdWinLang_RemoveClick(Sender: TObject);
    procedure cmdISOLang_LookupClick(Sender: TObject);
    procedure lbWinLang_SupportedClick(Sender: TObject);
    procedure editEthnologueCodeChange(Sender: TObject);
    procedure FormActivate(Sender: TObject);
    procedure cmdTouchLayoutTemplateClick(Sender: TObject);
    procedure cmdImportFromOnScreenClick(Sender: TObject);
    procedure pagesDrawTab(Control: TCustomTabControl; TabIndex: Integer;
      const Rect: TRect; Active: Boolean);
    procedure pagesChange(Sender: TObject);
    procedure pagesChanging(Sender: TObject; var AllowChange: Boolean);
    procedure gridWinLanguagesClick(Sender: TObject);
    procedure gridWinLanguagesKeyPress(Sender: TObject; var Key: Char);
    procedure gridWinLanguagesDrawCell(Sender: TObject; ACol, ARow: Integer;
      Rect: TRect; State: TGridDrawState);
    procedure cmdKMWBrowseHelpFileClick(Sender: TObject);
    procedure cmdAddFeatureClick(Sender: TObject);
    procedure cmdRemoveFeatureClick(Sender: TObject);
    procedure cmdEditFeatureClick(Sender: TObject);
    procedure cmdOpenDebugHostClick(Sender: TObject);
    procedure gridFeaturesDblClick(Sender: TObject);
    procedure gridFeaturesClick(Sender: TObject);
    procedure sbDetailsMouseWheel(Sender: TObject; Shift: TShiftState;
      WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean);
    procedure editKeyboardVersionChange(Sender: TObject);
    procedure clbTargetsClickCheck(Sender: TObject);
    procedure cmdSendURLsToEmailClick(Sender: TObject);
    procedure pagesLayoutChange(Sender: TObject);
    procedure pagesLayoutChanging(Sender: TObject; var AllowChange: Boolean);
    procedure pagesTouchLayoutChange(Sender: TObject);
    procedure pagesTouchLayoutChanging(Sender: TObject;
      var AllowChange: Boolean);
    procedure lbDebugHostsClick(Sender: TObject);
    procedure cmdOpenBuildFolderClick(Sender: TObject);
    procedure cmdOpenProjectFolderClick(Sender: TObject);
    procedure cmdFixupShiftStatesClick(Sender: TObject);
    procedure cmdCopyDebuggerLinkClick(Sender: TObject);
  private
    frameSource: TframeTextEditor;

    FTextFileFormat: TTextFileFormat;

    FKeyboardParser: TKeyboardParser;
    FCurrentRule: TKeyboardParser_LayoutRule;

    frameOSK: TframeOnScreenKeyboardEditor;

    frameTouchLayout: TframeTouchLayoutBuilder;
    frameTouchLayoutSource: TframeTextEditor;   // I4034

    FFeature: array[TKeyboardParser_FeatureID] of TWizardFeature;

    FUnderlyingLayout: HKL;
    FLoadedSystemKeyboard: Boolean;

    FLoading: Boolean;
    FDebugForm: TfrmDebug;
    FDebugStatusForm: TfrmDebugStatus;

    FKeyChanging: Boolean;
    FLayoutSetup: Boolean;
    FCheckboxGridHelper: TCheckboxGridHelper;

    function GetCurrentRule: TKeyboardParser_LayoutRule;

    procedure EnableControls;

    procedure InitTabIntro;
    procedure InitTabKeyboard;
    procedure InitTabDetails;
    procedure InitTabBitmap;
    procedure InitTabFinish;

    procedure FocusTabIntro;
    procedure FocusTabLayout;
    procedure FocusTabBitmap;
    procedure FocusTabFinish;
    procedure FocusTabOnScreenKeyboard;

    procedure UpdateKeyFont;
    procedure Layout_SetAllKeyDetails(UpdateLRShift: Boolean = False; FixupShiftStates: Boolean = False);  // I2532   // I4137
    procedure SelectVKey(VKey: Integer);
    //function ParseFile(FText: string): Boolean;
    //function GetTag(var s: string): string;
    //procedure ConvertWhiteSpace(var s: string);
    //function Dequote(s: string): string;
    procedure Layout_UpdateCharacterSet;
    {function ParseKMN10File(FText: string): Boolean;}

    procedure InitSystemKeyboard;
    procedure UninitSystemKeyboard;

    function SaveBitmap: Boolean;
    procedure LoadBitmap;

    procedure MoveSourceToParser(UpdateLRShift: Boolean);   // I4137
    procedure MoveParserToSource;

    procedure UpdateControls(UpdateLRShift: Boolean);   // I4137

    procedure BitmapModifiedChanged(Sender: TObject);
    procedure WMUserFormShown(var Message: TMessage); message WM_USER_FORMSHOWN;
    procedure WMSysCommand(var Message: TWMSysCommand); message WM_SYSCOMMAND;
    procedure WMUserInputLangChange(var Message: TMessage); message WM_USER_INPUTLANGCHANGE;
    procedure Layout_FocusOutput;
    procedure KeyboardParserChanged(Sender: TObject);
    {function ParseKey(s: string; var key: Integer;
      var shiftstate: TExtShiftState; var FLR: Boolean): Boolean;}

    procedure SetTextFileFormat(const Value: TTextFileFormat);

    procedure SourceChanged(Sender: TObject);
    procedure SetupDebugForm;
    procedure InitTabOnScreenKeyboard;

    procedure Layout_UpdateSelectedKeyDetails(UpdateOutput: Boolean);
    procedure Layout_UpdateShiftKeys;
    function Layout_FixupRules_LRShift(FSplit: Boolean): Boolean;

    procedure LoadOSK;   // I4034
    procedure SaveOSK;

    procedure BitmapEnableControls;
    procedure OSKModified(Sender: TObject);
    procedure OSKImportKMX(Sender: TObject; var KMXFileName: TTempFile);   // I4181
    procedure OSKImportKMXFinished(Sender: TObject; KMXFileName: TTempFile);   // I4181

    procedure DoFocus(control: TWinControl);
    procedure DoUpdateCharacterMap;
    procedure InitTabLanguages;
    procedure Languages_UpdateControls;
    function DoOpenFileFormat(FFormat: TTextFileFormat;   // I3637
      FUseFormat: Boolean): Boolean;

    procedure InitTabTouchLayout;
    procedure SaveTouchLayout;
    procedure TouchLayoutModified(Sender: TObject);
    procedure FocusTabTouchLayout;

    procedure EditorBreakpointClicked(Sender: TObject; ALine: Integer);

    procedure FillFeatureGrid;
    procedure CheckParserSourceMove(Changed: Boolean);
    procedure ToggleWinLanguagesRow(Y: Integer);
    procedure ConfirmSaveOfOldEditorWindows;
    procedure ConfirmSaveOfOldEditorWindow(FeatureID: TKeyboardParser_FeatureID;
      FModified: Boolean; const FOldFilename: string; DoSave: TProc; DoLoad: TProc<String>);
    procedure LoadFeature(ID: TKeyboardParser_FeatureID);
    function FeatureTab(kf: TKeyboardParser_FeatureID): TTabSheet;
    procedure InitFeatureTab(ID: TKeyboardParser_FeatureID);
    procedure FeatureModified(Sender: TObject);
    function SaveFeature(ID: TKeyboardParser_FeatureID): Boolean;
    procedure SelectTouchLayoutTemplate(APromptChange: Boolean);
    procedure LoadTouchLayout;   // I4034
    function GetFontInfo(Index: TKeyboardFont): TKeyboardFontInfo;   // I4057
    procedure SetFontInfo(Index: TKeyboardFont; const Value: TKeyboardFontInfo);   // I4057

    procedure SetTargetsListBoxFromTargets(ATargets: string);   // I4504
    procedure UpdateWizardForTargets;   // I4504
    function GetTargetsFromListBox: TKeymanTargets;   // I4504
    function GetCompileTargets: TKeymanTargets;   // I4504

    function IsInParserMode: Boolean;
    procedure ShowKeyboardComplexDesignMessage;
    function GetIsDebugVisible: Boolean;   // I4557
    function ShouldShowLanguageControls(field: TSystemStore): Boolean;
    procedure OrderDetailsPanels;
    function ValidateFileHasNoUnicodeCharacters(sw: WideString): Boolean;
    procedure UpdateQRCode;

  protected
    function GetHelpTopic: string; override;

    procedure FocusTab;

    procedure LoadSettings; override;
    procedure SaveSettings(SaveProject: Boolean); override;

    function GetFileNameFilter: string; override;
    function GetDefaultExt: string; override;
    function DoSaveFile: Boolean; override;
    function DoOpenFile: Boolean; override;
    procedure CodeFontChanged; override;
    procedure CharFontChanged; override;

    function GetProjectFile: TProjectFile; override;

    function ShouldRememberFocus(Control: TWinControl): Boolean; override;   // I4679

  public
    procedure StartDebugging(FStartTest: Boolean = False);
    procedure StopDebugging;
    function PrepareForBuild(var DebugReset: Boolean): Boolean;   // I4504

    procedure DebugSetBreakpoint(Sender: TObject; ALine: Integer);
    procedure DebugClearBreakpoint(Sender: TObject; ALine: Integer);
    procedure DebugUpdateExecutionPoint(Sender: TObject; ALine: Integer);

    function HasSubfilename(const Filename: string): Boolean; override;   // I4081

    procedure ControlKeyPressedAndReleased; override;

    procedure FindError(const Filename: string; s1: string; line: Integer); override;   // I4081
    procedure RefreshOptions; override;
    procedure TextFileFormatClick; override;
    function CanTextFileFormatClick: Boolean; override;

    function CanChangeTab(FForward: Boolean): Boolean; override;   // I4678
    procedure ChangeTab(FForward: Boolean); override;   // I4678

    function CanChangeView(FView: TCodeDesignView): Boolean; override;   // I4678
    procedure ChangeView(FView: TCodeDesignView); override;   // I4678

    { IKMDPrintActions }
    function PrintFile: Boolean;
    { IKMDPrintPreviewActions }
    //TODO: function PrintPreview: Boolean;

    function CanReloadAsTextFileFormatClick: Boolean; override;   // I3637
    procedure ReloadAsTextFileFormatClick(TextFileFormat: TTextFileFormat);   // I3637
      override;
    procedure CheckForReload; override;

    procedure NotifyStartedWebDebug;

    property TextFileFormat: TTextFileFormat read FTextFileFormat write SetTextFileFormat;

    property DebugForm: TfrmDebug read FDebugForm;
    property DebugStatusForm: TfrmDebugStatus read FDebugStatusForm;
    property IsDebugVisible: Boolean read GetIsDebugVisible;

    property Parser: TKeyboardParser read FKeyboardParser;   // I4505
    property CompileTargets: TKeymanTargets read GetCompileTargets;   // I4504
    property FontInfo[Index: TKeyboardFont]: TKeyboardFontInfo read GetFontInfo write SetFontInfo; // I4057
  end;

function IsKeymanWizardIdentifier(line: string): Boolean;

const
  SKeymanWizardIdentifier10 = 'c KeymanWizard 1.0';
  SKeymanWizardIdentifier11 = 'c KeymanWizard 1.1';
  SKeymanWizardFont       = 'c KeymanWizard Font: ';

  SKeymanWizardComment1   = 'c TIKE will use the line above to identify this file as a Keyman Wizard file';
  SKeymanWizardComment2   = 'c Delete the first line to stop the file opening in the Wizard editor';
  SKeymanWizardComment3   = 'c (Created by Keyman Developer %s)';

  SUnknownError = 'Unable to locate error in the wizard.  You should view the source and compile from there.';

implementation

{$R *.DFM}

uses
  System.Math,
  System.Win.ComObj,

  Keyman.Developer.System.HelpTopics,
  Keyman.System.Debug.DebugUIStatus,
  Keyman.System.QRCode,
  Keyman.UI.FontUtils,

  CharacterDragObject,
  CharacterInfo,
  CharMapDropTool,
  Clipbrd,
  dmActionsMain,
  KeymanDeveloperOptions,
  KeymanVersion,
  KMDevResourceStrings,
  kmxfile,
  Keyman.Developer.System.Project.Project,
  Keyman.Developer.System.Project.ProjectLog,
  Keyman.Developer.System.Project.kmnProjectFileAction,
  Keyman.Developer.System.ServerAPI,
  Keyman.Developer.System.KmcWrapper,
  Keyman.Developer.UI.Project.ProjectFileUI,
  Keyman.Developer.UI.UfrmMessageDlgWithSave,
  ErrorControlledRegistry,
  RedistFiles,
  RegistryKeys,
  ScanCodeMap,
  TouchLayoutUtils,
  UfrmAddKeyboardFeature,
  UfrmBitmapEditor,
  UfrmEditor,
  UfrmMain,
  UfrmMessages,
  UfrmMustIncludeDebug,
  UfrmSelectKey,
  UfrmSelectSystemKeyboard,
  UfrmSelectTouchLayoutTemplate,
  UfrmSelectWindowsLanguages,
  UfrmSendURLsToEmail,
  Upload_Settings,
  utildir,
  utilexecute,
  utilstr,
  utilsystem,
  VersionInfo,
  VKeys,
  WideStrings,
  WindowsLanguages,
  keymanstrings;

const
  KeyboardFeatureTabName: array[TKeyboardParser_FeatureID] of string = (
    'pageIcon', 'pageOnScreenKeyboard', 'pageTouchLayout',
    'pageKMWEmbedJS', 'pageKMWEmbedCSS', 'pageKMWHelp', 'pageIncludeCodes');   // I4369

{-----------------------------------------------------------------------------}
{ Form Events                                                                 }
{-----------------------------------------------------------------------------}

procedure TfrmKeymanWizard.FormActivate(Sender: TObject);
begin
  inherited;
  frmKeymanDeveloper.cbTextFileFormat.ItemIndex := Ord(TextFileFormat);
end;

procedure TfrmKeymanWizard.FormCreate(Sender: TObject);
begin
  FLoading := True;

  //SetWindowTheme(pages.Handle, 'Explorer', '');

  cmdAddToProject.Caption := '&Add to Project';
  cmdStartDebugging.Caption := '&Start Debugging';

  FKeyboardParser := TKeyboardParser.Create;
  FKeyboardParser.OnChanged := KeyboardParserChanged;
  FCurrentRule := nil;

  frameSource := TframeTextEditor.Create(Self);
  frameSource.Parent := pageLayoutCode;
  frameSource.Align := alClient;
  frameSource.EditorFormat := efKMN;
  frameSource.Visible := True;
  frameSource.OnChanged := SourceChanged;
  frameSource.OnBreakpointClicked := EditorBreakpointClicked;
  frameSource.TextFileFormat := FTextFileFormat;

  pages.ActivePage := pageDetails;
  InitTabIntro;
  InitTabLanguages;
  InitTabDetails;
  InitSystemKeyboard;
  InitTabKeyboard;
  InitTabOnScreenKeyboard;
  InitTabTouchLayout;   // I3885
  InitTabBitmap;
  InitTabFinish;

  inherited;

  SetupDebugForm;

  GetCharMapDropTool.Handle(Self, cmimText);
  GetCharMapDropTool.Handle(editKeyOutputCode, cmimCode);
  frameSource.SetupCharMapDrop;
  frameTouchLayout.SetupCharMapDrop;
  frameTouchLayoutSource.SetupCharMapDrop;

  FillFeatureGrid;

  FLoading := False;
end;

procedure TfrmKeymanWizard.FormDestroy(Sender: TObject);
begin
  inherited;
  FCurrentRule := nil;
  FreeAndNil(FKeyboardParser);   // I4557
  FreeAndNil(FDebugForm);
  FreeAndNil(FCheckboxGridHelper);
  UninitSystemKeyboard;
end;

{-----------------------------------------------------------------------------}
{ Interface Functions                                                         }
{-----------------------------------------------------------------------------}

function IsKeymanWizardIdentifier(line: string): Boolean;
begin
  Result :=
    (Trim(line) = SKeymanWizardIdentifier10) or
    (Trim(line) = SKeymanWizardIdentifier11);
end;

{-----------------------------------------------------------------------------}
{ General Functions                                                           }
{-----------------------------------------------------------------------------}

procedure TfrmKeymanWizard.EditorBreakpointClicked(Sender: TObject;
  ALine: Integer);
begin
  if Assigned(FDebugForm) then
    FDebugForm.ToggleBreakpoint(ALine);
end;

procedure TfrmKeymanWizard.EnableControls;
begin
  BitmapEnableControls;

  cmdWinLang_Remove.Enabled := gridWinLanguages.RowCount > 1;
  gridWinLanguages.Enabled := gridWinLanguages.RowCount > 1;

  cmdOpenDebugHost.Enabled := lbDebugHosts.ItemIndex >= 0;
  cmdSendURLsToEmail.Enabled := lbDebugHosts.Items.Count > 0;   // I4506
  cmdCopyDebuggerLink.Enabled := lbDebugHosts.ItemIndex >= 0;

  gridFeatures.Enabled := gridFeatures.RowCount > 1;   // I4587   // I4427
  cmdEditFeature.Enabled := gridFeatures.RowCount > 1;   // I4587   // I4427
  cmdRemoveFeature.Enabled := gridFeatures.RowCount > 1;   // I4587   // I4427

  // Prevent side-effect creation of FStandaloneProjectFile, e.g. in FormCreate #6149
  if not Assigned(FStandaloneProjectFile) or not Assigned(FProjectFile)
    then cmdOpenProjectFolder.Enabled := False
    else cmdOpenProjectFolder.Enabled := Assigned(ProjectFile.Project);
end;

procedure TfrmKeymanWizard.FocusTab;
begin
  EnableControls;

  if pages.ActivePage = pageDetails then
    FocusTabIntro
  else if pages.ActivePage = pageLayout then
    FocusTabLayout
  else if pages.ActivePage = pageIcon then
    FocusTabBitmap
  else if pages.ActivePage = pageOnScreenKeyboard then
    FocusTabOnScreenKeyboard
  else if pages.ActivePage = pageTouchLayout then
    FocusTabTouchLayout // I3885
  else if pages.ActivePage = pageCompile then
    FocusTabFinish;
end;

procedure TfrmKeymanWizard.FocusTabOnScreenKeyboard;
begin
end;

procedure TfrmKeymanWizard.FocusTabTouchLayout;   // I3885
begin
  if pagesTouchLayout.ActivePage = pageTouchLayoutDesign
    then DoFocus(frameTouchLayout)
    else DoFocus(frameTouchLayoutSource);
end;

{-----------------------------------------------------------------------------}
{ Intro Page                                                                  }
{-----------------------------------------------------------------------------}

procedure TfrmKeymanWizard.InitTabIntro;
begin
  FCheckboxGridHelper := TCheckboxGridHelper.Create(gridWinLanguages);
end;

procedure TfrmKeymanWizard.FocusTabIntro;
begin
  DoFocus(editName);
end;

procedure TfrmKeymanWizard.cmdRemoveFeatureClick(Sender: TObject);
var
  FFeature: TKeyboardParser_Feature;
begin
  FFeature := gridFeatures.Objects[0, gridFeatures.Row] as TKeyboardParser_Feature;
  if not Assigned(FFeature) then   // I4247
    Exit;
  FKeyboardParser.Features.Remove(FFeature.ID);
  FillFeatureGrid;
  EnableControls;
end;

procedure TfrmKeymanWizard.cmdRunTutorialClick(Sender: TObject);
begin
  frmKeymanDeveloper.HelpTopic('tutorial_keyboard');
end;

procedure TfrmKeymanWizard.cmdSendURLsToEmailClick(Sender: TObject);   // I4506
begin
  with TfrmSendURLsToEmail.Create(Application.MainForm) do
  try
    KeyboardName := Self.FKeyboardParser.GetSystemStoreValue(ssName);
    Hosts.Assign(lbDebugHosts.Items);
    ShowModal;
  finally
    Free;
  end;
end;

procedure TfrmKeymanWizard.cmdCopyDebuggerLinkClick(Sender: TObject);
begin
  try
    Clipboard.AsText := lbDebugHosts.Items[lbDebugHosts.ItemIndex];
  except
    on E:Exception do
      ShowMessage(E.Message);
  end;
end;


procedure TfrmKeymanWizard.CodeFontChanged;
begin
  inherited;
  frameSource.CodeFont := CodeFont;
  frameTouchLayoutSource.CodeFont := CodeFont;
  frameOSK.CodeFont := CodeFont;
end;

{-----------------------------------------------------------------------------}
{ Details Page                                                                }
{-----------------------------------------------------------------------------}

procedure TfrmKeymanWizard.InitTabDetails;
var
  i: TKeymanTarget;
begin
  for i := Low(TKeymanTarget) to High(TKeymanTarget) do   // I4504
    clbTargets.Items.Add(SKeymanTargets[i]);
  UpdateWizardForTargets;
end;

procedure TfrmKeymanWizard.UpdateWizardForTargets;   // I4504
var
  FTargets: TKeymanTargets;
begin
  // Some controls are only relevant for specific targets
  FTargets := GetTargetsFromListBox;

  // If the target list is empty, then we pretend it is 'windows', for fallback
  if FTargets = [] then
    FTargets := [ktWindows];

  if ktAny in FTargets then
    FTargets := AllKeymanTargets;

  // Control visibility based on targets
  panWindowsLanguages.Visible := (ktWindows in FTargets) and ShouldShowLanguageControls(ssLanguage);
  panISOLanguages.Visible := ShouldShowLanguageControls(ssEthnologueCode);
  panLanguageKeyman10.Visible := ShouldShowLanguageControls(ssVersion);
  panWebHelp.Visible := FTargets * KMWKeymanTargets <> [];
  panBuildWindows.Visible := ktWindows in FTargets;
  panBuildKMW.Visible := FTargets * KMWKeymanTargets <> [];
  OrderDetailsPanels;
end;

procedure TfrmKeymanWizard.OrderDetailsPanels;
var
  v: Integer;

  procedure Order(p: TPanel);
  begin
    if p.Visible then
    begin
      p.Top := v;
      Inc(v, p.Height);
    end;
  end;
begin
  v := 0;
  Order(panName);
  Order(panTargets);
  Order(panLanguageKeyman10);
  Order(panWindowsLanguages);
  Order(panISOLanguages);
  Order(panDetailsLeft);
  Order(panComments);
  Order(panFeatures);
end;

function TfrmKeymanWizard.ShouldShowLanguageControls(field: TSystemStore): Boolean;
var
  v: string;
begin
  // For older versions, we always show the language controls
  // We use 'ssVersion' for the Version 10 note field. Abuse but hey that
  // makes life more interesting for maintenance devs.
  v := FKeyboardParser.GetSystemStoreValue(ssVersion);
  if (v <> '') and (CompareVersions(v, SKeymanVersion100) > 0) then
    Exit(field <> ssVersion);

  // For newer versions, we show them only if there are values set for
  // the fields
  if field = ssVersion then
    Exit(True);

  if field = ssLanguage then
    Result :=
      (FKeyboardParser.GetSystemStoreValue(ssLanguage) <> '') or
      (FKeyboardParser.GetSystemStoreValue(ssWindowsLanguages) <> '')
  else
    Exit(FKeyboardParser.GetSystemStoreValue(field) <> '');
end;

function TfrmKeymanWizard.GetTargetsFromListBox: TKeymanTargets;   // I4504
var
  i: Integer;
begin
  Result := [];
  for i := 0 to clbTargets.Items.Count - 1 do
    if clbTargets.Checked[i] then
      Include(Result, TKeymanTarget(i));
end;

function TfrmKeymanWizard.GetCompileTargets: TKeymanTargets;   // I4504
begin
  Result := StringToKeymanTargets(FKeyboardParser.GetSystemStoreValue(ssTargets));
  if ktAny in Result then Result := AllKeymanTargets;
  if Result = [] then Result := [ktWindows];
end;

procedure TfrmKeymanWizard.SetTargetsListBoxFromTargets(ATargets: string);   // I4504
var
  i: TKeymanTarget;
  FTargets: TKeymanTargets;
begin
  FTargets := StringToKeymanTargets(ATargets);
  if FTargets = [] then
    FTargets := [ktWindows];
  for i := Low(TKeymanTarget) to High(TKeymanTarget) do
    clbTargets.Checked[Ord(i)] := i in FTargets;
end;

{ Control events }

procedure TfrmKeymanWizard.editNameChange(Sender: TObject);
begin
  if FLoading then Exit;
  FKeyboardParser.SetSystemStoreValue(ssName, editName.Text);
  EnableControls;
end;

procedure TfrmKeymanWizard.clbTargetsClickCheck(Sender: TObject);   // I4504
begin
  if FLoading then
    Exit;
  FKeyboardParser.SetSystemStoreValue(ssTargets,
    KeymanTargetsToString(GetTargetsFromListBox));
  UpdateWizardForTargets;
  EnableControls;
end;

procedure TfrmKeymanWizard.Layout_UpdateCharacterSet;
begin
  if not FKeyboardParser.IsComplex then   // I4557
  begin
    if FKeyboardParser.IsKeyboardUnicode then
    begin
      lblLayoutOutputCodeCaption.Caption := 'Unicode Character Value(s)';
    end
    else
    begin
      lblLayoutOutputCodeCaption.Caption := 'Character Value(s)';
    end;
  end;
end;

procedure TfrmKeymanWizard.editCopyrightChange(Sender: TObject);
begin
  if FLoading then Exit;
  FKeyboardParser.SetSystemStoreValue(ssCopyright, editCopyright.Text);
  EnableControls;
end;

procedure TfrmKeymanWizard.editKMWHelpTextChange(Sender: TObject);
begin
  if FLoading then Exit;
  FKeyboardParser.SetSystemStoreValue(ssKMW_HelpText, editKMWHelpText.Text);
  EnableControls;
end;

procedure TfrmKeymanWizard.editMessageChange(Sender: TObject);
begin
  if FLoading then Exit;
  FKeyboardParser.SetSystemStoreValue(ssMessage, editMessage.Text);
  EnableControls;
end;

procedure TfrmKeymanWizard.memoCommentsChange(Sender: TObject);
begin
  if FLoading then Exit;
  FKeyboardParser.InitialComment := memoComments.Text;
  Modified := True;
  EnableControls;
end;

procedure TfrmKeymanWizard.KeyboardParserChanged(Sender: TObject);
begin
  if not FLoading then Modified := True;
end;

procedure TfrmKeymanWizard.SourceChanged(Sender: TObject);
begin
  if not FLoading then Modified := True;
end;

procedure TfrmKeymanWizard.StartDebugging(FStartTest: Boolean);

  function KeyboardContainsDebugInformation: Boolean;
  var
    ki: TKeyboardInfo;
    buf: WideString;
  begin
    if not FileExists((ProjectFile as TkmnProjectFile).KmxTargetFilename) then
      Exit(False);

    try
      GetKeyboardInfo((ProjectFile as TkmnProjectFile).KmxTargetFilename, True, ki);   // I4695
      try
        Result := GetSystemStore(ki.MemoryDump.Memory, TSS_DEBUG_LINE, buf);
      finally
        ki.MemoryDump.Free;
      end
    except
      Result := False;
    end;
  end;

var
  FContainsDebugInformation: Boolean;
begin
  if IsInParserMode then   // I4557
    MoveParserToSource;

  pages.ActivePage := pageLayout; //pageSource; { For debugging }
  pagesLayout.ActivePage := pageLayoutCode;

  if not IsDebugVisible then
  begin
    FContainsDebugInformation := KeyboardContainsDebugInformation;

    if not FContainsDebugInformation and
        not FKeymanDeveloperOptions.DebuggerAutoRecompileWithDebugInfo then
    begin
      with TfrmMustIncludeDebug.Create(Application.MainForm) do
      try
        case ShowModal of
          mrOK:     ; // will recompile with debug
          mrYes:    frmKeymanDeveloper.RefreshOptions;
          mrCancel: Exit;
        end;
      finally
        Free;
      end;
    end;

    if FStartTest
      then FDebugForm.UIStatus := duiTest
      else FDebugForm.UIStatus := duiReadyForInput;

    (ProjectFileUI as TkmnProjectFileUI).Debug := True;   // I4687

    frmMessages.Clear;   // I4686

    if not (ProjectFileUI as TkmnProjectFileUI).DoAction(pfaCompile, False) then
    begin
      ShowMessage(SKErrorsInCompile);
      Exit;
    end;

    if not FileExists((ProjectFile as TkmnProjectFile).KmxTargetFilename) then
    begin
      ShowMessage(SKKeyboardKMXDoesNotExist);
      Exit;
    end;

    if not KeyboardContainsDebugInformation then
    begin
      ShowMessage(SKMustIncludeDebug);
      Exit;
    end;

    if FDebugForm.DefaultFont then
      FDebugForm.UpdateFont(nil);
//    FDebugForm.Visible := True;
    FDebugForm.DebugFileName := FileName;
    FDebugForm.CompiledFileName := (ProjectFile as TkmnProjectFile).KmxTargetFilename;   // I4695
    FDebugForm.ShowDebugForm;
  end;
end;

{-----------------------------------------------------------------------------}
{ Keyboard Page                                                               }
{-----------------------------------------------------------------------------}

procedure TfrmKeymanWizard.InitTabKeyboard;
begin
  FCurrentRule := nil;
  CharFont.Name := 'Arial';

  kbdLayout.SelectedKey := kbdLayout.Keys[0];

  UpdateKeyFont;
  Layout_SetAllKeyDetails;
  EnableControls;
end;

procedure TfrmKeymanWizard.Layout_UpdateSelectedKeyDetails(UpdateOutput: Boolean);
var
  k: TKeyboardParser_LayoutRule;
begin
  FCurrentRule := nil;
  FKeyChanging := True;
  try
    k := GetCurrentRule;
    if Assigned(k) then
    begin
      if UpdateOutput then
      begin
        editKeyOutputText.Text := k.Output;
        editKeyOutputText.SelectAll; //SelStart := Length(editKeyOutputText.Text);
        editKeyOutputCode.Text := StringToExtString(k.Output, FKeyboardParser.IsKeyboardUnicode);
        editKeyOutputCode.SelectAll; //SelStart := Length(editKeyOutputCode.Text);
      end;
      keySample.KeyData := k.Output;
    end
    else
    begin
      if UpdateOutput then
      begin
        editKeyOutputText.Text := '';
        editKeyOutputCode.Text := '';
      end;
      keySample.KeyData := '';
    end;

    if Assigned(kbdLayout.SelectedKey) then
    begin
      if (kbdLayout.SelectedKey.ActiveKeyCap = '') or (kbdLayout.SelectedKey.ActiveKeyCap[1] < #32)
        then keySample.KeyText := kbdLayout.SelectedKey.KeyCaps[0]
        else keySample.KeyText  := kbdLayout.SelectedKey.ActiveKeyCap;
      //keySample.KeyText := kbdLayout.SelectedKey.ActiveKeyCap;
      kbdLayout.SelectedKey.KeyValue := keySample.KeyData;
    end;
  finally
    FKeyChanging := False;
  end;

  DoUpdateCharacterMap;

  EnableControls;
end;

procedure TfrmKeymanWizard.kbdLayoutDragDrop(Sender, Source: TObject; X, Y: Integer);
var
  k: TOnScreenKeyboardKey;
  i: Integer;
  rule: TKeyboardParser_LayoutRule;
begin
  k := kbdLayout.GetKeyAtPoint(X, Y);
  if not Assigned(k) or (k.KeyType <> kktNormal) then Exit;

  rule := nil;

  for i := 0 to FKeyboardParser.Lines.Count - 1 do
    if (FKeyboardParser.Lines[i] is TKeyboardParser_LayoutRule) then
    begin
      rule := FKeyboardParser.Lines[i] as TKeyboardParser_LayoutRule;
      if (rule.VKey = k.USVKey) and (rule.Shift = kbdLayout.ShiftState) then Break;
      rule := nil;
    end;

  if not Assigned(rule) then
    rule := FKeyboardParser.AddLayoutRule(k.USVKey, kbdLayout.ShiftState, '');

  with Source as TCharacterDragObject do
  begin
    if GetAsyncKeyState(VK_CONTROL) < 0
      then rule.Output := rule.Output + Text[cmimCharacter]
      else rule.Output := Text[cmimCharacter];
  end;

  k.KeyValue := rule.Output;

  if Assigned(FCurrentRule) and (k.USVKey = FCurrentRule.VKey) then SelectVKey(k.USVKey);

  Modified := True;
end;

procedure TfrmKeymanWizard.kbdLayoutDragOver(Sender, Source: TObject; X, Y: Integer; State: TDragState; var Accept: Boolean);
var
  k: TOnScreenKeyboardKey;
begin
  Accept := Source is TCharacterDragObject;

  if Accept then
  begin
    Accept := False;
    k := kbdLayout.GetKeyAtPoint(X, Y);
    if not Assigned(k) or (k.KeyType <> kktNormal) then Exit;
    if not IsInParserMode then Exit;   // I4557

    Accept := True;
    {with Source as TCharacterDragObject do
    begin
      if IsUnicode and not FKeyboardParser.IsKeyboardUnicode then Accept := False
      else if not IsUnicode and FKeyboardParser.IsKeyboardUnicode then Accept := False;
    end;}
  end;
end;

procedure TfrmKeymanWizard.kbdLayoutSelectionChange(Sender: TObject);
begin
  if kbdLayout.SelectedKey = nil then
  begin
    kbdLayout.SelectedKey := kbdLayout.Keys[0];  // I2584
  end;
  Layout_UpdateSelectedKeyDetails(True);
end;

procedure TfrmKeymanWizard.kbdLayoutShiftChange(Sender: TObject);
begin
  Layout_UpdateShiftKeys;
  Layout_SetAllKeyDetails;
  Layout_UpdateSelectedKeyDetails(True);
end;

procedure TfrmKeymanWizard.Layout_UpdateShiftKeys;
var
  x: Integer;
begin
  x := keySample.Left;
  if (essAlt in kbdLayout.ShiftState) or (essLAlt in kbdLayout.ShiftState) or (essRAlt in kbdLayout.ShiftState) then
  begin
    if essLAlt in kbdLayout.ShiftState then keySampleAlt.KeyText := 'L Alt'
    else if essRAlt in kbdLayout.ShiftState then keySampleAlt.KeyText := 'R Alt'
    else keySampleAlt.KeyText := 'Alt';
    keySampleAlt.Left := x - 3 - keySampleAlt.Width;
    x := keySampleAlt.Left;
    keySampleAlt.Visible := True;
  end
  else
    keySampleAlt.Visible := False;

  if (essCtrl in kbdLayout.ShiftState) or (essLCtrl in kbdLayout.ShiftState) or (essRCtrl in kbdLayout.ShiftState) then
  begin
    if essLCtrl in kbdLayout.ShiftState then keySampleCtrl.KeyText := 'L Ctrl'
    else if essRCtrl in kbdLayout.ShiftState then keySampleCtrl.KeyText := 'R Ctrl'
    else keySampleCtrl.KeyText := 'Ctrl';
    keySampleCtrl.Left := x - 3 - keySampleCtrl.Width;
    x := keySampleCtrl.Left;
    keySampleCtrl.Visible := True;
  end
  else
    keySampleCtrl.Visible := False;

  if essShift in kbdLayout.ShiftState then
  begin
    keySampleShift.Left := x - 3 - keySampleShift.Width;
    keySampleShift.Visible := True;
    keySampleShift.Invalidate;
  end
  else
    keySampleShift.Visible := False;
end;

procedure TfrmKeymanWizard.FocusTabLayout;
begin
  if pagesLayout.ActivePage = pageLayoutDesign   // I4847
    then DoFocus(editKeyOutputText)
    else DoFocus(frameSource);
end;

procedure TfrmKeymanWizard.Layout_SetAllKeyDetails(UpdateLRShift, FixupShiftStates: Boolean);  // I2532   // I4137
var
  k: TOnScreenKeyboardKey;
  i: Integer;
  FOldLayoutSetup, FAShift, FLRShift: Boolean;
  rule: TKeyboardParser_LayoutRule;
begin
  kbdLayout.Keys.ClearValues;

  FLRShift := False;
  FAShift := False;

  for i := 0 to FKeyboardParser.Lines.Count - 1 do
  begin
    if FKeyboardParser.Lines[i] is TKeyboardParser_LayoutRule then
    begin
      rule := FKeyboardParser.Lines[i] as TKeyboardParser_LayoutRule;
      if (rule.Shift = kbdLayout.ShiftState) and (rule.VKey >= 0) and (rule.VKey <= 255) then   // I4533
      begin
        k := kbdLayout.Keys.ItemsByUSVK[rule.VKey];
        if Assigned(k) then
          k.KeyValue := rule.Output;
      end;

      if rule.Shift * [essLCtrl, essLAlt, essRCtrl, essRAlt] <> [] then FLRShift := True;
      if rule.Shift * [essCtrl, essAlt] <> [] then FAShift := True;

    end;
  end;

  // I2532 BEGIN - fix l/r shift transfer from source failure
  panWarnMixedShiftStates.Visible := FLRShift and FAShift;

  if (chkSplitCtrlAlt.Checked <> FLRShift) and UpdateLRShift then  // I2532
  begin
    FOldLayoutSetup := FLayoutSetup;
    FLayoutSetup := True;
    chkSplitCtrlAlt.Checked := FLRShift;

    kbdLayout.LRShift := chkSplitCtrlAlt.Checked;

    FLayoutSetup := FOldLayoutSetup;
  end;
  // I2532 END - fix l/r shift transfer from source failure

  Layout_UpdateSelectedKeyDetails(True);
end;

procedure TfrmKeymanWizard.UpdateKeyFont;
var
  f: TFont;
begin
  f := TFont.Create;
  f.Assign(CharFont);
  f.Size := keySample.DataFont.Size;

  kbdLayout.DataFont := CharFont;

  keySample.DataFont := f;
  keySample.Repaint;

  f.Free;
end;

procedure TfrmKeymanWizard.editKeyboardVersionChange(Sender: TObject);   // I4157
begin
  if FLoading then Exit;
  FKeyboardParser.SetSystemStoreValue(ssKeyboardVersion, editKeyboardVersion.Text);
  EnableControls;
end;

procedure TfrmKeymanWizard.editKeyOutputCodeChange(Sender: TObject);
var
  w: WideString;
  k: TKeyboardParser_LayoutRule;
  FError: Boolean;
begin
  if FKeyChanging then Exit;

  FKeyChanging := True;
  try
    Modified := True;
    w := Trim(editKeyOutputCode.Text);
    k := GetCurrentRule;
    if w <> '' then
    begin
      w := ExtStringToString(w, FError);
      if not Assigned(k)
        then FKeyboardParser.AddLayoutRule(kbdLayout.SelectedKey.USVKey, kbdLayout.ShiftState, w)
        else k.Output := w;

      if FError then editKeyOutputCode.Font.Color := clRed else editKeyOutputCode.Font.Color := clDefault;
    end
    else if Assigned(k) then
    begin
      FKeyboardParser.Lines.Remove(k);
      FCurrentRule := nil;
    end;
    editKeyOutputText.Text := w;
  finally
    FKeyChanging := False;
  end;
  Layout_UpdateSelectedKeyDetails(False);
end;

procedure TfrmKeymanWizard.editKeyOutputCodeClick(Sender: TObject);
begin
  DoUpdateCharacterMap;
end;

procedure TfrmKeymanWizard.editKeyOutputTextChange(Sender: TObject);
var
  w: WideString;
  k: TKeyboardParser_LayoutRule;
begin
  if FKeyChanging then Exit;

  FKeyChanging := True;
  try
    Modified := True;

    w := editKeyOutputText.Text;
    k := GetCurrentRule;
    if w <> '' then
    begin
      if not Assigned(k)
        then FKeyboardParser.AddLayoutRule(kbdLayout.SelectedKey.USVKey, kbdLayout.ShiftState, w)
        else k.Output := w;
    end
    else if Assigned(k) then
    begin
      FKeyboardParser.Lines.Remove(k);
      FCurrentRule := nil;
    end;

    editKeyOutputCode.Text := StringToExtString(w, FKeyboardParser.IsKeyboardUnicode);
    editKeyOutputCode.Font.Color := clWindowText;
  finally
    FKeyChanging := False;
  end;

  Layout_UpdateSelectedKeyDetails(False);
end;

procedure TfrmKeymanWizard.editKeyOutputTextClick(Sender: TObject);
begin
  DoUpdateCharacterMap;
end;

procedure TfrmKeymanWizard.editKeyOutputTextKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  DoUpdateCharacterMap;
end;

procedure TfrmKeymanWizard.tmrUpdateCharacterMapTimer(Sender: TObject);
var
  token: WideString;
  x, token_start: Integer;
  prev_token: WideString;
begin
  tmrUpdateCharacterMap.Enabled := False;
  if editKeyOutputText.Focused then
  begin
    x := editKeyOutputText.SelStart + editKeyOutputText.SelLength;
    token_start := 0;

    token := Copy(editKeyOutputText.Text, x, 1);  // I2843
    if token <> '' then
    begin
      if Uni_IsSurrogate1(token[1]) then token := Copy(editKeyOutputText.Text, x, 2)
      else if Uni_IsSurrogate2(token[1]) and (x > 1) then token := Copy(editKeyOutputText.Text, x-1, 2);
    end;

    token := FormatUnicode(token);
  end
  else if editKeyOutputCode.Focused then
  begin
    x := editKeyOutputCode.SelStart + editKeyOutputCode.SelLength;
    token := GetTokenAtCursor(editKeyOutputCode.Text, x, token_start, prev_token);
  end
  else
    Exit;
  UpdateCharacterMap(False, token, x, token_start, False);   // I4807
end;

procedure TfrmKeymanWizard.DoUpdateCharacterMap;
begin
  tmrUpdateCharacterMap.Enabled := False;
  tmrUpdateCharacterMap.Enabled := True;
end;

procedure TfrmKeymanWizard.SelectVKey(VKey: Integer);
begin
  kbdLayout.SelectedKey := kbdLayout.Keys.ItemsByUSVK[VKey];
  if kbdLayout.SelectedKey = nil then kbdLayout.SelectedKey := kbdLayout.Keys[0]; // I2584
  Layout_UpdateSelectedKeyDetails(True);
end;

function TfrmKeymanWizard.PrepareForBuild(var DebugReset: Boolean): Boolean;   // I4504
var
  FSave: Boolean;
begin
  if IsDebugVisible then
  begin
    if not FKeymanDeveloperOptions.DebuggerAutoResetBeforeCompiling then
    begin
      if TfrmMessageDlgWithSave.Execute(
          'You must reset the debugger before recompiling your keyboard.  Reset the debugger and recompile?',
          'Always reset the debugger automatically before compiling',
          '', True, FSave) in [mrNo, mrCancel] then
        Exit(False);
      if FSave then
      begin
        FKeymanDeveloperOptions.DebuggerAutoResetBeforeCompiling := True;
        FKeymanDeveloperOptions.Write;
      end;
    end;

    DebugReset := True;
    StopDebugging;
  end;

  if not IsInParserMode then   // I4557
    MoveSourceToParser(False);

  Result := True;
end;

function TfrmKeymanWizard.PrintFile: Boolean;
begin
  Result := frameSource.PrintFile(FileName);
end;

{TODO: function TfrmKeymanWizard.PrintPreview: Boolean;
begin
  Result := frameSource.PrintPreview(FileName);
end;}

function TfrmKeymanWizard.GetCurrentRule: TKeyboardParser_LayoutRule;
var
  i: Integer;
  rule: TKeyboardParser_LayoutRule;
begin
  if Assigned(FCurrentRule) then
  begin
    Result := FCurrentRule;
    Exit;
  end;

  if not Assigned(kbdLayout.SelectedKey) then
  begin
    FCurrentRule := nil;
    Result := nil;
    Exit;
  end;

  for i := 0 to FKeyboardParser.Lines.Count - 1 do
  begin
    if (FKeyboardParser.Lines[i] is TKeyboardParser_LayoutRule) then
    begin
      rule := FKeyboardParser.Lines[i] as TKeyboardParser_LayoutRule;

      if (rule.VKey = kbdLayout.SelectedKey.USVKey) and (rule.Shift = kbdLayout.ShiftState) then
      begin
        FCurrentRule := rule;
        Result := rule;
        Exit;
      end;
    end;
  end;

  FCurrentRule := nil;
  Result := nil;
end;

function TfrmKeymanWizard.GetDefaultExt: string;
begin
  Result := 'kmn';
end;

{-----------------------------------------------------------------------------}
{ Bitmap Page                                                                 }
{-----------------------------------------------------------------------------}

procedure TfrmKeymanWizard.InitTabBitmap;
begin
  frameBitmap.OnModifiedChanged := BitmapModifiedChanged;
end;

procedure TfrmKeymanWizard.FocusTabBitmap;
begin
  if frameBitmap.Visible then
    DoFocus(frameBitmap);
end;

procedure TfrmKeymanWizard.BitmapEnableControls;
begin
end;

procedure TfrmKeymanWizard.cmdAddFeatureClick(Sender: TObject);
begin
  with TfrmAddKeyboardFeature.Create(Application.MainForm) do
  try
    KeyboardParser := Self.FKeyboardParser;
    if ShowModal = mrOk then
    begin
      Self.FKeyboardParser.Features.Add(NewFeatureID);

      LoadFeature(NewFeatureID);

      if NewFeatureID = kfTouchLayout then
      begin
        if not FileExists(FFeature[kfTouchLayout].FileName) and not FFeature[kfTouchLayout].Modified then
        begin
          SelectTouchLayoutTemplate(False);
        end;
      end;

      FillFeatureGrid;
      EnableControls;
    end;
  finally
    Free;
  end;
end;

procedure TfrmKeymanWizard.cmdEditFeatureClick(Sender: TObject);
var
  FFeature: TKeyboardParser_Feature;
  c: TTabSheet;
begin
  FFeature := gridFeatures.Objects[0, gridFeatures.Row] as TKeyboardParser_Feature;
  if not Assigned(FFeature) then   // I4587   // I4427
    Exit;
  c := FeatureTab(FFeature.ID);
  if Assigned(c) then
    pages.ActivePage := c;
end;

procedure TfrmKeymanWizard.cmdFixupShiftStatesClick(Sender: TObject);
var
  FOldLayoutSetup: Boolean;
begin
  FOldLayoutSetup := FLayoutSetup;
  FLayoutSetup := True;
  if Layout_FixupRules_LRShift(False) then
  begin
    chkSplitCtrlAlt.Checked := True;
    kbdLayout.LRShift := True;
    FLayoutSetup := FOldLayoutSetup;
    Layout_SetAllKeyDetails;
  end
  else
    FLayoutSetup := FOldLayoutSetup;
end;

procedure TfrmKeymanWizard.LoadFeature(ID: TKeyboardParser_FeatureID);
begin
  if FKeyboardParser.Features.ContainsKey(ID) then
  begin
    FFeature[ID].Filename := FKeyboardParser.Features[ID].ExpandedFilename;
    if FileExists(FFeature[ID].Filename)
      then FileAge(FFeature[ID].Filename, FFeature[ID].OpenFileAge)
      else FFeature[ID].OpenFileAge := 0;
  end
  else
    FFeature[ID].Filename := '';

  case ID of
    kfIcon:
      begin
        LoadBitmap;
      end;
    kfOSK:
      begin
        LoadOSK;   // I4034
      end;
    kfTouchLayout:
      begin
        LoadTouchLayout;   // I4034
      end;
    else
    begin
      if FKeyboardParser.Features.ContainsKey(ID) then
      begin
        if not Assigned(FFeature[ID].Frame) then
          InitFeatureTab(ID);

        if FileExists(FFeature[ID].Filename)
          then FFeature[ID].Frame.LoadFromFile(FFeature[ID].Filename, tffUTF8)   // I4034
          else FFeature[ID].Frame.EditorText := '';
      end;
    end;
  end;
  FFeature[ID].Modified := False;
end;

function TfrmKeymanWizard.SaveFeature(ID: TKeyboardParser_FeatureID): Boolean;
begin
  try
    if FKeyboardParser.Features.ContainsKey(ID) then
    begin
      FFeature[ID].Filename := FKeyboardParser.Features[ID].ExpandedFilename;

      case ID of
        kfIcon: SaveBitmap;
        kfOSK: SaveOSK;
        kfTouchLayout: SaveTouchLayout;
        else FFeature[ID].Frame.SaveToFile(FFeature[ID].Filename);
      end;

      FileAge(FFeature[ID].FileName, FFeature[ID].OpenFileAge);

    end;

    FFeature[ID].Modified := False;
    Result := True;
  except
    on E:EFCreateError do   // I4728
    begin
      ShowMessage(E.Message);
      Result := False;
    end;
  end;
end;

procedure TfrmKeymanWizard.InitFeatureTab(ID: TKeyboardParser_FeatureID);
var
  c: TTabSheet;
begin
  if ID in [kfIcon, kfOSK, kfTouchLayout] then
    Exit; // These are pre-init

  c := FeatureTab(ID);
  if Assigned(c) and not Assigned(FFeature[ID].Frame) then
  begin
    FFeature[ID].Frame := TframeTextEditor.Create(Self);
    FFeature[ID].Frame.EditorFormat := KeyboardFeatureEditorFormat[ID];
    FFeature[ID].Frame.TextFileFormat := tffUTF8;
    FFeature[ID].Frame.Parent := c;
    FFeature[ID].Frame.Align := alClient;
    FFeature[ID].Frame.OnChanged := FeatureModified;
    FFeature[ID].Frame.Visible := True;
  end;
end;

procedure TfrmKeymanWizard.FeatureModified(Sender: TObject);
var
  i: TKeyboardParser_FeatureID;
begin
  if not FLoading then
  begin
    for i := Low(TKeyboardParser_FeatureID) to High(TKeyboardParser_FeatureID) do
      if FFeature[i].Frame = Sender then
        FFeature[i].Modified := True;
    Modified := True;
  end;
end;

procedure TfrmKeymanWizard.LoadBitmap;
begin
  if FKeyboardParser.Features.ContainsKey(kfIcon) then
  begin
    if (FFeature[kfIcon].Filename <> '') and FileExists(FFeature[kfIcon].Filename)
      then frameBitmap.LoadFromFile(FFeature[kfIcon].Filename)
      else frameBitmap.Clear;
  end;
end;

function TfrmKeymanWizard.SaveBitmap: Boolean;
begin
  if FKeyboardParser.Features.ContainsKey(kfIcon) then
    frameBitmap.SaveToFile(FKeyboardParser.Features[kfIcon].ExpandedFilename);
  Result := True;
end;

procedure TfrmKeymanWizard.chkKMWRTLClick(Sender: TObject);
begin
  if FLoading then Exit;
  if chkKMWRTL.Checked
    then FKeyboardParser.SetSystemStoreValue(ssKMW_RTL, '1')
    else FKeyboardParser.DeleteSystemStore(ssKMW_RTL);
end;

procedure TfrmKeymanWizard.chkLayoutDisplay102KeyClick(Sender: TObject);
begin
  kbdLayout.Display102Key := chkLayoutDisplay102Key.Checked;
end;

procedure TfrmKeymanWizard.BitmapModifiedChanged(Sender: TObject);
begin
  if frameBitmap.Modified then Modified := True;
  FFeature[kfIcon].Modified := frameBitmap.Modified;
end;

{ ----------------------------------------------------------------------------- }
{ Finish Page }
{ ----------------------------------------------------------------------------- }

procedure TfrmKeymanWizard.InitTabFinish;
begin
end;

procedure TfrmKeymanWizard.FocusTabFinish;
begin
  DoFocus(cmdCompile);
end;

{-----------------------------------------------------------------------------}
{ EOF                                                                         }
{-----------------------------------------------------------------------------}

function TfrmKeymanWizard.GetFileNameFilter: string;
begin
  Result := 'Keyboard definitions (*.kmn)|*.kmn';
end;

function TfrmKeymanWizard.GetFontInfo(Index: TKeyboardFont): TKeyboardFontInfo;   // I4057
begin
  case Index of
    kfontCode:
      begin
        Result.Enabled := True;
        Result.Name := CodeFont.Name;
        Result.Size := IntToStr(TFontUtils.FontSizeInPoints(CodeFont.Name, CodeFont.Size));   // I4872
      end;
    kfontChar:
      begin
        Result.Enabled := True;
        Result.Name := CharFont.Name;
        Result.Size := IntToStr(TFontUtils.FontSizeInPoints(CharFont.Name, CharFont.Size));   // I4872
      end;
    kfontOSK:
      begin
        Result.Enabled := FKeyboardParser.Features.ContainsKey(kfOSK);   // I4258   // I4557
        if Result.Enabled then
        begin
          Result.Name := frameOSK.KeyFont.Name;
          Result.Size := IntToStr(TFontUtils.FontSizeInPoints(frameOSK.KeyFont.Name, frameOSK.KeyFont.Size));   // I4872
        end;
      end;
    kfontTouchLayoutPhone, kfontTouchLayoutTablet, kfontTouchLayoutDesktop:
      begin
        Result.Enabled := FKeyboardParser.Features.ContainsKey(kfTouchLayout);   // I4258   // I4557
        if Result.Enabled then   // I4872
        begin
          Result := frameTouchLayout.FontInfo[Index];
          if Copy(Result.Size, Length(Result.Size)-1, 2) = 'em' then
            Delete(Result.Size, Length(Result.Size)-1, 2);
        end;
      end;
  end;
end;

function TfrmKeymanWizard.GetHelpTopic: string;
begin
  Result := SHelpTopic_Context_KeyboardEditor;
end;

function TfrmKeymanWizard.GetIsDebugVisible: Boolean;
begin
  Result := panDebugHost.Visible;
end;

function TfrmKeymanWizard.GetProjectFile: TProjectFile;
begin
  Result := inherited GetProjectFile;
  if not Assigned(Result) then
  begin
    FStandaloneProjectFile := TkmnProjectFileAction.Create(nil, FileName, nil);
    Result := FStandaloneProjectFile;
  end;
end;

procedure TfrmKeymanWizard.gridFeaturesClick(Sender: TObject);
begin
  EnableControls;
end;

procedure TfrmKeymanWizard.gridFeaturesDblClick(Sender: TObject);
begin
  if cmdEditFeature.Enabled then
    cmdEditFeature.Click;
end;

procedure TfrmKeymanWizard.gridWinLanguagesClick(Sender: TObject);
var
  gx: TGridCoord;
begin
  with gridWinLanguages.ScreenToClient(Mouse.CursorPos) do
    gx := gridWinLanguages.MouseCoord(X, Y);
  if (gx.Y > 0) and (gx.X = 2) then
  begin
    ToggleWinLanguagesRow(gx.Y);
  end;
end;

procedure TfrmKeymanWizard.ToggleWinLanguagesRow(Y: Integer);
var
  i: Integer;
begin
  FCheckboxGridHelper.Click(2, Y);
  (gridWinLanguages.Objects[0, Y] as TKeyboardParser_WinLanguage).IsDefault :=
    FCheckboxGridHelper.IsChecked(2, Y);
  for i := 1 to gridWinLanguages.RowCount - 1 do
  begin
    if i <> Y then
    begin
      (gridWinLanguages.Objects[0, i] as TKeyboardParser_WinLanguage).IsDefault := False;
      FCheckboxGridHelper.Checked[2, i] := False;
    end;
  end;
end;

procedure TfrmKeymanWizard.gridWinLanguagesDrawCell(Sender: TObject; ACol,
  ARow: Integer; Rect: TRect; State: TGridDrawState);
begin
  if (ACol = 2) and (ARow > 0) then
    FCheckboxGridHelper.DrawCell(ACol, ARow, Rect, State);
end;

procedure TfrmKeymanWizard.gridWinLanguagesKeyPress(Sender: TObject;
  var Key: Char);
begin
  if Key = ' ' then
  begin
    ToggleWinLanguagesRow(gridWinLanguages.Row);
    Key := #0;
  end;
end;

function TfrmKeymanWizard.HasSubfilename(const Filename: string): Boolean;   // I4081
var
  i: TKeyboardParser_FeatureID;
begin
  for i := Low(FFeature) to High(FFeature) do
    if FFeature[i].FileName = Filename then
      Exit(True);
  Result := False;
end;

procedure TfrmKeymanWizard.RefreshOptions;
begin
end;

procedure TfrmKeymanWizard.ReloadAsTextFileFormatClick(
  TextFileFormat: TTextFileFormat);   // I3637
begin
  if Modified then
  begin
    if MessageDlg('This action will cause the file to be reloaded from disk and you will lose all changes you have made to the file.  Continue and lose changes?',
        mtConfirmation, mbYesNoCancel, 0) <> mrYes then Exit;
  end;

  DoOpenFileFormat(TextFileFormat, True);
  frmKeymanDeveloper.cbTextFileFormat.ItemIndex := Ord(TextFileFormat);
end;

procedure TfrmKeymanWizard.DebugClearBreakpoint(Sender: TObject;
  ALine: Integer);
begin
  frameSource.DebugClearBreakpoint(ALine);
end;

procedure TfrmKeymanWizard.DebugSetBreakpoint(Sender: TObject; ALine: Integer);
begin
  frameSource.DebugSetBreakpoint(ALine);
end;

procedure TfrmKeymanWizard.DebugUpdateExecutionPoint(Sender: TObject; ALine: Integer);
begin
  frameSource.DebugUpdateExecutionPoint(ALine);
end;

procedure TfrmKeymanWizard.DoFocus(control: TWinControl);
begin
  if control.Enabled and control.Visible and control.Showing and control.CanFocus then
    control.SetFocus;
{
  if control.Visible and control.Showing and control.CanFocus then
  begin
    frmKeymanDeveloper.SetFocusedControl(frmKeymanDeveloper);
    control.SetFocus;
  end;
}
end;

function TfrmKeymanWizard.DoOpenFile: Boolean;
begin
  Result := DoOpenFileFormat(tffUTF8, False);   // I3637
end;

function TfrmKeymanWizard.DoOpenFileFormat(FFormat: TTextFileFormat; FUseFormat: Boolean): Boolean;   // I3637
var
  FLastFileCharSet: TEncoding;
  kf: TKeyboardParser_FeatureID;
begin
  Modified := False;
  FLoading := True;

  pages.ActivePage := pageDetails;

  with TStringList.Create do
  try
    if FileExists(FileName) then
    begin
      try
        if FUseFormat then
          LoadFromFile(FileName, TextFileFormatToEncoding(FFormat))   // I3637
        else
          LoadFromFile(FileName, TEncoding.UTF8); // Let prolog determine encoding  // I3337
      except
        on E:EEncodingError do
          LoadFromFile(FileName);
      end;
      FLastFileCharSet := Encoding;// LastFileCharSet;
    end
    else
      FLastFileCharSet := TEncoding.UTF8;

    FKeyboardParser.FileName := FileName;
    FKeyboardParser.KeyboardText := Text;

    if FLastFileCharSet = TEncoding.Default then TextFileFormat := tffANSI  // I3337
    else if FLastFileCharSet = TEncoding.Unicode then TextFileFormat := tffUTF16
    else TextFileFormat := tffUTF8;
  finally
    Free;
  end;

  UpdateControls(True);   // I4137
  Layout_UpdateCharacterSet;
  LoadSettings;

  for kf in FKeyboardParser.Features.Keys do
    LoadFeature(kf);

  if FKeyboardParser.IsComplex then   // I4557
    pagesLayout.ActivePage := pageLayoutCode;

  if FKeymanDeveloperOptions.OpenKeyboardFilesInSourceView then   // I4751
  begin
    MoveParserToSource;   // I4751
    pages.ActivePage := pageLayout;
    pagesLayout.ActivePage := pageLayoutCode;
  end;

  // if pages.ActivePage <> pageSource then
  // UpdateControls;
  FLoading := False;
  Result := True;
end;

function TfrmKeymanWizard.IsInParserMode: Boolean;   // I4557
begin
  Result := (pages.ActivePage <> pageLayout) or (pagesLayout.ActivePage <> pageLayoutCode);
end;

function TfrmKeymanWizard.ValidateFileHasNoUnicodeCharacters(sw: WideString): Boolean;
begin
  if WideString(AnsiString(sw)) <> sw then
  begin
    case MessageDlg('The file '+FileName+' is currently in ANSI format but the '+
        'editor contains Unicode characters that cannot be represented in ANSI and will be lost '+
        'if you continue. Do you want to change the format to UTF-8 to preserve the content?',
        mtWarning, mbYesNoCancel, 0) of
      mrYes: SetTextFileFormat(tffUTF8);
      mrNo: ;
      mrCancel: Exit(False);
    end;
  end;
  Exit(True);
end;

function TfrmKeymanWizard.DoSaveFile: Boolean;
var
  sw: WideString;
  FEncoding: TEncoding;
  FKey: TKeyboardParser_FeatureID;

begin
  Result := False;

  if not IsInParserMode then   // I4557
    MoveSourceToParser(True);

  try
    FKeyboardParser.AddRequiredLines;

    sw := FKeyboardParser.KeyboardText;

    if (FTextFileFormat = tffANSI) and
      not ValidateFileHasNoUnicodeCharacters(sw) then Exit;

    for FKey in FKeyboardParser.Features.Keys do
    begin
      SaveFeature(FKey);
    end;

    try
      case FTextFileFormat of
        tffANSI: FEncoding := TEncoding.Default;
        tffUTF8: FEncoding := TEncoding.UTF8;  // I3337
        tffUTF16: FEncoding := TEncoding.Unicode;
        else raise Exception.Create('Invalid encoding');
      end;

      with TStringList.Create do  // I3337
      try
        Text := sw;
        SaveToFile(FileName, FEncoding);
      finally
        Free;
      end;
    except
      on E:EStreamError do
      begin
        ShowMessage(E.Message);
        Exit;
      end;
    end;
  finally
    if not IsInParserMode then   // I4557
      MoveParserToSource;
  end;

  Result := True;
end;

procedure TfrmKeymanWizard.lbDebugHostsClick(Sender: TObject);
begin
  UpdateQRCode;
end;

procedure TfrmKeymanWizard.lbWinLang_SupportedClick(Sender: TObject);
begin
  EnableControls;
end;

procedure TfrmKeymanWizard.FindError(const Filename: string; s1: string; line: Integer);   // I4081
var
  ln: Integer;
  f: TframeTextEditor;

  procedure Activate(tab: TTabSheet; control: TWinControl);
  begin
    pages.ActivePage := tab; control.SetFocus;
  end;

  procedure FindKmnError;
  var
    FUnknown: Boolean;
  begin
    FUnknown := False;
    if ln >= 0 then
    begin
      if IsInParserMode then   // I4557
      begin
        if (ln < 0) or (ln >= FKeyboardParser.Lines.Count) then
          ShowMessage(SUnknownError)
        else
        begin
          if FKeyboardParser.Lines[ln] is TKeyboardParser_SystemStore then
          begin
            case (FKeyboardParser.Lines[ln] as TKeyboardParser_SystemStore).SystemStoreType of
              ssBitmap:
                pages.ActivePage := pageIcon;
              ssCopyright:
                Activate(pageDetails, editCopyright);
              ssMessage:
                Activate(pageDetails, editMessage);
              ssKeyboardVersion:
                Activate(pageDetails, editKeyboardVersion); // I4157
              ssName:
                Activate(pageDetails, editName);
              ssTargets:
                Activate(pageDetails, clbTargets);   // I4504
              ssEthnologueCode:   // I4766
                Activate(pageDetails, editEthnologueCode);
            else
              FUnknown := True;
            end;
            if not FUnknown then Exit;
          end
          else if (FKeyboardParser.Lines[ln] is TKeyboardParser_LayoutRule) then
          begin
            if FKeyboardParser.IsComplex then
            begin
              pages.ActivePage := pageLayout;
              pagesLayout.ActivePage := pageLayoutCode;
              frameSource.FindError(ln);
            end
            else
            begin
              pages.ActivePage := pageLayout;
              pagesLayout.ActivePage := pageLayoutDesign;
              with FKeyboardParser.Lines[ln] as TKeyboardParser_LayoutRule do
              begin
                kbdLayout.ShiftState := Shift;
                Layout_SetAllKeyDetails;
                SelectVKey(VKey);
                Exit;
              end;
            end;
          end
          else
            FUnknown := True;   // I4765
        end;
      end
      else
        FUnknown := True;

      if FUnknown then
      begin
        if IsInParserMode then   // I4765
          MoveParserToSource;
        pages.ActivePage := pageLayout;
        pagesLayout.ActivePage := pageLayoutCode;
        frameSource.FindError(ln);
      end;
    end;
  end;

  function FindEditor(c: TWinControl): TframeTextEditor;
  var
    I: Integer;
  begin
    for I := 0 to c.ControlCount - 1 do
      if c.Controls[i] is TframeTextEditor then
        Exit(c.Controls[i] as TframeTextEditor)
      else if c.Controls[i] is TWinControl then
      begin
        Result := FindEditor(c.Controls[i] as TWinControl);
        if Result <> nil then
          Exit;
      end;
    Result := nil;
  end;

var
  kf: TKeyboardParser_FeatureID;
  c: TTabSheet;
begin
  ln := line - 1;
  if ln < 0 then
    ln := 0;

  if SameText(Filename, Self.Filename) then
    FindKmnError
  else
    for kf := Low(FFeature) to High(FFeature) do
    begin
      if SameText(Filename, FFeature[kf].Filename) then
      begin
        c := FeatureTab(kf);
        if Assigned(c) and c.TabVisible then
        begin
          pages.ActivePage := c;
          f := FindEditor(c);
          if Assigned(f) then
            f.FindError(ln);
        end;
      end;
    end;
end;

procedure TfrmKeymanWizard.ConfirmSaveOfOldEditorWindow(FeatureID: TKeyboardParser_FeatureID; FModified: Boolean; const FOldFilename: string; DoSave: TProc; DoLoad: TProc<String>);
var
  FText: string;
  FHasFile: Boolean;
  FFileHasChanged: Boolean;
begin
  FHasFile := FKeyboardParser.Features.ContainsKey(FeatureID);
  FFileHasChanged := not FHasFile or not SameText(FKeyboardParser.Features[FeatureID].ExpandedFilename, FOldFilename);

  if FModified and not FFileHasChanged then
    // Destination file has been modified and the file has changed.
    Exit;

  if FModified then
  begin
    if FHasFile
      then FText := 'The file '+FOldFilename+' has been modified.  Do you want to save it before loading '+FKeyboardParser.Features[FeatureID].ExpandedFilename+'?'
      else FText := 'The file '+FOldFilename+' has been modified.  Do you want to save it before removing it from the keyboard?';
    case MessageDlg(FText, mtConfirmation, mbYesNoCancel, 0) of
      mrYes: DoSave;
      mrNo:  ;
      mrCancel:
        begin
          if Untitled
            then FKeyboardParser.SetSystemStoreValue(KeyboardFeatureStore[FeatureID], FOldFilename)
            else FKeyboardParser.SetSystemStoreValue(KeyboardFeatureStore[FeatureID], ExtractRelativePath(FileName, FOldFilename));
          Exit;
        end;
    end;
  end;

  if FHasFile and FFileHasChanged then
    DoLoad(FKeyboardParser.Features[FeatureID].ExpandedFilename);
end;

procedure TfrmKeymanWizard.ConfirmSaveOfOldEditorWindows;
var
  kf: TKeyboardParser_FeatureID;
begin
  ConfirmSaveOfOldEditorWindow(kfIcon, FFeature[kfIcon].Modified, FFeature[kfIcon].Filename,
    procedure begin
      frameBitmap.SaveToFile(FFeature[kfIcon].Filename);
    end,
    procedure(NewFilename: string) begin
      FFeature[kfIcon].Filename := NewFilename;
      LoadFeature(kf);
    end
  );

  ConfirmSaveOfOldEditorWindow(kfOSK, FFeature[kfOSK].Modified, FFeature[kfOSK].FileName,
    procedure begin
      frameOSK.Save;
    end,
    procedure(NewFilename: string) begin
      FFeature[kfOSK].Filename := NewFilename;
      LoadFeature(kf);
    end
  );

  ConfirmSaveOfOldEditorWindow(kfTouchLayout, FFeature[kfTouchLayout].Modified, FFeature[kfTouchLayout].FileName,
    procedure begin
      SaveTouchLayout;   // I4034
    end,
    procedure(NewFilename: string) begin
      FFeature[kfTouchLayout].Filename := NewFilename;
      LoadFeature(kf);
    end
  );

  for kf := Low(TKeyboardParser_FeatureID) to High(TKeyboardParser_FeatureID) do
    if not (kf in [kfIcon, kfOSK, kfTouchLayout]) then
      ConfirmSaveOfOldEditorWindow(kf, FFeature[kf].Modified, FFeature[kf].FileName,
        procedure begin
          FFeature[kf].Frame.SaveToFile(FFeature[kf].Filename);
        end,
        procedure(NewFilename: string) begin
          FFeature[kf].Filename := NewFilename;
          LoadFeature(kf);
        end
      );

  // TODO: other file types
end;

procedure TfrmKeymanWizard.ControlKeyPressedAndReleased;
var
  frmSelectKey: TfrmSelectKey;
begin
  if (Pages.ActivePage = pageLayout) and (pagesLayout.ActivePage = pageLayoutDesign) then
  begin
    frmSelectKey := TfrmSelectKey.Create(Application.MainForm);
    try
      frmSelectKey.DistinguishLeftRight := chkSplitCtrlAlt.Checked;
      if frmSelectKey.ShowModal = mrOk then
      begin
        kbdLayout.ShiftState := frmSelectKey.ShiftState;
        Layout_SetAllKeyDetails;
        SelectVKey(frmSelectKey.VKey);
      end;
    finally
      frmSelectKey.Free;
    end;
  end
  else if Pages.ActivePage = pageOnScreenKeyboard then
  begin
    frameOSK.ControlKeyPressedAndReleased;
  end;
end;

procedure TfrmKeymanWizard.UpdateControls(UpdateLRShift: Boolean);   // I4137
var
  i, n: Integer;
  nb: Integer;
begin
  //   // I4723 - removed line
  FLoading := True;
  Layout_SetAllKeyDetails(UpdateLRShift);  // I2532   // I4137
  editName.Text := FKeyboardParser.GetSystemStoreValue(ssName);
  SetTargetsListBoxFromTargets(FKeyboardParser.GetSystemStoreValue(ssTargets));   // I4504
  editMessage.Text := FKeyboardParser.GetSystemStoreValue(ssMessage);
  editCopyright.Text := FKeyboardParser.GetSystemStoreValue(ssCopyright);
  editKeyboardVersion.Text := FKeyboardParser.GetSystemStoreValue(ssKeyboardVersion);   // I4157

  editKMWHelpText.Text := FKeyboardParser.GetSystemStoreValue(ssKMW_HelpText);
  chkKMWRTL.Checked := (FKeyboardParser.GetSystemStoreValue(ssKMW_RTL) = '1');

  FillFeatureGrid;

  nb := 0;
  for i := 0 to FKeyboardParser.Lines.Count - 1 do
    if FKeyboardParser.Lines[i] is TKeyboardParser_Begin then
      Inc(nb);

  n := FKeyboardParser.Lines.IndexOfClass(TKeyboardParser_Begin);

  if (nb > 1) or (n = -1) then
    lblType.Caption := 'Unknown character set'
  else if (FKeyboardParser.Lines[n] as TKeyboardParser_Begin).IsUnicode then
  begin
    lblType.Caption := '';
    frameOSK.VKUnicode := True;
  end
  else
  begin
    lblType.Caption := 'Legacy character set';
    frameOSK.VKUnicode := False;
  end;

  Layout_UpdateCharacterSet;

  if FKeyboardParser.Features.ContainsKey(kfOSK) then
  begin
    frameOSK.KMXFileName := (ProjectFile as TkmnProjectFile).KmxTargetFilename;   // I4695
    frameOSK.UpdateControls;
  end;

  Languages_UpdateControls;

  UpdateWizardForTargets;   // I4504

  memoComments.Text := FKeyboardParser.InitialComment;
  FLoading := False;
end;

function TfrmKeymanWizard.FeatureTab(kf: TKeyboardParser_FeatureID): TTabSheet;
begin
  Result := FindComponent(KeyboardFeatureTabName[kf]) as TTabSheet;
end;

procedure TfrmKeymanWizard.FillFeatureGrid;
var
  n: Integer;
  kf: TKeyboardParser_FeatureID;
  i: System.Generics.Collections.TPair<KeyboardParser.TKeyboardParser_FeatureID,KeyboardParser.TKeyboardParser_Feature>;
  c: TTabSheet;
begin
  gridFeatures.RowCount := FKeyboardParser.Features.Count + 1;
  if gridFeatures.RowCount > 1 then   // I4427   // I4587
    gridFeatures.FixedRows := 1;

  gridFeatures.Cells[0, 0] := 'Feature';
  gridFeatures.Cells[1, 0] := 'Filename';

  for kf := Low(kf) to High(kf) do
  begin
    c := FeatureTab(kf);
    if Assigned(c) then
      c.TabVisible := FKeyboardParser.Features.ContainsKey(kf);
  end;

  n := 1; // row index
  for i in FKeyboardParser.Features do
  begin
    gridFeatures.Cells[0, n] := KeyboardFeatureName[i.Key];
    gridFeatures.Objects[0, n] := i.Value;
    gridFeatures.Cells[1, n] := i.Value.Filename;
    Inc(n);
  end;

  EnableControls;   // I4587   // I4427
end;

{ Other buttons }

function TfrmKeymanWizard.CanReloadAsTextFileFormatClick: Boolean;   // I3637
begin
  Result := True;
end;

function TfrmKeymanWizard.CanTextFileFormatClick: Boolean;
begin
  Result := True;
end;

function TfrmKeymanWizard.CanChangeTab(FForward: Boolean): Boolean;   // I4678
begin
  Result := True;
end;

function TfrmKeymanWizard.CanChangeView(FView: TCodeDesignView): Boolean;   // I4678
begin
  Result :=
    (pages.ActivePage = pageLayout) or
    (pages.ActivePage = pageTouchLayout);
end;

procedure TfrmKeymanWizard.ChangeTab(FForward: Boolean);   // I4678
begin
  pages.SelectNextPage(FForward);
end;

procedure TfrmKeymanWizard.ChangeView(FView: TCodeDesignView);   // I4678
begin
  if pages.ActivePage = pageLayout then
  begin
//    CheckParserSourceMove(False);
    case FView of
      cdvDesign: if pagesLayout.ActivePage <> pageLayoutDesign then pagesLayout.SelectNextPage(False);
      cdvCode: if pagesLayout.ActivePage <> pageLayoutCode then pagesLayout.SelectNextPage(True);
    end
  end
  else if pages.ActivePage = pageTouchLayout then
    case FView of
      cdvDesign: if pagesTouchLayout.ActivePage <> pageTouchLayoutDesign then pagesTouchLayout.SelectNextPage(False);
      cdvCode:   if pagesTouchLayout.ActivePage <> pageTouchLayoutCode then pagesTouchLayout.SelectNextPage(True);
    end
  else if pages.ActivePage = pageOnScreenKeyboard then
    case FView of
      cdvDesign: if frameOSK.pages.ActivePage <> frameOSK.pageDesign then frameOSK.pages.SelectNextPage(False);
      cdvCode:   if frameOSK.pages.ActivePage <> frameOSK.pageCode then frameOSK.pages.SelectNextPage(False);
    end

end;

procedure TfrmKeymanWizard.CharFontChanged;
begin
  inherited;
  UpdateKeyFont;
  //VK_UpdateKeyFont;
  frameSource.CharFont := CharFont;
  editName.Font := CharFont;
  editCopyright.Font := CharFont;
  editMessage.Font := CharFont;
  editKMWHelpText.Font := CharFont;
  memoComments.Font := CharFont;
  editKeyOutputText.Font := CharFont;
  if Assigned(FDebugForm) and FDebugForm.DefaultFont then
    FDebugForm.UpdateFont(nil);
end;

procedure TfrmKeymanWizard.LoadSettings;
var
  FFont: TFont;
begin
  inherited;

  chkLayoutDisplay102Key.Checked := ProjectFile.IDEState['LayoutDisplay102Key'] = '1';   // I4702
  chkLayoutDisplay102KeyClick(chkLayoutDisplay102Key);
  if ProjectFile.IDEState['DebugDefaultFont'] <> '0' then   // I4702
    FDebugForm.UpdateFont(nil)
  else
  begin
    FFont := TFont.Create;
    try
      SetFontFromString(FFont, ProjectFile.IDEState['DebugFont']);   // I4702
      FDebugForm.UpdateFont(FFont);
    finally
      FFont.Free;
    end;
  end;
end;

procedure TfrmKeymanWizard.SaveSettings(SaveProject: Boolean);
begin
  if chkLayoutDisplay102Key.Checked
    then ProjectFile.IDEState['LayoutDisplay102Key'] := '1'   // I4702
    else ProjectFile.IDEState['LayoutDisplay102Key'] := '0';   // I4702

  if FDebugForm.DefaultFont
    then ProjectFile.IDEState['DebugDefaultFont'] := '1'   // I4702
    else ProjectFile.IDEState['DebugDefaultFont'] := '0';   // I4702

  ProjectFile.IDEState['DebugFont'] := FontAsString(FDebugForm.memo.Font);   // I4702

  inherited;
end;

{ System keyboard }

procedure TfrmKeymanWizard.InitSystemKeyboard;
begin
  FLoadedSystemKeyboard := False;
end;

procedure TfrmKeymanWizard.UninitSystemKeyboard;
begin
  if FLoadedSystemKeyboard then
    UnloadKeyboardLayout(FUnderlyingLayout);
  FLoadedSystemKeyboard := False;
end;

procedure TfrmKeymanWizard.mnuViewUnderlyingLayoutClick(Sender: TObject);
var
  FSystemKeyboardName: string;
begin
  if SelectSystemKeyboard(frmKeymanDeveloper, FSystemKeyboardName) then
    FUnderlyingLayout := LoadSystemKeyboard(FLoadedSystemKeyboard, FSystemKeyboardName);
end;

procedure TfrmKeymanWizard.MoveParserToSource;   // I4557
begin
  FCurrentRule := nil;
  FKeyboardParser.AddRequiredLines;
  frameSource.EditorText := FKeyboardParser.KeyboardText;
end;

procedure TfrmKeymanWizard.MoveSourceToParser(UpdateLRShift: Boolean);   // I4137
begin
  FCurrentRule := nil;
  FKeyboardParser.FileName := FileName;
  FKeyboardParser.KeyboardText := frameSource.EditorText;
  ConfirmSaveOfOldEditorWindows;
  UpdateControls(UpdateLRShift);   // I4137
end;

procedure TfrmKeymanWizard.NotifyStartedWebDebug;
begin
  lbDebugHosts.Clear;
  TServerDebugAPI.GetServerURLs(lbDebugHosts.Items);
  if lbDebugHosts.Items.Count > 0 then
    lbDebugHosts.ItemIndex := 0;
  UpdateQRCode;
  EnableControls;
end;

procedure TfrmKeymanWizard.WMSysCommand(var Message: TWMSysCommand);
begin
  if ((Message.CmdType = SC_NEXTWINDOW) or (Message.CmdType = SC_PREVWINDOW)) and (Message.XPos = 9)
    then PostMessage(Handle, CM_DIALOGKEY, VK_TAB, 0)
    else inherited;
end;

procedure TfrmKeymanWizard.WMUserFormShown(var Message: TMessage);
begin
  inherited;
  FocusTab;
end;

procedure TfrmKeymanWizard.WMUserInputLangChange(var Message: TMessage);
begin
  kbdLayout.UnderlyingLayout := GetKeyboardLayout(0);
  kbdLayout.Repaint;
  frameOSK.UnderlyingLayout := GetKeyboardLayout(0);
end;

procedure TfrmKeymanWizard.FormShow(Sender: TObject);
begin
  inherited;
  PostMessage(Handle, WM_USER_FormShown, 0, 0);
end;

procedure TfrmKeymanWizard.chkSplitCtrlAltClick(Sender: TObject);
begin
  if FLayoutSetup then Exit;

  FLayoutSetup := True;
  try
    if not Layout_FixupRules_LRShift(chkSplitCtrlAlt.Checked) then
      chkSplitCtrlAlt.Checked := not chkSplitCtrlAlt.Checked;

    kbdLayout.LRShift := chkSplitCtrlAlt.Checked;

    Layout_SetAllKeyDetails;
    Layout_FocusOutput;
  finally
    FLayoutSetup := False;
  end;
end;

function TfrmKeymanWizard.Layout_FixupRules_LRShift(FSplit: Boolean): Boolean;
var
  i, j: Integer;
  Found: Boolean;
  rule: TKeyboardParser_LayoutRule;
  rule2: TKeyboardParser_LayoutRule;
begin
  Result := False;
  if FSplit then
  begin
    { Check if any lctrl/lalt/rctrl/alt are currently defined,  }
    { warn that they will all be merged and some entries may be }
    { lost if they conflict }

    Found := False;
    i := FKeyboardParser.Lines.IndexOfClass(TKeyboardParser_LayoutRule, -1);
    while i > 0 do
    begin
      rule := FKeyboardParser.Lines[i] as TKeyboardParser_LayoutRule;

      if (essLCtrl in rule.Shift) or (essLAlt in rule.Shift) or
          (essRCtrl in rule.Shift) or (essRAlt in rule.Shift) then
      begin
        Found := True;
        Break;
      end;
      i := FKeyboardParser.Lines.IndexOfClass(TKeyboardParser_LayoutRule, i);
    end;

    if Found and (MessageDlg('You have already defined some keys to have left or right-specific states.'+
        '  If you continue, these shift states will be merged and you may lose some key states.'#13#10#13#10 +
        'Continue and merge shift states?', mtConfirmation, mbOkCancel, 0) = mrCancel) then
      Exit;

    i := FKeyboardParser.Lines.IndexOfClass(TKeyboardParser_LayoutRule, -1);
    while i > 0 do
    begin
      Found := False;
      rule := FKeyboardParser.Lines[i] as TKeyboardParser_LayoutRule;
      if (essLCtrl in rule.Shift) or (essRCtrl in rule.Shift) then
      begin
        rule.Shift := rule.Shift - [essLCtrl, essRCtrl] + [essCtrl];
        Found := True;
      end;
      if (essLAlt in rule.Shift) or (essRAlt in rule.Shift) then
      begin
        rule.Shift := rule.Shift - [essLAlt, essRAlt] + [essAlt];
        Found := True;
      end;
      if Found then
      begin
        for j := 0 to i-1 do
        begin
          if FKeyboardParser.Lines[j] is TKeyboardParser_LayoutRule then
          begin
            rule2 := FKeyboardParser.Lines[j] as TKeyboardParser_LayoutRule;
            if (rule2.Shift = rule.Shift) and (rule2.VKey = rule.VKey) then
            begin
              FKeyboardParser.Lines.Delete(i);
              Dec(i); // Search from previous line - because we just deleted the current line
              Break;
            end;
          end;
        end;
      end;
      i := FKeyboardParser.Lines.IndexOfClass(TKeyboardParser_LayoutRule, i);
    end;
  end
  else
  begin
    { Warn that all ctrl/alt will be set to lctrl/alt }

    Found := False;
    i := FKeyboardParser.Lines.IndexOfClass(TKeyboardParser_LayoutRule, -1);
    while i > 0 do
    begin
      rule := FKeyboardParser.Lines[i] as TKeyboardParser_LayoutRule;
      if (essCtrl in rule.Shift) or (essAlt in rule.Shift) then
      begin
        Found := True;
        Break;
      end;
      i := FKeyboardParser.Lines.IndexOfClass(TKeyboardParser_LayoutRule, i);
    end;

    if Found and (MessageDlg('You have already defined some keys to have alt or ctrl states.'+
        '  If you continue, these shift states will be converted to left-alt and left-ctrl respectively.'#13#10#13#10 +
        'Continue and convert shift states?', mtConfirmation, mbOkCancel, 0) = mrCancel) then
      Exit;

    i := FKeyboardParser.Lines.IndexOfClass(TKeyboardParser_LayoutRule, -1);
    while i > 0 do
    begin
      rule := FKeyboardParser.Lines[i] as TKeyboardParser_LayoutRule;
      if essCtrl in rule.Shift then
        rule.Shift := rule.Shift - [essCtrl] + [essLCtrl];
      if essAlt in rule.Shift then
        rule.Shift := rule.Shift - [essAlt] + [essLAlt];

      i := FKeyboardParser.Lines.IndexOfClass(TKeyboardParser_LayoutRule, i);
    end;
  end;
  Result := True;
end;

procedure TfrmKeymanWizard.Layout_FocusOutput;
begin
  if editKeyOutputText.CanFocus then editKeyOutputText.SetFocus;
end;

procedure TfrmKeymanWizard.cmdInsertCopyrightClick(Sender: TObject);
begin
  editCopyright.Text := editCopyright.Text + #$A9;
  editCopyright.SetFocus;
end;

procedure TfrmKeymanWizard.cmdOpenSourceFolderClick(Sender: TObject);
begin
  OpenContainingFolder(FileName);
end;

procedure TfrmKeymanWizard.cmdOpenBuildFolderClick(Sender: TObject);
begin
  OpenContainingFolder((ProjectFile as TkmnProjectFile).TargetFilename);
end;

procedure TfrmKeymanWizard.cmdOpenProjectFolderClick(Sender: TObject);
begin
  if Assigned(ProjectFile.Project) then
    OpenContainingFolder(ProjectFile.Project.FileName);
end;

procedure TfrmKeymanWizard.cmdOpenDebugHostClick(Sender: TObject);
begin
  TUtilExecute.URL(lbDebugHosts.Items[lbDebugHosts.ItemIndex]);
end;

procedure TfrmKeymanWizard.SetFontInfo(Index: TKeyboardFont; const Value: TKeyboardFontInfo);   // I4057
var
  f: TFont;
  sz: Integer;
  NewValue: TKeyboardFontInfo;

  procedure UpdateTouchLayoutSourceFont;
  var
    s: string;
  begin
    s := frameTouchLayoutSource.EditorText;
    if UpdateTouchLayoutFont(s, Index, NewValue.Name, NewValue.Size) then   // I4872
    begin
      frameTouchLayoutSource.EditorText := s;
      Modified := True;
    end;
  end;

begin
  if not Value.Enabled then
    Exit;

  case Index of
    kfontCode:
      begin
        CodeFont.Name := Value.Name;
        CodeFont.Size := StrToIntDef(Value.Size, 12);
      end;
    kfontChar:
      begin
        CharFont.Name := Value.Name;
        CharFont.Size := StrToIntDef(Value.Size, 12);
      end;
    kfontOSK:
      begin
        if FKeyboardParser.Features.ContainsKey(kfOSK) then   // I4258
        begin
          f := TFont.Create;
          try
            f.Name := Value.Name;
            f.Size := StrToIntDef(Value.Size, 12);
            frameOSK.KeyFont := f;
          finally
            f.Free;
          end;
        end;
      end;
    kfontTouchLayoutPhone, kfontTouchLayoutTablet, kfontTouchLayoutDesktop:
      begin
        if FKeyboardParser.Features.ContainsKey(kfTouchLayout) then   // I4258
        begin
          NewValue := Value;   // I4872
          if TryStrToInt(NewValue.Size, sz) then   // I4872
            NewValue.Size := NewValue.Size + 'em';   // I4872
          if pagesTouchLayout.ActivePage = pageTouchLayoutDesign
            then frameTouchLayout.FontInfo[Index] := NewValue   // I4872
            else UpdateTouchLayoutSourceFont;

          if Index = kfontTouchLayoutPhone then
          begin
            f := TFont.Create;
            try
              f.Name := Value.Name;
              f.Size := StrToIntDef(Value.Size, 12);
              frameTouchLayoutSource.CharFont := f;
            finally
              f.Free;
            end;
          end;
        end;
      end;
  end;
end;

procedure TfrmKeymanWizard.SetTextFileFormat(const Value: TTextFileFormat);
var
  i: Integer;
begin
  frmKeymanDeveloper.cbTextFileFormat.ItemIndex := Ord(Value);
  if FTextFileFormat = Value then Exit;
  FTextFileFormat := Value;
  if FTextFileFormat = tffANSI then
  begin
    if IsInParserMode then   // I4557
    begin
      for i := 0 to FKeyboardParser.Lines.Count - 1 do
        FKeyboardParser.Lines[i].ConvertToANSI;
      UpdateControls(True);   // I4137
    end;
  end;
  frameSource.TextFileFormat := FTextFileFormat;
  if not Floading then Modified := True;
end;

procedure TfrmKeymanWizard.TextFileFormatClick;
begin
  //FInTextFileFormatChange := True;
  try
    if Ord(FTextFileFormat) = frmKeymanDeveloper.cbTextFileFormat.ItemIndex then Exit;

    if (TextFileFormat <> tffANSI) and
        (frmKeymanDeveloper.cbTextFileFormat.ItemIndex = Ord(tffANSI)) then
      if MessageDlg('You may lose information if you transfer the file to ANSI.  Are you sure you want to do this?',
          mtConfirmation, mbYesNoCancel, 0) <> mrYes then
      begin
        frmKeymanDeveloper.cbTextFileFormat.ItemIndex := Ord(FTextFileFormat);
        Exit;
      end;

    TextFileFormat := TTextFileFormat(frmKeymanDeveloper.cbTextFileFormat.ItemIndex);
  finally
    //FInTextFileFormatChange := False;
  end;
end;

procedure TfrmKeymanWizard.StopDebugging;
begin
  FDebugForm.HideDebugForm;
  panDebugHost.Visible := False;
  FocusTab;
end;

procedure TfrmKeymanWizard.SetupDebugForm;
begin
  panDebugHost.Visible := False;

  FDebugStatusForm := TfrmDebugStatus.Create(Self);
  FDebugStatusForm.BorderStyle := bsNone;
  FDebugStatusForm.Parent := panDebugStatusHost;
  FDebugStatusForm.Align := alClient;
  FDebugStatusForm.Visible := True;
  //FDebugForm.RefreshOptions;

  FDebugForm := TfrmDebug.Create(Self);
  FDebugForm.BorderStyle := bsNone;
  FDebugForm.Parent := panDebugWindowHost;
  FDebugForm.Align := alClient;
  FDebugForm.OnSetBreakpoint := DebugSetBreakpoint;
  FDebugForm.OnClearBreakpoint := DebugClearBreakpoint;
  FDebugForm.OnUpdateExecutionPoint := DebugUpdateExecutionPoint;
  FDebugForm.Visible := True;
  FDebugForm.EditorMemo := frameSource;
end;

function TfrmKeymanWizard.ShouldRememberFocus(Control: TWinControl): Boolean;   // I4679
begin
  Result := (Control <> pages) and (Control.ClassName <> 'TTabSheet');
end;

{ On Screen Keyboard Tab }

procedure TfrmKeymanWizard.InitTabOnScreenKeyboard;
begin
  frameOSK := TframeOnScreenKeyboardEditor.Create(Self);
  frameOSK.Parent := pageOnScreenKeyboard;
  frameOSK.Align := alClient;
  frameOSK.OnModified := OSKModified;
  frameOSK.OnImportingKMX := OSKImportKMX;
  frameOSK.OnImportingKMXFinished := OSKImportKMXFinished;
  frameOSK.Visible := True;
end;

procedure TfrmKeymanWizard.OSKModified(Sender: TObject);
begin
  Modified := True;
  FFeature[kfOSK].Modified := True;
end;

procedure TfrmKeymanWizard.OSKImportKMX(Sender: TObject; var KMXFileName: TTempFile);   // I4181
var
  KMNFileName: string;
  sw: WideString;
  kbdparser: TKeyboardParser;
  FEncoding: TEncoding;
  FIncludeCodes: string;
  w: TKmcWrapper;
begin
  KMXFileName := TTempFileManager.Get('.kmx');   // I4181
  KMNFileName := ExtractFilePath(Filename) + '__temp_osk_import_' + ExtractFileName(Filename);

  kbdparser := TKeyboardParser.Create;
  try
    if IsInParserMode   // I4557
      then kbdparser.KeyboardText := FKeyboardParser.KeyboardText
      else kbdparser.KeyboardText := frameSource.EditorText;

    kbdparser.AddRequiredLines;
    kbdparser.SetSystemStoreValue(ssTargets, 'windows native');
    kbdparser.SetSystemStoreValue(ssVersion, SKeymanVersion90);
    kbdparser.DeleteSystemStore(ssVisualKeyboard);
    kbdparser.DeleteSystemStore(ssBitmap);

    FIncludeCodes := kbdparser.GetSystemStoreValue(ssIncludeCodes);   // I4979
    if FIncludeCodes <> '' then
    begin
      FIncludeCodes := ExpandFileNameClean((ProjectFile as TkmnProjectFile).FileName, FIncludeCodes);
      kbdparser.SetSystemStoreValue(ssIncludeCodes, FIncludeCodes);
    end;

    // Don't use KeyboardText because that stomps on the full path constructed above.
    sw := kbdparser.GetKeyboardTextRaw;
  finally
    kbdparser.Free;
  end;

  case FTextFileFormat of  // I3337
    tffANSI: FEncoding := TEncoding.Default;
    tffUTF8: FEncoding := TEncoding.UTF8;
    tffUTF16: FEncoding := TEncoding.Unicode;
    else raise Exception.Create('Invalid encoding');
  end;

  with TStringList.Create do  // I3337
  try
    Text := sw;
    SaveToFile(KMNFileName, FEncoding);   // I4181
  finally
    Free;
  end;

  frmMessages.Clear;
  w := TKmcWrapper.Create;
  try
    if not w.Compile(ProjectFile, KMNFileName, KMXFileName.Name, False) then
    begin
      frmMessages.DoShowForm;
      ShowMessage('There were errors compiling the keyboard to convert to the On Screen Keyboard.');
      FreeAndNil(KMXFileName);   // I4181
    end;
  finally
    w.Free;
    DeleteFile(KMNFileName);
  end;
end;

procedure TfrmKeymanWizard.OSKImportKMXFinished(Sender: TObject; KMXFileName: TTempFile);   // I4181
begin
  FreeAndNil(KMXFileName);   // I4181
end;

procedure TfrmKeymanWizard.pagesChange(Sender: TObject);
begin
  CheckParserSourceMove(True);
  FocusTab;
end;

procedure TfrmKeymanWizard.pagesChanging(Sender: TObject;
  var AllowChange: Boolean);
begin
  CheckParserSourceMove(False);
end;

procedure TfrmKeymanWizard.pagesDrawTab(Control: TCustomTabControl;
  TabIndex: Integer; const Rect: TRect; Active: Boolean);

    function GetPageIndexFromTabIndex(ix: Integer): Integer;
    begin
      Result := -1;
      while ix >= 0 do
      begin
        Inc(Result);
        if pages.Pages[Result].TabVisible then
          Dec(ix);
      end;
    end;

begin
  TabIndex := GetPageIndexFromTabIndex(TabIndex);

  modActionsMain.ilEditorPages.Draw(Control.Canvas,
    (Rect.Right + Rect.Left - modActionsMain.ilEditorPages.Width) div 2, Rect.Top + 4,
    pages.Pages[TabIndex].ImageIndex);
  with Control.Canvas do
  begin
    Font.Style := [fsBold];
    TextOut((Rect.Right + Rect.Left - TextWidth(pages.Pages[TabIndex].Caption)) div 2,
      Rect.Top + modActionsMain.ilEditorPages.Height + 8, pages.Pages[TabIndex].Caption);
  end;
end;

procedure TfrmKeymanWizard.pagesLayoutChange(Sender: TObject);
begin
  Layout_UpdateCharacterSet;
  CheckParserSourceMove(True);
end;

procedure TfrmKeymanWizard.ShowKeyboardComplexDesignMessage;
begin
  ShowMessage('The Design tab is disabled because the keyboard file is too complex to represent and modify visually.  '+
    'You must make any further changes to the keyboard directly in the Code tab.');
end;

procedure TfrmKeymanWizard.pagesLayoutChanging(Sender: TObject;
  var AllowChange: Boolean);
begin
  CheckParserSourceMove(False);
  if (pagesLayout.ActivePage = pageLayoutCode) and FKeyboardParser.IsComplex then   // I4557
  begin
    AllowChange := False;
    ShowKeyboardComplexDesignMessage;
  end
  else
    AllowChange := True;
end;

procedure TfrmKeymanWizard.CheckForReload;
var
  f: TKeyboardParser_FeatureID;
  FFileAge: TDateTime;
  s: string;
begin
  inherited;

  if not IsInParserMode then   // I4557
  begin
    MoveSourceToParser(True);   // I4137
  end;

  for f := Low(f) to High(f) do
  begin
    if FFeature[f].Filename <> '' then
    begin
      if FileAge(FFeature[f].Filename, FFileAge) and
        (FFileAge > FFeature[f].OpenFileAge) then
      begin
        if FFeature[f].Modified then s := SMsgExternalEditModified else s := SMsgExternalEdit;
        if MessageDlg(Format(s, [FFeature[f].FileName]), mtConfirmation, [mbYes,mbNo], 0) = mrYes then
        begin
          LoadFeature(f); //DoOpenFile;
        end;
      end;
    end;
  end;
end;

procedure TfrmKeymanWizard.CheckParserSourceMove(Changed: Boolean);
begin
  if Changed then
  begin
    if not IsInParserMode then   // I4557
      MoveParserToSource;
  end
  else if not IsInParserMode and not FLoading then   // I4557
    MoveSourceToParser(True);   // I4137
end;

procedure TfrmKeymanWizard.LoadOSK;   // I4034
begin
  frameOSK.FileName := FFeature[kfOSK].Filename;
  frameOSK.Load;
end;

procedure TfrmKeymanWizard.SaveOSK;
begin
  frameOSK.KMXFileName := (ProjectFile as TkmnProjectFile).KmxTargetFilename;   // I4695
  if FFeature[kfOSK].FileName = '' then
    FFeature[kfOSK].FileName := ChangeFileExt(FileName, '.kvks');
  frameOSK.FileName := FFeature[kfOSK].Filename;
  frameOSK.Save;
end;

{ Touch Layout tab }

procedure TfrmKeymanWizard.InitTabTouchLayout;   // I3885
begin
  frameTouchLayout := TframeTouchLayoutBuilder.Create(Self);
  frameTouchLayout.Parent := pageTouchLayoutDesign;   // I4034
  frameTouchLayout.Align := alClient;
  frameTouchLayout.OnModified := TouchLayoutModified;
  frameTouchLayout.OnImportFromOSKCommand := cmdImportFromOnScreenClick;   // I4034
  frameTouchLayout.OnSelectTemplateCommand := cmdTouchLayoutTemplateClick;   // I4034
  frameTouchLayout.Visible := True;

  frameTouchLayoutSource := TframeTextEditor.Create(Self);   // I4034
  frameTouchLayoutSource.EditorFormat := efJSON;
  frameTouchLayoutSource.TextFileFormat := tffUTF8;
  frameTouchLayoutSource.Parent := pageTouchLayoutCode;
  frameTouchLayoutSource.Align := alClient;
  frameTouchLayoutSource.OnChanged := TouchLayoutModified;
  frameTouchLayoutSource.Visible := True;
end;

procedure TfrmKeymanWizard.TouchLayoutModified(Sender: TObject);   // I3885
begin
  if FLoading then Exit;   // I4034
  Modified := True;
  FFeature[kfTouchLayout].Modified := True;
end;

procedure TfrmKeymanWizard.LoadTouchLayout;   // I4034
begin
  if pagesTouchLayout.ActivePage = pageTouchLayoutDesign then
  begin
    if not frameTouchLayout.Load(FFeature[kfTouchLayout].Filename, False, False) then
    begin
      pagesTouchLayout.ActivePage := pageTouchLayoutCode;
      frameTouchLayoutSource.LoadFromFile(FFeature[kfTouchLayout].Filename, tffUTF8);
    end;
  end
  else
    frameTouchLayoutSource.LoadFromFile(FFeature[kfTouchLayout].Filename, tffUTF8);
end;

procedure TfrmKeymanWizard.SaveTouchLayout;   // I3885
begin
  if not Untitled then   // I3909
  begin
    if (FFeature[kfTouchLayout].FileName = '') then   // I3909
    begin
      FFeature[kfTouchLayout].FileName :=
        // See also TKeyboardParser_Features.GetDefaultFeatureFilename
        Format(KeyboardFeatureFilename[kfTouchLayout], [ChangeFileExt(ExtractFileName(FileName), '')]);
    end;

    if pagesTouchLayout.ActivePage = pageTouchLayoutDesign   // I4034
      then frameTouchLayout.Save(FFeature[kfTouchLayout].Filename)
      else frameTouchLayoutSource.SaveToFile(FFeature[kfTouchLayout].Filename);

    FFeature[kfTouchLayout].Modified := False;
  end;
end;

procedure TfrmKeymanWizard.sbDetailsMouseWheel(Sender: TObject;
  Shift: TShiftState; WheelDelta: Integer; MousePos: TPoint;
  var Handled: Boolean);   // I4082
begin
  (Sender as TScrollBox).VertScrollBar.Position := (Sender as TScrollBox).VertScrollBar.Position - WheelDelta div 2;
  Handled := True;
end;

procedure TfrmKeymanWizard.pagesTouchLayoutChange(Sender: TObject);
begin
  FLoading := True;
  if pagesTouchLayout.ActivePage = pageTouchLayoutCode then
  begin
    frameTouchLayoutSource.EditorText := frameTouchLayout.SaveToString;
    DoFocus(frameTouchLayoutSource);
  end
  else
    DoFocus(frameTouchLayout);
  FLoading := False;
end;

procedure TfrmKeymanWizard.pagesTouchLayoutChanging(Sender: TObject;
  var AllowChange: Boolean);
begin
  if pagesTouchLayout.ActivePage = pageTouchLayoutCode then
  begin
    // Move from Source to Design
    FLoading := True;
    if not frameTouchLayout.Load(frameTouchLayoutSource.EditorText, False, True) then
    begin
      AllowChange := False;
      if frameTouchLayout.LastErrorOffset >= 0 then   // I4083
      begin
        frmMessages.Add(
          plsError,
          FFeature[kfTouchLayout].Filename,
          frameTouchLayout.LastError,
          CERR_ERROR,
          0);
        frameTouchLayoutSource.FindErrorByOffset(frameTouchLayout.LastErrorOffset);
      end;
    end;
    FLoading := False;
  end;
end;

procedure TfrmKeymanWizard.cmdTouchLayoutTemplateClick(Sender: TObject);   // I3885
begin
  SelectTouchLayoutTemplate(True);
end;

procedure TfrmKeymanWizard.cmdImportFromOnScreenClick(Sender: TObject);   // I3945   // I4034
var
  FSave: Boolean;
begin
  if (FFeature[kfOSK].FileName = '') or not FKeyboardParser.Features.ContainsKey(kfOSK) then   // I4058   // I4138
  begin
    ShowMessage('This keyboard does not include a Desktop On Screen Keyboard');
    Exit;
  end;

  if Self.Modified then   // I4059
  begin
    if not FKeymanDeveloperOptions.OSKAutoSaveBeforeImporting then
    begin
      if TfrmMessageDlgWithSave.Execute(
          'You must save changes to your keyboard before import. Save now and import?',
          'Always save changes automatically before importing',
          '', True, FSave) in [mrNo, mrCancel] then
        Exit;
      if FSave then
      begin
        FKeymanDeveloperOptions.OSKAutoSaveBeforeImporting := True;
        FKeymanDeveloperOptions.Write;
      end;

      if not modActionsMain.actFileSave.Execute then
      begin
        Exit;
      end;
    end;
  end;

  Self.Modified := True;

  frameTouchLayout.ImportFromKVK(FFeature[kfOSK].FileName);
end;

procedure TfrmKeymanWizard.SelectTouchLayoutTemplate(APromptChange: Boolean);
begin
  with TfrmSelectTouchLayoutTemplate.Create(Application.MainForm) do
  try
    PromptChange := APromptChange;
    if ShowModal = mrOk then
    begin
      frameTouchLayout.SaveToString;
      frameTouchLayout.TemplateFileName := TemplateFileName;
      frameTouchLayout.Load('', True, False);   // I4034
      Self.Modified := True;
    end;
  finally
    Free;
  end;
end;

{ Languages tab }

procedure TfrmKeymanWizard.InitTabLanguages;
begin
  gridWinLanguages.Cells[0, 0] := 'Language Name';
  gridWinLanguages.Cells[1, 0] := 'LANGID';
  //gridWinLanguages.Cells[2, 0] := 'ISO Code';
  gridWinLanguages.Cells[2, 0] := 'Default';
end;

procedure TfrmKeymanWizard.Languages_UpdateControls;
var
  i: Integer;
  L: TKeyboardParser_WinLanguage;
begin
  { Get list of language ids }

  gridWinLanguages.RowCount := FKeyboardParser.WinLanguages.Count + 1;

  if gridWinLanguages.RowCount > 1 then
    gridWinLanguages.FixedRows := 1;

  for i := 0 to FKeyboardParser.WinLanguages.Count - 1 do
  begin
    L := FKeyboardParser.WinLanguages[i];
    gridWinLanguages.Objects[0, i+1] := L;
    gridWinLanguages.Cells[0, i+1] := L.Name;
    gridWinLanguages.Cells[1, i+1] := IntToHex(L.ID, 4);
    if L.IsDefault
      then gridWinLanguages.Cells[2, i+1] := '1'
      else gridWinLanguages.Cells[2, i+1] := '0';
  end;

  editEthnologueCode.Text := FKeyboardParser.GetSystemStoreValue(ssEthnologueCode);
end;

procedure TfrmKeymanWizard.cmdWinLang_AddClick(Sender: TObject);
var
  i: Integer;
begin
  with TfrmSelectWindowsLanguages.Create(Self) do
  try
    for I := 0 to FKeyboardParser.WinLanguages.Count  - 1 do
      RemoveLanguage(FKeyboardParser.WinLanguages[I].ID);

    if ShowModal = mrOk then
    begin
      for I := 0 to LanguageCount - 1 do
        FKeyboardParser.WinLanguages.Add(Languages[I]);
      Languages_UpdateControls;
      EnableControls;
    end;
  finally
    Free;
  end;
end;

procedure TfrmKeymanWizard.cmdWinLang_RemoveClick(Sender: TObject);
begin
  FKeyboardParser.WinLanguages.Remove(gridWinLanguages.Objects[0, gridWinLanguages.Row] as TKeyboardParser_WinLanguage);
  Languages_UpdateControls;
  EnableControls;
end;

procedure TfrmKeymanWizard.editEthnologueCodeChange(Sender: TObject);
begin
  if FLoading then Exit;
  FKeyboardParser.SetSystemStoreValue(ssEthnologueCode, editEthnologueCode.Text);
  EnableControls;
end;

procedure TfrmKeymanWizard.cmdISOLang_LookupClick(Sender: TObject);
begin
  TUtilExecute.url(MakeKeymanURL(URLPath_KeymanLanguageLookup));  // I3349
end;

procedure TfrmKeymanWizard.cmdKMWBrowseHelpFileClick(Sender: TObject);
begin
  inherited;

end;

(*procedure TfrmKeymanWizard.VK_UpdateUnderlyingLayoutCaption;
var
  s, t: string;
begin
  with TRegistryErrorControlled.Create do  // I2890
  try
    RootKey := HKEY_LOCAL_MACHINE;
    if OpenKeyReadOnly(SRegKey_KeyboardLayouts + '\' + FVK.Header.UnderlyingLayout) then
    begin
      s := ReadString('Layout File');
      t := ReadString('Layout Text');
    end
    else
    begin
      s := 'Unknown';
      t := 'Could not find keyboard details in registry';
    end;
  finally
    Free;
  end;
  chkVKOnlyUseWithUnderlyingLayout.Caption := '&Only use this layout with the underlying keyboard "'+t+'" ('+s+')';
end;*)

procedure TfrmKeymanWizard.UpdateQRCode;
var
  b: TBitmap;
begin
  imgQRCode.Picture := nil;
  if lbDebugHosts.ItemIndex >= 0 then
  begin
    b := TBitmap.Create;
    try
      DrawQRCode(lbDebugHosts.Items[lbDebugHosts.ItemIndex], b);
      imgQRCode.Picture.Bitmap := b;
    finally
      b.Free;
    end;
  end;
end;

end.


