(*
  Name:             UfrmPackageEditor
  Copyright:        Copyright (C) SIL International.
  Documentation:
  Description:
  Create Date:      20 Jun 2006

  Modified Date:    3 Aug 2015
  Authors:          mcdurdin
  Related Files:
  Dependencies:

  Bugs:
  Todo:
  Notes:
  History:          20 Jun 2006 - mcdurdin - Initial version
                    01 Aug 2006 - mcdurdin - Support tab child format
                    23 Aug 2006 - mcdurdin - Rework UI with new Unicode controls
                    23 Aug 2006 - mcdurdin - Integrate source editor
                    28 Sep 2006 - mcdurdin - Added Open Containing Folder buttons and Edit File button
                    28 Sep 2006 - mcdurdin - Added on-screen keyboard to the package when autopackaging a .kmx
                    06 Oct 2006 - mcdurdin - Warn if package is being upgraded to version 7.0
                    04 Dec 2006 - mcdurdin - Add upgrade warning; upload to website; default ext
                    04 Dec 2006 - mcdurdin - Fix enabled controls and filetype info
                    12 Dec 2006 - mcdurdin - Fix FocusTab
                    04 Jan 2007 - mcdurdin - Fix start menu path text and clear listbox when loading
                    15 Jan 2007 - mcdurdin - Add (Start Product) and other shortcuts targets, remove $KEYMAN\keyman.exe, $KEYMAN\kmshell.exe (deprecated)
                    22 Jan 2007 - mcdurdin - Fix the package image availability for Standard Edition
                    30 Jan 2007 - mcdurdin - Remove pro edition watermark
                    21 Mar 2007 - mcdurdin - I709 - Changes to source editor do not save when Save is clicked
                    16 May 2007 - mcdurdin - I233 - Ensure all text is stored as Unicode
                    16 May 2007 - mcdurdin - I333 - Additional widestring support
                    30 May 2007 - mcdurdin - I817 - Build bootstrap installer
                    23 Aug 2007 - mcdurdin - I962 - Fix for uploading package and pressing Cancel
                    19 Nov 2007 - mcdurdin - I1157 - const string parameters
                    16 Jan 2009 - mcdurdin - I1730 - Add label suggesting version format for online update support
                    25 May 2010 - mcdurdin - I2392 - Activation Client integration
                    18 Mar 2011 - mcdurdin - I2178 - Compile Product Installer should prompt to save first
                    18 Mar 2011 - mcdurdin - I1756 - Adding shortcuts to package does not mark package as dirty
                    18 May 2012 - mcdurdin - I3306 - V9.0 - Remove TntControls + Win9x support
                    02 Feb 2012 - mcdurdin - I3100 - Rare crash when adding files to a package
                    03 Nov 2012 - mcdurdin - I3506 - V9.0 - Merge of I3100 - Rare crash when adding files to a package
                    27 Feb 2014 - mcdurdin - I4081 - V9.0 - Trace compile errors in subfiles in Keyman Developer
                    04 May 2015 - mcdurdin - I4689 - V9.0 - Redesign package editor as left tabbed
                    04 May 2015 - mcdurdin - I4690 - V9.0 - Pull keyboard version into package version when adding a keyboard
                    04 May 2015 - mcdurdin - I4687 - V9.0 - Split project UI actions into separate classes
                    04 May 2015 - mcdurdin - I4688 - V9.0 - Add build path to project settings
                    06 May 2015 - mcdurdin - I4702 - Package actions crash if package not part of a project [CrashID:tike.exe_9.0.496.0_008033BD_EAccessViolation]
                    27 May 2015 - mcdurdin - I4253 - Developer crashes when upload to website fails [CrashID:tike.exe_9.0.449.0_008FED3C_Exception]
                    27 May 2015 - mcdurdin - I4618 - Developer crashes when upload to website fails [CrashID:tike.exe_9.0.481.0_0088D9A8_Exception]
                    24 Jul 2015 - mcdurdin - I4796 - Refresh Keyman Developer look and feel for release
                    03 Aug 2015 - mcdurdin - I4814 - Image preview in package editor doesn't refresh after changes
*)
unit UfrmPackageEditor;  // I3306   // I4796

interface

uses
  System.UITypes,
  Windows, SysUtils, Messages, Classes, Graphics, Forms, Controls, StdCtrls,
  Buttons, ComCtrls, ExtCtrls, Dialogs, CommDlg, kmxfile, kpsfile,
  ImgList, UfrmMDIChild, Keyman.Developer.System.Project.ProjectFile, PackageInfo,
  UfrmMDIEditor, Grids, dmActionsMain,
  UserMessages, Keyman.Developer.System.Project.ProjectLog,
  UframeTextEditor, LeftTabbedPageControl, Keyman.Developer.UI.Project.ProjectFileUI,
  utilfiletypes;

type
  TfrmPackageEditor = class(TfrmTikeEditor)   // I4689
    dlgFiles: TOpenDialog;
    dlgNewCustomisation: TSaveDialog;
    pages: TLeftTabbedPageControl;
    pageFiles: TTabSheet;
    pageDetails: TTabSheet;
    pageShortcuts: TTabSheet;
    pageSource: TTabSheet;
    pageCompile: TTabSheet;
    Panel1: TPanel;
    imgFileIcon: TImage;
    Label1: TLabel;
    Label8: TLabel;
    lbFiles: TListBox;
    cmdAddFile: TButton;
    cmdRemoveFile: TButton;
    editFileType: TEdit;
    editFilePath: TEdit;
    lblFileDetails: TLabel;
    lblFilePath: TLabel;
    lblFileType: TLabel;
    memoFileDetails: TMemo;
    cmdOpenFile: TButton;
    cmdOpenContainingFolder: TButton;
    lblKMPImageFile: TLabel;
    lblKMPImageSize: TLabel;
    lblReadme: TLabel;
    lblStep2b: TLabel;
    lblStep2c: TLabel;
    lblStep2d: TLabel;
    lblStep2e: TLabel;
    lblStep2f: TLabel;
    lblStep2g: TLabel;
    lblStep2a: TLabel;
    lblPackageDetails: TLabel;
    lblPackageRequiredInformation: TLabel;
    sbDetails: TScrollBox;
    sbCompile: TScrollBox;
    lblVersionHint: TLabel;
    cbReadMe: TComboBox;
    editInfoName: TEdit;
    editInfoVersion: TEdit;
    editInfoCopyright: TEdit;
    editInfoAuthor: TEdit;
    editInfoEmail: TEdit;
    editInfoWebSite: TEdit;
    cmdInsertCopyright: TButton;
    cbKMPImageFile: TComboBox;
    panKMPImageSample: TPanel;
    imgKMPSample: TImage;
    Panel3: TPanel;
    lblStartMenuOptions: TLabel;
    lblStartMenuDescription: TLabel;
    lblStartMenuParameters: TLabel;
    lblStartMenuProgram: TLabel;
    lblStartMenuEntries: TLabel;
    lblShortcuts: TLabel;
    chkStartMenuUninstall: TCheckBox;
    chkCreateStartMenu: TCheckBox;
    editStartMenuPath: TEdit;
    lbStartMenuEntries: TListBox;
    cmdNewStartMenuEntry: TButton;
    cmdDeleteStartMenuEntry: TButton;
    editStartMenuDescription: TEdit;
    editStartMenuParameters: TEdit;
    cbStartMenuProgram: TComboBox;
    Label13: TLabel;
    lblCompilePackage: TLabel;
    pageKeyboards: TTabSheet;
    Panel5: TPanel;
    Label2: TLabel;
    Label3: TLabel;
    lblKeyboardFiles: TLabel;
    lblKeyboardDescription: TLabel;
    lbKeyboards: TListBox;
    editKeyboardDescription: TEdit;
    memoKeyboardFiles: TMemo;
    lblKeyboardVersion: TLabel;
    editKeyboardVersion: TEdit;
    lblKeyboardOSKFont: TLabel;
    cbKeyboardOSKFont: TComboBox;
    cbKeyboardDisplayFont: TComboBox;
    lblKeyboardDisplayFont: TLabel;
    gridKeyboardLanguages: TStringGrid;
    lblKeyboardLanguages: TLabel;
    cmdKeyboardAddLanguage: TButton;
    cmdKeyboardRemoveLanguage: TButton;
    chkFollowKeyboardVersion: TCheckBox;
    lblKeyboardRTL: TLabel;
    editKeyboardRTL: TEdit;
    cmdKeyboardEditLanguage: TButton;
    panBuildMobile: TPanel;
    lblDebugHostCaption: TLabel;
    lblCrossPlatform: TLabel;
    cmdStartTestOnline: TButton;
    cmdOpenDebugHost: TButton;
    lbDebugHosts: TListBox;
    panBuildDesktop: TPanel;
    Label4: TLabel;
    lblCompileTargetHeader: TLabel;
    cmdInstall: TButton;
    cmdUninstall: TButton;
    pageLexicalModels: TTabSheet;
    panLexicalModels: TPanel;
    lblLexlicalModels: TLabel;
    lblLexicalModelsSubtitle: TLabel;
    lblLexicalModelFilename: TLabel;
    lblLexicalModelDescription: TLabel;
    lblLexicalModelLanguages: TLabel;
    lbLexicalModels: TListBox;
    editLexicalModelDescription: TEdit;
    gridLexicalModelLanguages: TStringGrid;
    cmdLexicalModelLanguageAdd: TButton;
    cmdLexicalModelLanguageRemove: TButton;
    cmdLexicalModelLanguageEdit: TButton;
    chkLexicalModelRTL: TCheckBox;
    editLexicalModelFilename: TEdit;
    imgQRCode: TImage;
    panOpenInExplorer: TPanel;
    lblOpenInExplorer: TLabel;
    cmdOpenSourceFolder: TButton;
    cmdOpenBuildFolder: TButton;
    cmdOpenProjectFolder: TButton;
    panFileActions: TPanel;
    lblFileActions: TLabel;
    editOutPath: TEdit;
    cmdAddToProject: TButton;
    cmdBuildPackage: TButton;
    Label5: TLabel;
    cmdConfigureWebDebugger: TButton;
    cmdSendURLsToEmail: TButton;
    cmdCopyDebuggerLink: TButton;
    lblDescription: TLabel;
    memoInfoDescription: TMemo;
    lblDescriptionMarkdown: TLabel;
    gridKeyboardExamples: TStringGrid;
    lblKeyboardExamples: TLabel;
    cmdKeyboardAddExample: TButton;
    cmdKeyboardEditExample: TButton;
    cmdKeyboardRemoveExample: TButton;
    lblRelatedPackages: TLabel;
    gridRelatedPackages: TStringGrid;
    cmdAddRelatedPackage: TButton;
    cmdEditRelatedPackage: TButton;
    cmdRemoveRelatedPackage: TButton;
    cmdKeyboardWebOSKFonts: TButton;
    cmdKeyboardWebDisplayFonts: TButton;
    lblWebOSKFonts: TLabel;
    lblWebDisplayFonts: TLabel;
    lblLicenseFile: TLabel;
    cbLicense: TComboBox;
    lblWelcomeFile: TLabel;
    cbWelcomeFile: TComboBox;
    procedure cmdCloseClick(Sender: TObject);
    procedure cmdAddFileClick(Sender: TObject);
    procedure cmdRemoveFileClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure lbFilesClick(Sender: TObject);
    procedure cmdBuildPackageClick(Sender: TObject);
    procedure editInfoNameChange(Sender: TObject);
    procedure editInfoVersionChange(Sender: TObject);
    procedure editInfoCopyrightChange(Sender: TObject);
    procedure editInfoAuthorChange(Sender: TObject);
    procedure editInfoEmailChange(Sender: TObject);
    procedure editInfoWebSiteChange(Sender: TObject);
    procedure editCmdLineChange(Sender: TObject);
    procedure cmdInstallClick(Sender: TObject);
    procedure cmdInsertCopyrightClick(Sender: TObject);
    procedure chkStartMenuUninstallClick(Sender: TObject);
    procedure chkCreateStartMenuClick(Sender: TObject);
    procedure cbReadMeClick(Sender: TObject);
    procedure lbStartMenuEntriesClick(Sender: TObject);
    procedure editStartMenuDescriptionChange(Sender: TObject);
    procedure cbStartMenuProgramChange(Sender: TObject);
    procedure editStartMenuParametersChange(Sender: TObject);
    procedure cmdNewStartMenuEntryClick(Sender: TObject);
    procedure cmdDeleteStartMenuEntryClick(Sender: TObject);
    procedure editStartMenuPathChange(Sender: TObject);
    procedure cbKMPImageFileClick(Sender: TObject);
    procedure cmdUninstallClick(Sender: TObject);
    procedure cmdOpenContainingFolderClick(Sender: TObject);
    procedure cmdOpenFileClick(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure pagesChanging(Sender: TObject;
      var AllowChange: Boolean);
    procedure pagesChange(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure lbKeyboardsClick(Sender: TObject);
    procedure cbKeyboardOSKFontClick(Sender: TObject);
    procedure cbKeyboardDisplayFontClick(Sender: TObject);
    procedure gridKeyboardLanguagesClick(Sender: TObject);
    procedure cmdKeyboardRemoveLanguageClick(Sender: TObject);
    procedure cmdKeyboardAddLanguageClick(Sender: TObject);
    procedure chkFollowKeyboardVersionClick(Sender: TObject);
    procedure gridKeyboardLanguagesDblClick(Sender: TObject);
    procedure cmdKeyboardEditLanguageClick(Sender: TObject);
    procedure cmdStartTestOnlineClick(Sender: TObject);
    procedure cmdOpenDebugHostClick(Sender: TObject);
    procedure lbLexicalModelsClick(Sender: TObject);
    procedure gridLexicalModelLanguagesClick(Sender: TObject);
    procedure gridLexicalModelLanguagesDblClick(Sender: TObject);
    procedure cmdLexicalModelLanguageAddClick(Sender: TObject);
    procedure cmdLexicalModelLanguageEditClick(Sender: TObject);
    procedure cmdLexicalModelLanguageRemoveClick(Sender: TObject);
    procedure chkLexicalModelRTLClick(Sender: TObject);
    procedure editLexicalModelDescriptionChange(Sender: TObject);
    procedure lbDebugHostsClick(Sender: TObject);
    procedure cmdOpenSourceFolderClick(Sender: TObject);
    procedure cmdOpenBuildFolderClick(Sender: TObject);
    procedure cmdOpenProjectFolderClick(Sender: TObject);
    procedure cmdSendURLsToEmailClick(Sender: TObject);
    procedure cmdCopyDebuggerLinkClick(Sender: TObject);
    procedure sbDetailsMouseWheel(Sender: TObject; Shift: TShiftState;
      WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean);
    procedure memoInfoDescriptionChange(Sender: TObject);
    procedure cmdKeyboardAddExampleClick(Sender: TObject);
    procedure cmdKeyboardEditExampleClick(Sender: TObject);
    procedure cmdKeyboardRemoveExampleClick(Sender: TObject);
    procedure FormResize(Sender: TObject);
    procedure gridKeyboardExamplesDblClick(Sender: TObject);
    procedure gridKeyboardExamplesClick(Sender: TObject);
    procedure gridRelatedPackagesDblClick(Sender: TObject);
    procedure cmdAddRelatedPackageClick(Sender: TObject);
    procedure cmdEditRelatedPackageClick(Sender: TObject);
    procedure cmdRemoveRelatedPackageClick(Sender: TObject);
    procedure cmdKeyboardWebOSKFontsClick(Sender: TObject);
    procedure cmdKeyboardWebDisplayFontsClick(Sender: TObject);
    procedure cbLicenseClick(Sender: TObject);
    procedure cbWelcomeFileClick(Sender: TObject);
  private
    pack: TKPSFile;
    FSetup: Integer;
    frameSource: TframeTextEditor;

    procedure UpdateOutPath;
    procedure UpdateData;

    procedure UpdateInfo(nm, desc, url: WideString);
    procedure WMUserFormShown(var Message: TMessage); message WM_User_FormShown;
    procedure WMSysCommand(var Message: TWMSysCommand); message WM_SYSCOMMAND;
    procedure EnableStartMenuControls;
    procedure EnableDetailsTabControls;
    procedure EnableCompileTabControls;
    function DoAction(action: TProjectFileAction): Boolean;
    procedure UpdateWelcomeFile;
    procedure UpdateReadme;
    procedure UpdateImageFiles;
    procedure GetStartMenuEntries(var AName, AProg, AParams: WideString);
    procedure SetStartMenuEntries(AName, AProg, AParams: WideString);
    procedure UpdateStartMenuPrograms;
    procedure FillFileList(combo: TComboBox; obj: TObject; FileType: TKMFileType = ftOther);
    procedure UpdateImagePreviews;
    procedure AddFile(FileName: WideString);
    procedure SourceChanged(Sender: TObject);
    procedure MovePackageToSource;
    function MoveSourceToPackage: Boolean;
    procedure RefreshKeyboardList;
    function SelectedKeyboard: TPackageKeyboard;
    procedure EnableKeyboardTabControls;
    function SelectedKeyboardLanguage: TPackageKeyboardLanguage;
    procedure RefreshKeyboardLanguageList(k: TPackageKeyboard);
    procedure HandlePackageRefreshError(Sender: TObject; msg: string;
      State: TProjectLogState);
    procedure RefreshTargetPanels;
    procedure EnableControls;
    function CheckFilenameConventions(FileName: string): Boolean;
    function SelectedLexicalModel: TPackageLexicalModel;
    function SelectedLexicalModelLanguage: TPackageKeyboardLanguage;
    procedure RefreshLanguageList(grid: TStringGrid;
      langs: TPackageKeyboardLanguageList);
    procedure EnableLexicalModelTabControls;
    procedure ShowEditLanguageForm(grid: TStringGrid;
      langs: TPackageKeyboardLanguageList; lang: TPackageKeyboardLanguage);
    procedure ShowAddLanguageForm(grid: TStringGrid; langs: TPackageKeyboardLanguageList);
    procedure RefreshLexicalModelList;
    procedure UpdateQRCode;
    procedure RefreshKeyboardExampleList(k: TPackageKeyboard);
    procedure ShowAddExampleForm(k: TPackageKeyboard);
    procedure ShowEditExampleForm(k: TPackageKeyboard; example: TPackageKeyboardExample);
    function SelectedKeyboardExample: TPackageKeyboardExample;
    procedure ResizeGridColumns;
    procedure RefreshRelatedPackagesList;
    function SelectedRelatedPackage: TPackageRelatedPackage;
    procedure ShowEditWebFontsForm(Fonts: TPackageContentFileReferenceList);
    procedure UpdateLicense;

  protected
    function GetHelpTopic: string; override;
    function DoOpenFile: Boolean; override;
    function DoSaveFile: Boolean; override;
    function GetFileNameFilter: string; override;
    function GetDefaultExt: string; override;

    procedure FocusTab;
    function GetProjectFile: TProjectFile; override;   // I4702

    procedure CodeFontChanged; override;
    procedure CharFontChanged; override;

  public
    function GetPack: TKPSFile;

    procedure NotifyStartedWebDebug;

    procedure FindError(const Filename: string; s: string; line: Integer); override;   // I4081

    function CanChangeTab(FForward: Boolean): Boolean; override;
    procedure ChangeTab(FForward: Boolean); override;
  end;

implementation

uses
  System.Math,
  Vcl.Clipbrd,
  Vcl.Imaging.GifImg,

  Keyman.Developer.System.HelpTopics,

  CharMapDropTool,
  CharMapInsertMode,
  Keyman.Developer.System.Project.kpsProjectFile,
  Keyman.Developer.System.Project.kpsProjectFileAction,
  Keyman.Developer.System.ServerAPI,
  KeymanVersion,
  Keyman.System.PackageInfoRefreshKeyboards,
  Keyman.System.PackageInfoRefreshLexicalModels,
  Keyman.System.QRCode,
  Keyman.System.KeyboardUtils,
  Keyman.System.LexicalModelUtils,
  kmxfileconsts,
  Keyman.Developer.System.Project.Project,
  Keyman.Developer.System.Project.ProjectFileType,
  ShellApi,
  TextFileFormat,
  TTInfo,
  utilsystem,
  UfrmMain,
  UfrmMessages,
  UfrmSendURLsToEmail,
  utilexecute,
  Keyman.Developer.UI.UfrmEditLanguageExample,
  Keyman.Developer.UI.UfrmEditPackageWebFonts,
  Keyman.Developer.UI.UfrmEditRelatedPackage,
  Keyman.Developer.UI.UfrmSelectBCP47Language,
  xmldoc;

{$R *.DFM}

{-------------------------------------------------------------------------------
 - Form and general routines                                                   -
 -------------------------------------------------------------------------------}

procedure TfrmPackageEditor.FocusTab;
begin
  if not CanFocus then
    Exit;
  if pages.ActivePage = pageFiles then
    lbFiles.SetFocus
  else if pages.ActivePage = pageKeyboards then
    lbKeyboards.SetFocus
  else if pages.ActivePage = pageDetails then
    editInfoName.SetFocus
  else if pages.ActivePage = pageShortcuts then
    chkCreateStartMenu.SetFocus
  else if pages.ActivePage = pageSource then
    frameSource.SetFocus
  else if pages.ActivePage = pageCompile then
    cmdBuildPackage.SetFocus;
end;

procedure TfrmPackageEditor.FormCreate(Sender: TObject);
begin
  Inc(FSetup);
  try
    pages.ActivePage := pageFiles;
    pack := TKPSFile.Create;

    EnableControls;

    gridRelatedPackages.ColWidths[0] := 240;
    gridRelatedPackages.Cells[0, 0] := 'ID';
    gridRelatedPackages.Cells[1, 0] := 'Relationship';

    UpdateStartMenuPrograms;
    UpdateWelcomeFile;
    UpdateReadme;
    UpdateLicense;
    UpdateImageFiles;
    UpdateImagePreviews;
    RefreshKeyboardList;
    RefreshLexicalModelList;
    RefreshRelatedPackagesList;

    frameSource := TframeTextEditor.Create(Self);
    frameSource.Parent := pageSource;
    frameSource.Align := alClient;
    frameSource.EditorFormat := efXML;
    frameSource.Visible := True;
    frameSource.OnChanged := SourceChanged;
    frameSource.TextFileFormat := tffUTF8;

    GetCharMapDropTool.Handle(Self, cmimText);

    cmdAddToProject.Caption := '&Add to Project';

    inherited;
  finally
    Dec(FSetup);
  end;
end;

procedure TfrmPackageEditor.SourceChanged(Sender: TObject);
begin
  if FSetup = 0 then
    Modified := True;
end;

procedure TfrmPackageEditor.cmdCopyDebuggerLinkClick(Sender: TObject);
begin
  try
    Clipboard.AsText := lbDebugHosts.Items[lbDebugHosts.ItemIndex];
  except
    on E:Exception do
      ShowMessage(E.Message);
  end;
end;

procedure TfrmPackageEditor.FormDestroy(Sender: TObject);
begin
  inherited;
  pack.Free;
end;

procedure TfrmPackageEditor.FormKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  inherited;
  if (Shift = []) and (Key = VK_F7) then
  begin
    Key := 0;
    cmdBuildPackageClick(Self);
  end;
end;

procedure TfrmPackageEditor.FormResize(Sender: TObject);
begin
  inherited;
  ResizeGridColumns;
end;

procedure TfrmPackageEditor.ResizeGridColumns;
begin
  gridKeyboardLanguages.ColWidths[1] := System.Math.Max(gridKeyboardLanguages.ClientWidth - 120 - 1, 80);
  gridLexicalModelLanguages.ColWidths[1] := System.Math.Max(gridLexicalModelLanguages.ClientWidth - 120 - 1, 80);
  gridKeyboardExamples.ColWidths[3] := System.Math.Max(gridKeyboardExamples.ClientWidth - 120 - 200 - 120 - 3, 80);
  gridRelatedPackages.ColWidths[1] := System.Math.Max(gridRelatedPackages.ClientWidth - 240 - 1, 80);
end;

procedure TfrmPackageEditor.FormShow(Sender: TObject);
begin
  inherited;
  PostMessage(Handle, WM_USER_FormShown, 0, 0);
end;

function TfrmPackageEditor.DoSaveFile: Boolean;
begin
  Result := False;

  if pages.ActivePage = pageSource then
  begin
    if not MoveSourceToPackage then
    begin
      Result := False;
      Exit;
    end;
  end;

  if pack.WasIni then
  begin
    if MessageDlg('The file '+FileName+' was originally created in Keyman Developer 6.2 or an earlier version.  '+
        'Saving it in Keyman Developer '+SKeymanVersion+' changes the file format and it will no longer be editable in '+
        'Keyman Developer 6.2.'#13#10#13#10+'Continue save?', mtWarning, mbOkCancel, 0) = mrCancel then
      Exit;
  end;
  pack.FileName := FileName;
  pack.SaveXML;
  Modified := False;
  Result := True;
end;

procedure TfrmPackageEditor.cmdCloseClick(Sender: TObject);
begin
  Close;
end;

procedure TfrmPackageEditor.WMSysCommand(var Message: TWMSysCommand);
begin
  if ((Message.CmdType = SC_NEXTWINDOW) or (Message.CmdType = SC_PREVWINDOW)) and (Message.XPos = 9)
    then PostMessage(Handle, CM_DIALOGKEY, VK_TAB, 0)
    else inherited;
end;

procedure TfrmPackageEditor.WMUserFormShown(var Message: TMessage);
begin
  inherited;
  FocusTab;
end;

function TfrmPackageEditor.GetPack: TKPSFile;
begin
  Result := pack;
end;

function TfrmPackageEditor.GetProjectFile: TProjectFile;   // I4702
begin
  Result := inherited GetProjectFile;
  if not Assigned(Result) then
  begin
    FStandaloneProjectFile := TkpsProjectFileAction.Create(nil, FileName, nil);
    Result := FStandaloneProjectFile;
  end;
end;

{-------------------------------------------------------------------------------
 - Interface routines                                                          -
 -------------------------------------------------------------------------------}

function TfrmPackageEditor.GetDefaultExt: string;
begin
  Result := 'kps';
end;

function TfrmPackageEditor.GetFileNameFilter: string;
begin
  Result := 'Package source files (*.kps)|*.kps|All files (*.*)|*.*';
end;

function TfrmPackageEditor.GetHelpTopic: string;
begin
  Result := SHelpTopic_Context_PackageEditor;
end;

function TfrmPackageEditor.DoOpenFile: Boolean;
begin
  Result := True;
  pack.FileName := FileName;

  try
    pack.LoadXML;
  except
    on E:Exception do
    begin
      ShowMessage('Unable to load package file - opening in source editor: '+E.Message);
      pages.ActivePage := pageSource;
      frameSource.LoadFromFile(FileName);
      Exit;
    end;
  end;

  UpdateData;
  Result := True;
end;

procedure TfrmPackageEditor.FindError(const Filename: string; s: string; line: Integer);   // I4081
begin
  SetFocus;
end;

{-------------------------------------------------------------------------------
 - Info page                                                                   -
 -------------------------------------------------------------------------------}

procedure TfrmPackageEditor.editInfoNameChange(Sender: TObject);
begin
  if FSetup > 0 then Exit;
  pack.Info.Desc['Name'] := Trim(editInfoName.Text);
  Modified := True;
end;

procedure TfrmPackageEditor.editInfoVersionChange(Sender: TObject);
begin
  if FSetup > 0 then Exit;
  pack.Info.Desc['Version'] := Trim(editInfoVersion.Text);
  Modified := True;
end;

procedure TfrmPackageEditor.editInfoCopyrightChange(Sender: TObject);
begin
  if FSetup > 0 then Exit;
  pack.Info.Desc['Copyright'] := Trim(editInfoCopyright.Text);
  Modified := True;
end;

procedure TfrmPackageEditor.cmdInsertCopyrightClick(Sender: TObject);
begin
  Modified := True;
  editInfoCopyright.SelLength := 0;
  editInfoCopyright.SelText := '�';
end;

procedure TfrmPackageEditor.editInfoAuthorChange(Sender: TObject);
begin
  if FSetup > 0 then Exit;
  pack.Info.Desc['Author'] := Trim(editInfoAuthor.Text);
  Modified := True;
end;

procedure TfrmPackageEditor.editInfoEmailChange(Sender: TObject);
begin
  if FSetup > 0 then Exit;
  if Trim(editInfoEmail.Text) = ''
    then pack.Info.URL['Author'] := ''
    else pack.Info.URL['Author'] := 'mailto:'+Trim(editInfoEmail.Text);
  Modified := True;
end;

procedure TfrmPackageEditor.editInfoWebSiteChange(Sender: TObject);
begin
  if FSetup > 0 then Exit;
  pack.Info.Desc['WebSite'] := Trim(editInfoWebSite.Text);
  pack.Info.URL['WebSite'] := Trim(editInfoWebSite.Text);
  Modified := True;
end;

{-------------------------------------------------------------------------------
 - Files page                                                                  -
 -------------------------------------------------------------------------------}

function TfrmPackageEditor.CheckFilenameConventions(FileName: string): Boolean;
begin
  if not FGlobalProject.Options.CheckFilenameConventions then
    Exit(True);

  if TKeyboardUtils.DoesFilenameFollowConventions(FileName) then
    Exit(True);

  Result := MessageDlg(Format(TKeyboardUtils.SFilenameDoesNotFollowConventions_Prompt, [FileName]),
    mtConfirmation, mbOkCancel, 0) = mrOk;
end;

procedure TfrmPackageEditor.AddFile(FileName: WideString);
var
  f: TPackageContentFile;
  ki: TKeyboardInfo;
  j: Integer;
  viskbdname: WideString;
begin
  if pack.Files.FromFileNameEx(FileName) <> nil then Exit; // Already added

  if not CheckFilenameConventions(FileName) then
    Exit;

  f := TPackageContentFile.Create(pack);
  f.FileName := FileName;

  case f.FileType of
    ftFont:
      try
        with TTTInfo.Create(FileName, [tfNames]) do
        try
          f.Description := 'Font '+FullName;
        finally
          Free;
        end;
      except
        f.Description := 'Unreadable font ' + ChangeFileExt(ExtractFileName(FileName), '');
      end;
    ftKeymanFile:
      begin
        try
          GetKeyboardInfo(FileName, true, ki);
          try
            f.Description := 'Keyboard '+ki.KeyboardName;
            // Fill in some info stuff as well...
            if pack.Info.Desc[PackageInfo_Version] = '' then   // I4690
            begin
              pack.Info.Desc[PackageInfo_Version] := ki.KeyboardVersion;
            end;
            if pack.Info.Desc[PackageInfo_Name] = '' then
              pack.Info.Desc[PackageInfo_Name] := ki.KeyboardName;
            if (pack.Info.Desc[PackageInfo_Copyright] = '') and (ki.CopyrightString <> '') then
              pack.Info.Desc[PackageInfo_Copyright] := ki.CopyrightString;
            for j := 0 to pack.Info.Count - 1 do
              UpdateInfo(pack.Info[j].Name, pack.Info[j].Description, pack.Info[j].URL);
            if GetSystemStore(ki.MemoryDump.Memory, TSS_VISUALKEYBOARD, viskbdname) then
            begin
              viskbdname := ExpandFileNameEx(FileName, viskbdname);
              if FileExists(viskbdname) then
                AddFile(viskbdname);
            end;
          finally
            ki.MemoryDump.Free;
          end;

        except
          f.Description := 'Damaged keyboard '+ChangeFileExt(ExtractFileName(FileName), '');
        end;
      end;
    else
      f.Description := 'File '+ExtractFileName(FileName);
  end;

  pack.Files.Add(f);
  lbFiles.ItemIndex := lbFiles.Items.AddObject(ExtractFileName(FileName), f);
  lbFilesClick(lbFiles);
  UpdateWelcomeFile;
  UpdateReadme;
  UpdateLicense;
  UpdateImageFiles;
  UpdateStartMenuPrograms;

  frmMessages.Clear;
  RefreshKeyboardList;
  RefreshLexicalModelList;
////  UpdateCustomisationFile;
  Modified := True;
end;

procedure TfrmPackageEditor.cmdAddFileClick(Sender: TObject);
var
  i, j: Integer;
  Found: Boolean;
begin
  with dlgFiles do
    if Execute then
      for i := 0 to Files.Count - 1 do
      begin
        Found := False;
        for j := 0 to pack.Files.Count - 1 do
          if LowerCase(pack.Files[j].FileName) = LowerCase(Files[i]) then
          begin
            Found := True;
            Break;
          end;
        if Found then Continue; // Don't add the file again

        AddFile(Files[i]);
      end;
  if lbFiles.CanFocus then lbFiles.SetFocus;  // I3100   // I3506
end;

procedure TfrmPackageEditor.cmdRemoveFileClick(Sender: TObject);
var
  n: Integer;
begin
  if lbFiles.ItemIndex > -1 then
  begin
    n := pack.Files.IndexOf(lbFiles.Items.Objects[lbFiles.ItemIndex] as TPackageContentFile);
    pack.Files.Delete(n);
    lbFiles.Items.Delete(lbFiles.ItemIndex);
    if lbFiles.Items.Count > 0 then lbFiles.ItemIndex := 0;
    lbFilesClick(lbFiles);
    UpdateWelcomeFile;
    UpdateReadme;
    UpdateLicense;
    UpdateImageFiles;
    UpdateStartMenuPrograms;

    frmMessages.Clear;
    RefreshKeyboardList;
    RefreshLexicalModelList;
    Modified := True;
  end;
end;

procedure TfrmPackageEditor.cmdSendURLsToEmailClick(Sender: TObject);
begin
  with TfrmSendURLsToEmail.Create(Application.MainForm) do
  try
    //TODO: KeyboardName := Self.FKeyboardParser.GetSystemStoreValue(ssName);
    Hosts.Assign(lbDebugHosts.Items);
    ShowModal;
  finally
    Free;
  end;
end;

procedure TfrmPackageEditor.cmdStartTestOnlineClick(Sender: TObject);
begin
  DoAction(pfaTestKeymanWeb);
end;

{$EXTERNALSYM SHGetFileInfoW}
function SHGetFileInfoW(pszPath: PWideChar; dwFileAttributes: DWORD;
  var psfi: TSHFileInfoW; cbFileInfo, uFlags: UINT): DWORD; stdcall; external shell32;

procedure TfrmPackageEditor.lbDebugHostsClick(Sender: TObject);
begin
  UpdateQRCode;
end;

procedure TfrmPackageEditor.lbFilesClick(Sender: TObject);
var
  e: Boolean;
  FIcon: TIcon;
  FFileInfo: TShFileInfoW;
  r: TRect;
begin
  Inc(FSetup);
  try
    e := lbFiles.ItemIndex >= 0;
    editFileType.Enabled := e;
    editFilePath.Enabled := e;
    memoFileDetails.Enabled := e;
    lblFileType.Enabled := e;
    lblFilePath.Enabled := e;
    lblFileDetails.Enabled := e;
    cmdRemoveFile.Enabled := e;
    cmdOpenFile.Enabled := e;
    cmdOpenContainingFolder.Enabled := e;

    imgFileIcon.Picture := nil;
    imgFileIcon.Visible := False;
    r := imgFileIcon.BoundsRect;
    InvalidateRect(imgFileIcon.Parent.Handle, @r, True);
    imgFileIcon.Parent.Update;
    imgFileIcon.Visible := True;
    if e then
      with lbFiles.Items.Objects[lbFiles.ItemIndex] as TPackageContentFile do
      begin
        FillChar(FFileInfo, SizeOf(TShFileInfoW), 0);
        if SHGetFileInfoW(PWideChar(FileName), 0, FFileInfo, SizeOf(TShFileInfoW), SHGFI_TYPENAME or SHGFI_ICON or SHGFI_SMALLICON) <> 0 then
        begin
          if FFileInfo.hIcon <> 0 then
          begin
            FIcon := TIcon.Create;
            try
              FIcon.ReleaseHandle;
              FIcon.Handle := FFileInfo.hIcon;
              imgFileIcon.Picture.Icon := FIcon;
              //DestroyIcon(FFileInfo.hIcon);
            finally
              FIcon.Free;
            end;
          end
          else
            imgFileIcon.Picture := nil;
          editFileType.Text := FFileInfo.szTypeName;
        end
        else
        begin
          imgFileIcon.Picture := nil;
          editFileType.Text := ExtractFileExt(lbFiles.Items[lbFiles.ItemIndex]) + ' File';
        end;

        editFilePath.Text := FileName;
        if FileType <> ftOther then
          editFileType.Text := FileTypeNames[FileType];

        if FileType = ftFont then
        try
          with TTTInfo.Create(FileName, [tfNames]) do
          try
            memoFileDetails.Text := 'Font Name: '+FullName+#13#10+
              'Copyright: '+Copyright;
          finally
            Free;
          end;
        except
          memoFileDetails.Text := 'Font details were not read successfully.';
        end
        else
        begin
          memoFileDetails.Text := '';
        end;
      end
    else
    begin
      memoFileDetails.Text := '';
      editFileType.Text := '';
      editFilePath.Text := '';
    end;
  finally
    Dec(FSetup);
  end;
end;

{-------------------------------------------------------------------------------
 - Options page                                                                -
 -------------------------------------------------------------------------------}

procedure TfrmPackageEditor.cbWelcomeFileClick(Sender: TObject);
begin
  if FSetup > 0 then Exit;
  if cbWelcomeFile.ItemIndex <= 0
    then pack.Options.WelcomeFile := nil
    else pack.Options.WelcomeFile := cbWelcomeFile.Items.Objects[cbWelcomeFile.ItemIndex] as TPackageContentFile;
  Modified := True;
end;

procedure TfrmPackageEditor.cbReadMeClick(Sender: TObject);
begin
  if FSetup > 0 then Exit;
  if cbReadMe.ItemIndex <= 0
    then pack.Options.ReadmeFile := nil
    else pack.Options.ReadmeFile := cbReadMe.Items.Objects[cbReadMe.ItemIndex] as TPackageContentFile;
  Modified := True;
end;

procedure TfrmPackageEditor.cbLicenseClick(Sender: TObject);
begin
  if FSetup > 0 then Exit;
  if cbLicense.ItemIndex <= 0
    then pack.Options.LicenseFile := nil
    else pack.Options.LicenseFile := cbLicense.Items.Objects[cbLicense.ItemIndex] as TPackageContentFile;
  Modified := True;
end;


procedure TfrmPackageEditor.editCmdLineChange(Sender: TObject);
begin
  if FSetup > 0 then Exit;
  Modified := True;
end;

{-------------------------------------------------------------------------------
 - Start Menu page                                                             -
 -------------------------------------------------------------------------------}

procedure TfrmPackageEditor.EnableStartMenuControls;
var
  e: Boolean;
begin
  e := pack.StartMenu.DoCreate;
  chkStartMenuUninstall.Enabled := e;
  editStartMenuPath.Enabled := e;
  lblStartMenuEntries.Enabled := e;
  cmdNewStartMenuEntry.Enabled := e;
  lbStartMenuEntries.Enabled := e;
  e := e and (lbStartMenuEntries.ItemIndex >= 0);
  cmdDeleteStartMenuEntry.Enabled := e;
  lblStartMenuDescription.Enabled := e;
  editStartMenuDescription.Enabled := e;
  lblStartMenuProgram.Enabled := e;
  cbStartMenuProgram.Enabled := e;
  lblStartMenuParameters.Enabled := e;
  editStartMenuParameters.Enabled := e;
end;

procedure TfrmPackageEditor.chkCreateStartMenuClick(Sender: TObject);
begin
  if FSetup > 0 then Exit;
  pack.StartMenu.DoCreate := chkCreateStartMenu.Checked;
  EnableStartMenuControls;
  if chkCreateStartMenu.Checked then
  begin
    if editStartMenuPath.Text = '' then
      editStartMenuPath.Text := editInfoName.Text;
    editStartMenuPath.SetFocus;
  end
  else
    editStartMenuPath.Text := '';
  Modified := True;
end;

procedure TfrmPackageEditor.chkFollowKeyboardVersionClick(Sender: TObject);
begin
  if FSetup > 0 then Exit;
  pack.KPSOptions.FollowKeyboardVersion := chkFollowKeyboardVersion.Checked;
  EnableDetailsTabControls;
  Modified := True;
end;

procedure TfrmPackageEditor.chkStartMenuUninstallClick(Sender: TObject);
begin
  pack.StartMenu.AddUninstallEntry := chkStartMenuUninstall.Checked;
  Modified := True;
end;

procedure TfrmPackageEditor.GetStartMenuEntries(var AName, AProg, AParams: WideString);
begin
  AName := '';
  AProg := '';
  AParams := '';
  if lbStartMenuEntries.ItemIndex < 0 then Exit;
  with lbStartMenuEntries.Items.Objects[lbStartMenuEntries.ItemIndex] as TPackageStartMenuEntry do
  begin
    AName := Name;
    AProg := Prog;
    AParams := Params;
  end;
end;

procedure TfrmPackageEditor.SetStartMenuEntries(AName, AProg, AParams: WideString);
begin
  if lbStartMenuEntries.ItemIndex < 0 then Exit;
  with lbStartMenuEntries.Items.Objects[lbStartMenuEntries.ItemIndex] as TPackageStartMenuEntry do
  begin
    Name := AName;
    Prog := AProg;
    Params := AParams;
  end;
end;

procedure TfrmPackageEditor.cmdUninstallClick(Sender: TObject);
begin
  if DoAction(pfaUninstall)
    then ShowMessage('Package uninstalled successfully.')
    else ShowMessage('Failed to uninstall package.');
end;

procedure TfrmPackageEditor.lbStartMenuEntriesClick(Sender: TObject);
var
  desc, prog, params: WideString;
begin
  Inc(FSetup);  // I1756
  try
    GetStartMenuEntries(desc, prog, params);
    editStartMenuDescription.Text := desc;
    cbStartMenuProgram.Text := prog;
    editStartMenuParameters.Text := params;
    EnableStartMenuControls;
  finally
    Dec(FSetup);  // I1756
  end;
end;

procedure TfrmPackageEditor.memoInfoDescriptionChange(Sender: TObject);
begin
  if FSetup > 0 then Exit;
  pack.Info.Desc['Description'] := Trim(memoInfoDescription.Text);
  Modified := True;
end;

procedure TfrmPackageEditor.pagesChange(Sender: TObject);
begin
  if FSetup > 0 then Exit;

  if pages.ActivePage = pageSource then
    MovePackageToSource;

  FocusTab;
end;

procedure TfrmPackageEditor.pagesChanging(Sender: TObject; var AllowChange: Boolean);
begin
  if FSetup > 0 then Exit;

  if pages.ActivePage = pageSource then
    AllowChange := MoveSourceToPackage
  else
    AllowChange := True;
end;

procedure TfrmPackageEditor.editStartMenuDescriptionChange(Sender: TObject);
var
  desc, prog, params: WideString;
begin
  if lbStartMenuEntries.ItemIndex < 0 then Exit;
  if FSetup = 0 then Modified := True;  // I1756
  GetStartMenuEntries(desc, prog, params);
  desc := editStartMenuDescription.Text;
  lbStartMenuEntries.Items[lbStartMenuEntries.ItemIndex] := desc;
  SetStartMenuEntries(desc, prog, params);
end;

procedure TfrmPackageEditor.cbStartMenuProgramChange(Sender: TObject);
var
  desc, prog, params: WideString;
begin
  if lbStartMenuEntries.ItemIndex < 0 then Exit;
  if FSetup = 0 then Modified := True;  // I1756
  GetStartMenuEntries(desc, prog, params);
  prog := cbStartMenuProgram.Text;
  SetStartMenuEntries(desc, prog, params);
end;

procedure TfrmPackageEditor.ChangeTab(FForward: Boolean);
begin
  pages.SelectNextPage(FForward);
end;

procedure TfrmPackageEditor.editStartMenuParametersChange(Sender: TObject);
var
  desc, prog, params: WideString;
begin
  if lbStartMenuEntries.ItemIndex < 0 then Exit;
  if FSetup = 0 then Modified := True;  // I1756
  GetStartMenuEntries(desc, prog, params);
  params := editStartMenuParameters.Text;
  SetStartMenuEntries(desc, prog, params);
end;

procedure TfrmPackageEditor.cmdNewStartMenuEntryClick(Sender: TObject);
var
  sme: TPackageStartMenuEntry;
begin
  Modified := True;  // I1756
  sme := TPackageStartMenuEntry.Create(pack);
  sme.Name := '(new)';
  pack.StartMenu.Entries.Add(sme);
  lbStartMenuEntries.ItemIndex := lbStartMenuEntries.Items.AddObject('(new)', sme);
  lbStartMenuEntriesClick(lbStartMenuEntries);
end;

procedure TfrmPackageEditor.cmdOpenDebugHostClick(Sender: TObject);
begin
  TUtilExecute.URL(lbDebugHosts.Items[lbDebugHosts.ItemIndex]);
end;

procedure TfrmPackageEditor.cmdOpenFileClick(Sender: TObject);   // I4687
var
  s: WideString;
  f: TProjectFile;
  fui: TProjectFileUI;
begin
  s := (lbFiles.Items.Objects[lbFiles.ItemIndex] as TPackageContentFile).FileName;
  f := FGlobalProject.FindFile(s);
  if Assigned(f) then
  begin
    fui := f.UI as TProjectFileUI;
    fui.DefaultEvent(Self);
  end
  else
  begin
    f := CreateProjectFile(FGlobalProject, s, ProjectFile);
    try
      fui := f.UI as TProjectFileUI;
      fui.DefaultEvent(Self);
    finally
      f.Free;
    end;
  end;
end;

procedure TfrmPackageEditor.cmdOpenSourceFolderClick(Sender: TObject);
begin
  OpenContainingFolder(FileName);
end;

procedure TfrmPackageEditor.cmdOpenBuildFolderClick(Sender: TObject);
begin
  OpenContainingFolder((ProjectFile as TkpsProjectFile).TargetFilename);
end;

procedure TfrmPackageEditor.cmdOpenProjectFolderClick(Sender: TObject);
begin
  if Assigned(ProjectFile.Project) then
    OpenContainingFolder(ProjectFile.Project.FileName);
end;

procedure TfrmPackageEditor.cmdOpenContainingFolderClick(Sender: TObject);
begin
  OpenContainingFolder((lbFiles.Items.Objects[lbFiles.ItemIndex] as TPackageContentFile).FileName);
end;

procedure TfrmPackageEditor.cmdDeleteStartMenuEntryClick(Sender: TObject);
var
  n: Integer;
  sme: TPackageStartMenuEntry;
begin
  sme := lbStartMenuEntries.Items.Objects[lbStartMenuEntries.ItemIndex] as TPackageStartMenuEntry;
  pack.StartMenu.Entries.Delete(pack.StartMenu.Entries.IndexOf(sme));

  n := lbStartMenuEntries.ItemIndex;
  lbStartMenuEntries.Items.Delete(lbStartMenuEntries.ItemIndex);
  if n < lbStartMenuEntries.Items.Count
    then lbStartMenuEntries.ItemIndex := n
    else lbStartMenuEntries.ItemIndex := n - 1;

  lbStartMenuEntriesClick(lbStartMenuEntries);

  Modified := True;  // I1756
end;

{-------------------------------------------------------------------------------
 - Build page                                                                  -
 -------------------------------------------------------------------------------}

function TfrmPackageEditor.DoAction(action: TProjectFileAction): Boolean;
begin
  Result := (ProjectFile.UI as TProjectFileUI).DoAction(action, False);   // I4687   // I4702
end;

procedure TfrmPackageEditor.cmdBuildPackageClick(Sender: TObject);
begin
  if Untitled or Modified then
    ShowMessage('You must save the package before you can build it.')
  else
  begin
    frmMessages.Clear;
    DoAction(pfaCompile);
  end;
  UpdateOutPath;
  EnableControls;
end;

procedure TfrmPackageEditor.cmdInstallClick(Sender: TObject);
begin
  DoAction(pfaInstall);
end;

{-------------------------------------------------------------------------------
 - Display refresh routines                                                    -
 -------------------------------------------------------------------------------}

procedure TfrmPackageEditor.UpdateStartMenuPrograms;
begin
  cbStartMenuProgram.Clear;
  cbStartMenuProgram.Items.Assign(lbFiles.Items);
  cbStartMenuProgram.Items.Insert(0, '(Start Product)');
  cbStartMenuProgram.Items.Insert(1, '(Product Configuration)');
  cbStartMenuProgram.Items.Insert(2, '(Product Help)');
  cbStartMenuProgram.Items.Insert(3, '(About Product)');
end;

procedure TfrmPackageEditor.UpdateWelcomeFile;
begin
  FillFileList(cbWelcomeFile, pack.Options.WelcomeFile);
end;

procedure TfrmPackageEditor.UpdateReadme;
begin
  FillFileList(cbReadme, pack.Options.ReadmeFile);
end;

procedure TfrmPackageEditor.UpdateLicense;
begin
  FillFileList(cbLicense, pack.Options.LicenseFile);
end;

procedure TfrmPackageEditor.FillFileList(combo: TComboBox; obj: TObject; FileType: TKMFileType);
var
  i: Integer;
begin
  combo.Clear;

  combo.Items.AddObject('(none)', nil);

  for i := 0 to pack.Files.Count - 1 do
    if (FileType = ftOther) or (pack.Files[i].FileType = FileType) then
      combo.Items.AddObject(ExtractFileName(pack.Files[i].FileName), pack.Files[i]);

  if Assigned(obj)
    then combo.ItemIndex := combo.Items.IndexOfObject(obj)
    else combo.ItemIndex := 0;
end;

procedure TfrmPackageEditor.UpdateData;
var
  i: Integer;
begin
  Inc(FSetup);
  try
    lbFiles.Clear;

    for i := 0 to pack.Files.Count - 1 do
      lbFiles.Items.AddObject(ExtractFileName(pack.Files[i].FileName), pack.Files[i]);

    UpdateOutPath;
    UpdateWelcomeFile;
    UpdateReadme;
    UpdateLicense;
    UpdateImageFiles;
    UpdateStartMenuPrograms;

    for i := 0 to pack.Info.Count - 1 do
      UpdateInfo(pack.Info[i].Name, pack.Info[i].Description, pack.Info[i].URL);
    chkFollowKeyboardVersion.Checked := pack.KPSOptions.FollowKeyboardVersion;

    lbStartMenuEntries.Clear;

    for i := 0 to pack.StartMenu.Entries.Count - 1 do
      lbStartMenuEntries.Items.AddObject(pack.StartMenu.Entries[i].Name, pack.StartMenu.Entries[i]);

    chkCreateStartMenu.Checked := pack.StartMenu.DoCreate;
    chkStartMenuUninstall.Checked := pack.StartMenu.AddUninstallEntry;
    editStartMenuPath.Text := pack.StartMenu.Path;

    editOutPath.Text := (ProjectFile as TkpsProjectFile).TargetFilename;   // I4688

    if lbFiles.Items.Count > 0 then lbFiles.ItemIndex := 0;
    lbFilesClick(lbFiles);

    UpdateImagePreviews;   // I4814

    RefreshKeyboardList;
    RefreshLexicalModelList;
    RefreshRelatedPackagesList;

    EnableControls;
  finally
    Dec(FSetup);
  end;
end;

procedure TfrmPackageEditor.UpdateInfo(nm, desc, url: WideString);
var
  nmlc: WideString;
begin
  nmlc := LowerCase(nm);
  Inc(FSetup);
  try
    if nmlc = 'name' then editInfoName.Text := desc
    else if nmlc = 'copyright' then editInfoCopyright.Text := desc
    else if nmlc = 'version' then editInfoVersion.Text := desc                                    // less 'mailto:'
    else if nmlc = 'author' then begin editInfoAuthor.Text := desc; editInfoEmail.Text := Copy(url,8,1024); end
    else if nmlc = 'website' then editInfoWebSite.Text := desc
    else if nmlc = 'description' then memoInfoDescription.Text := desc
  finally
    Dec(FSetup);
  end;
end;

procedure TfrmPackageEditor.UpdateOutPath;
begin
  if Untitled then
  begin
    editOutPath.Text := '';
    cmdInstall.Enabled := False;
  end
  else
  begin
    editOutPath.Text := (ProjectFile as TkpsProjectFile).TargetFileName;   // I4688
    cmdInstall.Enabled := FileExists(editOutPath.Text);   // I4688
  end;
end;

procedure TfrmPackageEditor.editStartMenuPathChange(Sender: TObject);
begin
  if FSetup > 0 then Exit;
  pack.StartMenu.Path := Trim(editStartMenuPath.Text);
  Modified := True;
end;


{-------------------------------------------------------------------------------
 - Package image page                                                          -
 -------------------------------------------------------------------------------}

{
  Columns:
     [TYPE] X, Y, [W,H -- not req.], [Caption|Graphic File], URL/Command
}

function TfrmPackageEditor.CanChangeTab(FForward: Boolean): Boolean;
begin
  Result := True;
end;

procedure TfrmPackageEditor.cbKMPImageFileClick(Sender: TObject);
begin
  if FSetup > 0 then Exit;
  if cbKMPImageFile.ItemIndex <= 0
    then pack.Options.GraphicFile := nil
    else pack.Options.GraphicFile := cbKMPImageFile.Items.Objects[cbKMPImageFile.ItemIndex] as TPackageContentFile;
  UpdateImagePreviews;
  Modified := True;
end;

procedure TfrmPackageEditor.UpdateImageFiles;
begin
  FillFileList(cbKMPImageFile, pack.Options.GraphicFile);
end;

procedure TfrmPackageEditor.UpdateImagePreviews;
var
  filename, msg: string;
  p: TPicture;
begin
  filename := '';

  lblKMPImageSize.Caption := '(Unknown image format)';
  imgKMPSample.Picture := nil;

  if cbKMPImageFile.ItemIndex > 0 then
    filename := (cbKMPImageFile.Items.Objects[cbKMPImageFile.ItemIndex] as TPackageContentFile).FileName;

  if filename <> '' then
  begin
    try
      p := TPicture.Create;
      try
        p.LoadFromFile(filename);
        if (p.Width = 0) or (p.Height = 0) then
          Exit;

        imgKMPSample.Picture.Bitmap.SetSize(p.Width, p.Height);
        imgKMPSample.Picture.Bitmap.PixelFormat := pf24bit;
        imgKMPSample.Picture.Bitmap.Canvas.StretchDraw(imgKMPSample.ClientRect, p.Graphic);

        msg := Format('Image size: (%d x %d)', [p.Width, p.Height]);

        // Check dimensions and aspect ratio
        if Round(100 * p.Width / p.Height) <> 56 then
        begin
          msg := msg + ' WARNING: image has wrong aspect ratio; should be 140 x 250 pixels';
        end
        else if (p.Width <> 140) or (p.Height <> 250) then
        begin
          msg := msg + ' WARNING: image should be 140 x 250 pixels';
        end;
      finally
        p.Free;
      end;

      lblKMPImageSize.Caption := msg;

    except
      imgKMPSample.Picture := nil;
      lblKMPImageSize.Caption := '(Unknown image format)';
    end;
  end;
end;

procedure TfrmPackageEditor.MovePackageToSource;
begin
  frameSource.EditorText := FormatXMLData(pack.SaveXMLToText);
end;

function TfrmPackageEditor.MoveSourceToPackage: Boolean;
begin
  try
    pack.LoadXMLFromText(frameSource.EditorText);
    UpdateData;
    Result := True;
  except
    on E:Exception do
    begin
      ShowMessage('Unable to parse package source: '+E.Message);
      Result := False;
    end;
  end;
end;

procedure TfrmPackageEditor.CodeFontChanged;
begin
  inherited;
  frameSource.CodeFont := CodeFont;
end;

procedure TfrmPackageEditor.CharFontChanged;
begin
  inherited;
  frameSource.CharFont := CharFont;
end;

procedure TfrmPackageEditor.NotifyStartedWebDebug;
begin
  lbDebugHosts.Clear;
  TServerDebugAPI.GetServerURLs(lbDebugHosts.Items);
  if lbDebugHosts.Items.Count > 0 then
    lbDebugHosts.ItemIndex := 0;
  UpdateQRCode;
  EnableCompileTabControls;
end;

{-------------------------------------------------------------------------------
 - Keyboards tab
 -------------------------------------------------------------------------------}

(**
  Adds new keyboards to the Keyboards object in the kmp.inf and
  removes any that are no longer in the list.

  Note: if both a .js and a .kmx exist for a given keyboard id,
  then both will be checked for version consistency, etc.??

  Finally, updates the Keyboards tab
*)

procedure TfrmPackageEditor.HandlePackageRefreshError(Sender: TObject; msg: string; State: TProjectLogState);
begin
  if Assigned(Self.ProjectFile.Project) then
    Self.ProjectFile.Project.Log(State, Filename, Msg, 0, 0);
end;

procedure TfrmPackageEditor.RefreshKeyboardList;
var
  n, i: Integer;
begin
  n := lbKeyboards.ItemIndex;

  with TPackageInfoRefreshKeyboards.Create(pack) do
  try
    OnError := Self.HandlePackageRefreshError;
    if not Execute then
      frmMessages.DoShowForm;
  finally
    Free;
  end;

  lbKeyboards.Clear;
  for i := 0 to pack.Keyboards.Count - 1 do
    lbKeyboards.Items.AddObject(pack.Keyboards[i].ID, pack.Keyboards[i]);

  if lbKeyboards.Count = 0 then
    n := -1
  else if n >= lbKeyboards.Count then
    n := lbKeyboards.Count - 1
  else if (n < 0) and (lbKeyboards.Count > 0) then
    n := 0;
  lbKeyboards.ItemIndex := n;
  lbKeyboardsClick(lbKeyboards);

  RefreshTargetPanels;
end;

function TfrmPackageEditor.SelectedKeyboard: TPackageKeyboard;
begin
  if lbKeyboards.ItemIndex < 0
    then Result := nil
    else Result := lbKeyboards.Items.Objects[lbKeyboards.ItemIndex] as TPackageKeyboard;
end;

function TfrmPackageEditor.SelectedKeyboardLanguage: TPackageKeyboardLanguage;
var
  k: TPackageKeyboard;
begin
  k := SelectedKeyboard;
  if not Assigned(k) then
    Exit(nil);

  if gridKeyboardLanguages.Row = 0 then
    Exit(nil);

  Result := gridKeyboardLanguages.Objects[0, gridKeyboardLanguages.Row] as TPackageKeyboardLanguage;
end;

function TfrmPackageEditor.SelectedKeyboardExample: TPackageKeyboardExample;
var
  k: TPackageKeyboard;
begin
  k := SelectedKeyboard;
  if not Assigned(k) then
    Exit(nil);

  if gridKeyboardExamples.Row = 0 then
    Exit(nil);

  Result := gridKeyboardExamples.Objects[0, gridKeyboardExamples.Row] as TPackageKeyboardExample;
end;

procedure TfrmPackageEditor.lbKeyboardsClick(Sender: TObject);
var
  k: TPackageKeyboard;
  i: Integer;
begin
  Inc(FSetup);
  try
    gridKeyboardLanguages.Cells[0, 0] := 'BCP 47 tag';
    gridKeyboardLanguages.Cells[1, 0] := 'Language name';
    gridKeyboardLanguages.ColWidths[0] := 120;
    gridKeyboardLanguages.ColWidths[1] := 10;

    gridKeyboardExamples.Cells[0, 0] := 'BCP 47 tag';
    gridKeyboardExamples.Cells[1, 0] := 'Keys';
    gridKeyboardExamples.Cells[2, 0] := 'Text';
    gridKeyboardExamples.Cells[3, 0] := 'Note';
    gridKeyboardExamples.ColWidths[0] := 120;
    gridKeyboardExamples.ColWidths[1] := 200;
    gridKeyboardExamples.ColWidths[2] := 120;
    gridKeyboardExamples.ColWidths[3] := 10;

    k := SelectedKeyboard;
    if not Assigned(k) then
    begin
      editKeyboardDescription.Text := '';
      editKeyboardVersion.Text := '';
      memoKeyboardFiles.Text := '';
      editKeyboardRTL.Text := '';
      cbKeyboardOSKFont.ItemIndex := -1;
      cbKeyboardDisplayFont.ItemIndex := -1;
      gridKeyboardLanguages.RowCount := 1;
      gridKeyboardExamples.RowCount := 1;
      lblWebOSKFonts.Caption := '';
      lblWebDisplayFonts.Caption := '';
      EnableKeyboardTabControls;
      Exit;
    end;

    // Details

    memoKeyboardFiles.Text := '';
    editKeyboardDescription.Text := k.Name;
    editKeyboardVersion.Text := k.Version;

    if k.RTL
      then editKeyboardRTL.Text := 'True'
      else editKeyboardRTL.Text := 'False (or not .js format)';

    for i := 0 to pack.Files.Count - 1 do
      if SameText(TKeyboardUtils.KeyboardFileNameToID(pack.Files[i].FileName), k.ID) then
      begin
        memoKeyboardFiles.Lines.Add(ExtractFileName(pack.Files[i].FileName));
      end;

    // Fonts

    FillFileList(cbKeyboardOSKFont, k.OSKFont, ftFont);
    FillFileList(cbKeyboardDisplayFont, k.DisplayFont, ftFont);

    lblWebOSKFonts.Caption := k.WebOSKFonts.GetAsString;
    lblWebDisplayFonts.Caption := k.WebDisplayFonts.GetAsString;

    // Languages

    RefreshKeyboardLanguageList(k);
    RefreshKeyboardExampleList(k);
    EnableKeyboardTabControls;
  finally
    Dec(FSetup);
  end;
end;

procedure TfrmPackageEditor.RefreshKeyboardExampleList(k: TPackageKeyboard);
var
  i: Integer;
begin
  Inc(FSetup);
  try
    gridKeyboardExamples.RowCount := k.Examples.Count + 1;
    ResizeGridColumns;

    if k.Examples.Count > 0 then
    begin
      gridKeyboardExamples.FixedRows := 1;
    end
    else
      gridKeyboardExamples.Enabled := False;

    for i := 0 to k.Examples.Count - 1 do
    begin
      gridKeyboardExamples.Objects[0, i+1] := k.Examples[i];
      gridKeyboardExamples.Cells[0, i+1] := k.Examples[i].ID;
      gridKeyboardExamples.Cells[1, i+1] := k.Examples[i].Keys;
      gridKeyboardExamples.Cells[2, i+1] := k.Examples[i].Text;
      gridKeyboardExamples.Cells[3, i+1] := k.Examples[i].Note;
    end;
  finally
    Dec(FSetup);
  end;
end;

procedure TfrmPackageEditor.RefreshKeyboardLanguageList(k: TPackageKeyboard);
begin
  RefreshLanguageList(gridKeyboardLanguages, k.Languages);
end;

procedure TfrmPackageEditor.cbKeyboardDisplayFontClick(Sender: TObject);
var
  k: TPackageKeyboard;
begin
  if FSetup > 0 then Exit;
  k := SelectedKeyboard;
  Assert(Assigned(k));

  if cbKeyboardDisplayFont.ItemIndex <= 0
    then k.DisplayFont := nil
    else k.DisplayFont := cbKeyboardDisplayFont.Items.Objects[cbKeyboardDisplayFont.ItemIndex] as TPackageContentFile;
  Modified := True;
end;

procedure TfrmPackageEditor.cbKeyboardOSKFontClick(Sender: TObject);
var
  k: TPackageKeyboard;
begin
  if FSetup > 0 then Exit;
  k := SelectedKeyboard;
  Assert(Assigned(k));
  if cbKeyboardOSKFont.ItemIndex <= 0
    then k.OSKFont := nil
    else k.OSKFont := cbKeyboardOSKFont.Items.Objects[cbKeyboardOSKFont.ItemIndex] as TPackageContentFile;
  Modified := True;
end;

procedure TfrmPackageEditor.EnableControls;
begin
  EnableStartMenuControls;
  EnableDetailsTabControls;
  EnableKeyboardTabControls;
  EnableLexicalModelTabControls;
  EnableCompileTabControls;
end;

procedure TfrmPackageEditor.EnableCompileTabControls;
begin
  cmdOpenDebugHost.Enabled := lbDebugHosts.ItemIndex >= 0;

  // We use FProjectFile because we don't want to accidentally create a standalone
  // project file as GetProjectFile is side-effecty. EnableControls is called early
  // in construction before FProjectFile is assigned. It is called again later so
  // enabled state will be correct.
  cmdOpenProjectFolder.Enabled := Assigned(FProjectFile) and Assigned(FProjectFile.Project);
end;

procedure TfrmPackageEditor.EnableDetailsTabControls;
var
  e: Boolean;
begin
  e := not chkFollowKeyboardVersion.Checked;
  editInfoVersion.Enabled := e;
  lblStep2C.Enabled := e;
  lblVersionHint.Enabled := e;

  cmdAddRelatedPackage.Enabled := True;

  e := gridRelatedPackages.Row > 0;
  gridRelatedPackages.Enabled := e;
  cmdEditRelatedPackage.Enabled := e;
  cmdRemoveRelatedPackage.Enabled := e;
end;

procedure TfrmPackageEditor.EnableKeyboardTabControls;
var
  e: Boolean;
begin
  e := lbKeyboards.ItemIndex >= 0;
  lblKeyboardDescription.Enabled := e;
  editKeyboardDescription.Enabled := e;
  lblKeyboardFiles.Enabled := e;
  memoKeyboardFiles.Enabled := e;
  lblKeyboardVersion.Enabled := e;
  editKeyboardVersion.Enabled := e;
  lblKeyboardOSKFont.Enabled := e;
  cbKeyboardOSKFont.Enabled := e;
  lblKeyboardDisplayFont.Enabled := e;
  cbKeyboardDisplayFont.Enabled := e;

  cmdKeyboardWebOSKFonts.Enabled := e;
  cmdKeyboardWebDisplayFonts.Enabled := e;
  lblWebOSKFonts.Enabled := e;
  lblWebDisplayFonts.Enabled := e;

  lblKeyboardLanguages.Enabled := e;
  cmdKeyboardAddLanguage.Enabled := e;
  lblKeyboardExamples.Enabled := e;
  cmdKeyboardAddExample.Enabled := e;

  e := e and (gridKeyboardLanguages.Row > 0);
  gridKeyboardLanguages.Enabled := e;
  cmdKeyboardRemoveLanguage.Enabled := e;
  cmdKeyboardEditLanguage.Enabled := e;

  e := (lbKeyboards.ItemIndex >= 0) and (gridKeyboardExamples.Row > 0);
  gridKeyboardExamples.Enabled := e;
  cmdKeyboardRemoveExample.Enabled := e;
  cmdKeyboardEditExample.Enabled := e;

end;

procedure TfrmPackageEditor.gridKeyboardExamplesClick(Sender: TObject);
begin
  EnableKeyboardTabControls;
end;

procedure TfrmPackageEditor.gridKeyboardExamplesDblClick(Sender: TObject);
begin
  if SelectedKeyboardExample <> nil then
    cmdKeyboardEditExample.Click;
end;

procedure TfrmPackageEditor.gridKeyboardLanguagesClick(Sender: TObject);
begin
  EnableKeyboardTabControls;
end;

procedure TfrmPackageEditor.gridKeyboardLanguagesDblClick(Sender: TObject);
begin
  if SelectedKeyboardLanguage <> nil then
    cmdKeyboardEditLanguage.Click;
end;

procedure TfrmPackageEditor.cmdKeyboardAddExampleClick(Sender: TObject);
var
  k: TPackageKeyboard;
begin
  k := SelectedKeyboard;
  Assert(Assigned(k));
  ShowAddExampleForm(k);
end;

procedure TfrmPackageEditor.cmdKeyboardAddLanguageClick(Sender: TObject);
var
  k: TPackageKeyboard;
begin
  k := SelectedKeyboard;
  Assert(Assigned(k));
  ShowAddLanguageForm(gridKeyboardLanguages, k.Languages);
end;

procedure TfrmPackageEditor.cmdKeyboardEditExampleClick(Sender: TObject);
var
  k: TPackageKeyboard;
  example: TPackageKeyboardExample;
begin
  k := SelectedKeyboard;
  Assert(Assigned(k));

  example := SelectedKeyboardExample;
  Assert(Assigned(example));

  ShowEditExampleForm(k, example);
end;

procedure TfrmPackageEditor.cmdKeyboardEditLanguageClick(Sender: TObject);
var
  k: TPackageKeyboard;
  lang: TPackageKeyboardLanguage;
begin
  k := SelectedKeyboard;
  Assert(Assigned(k));

  lang := SelectedKeyboardLanguage;
  Assert(Assigned(lang));

  ShowEditLanguageForm(gridKeyboardLanguages, k.Languages, lang);
end;

procedure TfrmPackageEditor.cmdKeyboardRemoveExampleClick(Sender: TObject);
var
  k: TPackageKeyboard;
  example: TPackageKeyboardExample;
begin
  k := SelectedKeyboard;
  Assert(Assigned(k));
  example := SelectedKeyboardExample;
  Assert(Assigned(example));

  k.Examples.Remove(example);
  RefreshKeyboardExampleList(k);
  EnableKeyboardTabControls;
  Modified := True;
end;

procedure TfrmPackageEditor.cmdKeyboardRemoveLanguageClick(Sender: TObject);
var
  k: TPackageKeyboard;
  lang: TPackageKeyboardLanguage;
begin
  k := SelectedKeyboard;
  Assert(Assigned(k));
  lang := SelectedKeyboardLanguage;
  Assert(Assigned(lang));

  k.Languages.Remove(lang);
  RefreshKeyboardLanguageList(k);
  EnableKeyboardTabControls;
  Modified := True;
end;

procedure TfrmPackageEditor.cmdKeyboardWebDisplayFontsClick(Sender: TObject);
var
  k: TPackageKeyboard;
begin
  k := SelectedKeyboard;
  Assert(Assigned(k));
  ShowEditWebFontsForm(k.WebDisplayFonts);
end;

procedure TfrmPackageEditor.cmdKeyboardWebOSKFontsClick(Sender: TObject);
var
  k: TPackageKeyboard;
begin
  k := SelectedKeyboard;
  Assert(Assigned(k));
  ShowEditWebFontsForm(k.WebOSKFonts);
end;

procedure TfrmPackageEditor.ShowEditWebFontsForm(Fonts: TPackageContentFileReferenceList);
var
  frm: TfrmEditPackageWebFonts;
begin
  frm := TfrmEditPackageWebFonts.Create(Application.MainForm);
  try
    frm.Files := pack.Files;
    frm.SelectedFonts := Fonts;
    if frm.ShowModal = mrOk then
    begin
      lbKeyboardsClick(lbKeyboards);
      Modified := True;
    end;
  finally
    frm.Free;
  end;
end;

procedure TfrmPackageEditor.RefreshTargetPanels;
var
  i: Integer;
  k: TPackageKeyboard;
  FHasDesktopTarget, FHasMobileTarget: Boolean;
begin
  FHasDesktopTarget := False;
  FHasMobileTarget := pack.LexicalModels.Count > 0;
  for k in pack.Keyboards do
  begin
    for i := 0 to pack.Files.Count - 1 do
    begin
      if SameText(TKeyboardUtils.KeyboardFileNameToID(pack.Files[i].FileName), k.ID) then
      begin
        if pack.Files[i].FileType = ftKeymanFile
          then FHasDesktopTarget := True
          else FHasMobileTarget := True;
      end;
    end;
  end;

  panBuildDesktop.Visible := FHasDesktopTarget;
  panBuildMobile.Visible := FHasMobileTarget;
end;

procedure TfrmPackageEditor.sbDetailsMouseWheel(Sender: TObject;
  Shift: TShiftState; WheelDelta: Integer; MousePos: TPoint;
  var Handled: Boolean);
begin
  (Sender as TScrollBox).VertScrollBar.Position := (Sender as TScrollBox).VertScrollBar.Position - WheelDelta div 2;
  Handled := True;
end;

{-------------------------------------------------------------------------------
 - Lexical Models and Keyboards - language list shared functions
 -------------------------------------------------------------------------------}

procedure TfrmPackageEditor.RefreshLanguageList(grid: TStringGrid; langs: TPackageKeyboardLanguageList);
var
  i: Integer;
begin
  Inc(FSetup);
  try
    grid.RowCount := langs.Count + 1;
    ResizeGridColumns;

    if langs.Count > 0 then
    begin
      grid.FixedRows := 1;
    end
    else
      grid.Enabled := False;

    for i := 0 to langs.Count - 1 do
    begin
      grid.Objects[0, i+1] := langs[i];
      grid.Cells[0, i+1] := langs[i].ID;
      grid.Cells[1, i+1] := langs[i].Name;
    end;
  finally
    Dec(FSetup);
  end;
end;

procedure TfrmPackageEditor.ShowAddLanguageForm(grid: TStringGrid; langs: TPackageKeyboardLanguageList);
var
  lang: TPackageKeyboardLanguage;
  frm: TfrmSelectBCP47Language;
  n: Integer;
begin
  frm := TfrmSelectBCP47Language.Create(Application.MainForm);
  try
    if frm.ShowModal = mrOk then
    begin
      n := langs.IndexOfID(frm.LanguageID);
      if n >= 0 then
      begin
        // Duplicate - we won't re-add the item, just select the existing item
        grid.Row := n + 1;
        EnableControls;
        Exit;
      end;

      lang := TPackageKeyboardLanguage.Create(pack);
      lang.ID := frm.LanguageID;
      lang.Name := frm.LanguageName;
      langs.Add(lang);
      RefreshLanguageList(grid, langs);
      grid.Row := grid.RowCount - 1;
      Modified := True;
      EnableControls;
    end;
  finally
    frm.Free;
  end;
end;

procedure TfrmPackageEditor.ShowEditLanguageForm(grid: TStringGrid; langs: TPackageKeyboardLanguageList; lang: TPackageKeyboardLanguage);
var
  frm: TfrmSelectBCP47Language;
  n: Integer;
begin
  frm := TfrmSelectBCP47Language.Create(Application.MainForm);
  try
    frm.LanguageID := lang.ID;
    frm.LanguageName := lang.Name;
    if frm.ShowModal = mrOk then
    begin
      if not SameText(frm.LanguageID, lang.ID) then
      begin
        // If the id has changed, check for duplicates
        n := langs.IndexOfID(frm.LanguageID);
        if n >= 0 then
        begin
          // Duplicate - we will delete the edited one and select the existing
          // one
          langs.Remove(lang);
          RefreshLanguageList(grid, langs);

          // The index may have changed, search again
          n := langs.IndexOfID(frm.LanguageID);
          grid.Row := n + 1;
          EnableControls;
          Modified := True;
          Exit;
        end;
      end;

      lang.ID := frm.LanguageID;
      lang.Name := frm.LanguageName;
      RefreshLanguageList(grid, langs);
      EnableControls;
      Modified := True;
    end;
  finally
    frm.Free;
  end;
end;

procedure TfrmPackageEditor.ShowAddExampleForm(k: TPackageKeyboard);
var
  example: TPackageKeyboardExample;
  frm: TfrmEditLanguageExample;
begin
  frm := TfrmEditLanguageExample.Create(Application.MainForm);
  try
    frm.SetTitle(True);
    if frm.ShowModal = mrOk then
    begin
      example := TPackageKeyboardExample.Create(pack);
      example.ID := frm.LanguageID;
      example.Keys := frm.ExampleKeys;
      example.Text := frm.ExampleText;
      example.Note := frm.ExampleNote;
      k.Examples.Add(example);
      RefreshKeyboardExampleList(k);
      gridKeyboardExamples.Row := gridKeyboardExamples.RowCount - 1;
      Modified := True;
      EnableControls;
    end;
  finally
    frm.Free;
  end;
end;

procedure TfrmPackageEditor.ShowEditExampleForm(k: TPackageKeyboard; example: TPackageKeyboardExample);
var
  frm: TfrmEditLanguageExample;
begin
  frm := TfrmEditLanguageExample.Create(Application.MainForm);
  try
    frm.SetTitle(False);
    frm.LanguageID := example.ID;
    frm.ExampleKeys := example.Keys;
    frm.ExampleText := example.Text;
    frm.ExampleNote := example.Note;
    if frm.ShowModal = mrOk then
    begin
      example.ID := frm.LanguageID;
      example.Keys := frm.ExampleKeys;
      example.Text := frm.ExampleText;
      example.Note := frm.ExampleNote;
      RefreshKeyboardExampleList(k);
      EnableControls;
      Modified := True;
    end;
  finally
    frm.Free;
  end;
end;

{-------------------------------------------------------------------------------
 - Related Packages
 -------------------------------------------------------------------------------}

procedure TfrmPackageEditor.RefreshRelatedPackagesList;
var
  i: Integer;
begin
  Inc(FSetup);
  try
    gridRelatedPackages.RowCount := pack.RelatedPackages.Count + 1;
    ResizeGridColumns;

    if pack.RelatedPackages.Count > 0 then
    begin
      gridRelatedPackages.FixedRows := 1;
    end;

    for i := 0 to pack.RelatedPackages.Count - 1 do
    begin
      gridRelatedPackages.Objects[0, i+1] := pack.RelatedPackages[i];
      gridRelatedPackages.Cells[0, i+1] := pack.RelatedPackages[i].ID;
      if pack.RelatedPackages[i].Relationship = ''
        then gridRelatedPackages.Cells[1, i+1] := 'related' // UI string
        else gridRelatedPackages.Cells[1, i+1] := pack.RelatedPackages[i].Relationship;
    end;

    EnableDetailsTabControls;
  finally
    Dec(FSetup);
  end;
end;

function TfrmPackageEditor.SelectedRelatedPackage: TPackageRelatedPackage;
begin
  if gridRelatedPackages.Row = 0 then
    Exit(nil);

  Result := gridRelatedPackages.Objects[0, gridRelatedPackages.Row] as TPackageRelatedPackage;
end;

procedure TfrmPackageEditor.cmdAddRelatedPackageClick(Sender: TObject);
var
  rp: TPackageRelatedPackage;
  frm: TfrmEditRelatedPackage;
begin
  frm := TfrmEditRelatedPackage.Create(Application.MainForm);
  try
    frm.SetTitle(True);
    if frm.ShowModal = mrOk then
    begin
      rp := TPackageRelatedPackage.Create(pack);
      rp.ID := frm.PackageID;
      if frm.Deprecates
        then rp.Relationship := S_RelatedPackage_Deprecates
        else rp.Relationship := '';
      pack.RelatedPackages.Add(rp);
      RefreshRelatedPackagesList;
      gridRelatedPackages.Row := gridRelatedPackages.RowCount - 1;
      Modified := True;
      EnableControls;
    end;
  finally
    frm.Free;
  end;
end;

procedure TfrmPackageEditor.cmdEditRelatedPackageClick(Sender: TObject);
var
  rp: TPackageRelatedPackage;
  frm: TfrmEditRelatedPackage;
begin
  rp := SelectedRelatedPackage;
  Assert(Assigned(rp));
  frm := TfrmEditRelatedPackage.Create(Application.MainForm);
  try
    frm.SetTitle(False);
    frm.PackageID := rp.ID;
    frm.Deprecates := rp.Relationship = S_RelatedPackage_Deprecates;
    if frm.ShowModal = mrOk then
    begin
      rp.ID := frm.PackageID;
      if frm.Deprecates
        then rp.Relationship := S_RelatedPackage_Deprecates
        else rp.Relationship := '';
      RefreshRelatedPackagesList;
      Modified := True;
      EnableControls;
    end;
  finally
    frm.Free;
  end;
end;

procedure TfrmPackageEditor.cmdRemoveRelatedPackageClick(Sender: TObject);
var
  rp: TPackageRelatedPackage;
begin
  rp := SelectedRelatedPackage;
  Assert(Assigned(rp));

  pack.RelatedPackages.Remove(rp);
  RefreshRelatedPackagesList;
  EnableDetailsTabControls;
  Modified := True;
end;

procedure TfrmPackageEditor.gridRelatedPackagesDblClick(Sender: TObject);
begin
  inherited;
  if SelectedRelatedPackage <> nil then
    cmdEditRelatedPackage.Click;
end;

{-------------------------------------------------------------------------------
 - Lexical Models tab
 -------------------------------------------------------------------------------}

(**
  Adds new lexical models to the LexicalModels object in the kmp.json and
  removes any that are no longer in the list.

  Finally, updates the LexicalModels tab
*)

procedure TfrmPackageEditor.RefreshLexicalModelList;
var
  n: Integer;
  lm: TPackageLexicalModel;
begin
  n := lbLexicalModels.ItemIndex;

  with TPackageInfoRefreshLexicalModels.Create(pack) do
  try
    OnError := Self.HandlePackageRefreshError;
    if not Execute then
      frmMessages.DoShowForm;
  finally
    Free;
  end;

  lbLexicalModels.Clear;
  for lm in pack.LexicalModels do
    lbLexicalModels.Items.AddObject(lm.ID, lm);

  if lbLexicalModels.Count = 0 then
    n := -1
  else if n >= lbLexicalModels.Count then
    n := lbLexicalModels.Count - 1
  else if (n < 0) and (lbLexicalModels.Count > 0) then
    n := 0;
  lbLexicalModels.ItemIndex := n;
  lbLexicalModelsClick(lbLexicalModels);
end;

function TfrmPackageEditor.SelectedLexicalModel: TPackageLexicalModel;
begin
  if lbLexicalModels.ItemIndex < 0
    then Result := nil
    else Result := lbLexicalModels.Items.Objects[lbLexicalModels.ItemIndex] as TPackageLexicalModel;
end;

function TfrmPackageEditor.SelectedLexicalModelLanguage: TPackageKeyboardLanguage;
var
  lm: TPackageLexicalModel;
begin
  lm := SelectedLexicalModel;
  if not Assigned(lm) then
    Exit(nil);

  if gridLexicalModelLanguages.Row = 0 then
    Exit(nil);

  Result := gridLexicalModelLanguages.Objects[0, gridLexicalModelLanguages.Row] as TPackageKeyboardLanguage;
end;

procedure TfrmPackageEditor.lbLexicalModelsClick(Sender: TObject);
var
  lm: TPackageLexicalModel;
  i: Integer;
begin
  Inc(FSetup);
  try
    gridLexicalModelLanguages.Cells[0, 0] := 'BCP 47 tag';
    gridLexicalModelLanguages.Cells[1, 0] := 'Language name';
    gridLexicalModelLanguages.ColWidths[0] := 120;
    gridLexicalModelLanguages.ColWidths[1] := 10;

    lm := SelectedLexicalModel;
    if not Assigned(lm) then
    begin
      editLexicalModelDescription.Text := '';
      editLexicalModelFilename.Text := '';
      chkLexicalModelRTL.Checked := False;
      gridLexicalModelLanguages.RowCount := 1;
      EnableLexicalModelTabControls;
      Exit;
    end;

    // Details

    editLexicalModelDescription.Text := lm.Name;
    chkLexicalModelRTL.Checked := lm.RTL;

    for i := 0 to pack.Files.Count - 1 do
      if TLexicalModelUtils.LexicalModelFileNameToID(pack.Files[i].FileName) = lm.ID then
      begin
        editLexicalModelFilename.Text := pack.Files[i].FileName;
        Break;
      end;

    // Languages

    RefreshLanguageList(gridLexicalModelLanguages, lm.Languages);
    EnableLexicalModelTabControls;
  finally
    Dec(FSetup);
  end;
end;

procedure TfrmPackageEditor.EnableLexicalModelTabControls;
var
  e: Boolean;
begin
  e := lbLexicalModels.ItemIndex >= 0;
  lblLexicalModelDescription.Enabled := e;
  editLexicalModelDescription.Enabled := e;
  lblLexicalModelFilename.Enabled := e;
  editLexicalModelFilename.Enabled := e;
  lblLexicalModelLanguages.Enabled := e;
  cmdLexicalModelLanguageAdd.Enabled := e;
  chkLexicalModelRTL.Enabled := e;

  e := e and (gridLexicalModelLanguages.Row > 0);
  gridLexicalModelLanguages.Enabled := e;
  cmdLexicalModelLanguageRemove.Enabled := e;
  cmdLexicalModelLanguageEdit.Enabled := e;
end;

procedure TfrmPackageEditor.gridLexicalModelLanguagesClick(Sender: TObject);
begin
  EnableLexicalModelTabControls;
end;

procedure TfrmPackageEditor.gridLexicalModelLanguagesDblClick(Sender: TObject);
begin
  if SelectedLexicalModelLanguage <> nil then
    cmdLexicalModelLanguageEdit.Click;
end;

procedure TfrmPackageEditor.cmdLexicalModelLanguageAddClick(Sender: TObject);
var
  lm: TPackageLexicalModel;
begin
  lm := SelectedLexicalModel;
  Assert(Assigned(lm));
  ShowAddLanguageForm(gridLexicalModelLanguages, lm.Languages);
end;

procedure TfrmPackageEditor.cmdLexicalModelLanguageEditClick(Sender: TObject);
var
  lm: TPackageLexicalModel;
  lang: TPackageKeyboardLanguage;
begin
  lm := SelectedLexicalModel;
  Assert(Assigned(lm));
  lang := SelectedLexicalModelLanguage;
  Assert(Assigned(lang));

  ShowEditLanguageForm(gridLexicalModelLanguages, lm.Languages, lang);
end;

procedure TfrmPackageEditor.cmdLexicalModelLanguageRemoveClick(Sender: TObject);
var
  lm: TPackageLexicalModel;
  lang: TPackageKeyboardLanguage;
begin
  lm := SelectedLexicalModel;
  Assert(Assigned(lm));
  lang := SelectedLexicalModelLanguage;
  Assert(Assigned(lang));

  lm.Languages.Remove(lang);
  RefreshLanguageList(gridLexicalModelLanguages, lm.Languages);
  EnableLexicalModelTabControls;
  Modified := True;
end;

procedure TfrmPackageEditor.editLexicalModelDescriptionChange(Sender: TObject);
var
  lm: TPackageLexicalModel;
begin
  if FSetup > 0 then
    Exit;

  lm := SelectedLexicalModel;
  Assert(Assigned(lm));
  lm.Name := editLexicalModelDescription.Text;
  Modified := True;
end;

procedure TfrmPackageEditor.chkLexicalModelRTLClick(Sender: TObject);
var
  lm: TPackageLexicalModel;
begin
  if FSetup > 0 then
    Exit;
  lm := SelectedLexicalModel;
  Assert(Assigned(lm));
  lm.RTL := chkLexicalModelRTL.Checked;
  Modified := True;
end;

procedure TfrmPackageEditor.UpdateQRCode;
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

