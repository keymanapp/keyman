(*
  Name:             dmActionsMain
  Copyright:        Copyright (C) SIL International.
  Documentation:    
  Description:      
  Create Date:      23 Aug 2006

  Modified Date:    3 Aug 2015
  Authors:          mcdurdin
  Related Files:    
  Dependencies:     

  Bugs:             
  Todo:             
  Notes:            
  History:          23 Aug 2006 - mcdurdin - Initial version
                    14 Sep 2006 - mcdurdin - Add CRM action
                    14 Sep 2006 - mcdurdin - Move font bold issue to UFixFontDialogBold unit
                    28 Sep 2006 - mcdurdin - Added check for updates
                    28 Sep 2006 - mcdurdin - Fixed View Help menu item state
                    04 Dec 2006 - mcdurdin - Tweak file editing for different file types, extensions, etc
                    04 Dec 2006 - mcdurdin - Localize
                    04 Jan 2007 - mcdurdin - Test for branding pack installation when opening branding file, opening CRM
                    04 Jan 2007 - mcdurdin - Fix scroll buttons on page control
                    04 Jan 2007 - mcdurdin - Add help support
                    04 Jan 2007 - mcdurdin - Add proxy support
                    15 Jan 2007 - mcdurdin - Disable page setup, print and print preview if no printers installed
                    25 Jan 2007 - mcdurdin - Fix search and replace in TPlus-MemoU
                    19 Mar 2007 - mcdurdin - I706 - Fix Save Copy As command
                    21 Mar 2007 - mcdurdin - I706 - Fixed extension and file type in Save Copy As
                    16 May 2007 - mcdurdin - I787 - Don't crash if Ctrl+F4 pressed when no windows open
                    30 May 2007 - mcdurdin - I825 - Added proxy username and password
                    19 Nov 2007 - mcdurdin - I1157 - const string parameters
                    20 Jul 2008 - mcdurdin - I1503 - Ctrl+F not working in Character Map
                    25 May 2010 - mcdurdin - I2392 - Activation Client integration
                    26 Jul 2010 - mcdurdin - I2467 - 8.0 renumber
                    17 Dec 2010 - mcdurdin - I2595 - Remove GnuGetText
                    18 May 2012 - mcdurdin - I3306 - V9.0 - Remove TntControls + Win9x support
                    17 Aug 2012 - mcdurdin - I3377 - KM9 - Update code references from 8.0 to 9.0
                    06 Feb 2012 - mcdurdin - I3082 - Reload text file with specific encoding support
                    03 Nov 2012 - mcdurdin - I3502 - V9.0 - Merge of I3082 - Reload text file with specific encoding support
                    30 Apr 2015 - mcdurdin - I4678 - V9.0 - Fixup Ctrl+PgUp, Ctrl+PgDn, Alt+Left, Alt+Right hotkeys
                    04 May 2015 - mcdurdin - I4687 - V9.0 - Split project UI actions into separate classes
                    04 May 2015 - mcdurdin - I4688 - V9.0 - Add build path to project settings
                    03 Aug 2015 - mcdurdin - I4807 - Add Character Identifier to Keyman Developer
*)
unit dmActionsMain;  // I3306

interface

uses
  System.UITypes,
  SysUtils, Classes, StdActns, ActnList, ImgList,
  Controls, MenuImgList, KMDActions, XPStyleActnCtrls, ActnMan,
  KMDActionInterfaces, Windows, System.ImageList, System.Actions;

type
  TmodActionsMain = class(TDataModule)
    actionsMain: TActionList;
    actFileNew: TAction;
    actFileOpen: TFileOpen;
    actFileSave: TAction;
    actFileSaveAs: TFileSaveAs;
    actFileSaveCopyAs: TAction;
    actFileRevert: TAction;
    actFilePageSetup: TFilePageSetup;
    actFilePrint: TPrintDlg;
    actFilePrintPreview: TAction;
    actFileExit: TFileExit;
    actEditCut: TKMDEditCut;
    actEditCopy: TKMDEditCopy;
    actEditPaste: TKMDEditPaste;
    actEditSelectAll: TKMDEditSelectAll;
    actEditDelete: TKMDEditDelete;
    actEditUndo: TKMDEditUndo;
    actEditRedo: TKMDEditRedo;
    actSearchFind: TKMDSearchFind;
    actSearchFindNext: TKMDSearchFindNext;
    actSearchReplace: TKMDSearchReplace;
    actHelpContents: THelpContents;
    actHelpTopicSearch: THelpTopicSearch;
    actHelpContextAction: THelpContextAction;
    actViewCharacterMap: TAction;
    actViewMessages: TAction;
    actViewProject: TAction;
    actViewToolbar: TAction;
    actViewStatusBar: TAction;
    actProjectNew: TAction;
    actProjectOpen: TFileOpen;
    actProjectSaveAs: TFileSaveAs;
    actProjectAddCurrentEditorFile: TAction;
    actProjectAddFiles: TFileOpen;
    actProjectSettings: TAction;
    actToolsCustomise: TAction;
    actToolsOptions: TAction;
    actToolsVirtualKeyIdentifier: TAction;
    actHelpAbout: TAction;
    actViewCodeFont: TFontEdit;
    actViewCharFont: TFontEdit;
    actViewExpandEditor: TAction;
    ActionManager1: TActionManager;
    actViewHelp: TAction;
    actWindowNext: TAction;
    actWindowPrev: TAction;
    actWindowClose: TAction;
    ilMain: TImageList;
    ilEditorPages: TImageList;
    actToolsFileFormat: TAction;
    actViewMessageNext: TAction;
    actViewMessagePrevious: TAction;
    actViewTabNext: TAction;
    actViewTabPrevious: TAction;
    actHelpCheckForUpdates: TAction;
    actToolsReloadANSI: TAction;
    actToolsReloadUTF8: TAction;
    actToolsReloadUTF16: TAction;
    actToolsReload: TAction;
    actViewDesign: TAction;   // I4678
    actViewCode: TAction;   // I4678
    actViewCharacterIdentifier: TAction;   // I4807
    procedure actFileNewExecute(Sender: TObject);
    procedure DataModuleCreate(Sender: TObject);
    procedure actFileOpenAccept(Sender: TObject);
    procedure actFileSaveExecute(Sender: TObject);
    procedure actFileSaveCopyAsExecute(Sender: TObject);
    procedure actFileRevertExecute(Sender: TObject);
    procedure actFilePrintPreviewExecute(Sender: TObject);
    procedure actFilePrintAccept(Sender: TObject);
    procedure actFilePrintPreviewUpdate(Sender: TObject);
    procedure actFilePrintUpdate(Sender: TObject);
    procedure actFileSaveAsAccept(Sender: TObject);
    procedure actFileSaveCopyAsUpdate(Sender: TObject);
    procedure actFileSaveAsUpdate(Sender: TObject);
    procedure actFileRevertUpdate(Sender: TObject);
    procedure actFilePageSetupUpdate(Sender: TObject);
    procedure actFileSaveUpdate(Sender: TObject);
    procedure actFileSaveAsBeforeExecute(Sender: TObject);
    procedure actViewCharacterMapExecute(Sender: TObject);
    procedure actViewMessagesExecute(Sender: TObject);
    procedure actViewProjectExecute(Sender: TObject);
    procedure actViewToolbarExecute(Sender: TObject);
    procedure actViewStatusBarExecute(Sender: TObject);
    procedure actViewCharacterMapUpdate(Sender: TObject);
    procedure actViewMessagesUpdate(Sender: TObject);
    procedure actViewProjectUpdate(Sender: TObject);
    procedure actViewToolbarUpdate(Sender: TObject);
    procedure actViewStatusBarUpdate(Sender: TObject);
    procedure actToolsOptionsExecute(Sender: TObject);
    procedure actToolsVirtualKeyIdentifierExecute(Sender: TObject);
    procedure actProjectNewExecute(Sender: TObject);
    procedure actProjectSaveExecute(Sender: TObject);
    procedure actProjectSaveAsBeforeExecute(Sender: TObject);
    procedure actProjectAddCurrentEditorFileExecute(Sender: TObject);
    procedure actProjectSettingsExecute(Sender: TObject);
    procedure actProjectOpenAccept(Sender: TObject);
    procedure actProjectSaveAsAccept(Sender: TObject);
    procedure actProjectAddFilesAccept(Sender: TObject);
    procedure actProjectAddCurrentEditorFileUpdate(Sender: TObject);
    procedure actHelpContentsExecute(Sender: TObject);
    procedure actHelpAboutExecute(Sender: TObject);
    procedure actViewCharFontAccept(Sender: TObject);
    procedure actViewCodeFontBeforeExecute(Sender: TObject);
    procedure actViewCodeFontAccept(Sender: TObject);
    procedure actViewCharFontBeforeExecute(Sender: TObject);
    procedure actViewCodeFontUpdate(Sender: TObject);
    procedure actViewCharFontUpdate(Sender: TObject);
    procedure actViewExpandEditorUpdate(Sender: TObject);
    procedure actViewExpandEditorExecute(Sender: TObject);
    procedure actViewHelpExecute(Sender: TObject);
    procedure actWindowNextExecute(Sender: TObject);
    procedure actWindowPrevExecute(Sender: TObject);
    procedure actWindowCloseUpdate(Sender: TObject);
    procedure actWindowPrevUpdate(Sender: TObject);
    procedure actWindowNextUpdate(Sender: TObject);
    procedure actWindowCloseExecute(Sender: TObject);
    procedure actViewCharFontFontDialogApply(Sender: TObject; Wnd: HWND);
    procedure actViewCodeFontFontDialogApply(Sender: TObject; Wnd: HWND);
    procedure actToolsFileFormatExecute(Sender: TObject);
    procedure actToolsFileFormatUpdate(Sender: TObject);
    procedure actViewMessagePreviousExecute(Sender: TObject);
    procedure actViewMessageNextExecute(Sender: TObject);
    procedure actViewMessagePreviousUpdate(Sender: TObject);
    procedure actViewMessageNextUpdate(Sender: TObject);
    procedure actViewTabNextUpdate(Sender: TObject);
    procedure actViewTabPreviousUpdate(Sender: TObject);
    procedure actViewTabNextExecute(Sender: TObject);
    procedure actViewTabPreviousExecute(Sender: TObject);
    procedure actHelpCheckForUpdatesExecute(Sender: TObject);
    procedure actViewHelpUpdate(Sender: TObject);
    procedure actViewCharFontFontDialogShow(Sender: TObject);
    procedure actViewCodeFontFontDialogShow(Sender: TObject);
    procedure actSearchFindBeforeExecute(Sender: TObject; var Cancel: Boolean);
    procedure actSearchFindUpdate(Sender: TObject);
    procedure actToolsReloadANSIExecute(Sender: TObject);
    procedure actToolsReloadANSIUpdate(Sender: TObject);
    procedure actToolsReloadUTF8Execute(Sender: TObject);
    procedure actToolsReloadUTF8Update(Sender: TObject);
    procedure actToolsReloadUTF16Execute(Sender: TObject);
    procedure actToolsReloadUTF16Update(Sender: TObject);
    procedure actViewDesignUpdate(Sender: TObject);   // I4678
    procedure actViewDesignExecute(Sender: TObject);   // I4678
    procedure actViewCodeUpdate(Sender: TObject);   // I4678
    procedure actViewCodeExecute(Sender: TObject);   // I4678
    procedure actViewCharacterIdentifierExecute(Sender: TObject);   // I4807
    procedure actViewCharacterIdentifierUpdate(Sender: TObject);   // I4807
  private
    procedure AboutShowStartup(Sender: TObject);
  public
    procedure OpenProject(FileName: WideString);
  end;

var
  modActionsMain: TmodActionsMain;

implementation

{$R *.dfm}

uses
  Dialogs,
  Forms,
  Graphics,
  Messages,
  OnlineConstants,
  OnlineUpdateCheck,
  Printers,
  Project,
  ProjectFile,
  ProjectFileType,
  ProjectFileUI,
  ProjectUI,
  GlobalProxySettings,
  RegistryKeys,
  TextFileFormat,
  UFixFontDialogBold,
  UfrmAboutTike,
  UfrmBitmapEditor,
  UfrmCharacterIdentifier,
  UfrmCharacterMapDock,
  UfrmCharacterMapNew,
  UfrmEditor,
  UfrmHelp,
  UfrmKeymanWizard,
  UfrmKeyTest,
  UfrmMain,
  UfrmMessages,
  UfrmMDIEditor,
  UfrmNew,
  UfrmOptions,
  UfrmOSKEditor,
  UfrmPackageEditor,
  UfrmProject,
  UfrmProjectSettings,
  UfrmStartup,
  Upload_Settings,
  utilfiletypes, UfrmMDIChild;

procedure TmodActionsMain.actFileNewExecute(Sender: TObject);
var
  FEditor: TfrmTikeEditor;
begin
  with TfrmNew.Create(frmKeymanDeveloper) do
  try
    if ShowModal = mrOk then
    begin
      case FileType of
        ftKeymanSource:    FEditor := TfrmKeymanWizard.Create(frmKeymanDeveloper);
        ftPackageSource:   FEditor := TfrmPackageEditor.Create(frmKeymanDeveloper);
        ftBitmap:          FEditor := TfrmBitmapEditor.Create(frmKeymanDeveloper);
        ftVisualKeyboard:  FEditor := TfrmOSKEditor.Create(frmKeymanDeveloper);
        ftTextFile:        begin FEditor := TfrmEditor.Create(frmKeymanDeveloper); (FEditor as TfrmEditor).EditorFormat := efText; end;
        ftXMLFile:         begin FEditor := TfrmEditor.Create(frmKeymanDeveloper); (FEditor as TfrmEditor).EditorFormat := efXML; end;
        ftHTMLFile:        begin FEditor := TfrmEditor.Create(frmKeymanDeveloper); (FEditor as TfrmEditor).EditorFormat := efHTML; end;
      else
        FEditor := TfrmEditor.Create(frmKeymanDeveloper);
      end;
      if FileName <> '' then
      begin
        if AddToProject then
          FEditor.ProjectFile := CreateProjectFile(FGlobalProject, FileName, nil);
        FEditor.OpenFile(FileName);
      end;
    end;
  finally
    Free;
  end;
end;

procedure TmodActionsMain.actFileOpenAccept(Sender: TObject);
var
  i: Integer;
begin
  for i := 0 to actFileOpen.Dialog.Files.Count - 1 do
    frmKeymanDeveloper.OpenFile(actFileOpen.Dialog.Files[i], True);
end;

procedure TmodActionsMain.actFilePageSetupUpdate(Sender: TObject);
begin
  with frmKeymanDeveloper do
    actFilePageSetup.Enabled := Assigned(ActiveChild) and Supports(ActiveChild, IKMDPrintActions) and (Printer.Printers.Count > 0);
end;

procedure TmodActionsMain.actFilePrintAccept(Sender: TObject);
begin
  (frmKeymanDeveloper.ActiveChild as IKMDPrintActions).PrintFile;
end;

procedure TmodActionsMain.actFilePrintUpdate(Sender: TObject);
begin
  with frmKeymanDeveloper do
    actFilePrint.Enabled := Assigned(ActiveChild) and Supports(ActiveChild, IKMDPrintActions) and (Printer.Printers.Count > 0);
end;

procedure TmodActionsMain.actFilePrintPreviewExecute(Sender: TObject);
begin
  (frmKeymanDeveloper.ActiveChild as IKMDPrintPreviewActions).PrintPreview;
end;

procedure TmodActionsMain.actFilePrintPreviewUpdate(Sender: TObject);
begin
  with frmKeymanDeveloper do
    actFilePrintPreview.Enabled := Assigned(ActiveChild) and Supports(ActiveChild, IKMDPrintPreviewActions) and (Printer.Printers.Count > 0);
end;

procedure TmodActionsMain.actFileRevertExecute(Sender: TObject);
begin
  if MessageDlg('Are you sure you want to revert to the last saved version?',
      mtConfirmation, mbYesNoCancel, 0) = mrYes then
    frmKeymanDeveloper.ActiveEditor.Revert;
end;

procedure TmodActionsMain.actFileRevertUpdate(Sender: TObject);
begin
  with frmKeymanDeveloper do
    actFileRevert.Enabled := Assigned(ActiveEditor);
end;

procedure TmodActionsMain.actFileSaveAsAccept(Sender: TObject);
begin
  with frmKeymanDeveloper do
    if ActiveEditor.SaveFile(actFileSaveAs.Dialog.FileName, True) then
      AddMRU(actFileSaveAs.Dialog.FileName);
end;

procedure TmodActionsMain.actFileSaveAsBeforeExecute(Sender: TObject);
begin
  with frmKeymanDeveloper do
  begin
    actFileSaveAs.Dialog.DefaultExt := ActiveEditor.DefaultExt;
    actFileSaveAs.Dialog.Filter := ActiveEditor.FileNameFilter;
    actFileSaveAs.Dialog.FileName := ActiveEditor.FileName;
  end;
end;

procedure TmodActionsMain.actFileSaveAsUpdate(Sender: TObject);
begin
  with frmKeymanDeveloper do
    actFileSaveAs.Enabled := Assigned(ActiveEditor);
end;

procedure TmodActionsMain.actFileSaveCopyAsExecute(Sender: TObject);
begin
  actFileSaveAs.Dialog.Title := 'Save Copy As';
  try
    with frmKeymanDeveloper do
    begin
      actFileSaveAs.Dialog.DefaultExt := ActiveEditor.DefaultExt;
      actFileSaveAs.Dialog.Filter := ActiveEditor.FileNameFilter;
      actFileSaveAs.Dialog.FileName := ActiveEditor.FileName;
    end;

    if actFileSaveAs.Dialog.Execute then
      with frmKeymanDeveloper do
        ActiveEditor.SaveFile(actFileSaveAs.Dialog.FileName, False);
  finally
    actFileSaveAs.Dialog.Title := 'Save As';
  end;
end;

procedure TmodActionsMain.actFileSaveCopyAsUpdate(Sender: TObject);
begin
  with frmKeymanDeveloper do
    actFileSaveCopyAs.Enabled := Assigned(ActiveEditor);
end;

procedure TmodActionsMain.actFileSaveExecute(Sender: TObject);
begin
  with frmKeymanDeveloper do
    if Assigned(ActiveEditor) then
    begin
      if ActiveEditor.Untitled then
      begin
        actFileSaveAs.Execute;
      end
      else
      begin
        ActiveEditor.SaveFile(ActiveEditor.FileName, True);
        AddMRU(ActiveEditor.FileName);
      end;
    end;
end;

procedure TmodActionsMain.actFileSaveUpdate(Sender: TObject);
begin
  with frmKeymanDeveloper do
    actFileSave.Enabled := Assigned(ActiveEditor) and ActiveEditor.Modified;
end;

procedure TmodActionsMain.AboutShowStartup(Sender: TObject);
begin
  ForceShowStartup(Sender as TForm);
end;

procedure TmodActionsMain.actHelpAboutExecute(Sender: TObject);
begin
  with TfrmAboutTike.Create(frmKeymanDeveloper) do
  try
    OnIconClick := AboutShowStartup;
    ShowModal;
  finally
    Free;
  end;
end;

procedure TmodActionsMain.actHelpCheckForUpdatesExecute(Sender: TObject);
begin
  if TOnlineUpdateCheck.Running then
  begin
    ShowMessage('An online update check is already running.');
    Exit;
  end;

  with TOnlineUpdateCheck.Create(SRegKey_KeymanDeveloper_CU, OnlineProductID_KeymanDeveloper_100, True, False, False, GetProxySettings.Server, GetProxySettings.Port, GetProxySettings.Username, GetProxySettings.Password) do  // I3377
  try
    if Run = oucShutDown then
      frmKeymanDeveloper.Close;
  finally
    Free;
  end;
end;

procedure TmodActionsMain.actHelpContentsExecute(Sender: TObject);
begin
  frmKeymanDeveloper.HelpTopic('index');
end;

procedure TmodActionsMain.actProjectAddCurrentEditorFileExecute(
  Sender: TObject);
begin
  with frmKeymanDeveloper do
  begin
    if not Assigned(ActiveEditor) or ActiveEditor.Untitled then Exit;
    if FGlobalProject.Files.IndexOfFileName(ActiveEditor.FileName) >= 0 then Exit;
    ActiveEditor.ProjectFile := CreateProjectFile(FGlobalProject, ActiveEditor.FileName, nil);
    ShowProject;
  end;
end;

procedure TmodActionsMain.actProjectAddCurrentEditorFileUpdate(Sender: TObject);
begin
  actProjectAddCurrentEditorFile.Enabled :=
    Assigned(frmKeymanDeveloper.ActiveEditor) and
    not frmKeymanDeveloper.ActiveEditor.Untitled and
    (not Assigned(frmKeymanDeveloper.ActiveEditor.ProjectFile) or
    (frmKeymanDeveloper.ActiveEditor.ProjectFile.Project <> FGlobalProject));
end;

procedure TmodActionsMain.actProjectAddFilesAccept(Sender: TObject);
var
  i: Integer;
begin
  for i := 0 to actProjectAddFiles.Dialog.Files.Count - 1 do
    if FGlobalProject.Files.IndexOfFileName(actProjectAddFiles.Dialog.Files[i]) < 0 then
      CreateProjectFile(FGlobalProject, actProjectAddFiles.Dialog.Files[i], nil);
  frmKeymanDeveloper.ShowProject;
end;

procedure TmodActionsMain.actProjectNewExecute(Sender: TObject);
begin
  with frmKeymanDeveloper do
  begin
    FGlobalProject.Save;
    ProjectForm.Free;
    FreeGlobalProjectUI;
    LoadGlobalProjectUI('');
    ShowProject;
  end;
end;

procedure TmodActionsMain.actProjectOpenAccept(Sender: TObject);
begin
  OpenProject(actProjectOpen.Dialog.FileName);
end;

procedure TmodActionsMain.OpenProject(FileName: WideString);
begin
  if (FileName <> '') and not FileExists(FileName) then
  begin
    ShowMessage('The project '+FileName+' does not exist.');
    Exit;
  end;

  FGlobalProject.Save;
  FreeGlobalProjectUI;
  LoadGlobalProjectUI(FileName);   // I4687
  frmKeymanDeveloper.ProjectMRU.Add(FGlobalProject.FileName);
  frmKeymanDeveloper.ShowProject;
end;

procedure TmodActionsMain.actProjectSaveAsAccept(Sender: TObject);
begin
  FGlobalProject.FileName := actProjectSaveAs.Dialog.FileName;
  FGlobalProject.Save;
  frmKeymanDeveloper.ProjectMRU.Add(FGlobalProject.FileName);
end;

procedure TmodActionsMain.actProjectSaveAsBeforeExecute(Sender: TObject);
begin
  actProjectSaveAs.Dialog.FileName := FGlobalProject.FileName;
end;

procedure TmodActionsMain.actProjectSaveExecute(Sender: TObject);
begin
  if FGlobalProject.Untitled
    then actProjectSaveAs.Execute
    else FGlobalProject.Save;
end;

procedure TmodActionsMain.actProjectSettingsExecute(Sender: TObject);
begin
  with TfrmProjectSettings.Create(Screen.ActiveForm) do   // I4688
  try
    ShowModal;
  finally
    Free;
  end;
end;

procedure TmodActionsMain.actSearchFindBeforeExecute(Sender: TObject;
  var Cancel: Boolean);
begin
  if frmCharacterMapNew.ContainsControl(Screen.ActiveControl) then
  begin
    frmCharacterMapNew.editFilter.SetFocus;
    Cancel := True;
  end;
end;

procedure TmodActionsMain.actSearchFindUpdate(Sender: TObject);
begin
  if frmCharacterMapNew.ContainsControl(Screen.ActiveControl) then
  begin
    actSearchFind.Enabled := True;
  end;
end;

procedure TmodActionsMain.actToolsFileFormatExecute(Sender: TObject);
begin
  frmKeymanDeveloper.ActiveChild.TextFileFormatClick;
end;

procedure TmodActionsMain.actToolsFileFormatUpdate(Sender: TObject);
begin
  actToolsFileFormat.Enabled := Assigned(frmKeymanDeveloper.ActiveChild) and
    frmKeymanDeveloper.ActiveChild.CanTextFileFormatClick;
  frmKeymanDeveloper.Reloadwithencoding1.Enabled := Assigned(frmKeymanDeveloper.ActiveChild) and  // I3082   // I3502
    frmKeymanDeveloper.ActiveChild.CanReloadAsTextFileFormatClick;  // because linking to the action causes the dropdown menu to break...
end;

procedure TmodActionsMain.actToolsOptionsExecute(Sender: TObject);
begin
  with TfrmOptions.Create(frmKeymanDeveloper) do
  try
    if ShowModal = mrOk then
    begin
      frmKeymanDeveloper.RefreshOptions;
    end;
  finally
    Free;
  end;
end;

procedure TmodActionsMain.actToolsReloadANSIExecute(Sender: TObject);  // I3082   // I3502
begin
  frmKeymanDeveloper.ActiveChild.ReloadAsTextFileFormatClick(tffANSI);
end;

procedure TmodActionsMain.actToolsReloadANSIUpdate(Sender: TObject);  // I3082   // I3502
begin
  actToolsReloadANSI.Enabled := Assigned(frmKeymanDeveloper.ActiveChild) and
    frmKeymanDeveloper.ActiveChild.CanReloadAsTextFileFormatClick;
end;

procedure TmodActionsMain.actToolsReloadUTF16Execute(Sender: TObject);  // I3082   // I3502
begin
  frmKeymanDeveloper.ActiveChild.ReloadAsTextFileFormatClick(tffUTF16);
end;

procedure TmodActionsMain.actToolsReloadUTF16Update(Sender: TObject);  // I3082   // I3502
begin
  actToolsReloadUTF16.Enabled := Assigned(frmKeymanDeveloper.ActiveChild) and
    frmKeymanDeveloper.ActiveChild.CanReloadAsTextFileFormatClick;
end;

procedure TmodActionsMain.actToolsReloadUTF8Execute(Sender: TObject);  // I3082   // I3502
begin
  frmKeymanDeveloper.ActiveChild.ReloadAsTextFileFormatClick(tffUTF8);
end;

procedure TmodActionsMain.actToolsReloadUTF8Update(Sender: TObject); // I3082   // I3502
begin
  actToolsReloadUTF8.Enabled := Assigned(frmKeymanDeveloper.ActiveChild) and
    frmKeymanDeveloper.ActiveChild.CanReloadAsTextFileFormatClick;
end;

procedure TmodActionsMain.actToolsVirtualKeyIdentifierExecute(Sender: TObject);
begin
  with TfrmKeyTest.Create(frmKeymanDeveloper) do
  try
    ShowModal;
  finally
    Free;
  end;
end;

procedure TmodActionsMain.actViewCharacterIdentifierExecute(Sender: TObject);   // I4807
begin
  frmCharacterIdentifier.ToggleVisibility;
end;

procedure TmodActionsMain.actViewCharacterIdentifierUpdate(Sender: TObject);   // I4807
begin
  with frmKeymanDeveloper do
    actViewCharacterIdentifier.Checked := Assigned(frmCharacterIdentifier) and frmCharacterIdentifier.Visible;
end;

procedure TmodActionsMain.actViewCharacterMapExecute(Sender: TObject);
begin
  frmCharacterMapDock.ToggleVisibility;
end;

procedure TmodActionsMain.actViewCharacterMapUpdate(Sender: TObject);
begin
  actViewCharacterMap.Checked := Assigned(frmCharacterMapDock) and frmCharacterMapDock.Visible;
end;

procedure TmodActionsMain.actViewCharFontAccept(Sender: TObject);
begin
  frmKeymanDeveloper.ActiveEditor.CharFont := FixFontDialogBold(actViewCharFont.Dialog.Font);
end;

procedure TmodActionsMain.actViewCharFontBeforeExecute(Sender: TObject);
begin
  actViewCharFont.Dialog.Font := frmKeymanDeveloper.ActiveEditor.CharFont;
end;

procedure TmodActionsMain.actViewCharFontFontDialogApply(Sender: TObject; Wnd: HWND);
begin
  frmKeymanDeveloper.ActiveEditor.CharFont := FixFontDialogBold(actViewCharFont.Dialog.Font);
end;

procedure TmodActionsMain.actViewCharFontFontDialogShow(Sender: TObject);
begin
  SetWindowText(actViewCharFont.Dialog.Handle, PChar('Character Font'));
end;

procedure TmodActionsMain.actViewCharFontUpdate(Sender: TObject);
begin
  actViewCharFont.Enabled := Assigned(frmKeymanDeveloper.ActiveEditor);
end;

procedure TmodActionsMain.actViewCodeFontAccept(Sender: TObject);
begin
  frmKeymanDeveloper.ActiveEditor.CodeFont := FixFontDialogBold(actViewCodeFont.Dialog.Font);
end;

procedure TmodActionsMain.actViewCodeFontBeforeExecute(Sender: TObject);
begin
  actViewCodeFont.Dialog.Font := frmKeymanDeveloper.ActiveEditor.CodeFont;
end;

procedure TmodActionsMain.actViewCodeFontFontDialogApply(Sender: TObject; Wnd: HWND);
begin
  frmKeymanDeveloper.ActiveEditor.CodeFont := FixFontDialogBold(actViewCodeFont.Dialog.Font);
end;

procedure TmodActionsMain.actViewCodeFontFontDialogShow(Sender: TObject);
begin
  SetWindowText(actViewCharFont.Dialog.Handle, PChar('Code Font'));
end;

procedure TmodActionsMain.actViewCodeFontUpdate(Sender: TObject);
begin
  actViewCodeFont.Enabled := Assigned(frmKeymanDeveloper.ActiveEditor);
end;

procedure TmodActionsMain.actViewCodeExecute(Sender: TObject);   // I4678
begin
  frmKeymanDeveloper.ActiveChild.ChangeView(cdvCode);
end;

procedure TmodActionsMain.actViewCodeUpdate(Sender: TObject);   // I4678
begin
  actViewTabNext.Enabled := Assigned(frmKeymanDeveloper.ActiveChild) and frmKeymanDeveloper.ActiveChild.CanChangeView(cdvCode);
end;

procedure TmodActionsMain.actViewDesignExecute(Sender: TObject);   // I4678
begin
  frmKeymanDeveloper.ActiveChild.ChangeView(cdvDesign);
end;

procedure TmodActionsMain.actViewDesignUpdate(Sender: TObject);   // I4678
begin
  actViewTabNext.Enabled := Assigned(frmKeymanDeveloper.ActiveChild) and frmKeymanDeveloper.ActiveChild.CanChangeView(cdvDesign);
end;

procedure TmodActionsMain.actViewExpandEditorExecute(Sender: TObject);
begin
  (frmKeymanDeveloper.ActiveChild as IKMDViewExpandEditorActions).ExpandContractEditor;
end;

procedure TmodActionsMain.actViewExpandEditorUpdate(Sender: TObject);
begin
  actViewExpandEditor.Enabled := Assigned(frmKeymanDeveloper.ActiveChild) and
    Supports(frmKeymanDeveloper.ActiveChild, IKMDViewExpandEditorActions);
  actViewExpandEditor.Checked := actViewExpandEditor.Enabled and
    (frmKeymanDeveloper.ActiveChild as IKMDViewExpandEditorActions ).IsEditorExpanded;
end;

procedure TmodActionsMain.actViewHelpExecute(Sender: TObject);
begin
  frmHelp.ToggleVisibility;
end;

procedure TmodActionsMain.actViewHelpUpdate(Sender: TObject);
begin
  with frmKeymanDeveloper do
    actViewHelp.Checked := frmHelp.Visible;
end;

procedure TmodActionsMain.actViewMessageNextExecute(Sender: TObject);
begin
  frmMessages.NextMessage;
end;

procedure TmodActionsMain.actViewMessageNextUpdate(Sender: TObject);
begin
  actViewMessageNext.Enabled := frmMessages.HasNextMessage;
end;

procedure TmodActionsMain.actViewMessagePreviousExecute(Sender: TObject);
begin
  frmMessages.PrevMessage;
end;

procedure TmodActionsMain.actViewMessagePreviousUpdate(Sender: TObject);
begin
  actViewMessagePrevious.Enabled := frmMessages.HasPrevMessage;
end;

procedure TmodActionsMain.actViewMessagesExecute(Sender: TObject);
begin
  frmMessages.ToggleVisibility;
end;

procedure TmodActionsMain.actViewMessagesUpdate(Sender: TObject);
begin
  with frmKeymanDeveloper do
    actViewMessages.Checked := frmMessages.Visible; //panMessages.Visible;
end;

procedure TmodActionsMain.actViewProjectExecute(Sender: TObject);
var
  frmProject: TfrmProject;
begin
  with frmKeymanDeveloper do
  begin
    frmProject := ProjectForm;
    if Assigned(frmProject) and (ActiveChild = frmProject)
      then PostMessage(frmProject.Handle, WM_CLOSE, 0, 0)
      else ShowProject;
  end;
end;

procedure TmodActionsMain.actViewProjectUpdate(Sender: TObject);
begin
  with frmKeymanDeveloper do
    actViewProject.Checked := ActiveChild is TfrmProject;
end;

procedure TmodActionsMain.actViewStatusBarExecute(Sender: TObject);
begin
  with frmKeymanDeveloper do
    barStatus.Visible := not barStatus.Visible;
end;

procedure TmodActionsMain.actViewStatusBarUpdate(Sender: TObject);
begin
  with frmKeymanDeveloper do
    actViewStatusBar.Checked := barStatus.Visible;
end;

procedure TmodActionsMain.actViewTabNextExecute(Sender: TObject);
begin
  frmKeymanDeveloper.ActiveChild.ChangeTab(True);
end;

procedure TmodActionsMain.actViewTabNextUpdate(Sender: TObject);
begin
  actViewTabNext.Enabled := Assigned(frmKeymanDeveloper.ActiveChild) and frmKeymanDeveloper.ActiveChild.CanChangeTab(True);
end;

procedure TmodActionsMain.actViewTabPreviousExecute(Sender: TObject);
begin
  frmKeymanDeveloper.ActiveChild.ChangeTab(False);
end;

procedure TmodActionsMain.actViewTabPreviousUpdate(Sender: TObject);
begin
  actViewTabPrevious.Enabled := Assigned(frmKeymanDeveloper.ActiveChild) and frmKeymanDeveloper.ActiveChild.CanChangeTab(False);
end;

procedure TmodActionsMain.actViewToolbarExecute(Sender: TObject);
begin
  with frmKeymanDeveloper do
    barTools.Visible := not barTools.Visible;
end;

procedure TmodActionsMain.actViewToolbarUpdate(Sender: TObject);
begin
  with frmKeymanDeveloper do
    actViewToolbar.Checked := barTools.Visible;
end;

procedure TmodActionsMain.actWindowCloseExecute(Sender: TObject);
begin
  PostMessage(frmKeymanDeveloper.ActiveChild.Handle, WM_CLOSE, 0, 0);
end;

procedure TmodActionsMain.actWindowCloseUpdate(Sender: TObject);
begin
  actWindowClose.Enabled := Assigned(frmKeymanDeveloper.ActiveChild);
end;

procedure TmodActionsMain.actWindowNextExecute(Sender: TObject);
begin
  with frmKeymanDeveloper do
  begin
    if ActiveChildIndex = ChildWindows.Count - 1
      then ActiveChildIndex := 0
      else ActiveChildIndex := ActiveChildIndex + 1;
    pagesChange(pages);
  end;
end;

procedure TmodActionsMain.actWindowNextUpdate(Sender: TObject);
begin
  actWindowNext.Enabled := (frmKeymanDeveloper.ChildWindows.Count > 1) and Assigned(frmKeymanDeveloper.ActiveChild);
end;

procedure TmodActionsMain.actWindowPrevExecute(Sender: TObject);
begin
  with frmKeymanDeveloper do
  begin
    if ActiveChildIndex = 0
      then ActiveChildIndex := ChildWindows.Count - 1
      else ActiveChildIndex := ActiveChildIndex - 1;
    pagesChange(pages);
  end;
end;

procedure TmodActionsMain.actWindowPrevUpdate(Sender: TObject);
begin
  actWindowNext.Enabled := (frmKeymanDeveloper.ChildWindows.Count > 1) and Assigned(frmKeymanDeveloper.ActiveChild);
end;

procedure TmodActionsMain.DataModuleCreate(Sender: TObject);
begin
  actFileOpen.Dialog.Filter :=
    'Keyman source files|*.kmn;*.kps;*.kpp;*.txt;*.bmp;*.kpj;*.kvk;*.kct|'+
    'Keyboard files (*.kmn)|*.kmn|'+
    'Package files (*.kps)|*.kps|'+
    'Product files (*.kpp)|*.kpp|'+
    'Customisation files (*.kct)|*.kct|'+
    'Text files (*.txt)|*.txt|'+
    'Virtual keyboard files (*.kvk)|*.kvk|'+
    'Bitmap files (*.bmp)|*.bmp|'+
    'TIKE projects (*.kpj)|*.kpj|'+
    'Compiled keyboard files (*.kmx)|*.kmx|'+
    'HTML files (*.htm, *.html)|*.htm?|'+
    'XML files (*.xml)|*.xml|'+
    'All files (*.*)|*.*';
end;

end.
