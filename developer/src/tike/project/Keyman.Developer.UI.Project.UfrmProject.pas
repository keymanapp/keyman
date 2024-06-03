(*
  Name:             Keyman.Developer.UI.Project.UfrmProject
  Copyright:        Copyright (C) SIL International.
  Documentation:
  Description:
  Create Date:      20 Jun 2006

  Modified Date:    9 Aug 2015
  Authors:          mcdurdin
  Related Files:
  Dependencies:

  Bugs:
  Todo:
  Notes:
  History:          20 Jun 2006 - mcdurdin - Initial version
                    01 Aug 2006 - mcdurdin - Rework for Keyman 7
                    23 Aug 2006 - mcdurdin - Fix broken focus of web browser
                    23 Aug 2006 - mcdurdin - Disconnect click in web browser from actual command execution
                    23 Aug 2006 - mcdurdin - Fix hotkeys
                    14 Sep 2006 - mcdurdin - Add path information to New File dialog
                    28 Sep 2006 - mcdurdin - Add theming
                    04 Dec 2006 - mcdurdin - Support right-click options on MRU files
                    04 Jan 2007 - mcdurdin - Add help support, links to checkforupdates and upgrade
                    05 Jan 2007 - mcdurdin - Refresh after activating modules
                    30 Jan 2007 - mcdurdin - F1 - help
                    19 Mar 2007 - mcdurdin - I733 - Add Compile to Web button
                    30 May 2007 - mcdurdin - I762 - Fixed crash when refreshing project (rare)
                    30 May 2007 - mcdurdin - I727 - Stop dragging files into project,help panes
                    13 Jul 2007 - mcdurdin - I939 - Report script errors to website
                    23 Aug 2007 - mcdurdin - I927 - external editor support
                    19 Nov 2007 - mcdurdin - I1157 - const string parameters
                    14 Jun 2008 - mcdurdin - I1420 - Improve project performance
                    20 Jul 2008 - mcdurdin - I1553 - External exceptions
                    29 Mar 2010 - mcdurdin - I2199 - Shift+Click opens new window
                    26 Jul 2010 - mcdurdin - I2468 - Eliminate KeymanWeb Pack
                    17 Dec 2010 - mcdurdin - I2570 - Use new E-mbeddedWB (see also I2595)
                    18 Feb 2011 - mcdurdin - I2721 - Override Javascript-disabled security for web controls
                    03 May 2011 - mcdurdin - I2890 - Record diagnostic data when encountering registry errors
                    18 May 2012 - mcdurdin - I3306 - V9.0 - Remove TntControls + Win9x support
                    18 May 2012 - mcdurdin - I3309 - V9.0 - Migrate to Delphi XE2, VS2010, svn 1.7
                    08 Jun 2012 - mcdurdin - I3349 - V9.0 - Consolidate all process creation into TUtilExecute
                    04 May 2015 - mcdurdin - I4687 - V9.0 - Split project UI actions into separate classes
                    04 May 2015 - mcdurdin - I4686 - V9.0 - Refactor compile into project file action
                    04 May 2015 - mcdurdin - I4692 - V9.0 - Add Clean as project action
                    06 Jun 2015 - mcdurdin - I4734 - Project files need to build package installers
                    06 Jun 2015 - mcdurdin - I4735 - Add clean and build single file commands to project
                    09 Aug 2015 - mcdurdin - I2986 - Ctrl+N, Ctrl+O not working in Developer Project view
*)
unit Keyman.Developer.UI.Project.UfrmProject;  // I3306  // I3309

interface

uses
  System.Contnrs,
  System.Types,
  System.UITypes,
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, Menus, UfrmMDIEditor, UfrmMDIChild, Keyman.Developer.System.Project.ProjectFile,
  KeymanDeveloperUtils, UserMessages,
  Keyman.UI.UframeCEFHost;

type
  TfrmProject = class(TfrmTikeChild)  // I2721
    dlgOpenFile: TOpenDialog;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormActivate(Sender: TObject);
  private
    FShouldRefresh: Boolean;
    FNextCommand: WideString;
    FNextCommandParams: TStringList;
    cef: TframeCEFHost;

    procedure cefLoadEnd(Sender: TObject);
    procedure cefBeforeBrowse(Sender: TObject; const Url: string; isPopup, wasHandled: Boolean);
    procedure cefBeforeBrowseSync(Sender: TObject; const Url: string; isPopup: Boolean; out Handled: Boolean);
    procedure cefCommand(Sender: TObject; const command: string; params: TStringList);

    procedure ProjectRefresh(Sender: TObject);
    procedure ProjectRefreshCaption(Sender: TObject);
    procedure RefreshCaption;
    procedure WebCommand(Command: WideString; Params: TStringList);
    procedure RefreshHTML;
    procedure EditFileExternal(FileName: WideString);
    procedure ClearMessages;
    function ShouldHandleNavigation(URL: string): Boolean;
    procedure WebCommandProject(Command: WideString; Params: TStringList);
    procedure WebCommandWelcome(Command: WideString; Params: TStringList);
  protected
    function GetHelpTopic: string; override;
  public
    procedure SetFocus; override;
    procedure SetGlobalProject;
    procedure StartClose; override;
    procedure CompileAll;
    procedure RefreshOptions; override;
  end;

implementation

uses
  System.StrUtils,
  Winapi.ShellApi,

  Keyman.Developer.System.HelpTopics,

  dmActionsMain,
  KeymanDeveloperOptions,
  Keyman.Developer.System.Project.kmnProjectFile,
  Keyman.Developer.System.Project.kpsProjectFile,
  Keyman.Developer.System.Project.modelTsProjectFile,
  Keyman.Developer.System.Project.xmlLdmlProjectFile,
  Keyman.Developer.System.Project.Project,
  Keyman.Developer.UI.Project.ProjectUI,
  Keyman.Developer.UI.Project.ProjectFileUI,
  keyman.Developer.UI.Project.UpgradeProject,
  Keyman.Developer.System.Project.ProjectFileType,
  typinfo,
  ErrorControlledRegistry,
  UfrmEditor,
  UfrmMain,
  UfrmMessages,
  UfrmNewFileDetails,
  utilfiletypes,
  utilhttp,
  utilsystem,
  utilexecute,
  utilxml,
  mrulist,
  UmodWebHttpServer;

{$R *.DFM}

{ TfrmProject }

// Destruction steps
// =================
// 1. The FormCloseQuery event sets CanClose to False and calls TChromiumWindow.CloseBrowser, which triggers the TChromiumWindow.OnClose event.
// 2. The TChromiumWindow.OnClose event calls TChromiumWindow.DestroyChildWindow which triggers the TChromiumWindow.OnBeforeClose event.
// 3. TChromiumWindow.OnBeforeClose sets FCanClose to True and closes the form.

procedure TfrmProject.EditFileExternal(FileName: WideString);
begin
  if FKeymanDeveloperOptions.ExternalEditorPath = '' then
  begin
    if MessageDlg('You have not configured an external editor.  Do you want to configure one now?', mtConfirmation, mbOkCancel, 0) = mrCancel then Exit;
    modActionsMain.actToolsOptions.Execute;
    if FKeymanDeveloperOptions.ExternalEditorPath = '' then
    begin
      ShowMessage('The external editor has not been configured.');
      Exit;
    end;
  end;
  if not TUtilExecute.Shell(Handle, FKeymanDeveloperOptions.ExternalEditorPath, ExtractFileDir(FileName), '"'+FileName+'"') then  // I3349
  begin
    ShowMessage('Keyman Developer could not start the external editor '+IntToStr(GetLastError)+': '+SysErrorMessage(GetLastError));
  end;
end;

procedure TfrmProject.FormActivate(Sender: TObject);
begin
  inherited;

  if FShouldRefresh then
    RefreshHTML;
  FShouldRefresh := False;
end;

procedure TfrmProject.StartClose;
begin
  Visible := False;
  cef.StartClose;
end;

procedure TfrmProject.FormCreate(Sender: TObject);
begin
  inherited;

  FNextCommandParams := TStringList.Create;

  FGlobalProjectRefresh := ProjectRefresh;
  FGlobalProjectRefreshCaption := ProjectRefreshCaption;

  cef := TframeCEFHost.Create(Self);
  cef.Parent := Self;
  cef.Visible := True;
  cef.OnBeforeBrowse := cefBeforeBrowse;
  cef.OnBeforeBrowseSync := cefBeforeBrowseSync;
  cef.OnCommand := cefCommand;
  cef.OnLoadEnd := cefLoadEnd;
  RefreshHTML;
end;

procedure TfrmProject.RefreshHTML;
begin
  if IsGlobalProjectUIReady then
  begin
    cef.Navigate(modWebHttpServer.GetAppURL('project/?path='+URLEncode(GetGlobalProjectUI.FileName)));
  end
  else
    cef.Navigate(modWebHttpServer.GetAppURL('project/welcome'));
  RefreshCaption;
end;

procedure TfrmProject.RefreshOptions;
begin
  inherited;
  ProjectRefresh(nil);
end;

procedure TfrmProject.ProjectRefresh(Sender: TObject);
begin
  if frmKeymanDeveloper.ActiveChild <> Self
    then FShouldRefresh := True
    else RefreshHTML;
end;

procedure TfrmProject.ProjectRefreshCaption(Sender: TObject);
begin
  RefreshCaption;
end;

procedure TfrmProject.RefreshCaption;
var
  s: string;
begin
  if IsGlobalProjectUIReady then
  begin
    Hint := FGlobalProject.FileName;
    s := GetGlobalProjectUI.DisplayFileName;   // I4687
  end
  else
  begin
    Hint := 'Welcome';
    s := 'Welcome';
  end;
  Caption := s;
end;

procedure TfrmProject.SetFocus;
begin
  inherited;
  cef.SetFocus;
end;

procedure TfrmProject.SetGlobalProject;
begin
  ProjectRefresh(nil);
end;

procedure TfrmProject.FormDestroy(Sender: TObject);
begin
  inherited;

  FGlobalProjectRefresh := nil;
  FGlobalProjectRefreshCaption := nil;

  FreeAndNil(FNextCommandParams);
end;

procedure TfrmProject.cefBeforeBrowse(Sender: TObject; const Url: string; isPopup, wasHandled: Boolean);
var
  s: string;
begin
  if Copy(URL, 1, 5) = 'help:' then
  begin
    s := URL;
    Delete(s,1,5);
    FNextCommand := LowerCase(s);
    frmKeymanDeveloper.HelpTopic(LowerCase(s));
  end
  else if not URL.StartsWith(modWebHttpServer.GetLocalhostURL) and (Copy(URL, 1, 4) = 'http') then
  begin
    FNextCommand := URL;
    TUtilExecute.URL(URL);
  end;
end;

procedure TfrmProject.cefBeforeBrowseSync(Sender: TObject;
  const Url: string; isPopup: Boolean; out Handled: Boolean);
begin
  Handled := ShouldHandleNavigation(Url);
end;

procedure TfrmProject.cefCommand(Sender: TObject; const command: string; params: TStringList);
begin
  WebCommand(LowerCase(command), params);
end;

procedure TfrmProject.CompileAll;
var
  i: Integer;
begin
  ClearMessages;
  for i := 0 to FGlobalProject.Files.Count - 1 do
  begin
    if not FGlobalProject.Files[i].IsSourceFile then
      Continue;
    if FGlobalProject.Files[i].IsCompilable and
      not (FGlobalProject.Files[i] is TkpsProjectFile) then
    begin
      if not (FGlobalProject.Files[i].UI as TProjectFileUI).DoAction(pfaCompile, False) then Exit;   // I4687
    end;
  end;
  for i := 0 to FGlobalProject.Files.Count - 1 do
  begin
    if not FGlobalProject.Files[i].IsSourceFile then
      Continue;
    if FGlobalProject.Files[i] is TkpsProjectFile then
      if not (FGlobalProject.Files[i].UI as TProjectFileUI).DoAction(pfaCompile, False) then Exit;   // I4687
  end;
end;

procedure TfrmProject.ClearMessages;
begin
  frmMessages.Clear;
end;

function TfrmProject.ShouldHandleNavigation(URL: string): Boolean;
begin
  Result := False;
  if Copy(URL, 1, 5) = 'help:' then
  begin
    Result := True;
  end
  else if not URL.StartsWith(modWebHttpServer.GetLocalhostURL) and (Copy(URL, 1, 4) = 'http') then
  begin
    Result := True;
  end
end;

procedure TfrmProject.WebCommand(Command: WideString; Params: TStringList);
begin
  if not IsGlobalProjectUIReady
    then WebCommandWelcome(Command, Params)
    else WebCommandProject(Command, Params);
end;

procedure TfrmProject.WebCommandWelcome(Command: WideString; Params: TStringList);
    function SelectedMRUFileName: WideString;
    var
      n: Integer;
    begin
      Result := '';
      if Params.Values['id'].StartsWith('id_MRU') then
      begin
        if TryStrToInt(Params.Values['id'].Substring(6), n) and
            (n >= 0) and
            (n < frmKeymanDeveloper.ProjectMRU.FileCount) then
          Result := frmKeymanDeveloper.ProjectMRU.Files[n];
      end
    end;
begin
  if Command = 'newproject' then
    modActionsMain.actProjectNew.Execute
  else if Command = 'openproject' then
    modActionsMain.actProjectOpen.Execute
  else if Command = 'editfile' then // MRU
  begin
    if SelectedMRUFileName <> '' then
      frmKeymanDeveloper.OpenProject(SelectedMRUFileName);
  end
  else if Command = 'removefrommru' then
  begin
    if SelectedMRUFileName <> '' then
    begin
      frmKeymanDeveloper.ProjectMRU.Delete(SelectedMRUFileName);
      RefreshHTML;
    end;
  end
end;

procedure TfrmProject.WebCommandProject(Command: WideString; Params: TStringList);
    function FileTypeFromParamType: TKMFileType;
    begin
      if Params.Values['type'] = 'keyboard' then Result := ftKeymanSource
      else if Params.Values['type'] = 'package' then Result := ftPackageSource
      else if Params.Values['type'] = 'model' then Result := ftModelSource
      else Result := ftOther;
    end;

    function SelectedProjectFile: TProjectFile;
    var
      i: Integer;
    begin
      i := FGlobalProject.Files.IndexOfID(Params.Values['id']);
      if i < 0
        then Result := nil
        else Result := FGlobalProject.Files[i];
    end;

    function SelectedMRUFileName: WideString;
    var
      n: Integer;
    begin
      Result := '';
      if Copy(Params.Values['id'], 1, 6) = 'id_MRU' then
      begin
        n := StrToIntDef(Copy(Params.Values['id'], 7, 10), -1);
        if (n >= 0) and (n < FGlobalProject.MRU.FileCount) then
          Result := FGlobalProject.MRU.Files[n];
      end
    end;
var
  pf: TProjectFile;
  FFileType: TKMFileType;
  FDefaultExtension: string;
  i: Integer;
  frmNewFileDetails: TfrmNewFileDetails;
begin
  pf := nil;


  if Command = 'fileaddnew' then
  begin
    { create a new file, add it to the project }

    frmNewFileDetails := TfrmNewFileDetails.Create(Self);
    try
      frmNewFileDetails.BaseFileName := FGlobalProject.ResolveSourcePath;
      frmNewFileDetails.FileType := FileTypeFromParamType;
      frmNewFileDetails.CanChangePath := FGlobalProject.Options.Version = pv10;

      if frmNewFileDetails.ShowModal = mrOk then
      begin
        pf := CreateProjectFile(FGlobalProject, frmNewFileDetails.FileName, nil);
      end;
    finally
      frmNewFileDetails.Free;
    end;
    if Assigned(pf) then (pf.UI as TProjectFileUI).NewFile;   // I4687
  end
  else if Command = 'fileaddexisting' then
  begin
    { locate an existing file, add it to the project }
    Assert(FGlobalProject.Options.Version = pv10);
    FFileType := FileTypeFromParamType;

    dlgOpenFile.Filter := GetFileTypeFilter(FFileType, FDefaultExtension);
    dlgOpenFile.DefaultExt := FDefaultExtension;
    if dlgOpenFile.Execute then
    begin
      CreateProjectFile(FGlobalProject, dlgOpenFile.FileName, nil);
    end;
  end
  else if (Command = 'editfile') or (Command = 'openfile') then
  begin
    pf := SelectedProjectFile;
    if Assigned(pf) then (pf.UI as TProjectFileUI).DefaultEvent(Self)   // I4687
    else if SelectedMRUFileName <> '' then
      frmKeymanDeveloper.OpenFilesInProject([SelectedMRUFileName])
    else if Params.Values['name'] <> '' then
      frmKeymanDeveloper.OpenFilesInProject([Params.Values['name']]);
  end
  else if Command = 'viewfilesource' then
  begin
    pf := SelectedProjectFile;
    if Assigned(pf) then
      frmKeymanDeveloper.OpenEditor(pf.FileName, TfrmEditor)
    else if SelectedMRUFileName <> '' then
      frmKeymanDeveloper.OpenEditor(SelectedMRUFileName, TfrmEditor);
  end
  else if Command = 'editfileexternal' then
  begin
    pf := SelectedProjectFile;
    if Assigned(pf) then
      EditFileExternal(pf.FileName)
    else if SelectedMRUFileName <> '' then
      EditFileExternal(SelectedMRUFileName);
  end
  else if Command = 'opencontainingfolder' then
  begin
    pf := SelectedProjectFile;
    if Assigned(pf) then
      OpenContainingFolder(pf.FileName)
    else if SelectedMRUFileName <> '' then
      OpenContainingFolder(SelectedMRUFileName);
  end
  else if Command = 'openbuildfolder' then
  begin
    pf := SelectedProjectFile;
    if Assigned(pf) and (pf is TkmnProjectFile) then
      OpenContainingFolder((pf as TkmnProjectFile).TargetFileName)
    else if Assigned(pf) and (pf is TxmlLdmlProjectFile) then
      OpenContainingFolder((pf as TxmlLdmlProjectFile).TargetFileName)
    else if Assigned(pf) and (pf is TkpsProjectFile) then
      OpenContainingFolder((pf as TkpsProjectFile).TargetFileName)
    else if Assigned(pf) and (pf is TmodelTsProjectFile) then
      OpenContainingFolder((pf as TmodelTsProjectFile).TargetFileName);
  end
  else if Command = 'openprojectfolder' then
  begin
    OpenContainingFolder(FGlobalProject.FileName);
  end
  else if Command = 'removefile' then
  begin
    Assert(FGlobalProject.Options.Version = pv10);
    pf := SelectedProjectFile;
    if Assigned(pf) then
    begin
      if MessageDlg('Remove file '+pf.FileName+' from project?  The file will not be deleted from the disk.',
        mtConfirmation, mbOkCancel, 0) = mrOk then
      begin
        FGlobalProject.Files.Remove(pf);
      end;
    end;
  end
  else if Command = 'removefrommru' then
  begin
    if SelectedMRUFileName <> '' then
      FGlobalProject.MRU.Delete(SelectedMRUFileName);
  end
  else if Command = 'compileall' then   // I4686
  begin
    CompileAll;
  end
  else if Command = 'cleanall' then   // I4692
  begin
    ClearMessages;
    for i := 0 to FGlobalProject.Files.Count - 1 do
    begin
      if not FGlobalProject.Files[i].IsSourceFile then
        Continue;
      (FGlobalProject.Files[i].UI as TProjectFileUI).DoAction(pfaClean, False);   // I4687
    end;
  end
  else if Command = 'keyboard_compileall' then
  begin
    ClearMessages;
    for i := 0 to FGlobalProject.Files.Count - 1 do
    begin
      if not FGlobalProject.Files[i].IsSourceFile then
        Continue;

      if (FGlobalProject.Files[i] is TkmnProjectFile) or
          (FGlobalProject.Files[i] is TxmlLdmlProjectFile) then
        (FGlobalProject.Files[i].UI as TProjectFileUI).DoAction(pfaCompile, False);   // I4687
    end;
  end
  else if Command = 'keyboard_cleanall' then   // I4692
  begin
    ClearMessages;
    for i := 0 to FGlobalProject.Files.Count - 1 do
    begin
      if not FGlobalProject.Files[i].IsSourceFile then
        Continue;
      if (FGlobalProject.Files[i] is TkmnProjectFile) or
          (FGlobalProject.Files[i] is TxmlLdmlProjectFile) then
        (FGlobalProject.Files[i].UI as TProjectFileUI).DoAction(pfaClean, False);
    end;
  end
  else if Command = 'model_compileall' then
  begin
    ClearMessages;
    for i := 0 to FGlobalProject.Files.Count - 1 do
    begin
      if not FGlobalProject.Files[i].IsSourceFile then
        Continue;
      if FGlobalProject.Files[i] is TmodelTsProjectFile then
        (FGlobalProject.Files[i].UI as TProjectFileUI).DoAction(pfaCompile, False);   // I4687
    end;
  end
  else if Command = 'model_cleanall' then   // I4692
  begin
    ClearMessages;
    for i := 0 to FGlobalProject.Files.Count - 1 do
    begin
      if not FGlobalProject.Files[i].IsSourceFile then
        Continue;
      if FGlobalProject.Files[i] is TmodelTsProjectFile then
        (FGlobalProject.Files[i].UI as TProjectFileUI).DoAction(pfaClean, False);
    end;
  end

  else if Command = 'package_compileall' then
  begin
    ClearMessages;
    for i := 0 to FGlobalProject.Files.Count - 1 do
    begin
      if not FGlobalProject.Files[i].IsSourceFile then
        Continue;
      if FGlobalProject.Files[i] is TkpsProjectFile then
        (FGlobalProject.Files[i].UI as TProjectFileUI).DoAction(pfaCompile, False);
    end;
  end
  else if Command = 'package_cleanall' then   // I4692
  begin
    ClearMessages;
    for i := 0 to FGlobalProject.Files.Count - 1 do
    begin
      if not FGlobalProject.Files[i].IsSourceFile then
        Continue;
      if FGlobalProject.Files[i] is TkpsProjectFile then
        (FGlobalProject.Files[i].UI as TProjectFileUI).DoAction(pfaClean, False);
    end;
  end
  else if Command = 'cleanfile' then   // I4735
  begin
    ClearMessages;
    pf := SelectedProjectFile;
    if Assigned(pf) then (pf.UI as TProjectFileUI).DoAction(pfaClean, False);
  end
  else if Command = 'compilefile' then
  begin
    ClearMessages;
    pf := SelectedProjectFile;
    if Assigned(pf) then (pf.UI as TProjectFileUI).DoAction(pfaCompile, False);
  end
  else if Command = 'upgradeproject' then
  begin
    TryUpgradeProject(FGlobalProject);
    ProjectRefresh(nil);
  end
  else if Command = 'checkforupdates' then
  begin
    modActionsMain.actHelpCheckForUpdates.Execute;
  end;
end;

{procedure TfrmProject.DoRefresh;
begin
  ProjectRefresh(nil);
end;}

procedure TfrmProject.cefLoadEnd(Sender: TObject);
begin
  if csDestroying in ComponentState then
    Exit;

  if (frmKeymanDeveloper.ActiveChild = Self) and (Screen.ActiveForm = frmKeymanDeveloper) and
    (GetWindowThreadProcessId(GetForegroundWindow, nil) = GetCurrentThreadId) then
  begin
    cef.SetFocus;
  end;
end;

//TODO: support dropping files
//  DropTarget := frmKeymanDeveloper.DropTargetIntf;

function TfrmProject.GetHelpTopic: string;
begin
  Result := SHelpTopic_Context_Project;
end;

end.
