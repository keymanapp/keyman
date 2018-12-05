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
                    17 Dec 2010 - mcdurdin - I2570 - Use new EmbeddedWB (see also I2595)
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
  StdCtrls, ExtCtrls, Menus, UfrmMDIEditor, UfrmMDIChild, Keyman.Developer.System.Project.ProjectFile,
  KeymanDeveloperUtils, UserMessages,
  Keyman.Developer.UI.UframeCEFHost;

type
  TfrmProject = class(TfrmTikeChild)  // I2721
    dlgOpenFile: TOpenDialog;
    tmrRefresh: TTimer;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure tmrRefreshTimer(Sender: TObject);
    procedure FormActivate(Sender: TObject);
  private
    FShouldRefresh: Boolean;
    FNextCommand: WideString;
    FNextCommandParams: TStringList;
    cef: TframeCEFHost;

    procedure cefLoadEnd(Sender: TObject);
    procedure cefBeforeBrowse(Sender: TObject; const Url: string; params: TStringList; wasHandled: Boolean);
    procedure cefBeforeBrowseSync(Sender: TObject; const Url: string; out Handled: Boolean);

    procedure ProjectRefresh(Sender: TObject);
    procedure ProjectRefreshCaption(Sender: TObject);
    procedure RefreshCaption;
    procedure WebCommand(Command: WideString; Params: TStringList);
    procedure RefreshHTML;
    procedure EditFileExternal(FileName: WideString);
    function DoNavigate(URL: string): Boolean;
    procedure ClearMessages;
    function ShouldHandleNavigation(URL: string): Boolean;
  protected
    function GetHelpTopic: string; override;
  public
    procedure SetFocus; override;
    procedure SetGlobalProject;
    procedure StartClose; override;
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
  Keyman.Developer.System.Project.Project,
  Keyman.Developer.UI.Project.ProjectUI,
  Keyman.Developer.UI.Project.ProjectFileUI,
  Keyman.Developer.System.Project.ProjectFileType,
  typinfo,
  ErrorControlledRegistry,
  UfrmEditor,
  ExternalExceptionHandler,
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
  GetGlobalProjectUI.OnRefresh := ProjectRefresh;   // I4687
  GetGlobalProjectUI.OnRefreshCaption := ProjectRefreshCaption;   // I4687

  cef := TframeCEFHost.Create(Self);
  cef.Parent := Self;
  cef.Visible := True;
  cef.OnBeforeBrowse := cefBeforeBrowse;
  cef.OnBeforeBrowseSync := cefBeforeBrowseSync;
  cef.OnLoadEnd := cefLoadEnd;
  RefreshHTML;
end;

procedure TfrmProject.RefreshHTML;
begin
  if GetGlobalProjectUI.Refreshing then   // I4687
    tmrRefresh.Enabled := True
  else
  begin
    cef.Navigate(modWebHttpServer.GetAppURL('project/?path='+URLEncode(GetGlobalProjectUI.FileName)));
  end;
  RefreshCaption;
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
  Hint := FGlobalProject.FileName;
  s := GetGlobalProjectUI.DisplayFileName;   // I4687
  Caption := s;
end;

procedure TfrmProject.SetFocus;
begin
  inherited;
  cef.SetFocus;
end;

procedure TfrmProject.SetGlobalProject;
begin
  if not Assigned(GetGlobalProjectUI.OnRefresh) then   // I4687
  begin
    GetGlobalProjectUI.OnRefresh := ProjectRefresh;   // I4687
    GetGlobalProjectUI.OnRefreshCaption := ProjectRefreshCaption;   // I4687
    ProjectRefresh(nil);
  end
  else if not GetGlobalProjectUI.Refreshing then   // I4687
    ProjectRefresh(nil);
end;

procedure TfrmProject.FormDestroy(Sender: TObject);
begin
  inherited;
  if GetGlobalProjectUI <> nil then
  begin
    GetGlobalProjectUI.OnRefresh := nil;   // I4687
    GetGlobalProjectUI.OnRefreshCaption := nil;   // I4687
  end;
  FreeAndNil(FNextCommandParams);
end;

procedure TfrmProject.cefBeforeBrowse(Sender: TObject; const Url: string; params: TStringList; wasHandled: Boolean);
begin
  DoNavigate(Url);
end;

procedure TfrmProject.cefBeforeBrowseSync(Sender: TObject;
  const Url: string; out Handled: Boolean);
begin
  Handled := ShouldHandleNavigation(Url);
end;

procedure TfrmProject.ClearMessages;
begin
  frmMessages.Clear;
end;

function TfrmProject.ShouldHandleNavigation(URL: string): Boolean;
begin
  Result := False;
  if Copy(URL, 1, 7) = 'keyman:' then
  begin
    Result := True;
  end
  else if Copy(URL, 1, 5) = 'help:' then
  begin
    Result := True;
  end
  else if not URL.StartsWith(modWebHttpServer.GetLocalhostURL) and (Copy(URL, 1, 4) = 'http') then
  begin
    Result := True;
  end
end;

function TfrmProject.DoNavigate(URL: string): Boolean;
var
  n: Integer;
  s, command: WideString;
  FCommandParams: TStringList;
begin
  Result := False;
  if Copy(URL, 1, 7) = 'keyman:' then
  begin
    s := URL;

    FCommandParams := TStringList.Create;
    try
      Delete(s,1,7);
      n := Pos('?',s);
      if n > 0 then
      begin
        command := Copy(s,1,n-1);
        DecodeAndSetParams(Copy(s,n+1,Length(s)), FCommandParams);
      end
      else
        command := s;

      WebCommand(LowerCase(Command), FCommandParams);
    finally
      FCommandParams.Free;
    end;
    Result := True;
  end
  else if Copy(URL, 1, 5) = 'help:' then
  begin
    Result := True;
    s := URL;
    Delete(s,1,5);
    FNextCommand := LowerCase(s);
    frmKeymanDeveloper.HelpTopic(LowerCase(s));
  end
  else if not URL.StartsWith(modWebHttpServer.GetLocalhostURL) and (Copy(URL, 1, 4) = 'http') then
  begin
    Result := True;
    FNextCommand := URL;
    TUtilExecute.URL(URL);
  end
end;

procedure TfrmProject.WebCommand(Command: WideString; Params: TStringList);
    function FileTypeFromParamType: TKMFileType;
    begin
      if Params.Values['type'] = 'keyboard' then Result := ftKeymanSource
      else if Params.Values['type'] = 'package' then Result := ftPackageSource
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
begin
  pf := nil;

  if Command = 'fileaddnew' then
  begin
    { create a new file, add it to the project }
    with TfrmNewFileDetails.Create(Self) do
    try
      BaseFileName := FGlobalProject.FileName;
      FileType := FileTypeFromParamType;
      if FileType = ftOther then Exit;

      if ShowModal = mrOk then
      begin
        pf := CreateProjectFile(FGlobalProject, FileName, nil);
      end;
    finally
      Free;
    end;
    if Assigned(pf) then (pf.UI as TProjectFileUI).NewFile;   // I4687
  end
  else if Command = 'fileaddexisting' then
  begin
    { locate an existing file, add it to the project }
    FFileType := FileTypeFromParamType;
    if FFileType = ftOther then Exit;

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
      frmKeymanDeveloper.OpenFile(SelectedMRUFileName, True)
    else if Params.Values['name'] <> '' then
      frmKeymanDeveloper.OpenFile(Params.Values['name'], True);
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
  else if Command = 'removefile' then
  begin
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
    ClearMessages;
    for i := 0 to FGlobalProject.Files.Count - 1 do
    begin
      if FGlobalProject.Files[i] is TkmnProjectFile then
        if not (FGlobalProject.Files[i].UI as TProjectFileUI).DoAction(pfaCompile, False) then Exit;   // I4687
    end;
    for i := 0 to FGlobalProject.Files.Count - 1 do
    begin
      if FGlobalProject.Files[i] is TkpsProjectFile then
        if not (FGlobalProject.Files[i].UI as TProjectFileUI).DoAction(pfaCompile, False) then Exit;   // I4687
    end;
  end
  else if Command = 'cleanall' then   // I4692
  begin
    ClearMessages;
    for i := 0 to FGlobalProject.Files.Count - 1 do
    begin
      (FGlobalProject.Files[i].UI as TProjectFileUI).DoAction(pfaClean, False);   // I4687
    end;
  end
  else if Command = 'keyboard_compileall' then
  begin
    ClearMessages;
    for i := 0 to FGlobalProject.Files.Count - 1 do
    begin
      if FGlobalProject.Files[i] is TkmnProjectFile then
        (FGlobalProject.Files[i].UI as TProjectFileUI).DoAction(pfaCompile, False);   // I4687
    end;
  end
  else if Command = 'keyboard_cleanall' then   // I4692
  begin
    ClearMessages;
    for i := 0 to FGlobalProject.Files.Count - 1 do
    begin
      if FGlobalProject.Files[i] is TkmnProjectFile then
        (FGlobalProject.Files[i].UI as TProjectFileUI).DoAction(pfaClean, False);
    end;
  end
  else if Command = 'package_compileall' then
  begin
    ClearMessages;
    for i := 0 to FGlobalProject.Files.Count - 1 do
    begin
      if FGlobalProject.Files[i] is TkpsProjectFile then
        (FGlobalProject.Files[i].UI as TProjectFileUI).DoAction(pfaCompile, False);
    end;
  end
  else if Command = 'package_compileallinstallers' then   // I4734
  begin
    ClearMessages;
    for i := 0 to FGlobalProject.Files.Count - 1 do
    begin
      if FGlobalProject.Files[i] is TkpsProjectFile then
        (FGlobalProject.Files[i].UI as TProjectFileUI).DoAction(pfaCompileInstaller, False);
    end;
  end
  else if Command = 'package_compileinstaller' then
  begin
    ClearMessages;
    pf := SelectedProjectFile;
    if Assigned(pf) then (pf.UI as TProjectFileUI).DoAction(pfaCompileInstaller, False);
  end
  else if Command = 'package_cleanall' then   // I4692
  begin
    ClearMessages;
    for i := 0 to FGlobalProject.Files.Count - 1 do
    begin
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
  else if Command = 'checkforupdates' then
  begin
    modActionsMain.actHelpCheckForUpdates.Execute;
  end;
end;

{procedure TfrmProject.DoRefresh;
begin
  ProjectRefresh(nil);
end;}

procedure TfrmProject.tmrRefreshTimer(Sender: TObject);
begin
  tmrRefresh.Enabled := False;
  ProjectRefresh(nil);
end;

procedure TfrmProject.cefLoadEnd(Sender: TObject);
begin
  if csDestroying in ComponentState then
    Exit;
  if Assigned(FGlobalProject) then GetGlobalProjectUI.Refreshing := False;   // I4687
  if (frmKeymanDeveloper.ActiveChild = Self) and (Screen.ActiveForm = frmKeymanDeveloper) then
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
