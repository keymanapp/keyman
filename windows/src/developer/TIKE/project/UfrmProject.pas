(*
  Name:             UfrmProject
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
unit UfrmProject;  // I3306  // I3309

interface

uses
  System.Contnrs,
  System.Types,
  System.UITypes,
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, ExtCtrls, Menus, UfrmMDIEditor, UfrmMDIChild, ProjectFile,
  KeymanDeveloperUtils, UserMessages,
  uCEFInterfaces, uCEFWindowParent, uCEFChromiumWindow, uCEFTypes;

type
  TfrmProject = class(TfrmTikeChild)  // I2721
    dlgOpenFile: TOpenDialog;
    tmrRefresh: TTimer;
    cef: TChromiumWindow;
    tmrCreateBrowser: TTimer;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure tmrRefreshTimer(Sender: TObject);
    procedure FormActivate(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure tmrCreateBrowserTimer(Sender: TObject);
    procedure cefClose(Sender: TObject);
    procedure cefBeforeClose(Sender: TObject);
    procedure cefAfterCreated(Sender: TObject);   // I2986
  private
    FShouldRefresh: Boolean;
    FNextCommand: WideString;
    FNextCommandParams: TStringList;

    // CEF: You have to handle this two messages to call NotifyMoveOrResizeStarted or some page elements will be misaligned.
    procedure WMMove(var aMessage : TWMMove); message WM_MOVE;
    procedure WMMoving(var aMessage : TMessage); message WM_MOVING;
    // CEF: You also have to handle these two messages to set GlobalCEFApp.OsmodalLoop
    procedure WMEnterMenuLoop(var aMessage: TMessage); message WM_ENTERMENULOOP;
    procedure WMExitMenuLoop(var aMessage: TMessage); message WM_EXITMENULOOP;

    procedure cefLoadEnd(Sender: TObject; const browser: ICefBrowser;
                         const frame: ICefFrame; httpStatusCode: Integer);
    procedure cefBeforeBrowse(Sender: TObject; const browser: ICefBrowser;
                              const frame: ICefFrame; const request: ICefRequest; user_gesture,
                              isRedirect: Boolean; out Result: Boolean);
    procedure cefPreKeyEvent(Sender: TObject; const browser: ICefBrowser;
                             const event: PCefKeyEvent; osEvent: TCefEventHandle;
                             out isKeyboardShortcut: Boolean; out Result: Boolean);
    procedure cefConsoleMessage(Sender: TObject; const browser: ICefBrowser;
                                level: TCefLogSeverity; const message, source: ustring;
                                line: Integer; out Result: Boolean);
    procedure cefRunContextMenu(Sender: TObject; const browser: ICefBrowser;
                                const frame: ICefFrame; const params: ICefContextMenuParams;
                                const model: ICefMenuModel;
                                const callback: ICefRunContextMenuCallback;
                                var aResult : Boolean);
    procedure cefBeforePopup(Sender: TObject;
                             const browser: ICefBrowser;
                             const frame: ICefFrame;
                             const targetUrl,
                             targetFrameName: ustring;
                             targetDisposition: TCefWindowOpenDisposition;
                             userGesture: Boolean;
                             const popupFeatures: TCefPopupFeatures;
                             var windowInfo: TCefWindowInfo;
                             var client: ICefClient;
                             var settings: TCefBrowserSettings;
                             var noJavascriptAccess: Boolean;
                             var Result: Boolean);

    procedure ProjectRefresh(Sender: TObject);
    procedure ProjectRefreshCaption(Sender: TObject);
    procedure RefreshCaption;
    procedure WebCommand(Command: WideString; Params: TStringList);
    procedure SaveCurrentTab;
    procedure RefreshHTML(RefreshState: Boolean);
    procedure WMUserWebCommand(var Message: TMessage); message WM_USER_WEBCOMMAND;
    procedure EditFileExternal(FileName: WideString);
    function DoNavigate(URL: string): Boolean;
    procedure ClearMessages;
    procedure CreateBrowser;
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
  kmnProjectFile,
  kpsProjectFile,
  Project,
  ProjectUI,
  ProjectFileUI,
  ProjectFileType,
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
  UmodWebHttpServer,
  uCEFApplication;

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
  if FShouldRefresh then RefreshHTML(True);
  FShouldRefresh := False;
end;

procedure TfrmProject.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
begin
  inherited;
  if CanClose then
    SaveCurrentTab;
end;

procedure TfrmProject.StartClose;
begin
  Visible := False;
  cef.CloseBrowser(True);
end;

procedure TfrmProject.FormCreate(Sender: TObject);
begin
  inherited;

  FNextCommandParams := TStringList.Create;
  GetGlobalProjectUI.OnRefresh := ProjectRefresh;   // I4687
  GetGlobalProjectUI.OnRefreshCaption := ProjectRefreshCaption;   // I4687

  cef.ChromiumBrowser.OnLoadEnd := cefLoadEnd;
  cef.ChromiumBrowser.OnBeforeBrowse := cefBeforeBrowse;
  cef.ChromiumBrowser.OnPreKeyEvent := cefPreKeyEvent;
  cef.ChromiumBrowser.OnConsoleMessage := cefConsoleMessage;
  cef.ChromiumBrowser.OnRunContextMenu := cefRunContextMenu;
  cef.ChromiumBrowser.OnBeforePopup := cefBeforePopup;
  CreateBrowser;
end;

procedure TfrmProject.CreateBrowser;
begin
  tmrCreateBrowser.Enabled := not cef.CreateBrowser;
end;

procedure TfrmProject.RefreshHTML(RefreshState: Boolean);
begin
  if not cef.Initialized then
  begin
    // After initialization, refresh will happen
    // See cefAfterCreated
    Exit;
  end;

  if GetGlobalProjectUI.Refreshing then   // I4687
    tmrRefresh.Enabled := True
  else
  begin
//    GetGlobalProjectUI.Refreshing := True;   // I4687
    if RefreshState then SaveCurrentTab;
    cef.LoadURL(modWebHttpServer.GetLocalhostURL + '/app/project/?path='+URLEncode(GetGlobalProjectUI.FileName));
  end;
  RefreshCaption;
end;

procedure TfrmProject.ProjectRefresh(Sender: TObject);
begin
  if frmKeymanDeveloper.ActiveChild <> Self
    then FShouldRefresh := True
    else RefreshHTML(True);
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

procedure TfrmProject.SaveCurrentTab;
(*var
  elem: IHTMLElement;
begin
  try
  //TODO use http server backend
    if Assigned(web.Document) then
    begin
      elem:= (web.Document as IHTMLDocument3).getElementById('state');
      if elem <> nil then
        FGlobalProject.DisplayState := elem.innerText;
      elem := nil;
    end;
  except
    FGlobalProject.DisplayState := '';
  end;
  FGlobalProject.Save;*)
begin
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

procedure TfrmProject.cefAfterCreated(Sender: TObject);
begin
  RefreshHTML(False);
end;

procedure TfrmProject.cefBeforeBrowse(Sender: TObject;
  const browser: ICefBrowser; const frame: ICefFrame;
  const request: ICefRequest; user_gesture, isRedirect: Boolean;
  out Result: Boolean);
begin
  Result := DoNavigate(request.Url);
end;

procedure TfrmProject.cefBeforeClose(Sender: TObject);
begin
  Close;
end;


procedure TfrmProject.cefClose(Sender: TObject);
begin
  // DestroyChildWindow will destroy the child window created by CEF at the top of the Z order.
  if not cef.DestroyChildWindow then
  begin
    Close;
  end;
end;

procedure TfrmProject.cefConsoleMessage(Sender: TObject;
  const browser: ICefBrowser; level: TCefLogSeverity; const message,
  source: ustring; line: Integer; out Result: Boolean);
begin
  try
    with TStringList.Create do
    try
      LoadFromFile(GetGlobalProjectUI.RenderFileName);  // prolog determines encoding   // I4687

      LogExceptionToExternalHandler(
        'script_'+Self.ClassName+'_ScriptError',
        'Error occurred at line '+IntToStr(line)+' of '+source,
        message,
        'CEF'#13#10#13#10'<pre>'+XMLEncode(Text)+'</pre>');
    finally
      Free;
    end;
  except
    on E:Exception do
      LogExceptionToExternalHandler(
        'script_'+Self.ClassName+'_ScriptError',
        'Error occurred at line '+IntToStr(line)+' of '+source,
        message,
        'Exception '+E.Message+' trying to load '+GetGlobalProjectUI.RenderFileName+' for review');   // I4687
  end;

  Result := True;
end;

procedure TfrmProject.ClearMessages;
begin
  frmMessages.Clear;
end;

function TfrmProject.DoNavigate(URL: string): Boolean;
var
  n: Integer;
  s, command: WideString;
begin
  Result := False;
  if Copy(URL, 1, 7) = 'keyman:' then
  begin
    s := URL;

    Delete(s,1,7);
    n := Pos('?',s);
    FNextCommandParams.Clear;
    if n > 0 then
    begin
      command := Copy(s,1,n-1);
      DecodeAndSetParams(Copy(s,n+1,Length(s)), FNextCommandParams);
    end
    else
      command := s;

    FNextCommand := LowerCase(Command);
    PostMessage(Handle, WM_USER_WebCommand, WC_COMMAND, 0);
    Result := True;
  end
  else if Copy(URL, 1, 5) = 'help:' then
  begin
    Result := True;
    s := URL;
    Delete(s,1,5);
    FNextCommand := LowerCase(s);
    PostMessage(Handle, WM_USER_WebCommand, WC_HELP, 0);
  end
  else if not URL.StartsWith(modWebHttpServer.GetLocalhostURL) and (Copy(URL, 1, 4) = 'http') then
  begin
    Result := True;
    FNextCommand := URL;
    PostMessage(Handle, WM_USER_WebCommand, WC_OPENURL, 0);
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

procedure TfrmProject.tmrCreateBrowserTimer(Sender: TObject);
begin
  tmrCreateBrowser.Enabled := False;
  CreateBrowser;
end;

procedure TfrmProject.tmrRefreshTimer(Sender: TObject);
begin
  tmrRefresh.Enabled := False;
  ProjectRefresh(nil);
end;

procedure TfrmProject.cefLoadEnd(Sender: TObject; const browser: ICefBrowser;
  const frame: ICefFrame; httpStatusCode: Integer);
begin
  if Assigned(FGlobalProject) then GetGlobalProjectUI.Refreshing := False;   // I4687
  if (frmKeymanDeveloper.ActiveChild = Self) and (Screen.ActiveForm = frmKeymanDeveloper) then
  begin
    cef.SetFocus;
  end;
end;

procedure TfrmProject.cefPreKeyEvent(Sender: TObject;
  const browser: ICefBrowser; const event: PCefKeyEvent;
  osEvent: TCefEventHandle; out isKeyboardShortcut, Result: Boolean);
begin
  Result := False;
  if (event.windows_key_code <> VK_CONTROL) and (event.kind in [TCefKeyEventType.KEYEVENT_KEYDOWN, TCefKeyEventType.KEYEVENT_RAWKEYDOWN]) then
  begin
    if event.windows_key_code = VK_F1 then
    begin
      isKeyboardShortcut := True;
      Result := True;
      frmKeymanDeveloper.HelpTopic(Self);
    end
    else if event.windows_key_code = VK_F12 then
    begin
      cef.ChromiumBrowser.ShowDevTools(Point(Low(Integer),Low(Integer)), nil);
    end
    else if SendMessage(Application.Handle, CM_APPKEYDOWN, event.windows_key_code, 0) = 1 then
    begin
      isKeyboardShortcut := True;
      Result := True;
    end;
  end;
end;

//TODO: support dropping files
//  DropTarget := frmKeymanDeveloper.DropTargetIntf;

procedure TfrmProject.cefBeforePopup(Sender: TObject;
  const browser: ICefBrowser; const frame: ICefFrame; const targetUrl,
  targetFrameName: ustring; targetDisposition: TCefWindowOpenDisposition;
  userGesture: Boolean; const popupFeatures: TCefPopupFeatures;
  var windowInfo: TCefWindowInfo; var client: ICefClient;
  var settings: TCefBrowserSettings; var noJavascriptAccess, Result: Boolean);
begin
  Result := True;
  if not DoNavigate(targetUrl) then
    cef.LoadURL(targetUrl);
end;

function TfrmProject.GetHelpTopic: string;
begin
  Result := SHelpTopic_Context_Project;
end;

procedure TfrmProject.cefRunContextMenu(Sender: TObject;
  const browser: ICefBrowser; const frame: ICefFrame;
  const params: ICefContextMenuParams; const model: ICefMenuModel;
  const callback: ICefRunContextMenuCallback; var aResult: Boolean);
begin
  aResult := GetKeyState(VK_SHIFT) >= 0;
end;

procedure TfrmProject.WMEnterMenuLoop(var aMessage: TMessage);
begin
  inherited;
  if (aMessage.wParam = 0) and (GlobalCEFApp <> nil) then GlobalCEFApp.OsmodalLoop := True;
end;

procedure TfrmProject.WMExitMenuLoop(var aMessage: TMessage);
begin
  inherited;
  if (aMessage.wParam = 0) and (GlobalCEFApp <> nil) then GlobalCEFApp.OsmodalLoop := False;
end;

procedure TfrmProject.WMMove(var aMessage: TWMMove);
begin
  inherited;
  if cef <> nil then cef.NotifyMoveOrResizeStarted;
end;

procedure TfrmProject.WMMoving(var aMessage: TMessage);
begin
  inherited;
  if cef <> nil then cef.NotifyMoveOrResizeStarted;
end;

procedure TfrmProject.WMUserWebCommand(var Message: TMessage);
begin
  case Message.wParam of
    WC_COMMAND: WebCommand(FNextCommand, FNextCommandParams);
    WC_HELP: frmKeymanDeveloper.HelpTopic(FNextCommand);
    WC_OPENURL: TUtilExecute.URL(FNextCommand);
  end;
end;

end.
