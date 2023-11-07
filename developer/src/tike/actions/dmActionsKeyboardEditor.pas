(*
  Name:             dmActionsKeyboardEditor
  Copyright:        Copyright (C) SIL International.
  Documentation:    
  Description:      
  Create Date:      23 Aug 2006

  Modified Date:    4 May 2015
  Authors:          mcdurdin
  Related Files:    
  Dependencies:     

  Bugs:             
  Todo:             
  Notes:            
  History:          23 Aug 2006 - mcdurdin - Initial version
                    14 Sep 2006 - mcdurdin - Add debugger actions
                    28 Sep 2006 - mcdurdin - Added Test Keyboard menu item
                    06 Oct 2006 - mcdurdin - Add Test Keyman Web keyboard
                    04 Dec 2006 - mcdurdin - Implement all debug actions
                    04 Dec 2006 - mcdurdin - Localize
                    04 Dec 2006 - mcdurdin - Add keyboard upload
                    22 Jan 2007 - mcdurdin - Reset debugger before compiling
                    23 Aug 2007 - mcdurdin - I962 - Fix crash when uploading a keyboard and pressing Cancel
                    12 Oct 2007 - mcdurdin - I654 - handle error when installing or uninstalling an invalid keyboard file
                    16 Jan 2009 - mcdurdin - Add keyboard font helper command (incomplete)
                    26 Jul 2010 - mcdurdin - I2468 - KeymanWeb Pack elimination, part of standard build
                    17 Dec 2010 - mcdurdin - I2595 - Remove GnuGetText
                    18 May 2012 - mcdurdin - I3306 - V9.0 - Remove TntControls + Win9x support
                    18 May 2012 - mcdurdin - I3323 - V9.0 - Change from Plus-MemoU to Plus-Memo
                    04 Dec 2013 - mcdurdin - I3655 - V9.0 - Keyboard debugger does not appear to function in 9.0.419.0
                    21 Feb 2014 - mcdurdin - I4057 - V9.0 - Keyman Developer Keyboard Font dialog helpful to reduce font confusion
                    04 Nov 2014 - mcdurdin - I4504 - V9.0 - Consolidate the compile action into single command
                    04 May 2015 - mcdurdin - I4686 - V9.0 - Refactor compile into project file action
                    04 May 2015 - mcdurdin - I4687 - V9.0 - Split project UI actions into separate classes
                    
*)
unit dmActionsKeyboardEditor;  // I3306  // I3323

interface

uses
  SysUtils, Classes, ActnList,
  UfrmSelectSystemKeyboard, ImgList, Controls, Dialogs, System.Actions;

type
  TmodActionsKeyboardEditor = class(TDataModule)
    actionsKeyboardEditor: TActionList;
    actKeyboardCompile: TAction;
    actKeyboardIncludeDebugInformation: TAction;
    actKeyboardInstall: TAction;
    actKeyboardUninstall: TAction;
    actionsDebug: TActionList;
    actDebugStartDebugger: TAction;
    actDebugSetClearBreakpoint: TAction;
    actDebugStopDebugger: TAction;
    actDebugSingleStepMode: TAction;
    actDebugStepForward: TAction;
    actDebugRun: TAction;
    actDebugSelectSystemKeyboard: TAction;
    actDebugANSITestMode: TAction;
    actDebugViewElements: TAction;
    actDebugViewCallStack: TAction;
    actDebugViewDeadkeys: TAction;
    actDebugViewRegressionTesting: TAction;
    actDebugTestMode: TAction;
    actDebugDebuggerMode: TAction;
    actDebugPause: TAction;
    actDebugViewFont: TAction;
    actDebugViewDefaultFont: TAction;
    dlgFont: TFontDialog;
    actKeyboardTest: TAction;
    actKeyboardTestKeymanWeb: TAction;
    actDebugViewState: TAction;
    actKeyboardFontHelper: TAction;
    actKeyboardFonts: TAction;
    actDebugSwitchToDebuggerWindow: TAction;
    procedure actKeyboardCompileExecute(Sender: TObject);
    procedure actionsKeyboardEditorUpdate(Action: TBasicAction;
      var Handled: Boolean);
    procedure actKeyboardIncludeDebugInformationExecute(Sender: TObject);
    procedure actDebugViewElementsExecute(Sender: TObject);
    procedure actDebugSetClearBreakpointExecute(Sender: TObject);
    procedure actDebugSetClearBreakpointUpdate(Sender: TObject);
    procedure actDebugStartDebuggerExecute(Sender: TObject);
    procedure actDebugStartDebuggerUpdate(Sender: TObject);
    procedure actDebugStopDebuggerExecute(Sender: TObject);
    procedure actDebugStopDebuggerUpdate(Sender: TObject);
    procedure actKeyboardInstallExecute(Sender: TObject);
    procedure actKeyboardUninstallExecute(Sender: TObject);
    procedure actKeyboardIncludeDebugInformationUpdate(Sender: TObject);
    procedure actDebugSingleStepModeUpdate(Sender: TObject);
    procedure actDebugSingleStepModeExecute(Sender: TObject);
    procedure actDebugStepForwardUpdate(Sender: TObject);
    procedure actDebugStepForwardExecute(Sender: TObject);
    procedure actDebugRunUpdate(Sender: TObject);
    procedure actDebugRunExecute(Sender: TObject);
    procedure actDebugSelectSystemKeyboardExecute(Sender: TObject);
    procedure actDebugDebuggerModeExecute(Sender: TObject);
    procedure actDebugTestModeExecute(Sender: TObject);
    procedure actDebugTestModeUpdate(Sender: TObject);
    procedure actDebugDebuggerModeUpdate(Sender: TObject);
    procedure actDebugPauseUpdate(Sender: TObject);
    procedure actDebugPauseExecute(Sender: TObject);
    procedure actDebugSelectSystemKeyboardUpdate(Sender: TObject);
    procedure actDebugViewDefaultFontExecute(Sender: TObject);
    procedure actDebugViewDefaultFontUpdate(Sender: TObject);
    procedure actDebugViewFontExecute(Sender: TObject);
    procedure actDebugViewFontUpdate(Sender: TObject);
    procedure actKeyboardTestExecute(Sender: TObject);
    procedure actKeyboardTestKeymanWebExecute(Sender: TObject);
    procedure actDebugViewCallStackExecute(Sender: TObject);
    procedure actDebugViewDeadkeysExecute(Sender: TObject);
    procedure actDebugViewRegressionTestingExecute(Sender: TObject);
    procedure actDebugViewElementsUpdate(Sender: TObject);
    procedure actDebugViewCallStackUpdate(Sender: TObject);
    procedure actDebugViewDeadkeysUpdate(Sender: TObject);
    procedure actDebugViewRegressionTestingUpdate(Sender: TObject);
    procedure actDebugViewStateExecute(Sender: TObject);
    procedure actDebugViewStateUpdate(Sender: TObject);
    procedure actKeyboardFontHelperExecute(Sender: TObject);
    procedure actKeyboardFontsExecute(Sender: TObject);
    procedure actKeyboardCompileUpdate(Sender: TObject);
    procedure actDebugSwitchToDebuggerWindowUpdate(Sender: TObject);
    procedure actDebugSwitchToDebuggerWindowExecute(Sender: TObject);
  private
    function IsDebuggerVisible: Boolean;
    function IsDebuggerInTestMode: Boolean;
    { Private declarations }
  public
    { Public declarations }
    procedure SelectDebugSystemKeyboard(k: TSystemKeyboardItem);
  end;

var
  modActionsKeyboardEditor: TmodActionsKeyboardEditor;

implementation

uses
  Vcl.Forms,
  Vcl.Menus,
  System.Win.ComObj,
  Winapi.Windows,

  KMDActions,
  KMDActionInterfaces,
  Keyman.System.Debug.DebugUIStatus,
  Keyman.Developer.System.Project.kmnProjectFile,
  Keyman.Developer.UI.Project.kmnProjectFileUI,
  Keyman.Developer.System.Project.kpsProjectFile,
  Keyman.Developer.System.Project.Project,
  Keyman.Developer.System.Project.ProjectFile,
  Keyman.Developer.UI.Project.ProjectFileUI,
  Keyman.Developer.UI.Project.UfrmProject,
  Keyman.Developer.UI.UfrmLdmlKeyboardEditor,
  Keyman.Developer.System.Project.xmlLdmlProjectFile,
  UframeTextEditor,
  UfrmKeymanWizard,
  UfrmDebug,
  UfrmDebugStatus,
  UfrmMessages,
  UfrmMain,
  UfrmPackageEditor,
  UKeymanTargets;

{$R *.dfm}

function ActiveLdmlKeyboardEditor: TfrmLdmlKeyboardEditor;
begin
  if Assigned(frmKeymanDeveloper.ActiveChild) and (frmKeymanDeveloper.ActiveChild is TfrmLdmlKeyboardEditor)
    then Result := frmKeymanDeveloper.ActiveChild as TfrmLdmlKeyboardEditor
    else Result := nil;
end;

function ActiveKmnKeyboardEditor: TfrmKeymanWizard;
begin
  if Assigned(frmKeymanDeveloper.ActiveChild) and (frmKeymanDeveloper.ActiveChild is TfrmKeymanWizard)
    then Result := frmKeymanDeveloper.ActiveChild as TfrmKeymanWizard
    else Result := nil;
end;

function ActivePackageEditor: TfrmPackageEditor;
begin
  if Assigned(frmKeymanDeveloper.ActiveChild) and (frmKeymanDeveloper.ActiveChild is TfrmPackageEditor)
    then Result := frmKeymanDeveloper.ActiveChild as TfrmPackageEditor
    else Result := nil;
end;

function ActiveLdmlKeyboardProjectFile: TxmlLdmlProjectFile;
begin
  Result := ActiveLdmlKeyboardEditor.ProjectFile as TxmlLdmlProjectFile;
end;

function ActiveKmnKeyboardProjectFile: TkmnProjectFile;
begin
  Result := ActiveKmnKeyboardEditor.ProjectFile as TkmnProjectFile;
end;

function ActivePackageProjectFile: TkpsProjectFile;
begin
  Result := ActivePackageEditor.ProjectFile as TkpsProjectFile;
end;

function ActiveMemo: IKMDTextEditorActions;
begin
  Result := GetTextEditorController(Screen.ActiveControl);
end;

procedure TmodActionsKeyboardEditor.actionsKeyboardEditorUpdate(Action: TBasicAction; var Handled: Boolean);
begin
  with Action as TAction do
  begin
    if not Assigned(Action.OnUpdate) then
    begin
      Enabled := ActiveKmnKeyboardEditor <> nil;
      if not Enabled then Handled := True;
    end;
  end;
end;

{ ---- Debug Menu ---- }

procedure TmodActionsKeyboardEditor.actDebugDebuggerModeExecute(Sender: TObject);
begin
  if ActiveKmnKeyboardEditor.DebugForm.UIStatus = duiTest then
    ActiveKmnKeyboardEditor.DebugForm.UIStatus := duiReadyForInput;
end;

procedure TmodActionsKeyboardEditor.actDebugDebuggerModeUpdate(Sender: TObject);
begin
  actDebugDebuggerMode.Checked := (ActiveKmnKeyboardEditor <> nil) and (ActiveKmnKeyboardEditor.DebugForm.UIStatus <> duiTest);
end;

procedure TmodActionsKeyboardEditor.actDebugPauseExecute(Sender: TObject);
begin
  if ActiveKmnKeyboardEditor.DebugForm.UIStatus = duiPaused
    then ActiveKmnKeyboardEditor.DebugForm.Unpause
    else ActiveKmnKeyboardEditor.DebugForm.Pause;
end;

procedure TmodActionsKeyboardEditor.actDebugPauseUpdate(Sender: TObject);
begin
  actDebugPause.Enabled :=
    (ActiveKmnKeyboardEditor <> nil) and
    IsDebuggerVisible and
    not IsDebuggerInTestMode and
    (ActiveKmnKeyboardEditor.DebugForm.UIStatus in [duiPaused, duiFocusedForInput, duiReadyForInput]);

  if actDebugPause.Enabled
    then actDebugPause.Checked := ActiveKmnKeyboardEditor.DebugForm.UIStatus = duiPaused
    else actDebugPause.Checked := False;
end;

procedure TmodActionsKeyboardEditor.actDebugRunExecute(Sender: TObject);
begin
  ActiveKmnKeyboardEditor.DebugForm.Run;
end;

procedure TmodActionsKeyboardEditor.actDebugRunUpdate(Sender: TObject);
begin
  actDebugRun.Enabled := IsDebuggerVisible and not IsDebuggerInTestMode and 
    (ActiveKmnKeyboardEditor.DebugForm.UIStatus = duiDebugging);
end;

procedure TmodActionsKeyboardEditor.actDebugSelectSystemKeyboardExecute(Sender: TObject);
var
  FKeyboardID: string;
begin
  if SelectSystemKeyboard(frmKeymanDeveloper, FKeyboardID) then
  begin
    // TODO: this is disabled?
    //ActiveKmnKeyboardEditor.DebugForm.SetSystemKeyboardID(FKeyboardID);   // I3655
  end;
end;

procedure TmodActionsKeyboardEditor.actDebugSelectSystemKeyboardUpdate(Sender: TObject);
begin
  actDebugSelectSystemKeyboard.Enabled := IsDebuggerVisible;
end;

procedure TmodActionsKeyboardEditor.actDebugSetClearBreakpointExecute(Sender: TObject);
begin
  if ActiveKmnKeyboardEditor.DebugForm.IsBreakPointLine(ActiveMemo.SelectedRow)
    then ActiveKmnKeyboardEditor.DebugForm.ClearBreakpoint(ActiveMemo.SelectedRow)
    else ActiveKmnKeyboardEditor.DebugForm.SetBreakpoint(ActiveMemo.SelectedRow);
end;

procedure TmodActionsKeyboardEditor.actDebugSetClearBreakpointUpdate(Sender: TObject);
begin
  if (ActiveKmnKeyboardEditor <> nil) and (ActiveMemo <> nil) and Assigned(ActiveKmnKeyboardEditor.DebugForm) then
  begin
    actDebugSetClearBreakpoint.Enabled := True;
    actDebugSetClearBreakpoint.Visible := True;
    if ActiveKmnKeyboardEditor.DebugForm.IsBreakPointLine(ActiveMemo.SelectedRow)
      then actDebugSetClearBreakpoint.Caption := 'Clear &Breakpoint'
      else actDebugSetClearBreakpoint.Caption := 'Set &Breakpoint';
  end
  else
  begin
    actDebugSetClearBreakpoint.Enabled := False;
    actDebugSetClearBreakpoint.Visible := False;
  end;
end;

procedure TmodActionsKeyboardEditor.actDebugSingleStepModeExecute(
  Sender: TObject);
begin
  ActiveKmnKeyboardEditor.DebugForm.SingleStepMode := not ActiveKmnKeyboardEditor.DebugForm.SingleStepMode;
end;

procedure TmodActionsKeyboardEditor.actDebugSingleStepModeUpdate(Sender: TObject);
begin
  actDebugSingleStepMode.Enabled := IsDebuggerVisible and not IsDebuggerInTestMode;
  if actDebugSingleStepMode.Enabled
    then actDebugSingleStepMode.Checked := ActiveKmnKeyboardEditor.DebugForm.SingleStepMode
    else actDebugSingleStepMode.Checked := False;
end;

procedure TmodActionsKeyboardEditor.actDebugStartDebuggerExecute(Sender: TObject);
begin
  ActiveKmnKeyboardEditor.StartDebugging;
end;

procedure TmodActionsKeyboardEditor.actDebugStartDebuggerUpdate(Sender: TObject);
begin
  actDebugStartDebugger.Enabled := (ActiveKmnKeyboardEditor <> nil) and not ActiveKmnKeyboardEditor.IsDebugVisible;
end;

procedure TmodActionsKeyboardEditor.actDebugStepForwardExecute(Sender: TObject);
begin
  ActiveKmnKeyboardEditor.DebugForm.StepForward;
end;

procedure TmodActionsKeyboardEditor.actDebugStepForwardUpdate(Sender: TObject);
begin
  actDebugStepForward.Enabled := IsDebuggerVisible and not IsDebuggerInTestMode  and
    (ActiveKmnKeyboardEditor.DebugForm.UIStatus = duiDebugging);
end;

procedure TmodActionsKeyboardEditor.actDebugStopDebuggerExecute(
  Sender: TObject);
begin
  ActiveKmnKeyboardEditor.StopDebugging;
end;

procedure TmodActionsKeyboardEditor.actDebugStopDebuggerUpdate(Sender: TObject);
begin
  actDebugStopDebugger.Enabled := (ActiveKmnKeyboardEditor <> nil) and ActiveKmnKeyboardEditor.IsDebugVisible;
end;

procedure TmodActionsKeyboardEditor.actDebugSwitchToDebuggerWindowExecute(
  Sender: TObject);
begin
  ActiveKmnKeyboardEditor.DebugForm.SetFocus;
end;

procedure TmodActionsKeyboardEditor.actDebugSwitchToDebuggerWindowUpdate(
  Sender: TObject);
begin
  actDebugSwitchToDebuggerWindow.Enabled := (ActiveKmnKeyboardEditor <> nil) and ActiveKmnKeyboardEditor.IsDebugVisible and
    ((Screen.ActiveControl = nil) or (Screen.ActiveControl.Owner <> ActiveKmnKeyboardEditor.DebugForm));
end;

procedure TmodActionsKeyboardEditor.actDebugTestModeExecute(Sender: TObject);
begin
  ActiveKmnKeyboardEditor.DebugForm.UIStatus := duiTest;
end;

procedure TmodActionsKeyboardEditor.actDebugTestModeUpdate(Sender: TObject);
begin
  actDebugTestMode.Enabled := (ActiveKmnKeyboardEditor <> nil);
  actDebugTestMode.Checked := actDebugTestMode.Enabled and (ActiveKmnKeyboardEditor.DebugForm.UIStatus = duiTest);
end;

procedure TmodActionsKeyboardEditor.actDebugViewCallStackExecute(Sender: TObject);
begin
  ActiveKmnKeyboardEditor.DebugStatusForm.Visible := True;
  ActiveKmnKeyboardEditor.DebugStatusForm.pagesDebug.ActivePage := ActiveKmnKeyboardEditor.DebugStatusForm.tabDebugCallStack;
end;

procedure TmodActionsKeyboardEditor.actDebugViewCallStackUpdate(
  Sender: TObject);
begin
  actDebugViewCallStack.Enabled := (ActiveKmnKeyboardEditor <> nil) and ActiveKmnKeyboardEditor.IsDebugVisible;
end;

procedure TmodActionsKeyboardEditor.actDebugViewDeadkeysExecute(Sender: TObject);
begin
  ActiveKmnKeyboardEditor.DebugStatusForm.Visible := True;
  ActiveKmnKeyboardEditor.DebugStatusForm.pagesDebug.ActivePage := ActiveKmnKeyboardEditor.DebugStatusForm.tabDebugDeadkeys;
end;

procedure TmodActionsKeyboardEditor.actDebugViewDeadkeysUpdate(Sender: TObject);
begin
  actDebugViewDeadkeys.Enabled := (ActiveKmnKeyboardEditor <> nil) and ActiveKmnKeyboardEditor.IsDebugVisible;
end;

procedure TmodActionsKeyboardEditor.actDebugViewDefaultFontExecute(Sender: TObject);
begin
  ActiveKmnKeyboardEditor.DebugForm.UpdateFont(nil);
end;

procedure TmodActionsKeyboardEditor.actDebugViewDefaultFontUpdate(Sender: TObject);
begin
  actDebugViewDefaultFont.Enabled := IsDebuggerVisible;
  actDebugViewDefaultFont.Checked := actDebugViewDefaultFont.Enabled and
    ActiveKmnKeyboardEditor.DebugForm.DefaultFont;
end;

procedure TmodActionsKeyboardEditor.actDebugViewFontExecute(Sender: TObject);
begin
  dlgFont.Font := ActiveKmnKeyboardEditor.DebugForm.memo.Font;
  if dlgFont.Execute then
    ActiveKmnKeyboardEditor.DebugForm.UpdateFont(dlgFont.Font);
end;

procedure TmodActionsKeyboardEditor.actDebugViewFontUpdate(Sender: TObject);
begin
  actDebugViewFont.Enabled := IsDebuggerVisible;
  actDebugViewFont.Checked := actDebugViewFont.Enabled and not ActiveKmnKeyboardEditor.DebugForm.DefaultFont;
end;

procedure TmodActionsKeyboardEditor.actDebugViewRegressionTestingExecute(Sender: TObject);
begin
  ActiveKmnKeyboardEditor.DebugStatusForm.Visible := True;
  ActiveKmnKeyboardEditor.DebugStatusForm.pagesDebug.ActivePage := ActiveKmnKeyboardEditor.DebugStatusForm.tabDebugRegressionTesting;
end;

procedure TmodActionsKeyboardEditor.actDebugViewRegressionTestingUpdate(Sender: TObject);
begin
  actDebugViewRegressionTesting.Enabled := (ActiveKmnKeyboardEditor <> nil) and ActiveKmnKeyboardEditor.IsDebugVisible;
end;

procedure TmodActionsKeyboardEditor.actDebugViewStateExecute(Sender: TObject);
begin
  ActiveKmnKeyboardEditor.DebugStatusForm.Visible := True;
  ActiveKmnKeyboardEditor.DebugStatusForm.pagesDebug.ActivePage := ActiveKmnKeyboardEditor.DebugStatusForm.tabDebugKey;
end;

procedure TmodActionsKeyboardEditor.actDebugViewStateUpdate(Sender: TObject);
begin
  actDebugViewState.Enabled := (ActiveKmnKeyboardEditor <> nil) and ActiveKmnKeyboardEditor.IsDebugVisible;
end;

procedure TmodActionsKeyboardEditor.actDebugViewElementsExecute(Sender: TObject);
begin
  ActiveKmnKeyboardEditor.DebugStatusForm.Visible := True;
  ActiveKmnKeyboardEditor.DebugStatusForm.pagesDebug.ActivePage := ActiveKmnKeyboardEditor.DebugStatusForm.tabDebugStores;
end;

procedure TmodActionsKeyboardEditor.actDebugViewElementsUpdate(Sender: TObject);
begin
  actDebugViewElements.Enabled := (ActiveKmnKeyboardEditor <> nil) and ActiveKmnKeyboardEditor.IsDebugVisible;
end;

{ ---- Keyboard Menu ---- }

procedure TmodActionsKeyboardEditor.actKeyboardCompileExecute(Sender: TObject);   // I4504
var
  DebugReset: Boolean;
begin
  DebugReset := False;

  frmMessages.Clear;   // I4686

  if ActiveKmnKeyboardEditor <> nil then
  begin
    if not ActiveKmnKeyboardEditor.PrepareForBuild(DebugReset) then
      Exit;

    if (ActiveKmnKeyboardProjectFile.UI as TProjectFileUI).DoAction(pfaCompile, False) and DebugReset then   // I4686
      ActiveKmnKeyboardEditor.StartDebugging;
  end
  else if ActivePackageEditor <> nil then
  begin
    (ActivePackageProjectFile.UI as TProjectFileUI).DoAction(pfaCompile, False);
  end
  else if frmKeymanDeveloper.ActiveChild is TfrmProject then
    (frmKeymanDeveloper.ActiveChild as TfrmProject).CompileAll;
end;

procedure TmodActionsKeyboardEditor.actKeyboardCompileUpdate(Sender: TObject);
begin
  // TODO: Split Keyboard menu and package editor functions
  actKeyboardCompile.Enabled :=
    (ActiveKmnKeyboardEditor <> nil) or
    (ActivePackageEditor <> nil) or
    ((frmKeymanDeveloper.ActiveChild is TfrmProject) and (FGlobalProject <> nil));

  if actKeyboardCompile.Enabled
    then actKeyboardCompile.ShortCut := Vcl.Menus.Shortcut(VK_F7, [])
    else actKeyboardCompile.ShortCut := scNone;

  frmKeymanDeveloper.mnuKeyboard.Visible := True;
  frmKeymanDeveloper.mnuDebug.Visible := ActiveKmnKeyboardEditor <> nil;
end;

procedure TmodActionsKeyboardEditor.actKeyboardFontHelperExecute(
  Sender: TObject);
begin
  (ActiveKmnKeyboardProjectFile.UI as TProjectFileUI).DoAction(pfaFontHelper, False)   // I4687
end;

procedure TmodActionsKeyboardEditor.actKeyboardFontsExecute(Sender: TObject);   // I4057
begin
  (ActiveKmnKeyboardProjectFile.UI as TProjectFileUI).DoAction(pfaFontDialog, False);   // I4687
end;

procedure TmodActionsKeyboardEditor.actKeyboardIncludeDebugInformationExecute(Sender: TObject);
begin
  (ActiveKmnKeyboardProjectFile.UI as TkmnProjectFileUI).Debug := not (ActiveKmnKeyboardProjectFile.UI as TkmnProjectFileUI).Debug;   // I4687
end;

procedure TmodActionsKeyboardEditor.actKeyboardIncludeDebugInformationUpdate(
  Sender: TObject);
begin
  actKeyboardIncludeDebugInformation.Enabled := ActiveKmnKeyboardEditor <> nil;
  if (ActiveKmnKeyboardEditor <> nil) and (ActiveKmnKeyboardProjectFile <> nil)
    then actKeyboardIncludeDebugInformation.Checked := (ActiveKmnKeyboardProjectFile.UI as TkmnProjectFileUI).Debug   // I4687
    else actKeyboardIncludeDebugInformation.Checked := False;
end;

procedure TmodActionsKeyboardEditor.actKeyboardInstallExecute(Sender: TObject);
begin
  try
    (ActiveKmnKeyboardProjectFile.UI as TProjectFileUI).DoAction(pfaInstall, False);   // I4687
    //WideShowMessage('Keyboard installed successfully.');
  except
    on E:EOleException do // I654
    begin
      ShowMessage(E.Message);
    end;
  end;
end;

procedure TmodActionsKeyboardEditor.actKeyboardTestExecute(Sender: TObject);
begin
  ActiveKmnKeyboardEditor.DebugForm.UIStatus := duiTest;
  ActiveKmnKeyboardEditor.StartDebugging(True);
end;

procedure TmodActionsKeyboardEditor.actKeyboardTestKeymanWebExecute(
  Sender: TObject);
begin
  (ActiveKmnKeyboardProjectFile.UI as TProjectFileUI).DoAction(pfaTestKeymanWeb, False);   // I4687
end;

procedure TmodActionsKeyboardEditor.actKeyboardUninstallExecute(Sender: TObject);
begin
  try
    if (ActiveKmnKeyboardProjectFile.UI as TProjectFileUI).DoAction(pfaUninstall, False)
      then ShowMessage('Keyboard uninstalled successfully.')
      else ShowMessage('Failed to uninstall keyboard.');
  except
    on E:EOleException do // I654
    begin
      ShowMessage(E.Message);
    end;
  end;
end;

function TmodActionsKeyboardEditor.IsDebuggerInTestMode: Boolean;
begin
  Result := (ActiveKmnKeyboardEditor <> nil) and (ActiveKmnKeyboardEditor.DebugForm.UIStatus = duiTest);
end;

function TmodActionsKeyboardEditor.IsDebuggerVisible: Boolean;
begin
  Result := (ActiveKmnKeyboardEditor <> nil) and ActiveKmnKeyboardEditor.IsDebugVisible;
end;

procedure TmodActionsKeyboardEditor.SelectDebugSystemKeyboard(k: TSystemKeyboardItem);
begin
//TODO: #1225, #1074 (note both menu item and toolbar combo are hidden while this is not working)
//  if IsDebuggerVisible then   // I3655
//    ActiveKmnKeyboardEditor.DebugForm.SetSystemKeyboardID(k.KeyboardID);
end;

end.
