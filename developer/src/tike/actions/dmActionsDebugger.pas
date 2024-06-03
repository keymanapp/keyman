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
unit dmActionsDebugger;  // I3306  // I3323

interface

uses
  SysUtils,
  Classes,
  ActnList,
  UfrmSelectSystemKeyboard,
  ImgList,
  Controls,
  Dialogs,
  System.Actions;

type
  TmodActionsDebugger = class(TDataModule)
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
    actDebugViewState: TAction;
    actDebugSwitchToDebuggerWindow: TAction;
    procedure actionsKeyboardEditorUpdate(Action: TBasicAction;
      var Handled: Boolean);
    procedure actDebugViewElementsExecute(Sender: TObject);
    procedure actDebugSetClearBreakpointExecute(Sender: TObject);
    procedure actDebugSetClearBreakpointUpdate(Sender: TObject);
    procedure actDebugStartDebuggerExecute(Sender: TObject);
    procedure actDebugStartDebuggerUpdate(Sender: TObject);
    procedure actDebugStopDebuggerExecute(Sender: TObject);
    procedure actDebugStopDebuggerUpdate(Sender: TObject);
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
    procedure actDebugViewCallStackExecute(Sender: TObject);
    procedure actDebugViewDeadkeysExecute(Sender: TObject);
    procedure actDebugViewRegressionTestingExecute(Sender: TObject);
    procedure actDebugViewElementsUpdate(Sender: TObject);
    procedure actDebugViewCallStackUpdate(Sender: TObject);
    procedure actDebugViewDeadkeysUpdate(Sender: TObject);
    procedure actDebugViewRegressionTestingUpdate(Sender: TObject);
    procedure actDebugViewStateExecute(Sender: TObject);
    procedure actDebugViewStateUpdate(Sender: TObject);
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
  modActionsDebugger: TmodActionsDebugger;

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
  Keyman.Developer.UI.UfrmLdmlKeyboardEditor,
  UfrmKeymanWizard,
  UfrmDebug,
  UfrmDebugStatus,
  UfrmMain;

{$R *.dfm}

function ActiveKmnKeyboardEditor: TfrmKeymanWizard;
begin
  if Assigned(frmKeymanDeveloper.ActiveChild) and (frmKeymanDeveloper.ActiveChild is TfrmKeymanWizard)
    then Result := frmKeymanDeveloper.ActiveChild as TfrmKeymanWizard
    else Result := nil;
end;

function ActiveLdmlKeyboardEditor: TfrmLdmlKeyboardEditor;
begin
  if Assigned(frmKeymanDeveloper.ActiveChild) and (frmKeymanDeveloper.ActiveChild is TfrmLdmlKeyboardEditor)
    then Result := frmKeymanDeveloper.ActiveChild as TfrmLdmlKeyboardEditor
    else Result := nil;
end;

function ActiveKmnKeyboardProjectFile: TkmnProjectFile;
begin
  Result := ActiveKmnKeyboardEditor.ProjectFile as TkmnProjectFile;
end;

function ActiveMemo: IKMDTextEditorActions;
begin
  Result := GetTextEditorController(Screen.ActiveControl);
end;

procedure TmodActionsDebugger.actionsKeyboardEditorUpdate(Action: TBasicAction; var Handled: Boolean);
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

procedure TmodActionsDebugger.actDebugDebuggerModeExecute(Sender: TObject);
begin
  if ActiveKmnKeyboardEditor.DebugForm.UIStatus = duiTest then
    ActiveKmnKeyboardEditor.DebugForm.UIStatus := duiReadyForInput;
end;

procedure TmodActionsDebugger.actDebugDebuggerModeUpdate(Sender: TObject);
begin
  actDebugDebuggerMode.Enabled := (ActiveKmnKeyboardEditor <> nil);
  actDebugDebuggerMode.Checked := (ActiveKmnKeyboardEditor <> nil) and (ActiveKmnKeyboardEditor.DebugForm.UIStatus <> duiTest);
end;

procedure TmodActionsDebugger.actDebugPauseExecute(Sender: TObject);
begin
  if ActiveKmnKeyboardEditor.DebugForm.UIStatus = duiPaused
    then ActiveKmnKeyboardEditor.DebugForm.Unpause
    else ActiveKmnKeyboardEditor.DebugForm.Pause;
end;

procedure TmodActionsDebugger.actDebugPauseUpdate(Sender: TObject);
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

procedure TmodActionsDebugger.actDebugRunExecute(Sender: TObject);
begin
  ActiveKmnKeyboardEditor.DebugForm.Run;
end;

procedure TmodActionsDebugger.actDebugRunUpdate(Sender: TObject);
begin
  actDebugRun.Enabled := IsDebuggerVisible and not IsDebuggerInTestMode and 
    (ActiveKmnKeyboardEditor.DebugForm.UIStatus = duiDebugging);
end;

procedure TmodActionsDebugger.actDebugSelectSystemKeyboardExecute(Sender: TObject);
var
  FKeyboardID: string;
begin
  if SelectSystemKeyboard(frmKeymanDeveloper, FKeyboardID) then
  begin
    // TODO: this is disabled?
    //ActiveKmnKeyboardEditor.DebugForm.SetSystemKeyboardID(FKeyboardID);   // I3655
  end;
end;

procedure TmodActionsDebugger.actDebugSelectSystemKeyboardUpdate(Sender: TObject);
begin
  actDebugSelectSystemKeyboard.Enabled := IsDebuggerVisible;
end;

procedure TmodActionsDebugger.actDebugSetClearBreakpointExecute(Sender: TObject);
begin
  if ActiveKmnKeyboardEditor.DebugForm.IsBreakPointLine(ActiveMemo.SelectedRow)
    then ActiveKmnKeyboardEditor.DebugForm.ClearBreakpoint(ActiveMemo.SelectedRow)
    else ActiveKmnKeyboardEditor.DebugForm.SetBreakpoint(ActiveMemo.SelectedRow);
end;

procedure TmodActionsDebugger.actDebugSetClearBreakpointUpdate(Sender: TObject);
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

procedure TmodActionsDebugger.actDebugSingleStepModeExecute(
  Sender: TObject);
begin
  ActiveKmnKeyboardEditor.DebugForm.SingleStepMode := not ActiveKmnKeyboardEditor.DebugForm.SingleStepMode;
end;

procedure TmodActionsDebugger.actDebugSingleStepModeUpdate(Sender: TObject);
begin
  actDebugSingleStepMode.Enabled := IsDebuggerVisible and not IsDebuggerInTestMode;
  if actDebugSingleStepMode.Enabled
    then actDebugSingleStepMode.Checked := ActiveKmnKeyboardEditor.DebugForm.SingleStepMode
    else actDebugSingleStepMode.Checked := False;
end;

procedure TmodActionsDebugger.actDebugStartDebuggerExecute(Sender: TObject);
begin
  if ActiveKmnKeyboardEditor <> nil then
    ActiveKmnKeyboardEditor.StartDebugging
  else
    ActiveLdmlKeyboardEditor.StartDebugging;
end;

procedure TmodActionsDebugger.actDebugStartDebuggerUpdate(Sender: TObject);
begin
  actDebugStartDebugger.Enabled := (
    (ActiveKmnKeyboardEditor <> nil) and not ActiveKmnKeyboardEditor.IsDebugVisible
   ) or (
    (ActiveLdmlKeyboardEditor <> nil) and not ActiveLdmlKeyboardEditor.IsDebugVisible
   );
end;

procedure TmodActionsDebugger.actDebugStepForwardExecute(Sender: TObject);
begin
  ActiveKmnKeyboardEditor.DebugForm.StepForward;
end;

procedure TmodActionsDebugger.actDebugStepForwardUpdate(Sender: TObject);
begin
  actDebugStepForward.Enabled := IsDebuggerVisible and not IsDebuggerInTestMode  and
    (ActiveKmnKeyboardEditor.DebugForm.UIStatus = duiDebugging);
end;

procedure TmodActionsDebugger.actDebugStopDebuggerExecute(
  Sender: TObject);
begin
  if ActiveKmnKeyboardEditor <> nil then
    ActiveKmnKeyboardEditor.StopDebugging
  else
    ActiveLdmlKeyboardEditor.StopDebugging;
end;

procedure TmodActionsDebugger.actDebugStopDebuggerUpdate(Sender: TObject);
begin
  actDebugStopDebugger.Enabled :=
    ((ActiveKmnKeyboardEditor <> nil) and ActiveKmnKeyboardEditor.IsDebugVisible) or
    ((ActiveLdmlKeyboardEditor <> nil) and ActiveLdmlKeyboardEditor.IsDebugVisible);
end;

procedure TmodActionsDebugger.actDebugSwitchToDebuggerWindowExecute(
  Sender: TObject);
begin
  if ActiveKmnKeyboardEditor <> nil then
    ActiveKmnKeyboardEditor.DebugForm.SetFocus
  else
    ActiveLdmlKeyboardEditor.DebugForm.SetFocus;
end;

procedure TmodActionsDebugger.actDebugSwitchToDebuggerWindowUpdate(
  Sender: TObject);
begin
  actDebugSwitchToDebuggerWindow.Enabled := (
    (ActiveKmnKeyboardEditor <> nil) and ActiveKmnKeyboardEditor.IsDebugVisible and
    ((Screen.ActiveControl = nil) or (Screen.ActiveControl.Owner <> ActiveKmnKeyboardEditor.DebugForm))
   ) or (
    (ActiveLdmlKeyboardEditor <> nil) and ActiveLdmlKeyboardEditor.IsDebugVisible and
    ((Screen.ActiveControl = nil) or (Screen.ActiveControl.Owner <> ActiveLdmlKeyboardEditor.DebugForm))
   );
end;

procedure TmodActionsDebugger.actDebugTestModeExecute(Sender: TObject);
begin
  ActiveKmnKeyboardEditor.DebugForm.UIStatus := duiTest;
end;

procedure TmodActionsDebugger.actDebugTestModeUpdate(Sender: TObject);
begin
  actDebugTestMode.Enabled := (ActiveKmnKeyboardEditor <> nil);
  actDebugTestMode.Checked := actDebugTestMode.Enabled and (ActiveKmnKeyboardEditor.DebugForm.UIStatus = duiTest);
end;

procedure TmodActionsDebugger.actDebugViewCallStackExecute(Sender: TObject);
begin
  ActiveKmnKeyboardEditor.DebugStatusForm.Visible := True;
  ActiveKmnKeyboardEditor.DebugStatusForm.pagesDebug.ActivePage := ActiveKmnKeyboardEditor.DebugStatusForm.tabDebugCallStack;
end;

procedure TmodActionsDebugger.actDebugViewCallStackUpdate(
  Sender: TObject);
begin
  actDebugViewCallStack.Enabled := (ActiveKmnKeyboardEditor <> nil) and ActiveKmnKeyboardEditor.IsDebugVisible;
end;

procedure TmodActionsDebugger.actDebugViewDeadkeysExecute(Sender: TObject);
begin
  ActiveKmnKeyboardEditor.DebugStatusForm.Visible := True;
  ActiveKmnKeyboardEditor.DebugStatusForm.pagesDebug.ActivePage := ActiveKmnKeyboardEditor.DebugStatusForm.tabDebugDeadkeys;
end;

procedure TmodActionsDebugger.actDebugViewDeadkeysUpdate(Sender: TObject);
begin
  actDebugViewDeadkeys.Enabled := (ActiveKmnKeyboardEditor <> nil) and ActiveKmnKeyboardEditor.IsDebugVisible;
end;

procedure TmodActionsDebugger.actDebugViewDefaultFontExecute(Sender: TObject);
begin
  if ActiveKmnKeyboardEditor <> nil
    then ActiveKmnKeyboardEditor.DebugForm.UpdateFont(nil)
    else ActiveLdmlKeyboardEditor.DebugForm.UpdateFont(nil)
end;

procedure TmodActionsDebugger.actDebugViewDefaultFontUpdate(Sender: TObject);
begin
  actDebugViewDefaultFont.Enabled := IsDebuggerVisible;
  actDebugViewDefaultFont.Checked := actDebugViewDefaultFont.Enabled and (
    ((ActiveKmnKeyboardEditor <> nil) and ActiveKmnKeyboardEditor.DebugForm.DefaultFont)
  ) or (
    ((ActiveLdmlKeyboardEditor <> nil) and ActiveLdmlKeyboardEditor.DebugForm.DefaultFont)
  );
end;

procedure TmodActionsDebugger.actDebugViewFontExecute(Sender: TObject);
begin
  if ActiveKmnKeyboardEditor <> nil then
  begin
    dlgFont.Font := ActiveKmnKeyboardEditor.DebugForm.memo.Font;
    if dlgFont.Execute then
      ActiveKmnKeyboardEditor.DebugForm.UpdateFont(dlgFont.Font);
  end
  else
  begin
    dlgFont.Font := ActiveLdmlKeyboardEditor.DebugForm.memo.Font;
    if dlgFont.Execute then
      ActiveLdmlKeyboardEditor.DebugForm.UpdateFont(dlgFont.Font);
  end;
end;

procedure TmodActionsDebugger.actDebugViewFontUpdate(Sender: TObject);
begin
  actDebugViewFont.Enabled := IsDebuggerVisible;
  actDebugViewFont.Checked := actDebugViewFont.Enabled and (
    ((ActiveKmnKeyboardEditor <> nil) and not ActiveKmnKeyboardEditor.DebugForm.DefaultFont)
  ) or (
    ((ActiveLdmlKeyboardEditor <> nil) and not ActiveLdmlKeyboardEditor.DebugForm.DefaultFont)
  );
end;

procedure TmodActionsDebugger.actDebugViewRegressionTestingExecute(Sender: TObject);
begin
  ActiveKmnKeyboardEditor.DebugStatusForm.Visible := True;
  ActiveKmnKeyboardEditor.DebugStatusForm.pagesDebug.ActivePage := ActiveKmnKeyboardEditor.DebugStatusForm.tabDebugRegressionTesting;
end;

procedure TmodActionsDebugger.actDebugViewRegressionTestingUpdate(Sender: TObject);
begin
  actDebugViewRegressionTesting.Enabled := (ActiveKmnKeyboardEditor <> nil) and ActiveKmnKeyboardEditor.IsDebugVisible;
end;

procedure TmodActionsDebugger.actDebugViewStateExecute(Sender: TObject);
begin
  ActiveKmnKeyboardEditor.DebugStatusForm.Visible := True;
  ActiveKmnKeyboardEditor.DebugStatusForm.pagesDebug.ActivePage := ActiveKmnKeyboardEditor.DebugStatusForm.tabDebugKey;
end;

procedure TmodActionsDebugger.actDebugViewStateUpdate(Sender: TObject);
begin
  actDebugViewState.Enabled := (ActiveKmnKeyboardEditor <> nil) and ActiveKmnKeyboardEditor.IsDebugVisible;
end;

procedure TmodActionsDebugger.actDebugViewElementsExecute(Sender: TObject);
begin
  ActiveKmnKeyboardEditor.DebugStatusForm.Visible := True;
  ActiveKmnKeyboardEditor.DebugStatusForm.pagesDebug.ActivePage := ActiveKmnKeyboardEditor.DebugStatusForm.tabDebugStores;
end;

procedure TmodActionsDebugger.actDebugViewElementsUpdate(Sender: TObject);
begin
  actDebugViewElements.Enabled := (ActiveKmnKeyboardEditor <> nil) and ActiveKmnKeyboardEditor.IsDebugVisible;
end;

{ ---- Keyboard Menu ---- }

function TmodActionsDebugger.IsDebuggerInTestMode: Boolean;
begin
  Result := ((ActiveKmnKeyboardEditor <> nil) and (ActiveKmnKeyboardEditor.DebugForm.UIStatus = duiTest)) or
    (ActiveLdmlKeyboardEditor <> nil);
end;

function TmodActionsDebugger.IsDebuggerVisible: Boolean;
begin
  Result := ((ActiveKmnKeyboardEditor <> nil) and ActiveKmnKeyboardEditor.IsDebugVisible) or
    ((ActiveLdmlKeyboardEditor <> nil) and ActiveLdmlKeyboardEditor.IsDebugVisible);
end;

procedure TmodActionsDebugger.SelectDebugSystemKeyboard(k: TSystemKeyboardItem);
begin
//TODO: #1225, #1074 (note both menu item and toolbar combo are hidden while this is not working)
//  if IsDebuggerVisible then   // I3655
//    ActiveKmnKeyboardEditor.DebugForm.SetSystemKeyboardID(k.KeyboardID);
end;

end.
