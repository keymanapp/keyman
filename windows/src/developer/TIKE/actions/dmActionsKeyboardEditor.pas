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
    actKeyboardCreatePackage: TAction;
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
    procedure actKeyboardCompileExecute(Sender: TObject);
    procedure actionsKeyboardEditorUpdate(Action: TBasicAction;
      var Handled: Boolean);
    procedure actKeyboardIncludeDebugInformationExecute(Sender: TObject);
    procedure actKeyboardCreatePackageExecute(Sender: TObject);
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
  ComObj,
  Forms,
  kmnProjectFile,
  kmnProjectFileUI,
  KeymanDeveloperMemo,
  ProjectFile,
  ProjectFileUI,
  UfrmKeymanWizard,
  UfrmDebug,
  UfrmDebugStatus,
  UfrmMessages,
  UfrmMain,
  UKeymanTargets;

{$R *.dfm}

function ActiveEditor: TfrmKeymanWizard;
begin
  if Assigned(frmKeymanDeveloper.ActiveChild) and (frmKeymanDeveloper.ActiveChild is TfrmKeymanWizard)
    then Result := frmKeymanDeveloper.ActiveChild as TfrmKeymanWizard
    else Result := nil;
end;

function ActiveProjectFile: TkmnProjectFile;
begin
  Result := ActiveEditor.ProjectFile as TkmnProjectFile;
end;

function ActiveMemo: TKeymanDeveloperMemo;
begin
  if Screen.ActiveControl is TKeymanDeveloperMemo
    then Result := Screen.ActiveControl as TKeymanDeveloperMemo
    else Result := nil;
end;

procedure TmodActionsKeyboardEditor.actionsKeyboardEditorUpdate(Action: TBasicAction; var Handled: Boolean);
begin
  with Action as TAction do
  begin
    Enabled := ActiveEditor <> nil;
    if not Enabled then Handled := True;
  end;
end;

{ ---- Debug Menu ---- }

procedure TmodActionsKeyboardEditor.actDebugDebuggerModeExecute(Sender: TObject);
begin
  if ActiveEditor.DebugForm.UIStatus = duiTest then
    ActiveEditor.DebugForm.UIStatus := duiReadyForInput;
end;

procedure TmodActionsKeyboardEditor.actDebugDebuggerModeUpdate(Sender: TObject);
begin
  actDebugDebuggerMode.Enabled := (ActiveEditor <> nil) and (ActiveEditor.DebugForm.CanDebug);
  actDebugDebuggerMode.Checked := actDebugDebuggerMode.Enabled and (ActiveEditor.DebugForm.UIStatus <> duiTest);
end;

procedure TmodActionsKeyboardEditor.actDebugPauseExecute(Sender: TObject);
begin
  if ActiveEditor.DebugForm.UIStatus = duiPaused
    then ActiveEditor.DebugForm.Unpause
    else ActiveEditor.DebugForm.Pause;
end;

procedure TmodActionsKeyboardEditor.actDebugPauseUpdate(Sender: TObject);
begin
  actDebugPause.Enabled :=
    (ActiveEditor <> nil) and
    IsDebuggerVisible and
    not IsDebuggerInTestMode and
    (ActiveEditor.DebugForm.UIStatus in [duiPaused, duiFocusedForInput, duiReadyForInput]);

  if actDebugPause.Enabled
    then actDebugPause.Checked := ActiveEditor.DebugForm.UIStatus = duiPaused
    else actDebugPause.Checked := False;
end;

procedure TmodActionsKeyboardEditor.actDebugRunExecute(Sender: TObject);
begin
  ActiveEditor.DebugForm.Run;
end;

procedure TmodActionsKeyboardEditor.actDebugRunUpdate(Sender: TObject);
begin
  actDebugRun.Enabled := IsDebuggerVisible and not IsDebuggerInTestMode and 
    (ActiveEditor.DebugForm.UIStatus = duiDebugging);
end;

procedure TmodActionsKeyboardEditor.actDebugSelectSystemKeyboardExecute(Sender: TObject);
var
  FKeyboardID: string;
begin
  if SelectSystemKeyboard(frmKeymanDeveloper, FKeyboardID) then
  begin
    //ActiveEditor.DebugForm.SetSystemKeyboardID(FKeyboardID);   // I3655
  end;
end;

procedure TmodActionsKeyboardEditor.actDebugSelectSystemKeyboardUpdate(Sender: TObject);
begin
  actDebugSelectSystemKeyboard.Enabled := IsDebuggerVisible;
end;

procedure TmodActionsKeyboardEditor.actDebugSetClearBreakpointExecute(Sender: TObject);
begin
  if ActiveEditor.DebugForm.IsBreakPointLine(ActiveMemo.SelPar)
    then ActiveEditor.DebugForm.ClearBreakpoint(ActiveMemo.SelPar)
    else ActiveEditor.DebugForm.SetBreakpoint(ActiveMemo.SelPar);
end;

procedure TmodActionsKeyboardEditor.actDebugSetClearBreakpointUpdate(Sender: TObject);
begin
  if (ActiveEditor <> nil) and (ActiveMemo <> nil) and Assigned(ActiveEditor.DebugForm) then
  begin
    actDebugSetClearBreakpoint.Enabled := True;
    actDebugSetClearBreakpoint.Visible := True;
    if ActiveEditor.DebugForm.IsBreakPointLine(ActiveMemo.SelPar)
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
  ActiveEditor.DebugForm.SingleStepMode := not ActiveEditor.DebugForm.SingleStepMode;
end;

procedure TmodActionsKeyboardEditor.actDebugSingleStepModeUpdate(Sender: TObject);
begin
  actDebugSingleStepMode.Enabled := IsDebuggerVisible and not IsDebuggerInTestMode;
  if actDebugSingleStepMode.Enabled
    then actDebugSingleStepMode.Checked := ActiveEditor.DebugForm.SingleStepMode
    else actDebugSingleStepMode.Checked := False;
end;

procedure TmodActionsKeyboardEditor.actDebugStartDebuggerExecute(Sender: TObject);
begin
  ActiveEditor.StartDebugging;
end;

procedure TmodActionsKeyboardEditor.actDebugStartDebuggerUpdate(Sender: TObject);
begin
  actDebugStartDebugger.Enabled := (ActiveEditor <> nil) and not ActiveEditor.IsDebugVisible;
end;

procedure TmodActionsKeyboardEditor.actDebugStepForwardExecute(Sender: TObject);
begin
  ActiveEditor.DebugForm.StepForward;
end;

procedure TmodActionsKeyboardEditor.actDebugStepForwardUpdate(Sender: TObject);
begin
  actDebugStepForward.Enabled := IsDebuggerVisible and not IsDebuggerInTestMode  and
    (ActiveEditor.DebugForm.UIStatus = duiDebugging);
end;

procedure TmodActionsKeyboardEditor.actDebugStopDebuggerExecute(
  Sender: TObject);
begin
  ActiveEditor.StopDebugging;
end;

procedure TmodActionsKeyboardEditor.actDebugStopDebuggerUpdate(Sender: TObject);
begin
  actDebugStopDebugger.Enabled := (ActiveEditor <> nil) and ActiveEditor.IsDebugVisible;
end;

procedure TmodActionsKeyboardEditor.actDebugTestModeExecute(Sender: TObject);
begin
  ActiveEditor.DebugForm.UIStatus := duiTest;
end;

procedure TmodActionsKeyboardEditor.actDebugTestModeUpdate(Sender: TObject);
begin
  actDebugTestMode.Enabled := (ActiveEditor <> nil);
  actDebugTestMode.Checked := actDebugTestMode.Enabled and (ActiveEditor.DebugForm.UIStatus = duiTest);
end;

procedure TmodActionsKeyboardEditor.actDebugViewCallStackExecute(Sender: TObject);
begin
  ActiveEditor.DebugStatusForm.Visible := True;
  ActiveEditor.DebugStatusForm.pagesDebug.ActivePage := ActiveEditor.DebugStatusForm.tabDebugCallStack;
end;

procedure TmodActionsKeyboardEditor.actDebugViewCallStackUpdate(
  Sender: TObject);
begin
  actDebugViewCallStack.Enabled := (ActiveEditor <> nil) and ActiveEditor.IsDebugVisible;
end;

procedure TmodActionsKeyboardEditor.actDebugViewDeadkeysExecute(Sender: TObject);
begin
  ActiveEditor.DebugStatusForm.Visible := True;
  ActiveEditor.DebugStatusForm.pagesDebug.ActivePage := ActiveEditor.DebugStatusForm.tabDebugDeadkeys;
end;

procedure TmodActionsKeyboardEditor.actDebugViewDeadkeysUpdate(Sender: TObject);
begin
  actDebugViewDeadkeys.Enabled := (ActiveEditor <> nil) and ActiveEditor.IsDebugVisible;
end;

procedure TmodActionsKeyboardEditor.actDebugViewDefaultFontExecute(Sender: TObject);
begin
  ActiveEditor.DebugForm.UpdateFont(nil);
end;

procedure TmodActionsKeyboardEditor.actDebugViewDefaultFontUpdate(Sender: TObject);
begin
  actDebugViewDefaultFont.Enabled := IsDebuggerVisible;
  actDebugViewDefaultFont.Checked := actDebugViewDefaultFont.Enabled and
    ActiveEditor.DebugForm.DefaultFont;
end;

procedure TmodActionsKeyboardEditor.actDebugViewFontExecute(Sender: TObject);
begin
  dlgFont.Font := ActiveEditor.DebugForm.memo.Font;
  if dlgFont.Execute then
    ActiveEditor.DebugForm.UpdateFont(dlgFont.Font);
end;

procedure TmodActionsKeyboardEditor.actDebugViewFontUpdate(Sender: TObject);
begin
  actDebugViewFont.Enabled := IsDebuggerVisible;
  actDebugViewFont.Checked := actDebugViewFont.Enabled and not ActiveEditor.DebugForm.DefaultFont;
end;

procedure TmodActionsKeyboardEditor.actDebugViewRegressionTestingExecute(Sender: TObject);
begin
  ActiveEditor.DebugStatusForm.Visible := True;
  ActiveEditor.DebugStatusForm.pagesDebug.ActivePage := ActiveEditor.DebugStatusForm.tabDebugRegressionTesting;
end;

procedure TmodActionsKeyboardEditor.actDebugViewRegressionTestingUpdate(Sender: TObject);
begin
  actDebugViewRegressionTesting.Enabled := (ActiveEditor <> nil) and ActiveEditor.IsDebugVisible;
end;

procedure TmodActionsKeyboardEditor.actDebugViewStateExecute(Sender: TObject);
begin
  ActiveEditor.DebugStatusForm.Visible := True;
  ActiveEditor.DebugStatusForm.pagesDebug.ActivePage := ActiveEditor.DebugStatusForm.tabDebugKey;
end;

procedure TmodActionsKeyboardEditor.actDebugViewStateUpdate(Sender: TObject);
begin
  actDebugViewState.Enabled := (ActiveEditor <> nil) and ActiveEditor.IsDebugVisible;
end;

procedure TmodActionsKeyboardEditor.actDebugViewElementsExecute(Sender: TObject);
begin
  ActiveEditor.DebugStatusForm.Visible := True;
  ActiveEditor.DebugStatusForm.pagesDebug.ActivePage := ActiveEditor.DebugStatusForm.tabDebugStores;
end;

procedure TmodActionsKeyboardEditor.actDebugViewElementsUpdate(Sender: TObject);
begin
  actDebugViewElements.Enabled := (ActiveEditor <> nil) and ActiveEditor.IsDebugVisible;
end;

{ ---- Keyboard Menu ---- }

procedure TmodActionsKeyboardEditor.actKeyboardCompileExecute(Sender: TObject);   // I4504
var
  DebugReset: Boolean;
begin
  DebugReset := False;

  frmMessages.Clear;   // I4686

  if not ActiveEditor.PrepareForBuild(DebugReset) then
    Exit;

  if (ActiveProjectFile.UI as TProjectFileUI).DoAction(pfaCompile, False) and DebugReset then   // I4686
    ActiveEditor.StartDebugging;
end;

procedure TmodActionsKeyboardEditor.actKeyboardCreatePackageExecute(Sender: TObject);
begin
  (ActiveProjectFile.UI as TProjectFileUI).DoAction(pfaPackage, False);   // I4687
end;

procedure TmodActionsKeyboardEditor.actKeyboardFontHelperExecute(
  Sender: TObject);
begin
  (ActiveProjectFile.UI as TProjectFileUI).DoAction(pfaFontHelper, False)   // I4687
end;

procedure TmodActionsKeyboardEditor.actKeyboardFontsExecute(Sender: TObject);   // I4057
begin
  (ActiveProjectFile.UI as TProjectFileUI).DoAction(pfaFontDialog, False);   // I4687
end;

procedure TmodActionsKeyboardEditor.actKeyboardIncludeDebugInformationExecute(Sender: TObject);
begin
  (ActiveProjectFile.UI as TkmnProjectFileUI).Debug := not (ActiveProjectFile.UI as TkmnProjectFileUI).Debug;   // I4687
end;

procedure TmodActionsKeyboardEditor.actKeyboardIncludeDebugInformationUpdate(
  Sender: TObject);
begin
  if ActiveProjectFile <> nil
    then actKeyboardIncludeDebugInformation.Checked := (ActiveProjectFile.UI as TkmnProjectFileUI).Debug   // I4687
    else actKeyboardIncludeDebugInformation.Checked := False;
end;

procedure TmodActionsKeyboardEditor.actKeyboardInstallExecute(Sender: TObject);
begin
  try
    (ActiveProjectFile.UI as TProjectFileUI).DoAction(pfaInstall, False);   // I4687
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
  ActiveEditor.DebugForm.UIStatus := duiTest;
  ActiveEditor.StartDebugging(True);
end;

procedure TmodActionsKeyboardEditor.actKeyboardTestKeymanWebExecute(
  Sender: TObject);
begin
  (ActiveProjectFile.UI as TProjectFileUI).DoAction(pfaTestKeymanWeb, False);   // I4687
end;

procedure TmodActionsKeyboardEditor.actKeyboardUninstallExecute(Sender: TObject);
begin
  try
    (ActiveProjectFile.UI as TProjectFileUI).DoAction(pfaUninstall, False);   // I4687
    ShowMessage('Keyboard uninstalled successfully.');
  except
    on E:EOleException do // I654
    begin
      ShowMessage(E.Message);
    end;
  end;
end;

function TmodActionsKeyboardEditor.IsDebuggerInTestMode: Boolean;
begin
  Result := (ActiveEditor <> nil) and (ActiveEditor.DebugForm.UIStatus = duiTest);
end;

function TmodActionsKeyboardEditor.IsDebuggerVisible: Boolean;
begin
  Result := (ActiveEditor <> nil) and ActiveEditor.IsDebugVisible;
end;

procedure TmodActionsKeyboardEditor.SelectDebugSystemKeyboard(k: TSystemKeyboardItem);
begin
//  if IsDebuggerVisible then   // I3655
//    ActiveEditor.DebugForm.SetSystemKeyboardID(k.KeyboardID);
end;

end.
