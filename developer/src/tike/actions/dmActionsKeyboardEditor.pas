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
  SysUtils,
  Classes,
  ActnList,
  UfrmSelectSystemKeyboard,
  ImgList,
  Controls,
  Dialogs,
  System.Actions;

type
  TmodActionsKeyboardEditor = class(TDataModule)
    actionsKeyboardEditor: TActionList;
    actKeyboardCompile: TAction;
    actKeyboardIncludeDebugInformation: TAction;
    actKeyboardInstall: TAction;
    actKeyboardUninstall: TAction;
    actKeyboardTest: TAction;
    actKeyboardTestKeymanWeb: TAction;
    actKeyboardFontHelper: TAction;
    actKeyboardFonts: TAction;
    procedure actKeyboardCompileExecute(Sender: TObject);
    procedure actKeyboardIncludeDebugInformationExecute(Sender: TObject);
    procedure actKeyboardInstallExecute(Sender: TObject);
    procedure actKeyboardUninstallExecute(Sender: TObject);
    procedure actKeyboardIncludeDebugInformationUpdate(Sender: TObject);
    procedure actKeyboardTestExecute(Sender: TObject);
    procedure actKeyboardTestKeymanWebExecute(Sender: TObject);
    procedure actKeyboardFontHelperExecute(Sender: TObject);
    procedure actKeyboardFontsExecute(Sender: TObject);
    procedure actKeyboardCompileUpdate(Sender: TObject);
    procedure actKeyboardInstallUpdate(Sender: TObject);
    procedure actKeyboardUninstallUpdate(Sender: TObject);
    procedure actKeyboardTestUpdate(Sender: TObject);
    procedure actKeyboardTestKeymanWebUpdate(Sender: TObject);
    procedure actKeyboardFontHelperUpdate(Sender: TObject);
    procedure actKeyboardFontsUpdate(Sender: TObject);
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
  if ActiveLdmlKeyboardEditor = nil
    then Result := nil
    else Result := ActiveLdmlKeyboardEditor.ProjectFile as TxmlLdmlProjectFile;
end;

function ActiveKmnKeyboardProjectFile: TkmnProjectFile;
begin
  if ActiveKmnKeyboardEditor = nil
    then Result := nil
    else Result := ActiveKmnKeyboardEditor.ProjectFile as TkmnProjectFile;
end;

function ActivePackageProjectFile: TkpsProjectFile;
begin
  if ActivePackageEditor = nil
    then Result := nil
    else Result := ActivePackageEditor.ProjectFile as TkpsProjectFile;
end;

function ActiveProjectFileUI: TProjectFileUI;
begin
  if ActiveKmnKeyboardProjectFile <> nil then
    Result := ActiveKmnKeyboardProjectFile.UI as TProjectFileUI
  else if ActiveLdmlKeyboardProjectFile <> nil then
    Result := ActiveLdmlKeyboardProjectFile.UI as TProjectFileUI
  else if ActivePackageProjectFile <> nil then
    Result := ActivePackageProjectFile.UI as TProjectFileUI
  else
    Result := nil;
end;

{ ---- Keyboard Menu ---- }

procedure TmodActionsKeyboardEditor.actKeyboardCompileUpdate(Sender: TObject);
begin
  // TODO: Split Keyboard menu and package editor functions
  actKeyboardCompile.Enabled :=
    (ActiveKmnKeyboardEditor <> nil) or
    (ActiveLdmlKeyboardEditor <> nil) or
    (ActivePackageEditor <> nil) or
    ((frmKeymanDeveloper.ActiveChild is TfrmProject) and (FGlobalProject <> nil));

  if actKeyboardCompile.Enabled
    then actKeyboardCompile.ShortCut := Vcl.Menus.Shortcut(VK_F7, [])
    else actKeyboardCompile.ShortCut := scNone;

  // This is a little side-effecty
  frmKeymanDeveloper.mnuKeyboard.Visible := True;
  frmKeymanDeveloper.mnuDebug.Visible := ActiveKmnKeyboardEditor <> nil;
end;

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

    if ActiveProjectFileUI.DoAction(pfaCompile, False) and DebugReset then   // I4686
      ActiveKmnKeyboardEditor.StartDebugging;
  end
  else if ActiveLdmlKeyboardEditor <> nil then
  begin
    if not ActiveLdmlKeyboardEditor.PrepareForBuild(DebugReset) then
      Exit;

    if ActiveProjectFileUI.DoAction(pfaCompile, False) and DebugReset then
      ActiveLdmlKeyboardEditor.StartDebugging;
  end
  else if ActivePackageEditor <> nil then
  begin
    ActiveProjectFileUI.DoAction(pfaCompile, False);
  end
  else if frmKeymanDeveloper.ActiveChild is TfrmProject then
  begin
    (frmKeymanDeveloper.ActiveChild as TfrmProject).CompileAll;
  end;
end;

//------------------------------------------------------------------------------

procedure TmodActionsKeyboardEditor.actKeyboardFontHelperUpdate(
  Sender: TObject);
begin
   actKeyboardFontHelper.Enabled := (ActiveKmnKeyboardProjectFile <> nil);
end;

procedure TmodActionsKeyboardEditor.actKeyboardFontHelperExecute(
  Sender: TObject);
begin
  ActiveProjectFileUI.DoAction(pfaFontHelper, False)   // I4687
end;

//------------------------------------------------------------------------------

procedure TmodActionsKeyboardEditor.actKeyboardFontsUpdate(Sender: TObject);
begin
   actKeyboardFonts.Enabled := (ActiveKmnKeyboardProjectFile <> nil);
end;

procedure TmodActionsKeyboardEditor.actKeyboardFontsExecute(Sender: TObject);   // I4057
begin
  ActiveProjectFileUI.DoAction(pfaFontDialog, False);   // I4687
end;

//------------------------------------------------------------------------------

procedure TmodActionsKeyboardEditor.actKeyboardIncludeDebugInformationUpdate(
  Sender: TObject);
begin
  actKeyboardIncludeDebugInformation.Enabled := ActiveKmnKeyboardEditor <> nil;
  if (ActiveKmnKeyboardEditor <> nil) and (ActiveKmnKeyboardProjectFile <> nil)
    then actKeyboardIncludeDebugInformation.Checked := (ActiveProjectFileUI as TkmnProjectFileUI).Debug   // I4687
    else actKeyboardIncludeDebugInformation.Checked := False;
end;

procedure TmodActionsKeyboardEditor.actKeyboardIncludeDebugInformationExecute(Sender: TObject);
begin
  (ActiveProjectFileUI as TkmnProjectFileUI).Debug := not (ActiveProjectFileUI as TkmnProjectFileUI).Debug;   // I4687
end;

//------------------------------------------------------------------------------

procedure TmodActionsKeyboardEditor.actKeyboardInstallUpdate(Sender: TObject);
begin
  actKeyboardInstall.Enabled := ActiveProjectFileUI <> nil;
end;

procedure TmodActionsKeyboardEditor.actKeyboardInstallExecute(Sender: TObject);
begin
  try
    ActiveProjectFileUI.DoAction(pfaInstall, False);   // I4687
  except
    on E:EOleException do // I654
    begin
      ShowMessage(E.Message);
    end;
  end;
end;

//------------------------------------------------------------------------------

procedure TmodActionsKeyboardEditor.actKeyboardTestUpdate(Sender: TObject);
begin
  actKeyboardTest.Enabled := (ActiveKmnKeyboardEditor <> nil) or (ActiveLdmlKeyboardEditor <> nil);
  // TODO: test xml keyboards in editor
end;

procedure TmodActionsKeyboardEditor.actKeyboardTestExecute(Sender: TObject);
begin
  if ActiveKmnKeyboardEditor <> nil then
  begin
    ActiveKmnKeyboardEditor.DebugForm.UIStatus := duiTest;
    ActiveKmnKeyboardEditor.StartDebugging(True);
  end
  else
  begin
    ActiveLdmlKeyboardEditor.StartDebugging;
  end;
end;

//------------------------------------------------------------------------------

procedure TmodActionsKeyboardEditor.actKeyboardTestKeymanWebUpdate(
  Sender: TObject);
begin
  actKeyboardTestKeymanWeb.Enabled := (ActiveKmnKeyboardEditor <> nil);
end;

procedure TmodActionsKeyboardEditor.actKeyboardTestKeymanWebExecute(
  Sender: TObject);
begin
  ActiveProjectFileUI.DoAction(pfaTestKeymanWeb, False);   // I4687
end;

//------------------------------------------------------------------------------

procedure TmodActionsKeyboardEditor.actKeyboardUninstallUpdate(Sender: TObject);
begin
  actKeyboardUninstall.Enabled := ActiveProjectFileUI <> nil;
end;

procedure TmodActionsKeyboardEditor.actKeyboardUninstallExecute(Sender: TObject);
begin
  try
    if ActiveProjectFileUI.DoAction(pfaUninstall, False)
      then ShowMessage('Keyboard uninstalled successfully.')
      else ShowMessage('Failed to uninstall keyboard.');
  except
    on E:EOleException do // I654
    begin
      ShowMessage(E.Message);
    end;
  end;
end;

end.
