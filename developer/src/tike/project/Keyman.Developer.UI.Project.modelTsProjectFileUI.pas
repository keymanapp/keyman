(*
  Name:             Keyman.Developer.UI.Project.kmnProjectFileUI
  Copyright:        Copyright (C) 2003-2019 SIL International.
  Documentation:
  Description:
  Create Date:      22 August 2019

  Modified Date:    22 August 2019
  Authors:          mcdurdin
  Related Files:
  Dependencies:

  Bugs:
  Todo:
  Notes:
  History:
*)
unit Keyman.Developer.UI.Project.modelTsProjectFileUI;

interface

uses
  System.UITypes,
  Menus,
  Keyman.Developer.UI.Project.ProjectFilesUI,
  Keyman.Developer.UI.Project.ProjectFileUI,
  Keyman.Developer.UI.Project.ProjectUIFileType,
  UfrmMessages,
  Keyman.Developer.System.Project.modelTsProjectFile,
  Keyman.Developer.System.Project.modelTsProjectFileAction;

type
  TmodelTsProjectFileUI = class(TOpenableProjectFileUI)
  private
    function GetProjectFile: TmodelTsProjectFileAction;

    function GetDebug: Boolean;
    procedure SetDebug(const Value: Boolean);
    function CompileModel(FSilent: Boolean): Boolean;
    function TestKeymanWeb(FSilent: Boolean): Boolean;
    function TestModelState(FCompiledName: string; FSilent: Boolean): Boolean;
  public
    function DoAction(action: TProjectFileAction; FSilent: Boolean): Boolean; override;
    property Debug: Boolean read GetDebug write SetDebug;
    property ProjectFile: TmodelTsProjectFileAction read GetProjectFile;
  end;

implementation

uses
  Winapi.Windows,
  System.SysUtils,
  Vcl.Dialogs,
  Vcl.Graphics,
  Vcl.Controls,

  dmActionsMain,
  KeyboardFonts,
  UmodWebHttpServer,
  UfrmMain,
  UfrmMDIEditor,
  Keyman.Developer.System.ServerAPI,
  Keyman.Developer.UI.ServerUI,
  Keyman.Developer.UI.UfrmModelEditor,
  KeymanDeveloperUtils,
  KeymanDeveloperOptions,
  System.Classes,
  System.Variants,
  utilsystem;

function TmodelTsProjectFileUI.DoAction(action: TProjectFileAction; FSilent: Boolean): Boolean;
begin
  case action of
    pfaCompile:       Result := CompileModel(FSilent);
    pfaClean:         Result := ProjectFile.Clean;
    pfaTestKeymanWeb: Result := TestKeymanWeb(FSilent);
  else
    Result := False;
  end;
end;

function TmodelTsProjectFileUI.CompileModel(FSilent: Boolean): Boolean;
begin
  Result := False;

  if ProjectFile.Modified then
  begin
    if not FSilent then
    begin
      if MessageDlg('The lexical model file has been modified.  You must save before compiling.'+#13#10+'Save the lexical model and continue?',
        mtConfirmation, mbOkCancel, 0) = mrCancel then Exit;
      if not modActionsMain.actFileSave.Execute then Exit;
    end
    else
      Exit;
  end;

  if not FSilent then
    frmMessages.DoShowForm;

  Result := ProjectFile.CompileModel;

  if Result and
      TServerDebugAPI.Running and
      TServerDebugAPI.IsModelRegistered(ProjectFile.TargetFileName) then
    TestKeymanWeb(True);
end;

function TmodelTsProjectFileUI.GetDebug: Boolean;
begin
  Result := ProjectFile.Debug;
end;

function TmodelTsProjectFileUI.GetProjectFile: TmodelTsProjectFileAction;
begin
  Result := FOwner as TmodelTsProjectFileAction;
end;

procedure TmodelTsProjectFileUI.SetDebug(const Value: Boolean);
begin
  ProjectFile.Debug := Value;
end;

function TmodelTsProjectFileUI.TestKeymanWeb(FSilent: Boolean): Boolean;
var
  FCompiledName: string;
  editor: TfrmTikeEditor;
  wizard: TfrmModelEditor;
begin
  editor := frmKeymanDeveloper.FindEditorByFileName(ProjectFile.FileName);   // I4021
  if not Assigned(editor) or not (editor is TfrmModelEditor) then
    Exit(False);
  wizard := editor as TfrmModelEditor;

  FCompiledName := ProjectFile.TargetFilename;
  if not TestModelState(FCompiledName, FSilent) then
    Exit(False);

  if TServerUI.VerifyServerRunning then
  begin
    if FileExists(ProjectFile.TestKeyboard) then
      TServerDebugAPI.RegisterKeyboard(ProjectFile.TestKeyboard, '1.0', '', '');
    TServerDebugAPI.RegisterModel(FCompiledName);

    wizard.NotifyStartedWebDebug;   // I4021
  end;

  Result := True;
end;

function TmodelTsProjectFileUI.TestModelState(FCompiledName: string;
  FSilent: Boolean): Boolean;
var
  ftts, ftjs: TDateTime;
begin
  Result := False;

  if not FileExists(FCompiledName) then
    if FSilent then
    begin
      if not CompileModel(FSilent) then Exit;
    end
    else
      case MessageDlg('You need to compile the model before you can continue.  Compile now?',
          mtConfirmation, mbOkCancel, 0) of
        mrOk:     if not CompileModel(FSilent) then Exit;
        mrCancel: Exit;
      end;

  FileAge(ProjectFile.FileName, ftts);
  FileAge(FCompiledName, ftjs);

  if ProjectFile.Modified or (ftts > ftjs) then
    if FSilent then
    begin
      if not CompileModel(FSilent) then Exit;
    end
    else
      case MessageDlg('The source file has changed.  Recompile before continuing?',
          mtConfirmation, mbYesNoCancel, 0) of
        mrYes:    if not CompileModel(FSilent) then Exit;
        mrNo:     ;
        mrCancel: Exit;
      end;

  Result := True;
end;

initialization
  RegisterProjectFileUIType(TmodelTsProjectFileAction, TmodelTsProjectFileUI);
end.




