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
  UfrmMain,
  UfrmMDIEditor,
  KeyboardFonts,
  KeymanDeveloperUtils,
  KeymanDeveloperOptions,
  System.Classes,
  System.Variants,
  utilsystem;

function TmodelTsProjectFileUI.DoAction(action: TProjectFileAction; FSilent: Boolean): Boolean;
begin
  case action of
    pfaCompile:   Result := CompileModel(FSilent);
    pfaClean:      Result := ProjectFile.Clean;
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

initialization
  RegisterProjectFileUIType(TmodelTsProjectFileAction, TmodelTsProjectFileUI);
end.




