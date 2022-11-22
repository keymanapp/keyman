(*
  Name:             Keyman.Developer.UI.Project.kmxProjectFileUI
  Copyright:        Copyright (C) 2003-2017 SIL International.
  Documentation:    
  Description:      
  Create Date:      4 May 2015

  Modified Date:    4 May 2015
  Authors:          mcdurdin
  Related Files:    
  Dependencies:     

  Bugs:             
  Todo:             
  Notes:            
  History:          04 May 2015 - mcdurdin - I4694 - V9.0 - Split UI actions from non-UI actions in projects
*)
unit Keyman.Developer.UI.Project.kmxProjectFileUI;

interface

uses
  System.SysUtils,
  Vcl.Menus,

  Keyman.Developer.System.Project.kmxProjectFile,
  Keyman.Developer.System.Project.ProjectFile,
  Keyman.Developer.UI.Project.ProjectFileUI,
  Keyman.Developer.UI.Project.ProjectFilesUI,
  Keyman.Developer.System.Project.ProjectFileType,
  UfrmMessages,
  UfrmEditor;

type
  TkmxProjectFileUI = class(TOpenableProjectFileUI)
  private
    function InstallKeyboard: Boolean;
    function UninstallKeyboard: Boolean;
    function GetProjectFile: TkmxProjectFile;
  public
    procedure DefaultEvent(Sender: TObject); override;
    function DoAction(action: TProjectFileAction; FSilent: Boolean): Boolean; override;
    property ProjectFile: TkmxProjectFile read GetProjectFile;
  end;

implementation

uses
  KeymanDeveloperUtils,
  Keyman.Developer.UI.Project.ProjectUIFileType,
  UfrmMain;

procedure TkmxProjectFileUI.DefaultEvent(Sender: TObject);
begin
  InstallKeyboard;
end;

function TkmxProjectFileUI.DoAction(action: TProjectFileAction; FSilent: Boolean): Boolean;
begin
  case action of
    pfaInstall: Result := InstallKeyboard;
    pfaUninstall: Result := UninstallKeyboard;
  else
    Result := False;
  end;
end;

function TkmxProjectFileUI.GetProjectFile: TkmxProjectFile;
begin
  Result := FOwner as TkmxProjectFile;
end;

function TkmxProjectFileUI.InstallKeyboard: Boolean;
begin
  KeymanDeveloperUtils.InstallKeyboard(ProjectFile.FileName, False);
  Result := True;
end;

function TkmxProjectFileUI.UninstallKeyboard: Boolean;
begin
  KeymanDeveloperUtils.UninstallKeyboard(ChangeFileExt(ExtractFileName(ProjectFile.FileName), ''));
  Result := True;
end;

initialization
  RegisterProjectFileUIType(TkmxProjectFile, TkmxProjectFileUI);
end.
