(*
  Name:             Keyman.Developer.UI.Project.kvkProjectFileUI
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
unit Keyman.Developer.UI.Project.kvkProjectFileUI;

interface

uses
  System.SysUtils,
  Vcl.Menus,

  Keyman.Developer.System.Project.kvkProjectFile,
  Keyman.Developer.System.Project.ProjectFile,
  Keyman.Developer.UI.Project.ProjectFileUI,
  Keyman.Developer.UI.Project.ProjectFilesUI,
  Keyman.Developer.UI.Project.ProjectUIFileType,
  Keyman.Developer.System.Project.ProjectFileType,
  UfrmMessages,
  UfrmEditor;

type
  TkvkProjectFileUI = class(TOpenableProjectFileUI)
  private
    FDebug: Boolean;
    function GetProjectFile: TkvkProjectFile;
  public
    function DoAction(action: TProjectFileAction; FSilent: Boolean): Boolean; override;
    property Debug: Boolean read FDebug write FDebug;
    property ProjectFile: TkvkProjectFile read GetProjectFile;
  end;

type
  TkvksProjectFileUI = class(TOpenableProjectFileUI)
  private
    FDebug: Boolean;
    function GetProjectFile: TkvksProjectFile;
  public
    function DoAction(action: TProjectFileAction; FSilent: Boolean): Boolean; override;
    property Debug: Boolean read FDebug write FDebug;
    property ProjectFile: TkvksProjectFile read GetProjectFile;
  end;

implementation

function TkvkProjectFileUI.DoAction(action: TProjectFileAction; FSilent: Boolean): Boolean;
begin
  Result := False;
end;

function TkvkProjectFileUI.GetProjectFile: TkvkProjectFile;
begin
  Result := FOwner as TkvkProjectFile;
end;

function TkvksProjectFileUI.DoAction(action: TProjectFileAction; FSilent: Boolean): Boolean;
begin
  Result := False;
end;

function TkvksProjectFileUI.GetProjectFile: TkvksProjectFile;
begin
  Result := FOwner as TkvksProjectFile;
end;

initialization
  RegisterProjectFileUIType(TkvkProjectFile, TkvkProjectFileUI);
  RegisterProjectFileUIType(TkvksProjectFile, TkvksProjectFileUI);
end.
