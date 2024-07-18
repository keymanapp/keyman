(*
  Name:             Keyman.Developer.UI.Project.ProjectFilesUI
  Copyright:        Copyright (C) SIL International.
  Documentation:    
  Description:      
  Create Date:      4 May 2015

  Modified Date:    12 May 2015
  Authors:          mcdurdin
  Related Files:    
  Dependencies:     

  Bugs:             
  Todo:             
  Notes:            
  History:          04 May 2015 - mcdurdin - I4694 - V9.0 - Split UI actions from non-UI actions in projects
                    12 May 2015 - mcdurdin - I4708 - CrashID:tike.exe_9.0.500.0_00967F1C_EInvalidCast
*)
unit Keyman.Developer.UI.Project.ProjectFilesUI;

interface

uses
  System.UITypes,
  System.SysUtils,
  System.Classes,
  Vcl.Menus,
  Winapi.ShellApi,
  Winapi.Windows,

  Keyman.Developer.System.Project.ProjectFile,
  Keyman.Developer.System.Project.ProjectFiles,
  Keyman.Developer.UI.Project.ProjectFileUI,
  UfrmMDIChild;

type
  TShellProjectFileUI = class(TProjectFileUI)
  private
    function GetProjectFile: TShellProjectFile;
  protected
    procedure OpenFile; override;
  public
    procedure NewFile; override;
    procedure DefaultEvent(Sender: TObject); override;

    function DoAction(action: TProjectFileAction; FSilent: Boolean): Boolean; override;

    property ProjectFile: TShellProjectFile read GetProjectFile;
  end;

  TOpenableProjectFileUI = class(TShellProjectFileUI)
  private
    function GetProjectFile: TOpenableProjectFile;
  protected
    FMDIChild: TfrmTikeChild;
    procedure CloseFile(Sender: TObject); virtual;
    procedure OpenFile; override;
    function WindowOpen: Boolean; override;
  public
    destructor Destroy; override;
    property MDIChild: TfrmTikeChild read FMDIChild write FMDIChild;
    property ProjectFile: TOpenableProjectFile read GetProjectFile;
  end;

implementation

uses
  Vcl.Controls,
  Vcl.Dialogs,

  Keyman.Developer.System.Project.ProjectFileType,
  Keyman.Developer.UI.Project.ProjectUIFileType,
  UfrmMain,
  UfrmMDIEditor,
  utilexecute;

function TShellProjectFileUI.DoAction(action: TProjectFileAction; FSilent: Boolean): Boolean;
begin
  Result := True;
end;

procedure TShellProjectFileUI.DefaultEvent(Sender: TObject);
begin
  if WindowOpen then
    OpenFile
  else if not FileExists(ProjectFile.FileName) then
  begin
    if MessageDlg('The file ''' + ProjectFile.FileName + ''' does not exist.  Do you want to create it now?',
      mtConfirmation, mbOkCancel, 0) = mrCancel then Exit;
    NewFile;
  end
  else
    OpenFile;
end;

procedure TShellProjectFileUI.NewFile;
var
  f: File;
begin
  if not FileExists(ProjectFile.FileName) then
  try
    AssignFile(f, ProjectFile.FileName);
    Rewrite(f);
    CloseFile(f);
    System.SysUtils.DeleteFile(ProjectFile.FileName);
  except
    on E:Exception do
    begin
      ShowMessage('Cannot create file '''+ProjectFile.FileName+''': '+ E.Message);
      Exit;
    end;
  end;
  if Assigned(ProjectFile.Project) then ProjectFile.Project.Refresh;
  OpenFile;
end;

procedure TShellProjectFileUI.OpenFile;
begin
  if not TUtilExecute.Shell(0, ProjectFile.FileName, ExtractFileDir(ProjectFile.FileName)) then  // I3349
    ShowMessage(SysErrorMessage(GetLastError));
end;

procedure TOpenableProjectFileUI.CloseFile(Sender: TObject);
begin
  if Sender = FMDIChild
    then FMDIChild := nil;
  if FOwner <> nil then   // I4708
    ProjectFile.Modified := False;
end;

destructor TOpenableProjectFileUI.Destroy;
begin
  if Assigned(FMDIChild) then
  begin
    FMDIChild.ProjectFile := nil;
    FMDIChild.OnCloseFile := nil;
  end;
  inherited Destroy;
end;

function TOpenableProjectFileUI.GetProjectFile: TOpenableProjectFile;
begin
  Result := FOwner as TOpenableProjectFile;
end;

procedure TOpenableProjectFileUI.OpenFile;
begin
  FMDIChild := frmKeymanDeveloper.OpenFile(ProjectFile.FileName, False);
  if Assigned(FMDIChild) then
  begin
    FMDIChild.OnCloseFile := CloseFile;
    FMDIChild.ProjectFile := ProjectFile;
  end;
end;

function TOpenableProjectFileUI.WindowOpen: Boolean;
var
  i: Integer;
begin
  with frmKeymanDeveloper do
    for i := 0 to ChildWindows.Count - 1 do
      if ChildWindows[i] is TfrmTikeEditor then
        with ChildWindows[i] as TfrmTikeEditor do
          if FileName = ProjectFile.FileName then
          begin
            Result := True;
            Exit;
          end;
  Result := False;
end;

function TShellProjectFileUI.GetProjectFile: TShellProjectFile;
begin
  Result := FOwner as TShellProjectFile;
end;

initialization
  RegisterProjectFileUIType(TShellProjectFile, TShellProjectFileUI);
  RegisterProjectFileUIType(TOpenableProjectFile, TOpenableProjectFileUI);
end.
