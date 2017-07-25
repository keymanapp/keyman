(*
  Name:             kmxProjectFileUI
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
unit kmxProjectFileUI;

interface

uses
  System.SysUtils,
  Vcl.Menus,

  kmxProjectFile,
  ProjectFile,
  ProjectFileUI,
  ProjectFilesUI,
  ProjectFileType,
  UfrmMessages,
  UfrmEditor;

type
  TkmxProjectFileUI = class(TOpenableProjectFileUI)
  private
    procedure MenuEventTest(Sender: TObject);
    function TestKeyboard: Boolean;
    function InstallKeyboard: Boolean;
    procedure MenuEventInstall(Sender: TObject);
    procedure MenuEventUninstall(Sender: TObject);
    function UninstallKeyboard: Boolean;
    function GetProjectFile: TkmxProjectFile;
  public
    procedure BuildMenu(Menu: TPopupMenu); override;
    procedure DefaultEvent(Sender: TObject); override;
    function DoAction(action: TProjectFileAction; FSilent: Boolean): Boolean; override;
    property ProjectFile: TkmxProjectFile read GetProjectFile;
  end;

implementation

uses
  KeymanDeveloperUtils,
  ProjectUIFileType,
  UfrmMain;

procedure TkmxProjectFileUI.BuildMenu(Menu: TPopupMenu);
var
  mi: TMenuItem;
begin
  inherited BuildMenu(Menu);

  mi := TMenuItem.Create(Menu);
  mi.Caption := '&Test';
  mi.OnClick := MenuEventTest;
  mi.Default := True;
  Menu.Items.Add(mi);

  mi := TMenuItem.Create(Menu);
  mi.Caption := '&Install';
  mi.OnClick := MenuEventInstall;
  Menu.Items.Add(mi);

  mi := TMenuItem.Create(Menu);
  mi.Caption := '&Uninstall';
  mi.OnClick := MenuEventUninstall;
  Menu.Items.Add(mi);
end;

procedure TkmxProjectFileUI.DefaultEvent(Sender: TObject);
begin
  TestKeyboard;
end;

function TkmxProjectFileUI.DoAction(action: TProjectFileAction; FSilent: Boolean): Boolean;
begin
  case action of
    pfaTest: Result := TestKeyboard;
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

procedure TkmxProjectFileUI.MenuEventTest(Sender: TObject);
begin
  TestKeyboard;
end;

procedure TkmxProjectFileUI.MenuEventInstall(Sender: TObject);
begin
  InstallKeyboard;
end;

procedure TkmxProjectFileUI.MenuEventUninstall(Sender: TObject);
begin
  UninstallKeyboard;
end;

function TkmxProjectFileUI.TestKeyboard: Boolean;
begin
  frmKeymanDeveloper.OpenTestWindow(ProjectFile.FileName);
  Result := True;
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
