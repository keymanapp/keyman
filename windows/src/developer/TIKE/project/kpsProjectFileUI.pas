(*
  Name:             kpsProjectFileUI
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
unit kpsProjectFileUI;

interface

uses
  System.UITypes,
  KPSFile,
  Menus,
  ProjectFile,
  ProjectFilesUI,
  ProjectFileUI,
  kpsProjectFile;

type
  TkpsProjectFileUI = class(TOpenableProjectFileUI)
  private
    procedure MenuEventCompile(Sender: TObject);
    procedure MenuEventTest(Sender: TObject);

    function TestPackage: Boolean;
    function InstallPackage: Boolean;
    function UninstallPackage: Boolean;
    function CompilePackage(FSilent: Boolean): Boolean;
    function CompilePackageInstaller(FSilent: Boolean): Boolean;

    function GetPack: TKPSFile;
    function GetProjectFile: TkpsProjectFile;
    function TestPackageState(FCompiledName: string; FSilent: Boolean): Boolean;
  public
    function DoAction(action: TProjectFileAction; FSilent: Boolean): Boolean; override;
    procedure BuildMenu(Menu: TPopupMenu); override;
    property ProjectFile: TkpsProjectFile read GetProjectFile;
  end;

implementation

uses
  System.SysUtils,

  Windows,
  Dialogs,
  dmActionsMain,
  Controls,
  Project,
  ProjectUIFileType,
  UfrmMain,
  UfrmMessages,
  UfrmMDIEditor,
  UfrmPackageEditor,
  utilexecute,
  Variants,
  ShellApi,
  KeymanDeveloperUtils,
  PackageInfo;

procedure TkpsProjectFileUI.BuildMenu(Menu: TPopupMenu);
var
  mi: TMenuItem;
begin
  inherited BuildMenu(Menu);

  mi := TMenuItem.Create(Menu);
  mi.Caption := '&Compile';
  mi.OnClick := MenuEventCompile;
  Menu.Items.Add(mi);

  mi := TMenuItem.Create(Menu);
  mi.Caption := '&Test';
  mi.OnClick := MenuEventTest;
  Menu.Items.Add(mi);
end;

function TkpsProjectFileUI.CompilePackage(FSilent: Boolean): Boolean;
begin
  Result := False;
  if ProjectFile.Modified then
    if not modActionsMain.actFileSave.Execute then Exit;

  Result := ProjectFile.CompilePackage(GetPack, FSilent);
end;

function TkpsProjectFileUI.CompilePackageInstaller(FSilent: Boolean): Boolean;
begin
  Result := False;
  if ProjectFile.Modified then
    if not modActionsMain.actFileSave.Execute then Exit;

  Result := ProjectFile.CompilePackageInstaller(GetPack, FSilent);
end;

function TkpsProjectFileUI.DoAction(action: TProjectFileAction; FSilent: Boolean): Boolean;
begin
  case action of
    pfaCompile: Result := CompilePackage(FSilent);
    pfaTest: Result := TestPackage;
    pfaInstall: Result := InstallPackage;
    pfaUninstall: Result := UninstallPackage;
    pfaCompileInstaller: Result := CompilePackageInstaller(FSilent);
    pfaClean: Result := ProjectFile.Clean;
  else
    Result := False;
  end;
end;

function TkpsProjectFileUI.GetPack: TKPSFile;
begin
  if Assigned(MDIChild)
    then with MDIChild as TfrmPackageEditor do Result := GetPack
    else Result := nil;
end;

function TkpsProjectFileUI.GetProjectFile: TkpsProjectFile;
begin
  Result := FOwner as TkpsProjectFile;
end;

function TkpsProjectFileUI.InstallPackage: Boolean;
var
  FCompiledName: string;
begin
  Result := False;
  FCompiledName := ProjectFile.TargetFilename;
  if not TestPackageState(FCompiledName, False) then Exit;
  KeymanDeveloperUtils.InstallPackage(FCompiledName, True);
  Result := True;
end;

procedure TkpsProjectFileUI.MenuEventCompile(Sender: TObject);
begin
  frmMessages.Clear;
  CompilePackage(False);
end;

procedure TkpsProjectFileUI.MenuEventTest(Sender: TObject);
begin
  TestPackage;
end;

function TkpsProjectFileUI.TestPackage: Boolean;
var
  pack: TKPSFile;
  FTargetFileName: string;
begin
  if Assigned(MDIChild) then
    with MDIChild as TfrmPackageEditor do pack := GetPack
  else
  begin
    pack := TKPSFile.Create;
    pack.FileName := ProjectFile.FileName;
    pack.LoadXML;
  end;

  try
    FTargetFileName := ProjectFile.TargetFilename;
    if not TUtilExecute.Shell(frmKeymanDeveloper.Handle, FTargetFileName, ExtractFileDir(FTargetFileName)) then  // I3349
      ShowMessage(SysErrorMessage(GetLastError));
  finally
    if not Assigned(MDIChild) then
      pack.Free;
  end;

  Result := True;
end;

function TkpsProjectFileUI.UninstallPackage: Boolean;
begin
  KeymanDeveloperUtils.UninstallPackage(ChangeFileExt(ExtractFileName(ProjectFile.FileName), ''));
  Result := True;
end;

function TkpsProjectFileUI.TestPackageState(FCompiledName: string; FSilent: Boolean): Boolean;
var
  ftkps, ftkmp: TDateTime;
begin
  Result := False;

  if not FileExists(FCompiledName) then
    if FSilent then
    begin
      if not CompilePackage(FSilent) then Exit;
    end
    else
      case MessageDlg('You need to compile the keyboard before you can test it.  Compile now?',
          mtConfirmation, mbOkCancel, 0) of
        mrOk:     if not CompilePackage(FSilent) then Exit;
        mrCancel: Exit;
      end;

  FileAge(ProjectFile.FileName, ftkps);
  FileAge(FCompiledName, ftkmp);

  if ProjectFile.Modified or (ftkps > ftkmp) then
    if FSilent then
    begin
      if not CompilePackage(FSilent) then Exit;
    end
    else
      case MessageDlg('The source file has changed.  Recompile before testing?',
          mtConfirmation, mbYesNoCancel, 0) of
        mrYes:    if not CompilePackage(FSilent) then Exit;
        mrNo:     ;
        mrCancel: Exit;
      end;

  Result := True;
end;

initialization
  RegisterProjectFileUIType(TkpsProjectFile, TkpsProjectFileUI);
end.
