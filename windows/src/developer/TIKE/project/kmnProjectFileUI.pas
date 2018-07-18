(*
  Name:             kmnProjectFileUI
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
unit kmnProjectFileUI;

interface

uses
  System.UITypes,
  Menus,
  ProjectFilesUI,
  ProjectFileUI,
  ProjectUIFileType,
  UfrmMessages,
  kmnProjectFile;

type
  TkmnProjectFileUI = class(TOpenableProjectFileUI)
  private
    procedure MenuEventCompile(Sender: TObject);
    procedure MenuEventClean(Sender: TObject);
    procedure MenuEventTest(Sender: TObject);
    procedure MenuEventDebug(Sender: TObject);

    function TestKeyboard(FSilent: Boolean): Boolean;
    function TestKeymanWeb(FSilent: Boolean): Boolean;
    function DebugKeyboard(FSilent: Boolean): Boolean;
    function FontHelper(FSilent: Boolean): Boolean;
    function FontDialog(FSilent: Boolean): Boolean;   // I4057
    function InstallKeyboard: Boolean;
    function UninstallKeyboard: Boolean;
    function PackageFile(FSilent: Boolean): Boolean;
    function GetProjectFile: TkmnProjectFile;

    function GetDebug: Boolean;
    procedure SetDebug(const Value: Boolean);
    function CompileKeyboard(FSilent: Boolean): Boolean;
    function TestKeyboardState(FCompiledName: string; FSilent: Boolean): Boolean;

  public
    function DoAction(action: TProjectFileAction; FSilent: Boolean): Boolean; override;
    procedure BuildMenu(Menu: TPopupMenu); override;
    property Debug: Boolean read GetDebug write SetDebug;
    property ProjectFile: TkmnProjectFile read GetProjectFile;
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
  UfrmFontHelper,
  UfrmKeymanWizard,
  UfrmKeyboardFonts,
  UfrmMDIEditor,
  UmodWebHttpServer,
  KeyboardFonts,
  KeymanDeveloperUtils,
  KeymanDeveloperOptions,
  System.Classes,
  UfrmPackageEditor,
  System.Variants,
  utilsystem;

procedure TkmnProjectFileUI.BuildMenu(Menu: TPopupMenu);
var
  mi: TMenuItem;
begin
  inherited BuildMenu(Menu);

  mi := TMenuItem.Create(Menu);
  mi.Caption := '&Compile';
  mi.OnClick := MenuEventCompile;
  Menu.Items.Add(mi);

  mi := TMenuItem.Create(Menu);
  mi.Caption := 'C&lean';
  mi.OnClick := MenuEventClean;
  Menu.Items.Add(mi);

  if FKeymanDeveloperOptions.UseOldDebugger then
  begin
    mi := TMenuItem.Create(Menu);
    mi.Caption := '&Test';
    mi.OnClick := MenuEventTest;
    Menu.Items.Add(mi);
  end
  else
  begin
    mi := TMenuItem.Create(Menu);
    mi.Caption := '&Debug';
    mi.OnClick := MenuEventDebug;
    Menu.Items.Add(mi);
  end;
end;

function TkmnProjectFileUI.DoAction(action: TProjectFileAction; FSilent: Boolean): Boolean;
begin
  case action of
    pfaCompile:   Result := CompileKeyboard(FSilent);
    pfaTest:      Result := TestKeyboard(FSilent);
    pfaTestKeymanWeb: Result := TestKeymanWeb(FSilent);
    pfaInstall:   Result := InstallKeyboard;
    pfaUninstall: Result := UninstallKeyboard;
    pfaDebug:     Result := DebugKeyboard(FSilent);
    pfaPackage:   Result := PackageFile(FSilent);
    pfaFontHelper: Result := FontHelper(FSilent);
    pfaFontDialog: Result := FontDialog(FSilent);   // I4057
    pfaClean:      Result := ProjectFile.Clean;
  else
    Result := False;
  end;
end;

function TkmnProjectFileUI.CompileKeyboard(FSilent: Boolean): Boolean;
begin
  Result := False;

  FCompilingFile := ProjectFile.FileName;

  if ProjectFile.Modified then
  begin
    if not FSilent then
    begin
      if MessageDlg('The keyboard file has been modified.  You must save before compiling.'+#13#10+'Save the keyboard and continue?',
        mtConfirmation, mbOkCancel, 0) = mrCancel then Exit;
      if not modActionsMain.actFileSave.Execute then Exit;
    end
    else
      Exit;
  end;

  if not FSilent then
    frmMessages.DoShowForm;

  Result := ProjectFile.CompileKeyboard;
end;

function TkmnProjectFileUI.FontDialog(FSilent: Boolean): Boolean;   // I4057
var
  editor: TfrmTikeEditor;
  wizard: TfrmKeymanWizard;
  kf: TKeyboardFont;
begin
  editor := frmKeymanDeveloper.ActiveEditor;
  if not Assigned(editor) or not (editor is TfrmKeymanWizard) then
    Exit(False);
  wizard := editor as TfrmKeymanWizard;

  with TfrmKeyboardFonts.Create(frmKeymanDeveloper) do
  try
    for kf := Low(TKeyboardFont) to High(TKeyboardFont) do
      FontInfo[kf] := wizard.FontInfo[kf];
    if ShowModal = mrOk then
    begin
      for kf := Low(TKeyboardFont) to High(TKeyboardFont) do
        wizard.FontInfo[kf] := FontInfo[kf];
    end;
  finally
    Free;
  end;

  Result := True;
end;

function TkmnProjectFileUI.FontHelper(FSilent: Boolean): Boolean;
var
  FCompiledName: string;
begin
  Result := False;
  FCompiledName := ProjectFile.TargetFileName;
  if not TestKeyboardState(FCompiledName, FSilent) then Exit;
//  with TfrmFontHelper.Create(frmKeymanDeveloper) do
//  try
//    FileName := FCompiledName;
//    ShowModal;
//  finally
//    Free;
//  end;
end;

function TkmnProjectFileUI.GetDebug: Boolean;
begin
  Result := ProjectFile.Debug;
end;

function TkmnProjectFileUI.GetProjectFile: TkmnProjectFile;
begin
  Result := FOwner as TkmnProjectFile;
end;

function TkmnProjectFileUI.PackageFile(FSilent: Boolean): Boolean;
var
  FKMXTarget, FJSTarget: string;
begin
  Result := False;
  FKMXTarget := ProjectFile.TargetFilename;
  FJSTarget := ProjectFile.JSTargetFilename;

  if (FKMXTarget <> '') and not TestKeyboardState(FKMXTarget, FSilent) then
    Exit;

  if (FJSTarget <> '') and not TestKeyboardState(FJSTarget, FSilent) then
    Exit;

  with TfrmPackageEditor.Create(frmKeymanDeveloper) do
    CreateFromCompiledKeyboard(FKMXTarget, FJSTarget);
  Result := True;
end;

procedure TkmnProjectFileUI.MenuEventClean(Sender: TObject);
begin
  frmMessages.Clear;
  ProjectFile.Clean;
end;

procedure TkmnProjectFileUI.MenuEventCompile(Sender: TObject);
begin
  frmMessages.Clear;
  CompileKeyboard(False);
end;

procedure TkmnProjectFileUI.MenuEventTest(Sender: TObject);
begin
  TestKeyboard(False);
end;

procedure TkmnProjectFileUI.MenuEventDebug(Sender: TObject);
begin
  DebugKeyboard(False);
end;

procedure TkmnProjectFileUI.SetDebug(const Value: Boolean);
begin
  ProjectFile.Debug := Value;
end;

function TkmnProjectFileUI.TestKeyboard(FSilent: Boolean): Boolean;
var
  FCompiledName: string;
begin
  Result := False;
  FCompiledName := ProjectFile.TargetFileName;
  if not TestKeyboardState(FCompiledName, FSilent) then Exit;
  frmKeymanDeveloper.OpenTestWindow(FCompiledName);
  Result := True;
end;

function TkmnProjectFileUI.TestKeymanWeb(FSilent: Boolean): Boolean;   // I4409
var
  FCompiledName: string;
  editor: TfrmTikeEditor;
  wizard: TfrmKeymanWizard;
  FontNames: TKeyboardFontArray;
  i: TKeyboardFont;
begin
  editor := frmKeymanDeveloper.FindEditorByFileName(ProjectFile.FileName);   // I4021
  if not Assigned(editor) or not (editor is TfrmKeymanWizard) then
    Exit(False);
  wizard := editor as TfrmKeymanWizard;

  FCompiledName := ProjectFile.JSTargetFilename;
  if not TestKeyboardState(FCompiledName, FSilent) then
    Exit(False);

  for i := Low(TKeyboardFont) to High(TKeyboardFont) do
    FontNames[i] := Wizard.FontInfo[i].Name;
  modWebHttpServer.Debugger.RegisterKeyboard(FCompiledName, ProjectFile.FileVersion, FontNames);

  wizard.NotifyStartedWebDebug;   // I4021

  Result := True;
end;

function TkmnProjectFileUI.InstallKeyboard: Boolean;
var
  FCompiledName: string;
begin
  Result := False;
  FCompiledName := ProjectFile.TargetFilename;
  if not TestKeyboardState(FCompiledName, False) then Exit;
  KeymanDeveloperUtils.InstallKeyboard(FCompiledName, True);
  Result := True;
end;

function TkmnProjectFileUI.UninstallKeyboard: Boolean;
begin
  KeymanDeveloperUtils.UninstallKeyboard(ChangeFileExt(ExtractFileName(ProjectFile.FileName), ''));
  Result := True;
end;

function TkmnProjectFileUI.DebugKeyboard(FSilent: Boolean): Boolean;
var
  editor: TfrmKeymanWizard;
begin
  editor := frmKeymanDeveloper.OpenEditor(ProjectFile.FileName, TfrmKeymanWizard) as TfrmKeymanWizard;
  editor.StartDebugging;
  Result := True;
end;

function TkmnProjectFileUI.TestKeyboardState(FCompiledName: string; FSilent: Boolean): Boolean;
var
  ftkmn, ftkmx: TDateTime;
begin
  Result := False;

  if not FileExists(FCompiledName) then
    if FSilent then
    begin
      if not CompileKeyboard(FSilent) then Exit;
    end
    else
      case MessageDlg('You need to compile the keyboard before you can continue.  Compile now?',
          mtConfirmation, mbOkCancel, 0) of
        mrOk:     if not CompileKeyboard(FSilent) then Exit;
        mrCancel: Exit;
      end;

  FileAge(ProjectFile.FileName, ftkmn);
  FileAge(FCompiledName, ftkmx);

  if ProjectFile.Modified or (ftkmn > ftkmx) then
    if FSilent then
    begin
      if not CompileKeyboard(FSilent) then Exit;
    end
    else
      case MessageDlg('The source file has changed.  Recompile before continuing?',
          mtConfirmation, mbYesNoCancel, 0) of
        mrYes:    if not CompileKeyboard(FSilent) then Exit;
        mrNo:     ;
        mrCancel: Exit;
      end;

  Result := True;
end;

initialization
  RegisterProjectFileUIType(TkmnProjectFile, TkmnProjectFileUI);
end.




