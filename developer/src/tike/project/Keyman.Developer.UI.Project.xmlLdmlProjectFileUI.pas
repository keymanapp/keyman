{
 * Keyman is copyright (C) SIL International. MIT License.
 *
 * xmlLdmlProjectFileUI: User Interface connections for LDML keyboard files
}
unit Keyman.Developer.UI.Project.xmlLdmlProjectFileUI;

interface

uses
  System.UITypes,
  Menus,
  Keyman.Developer.UI.Project.ProjectFilesUI,
  Keyman.Developer.UI.Project.ProjectFileUI,
  Keyman.Developer.UI.Project.ProjectUIFileType,
  UfrmMessages,
  Keyman.Developer.System.Project.xmlLdmlProjectFile,
  Keyman.Developer.System.Project.xmlLdmlProjectFileAction;

type
  TxmlLdmlProjectFileUI = class(TOpenableProjectFileUI)
  private
    function TestKeymanWeb(FSilent: Boolean): Boolean;
    function InstallKeyboard: Boolean;
    function UninstallKeyboard: Boolean;
    function GetProjectFile: TxmlLdmlProjectFileAction;

    function GetDebug: Boolean;
    procedure SetDebug(const Value: Boolean);
    function CompileKeyboard(FSilent: Boolean): Boolean;
    function TestKeyboardState(FCompiledName: string; FSilent: Boolean): Boolean;
  public
    function DoAction(action: TProjectFileAction; FSilent: Boolean): Boolean; override;
    property Debug: Boolean read GetDebug write SetDebug;
    property ProjectFile: TxmlLdmlProjectFileAction read GetProjectFile;
  end;

implementation

uses
  Winapi.Windows,
  System.Classes,
  System.StrUtils,
  System.SysUtils,
  System.Variants,
  Vcl.Dialogs,
  Vcl.Graphics,
  Vcl.Controls,
  dmActionsMain,

  Keyman.Developer.UI.UfrmMessageDlgWithSave,
  UfrmMain,
  Keyman.Developer.UI.UfrmLdmlKeyboardEditor,
  UfrmMDIEditor,
  UKeymanTargets,
  UmodWebHttpServer,
  Keyman.Developer.System.ServerAPI,
  Keyman.Developer.UI.ServerUI,
  KeyboardFonts,
  KeymanDeveloperUtils,
  KeymanDeveloperOptions,
  utilsystem;

function TxmlLdmlProjectFileUI.DoAction(action: TProjectFileAction; FSilent: Boolean): Boolean;
begin
  case action of
    pfaCompile:   Result := CompileKeyboard(FSilent);
    pfaTestKeymanWeb: Result := TestKeymanWeb(FSilent);
    pfaInstall:   Result := InstallKeyboard;
    pfaUninstall: Result := UninstallKeyboard;
//  pfaDebug:     Result := DebugKeyboard(FSilent);
    pfaClean:      Result := ProjectFile.Clean;
  else
    Result := False;
  end;
end;

function TxmlLdmlProjectFileUI.CompileKeyboard(FSilent: Boolean): Boolean;
var
  FSave: Boolean;
begin
  Result := False;

  if ProjectFile.Modified then
  begin
    if not FSilent then
    begin
      if not FKeymanDeveloperOptions.AutoSaveBeforeCompiling then
      begin
        if TfrmMessageDlgWithSave.Execute(
            'The keyboard file has been modified.  You must save before compiling.'+#13#10#13#10+
            'Save the keyboard and continue?',
            'Always save automatically before compiling',
            '', True, FSave) in [mrNo, mrCancel] then
          Exit(False);
        if FSave then
        begin
          FKeymanDeveloperOptions.AutoSaveBeforeCompiling := True;
          FKeymanDeveloperOptions.Write;
        end;
      end;

      if not modActionsMain.actFileSave.Execute then Exit;
    end
    else
      Exit;
  end;

  if not FSilent then
    frmMessages.DoShowForm;

  Result :=
    ProjectFile.CompileKeyboard;

  if Result and
      TServerDebugAPI.Running and
      TServerDebugAPI.IsKeyboardRegistered(ProjectFile.TargetFileName) and
      (ProjectFile.Targets * KMWKeymanTargets <> []) then
    TestKeymanWeb(True);
end;

function TxmlLdmlProjectFileUI.GetDebug: Boolean;
begin
  Result := ProjectFile.Debug;
end;

function TxmlLdmlProjectFileUI.GetProjectFile: TxmlLdmlProjectFileAction;
begin
  Result := FOwner as TxmlLdmlProjectFileAction;
end;

procedure TxmlLdmlProjectFileUI.SetDebug(const Value: Boolean);
begin
  ProjectFile.Debug := Value;
end;

function TxmlLdmlProjectFileUI.TestKeymanWeb(FSilent: Boolean): Boolean;   // I4409
begin
  // TODO (18.0): when we have web support for ldml keyboards
  Result := True;
end;

function TxmlLdmlProjectFileUI.InstallKeyboard: Boolean;
var
  FCompiledName: string;
begin
  Result := False;
  FCompiledName := ProjectFile.TargetFilename;
  if not TestKeyboardState(FCompiledName, False) then Exit;
  KeymanDeveloperUtils.InstallKeyboard(FCompiledName, True);
  Result := True;
end;

function TxmlLdmlProjectFileUI.UninstallKeyboard: Boolean;
begin
  Result := KeymanDeveloperUtils.UninstallKeyboard(ChangeFileExt(ExtractFileName(ProjectFile.FileName), ''));
end;

function TxmlLdmlProjectFileUI.TestKeyboardState(FCompiledName: string; FSilent: Boolean): Boolean;
var
  ftxml, ftkmx: TDateTime;
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

  FileAge(ProjectFile.FileName, ftxml);
  FileAge(FCompiledName, ftkmx);

  if ProjectFile.Modified or (ftxml > ftkmx) then
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
  RegisterProjectFileUIType(TxmlLdmlProjectFileAction, TxmlLdmlProjectFileUI);
end.
