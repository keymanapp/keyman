(*
  Name:             Keyman.Developer.UI.Project.kmnProjectFileUI
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
unit Keyman.Developer.UI.Project.kmnProjectFileUI;

interface

uses
  System.UITypes,
  Menus,
  Keyman.Developer.UI.Project.ProjectFilesUI,
  Keyman.Developer.UI.Project.ProjectFileUI,
  Keyman.Developer.UI.Project.ProjectUIFileType,
  UfrmMessages,
  Keyman.Developer.System.Project.kmnProjectFile,
  Keyman.Developer.System.Project.kmnProjectFileAction;

type
  TkmnProjectFileUI = class(TOpenableProjectFileUI)
  private
    function TestKeymanWeb(FSilent: Boolean): Boolean;
    function DebugKeyboard(FSilent: Boolean): Boolean;
    function FontHelper(FSilent: Boolean): Boolean;
    function FontDialog(FSilent: Boolean): Boolean;   // I4057
    function InstallKeyboard: Boolean;
    function UninstallKeyboard: Boolean;
    function GetProjectFile: TkmnProjectFileAction;

    function GetDebug: Boolean;
    procedure SetDebug(const Value: Boolean);
    function CompileKeyboard(FSilent: Boolean): Boolean;
    function TestKeyboardState(FCompiledName: string; FSilent: Boolean): Boolean;
  public
    function DoAction(action: TProjectFileAction; FSilent: Boolean): Boolean; override;
    property Debug: Boolean read GetDebug write SetDebug;
    property ProjectFile: TkmnProjectFileAction read GetProjectFile;
  end;

implementation

uses
  Winapi.Windows,
  System.StrUtils,
  System.SysUtils,
  Vcl.Dialogs,
  Vcl.Graphics,
  Vcl.Controls,
  dmActionsMain,

  Keyman.Developer.UI.UfrmMessageDlgWithSave,
  UfrmMain,
  UfrmFontHelper,
  UfrmKeymanWizard,
  UfrmKeyboardFonts,
  UfrmMDIEditor,
  UKeymanTargets,
  UmodWebHttpServer,
  Keyman.Developer.System.ServerAPI,
  Keyman.Developer.UI.ServerUI,
  KeyboardFonts,
  KeymanDeveloperUtils,
  KeymanDeveloperOptions,
  Keyman.System.FontLoadUtil,
  System.Classes,
  UfrmPackageEditor,
  System.Variants,
  utilsystem;

function TkmnProjectFileUI.DoAction(action: TProjectFileAction; FSilent: Boolean): Boolean;
begin
  case action of
    pfaCompile:   Result := CompileKeyboard(FSilent);
    pfaTestKeymanWeb: Result := TestKeymanWeb(FSilent);
    pfaInstall:   Result := InstallKeyboard;
    pfaUninstall: Result := UninstallKeyboard;
    pfaDebug:     Result := DebugKeyboard(FSilent);
    pfaFontHelper: Result := FontHelper(FSilent);
    pfaFontDialog: Result := FontDialog(FSilent);   // I4057
    pfaClean:      Result := ProjectFile.Clean;
  else
    Result := False;
  end;
end;

function TkmnProjectFileUI.CompileKeyboard(FSilent: Boolean): Boolean;
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
      TServerDebugAPI.IsKeyboardRegistered(ProjectFile.JSTargetFileName) and
      (ProjectFile.Targets * KMWKeymanTargets <> []) then
    TestKeymanWeb(True);
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
  FCompiledName := ProjectFile.KmxTargetFileName;
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

function TkmnProjectFileUI.GetProjectFile: TkmnProjectFileAction;
begin
  Result := FOwner as TkmnProjectFileAction;
end;

procedure TkmnProjectFileUI.SetDebug(const Value: Boolean);
begin
  ProjectFile.Debug := Value;
end;

function TkmnProjectFileUI.TestKeymanWeb(FSilent: Boolean): Boolean;   // I4409
var
  FCompiledName, OSKFont: string;
  editor: TfrmTikeEditor;
  wizard: TfrmKeymanWizard;
  i: TKeyboardFont;
  j: TKeyboardFont;
  Found: Boolean;

  function IsStandardFont(const FontName: string): Boolean;   // I4448
  const
    StandardFontNames: array[0..9] of string = (
      'Arial', 'Calibri', 'Consolas', 'Courier New', 'Lucida Console', 'Lucida Sans Unicode', 'Segoe UI', 'Tahoma', 'Times New Roman', 'Verdana'
      );
  begin
    Result := AnsiIndexText(FontName, StandardFontNames) >= 0;
  end;

  procedure RegisterFont(const fontname: string);
  var
    strm: TMemoryStream;
  begin
    if (fontname <> '') and not IsStandardFont(fontname) then
    begin
      strm := TMemoryStream.Create;
      try
        if TFontLoadUtil.LoadFontData(fontname, strm) and
            TServerDebugAPI.Running then
          TServerDebugAPI.RegisterFont(strm, fontname);
      finally
        strm.Free;
      end;
    end;
  end;
begin
  editor := frmKeymanDeveloper.FindEditorByFileName(ProjectFile.FileName);   // I4021
  if not Assigned(editor) or not (editor is TfrmKeymanWizard) then
    Exit(False);
  wizard := editor as TfrmKeymanWizard;

  if ProjectFile.Targets * KMWKeymanTargets = [] then
    Exit(False);

  FCompiledName := ProjectFile.JSTargetFilename;
  if FCompiledName = '' then
    Exit(False);

  if not TestKeyboardState(FCompiledName, FSilent) then
    Exit(False);

  // We register all fonts that are used by the layout,
  // but just once for each reference!
  for i := kfontChar to kfontDisplayMap do
  begin
    Found := False;
    for j := kfontChar to TKeyboardFont(Ord(i)-1) do
      if Wizard.FontInfo[j].Name = Wizard.FontInfo[i].Name then
      begin
        Found := True;
        Break;
      end;
    if not Found then
      RegisterFont(Wizard.FontInfo[i].Name);
  end;

  if TServerUI.VerifyServerRunning then
  begin
    if Wizard.FontInfo[kfontDisplayMap].Enabled then
    begin
      OSKFont := Wizard.FontInfo[kfontDisplayMap].Name
    end
    else
      OSKFont := Wizard.FontInfo[kfontOSK].Name;

    TServerDebugAPI.RegisterKeyboard(
      FCompiledName,
      ProjectFile.FileVersion,
      Wizard.FontInfo[kfontChar].Name,
      OSKFont
    );

    wizard.NotifyStartedWebDebug;   // I4021
  end;

  Result := True;
end;

function TkmnProjectFileUI.InstallKeyboard: Boolean;
var
  FCompiledName: string;
begin
  Result := False;
  FCompiledName := ProjectFile.KmxTargetFilename;
  if not TestKeyboardState(FCompiledName, False) then Exit;
  KeymanDeveloperUtils.InstallKeyboard(FCompiledName, True);
  Result := True;
end;

function TkmnProjectFileUI.UninstallKeyboard: Boolean;
begin
  Result := KeymanDeveloperUtils.UninstallKeyboard(ChangeFileExt(ExtractFileName(ProjectFile.FileName), ''));
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
  RegisterProjectFileUIType(TkmnProjectFileAction, TkmnProjectFileUI);
end.




