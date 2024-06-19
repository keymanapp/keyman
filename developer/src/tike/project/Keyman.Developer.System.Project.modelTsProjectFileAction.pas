unit Keyman.Developer.System.Project.modelTsProjectFileAction;

interface

uses
  System.SysUtils,

  Keyman.Developer.System.Project.modelTsProjectFile,
  Keyman.Developer.System.Project.ProjectFile,
  Keyman.Developer.System.Project.ProjectFiles,
  Keyman.Developer.System.Project.ProjectFileType,
  Keyman.Developer.System.Project.ProjectLog,
  UKeymanTargets;

type
  TmodelTsProjectFileAction = class(TmodelTsProjectFile)
  private
    procedure CheckFilenameConventions;
  public
    function CompileModel: Boolean;
    function Clean: Boolean;
  end;

implementation

uses
  System.Classes,
  System.StrUtils,
  System.Variants,
  Winapi.Windows,

  Keyman.System.LexicalModelUtils,
  Keyman.Developer.System.KmcWrapper,
  Keyman.Developer.System.LexicalModelCompile,
  VisualKeyboard;

function TmodelTsProjectFileAction.Clean: Boolean;
begin
  CleanFile(TargetFileName);
  Result := True;
end;

procedure TmodelTsProjectFileAction.CheckFilenameConventions;
begin
  if not OwnerProject.Options.CheckFilenameConventions then
    Exit;

  if not TLexicalModelUtils.DoesTSFilenameFollowLexicalModelConventions(FileName) then
  begin
    HasCompileWarning := True;
    Log(plsWarning, Format(TLexicalModelUtils.SModelFileNameDoesNotFollowConventions_Message, [ExtractFileName(FileName)]), CERR_WARNING, 0);
  end;
end;

function TmodelTsProjectFileAction.CompileModel: Boolean;
begin
  HasCompileWarning := False;   // I4706

  CheckFilenameConventions;

  Log(plsInfo, Format('Compiling ''%s''%s...', [Filename, IfThen(IsDebug, ' with debug symbols ', '')]), 0, 0);

  //compile the model
  ForceDirectories(ExtractFileDir(TargetFileName));
  Result := CompileModelFile(Self, FileName, TargetFileName, IsDebug);

  if HasCompileWarning and OwnerProject.Options.CompilerWarningsAsErrors then Result := False;   // I4706

  if Result
    then Log(plsSuccess, Format('''%s'' was compiled successfully  to ''%s''.', [FileName, TargetFileName]), 0, 0)   // I4504
    else Log(plsFailure, Format('''%s'' was not compiled successfully.', [FileName]), 0, 0);   // I4504
end;

initialization
  RegisterProjectFileType('.ts', TmodelTsProjectFileAction);
end.
