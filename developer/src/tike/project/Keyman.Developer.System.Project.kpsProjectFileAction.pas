unit Keyman.Developer.System.Project.kpsProjectFileAction;

interface

uses
  System.SysUtils,

  Keyman.Developer.System.Project.kpsProjectFile,
  Keyman.Developer.System.Project.ProjectFile,
  Keyman.Developer.System.Project.ProjectFiles,
  Keyman.Developer.System.Project.ProjectFileType,
  Keyman.Developer.System.Project.ProjectLog,
  Keyman.Developer.System.KmcWrapper,
  KPSFile;

type
  TkpsProjectFileAction = class(TkpsProjectFile)
  private
    procedure SelfMessage(Sender: TObject; msg: string; State: TProjectLogState);   // I4706

  public
    function CompilePackageInstaller(APack: TKPSFile; FSilent: Boolean): Boolean;
    function CompilePackage: Boolean;
    function Clean: Boolean;
  end;

implementation

uses
  compile,
  CompilePackageInstaller,
  Keyman.Developer.System.ValidateKpsFile,
  PackageInfo,
  utilexecute;

function TkpsProjectFileAction.CompilePackage: Boolean;
var
  w: TKmcWrapper;
begin
  w := TKmcWrapper.Create;
  try
    Result := w.Compile(Self, FileName, TargetFilename, False);
    // TODO(lowpri): FDebug flag
  finally
    w.Free;
  end;
end;

function TkpsProjectFileAction.CompilePackageInstaller(APack: TKPSFile; FSilent: Boolean): Boolean;
var
  pack: TKPSFile;
begin
  HasCompileWarning := False;   // I4706

  if APack = nil then
  begin
    pack := TKPSFile.Create;
    pack.FileName := FileName;
    pack.LoadXML;
  end
  else
    pack := APack;

  try
    try
      Result := DoCompilePackageInstaller(pack, SelfMessage, FSilent, '', TargetInstallerFilename, '', False, True, '', '', '', False, False);
      if HasCompileWarning and (WarnAsError or OwnerProject.Options.CompilerWarningsAsErrors) then   // I4706
        Result := False;

      if Result
        then Log(plsSuccess, '''' + FileName + ''' compiled successfully.', 0, 0)
        else Log(plsFailure, '''' + FileName + ''' was not compiled successfully.', 0, 0);
    except
      on E:Exception do
      begin
        Log(plsError, E.Message, CERR_ERROR, 0);
        Log(plsFailure, '''' + FileName + ''' was not compiled successfully.', 0, 0);
        Result := False;
      end;
    end;

  finally
    if APack = nil then
      pack.Free;
  end;
end;

function TkpsProjectFileAction.Clean: Boolean;
begin
  CleanFile(OutputFileName);
  CleanFile(TargetInstallerFilename);   // I4737
  Result := True;
end;

procedure TkpsProjectFileAction.SelfMessage(Sender: TObject; msg: string; State: TProjectLogState);   // I4706
begin
  if State = plsWarning then
    HasCompileWarning := True;
  Log(State, msg, 0, 0);
end;

initialization
  RegisterProjectFileType('.kps', TkpsProjectFileAction);
end.
