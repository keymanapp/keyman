unit Keyman.Developer.System.Project.kpsProjectFileAction;

interface

uses
  System.SysUtils,

  Keyman.Developer.System.Project.kpsProjectFile,
  Keyman.Developer.System.Project.ProjectFile,
  Keyman.Developer.System.Project.ProjectFiles,
  Keyman.Developer.System.Project.ProjectFileType,
  Keyman.Developer.System.Project.ProjectLog,
  KPSFile;

type
  TkpsProjectFileAction = class(TkpsProjectFile)
  private
    procedure SelfMessage(Sender: TObject; msg: string; State: TProjectLogState);   // I4706

  public
    function CompilePackageInstaller(APack: TKPSFile; FSilent: Boolean): Boolean;
    function CompilePackage(APack: TKPSFile; FSilent: Boolean): Boolean;
    function Clean: Boolean;
  end;

implementation

uses
  CompilePackage,
  CompilePackageInstaller,
  PackageInfo,
  utilexecute;

function TkpsProjectFileAction.CompilePackage(APack: TKPSFile; FSilent: Boolean): Boolean;
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
      Result := DoCompilePackage(pack, SelfMessage, FSilent, OwnerProject.Options.CheckFilenameConventions, TargetFilename);
      if HasCompileWarning and (WarnAsError or OwnerProject.Options.CompilerWarningsAsErrors) then   // I4706
        Result := False;

      if Result then
        Log(plsInfo, '''' + FileName + ''' compiled successfully to '''+TargetFileName+'''.')
      else
      begin
        if FileExists(TargetFilename) then
          System.SysUtils.DeleteFile(TargetFilename);
        Log(plsError, '''' + FileName + ''' was not compiled successfully.');
      end;
    except
      on E:Exception do
      begin
        Log(plsError, E.Message);
        Result := False;
      end;
    end;

  finally
    if APack = nil then
      pack.Free;
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
      Result := DoCompilePackageInstaller(pack, SelfMessage, FSilent, '', TargetInstallerFilename, False, True, '', '', '');
      if HasCompileWarning and (WarnAsError or OwnerProject.Options.CompilerWarningsAsErrors) then   // I4706
        Result := False;

      if Result
        then Log(plsInfo, '''' + FileName + ''' compiled successfully.')
        else Log(plsError, '''' + FileName + ''' was not compiled successfully.');
    except
      on E:Exception do
      begin
        Log(plsError, E.Message);
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
  Log(State, msg);
end;

initialization
  RegisterProjectFileType('.kps', TkpsProjectFileAction);
end.
