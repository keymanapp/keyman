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
  public
    function CompilePackage: Boolean;
    function Clean: Boolean;
  end;

implementation

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

function TkpsProjectFileAction.Clean: Boolean;
begin
  CleanFile(OutputFileName);
  CleanFile(TargetInstallerFilename);   // I4737
  Result := True;
end;

initialization
  RegisterProjectFileType('.kps', TkpsProjectFileAction);
end.
