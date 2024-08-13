unit Keyman.Developer.System.ProjectOwningFile;

interface

function FindOwnerProjectForFile(const filename: string): string;

implementation

uses
  System.SysUtils,

  Keyman.Developer.System.Project.ProjectFile,
  Keyman.Developer.System.Project.ProjectLoader;

function CheckOwnerProjectForFile(const project, filename: string): Boolean; forward;

{
  #2761

  Given an arbitrary/path/to/file.kmn, search for a project:
  [* arbitrary/path/to/keyman.kpj]
  * arbitrary/path/to/to.kpj (to.kpj matches folder name)
  [* arbitrary/path/keyman.kpj]
  * arbitrary/path/path.kpj (path.kpj matches folder name)

  For each project file, if found, verify that file.kmn is in the project:
  * For a v2.0 project, file.kmn is in the project if it is in $SOURCEPATH
  * For a v1.0 project, file.kmn must be listed explicitly.
}
function FindOwnerProjectForFile(const filename: string): string;
var
  path: string;
begin
  path := ExtractFilePath(filename);

{ TODO: 18.0, see #10113
  // Check arbitrary/path/to/keyman.kpj
  Result := path + C_ProjectStandardFilename;
  if CheckOwnerProjectForFile(Result, filename) then
    Exit;
}

  // Check arbitrary/path/to/to.kpj
  Result := path + ExtractFileName(ExcludeTrailingPathDelimiter(path)) + '.kpj';
  if CheckOwnerProjectForFile(Result, filename) then
    Exit;

  path := ExtractFilePath(ExcludeTrailingPathDelimiter(path));

{ TODO: 18.0, see #10113
  // Check arbitrary/path/keyman.kpj
  Result := path + C_ProjectStandardFilename;
  if CheckOwnerProjectForFile(Result, filename) then
    Exit;
}

  // Check arbitrary/path/path.kpj
  Result := path + ExtractFileName(ExcludeTrailingPathDelimiter(path)) + '.kpj';
  if CheckOwnerProjectForFile(Result, filename) then
    Exit;

  Result := '';
end;

function CheckOwnerProjectForFile(const project, filename: string): Boolean;
var
  p: TProject;
begin
  if not FileExists(project) then
    Exit(False);

  if SameFileName(project, filename) then
    Exit(True);

  try
    p := TProject.Create(ptUnknown, project, True);
  except
    on E:EProjectLoader do
    begin
      Result := False;
      Exit;
    end;
  end;
  try
    Result := p.Files.IndexOfFileName(filename) >= 0;
  finally
    p.Free;
  end;
end;

end.
