unit Keyman.Developer.System.TikeCommandLine;

interface

uses
  System.Classes,
  System.Generics.Collections,

  Keyman.Developer.System.TikeMultiProcess;

type
  TProcessCommandLine = (pclRun, pclExit);

type
  TLaunchProject = class
  strict private
    FProjectFilename: string;
    FFilenames: TStringList;
  public
    constructor Create(const AProjectFilename: string);
    destructor Destroy; override;
    property ProjectFilename: string read FProjectFilename;
    property Filenames: TStringList read FFilenames;
  end;

  TLaunchProjects = class(TObjectList<TLaunchProject>)
  public
    function Find(const ProjectFilename: string): TLaunchProject;
  end;

type
  TTikeCommandLine = class
  strict private
    FProcesses: TTikeProcessList;
  private
    FStartupProjectPath: string;
    FStartupFilenames: TArray<string>;
    function PassProjectToRunningProcess(const project: TLaunchProject): Boolean;
    function LaunchNewInstance(const project: TLaunchProject): Boolean;
    function ProcessSubProcess: Boolean;
  public
    constructor Create;
    destructor Destroy; override;

    function Process: TProcessCommandLine;
    property StartupProjectPath: string read FStartupProjectPath;
    property StartupFilenames: TArray<string> read FStartupFilenames;
  end;

function TikeCommandLine: TTikeCommandLine;

implementation

uses
  System.SysUtils,
  Winapi.Windows,

  ErrorControlledRegistry,
  Keyman.Developer.System.ProjectOwningFile,
  RegistryKeys,
  utilexecute;

{ TLaunchProject }

constructor TLaunchProject.Create(const AProjectFilename: string);
begin
  inherited Create;
  FProjectFilename := AProjectFilename;
  FFilenames := TStringList.Create;
end;

destructor TLaunchProject.Destroy;
begin
  FFilenames.Free;
  inherited Destroy;
end;


{ TTikeCommandLine }

constructor TTikeCommandLine.Create;
var
  reg: TRegistryErrorControlled;
begin
  inherited Create;

  FStartupProjectPath := '';

  reg := TRegistryErrorControlled.Create;  // I2890
  try
    if reg.OpenKeyReadOnly(SRegKey_IDEOptions_CU) then
    begin
      if reg.ValueExists(SRegValue_ActiveProject) then
        FStartupProjectPath := reg.ReadString(SRegValue_ActiveProject);
    end;
  finally
    reg.Free;
  end;

  if (FStartupProjectPath <> '') and not FileExists(FStartupProjectPath) then
  begin
    // The project has disappeared, so return to the welcome page
    FStartupProjectPath := '';
  end;

  FProcesses := TikeMultiProcess.Enumerate;
end;

destructor TTikeCommandLine.Destroy;
begin
  FProcesses.Free;
  inherited Destroy;
end;

/// Reads filenames passed on command line, and determines if they should be
/// opened in an existing instance of Keyman Developer or in this instance, or
/// even in multiple new instances
function TTikeCommandLine.Process: TProcessCommandLine;
var
  i: Integer;
  filename: string;
  filenames: TStringList;
  newProjects, launchProjects: TLaunchProjects;
  p: TLaunchProject;
  projectFilename: string;
begin
  // If launched by LaunchNewInstance then we'll process that and continue
  if ProcessSubProcess then
  begin
    Exit(pclRun);
  end;

  filenames := TStringList.Create;
  launchProjects := TLaunchProjects.Create;
  newProjects := TLaunchProjects.Create(False);
  try
    // TODO: are there any other parameters passed to TIKE?
    for i := 1 to ParamCount do
    begin
      filenames.Add(ExpandFileName(ParamStr(i)));
    end;

    if filenames.Count = 0 then
    begin
      // No filenames were passed on the command line
      if FProcesses.Count > 0 then
      begin
        // Because this is a new instance and there are already running instances,
        // we'll start Keyman Developer without opening a project, assuming that
        // the last opened project has already been opened.
        FStartupProjectPath := '';
      end;
      Exit(pclRun);
    end;

    for filename in filenames do
    begin
      projectFilename := FindOwnerProjectForFile(filename);

      p := launchProjects.Find(projectFilename);
      if p = nil then
      begin
        p := TLaunchProject.Create(projectFilename);
        launchProjects.Add(p);
      end;
      p.Filenames.Add(filename);
    end;

    // First, hand off files to existing processes, based on project folder
    for p in launchProjects do
    begin
      if not PassProjectToRunningProcess(p) then
      begin
        newProjects.Add(p);
      end;
    end;

    // If we've already passed all filenames off, then we're done
    if newProjects.Count = 0 then
    begin
      Exit(pclExit);
    end;

    // If there's more than one project left, then we need to launch new
    // processes for each one; we'll take the first for ourselves
    FStartupFilenames := newProjects[0].Filenames.ToStringArray;
    FStartupProjectPath := newProjects[0].ProjectFilename;

    newProjects.Delete(0);
    for p in newProjects do
    begin
      LaunchNewInstance(p);
    end;
  finally
    filenames.Free;
  end;

  Result := pclRun;
end;

function TTikeCommandLine.LaunchNewInstance(const project: TLaunchProject): Boolean;
var
  filename, cmdline: string;
begin
  cmdline := '"'+ParamStr(0)+'" --sub-process "'+project.ProjectFilename+'"';
  for filename in project.Filenames do
    cmdline := cmdline + ' "'+filename+'"';

  Result := TUtilExecute.Execute(cmdline, GetCurrentDir, SW_SHOWNORMAL);
end;

function TTikeCommandLine.ProcessSubProcess: Boolean;
var
  i: Integer;
begin
  if (ParamStr(1) <> '--sub-process') or (ParamCount < 2) then
  begin
    Exit(False);
  end;

  // TODO: Consider error handling

  FStartupProjectPath := ParamStr(2);
  SetLength(FStartupFilenames, ParamCount - 2);
  for i := 3 to ParamCount do
  begin
    FStartupFilenames[i-3] := ParamStr(i);
  end;

  Result := True;
end;

function TTikeCommandLine.PassProjectToRunningProcess(const project: TLaunchProject): Boolean;
var
  tp: TTikeProcess;
  filename: string;
begin
  for tp in FProcesses do
  begin
    if tp.OwnsProject(project.ProjectFilename) then
    begin
      Result := True;
      for filename in project.Filenames do
      begin
        tp.OpenFile(filename);
      end;
      Exit;
    end;
  end;
  Result := False;
end;

var
  FInstance: TTikeCommandLine = nil;

function TikeCommandLine: TTikeCommandLine;
begin
  if not Assigned(FInstance) then
    FInstance := TTikeCommandLine.Create;
  Result := FInstance;
end;

{ TLaunchProjects }

function TLaunchProjects.Find(const ProjectFilename: string): TLaunchProject;
begin
  for Result in Self do
  begin
    if Result.ProjectFilename = ProjectFilename then
    begin
      Exit;
    end;
  end;
  Result := nil;
end;

initialization
finalization
  FreeAndNil(FInstance);
end.
