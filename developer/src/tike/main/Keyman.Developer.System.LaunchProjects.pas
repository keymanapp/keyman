unit Keyman.Developer.System.LaunchProjects;

interface

uses
  System.Classes,
  System.Generics.Collections,
  System.SysUtils,

  Keyman.Developer.System.TikeMultiProcess;

type
  TLaunchProjectStatus = (lpsWaiting, lpsCurrentInstance, lpsOtherInstance, lpsNewInstance, lpsError);
  TLaunchProject = class
  private
    FProjectFilename: string;
    FFilenames: TStringList;
    FStatus: TLaunchProjectStatus;
  public
    constructor Create(const AProjectFilename: string);
    destructor Destroy; override;
    function LaunchAsNewInstance: Boolean;
    function PassToRunningProcess(AProcesses: TTikeProcessList): Boolean;
    property ProjectFilename: string read FProjectFilename;
    property Filenames: TStringList read FFilenames;
    property Status: TLaunchProjectStatus read FStatus;
  end;

  TLaunchProjects = class(TObjectList<TLaunchProject>)
  private
    FReserveStartupProject: Boolean;
    function GetStartupProject: TLaunchProject;
  public
    constructor Create(AReserveStartupProject: Boolean);
    procedure GroupFilenamesIntoProjects(const Filenames: TStringList);
    function LaunchAll(AProcesses: TTikeProcessList): Boolean;
    function Find(const ProjectFilename: string): TLaunchProject;
    class function LaunchNewEmptyInstance: Boolean;
    property ReserveStartupProject: Boolean read FReserveStartupProject;
    property StartupProject: TLaunchProject read GetStartupProject;
  end;

implementation

uses
  Winapi.Windows,

  Keyman.Developer.System.ProjectOwningFile,
  utilexecute;

{ TLaunchProject }

constructor TLaunchProject.Create(const AProjectFilename: string);
begin
  inherited Create;
  FProjectFilename := AProjectFilename;
  FFilenames := TStringList.Create;
  FStatus := lpsWaiting;
end;

destructor TLaunchProject.Destroy;
begin
  FFilenames.Free;
  inherited Destroy;
end;

function TLaunchProject.LaunchAsNewInstance: Boolean;
var
  filename, cmdline: string;
begin
  cmdline := '"'+ParamStr(0)+'" --sub-process "'+Self.ProjectFilename+'"';
  for filename in Self.Filenames do
    cmdline := cmdline + ' "'+filename+'"';

  Result := TUtilExecute.Execute(cmdline, GetCurrentDir, SW_SHOWNORMAL);

  if Result
    then FStatus := lpsNewInstance
    else FStatus := lpsError;
end;

function TLaunchProject.PassToRunningProcess(AProcesses: TTikeProcessList): Boolean;
var
  tp: TTikeProcess;
  filename: string;
begin
  for tp in AProcesses do
  begin
    if tp.OwnsProject(Self.ProjectFilename) then
    begin
      FStatus := lpsOtherInstance;

      if Self.Filenames.Count = 0 then
      begin
        // Ensure that the project file tab is opened
        tp.OpenFile(Self.ProjectFilename);
      end;

      for filename in Self.Filenames do
      begin
        tp.OpenFile(filename);
      end;
      Exit(True);
    end;
  end;
  Result := False;
end;

{ TLaunchProjects }

constructor TLaunchProjects.Create(AReserveStartupProject: Boolean);
begin
  inherited Create(True);
  FReserveStartupProject := AReserveStartupProject;
end;

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

function TLaunchProjects.GetStartupProject: TLaunchProject;
begin
  for Result in Self do
    if Result.Status = lpsCurrentInstance then
      Exit;
  Result := nil;
end;

procedure TLaunchProjects.GroupFilenamesIntoProjects(const Filenames: TStringList);
var
  projectFilename, filename: string;
  p: TLaunchProject;
begin
  for filename in filenames do
  begin
    projectFilename := FindOwnerProjectForFile(filename);

    p := Self.Find(projectFilename);
    if p = nil then
    begin
      p := TLaunchProject.Create(projectFilename);
      Self.Add(p);
    end;
    p.Filenames.Add(filename);
  end;
end;

function TLaunchProjects.LaunchAll(AProcesses: TTikeProcessList): Boolean;
var
  p: TLaunchProject;
begin
  Result := False;

  // Hand off files to existing processes, based on project folder
  for p in Self do
  begin
    if not p.PassToRunningProcess(AProcesses) then
    begin
      // If there isn't an existing process, then we need to launch a new
      // process for the project
      if not Result and ReserveStartupProject then
      begin
        // For performance, we'll take the first project for the current process
        // if requested
        p.FStatus := lpsCurrentInstance;
        Result := True;
      end
      else
      begin
        p.LaunchAsNewInstance;
      end;
    end;
  end;
end;

class function TLaunchProjects.LaunchNewEmptyInstance: Boolean;
var
  cmdline: string;
begin
  cmdline := '"'+ParamStr(0)+'" --sub-process *';
  Result := TUtilExecute.Execute(cmdline, GetCurrentDir, SW_SHOWNORMAL);
end;

end.
