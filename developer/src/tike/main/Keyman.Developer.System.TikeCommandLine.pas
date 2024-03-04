unit Keyman.Developer.System.TikeCommandLine;

interface

uses
  System.Classes,
  System.Generics.Collections,

  Keyman.Developer.System.TikeMultiProcess;

type
  TProcessCommandLine = (pclRun, pclExit);

type
  TTikeCommandLine = class
  strict private
    FProcesses: TTikeProcessList;
  private
    FStartupProjectPath: string;
    FStartupFilenames: TArray<string>;
    function ProcessSubProcess: Boolean;
    function GetFilenamesFromCommandLine(filenames: TStringList): Boolean;
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
  Vcl.Dialogs,
  Winapi.Windows,

  Keyman.Developer.System.LaunchProjects,
  KeymanDeveloperOptions,
  utilexecute;

{ TTikeCommandLine }

constructor TTikeCommandLine.Create;
begin
  inherited Create;

  FStartupProjectPath := FKeymanDeveloperOptions.StartupProjectPath;

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

function TTikeCommandLine.GetFilenamesFromCommandLine(filenames: TStringList): Boolean;
var
  filename, missingFilenames: string;
  NoMoreSwitches: Boolean;
  i: Integer;
begin
  missingFilenames := '';
  NoMoreSwitches := False;

  for i := 1 to ParamCount do
  begin
    if not NoMoreSwitches and ParamStr(i).StartsWith('-') then
    begin
      // We will treat all `-x` and `--x` parameters as command-line switches,
      // which provides forward compatibility for when we want to support
      // additional switches. A single `--` parameter tells us that remaining
      // parameters are filenames, even if they start with `-`.
      if ParamStr(i) = '--' then
        NoMoreSwitches := True;
      Continue;
    end;

    filename := ExpandFileName(ParamStr(i));
    if FileExists(filename)
      then filenames.Add(filename)
      else missingFilenames := missingFilenames + '• ' + filename + #13#10;
  end;

  if missingFilenames <> '' then
  begin
    ShowMessage('The following file(s) could not be found:'#13#10+missingFilenames);
    if filenames.Count = 0 then
      // If we only had bogus filenames passed in then we should abort
      Exit(False);
  end;

  Result := True;
end;

function TTikeCommandLine.ProcessSubProcess: Boolean;
var
  i: Integer;
begin
  if (ParamStr(1) <> '--sub-process') or (ParamCount < 2) then
  begin
    Exit(False);
  end;

  // TODO(lowpri): Consider error handling

  if ParamStr(2) = '*' then
  begin
    // Asked to open a new empty project, via Window|New
    FStartupProjectPath := '';
    SetLength(FStartupFilenames, 0);
    Exit(True);
  end;

  FStartupProjectPath := ParamStr(2);
  SetLength(FStartupFilenames, ParamCount - 2);
  for i := 3 to ParamCount do
  begin
    FStartupFilenames[i-3] := ParamStr(i);
  end;

  Result := True;
end;

/// Reads filenames passed on command line, and determines if they should be
/// opened in an existing instance of Keyman Developer or in this instance, or
/// even in multiple new instances
function TTikeCommandLine.Process: TProcessCommandLine;
var
  filenames: TStringList;
  projects: TLaunchProjects;
begin
  // If launched by LaunchNewInstance then we'll process that and continue
  if ProcessSubProcess then
  begin
    Exit(pclRun);
  end;

  filenames := TStringList.Create;
  projects := TLaunchProjects.Create(True);
  try
    // Collect filenames passed in on command line
    if not GetFilenamesFromCommandLine(filenames) then
      Exit(pclExit);

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

    // Sort filenames into project groupings

    projects.GroupFilenamesIntoProjects(filenames);

    // Launch new processes or load files into existing processes

    if not projects.LaunchAll(FProcesses)
      then Result := pclExit
      else Result := pclRun;

    if Assigned(projects.StartupProject) then
    begin
      // LaunchAll will leave one startup project for this current process
      // instance to load
      FStartupFilenames := projects.StartupProject.Filenames.ToStringArray;
      FStartupProjectPath := projects.StartupProject.ProjectFilename;
    end;
  finally
    projects.Free;
    filenames.Free;
  end;
end;

var
  FInstance: TTikeCommandLine = nil;

function TikeCommandLine: TTikeCommandLine;
begin
  if not Assigned(FInstance) then
    FInstance := TTikeCommandLine.Create;
  Result := FInstance;
end;

initialization
finalization
  FreeAndNil(FInstance);
end.
