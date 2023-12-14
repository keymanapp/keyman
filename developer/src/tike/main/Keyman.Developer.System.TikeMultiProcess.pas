unit Keyman.Developer.System.TikeMultiProcess;

interface

uses
  System.Generics.Collections,
  System.Generics.Defaults,
  Keyman.Developer.System.MultiProcess;

type
  TTikeProcess = class
    WindowHandle: THandle;
    ThreadID: Cardinal;
    ProjectFilename: string;
    SourcePath: string;
    IsProjectOpen: Boolean;
    function IsTemporaryProject: Boolean;
    function OwnsProject(const filename: string): Boolean;
    function OpenFile(const filename: string): Boolean;
    function FocusProcess: Boolean;
  private
    function GetTitle: string;
  public
    property Title: string read GetTitle;
  end;

  TTikeProcessList = TObjectList<TTikeProcess>;

  TTikeMultiProcess = class
  public
    constructor Create;
    destructor Destroy; override;
    procedure CloseProject;
    procedure OpenProject(const filename, sourcepath: string);
    function Enumerate: TTikeProcessList;
  end;

function TikeMultiProcess: TTikeMultiProcess;

implementation

uses
  System.JSON,
  System.SysUtils,
  Winapi.Messages,
  Winapi.Windows,

  Keyman.System.CopyDataHelper,
  RegistryKeys,
  UfrmMain,
  utilfiletypes;

const
  JSON_Filename = 'Filename';
  JSON_SourcePath = 'SourcePath';

{ TTikeMultiProcess }

procedure TTikeMultiProcess.CloseProject;
begin
  // Marks this instance as available to open another project
  MultiProcessCoordinator.SetProcessIdentifier('*');
end;

constructor TTikeMultiProcess.Create;
begin
  inherited Create;
  CreateMultiProcessCoordinator(TfrmKeymanDeveloper.ClassName, SRegKey_IDEActiveProjects_CU);
  CloseProject;
end;

destructor TTikeMultiProcess.Destroy;
begin
  inherited Destroy;
end;

type
  TSortByProjectFilename = class(TComparer<TTikeProcess>)
    function Compare(const Left, Right: TTikeProcess): Integer; override;
  end;

function TTikeMultiProcess.Enumerate: TTikeProcessList;
var
  tp: TTikeProcess;
  p: TMultiProcessInstance;
  j: TJSONValue;
  jo: TJSONObject;
  SortByProjectFilename: TSortByProjectFilename;
begin
  Result := TObjectList<TTikeProcess>.Create;
  MultiProcessCoordinator.Enumerate;
  for p in MultiProcessCoordinator.Processes do
  begin
    if p.Identifier = '*' then
    begin
      tp := TTikeProcess.Create;
      tp.IsProjectOpen := False;
      tp.ProjectFilename := '';
      tp.SourcePath := '';
      tp.WindowHandle := p.Handle;
      tp.ThreadID := p.ThreadId;
      Result.Add(tp);
    end;
    j := TJSONObject.ParseJSONValue(p.Identifier);
    try
      if (j = nil) or not (j is TJSONObject) then
      begin
        Continue;
      end;

      jo := j as TJSONObject;
      if not (jo.Values[JSON_Filename] is TJSONString) or
        not (jo.Values[JSON_SourcePath] is TJSONString) then
      begin
        Continue;
      end;

      tp := TTikeProcess.Create;
      tp.IsProjectOpen := True;
      tp.ProjectFilename := jo.Values[JSON_Filename].Value;
      tp.SourcePath := jo.Values[JSON_SourcePath].Value;
      tp.WindowHandle := p.Handle;
      tp.ThreadID := p.ThreadId;
      Result.Add(tp);
    finally
      j.Free;
    end;
  end;

  SortByProjectFilename := TSortByProjectFilename.Create;
  try
    Result.Sort(SortByProjectFilename);
  finally
    SortByProjectFilename.Free;
  end;
end;

procedure TTikeMultiProcess.OpenProject(const filename, sourcepath: string);
var
  j: TJSONObject;
begin
  j := TJSONObject.Create;
  try
    j.AddPair(JSON_Filename, filename);
    j.AddPair(JSON_SourcePath, sourcepath);
    MultiProcessCoordinator.SetProcessIdentifier(j.ToJSON);
  finally
    j.Free;
  end;
end;

{ TTikeProcess }

function TTikeProcess.FocusProcess: Boolean;
begin
  if not AttachThreadInput(GetCurrentThreadId, ThreadID, True) then
    Exit(False);

  Result := BringWindowToTop(WindowHandle);
  SetForegroundWindow(WindowHandle);

  AttachThreadInput(GetCurrentThreadId, ThreadID, False);
end;

function TTikeProcess.GetTitle: string;
begin
  if not IsProjectOpen then
    Result := '(No project loaded)'
  else if IsTemporaryProject then
    Result := 'Temporary Project'
  else
    Result := ProjectFilename;
end;

function TTikeProcess.IsTemporaryProject: Boolean;
begin
  Result := IsProjectOpen and (ProjectFilename = '');
end;

function TTikeProcess.OpenFile(const filename: string): Boolean;
begin
  if not TCopyDataHelper.SendData(0, WindowHandle, TCopyDataCommand.CD_OPENFILE, filename) then
  begin
    // The target app did not receive the data
    Exit(False);
  end;

  Exit(FocusProcess);
end;

function TTikeProcess.OwnsProject(const filename: string): Boolean;
begin
  Result := SameFileName(filename, ProjectFilename);
end;

var
  FInstance: TTikeMultiProcess = nil;

function TikeMultiProcess: TTikeMultiProcess;
begin
  if not Assigned(FInstance) then
  begin
    FInstance := TTikeMultiProcess.Create;
  end;
  Result := FInstance;
end;

{ TSortByProjectFilename }

function TSortByProjectFilename.Compare(const Left,
  Right: TTikeProcess): Integer;
begin
  Result := StrComp(PChar(Left.Title), PChar(Right.Title));
end;

initialization
finalization
  FreeAndNil(FInstance);
end.
