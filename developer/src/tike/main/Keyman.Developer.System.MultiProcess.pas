unit Keyman.Developer.System.MultiProcess;

interface

uses
  System.Generics.Collections,

  System.Win.Registry;

type
  TMultiProcessInstance = class
    ThreadId: Cardinal;
    Handle: THandle;
    Identifier: string;
  end;

  TMultiProcessCoordinator = class
  private
    FWindowHandles: TList<THandle>;
    FProcesses: TObjectList<TMultiProcessInstance>;
    FParentWindowClassName: string;
    FRegistryKey: string;
    function EnumWindowsProc(hwnd: THandle): Boolean;
    function OpenActiveProcessKey: TRegistry;
    procedure ClearProcessIdentifier;
    function CurrentProcessValueName: string;
  public
    constructor Create(const ParentWindowClassName, RegistryKey: string);
    destructor Destroy; override;
    procedure Enumerate;
    procedure SetProcessIdentifier(const Identifier: string);
    procedure CleanupStaleRegisteredInstances;

    property Processes: TObjectList<TMultiProcessInstance> read FProcesses;
  end;

function MultiProcessCoordinator: TMultiProcessCoordinator;
procedure CreateMultiProcessCoordinator(ParentWindowClassName, RegistryKey: string);

implementation

uses
  System.Classes,
  System.SysUtils,
  Winapi.Windows;

{ TMultiProcessCoordinator }

constructor TMultiProcessCoordinator.Create(const ParentWindowClassName, RegistryKey: string);
begin
  inherited Create;
  FParentWindowClassName := ParentWindowClassName;
  FRegistryKey := RegistryKey;
  FWindowHandles := TList<THandle>.Create;
  FProcesses := TObjectList<TMultiProcessInstance>.Create;
  SetProcessIdentifier('');
end;

destructor TMultiProcessCoordinator.Destroy;
begin
  ClearProcessIdentifier;
  FWindowHandles.Free;
  FProcesses.Free;
  inherited Destroy;
end;

function _enumwindowsproc(hwnd: THandle; lParam: LPARAM): BOOL; stdcall;
begin
  Result := TMultiProcessCoordinator(lParam).EnumWindowsProc(hwnd);
end;

procedure TMultiProcessCoordinator.Enumerate;
var
  h: THandle;
  reg: TRegistry;
  p: TMultiProcessInstance;
  tid: DWord;
begin
  // Looks for windows with the matching class name, and then
  // matches those with running instances

  FWindowHandles.Clear;
  FProcesses.Clear;
  EnumWindows(@_enumwindowsproc, LPARAM(Self));

  reg := OpenActiveProcessKey;
  try
    for h in FWindowHandles do
    begin
      tid := GetWindowThreadProcessId(h);
      if (tid <> 0) and reg.ValueExists(IntToStr(tid)) then
      begin
        p := TMultiProcessInstance.Create;
        p.ThreadId := tid;
        p.Handle := h;
        p.Identifier := reg.ReadString(IntToStr(tid));
        FProcesses.Add(p);
      end;
    end;
  finally
    reg.Free;
  end;
end;

function TMultiProcessCoordinator.EnumWindowsProc(hwnd: THandle): Boolean;
var
  szbuf: array[0..32] of char;
begin
  Result := True;

  if GetClassName(hwnd, szbuf, 32) = 0 then
    Exit;

  if FParentWindowClassName <> szbuf then
    Exit;

  FWindowHandles.Add(hwnd);
end;

/// Looks for windows with the matching class name, and then
/// matches those with running instances. Stale instances can be left
/// if an instance crashes or if Windows does not shutdown cleanly.
/// There is a slight risk of a race here, if a new process is started
/// between window enumeration and the loop in this function, but it is
/// not worth introducing the extra complexity to avoid this rare situation.
procedure TMultiProcessCoordinator.CleanupStaleRegisteredInstances;
  function ProcessListHasTid(tid: Cardinal): Boolean;
  var
    p: TMultiProcessInstance;
  begin
    for p in FProcesses do
    begin
      if p.ThreadId = tid then
      begin
        Exit(True);
      end;
    end;
    Result := False;
  end;
var
  reg: TRegistry;
  tidString: string;
  tid: Cardinal;
  tids: TStrings;
begin
  Enumerate;

  reg := OpenActiveProcessKey;
  tids := TStringList.Create;
  try
    reg.GetValueNames(tids);
    for tidString in tids do
    begin
      tid := StrToIntDef(tidString, 0);
      if not ProcessListHasTid(tid) then
      begin
        reg.DeleteValue(tidString);
      end;
    end;
  finally
    tids.Free;
    reg.Free;
  end;
end;

procedure TMultiProcessCoordinator.SetProcessIdentifier(const Identifier: string);
var
  reg: TRegistry;
begin
  reg := OpenActiveProcessKey;
  try
    reg.WriteString(CurrentProcessValueName, Identifier);
  finally
    reg.Free;
  end;
end;

procedure TMultiProcessCoordinator.ClearProcessIdentifier;
var
  reg: TRegistry;
begin
  reg := OpenActiveProcessKey;
  try
    if reg.ValueExists(CurrentProcessValueName) then
      reg.DeleteValue(CurrentProcessValueName);
  finally
    reg.Free;
  end;
end;

function TMultiProcessCoordinator.OpenActiveProcessKey: TRegistry;
begin
  Result := TRegistry.Create;
  if not Result.OpenKey(FRegistryKey, True) then
  begin
    FreeAndNil(Result);
    RaiseLastOSError;
  end;
end;

function TMultiProcessCoordinator.CurrentProcessValueName: string;
begin
  Result := IntToStr(GetCurrentThreadId);
end;

//------------------------------------------------------------------------------

var
  FInstance: TMultiProcessCoordinator = nil;

function MultiProcessCoordinator: TMultiProcessCoordinator;
begin
  Result := FInstance;
end;

procedure CreateMultiProcessCoordinator(ParentWindowClassName, RegistryKey: string);
begin
  Assert(FInstance = nil);
  FInstance := TMultiProcessCoordinator.Create(ParentWindowClassName, RegistryKey);
end;

initialization
finalization
  FreeAndNil(FInstance);
end.
