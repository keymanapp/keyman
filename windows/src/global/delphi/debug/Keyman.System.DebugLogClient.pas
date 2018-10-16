unit Keyman.System.DebugLogClient;

interface

uses
  Winapi.Windows,
  JwaEventDefs,
  JwaEventTracing,
  JwaEvntProv;

type
  TDebugLogClient = class
  private
    FRegHandle: TRegHandle;
    class var FInstance: TDebugLogClient;
  public
    constructor Create;
    destructor Destroy; override;
    procedure WriteLastError(const ParentMethod, FailingMethod: string; const Message: string = '');
    procedure WriteMessage(const Format: string; const Args: array of const);

    class function Instance: TDebugLogClient;
  end;

implementation

uses
  System.SysUtils,

  Keyman.System.DebugLogCommon;

{ TDebugLogClient }

const
  // This comes from keyman-debug-etw.h. If the event descriptor is ever changed,
  // then this needs to be updated.
  DebugEvent: EVENT_DESCRIPTOR = (
    Id : $1;
    Version : $0;
    Channel : $10;
    Level   : $4;
    Opcode  : $0;
    Task    : $0;
    Keyword : $8000000000000000
  );

constructor TDebugLogClient.Create;
var
  status: ULONG;
begin
  if ShouldDebug then
  begin
    status := EventRegister(@DebugLogProviderGuid, nil, nil, FRegHandle);
    if ERROR_SUCCESS <> status then
    begin
      FRegHandle := 0;
      RaiseLastOSError(status, 'EventRegister');
    end;
  end;
end;

destructor TDebugLogClient.Destroy;
var
  status: ULONG;
begin
  if FRegHandle <> 0 then
  begin
    status := EventUnregister(FRegHandle);
    if ERROR_SUCCESS <> status then
      OutputDebugString(PChar('EventUnregister failed with '+IntToStr(status)));
  end;

  inherited Destroy;
end;

class function TDebugLogClient.Instance: TDebugLogClient;
begin
  if not Assigned(FInstance) then
    FInstance := TDebugLogClient.Create;

  Result := FInstance;
end;

procedure TDebugLogClient.WriteLastError(const ParentMethod,
  FailingMethod, Message: string);
var
  FLastError: DWord;
  FLastErrorString: string;
begin
  FLastError := GetLastError;
  FLastErrorString := SysErrorMessage(GetLastError);
  WriteMessage('ERROR %d in %s [%s]: %s %s', [FLastError, FailingMethod, ParentMethod, FLastErrorString, Message]);
end;

procedure TDebugLogClient.WriteMessage(const Format: string;
  const Args: array of const);
const
  MAX_DESCRIPTORS = 12;
var
  Descriptors: array[0..MAX_DESCRIPTORS-1] of EVENT_DATA_DESCRIPTOR;
  dwPlatform: DWORD;
  sDummy, sMsg, sProcessName: string;
  pid, tid: DWORD;
  dwTickCount, dwDummy: DWORD;
  status: ULONG;
begin
  if not ShouldDebug then
    Exit;

  pid := GetCurrentProcessId;
  tid := GetCurrentThreadId;
  dwTickCount := GetTickCount;

  dwPlatform := 1; // x86
  dwDummy := 0;
  sProcessName := 'keyman[host]';
  sMsg := System.SysUtils.Format(Format, Args);

  // These must match the manifest template in keyman-debug-etw.man
  EventDataDescCreate(Descriptors[0], @dwPlatform, sizeof(DWORD)); // <data name = "Platform" inType = "Platform" / >
  EventDataDescCreate(Descriptors[1], PChar(sProcessName), (Length(sProcessName) + 1) * sizeof(WCHAR)); //  <data name = "Process" inType = "win:UnicodeString" / >
  EventDataDescCreate(Descriptors[2], @pid, sizeof(DWORD)); //  <data name = "PID" inType = "win:UInt32" / >
  EventDataDescCreate(Descriptors[3], @tid, sizeof(DWORD)); //  <data name = "TID" inType = "win:UInt32" / >
  EventDataDescCreate(Descriptors[4], @dwDummy, sizeof(DWORD)); //  <data name = "ShiftState" inType = "ShiftState" / >
  EventDataDescCreate(Descriptors[5], @dwDummy, sizeof(DWORD)); //  <data name = "ActualShiftState" inType = "ShiftState" / >
  EventDataDescCreate(Descriptors[6], @dwTickCount, sizeof(DWORD)); //  <data name = "TickCount" inType = "win:UInt32" / >
  EventDataDescCreate(Descriptors[7], @dwDummy, sizeof(DWORD)); //  <data name = "FocusHWND" inType = "win:UInt32" / >
  EventDataDescCreate(Descriptors[8], @dwDummy, sizeof(DWORD)); //  <data name = "ActiveHKL" inType = "win:UInt32" / >
  EventDataDescCreate(Descriptors[9], PChar(sDummy), sizeof(WCHAR)); //  <data name = "SourceFile" inType = "win:UnicodeString" / >
  EventDataDescCreate(Descriptors[10], @dwDummy, sizeof(DWORD)); //  <data name = "SourceLine" inType = "win:UInt32" / >
  EventDataDescCreate(Descriptors[11], PChar(sMsg), (Length(sMsg)+1) * sizeof(WCHAR)); //  <data name = "Message" inType = "win:UnicodeString" / >

  status := EventWrite(FRegHandle, @DebugEvent, MAX_DESCRIPTORS, @Descriptors[0]);
  if ERROR_SUCCESS <> status then
    OutputDebugString(PChar('EventWrite failed with '+IntToStr(status)));
end;

initialization
finalization
  FreeAndNil(TDebugLogClient.FInstance);
end.
