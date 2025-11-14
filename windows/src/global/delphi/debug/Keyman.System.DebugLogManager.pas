(*
  Name:             DebugManager
  Copyright:        Copyright (C) SIL International.
  Documentation:
  Description:
  Create Date:      14 Sep 2006

  Modified Date:    9 Aug 2015
  Authors:          mcdurdin
  Related Files:
  Dependencies:

  Bugs:
  Todo:
  Notes:
  History:          14 Sep 2006 - mcdurdin - Initial version
                    04 Jan 2007 - mcdurdin - Add ShouldDebug function
                    19 Jun 2007 - mcdurdin - Widestring it
                    19 Nov 2007 - mcdurdin - I1157 - const string parameters
                    04 May 2010 - mcdurdin - I2348 - Rework columns in debug log
                    04 May 2010 - mcdurdin - I2349 - Hotkey to capture debug log
                    04 May 2010 - mcdurdin - I2350 - Keyman.exe should be able to report events in debug log
                    04 May 2010 - mcdurdin - I2352 - Debug logging not reliable in some apps due to security
                    15 Jun 2010 - mcdurdin - I2423 - Fix crash in debug manager
                    24 Jun 2010 - mcdurdin - I2422 - Recreate, not rewrite existing log files
                    29 Jun 2010 - mcdurdin - I2446 - Keyman Engine crashes due to debug manager being freed late
                    17 Dec 2010 - mcdurdin - Add extra column to raw strings
                    11 Jan 2011 - mcdurdin - I2640 - Raise error at appropriate location so it isn't masked by cascading issue
                    31 Jan 2011 - mcdurdin - I2685 - Reduce registry noise from ShouldDebug function
                    31 Jan 2011 - mcdurdin - I2690 - Add foreground window info to debug log
                    31 Jan 2011 - mcdurdin - I2691 - Fix handle leaks
                    18 Feb 2011 - mcdurdin - I2685 : 9827 - Fix shoulddebug refresh counter not resetting
                    03 May 2011 - mcdurdin - I2890 - Record diagnostic data when encountering registry errors
                    18 May 2012 - mcdurdin - I3306 - V9.0 - Remove TntControls + Win9x support
                    08 Jun 2012 - mcdurdin - I3310 - V9.0 - Unicode in Delphi fixes
                    28 Nov 2012 - mcdurdin - I3601 - V9.0 - Debug messages were written partially Unicode, partially ANSI
                    13 Dec 2012 - mcdurdin - I3656 - V9.0 - Debug logs show too many columns for host messages
                    01 Jan 2013 - mcdurdin - I3692 - V9.0 - [host] entries in system.log out by 1 tabstop
                    09 Aug 2015 - mcdurdin - I4843 - Log reported modifier state as well as Keyman current modifier state
*)
unit Keyman.System.DebugLogManager;  // I3306

interface

uses
  System.Classes,
  System.SysUtils,
  Winapi.Windows,
  JwaWmiStr,
  JwaEventTracing,

  Keyman.System.DebugLogCommon,
  UserMessages;

type
  TDebugLogManager = class
  private
    FOwner: HWND;
    FDebugLogIndex: Integer;
    FDebugLogFileName: string;

    pSessionProperties: PEVENT_TRACE_PROPERTIES;
    FSessionHandle: TRACEHANDLE;
    FTraceRunning: Boolean;

    procedure StartNewLogFile;
    procedure FindFirstLogFileName;

    function DebugLogFileName(n: Integer): string;
  public
    constructor Create(AOwner: HWND); reintroduce;
    destructor Destroy; override;

  end;

function GetDebugManager(AOwner: HWND): TDebugLogManager;

implementation

uses
  Accctrl,
  AclApi,
  ErrorControlledRegistry,
  KeymanPaths,
  RegistryKeys,
  Unicode;

var
  FDebugManager: TDebugLogManager = nil;
  FTerminating: Boolean = False;

  FShouldDebug: Boolean = False;
  FShouldDebugLastTick: Cardinal = 0;

const
  // GUID that identifies our trace session.
  SessionGuid: TGUID = '{FADEA67E-0EE9-452B-AF04-22E342D1227A}';

function GetDebugManager(AOwner: HWND): TDebugLogManager;
begin
  Assert(not FTerminating);

  if ShouldDebug then
  begin
    if not Assigned(FDebugManager) then
    try
      FDebugManager := TDebugLogManager.Create(AOwner);
    except
      FDebugManager := nil;
      raise;
    end;
    Result := FDebugManager;
  end
  else
    Result := nil;
end;

{ TDebugManager }

const LOGSESSION_NAME: string = 'Keyman Event Trace Session';

function StringBufferSize(const s: string): Integer;
begin
  Result := (Length(s) + 1) * sizeof(WCHAR);
end;

constructor TDebugLogManager.Create(AOwner: HWND);
var
  BufferSize: ULONG;
  status: ULONG;
begin
  FOwner := AOwner;

  FindFirstLogFileName;
  StartNewLogFile;

  // Allocate memory for the session properties. The memory must
  // be large enough to include the log file name and session name,
  // which get appended to the end of the session properties structure.

  BufferSize := sizeof(EVENT_TRACE_PROPERTIES) + StringBufferSize(FDebugLogFileName) + StringBufferSize(LOGSESSION_NAME);
  pSessionProperties := PEVENT_TRACE_PROPERTIES(AllocMem(BufferSize));

  // Set the session properties. You only append the log file name
  // to the properties structure; the StartTrace function appends
  // the session name for you.

  pSessionProperties.Wnode.BufferSize := BufferSize;
  pSessionProperties.Wnode.Flags := WNODE_FLAG_TRACED_GUID;
  pSessionProperties.Wnode.ClientContext := 1; //QPC clock resolution
  pSessionProperties.Wnode.Guid := SessionGuid;
  pSessionProperties.LogFileMode := EVENT_TRACE_FILE_MODE_CIRCULAR;
  pSessionProperties.MaximumFileSize := 256;  // 256 MB
  pSessionProperties.LoggerNameOffset := sizeof(EVENT_TRACE_PROPERTIES);
  pSessionProperties.LogFileNameOffset := sizeof(EVENT_TRACE_PROPERTIES) + sizeof(LOGSESSION_NAME);
  StrPCopy(PWideChar(PByte(pSessionProperties) + pSessionProperties.LogFileNameOffset), FDebugLogFileName);

  status := StartTrace(@FSessionHandle, PWideChar(LOGSESSION_NAME), pSessionProperties^);
  if ERROR_ALREADY_EXISTS = status then
  begin
    // The trace was already started, perhaps Keyman did not close down cleanly
    // We'll stop it and restart it
    status := ControlTraceW(FSessionHandle, PWideChar(LOGSESSION_NAME), pSessionProperties^, EVENT_TRACE_CONTROL_STOP);
    if ERROR_SUCCESS <> status then
      OutputDebugString(PChar('ControlTrace failed with '+IntToStr(status)));

    status := StartTrace(@FSessionHandle, PWideChar(LOGSESSION_NAME), pSessionProperties^);
  end;

  if ERROR_SUCCESS <> status then
    RaiseLastOSError(status, 'StartTrace');

  // Enable the providers that you want to log events to your session.

  status := EnableTraceEx(
    @DebugLogProviderGuid,
    @SessionGuid,
    FSessionHandle,
    1, // Enable
    TRACE_LEVEL_INFORMATION,
    0,
    0,
    0,
    nil
  );

  if ERROR_SUCCESS <> status then
    RaiseLastOSError(status, 'EnableTraceEx');

  FTraceRunning := True;
end;

destructor TDebugLogManager.Destroy;
var
  status: ULONG;
begin
  if FSessionHandle <> 0 then
  begin
    if FTraceRunning then
    begin
      status := EnableTraceEx(
        @DebugLogProviderGuid,
        @SessionGuid,
        FSessionHandle,
        0, // Disable
        TRACE_LEVEL_INFORMATION,
        0,
        0,
        0,
        nil
      );
      if ERROR_SUCCESS <> status then
        OutputDebugString(PChar('EnableTraceEx(FALSE) failed with '+IntToStr(status)));

      FTraceRunning := False;
    end;

    // We use ControlTraceW because JwaEventTracing has a typo for ControlTrace
    status := ControlTraceW(FSessionHandle, PWideChar(LOGSESSION_NAME), pSessionProperties^, EVENT_TRACE_CONTROL_STOP);
    if ERROR_SUCCESS <> status then
      OutputDebugString(PChar('ControlTrace failed with '+IntToStr(status)));

    FSessionHandle := 0;
  end;

  if pSessionProperties <> nil then
  begin
    FreeMem(pSessionProperties);
    pSessionProperties := nil;
  end;

  inherited Destroy;
end;

const
  MAXDEBUGLOGS = 16;

function TDebugLogManager.DebugLogFileName(n: Integer): string;
begin
  Result := TKeymanPaths.ErrorLogPath + 'system' + IntToStr(n) + '.etl';
end;

procedure TDebugLogManager.FindFirstLogFileName;
var
  FSelectedTime: TDateTime;
  i: Integer;
  f: TSearchRec;
begin
  FSelectedTime := MaxDateTime;
  FDebugLogIndex := 0;

  for i := 0 to MAXDEBUGLOGS - 1 do
  begin
    if FindFirst(DebugLogFileName(i), 0, f) = 0 then
    begin
      if f.TimeStamp < FSelectedTime then
      begin
        FDebugLogIndex := i;
        FSelectedTime := f.TimeStamp;
      end;
      System.SysUtils.FindClose(f);
    end
    else
    begin
      FDebugLogIndex := i;
      Break;
    end;
  end;

  Dec(FDebugLogIndex); // It will be immediately incremented by StartNewLogFile
end;

procedure TDebugLogManager.StartNewLogFile;
begin
  Inc(FDebugLogIndex);
  if FDebugLogIndex > MAXDEBUGLOGS then
    FDebugLogIndex := 0;

  FDebugLogFileName := DebugLogFileName(FDebugLogIndex);
  if FileExists(FDebugLogFileName) then
    System.SysUtils.DeleteFile(FDebugLogFileName);
end;


initialization
finalization
  FTerminating := True;
  FreeAndNil(FDebugManager);
end.
