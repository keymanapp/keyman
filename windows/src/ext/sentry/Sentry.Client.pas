{$D+}
unit Sentry.Client;

interface

uses
  System.AnsiStrings,
  System.SysUtils,
  Winapi.ImageHlp,
  Winapi.Windows,

  sentry;

type
  TSentryClientOptions = record
    DSN: string;
    Release: string;
    Environment: string;
    Dist: string;
    HttpProxy: string;
    CACerts: string;
    Debug: Boolean;
    HandlerPath: string;
    DatabasePath: string;
  end;

  // TSentryLevel is a mirror of sentry_level_t so that consumers of this
  // API don't need to reference the low level sentry API
  TSentryLevel = (
    SENTRY_LEVEL_DEBUG = -1,
    SENTRY_LEVEL_INFO = 0,
    SENTRY_LEVEL_WARNING = 1,
    SENTRY_LEVEL_ERROR = 2,
    SENTRY_LEVEL_FATAL = 3
  );

  TSentryClientEventType = (scetException, scetMessage);
  TSentryClientEventAction = (sceaContinue, sceaTerminate);

  TSentryClientBeforeEvent = procedure(Sender: TObject; EventType: TSentryClientEventType;
    event: sentry_value_t;
    const EventClassName, Message: string;
    var EventAction: TSentryClientEventAction) of object;

  TSentryClientAfterEvent = procedure(Sender: TObject; EventType: TSentryClientEventType;
    const EventID, EventClassName, Message: string;
    var EventAction: TSentryClientEventAction) of object;

  TSentryClient = class
  protected
  class var
    FInstance: TSentryClient;
  private
    options: psentry_options_t;
    FOnBeforeEvent: TSentryClientBeforeEvent;
    FOnAfterEvent: TSentryClientAfterEvent;
    procedure DoAfterEvent(const EventID, ExceptionClassName, Message: string;
      EventType: TSentryClientEventType);
    procedure DoBeforeEvent(event: sentry_value_t;
      const ExceptionClassName, Message: string;
      EventType: TSentryClientEventType);
    procedure DoTerminate;
    function EventIDToString(Guid: TGUID): string;
  public
    constructor Create(AOptions: TSentryClientOptions; ACaptureExceptions: Boolean = True); virtual;
    destructor Destroy; override;

    function MessageEvent(Level: TSentryLevel; const Logger, Message: string; IncludeStack: Boolean = False): TGUID;
    function ExceptionEvent(const ExceptionClassName, Message: string; AExceptAddr: Pointer = nil): TGUID;

    property OnBeforeEvent: TSentryClientBeforeEvent read FOnBeforeEvent write FOnBeforeEvent;
    property OnAfterEvent: TSentryClientAfterEvent read FOnAfterEvent write FOnAfterEvent;
  end;

  TSentryClientClass = class of TSentryClient;

  // When testing Sentry integration, ESentryTest is an appropriate exception to
  // raise.
  ESentryTest = class(Exception)
  end;

procedure SentryHandleException(E: Exception; AExceptAddr: Pointer = nil);

implementation

{ TSentryClient }

//
// Handler for try/except blocks. Call SentryHandleException
// on outer blocks to report any unhandled exceptions.
//
procedure SentryHandleException(E: Exception; AExceptAddr: Pointer);
const
  Size = 1024;
var
  Buffer: array[0..Size-1] of Char;
begin
  if TSentryClient.FInstance <> nil then
  begin
    if AExceptAddr = nil then
      AExceptAddr := System.ExceptAddr;
    if ExceptionErrorMessage(E, AExceptAddr, Buffer, Size) > 0 then
      TSentryClient.FInstance.ExceptionEvent(E.ClassName, Buffer);
  end;
end;

//
// Last-gasp exception handler assigned to ExceptProc.
// Most processes drop their exceptions into SentryHandleException
//
procedure SentryExceptHandler(ExceptObject: TObject; ExceptAddr: Pointer);
const
  Size = 1024;
var
  Buffer: array[0..Size-1] of Char;
begin
  if TSentryClient.FInstance <> nil then
  begin
    if ExceptionErrorMessage(ExceptObject, ExceptAddr, Buffer, Size) > 0 then
      TSentryClient.FInstance.ExceptionEvent(ExceptObject.ClassName, Buffer);
  end;
end;


constructor TSentryClient.Create(AOptions: TSentryClientOptions; ACaptureExceptions: Boolean = True);
begin
  Assert(not Assigned(FInstance));
  FInstance := Self;

  inherited Create;
	options := sentry_options_new;

  if AOptions.DSN <> '' then
    sentry_options_set_dsn(options, PAnsiChar(UTF8Encode(AOptions.DSN)));

  if AOptions.Release <> '' then
    sentry_options_set_release(options, PAnsiChar(UTF8Encode(AOptions.Release)));

  if AOptions.Environment <> '' then
    sentry_options_set_environment(options, PAnsiChar(UTF8Encode(AOptions.Environment)));

  if AOptions.Dist <> '' then
    sentry_options_set_dist(options, PAnsiChar(UTF8Encode(AOptions.Dist)));

  if AOptions.HttpProxy <> '' then
    sentry_options_set_http_proxy(options, PAnsiChar(UTF8Encode(AOptions.HttpProxy)));

  if AOptions.CACerts <> '' then
    sentry_options_set_ca_certs(options, PAnsiChar(UTF8Encode(AOptions.CACerts)));

  if AOptions.Debug then
    sentry_options_set_debug(options, 1);

  sentry_init(options);


  if ACaptureExceptions then
  begin
    ExceptProc := @SentryExceptHandler;
  end;
end;

destructor TSentryClient.Destroy;
begin
  FInstance := nil;
  sentry_shutdown;
  inherited Destroy;
end;

function RtlCaptureStackBackTrace(FramesToSkip, FramesToCapture: DWORD; BackTrace: Pointer; BackTraceHash: PDWORD): WORD; stdcall; external 'ntdll.dll';

//
// Capture a stack trace and include the offending crash address at the top of
// the trace. Apart from skipping frames and the inclusion of TopAddr, this is
// very similar to sentry_event_value_add_stacktrace.
//
function CaptureStackTrace(TopAddr: Pointer; FramesToSkip: DWORD): sentry_value_t;
var
  walked_backtrace: array[0..255] of Pointer;
  s: Ansistring;
  frameCount: Word;
  frame, frames: sentry_value_t;
  stacktrace: sentry_value_t;
  i: Integer;
  threads: sentry_value_t;
  thread: sentry_value_t;
begin
  frameCount := RtlCaptureStackBackTrace(FramesToSkip, 256, @walked_backtrace[0], nil);
  if frameCount = 0 then
    Exit(0);

  frames := sentry_value_new_list;

  for i := frameCount - 1 downto 0 do
  begin
    frame := sentry_value_new_object;
    s := System.AnsiStrings.Format('0x%x', [NativeUInt(walked_backtrace[i])]);
    sentry_value_set_by_key(frame, 'instruction_addr', sentry_value_new_string(PAnsiChar(s)));
    sentry_value_append(frames, frame);
  end;

  // Insert the except address at the top of the stack
  if TopAddr <> nil then
  begin
    frame := sentry_value_new_object;
    s := System.AnsiStrings.Format('0x%x', [NativeUInt(TopAddr)]);
    sentry_value_set_by_key(frame, 'instruction_addr', sentry_value_new_string(PAnsiChar(s)));
    sentry_value_append(frames, frame);
  end;

  stacktrace := sentry_value_new_object;
  sentry_value_set_by_key(stacktrace, 'frames', frames);

  threads := sentry_value_new_list;
  thread := sentry_value_new_object;
  sentry_value_set_by_key(thread, 'stacktrace', stacktrace);
  sentry_value_append(threads, thread);

  Result := threads;
end;

//
// Force the application to terminate abruptly, but give
// time for Sentry to report outstanding events.
//
procedure TSentryClient.DoTerminate;
begin
  sentry_shutdown;
  ExitProcess(1);
end;

procedure TSentryClient.DoBeforeEvent(event: sentry_value_t;
  const ExceptionClassName, Message: string;
  EventType: TSentryClientEventType);
var
  EventAction: TSentryClientEventAction;
begin
  if Assigned(FOnBeforeEvent) then
  begin
    EventAction := sceaContinue;
    FOnBeforeEvent(Self, EventType, event, ExceptionClassName, Message, EventAction);
    if EventAction = sceaTerminate then
      DoTerminate;
  end;
end;

procedure TSentryClient.DoAfterEvent(const EventID, ExceptionClassName, Message: string;
  EventType: TSentryClientEventType);
var
  EventAction: TSentryClientEventAction;
begin
  if Assigned(FOnAfterEvent) then
  begin
    EventAction := sceaContinue;
    FOnAfterEvent(Self, EventType, EventID, ExceptionClassName, Message, EventAction);
    if EventAction = sceaTerminate then
      DoTerminate;
  end;
end;

function TSentryClient.EventIDToString(Guid: TGUID): string;
begin
  // Copied from System.SysUtils.GuidToString and cleaned up for use with Sentry
  SetLength(Result, 32);
  StrLFmt(PChar(Result), 38,'%.8X%.4X%.4X%.2X%.2X%.2X%.2X%.2X%.2X%.2X%.2X',   // do not localize
    [Guid.D1, Guid.D2, Guid.D3, Guid.D4[0], Guid.D4[1], Guid.D4[2], Guid.D4[3],
    Guid.D4[4], Guid.D4[5], Guid.D4[6], Guid.D4[7]]);
end;

function TSentryClient.ExceptionEvent(const ExceptionClassName, Message: string; AExceptAddr: Pointer = nil): TGUID;
var
  event: sentry_value_t;
  uuid: sentry_uuid_t;
  threads: sentry_value_t;
const
  FRAMES_TO_SKIP = 4;
  // We are not interested in the first three frames:
  //    Sentry.Client.CaptureStackTrace,
  //    Sentry.Client.TSentryClient.ExceptionEvent
  //    Sentry.Client.SentryHandleException
  //    exception handler that captured the event
  //    Note: there may be additional frames we can't deal with
  //    A pseudo-frame is inserted at the top of the stack which points
  //    to the address of the code that caused the exception
begin
  event := sentry_value_new_event;

  DoBeforeEvent(event, ExceptionClassName, Message, scetException);

(*
  When we set exception information, the report is corrupted. Not sure why. So
  for now we won't create as an exception event. We still get all the information
  we want from this.

  Investigating this further at https://forum.sentry.io/t/corrupted-display-when-exception-data-is-set-using-native-sdk/9167/2

  exc := sentry_value_new_object;
  sentry_value_set_by_key(exc, 'type', sentry_value_new_string(PAnsiChar(UTF8Encode(ExceptionClassName))));
  sentry_value_set_by_key(exc, 'value', sentry_value_new_string(PAnsiChar(UTF8Encode(Message))));
  sentry_value_set_by_key(event, 'exception', exc);
*)

  sentry_value_set_by_key(event, 'message', sentry_value_new_string(PAnsiChar(UTF8Encode(Message))));

  if AExceptAddr = nil then
    AExceptAddr := System.ExceptAddr;
  threads := CaptureStackTrace(AExceptAddr, FRAMES_TO_SKIP);
  if threads <> 0 then
    sentry_value_set_by_key(event, 'threads', threads);

  uuid := sentry_capture_event(event);
  Result := uuid.native_uuid;

  DoAfterEvent(EventIDToString(Result), ExceptionClassName, Message, scetException);
end;

function TSentryClient.MessageEvent(Level: TSentryLevel; const Logger,
  Message: string; IncludeStack: Boolean): TGUID;
var
  event: sentry_value_t;
  threads: sentry_value_t;
const
  FRAMES_TO_SKIP = 2;
  // We are not interested in the first two frames:
  //    Sentry.Client.CaptureStackTrace,
  //    Sentry.Client.TSentryClient.MessageEvent
begin
	event := sentry_value_new_message_event(
		{*   level *} sentry_level_t(Level),
		{*  logger *} PAnsiChar(UTF8Encode(Logger)),
		{* message *} PAnsiChar(UTF8Encode(Message))
	);

  DoBeforeEvent(event, Logger, Message, scetMessage);

  if IncludeStack then
  begin
    threads := CaptureStackTrace(nil, FRAMES_TO_SKIP);
    if threads <> 0 then
      sentry_value_set_by_key(event, 'threads', threads);
  end;

  Result := sentry_capture_event(event).native_uuid;

  DoAfterEvent(EventIDToString(Result), Logger, Message, scetMessage);
end;

end.
