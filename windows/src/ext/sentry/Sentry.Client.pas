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

  TSentryClient = class
  protected
  class var
    FInstance: TSentryClient;
  private
    options: psentry_options_t;

  public
    constructor Create(AOptions: TSentryClientOptions; ACaptureExceptions: Boolean = True); virtual;
    destructor Destroy; override;

    function MessageEvent(Level: TSentryLevel; const Logger, Message: string; IncludeStack: Boolean = True): TGUID;
    function ExceptionEvent(const ExceptionClassName, Message: string): TGUID;
  end;

  TSentryClientClass = class of TSentryClient;

procedure SentryHandleException(E: Exception);

implementation

{ TSentryClient }

(*
type
  PEXCEPTION_POINTERS = ^EXCEPTION_POINTERS;

type
  TUnhandledExceptionFilter = function(p: PEXCEPTION_POINTERS): LONG; stdcall;
var
  CrashpadFilter: TUnhandledExceptionFilter = nil;
  FLastExceptionInfo: EXCEPTION_POINTERS;

function AddVectoredExceptionHandler(first: ULONG; p: TUnhandledExceptionFilter): Pointer; stdcall; external 'kernel32.dll';
*)

procedure SentryHandleException(E: Exception);
const
  Size = 1024;
var
  Buffer: array[0..Size-1] of Char;
//  crumb: sentry_value_t;
begin
  if TSentryClient.FInstance <> nil then
  begin
    if ExceptionErrorMessage(ExceptObject, ExceptAddr, Buffer, Size) > 0 then
      TSentryClient.FInstance.ExceptionEvent(E.ClassName, Buffer);
//    begin
//      crumb := sentry_value_new_breadcrumb('default', PAnsiChar(UTF8Encode(Buffer)));
//      sentry_value_set_by_key(crumb, 'category', sentry_value_new_string('Exception'));
//      sentry_value_set_by_key(crumb, 'level', sentry_value_new_string('error'));
//      sentry_add_breadcrumb(crumb);
//    end;

//    TSentryClient.FInstance.AddBreadcrumb('Exception', Buffer);
//    TSentryClient.FInstance.AddBreadcrumb('ExceptionClass', ExceptObject.ClassName);

//    if @CrashpadFilter <> nil then
//      CrashpadFilter(@FLastExceptionInfo);
  end;
end;

(*
const
  EXCEPTION_CONTINUE_SEARCH = 0;

function FirstHandler(ExceptionInfo: PEXCEPTION_POINTERS): LONG; stdcall;
begin
  FLastExceptionInfo := ExceptionInfo^;
  Result := EXCEPTION_CONTINUE_SEARCH;
end;
*)

procedure SentryExceptHandler(ExceptObject: TObject; ExceptAddr: Pointer);
const
  Size = 1024;
var
  Buffer: array[0..Size-1] of Char;
begin
  if TSentryClient.FInstance <> nil then
  begin
    if ExceptionErrorMessage(ExceptObject, ExceptAddr, Buffer, Size) > 0 then
//  try
//      TSentryClient.FInstance.AddBreadcrumb('Exception', Buffer);
//      TSentryClient.FInstance.AddBreadcrumb('ExceptionClass', ExceptObject.ClassName);
      TSentryClient.FInstance.ExceptionEvent(ExceptObject.ClassName, Buffer);
  end;
//  except
//
//  end;
end;

(*
function AnnotateExceptions(
  event: sentry_value_t;
  hint: Pointer;
  closure: Pointer
): sentry_value_t; cdecl;
const
  Size = 1024;
var
  Buffer: array[0..Size-1] of Char;
begin
  if ExceptObject <> nil then
  begin
    if ExceptionErrorMessage(ExceptObject, ExceptAddr, Buffer, Size) > 0 then
      sentry_value_set_by_key(event, 'message', sentry_value_new_string(PAnsiChar(AnsiString(Buffer))));
  end;
  Result := event;
end;*)

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

  (*
  if AOptions.HandlerPath <> '' then
    sentry_options_set_handler_pathw(options, PChar(AOptions.HandlerPath));

  if AOptions.DatabasePath <> '' then
    sentry_options_set_database_pathw(options, PChar(AOptions.DatabasePath));
  *)

//  sentry_options_set_before_send(options, AnnotateExceptions, nil);

  sentry_init(options);


  if ACaptureExceptions then
  begin
//    AddVectoredExceptionHandler(1, FirstHandler);
//    CrashPadFilter := SetUnhandledExceptionFilter(nil);
//    SetUnhandledExceptionFilter(@CrashPadFilter);
    ExceptProc := @SentryExceptHandler;
    // TODO: Application.OnException
//    JITEnable := 2;
  end;
end;

destructor TSentryClient.Destroy;
begin
  FInstance := nil;
  sentry_shutdown;
  inherited Destroy;
end;

function RtlCaptureStackBackTrace(FramesToSkip, FramesToCapture: DWORD; BackTrace: Pointer; BackTraceHash: PDWORD): WORD; stdcall; external 'ntdll.dll';

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

function TSentryClient.ExceptionEvent(const ExceptionClassName, Message: string): TGUID;
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

(*
  When we set exception information, the report is corrupted. Not sure why. So
  for now we won't create as an exception event. We still get all the information
  we want from this.

  exc := sentry_value_new_object;
  sentry_value_set_by_key(exc, 'type', sentry_value_new_string(PAnsiChar(UTF8Encode(ExceptionClassName))));
  sentry_value_set_by_key(exc, 'value', sentry_value_new_string(PAnsiChar(UTF8Encode(Message))));
  sentry_value_set_by_key(event, 'exception', exc);
*)

  sentry_value_set_by_key(event, 'message', sentry_value_new_string(PAnsiChar(UTF8Encode(Message))));

  threads := CaptureStackTrace(ExceptAddr, FRAMES_TO_SKIP);
  if threads <> 0 then
    sentry_value_set_by_key(event, 'threads', threads);

  uuid := sentry_capture_event(event);
  Result := uuid.native_uuid;
end;

function TSentryClient.MessageEvent(Level: TSentryLevel; const Logger,
  Message: string; IncludeStack: Boolean): TGUID;
var
  event: sentry_value_t;
  threads: sentry_value_t;
const
  FRAMES_TO_SKIP = 2;
  // We are not interested in just the first two frames:
  //    Sentry.Client.CaptureStackTrace,
  //    Sentry.Client.TSentryClient.MessageEvent
begin
	event := sentry_value_new_message_event(
		{*   level *} sentry_level_t(Level),
		{*  logger *} PAnsiChar(UTF8Encode(Logger)),
		{* message *} PAnsiChar(UTF8Encode(Message))
	);

  if IncludeStack then
  begin
    threads := CaptureStackTrace(nil, FRAMES_TO_SKIP);
    if threads <> 0 then
      sentry_value_set_by_key(event, 'threads', threads);
  end;

  Result := sentry_capture_event(event).native_uuid;
end;

end.
