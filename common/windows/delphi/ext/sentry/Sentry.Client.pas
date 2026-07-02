{$D+}
unit Sentry.Client;

interface

uses
  System.AnsiStrings,
  System.SysUtils,
  Winapi.ImageHlp,
  Winapi.Windows,

  jwaimagehlp,
  jwantstatus,
  jwawinbase,

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

  TSentryClientFlag = (scfCaptureExceptions, scfReportExceptions, scfReportMessages);
  TSentryClientFlags = set of TSentryClientFlag;

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
    FEnabled: Boolean;
    FEnabledInitialised: Boolean;
  private
    FSentryInit: Boolean;
    options: psentry_options_t;
    FVectoredExceptionHandler: PVOID;

    FLogger: string;
    FOnBeforeEvent: TSentryClientBeforeEvent;
    FOnAfterEvent: TSentryClientAfterEvent;
    FReportExceptions: Boolean;
    FReportMessages: Boolean;
    procedure DoAfterEvent(const EventID, ExceptionClassName, Message: string;
      EventType: TSentryClientEventType);
    procedure DoBeforeEvent(event: sentry_value_t;
      const ExceptionClassName, Message: string;
      EventType: TSentryClientEventType);
    procedure DoTerminate;
    function EventIDToString(AGuid: PByte): String;
    function ConvertRawStackToSentryStack(wrapWithThread: Boolean): sentry_value_t;
    class function GetEnabled: Boolean; static;
  public
    constructor Create(AOptions: TSentryClientOptions; const ALogger: string; AFlags: TSentryClientFlags); virtual;
    destructor Destroy; override;

    function MessageEvent(Level: TSentryLevel; const Message: string; IncludeStack: Boolean = False): string;
    function ExceptionEvent(const ExceptionClassName, Message: string; AExceptAddr: Pointer = nil): string;

    procedure Breadcrumb(const BreadcrumbType, Message: string; const Category: string = ''; const Level: string = 'info');

    property OnBeforeEvent: TSentryClientBeforeEvent read FOnBeforeEvent write FOnBeforeEvent;
    property OnAfterEvent: TSentryClientAfterEvent read FOnAfterEvent write FOnAfterEvent;

    property ReportExceptions: Boolean read FReportExceptions;
    property ReportMessages: Boolean read FReportMessages;
  public
    class property Enabled: Boolean read GetEnabled;
  end;

  TSentryClientClass = class of TSentryClient;

  // When testing Sentry integration, ESentryTest is an appropriate exception to
  // raise.
  ESentryTest = class(Exception)
  end;

function SentryHandleException(E: Exception; AExceptAddr: Pointer = nil): Boolean;

implementation

const
  MAX_FRAMES = 64;

threadvar
  raw_frames: array[0..MAX_FRAMES-1] of NativeUInt;
  raw_frame_count: Integer;

type
  PEXCEPTION_POINTERS = ^EXCEPTION_POINTERS;

type
  PVECTORED_EXCEPTION_HANDLER = function(ExceptionInfo: PEXCEPTION_POINTERS): DWORD; stdcall;

  TAddVectoredExceptionHandler = function(FirstHandler: ULONG; VectoredHandler: PVECTORED_EXCEPTION_HANDLER): PVOID; stdcall;
  TRemoveVectoredExceptionHandler = function(VectoredHandlerHandle: PVOID): ULONG; stdcall;
  TStackWalk64 = function(MachineType: DWORD; hProcess: THANDLE; hThread: THANDLE; var StackFrame: STACKFRAME64;
    ContextRecord: PVOID; ReadMemoryRoutine: PREAD_PROCESS_MEMORY_ROUTINE64; FunctionTableAccessRoutine: PFUNCTION_TABLE_ACCESS_ROUTINE64;
    GetModuleBaseRoutine: PGET_MODULE_BASE_ROUTINE64; TranslateAddress: PTRANSLATE_ADDRESS_ROUTINE64): BOOL; stdcall;
  TSymFunctionTableAccess64 = function(hProcess: THANDLE; AddrBase: Int64): PVOID; stdcall;
  TSymGetModuleBase64 = function(hProcess: THANDLE; qwAddr: Int64): Int64; stdcall;
  TSymInitialize = function(hProcess: THANDLE; UserSearchPath: PCHAR; fInvadeProcess: BOOL): BOOL; stdcall;

var
  AddVectoredExceptionHandler: TAddVectoredExceptionHandler = nil;
  RemoveVectoredExceptionHandler: TRemoveVectoredExceptionHandler = nil;
  StackWalk64: TStackWalk64 = nil;
  SymFunctionTableAccess64: TSymFunctionTableAccess64 = nil;
  SymGetModuleBase64: TSymGetModuleBase64 = nil;
  SymInitialize: TSymInitialize = nil;

procedure CaptureStackTrace(TopAddr: Pointer; FramesToSkip: Integer); forward;
procedure CaptureStackTraceForException(TopAddr: Pointer; EP: PEXCEPTION_POINTERS; FramesToSkip: Integer); forward;

function RtlCaptureStackBackTrace(FramesToSkip, FramesToCapture: DWORD; BackTrace: Pointer; BackTraceHash: PDWORD): WORD; stdcall; external 'ntdll.dll';

{ TSentryClient }

///
/// Handler for try/except blocks. Call SentryHandleException
/// on outer blocks to report any unhandled exceptions.
///
function SentryHandleException(E: Exception; AExceptAddr: Pointer): Boolean;
const
  BufferSize = 1024;

  // We are not interested in the first two frames:
  //    Sentry.Client.CaptureStackTrace,
  //    SentryHandleException
  FRAMES_TO_SKIP = 2;
var
  Buffer: array[0..BufferSize-1] of Char;
  Message: string;
begin
  if TSentryClient.FInstance = nil then
  begin
    Exit(False);
  end;

  if AExceptAddr = nil then
    AExceptAddr := System.ExceptAddr;


  if ExceptionErrorMessage(E, AExceptAddr, Buffer, BufferSize) = 0 then
    StrCopy(Buffer, 'Unknown exception');

  Message := Buffer;

  if not (TObject(E) is Exception) then
  begin
    // Exceptions that bubble out of kmcomapi without safecall semantics
    // do not inherit from local module Exception object, so we manually import
    // the message
    Message := Message + #13#10 + E.Message;
  end;

  if (raw_frame_count = 0) or (raw_frames[0] <> NativeUInt(AExceptAddr)) then
  begin
    // If we get here, this is most likely an exception that was
    // never raised, as otherwise our vectored handler should have
    // already captured a more accurate stack for us. Let's get something
    // from this
    CaptureStackTrace(AExceptAddr, FRAMES_TO_SKIP);
  end;

  TSentryClient.FInstance.ExceptionEvent(E.ClassName, Message);

  Result := True;
end;

///
/// This is a thread-safe version of Set8087CW that avoids the global
/// variable Default8087CW. We would get occasional situations where
/// exceptions were raised on 2 threads simultaneously, which could lead to a
/// race where the first thread set Default8087CW to $1340, and then the second
/// thread would read that and think that is the default to keep. We want to
/// avoid touching Default8087CW altogether here.
///
/// See also https://stackoverflow.com/a/39684636/1836776 and RSP-13643.
///
procedure Set8087CW_Threadsafe(ANewCW: Word);
var
  L8087CW: Word;
asm
  mov L8087CW, ANewCW
  fnclex
  fldcw L8087CW
end;

///
/// First chance exception handler. We just want to take a copy of the raw
/// stack here.
///
function SentryVectoredHandler(ExceptionInfo: PEXCEPTION_POINTERS): DWORD; stdcall;
const
  cDelphiException    = $0EEDFADE;  // From System.pas
{$IFDEF WIN64}
  DELPHI_FRAMES_TO_SKIP = 3;  // Delphi x64 exceptions have RaiseException, System.@RaseAtExcept, System.@RaiseExcept frames
{$ELSE}
  DELPHI_FRAMES_TO_SKIP = 1;  // Delphi x86 exceptions have RaiseException frame
{$ENDIF}
var
  Skip: Integer;
  LastMask: WORD;
begin
  try
    if (ExceptionInfo.ExceptionRecord.ExceptionCode <> EXCEPTION_ACCESS_VIOLATION) and
        (ExceptionInfo.ExceptionRecord.ExceptionCode <> EXCEPTION_ILLEGAL_INSTRUCTION) and
        (ExceptionInfo.ExceptionRecord.ExceptionCode <> EXCEPTION_PRIV_INSTRUCTION) and
        (ExceptionInfo.ExceptionRecord.ExceptionCode <> EXCEPTION_POSSIBLE_DEADLOCK) and
        (ExceptionInfo.ExceptionRecord.ExceptionCode <> EXCEPTION_IN_PAGE_ERROR) and
        (ExceptionInfo.ExceptionRecord.ExceptionCode <> EXCEPTION_STACK_OVERFLOW) and
        (ExceptionInfo.ExceptionRecord.ExceptionCode <> STATUS_INVALID_PARAMETER) and
        (ExceptionInfo.ExceptionRecord.ExceptionCode <> cDelphiException) then
    begin
      // These are the most common exceptions we wish to catch; we won't get
      // call stacks for any other exception types, but we will still log
      // important exceptions
       Exit(0);
    end;

    // Floating point state may be broken here, so let's mask it out and continue
    // We'll restore state afterwards
    LastMask := Get8087CW;
    Set8087CW_Threadsafe($1332);

    if ExceptionInfo.ExceptionRecord.ExceptionCode = cDelphiException
      then Skip := DELPHI_FRAMES_TO_SKIP
      else Skip := 0;
    CaptureStackTraceForException(ExceptionInfo.ExceptionRecord.ExceptionAddress, ExceptionInfo, Skip);

    // Restore FP state
    Set8087CW_Threadsafe(LastMask);

    Result := 0; //EXCEPTION_CONTINUE_SEARCH;
  except
    // Silently handle errors here because we can't do anything with them;
    // we'll just get no stack
    Result := 0;
  end;
end;

constructor TSentryClient.Create(AOptions: TSentryClientOptions; const ALogger: string; AFlags: TSentryClientFlags);
begin
  Assert(not Assigned(FInstance));
  FInstance := Self;
  FLogger := ALogger;
  FReportExceptions := scfReportExceptions in AFlags;
  FReportMessages := scfReportMessages in AFlags;

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

  if AOptions.DatabasePath <> '' then
    sentry_options_set_database_pathw(options, PWideChar(AOptions.DatabasePath));

  if AOptions.HandlerPath <> '' then
    sentry_options_set_handler_pathw(options, PWideChar(AOptions.HandlerPath));

  if sentry_init(options) = 0 then
    FSentryInit := True;

  {$WARN SYMBOL_PLATFORM OFF} // W1002 Symbol 'CmdLine' is specific to a platform
  if CmdLine <> nil then
  begin
    sentry_set_extra('keyman.commandline', sentry_value_new_string(PAnsiChar(AnsiString(string(CmdLine)))));
  end;
  {$WARN SYMBOL_PLATFORM DEFAULT}

  sentry_set_extra('keyman.executable.fullpath', sentry_value_new_string(PAnsiChar(AnsiString(ParamStr(0)))));
  sentry_set_tag('keyman.executable', PAnsiChar(AnsiString(ExtractFileName(ParamStr(0)))));
  // TODO: callback to capture list of keyboard filenames, TSF settings, additional diag?

  if scfCaptureExceptions in AFlags then
  begin
    // This allows us to capture call stacks from the original exception context
    if Assigned(AddVectoredExceptionHandler) then
      // 1 = Register as first handler
      FVectoredExceptionHandler := AddVectoredExceptionHandler(1, @SentryVectoredHandler);
  end;
end;

destructor TSentryClient.Destroy;
begin
  FInstance := nil;
  if FSentryInit then
    sentry_shutdown;
  FSentryInit := False;
  if FVectoredExceptionHandler <> nil then
    RemoveVectoredExceptionHandler(FVectoredExceptionHandler);
  inherited Destroy;
end;

///
/// Force the application to terminate abruptly, but give
/// time for Sentry to report outstanding events.
///
procedure TSentryClient.DoTerminate;
begin
  sentry_shutdown;
  ExitProcess(1);
end;

/// Call registered OnBeforeEvent handler
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

/// Call registered OnAfterEvent handler
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

function TSentryClient.EventIDToString(AGuid: PByte): String;
var
  i: Integer;
  p: PChar;
begin
  SetLength(Result, 32);
  p := PChar(Result);
  for i := 0 to 15 do
  begin
    StrLFmt(p, 2, '%.2X', [AGuid^]);
    Inc(p, 2);
    Inc(AGuid);
  end;
end;

procedure TSentryClient.Breadcrumb(const BreadcrumbType, Message: string; const Category: string = ''; const Level: string = 'info');
var
  crumb: sentry_value_t;
begin
  crumb := sentry_value_new_breadcrumb(PAnsiChar(UTF8Encode(BreadcrumbType)), PAnsiChar(UTF8Encode(Message)));
  if Category <> '' then
    sentry_value_set_by_key(crumb, 'category', sentry_value_new_string(PAnsiChar(UTF8Encode(Category))));
  if Level <> '' then
    sentry_value_set_by_key(crumb, 'level', sentry_value_new_string(PAnsiChar(UTF8Encode(Level))));
  sentry_add_breadcrumb(crumb);
end;

function TSentryClient.ConvertRawStackToSentryStack(wrapWithThread: Boolean): sentry_value_t;
var
  frames, s_frame, stacktrace, thread, threads: sentry_value_t;
  s: AnsiString;
  n: Integer;
begin
  frames := sentry_value_new_list;
  n := raw_frame_count;
  while n > 0 do
  begin
    s_frame := sentry_value_new_object;
    s := System.AnsiStrings.Format('0x%x', [NativeUInt(raw_frames[n-1])]);
    sentry_value_set_by_key(s_frame, 'instruction_addr', sentry_value_new_string(PAnsiChar(s)));
    sentry_value_append(frames, s_frame);
    Dec(n);
  end;

  stacktrace := sentry_value_new_object;
  sentry_value_set_by_key(stacktrace, 'frames', frames);

  if wrapWithThread then
  begin
    threads := sentry_value_new_list;
    thread := sentry_value_new_object;
    sentry_value_set_by_key(thread, 'stacktrace', stacktrace);
    sentry_value_append(threads, thread);

    Result := threads;
  end
  else
    Result := stacktrace;
end;

function TSentryClient.ExceptionEvent(const ExceptionClassName, Message: string; AExceptAddr: Pointer = nil): String;
var
  exc, event: sentry_value_t;
  uuid: sentry_uuid_t;
  stacktrace: sentry_value_t;
begin
  if FReportExceptions then
  begin
    event := sentry_value_new_event;

    DoBeforeEvent(event, ExceptionClassName, Message, scetException);

    exc := sentry_value_new_object;
    sentry_value_set_by_key(exc, 'type', sentry_value_new_string(PAnsiChar(UTF8Encode(ExceptionClassName))));
    sentry_value_set_by_key(exc, 'value', sentry_value_new_string(PAnsiChar(UTF8Encode(Message))));

    if raw_frame_count > 0 then
    begin
      stacktrace := ConvertRawStackToSentryStack(false);
      if stacktrace <> 0 then
        sentry_value_set_by_key(exc, 'stacktrace', stacktrace);
      raw_frame_count := 0;
    end;

    sentry_value_set_by_key(event, 'exception', exc);
    sentry_value_set_by_key(event, 'logger', sentry_value_new_string(PAnsiChar(UTF8Encode(FLogger))));

    // We will rebuild the module list at time of event in order to ensure we
    // don't lose dynamically loaded modules, as far as possible. This is
    // potentially slow, but this is probably a fatal code path anyway ...
    sentry_clear_modulecache;

    uuid := sentry_capture_event(event);
    Result := EventIDToString(@uuid.bytes[0]);

    DoAfterEvent(Result, ExceptionClassName, Message, scetException);
  end
  else
  begin
    // We still call the event handlers, in case they want to do something ...
    // as normally DoAfterEvent will be terminating the process on an unhandled
    // exception
    DoBeforeEvent(0, ExceptionClassName, Message, scetException);
    DoAfterEvent('', ExceptionClassName, Message, scetException);
  end;
end;

class function TSentryClient.GetEnabled: Boolean;
begin
  if not FEnabledInitialised then
  begin
    // WINE is not coping with some of the Sentry/dbghelp calls so disable
    // sentry on WINE instances.
    FEnabled := GetProcAddress(GetModuleHandle('ntdll.dll'), 'wine_get_version') = nil;
    FEnabledInitialised := True;
  end;

  Exit(FEnabled);
end;

function TSentryClient.MessageEvent(Level: TSentryLevel; const Message: string;
  IncludeStack: Boolean): string;
var
  event: sentry_value_t;
  threads: sentry_value_t;
  uuid: sentry_uuid_t;
const
  FRAMES_TO_SKIP = 2;
  // We are not interested in the first two frames:
  //    Sentry.Client.CaptureStackTrace,
  //    Sentry.Client.TSentryClient.MessageEvent
begin
  if FReportMessages or (FReportExceptions and (Level in [SENTRY_LEVEL_ERROR, SENTRY_LEVEL_FATAL])) then
  begin
    event := sentry_value_new_message_event(
      {*   level *} sentry_level_t(Level),
      {*  logger *} PAnsiChar(UTF8Encode(FLogger)),
      {* message *} PAnsiChar(UTF8Encode(Message))
    );

    DoBeforeEvent(event, FLogger, Message, scetMessage);

    if IncludeStack then
    begin
      CaptureStackTrace(nil, FRAMES_TO_SKIP);
      if raw_frame_count > 0 then
      begin
        threads := ConvertRawStackToSentryStack(true);
        if threads <> 0 then
          sentry_value_set_by_key(event, 'threads', threads);
        raw_frame_count := 0;
      end;
    end;

    uuid := sentry_capture_event(event);
    Result := EventIDToString(@uuid.bytes[0]);

    DoAfterEvent(Result, FLogger, Message, scetMessage);
  end
  else
  begin
    DoBeforeEvent(0, FLogger, Message, scetMessage);
    DoAfterEvent('', FLogger, Message, scetMessage);
  end;
end;

///
/// Capture a stack trace and include the offending crash address at the top of
/// the trace, storing in the global threadvar raw_frames. It'd be possible to
/// replace this with CaptureStackTraceForException if we constructed our own
/// CONTEXT record.
///
procedure CaptureStackTrace(TopAddr: Pointer; FramesToSkip: Integer);
var
  p: PNativeUInt;
begin
  p := @raw_frames[0];
  if TopAddr <> nil then
  begin
    p^ := NativeUInt(TopAddr);
    Inc(p);
  end;

  raw_frame_count := RtlCaptureStackBackTrace(FramesToSkip, MAX_FRAMES-1, p, nil);

  if TopAddr <> nil then
    Inc(raw_frame_count);
end;

///
/// Capture a stack trace, storing in the global threadvar raw_frames. This
/// function uses StackWalk64, because we have a good entry CONTEXT.
///
procedure CaptureStackTraceForException(TopAddr: Pointer; EP: PEXCEPTION_POINTERS; FramesToSkip: Integer);
var
  c: TContext;
  frame: TStackFrame64;
  image: DWORD;
  n: Integer;
begin
  n := 0;
  c := EP.ContextRecord^;

  ZeroMemory(@frame, sizeof(TStackFrame64));
{$IFDEF WIN64}
  image := IMAGE_FILE_MACHINE_AMD64;
  frame.AddrPC.Offset := c.Rip;
  frame.AddrPC.Mode := AddrModeFlat;

  frame.AddrFrame.Offset := c.Rbp;
  frame.AddrFrame.Mode := AddrModeFlat;

  frame.AddrStack.Offset := c.Rsp;
  frame.AddrStack.Mode := AddrModeFlat;
{$ELSE}
  image := IMAGE_FILE_MACHINE_I386;
  frame.AddrPC.Offset := c.Eip;
  frame.AddrPC.Mode := AddrModeFlat;

  frame.AddrFrame.Offset := c.Ebp;
  frame.AddrFrame.Mode := AddrModeFlat;

  frame.AddrStack.Offset := c.Esp;
  frame.AddrStack.Mode := AddrModeFlat;
{$ENDIF}
  while (n < MAX_FRAMES) and StackWalk64(image, GetCurrentProcess, GetCurrentThread, frame, @c, nil, SymFunctionTableAccess64, SymGetModuleBase64, nil) do
  begin
    if frame.AddrFrame.Offset = 0 then
      // End of stack or broken stack
      Break;

    if (frame.AddrPC.Offset = 0) then
      // Empty stack frame
      Continue;

    if (frame.AddrPC.Offset = frame.AddrReturn.Offset) then
      // Recursive stack frame
      Continue;

    if FramesToSkip > 0 then
    begin
      Dec(FramesToSkip);
      Continue;
    end;

    raw_frames[n] := NativeUInt(frame.AddrPC.Offset);
    Inc(n);
  end;

  raw_frame_count := n;
end;

var
  hKernel32, hDbgHelp: THandle;
initialization
  hKernel32 := LoadLibrary('kernel32.dll');
  AddVectoredExceptionHandler := GetProcAddress(hKernel32, 'AddVectoredExceptionHandler');
  RemoveVectoredExceptionHandler := GetProcAddress(hKernel32, 'RemoveVectoredExceptionHandler');

  hDbgHelp := LoadLibrary('Dbghelp.dll');
  StackWalk64 := GetProcAddress(hDbgHelp, 'StackWalk64');
  SymFunctionTableAccess64 := GetProcAddress(hDbgHelp, 'SymFunctionTableAccess64');
  SymGetModuleBase64 := GetProcAddress(hDbgHelp, 'SymGetModuleBase64');
  SymInitialize := GetProcAddress(hDbgHelp, 'SymInitialize');

  if TSentryClient.Enabled then
    if Assigned(SymInitialize) then
      SymInitialize(GetCurrentProcess, nil, True);
end.
