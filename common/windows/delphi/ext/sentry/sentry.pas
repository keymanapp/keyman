{$D-} // Don't include debug information
// Delphi translation of sentry.h
// Sentry Native API 0.6.0 - ***excluding performance APIs***
// https://github.com/getsentry/sentry-native
unit sentry;

{
 * NOTE on encodings:
 *
 * Sentry will assume an encoding of UTF-8 for all string data that is captured
 * and being sent to sentry as an Event.
 * All the functions that are dealing with *paths* will assume an OS-specific
 * encoding, typically ANSI on Windows, UTF-8 macOS, and the locale encoding on
 * Linux; and they provide wchar-compatible alternatives on Windows which are
 * preferred.
}

interface

{$DEFINE SENTRY_API}

//{$ALIGN ON}
{$MINENUMSIZE 4}

{$WARN SYMBOL_PLATFORM OFF}

{$IF DEFINED(MSWINDOWS)}

uses
  Winapi.Windows;

{$DEFINE SENTRY_UUID_WINDOWS}

{$ELSEIF DEFINED(ANDROID)}

{$DEFINE SENTRY_UUID_ANDROID}

{$ELSE}

uses
  uuid; //TODO

{$DEFINE SENTRY_UUID_LIBUUID}

{$ENDIF}

const
{$IFDEF WIN64}
  sentry_dll = 'sentry.x64.dll';
{$ELSE}
  sentry_dll = 'sentry.dll';
{$ENDIF}

const
  SENTRY_SDK_NAME = 'sentry.native';
  SENTRY_SDK_VERSION = '0.6.0';
  SENTRY_SDK_USER_AGENT = SENTRY_SDK_NAME + '/' + SENTRY_SDK_VERSION;

{$IF DEFINED(MSWINDOWS)}
{$DEFINE SENTRY_PLATFORM_WINDOWS}
{$ELSE}
{$MESSAGE ERROR 'Unsupported platform'}
{$ENDIF}

type
  size_t = NativeUInt;

// The library internally uses the system malloc and free functions to manage
// memory.  It does not use realloc.  The reason for this is that on unix
// platforms we fall back to a simplistic page allocator once we have
// encountered a SIGSEGV or other terminating signal as malloc is no longer
// safe to use.  Since we cannot portably reallocate allocations made on the
// pre-existing allocator we're instead not using realloc.
//
// Note also that after SIGSEGV sentry_free() becomes a noop.

//
// allocates memory with the underlying allocator
//
function sentry_malloc(size: size_t): Pointer; cdecl; external sentry_dll delayed;

//
// releases memory allocated from the underlying allocator
procedure sentry_free(ptr: Pointer); cdecl; external sentry_dll delayed;

// legacy function.  Alias for `sentry_free`.
procedure sentry_string_free(str: PAnsiChar); cdecl;

// -- Protocol Value API --

//
// Type of a sentry value.
//

type
  sentry_value_type_t = (
    SENTRY_VALUE_TYPE_NULL = 0,
    SENTRY_VALUE_TYPE_BOOL,
    SENTRY_VALUE_TYPE_INT32,
    SENTRY_VALUE_TYPE_DOUBLE,
    SENTRY_VALUE_TYPE_STRING,
    SENTRY_VALUE_TYPE_LIST,
    SENTRY_VALUE_TYPE_OBJECT
  );

//
// Represents a sentry protocol value.
//
// The members of this type should never be accessed.  They are only here
// so that alignment for the type can be properly determined.
//
// Values must be released with `sentry_value_decref`.  This lowers the
// internal refcount by one.  If the refcount hits zero it's freed.  Some
// values like primitives have no refcount (like null) so operations on
// those are no-ops.
//
// In addition values can be frozen.  Some values like primitives are always
// frozen but lists and dicts are not and can be frozen on demand.  This
// automatically happens for some shared values in the event payload like
// the module list.
//

type
  {sentry_value_u = record
    case Integer of
      0: (_bits: UInt64);
      1: (_double: Double);
  end;

  sentry_value_t = sentry_value_u;}
  sentry_value_t = UInt64;

//
// Increments the reference count on the value.\
//
procedure sentry_value_incref(
  value: sentry_value_t
); cdecl; external sentry_dll  delayed;

//
// Decrements the reference count on the value.
//
procedure sentry_value_decref(
  value: sentry_value_t
); cdecl; external sentry_dll  delayed;

//
// Returns the refcount of a value.
//
function sentry_value_refcount(
  value: sentry_value_t
): size_t; cdecl; external sentry_dll  delayed;

//
// Freezes a value.
//
procedure sentry_value_freeze(
  value: sentry_value_t
); cdecl; external sentry_dll  delayed;

//
// Checks if a value is frozen.
//
function sentry_value_is_frozen(
  value: sentry_value_t
): Integer; cdecl; external sentry_dll  delayed;

//
// Creates a null value.
//
function sentry_value_new_null: sentry_value_t; cdecl; external sentry_dll  delayed;

//
// Creates a new 32bit signed integer value.
//
function sentry_value_new_int32(
  value: Integer
): sentry_value_t; cdecl; external sentry_dll  delayed;

//
// Creates a new double value.
//
function sentry_value_new_double(
  value: Double
): sentry_value_t; cdecl; external sentry_dll  delayed;

//
// Creates a new boolen value.
//
function sentry_value_new_bool(
  value: Integer
): sentry_value_t; cdecl; external sentry_dll  delayed;

//
// Creates a new null terminated string.
//
function sentry_value_new_string(
  value: PAnsiChar
): sentry_value_t; cdecl; external sentry_dll  delayed;

//
// Create a new list value.
//
function sentry_value_new_list: sentry_value_t; cdecl; external sentry_dll  delayed;

//
// Creates a new object.
//
function sentry_value_new_object: sentry_value_t; cdecl; external sentry_dll  delayed;

//
// Returns the type of the value passed.
//
function sentry_value_get_type(
  value: sentry_value_t
): sentry_value_type_t; cdecl; external sentry_dll  delayed;

//
// Sets a key to a value in the map.
//
// This moves the ownership of the value into the map.  The caller does not
// have to call `sentry_value_decref` on it.
//
function sentry_value_set_by_key(
  value: sentry_value_t;
  const k: PAnsiChar;
  v: sentry_value_t
): Integer; cdecl; external sentry_dll  delayed;

//
// This removes a value from the map by key.
//
function sentry_value_remove_by_key(
  value: sentry_value_t;
  const k: PAnsiChar
): Integer; cdecl; external sentry_dll  delayed;

//
// Appends a value to a list.
//
// This moves the ownership of the value into the list.  The caller does not
// have to call `sentry_value_decref` on it.
//
function sentry_value_append(
  value: sentry_value_t;
  v: sentry_value_t
): Integer; cdecl; external sentry_dll  delayed;

//
// Inserts a value into the list at a certain position.
//
// This moves the ownership of the value into the list.  The caller does not
// have to call `sentry_value_decref` on it.
//
// If the list is shorter than the given index it's automatically extended
// and filled with `null` values.
//
function sentry_value_set_by_index(
  value: sentry_value_t;
  index: size_t;
  v: sentry_value_t
): Integer; cdecl; external sentry_dll  delayed;

//
// This removes a value from the list by index.
//
function sentry_value_remove_by_index(
  value: sentry_value_t;
  index: size_t
): Integer; cdecl; external sentry_dll  delayed;

//
// Looks up a value in a map by key.  If missing a null value is returned.
// The returned value is borrowed.
//
function sentry_value_get_by_key(
  value: sentry_value_t;
  const k: PAnsiChar
): sentry_value_t; cdecl; external sentry_dll  delayed;


//

// Looks up a value in a map by key.  If missing a null value is returned.

// The returned value is owned.
//
// If the caller no longer needs the value it must be released with
// `sentry_value_decref`.
//
function sentry_value_get_by_key_owned(
  value: sentry_value_t;
  const k: PAnsiChar
): sentry_value_t; cdecl; external sentry_dll  delayed;

//
// Looks up a value in a list by index.  If missing a null value is returned.
// The returned value is borrowed.
//
function sentry_value_get_by_index(
  value: sentry_value_t;
  index: size_t): sentry_value_t; cdecl; external sentry_dll  delayed;

//
// Looks up a value in a list by index.  If missing a null value is returned.
// The returned value is owned.
//
// If the caller no longer needs the value it must be released with
// `sentry_value_decref`.
//
function sentry_value_get_by_index_owned(
  value: sentry_value_t;
  index: size_t): sentry_value_t; cdecl; external sentry_dll  delayed;

//
// Returns the length of the given map or list.
//
// If an item is not a list or map the return value is 0.
//
function sentry_value_get_length(
  value: sentry_value_t
): size_t; cdecl; external sentry_dll  delayed;

//
// Converts a value into a 32bit signed integer.
//
function sentry_value_as_int32(
  value: sentry_value_t
): Integer; cdecl; external sentry_dll  delayed;

//
// Converts a value into a double value.
//
function sentry_value_as_double(
  value: sentry_value_t
): Double; cdecl; external sentry_dll  delayed;

//
// Returns the value as c string.
//
function sentry_value_as_string(
  value: sentry_value_t
): PAnsiChar; cdecl; external sentry_dll  delayed;

//
// Returns `true` if the value is boolean true.
//
function sentry_value_is_true(
  value: sentry_value_t
): Integer; cdecl; external sentry_dll  delayed;

//
// Returns `true` if the value is null.
//
function sentry_value_is_null(
  value: sentry_value_t
): Integer; cdecl; external sentry_dll  delayed;

//
// Serialize a sentry value to JSON.
//
// The string is freshly allocated and must be freed with
// `sentry_string_free`.
//
function sentry_value_to_json(
  value: sentry_value_t
): PAnsiChar; cdecl; external sentry_dll  delayed;

//
// Sentry levels for events and breadcrumbs.
//
type sentry_level_e = (
    SENTRY_LEVEL_DEBUG = -1,
    SENTRY_LEVEL_INFO = 0,
    SENTRY_LEVEL_WARNING = 1,
    SENTRY_LEVEL_ERROR = 2,
    SENTRY_LEVEL_FATAL = 3
);
sentry_level_t = sentry_level_e;

//
// Creates a new empty Event value.
//
// See https://docs.sentry.io/platforms/native/enriching-events/ for how to
// further work with events, and https://develop.sentry.dev/sdk/event-payloads/
// for a detailed overview of the possible properties of an Event.
//
function sentry_value_new_event: sentry_value_t; cdecl; external sentry_dll  delayed;

//
// Creates a new Message Event value.
//
// See https://develop.sentry.dev/sdk/event-payloads/message/
//
// `logger` can be NULL to omit the logger value.
//
function sentry_value_new_message_event(
  level: sentry_level_t;
  const logger: PAnsiChar;
  const text: PAnsiChar
): sentry_value_t; cdecl; external sentry_dll  delayed;

//
// Creates a new Breadcrumb with a specific type and message.
//
// See https://develop.sentry.dev/sdk/event-payloads/breadcrumbs/
//
function sentry_value_new_breadcrumb(
  const _type: PAnsiChar;
  const message: PAnsiChar
): sentry_value_t; cdecl; external sentry_dll  delayed;

//
// Creates a new Exception value.
//
// This is intended for capturing language-level exception, such as from a
// try-catch block. `type` and `value` here refer to the exception class and
// a possible description.
//
// See https://develop.sentry.dev/sdk/event-payloads/exception/
//
// The returned value needs to be attached to an event via
// `sentry_event_add_exception`.
//
function {SENTRY_EXPERIMENTAL_API} sentry_value_new_exception(
    const _type: PAnsiChar;
    const value: PAnsiChar
): sentry_value_t; cdecl; external sentry_dll  delayed;

//
// Creates a new Thread value.
//
// See https://develop.sentry.dev/sdk/event-payloads/threads/
//
// The returned value needs to be attached to an event via
// `sentry_event_add_thread`.
//
// `name` can be NULL.
//
function {SENTRY_EXPERIMENTAL_API} sentry_value_new_thread(
    id: UInt64;
    const name: PAnsiChar
): sentry_value_t; cdecl; external sentry_dll  delayed;

//
// Creates a new Stack Trace conforming to the Stack Trace Interface.
//
// See https://develop.sentry.dev/sdk/event-payloads/stacktrace/
//
// The returned object needs to be attached to either an exception
// event, or a thread object.
//
// If `ips` is NULL the current stack trace is captured, otherwise `len`
// stack trace instruction pointers are attached to the event.
//
function {SENTRY_EXPERIMENTAL_API} sentry_value_new_stacktrace(
  ips: PPVoid;
  len: NativeUInt
): sentry_value_t; cdecl; external sentry_dll  delayed;

//
// Sets the Stack Trace conforming to the Stack Trace Interface in a value.
//
// The value argument must be either an exception or thread object.
//
// If `ips` is NULL the current stack trace is captured, otherwise `len` stack
// trace instruction pointers are attached to the event.
//
procedure {SENTRY_EXPERIMENTAL_API} sentry_value_set_stacktrace(
    value: sentry_value_t;
    ips: PPVoid;
    len: NativeUInt
); cdecl; external sentry_dll  delayed;

//
// Adds an Exception to an Event value.
//
// This takes ownership of the `exception`.
//
procedure {SENTRY_EXPERIMENTAL_API} sentry_event_add_exception(
    event: sentry_value_t;
    exception: sentry_value_t
); cdecl; external sentry_dll  delayed;

//
// Adds a Thread to an Event value.
//
// This takes ownership of the `thread`.
//
procedure {SENTRY_EXPERIMENTAL_API} sentry_event_add_thread(
    event: sentry_value_t;
    thread: sentry_value_t
); cdecl; external sentry_dll  delayed;

// -- Experimental APIs --

//
// Serialize a sentry value to msgpack.
//
// The string is freshly allocated and must be freed with
// `sentry_string_free`.  Since msgpack is not zero terminated
// the size is written to the `size_out` parameter.
//
function sentry_value_to_msgpack(
  const value: sentry_value_t;
  var size_out: size_t
): PAnsiChar; cdecl; external sentry_dll  delayed;

//
// Adds a stack trace to an event.
//
// The stack trace is added as part of a new thread object.
// This function is **deprecated** in favor of using
// `sentry_value_new_stacktrace` in combination with `sentry_value_new_thread`
// and `sentry_event_add_thread`.
//
// If `ips` is NULL the current stack trace is captured, otherwise `len`
// stack trace instruction pointers are attached to the event.
//
procedure sentry_event_value_add_stacktrace(
  event: sentry_value_t;
  ips: Pointer;
  len: Integer

); cdecl; external sentry_dll  delayed;



//
// This represents the OS dependent user context in the case of a crash, and can
// be used to manually capture a crash.
//
type
  sentry_ucontext_s = record
{$IF DEFINED(MSWINDOWS)}
    exception_ptrs: EXCEPTION_POINTERS;
{$ELSE}
    signum: Integer;
    siginfo: siginfo_t;
    user_context: ucontext_t;
{$ENDIF}
  end;

  sentry_ucontext_t = sentry_ucontext_s;
  psentry_ucontext_t = ^sentry_ucontext_t;

//
// Unwinds the stack from the given address.
//
// If the address is given in `addr` the stack is unwound form there.  Otherwise
// (NULL is passed) the current instruction pointer is used as start address.
// The stack trace is written to `stacktrace_out` with up to `max_len` frames
// being written.  The actual number of unwound stackframes is returned.
//
function sentry_unwind_stack(
  addr: Pointer;
  var stacktrace_out: Pointer;
  max_len: size_t
): size_t; cdecl; external sentry_dll  delayed;

//
// Unwinds the stack from the given context.
//
// The stack trace is written to `stacktrace_out` with up to `max_len` frames
// being written.  The actual number of unwound stackframes is returned.
//
function sentry_unwind_stack_from_ucontext(
  const uctx: psentry_ucontext_t;
  var stacktrace_out: Pointer;
  max_len: size_t
): size_t; cdecl; external sentry_dll  delayed;




//
// A UUID
//
type
  sentry_uuid_s = record
    bytes: array[0..15] of Byte;
  end;

  sentry_uuid_t = sentry_uuid_s;
  psentry_uuid_t = ^sentry_uuid_t;

//
// Creates the nil uuid.
//
function sentry_uuid_nil: sentry_uuid_t; cdecl; external sentry_dll  delayed;

//
// Creates a new uuid4.
//
function sentry_uuid_new_v4: sentry_uuid_t; cdecl; external sentry_dll  delayed;

//
// Parses a uuid from a string.
//
function sentry_uuid_from_string(
  const str: PAnsiChar
): sentry_uuid_t; cdecl; external sentry_dll  delayed;

//
// Creates a uuid from bytes.
//
function sentry_uuid_from_bytes(
  const bytes: PAnsiChar // TODO check signature
): sentry_uuid_t; cdecl; external sentry_dll  delayed;

//
// Checks if the uuid is nil.
//
function sentry_uuid_is_nil(
  const uuid: psentry_uuid_t
): Integer; cdecl; external sentry_dll  delayed;

//
// Returns the bytes of the uuid.
//
procedure sentry_uuid_as_bytes(
  const uuid: psentry_uuid_t;
  bytes: PAnsiChar // TODO check signature
); cdecl; external sentry_dll  delayed;

//
// Formats the uuid into a string buffer.
//
procedure sentry_uuid_as_string(
  const uuid: psentry_uuid_t;
  str: PAnsiChar // TODO check signature
); cdecl; external sentry_dll  delayed;

type
//
// A Sentry Envelope.
//
// The Envelope is an abstract type which represents a payload being sent to
// sentry. It can contain one or more items, typically an Event.
// See https://develop.sentry.dev/sdk/envelopes/
//
  sentry_envelope_s = record end;
  sentry_envelope_t = sentry_envelope_s;
  psentry_envelope_t = Pointer;

//
// Frees an envelope.
//
procedure sentry_envelope_free(
  envelope: sentry_envelope_t
); cdecl; external sentry_dll  delayed;

//
// Given an envelope returns the embedded event if there is one.
//
// This returns a borrowed value to the event in the envelope.
//
function sentry_envelope_get_event(
  const envelope: psentry_envelope_t
): sentry_value_t; cdecl; external sentry_dll  delayed;

//
// Given an Envelope, returns the embedded Transaction if there is one.
//
// This returns a borrowed value to the Transaction in the Envelope.
//
function {SENTRY_EXPERIMENTAL_API} sentry_envelope_get_transaction(
  const envelope: psentry_envelope_t
): sentry_value_t; cdecl; external sentry_dll  delayed;

//
// Serializes the envelope
//
// The return value needs to be freed with sentry_string_free().
//
function sentry_envelope_serialize(
  const envelope: psentry_envelope_t;
  var size_out: size_t
): PAnsiChar; cdecl; external sentry_dll  delayed;

//
// Serializes the envelope into a file.
//
// `path` is assumed to be in platform-specific filesystem path encoding.
//
// Returns 0 on success.
//
function sentry_envelope_write_to_file(
  const envelope: psentry_envelope_t;
  const path: PAnsiChar
): Integer; cdecl; external sentry_dll  delayed;

//
// The Sentry Client Options.
//
// See https://docs.sentry.io/error-reporting/configuration/
//
type
  sentry_options_s = record end;
  sentry_options_t = sentry_options_s;
  psentry_options_t = ^sentry_options_t;

//
// This represents an interface for user-defined transports.
//
// Transports are responsible for sending envelopes to sentry and are the last
// step in the event pipeline.
//
// Envelopes will be submitted to the transport in a _fire and forget_ fashion,
// and the transport must send those envelopes _in order_.
//
// A transport has the following hooks, all of which
// take the user provided `state` as last parameter. The transport state needs
// to be set with `sentry_transport_set_state` and typically holds handles and
// other information that can be reused across requests.
//
// * `send_func`: This function will take ownership of an envelope, and is
//   responsible for freeing it via `sentry_envelope_free`.
// * `startup_func`: This hook will be called by sentry inside of `sentry_init`
//   and instructs the transport to initialize itself. Failures will bubble up
//   to `sentry_init`.
// * `shutdown_func`: Instructs the transport to flush its queue and shut down.
//   This hook receives a millisecond-resolution `timeout` parameter and should
//   return `true` when the transport was flushed and shut down successfully.
//   In case of `false`, sentry will log an error, but continue with freeing the
//   transport.
// * `free_func`: Frees the transports `state`. This hook might be called even
//   though `shutdown_func` returned `false` previously.
//
// The transport interface might be extended in the future with hooks to flush
// its internal queue without shutting down, and to dump its internal queue to
// disk in case of a hard crash.
//
type
  sentry_transport_s = record end;
  psentry_transport_s = ^sentry_transport_s;
  sentry_transport_t = sentry_transport_s;
  psentry_transport_t = ^sentry_transport_t;

  _sentry_transport_new_func = procedure(e: psentry_envelope_t; state: Pointer); cdecl;
  _sentry_transport_free_func = procedure(state: Pointer); cdecl;
  _sentry_transport_startup_func = function(const options: psentry_options_t; state: Pointer): Integer; cdecl;
  _sentry_transport_shutdown_func = function(timeout: UInt64; state: Pointer): Integer; cdecl;
  _sentry_transport_flush_func = function(timeout: UInt64; state: Pointer): Integer; cdecl;

//
// Creates a new transport with an initial `send_func`.
//
function sentry_transport_new(
    send_func: _sentry_transport_new_func
): psentry_transport_t; cdecl; external sentry_dll  delayed;

//
// Sets the transport `state`.
//
// If the state is owned by the transport and needs to be freed, use
// `sentry_transport_set_free_func` to set an appropriate hook.
//
procedure sentry_transport_set_state(
    transport: psentry_transport_t;
    state: Pointer
); cdecl; external sentry_dll  delayed;

//
// Sets the transport hook to free the transport `state`.
//
procedure sentry_transport_set_free_func(
    transport: psentry_transport_t;
    free_func: _sentry_transport_free_func
); cdecl; external sentry_dll  delayed;

//
// Sets the transport startup hook.
//
// This hook is called from within `sentry_init` and will get a reference to the
// options which can be used to initialize a transports internal state.
// It should return `0` on success. A failure will bubble up to `sentry_init`.
//
procedure sentry_transport_set_startup_func(
    transport: psentry_transport_t;
    startup_func: _sentry_transport_startup_func
); cdecl; external sentry_dll  delayed;

//
// Sets the transport flush hook.
//
// This hook will receive a millisecond-resolution timeout.
// It should return `0` if all the pending envelopes are
// sent within the timeout, or `1` if the timeout is hit.
//
procedure {SENTRY_API} sentry_transport_set_flush_func(
  transport: psentry_transport_t;
  flush_func: _sentry_transport_flush_func
); cdecl; external sentry_dll  delayed;



//
// Sets the transport shutdown hook.
//
// This hook will receive a millisecond-resolution timeout.
// It should return `0` on success in case all the pending envelopes have been
// sent within the timeout, or `1` if the timeout was hit.
//
procedure sentry_transport_set_shutdown_func(
    transport: psentry_transport_t;
    shutdown_func: _sentry_transport_shutdown_func
); cdecl; external sentry_dll  delayed;

//
// Generic way to free a transport.
//
procedure sentry_transport_free(
    transport: psentry_transport_t
); cdecl; external sentry_dll  delayed;

//
// Create a new function transport.
//
// It is a convenience function which works with a borrowed `data`, and will
// automatically free the envelope, so the user provided function does not need
// to do that.
//
// This function is *deprecated* and will be removed in a future version.
// It is here for backwards compatibility. Users should migrate to the
// `sentry_transport_new` API.
//
function sentry_new_function_transport(
    func: _sentry_transport_new_func;
    data: Pointer
): psentry_transport_t; cdecl; external sentry_dll  delayed;

//
// This represents an interface for user-defined backends.
//
// Backends are responsible to handle crashes. They are maintained at runtime
// via various life-cycle hooks from the sentry-core.
//
// At this point none of those interfaces are exposed in the API including
// creation and destruction. The main use-case of the backend in the API at this
// point is to disable it via `sentry_options_set_backend` at runtime before it
// is initialized.
//
type
  sentry_backend_s = record end;
  sentry_backend_t = sentry_backend_s;
  psentry_backend_t = Pointer;

// -- Options APIs --

//
// The state of user consent.
//
type sentry_user_consent_t = (
    SENTRY_USER_CONSENT_UNKNOWN = -1,
    SENTRY_USER_CONSENT_GIVEN = 1,
    SENTRY_USER_CONSENT_REVOKED = 0
);

//
// Creates a new options struct.
// Can be freed with `sentry_options_free`.
//
function sentry_options_new: psentry_options_t; cdecl; external sentry_dll  delayed;

//
// Deallocates previously allocated sentry options.
//
procedure sentry_options_free(
  opts: psentry_options_t
); cdecl; external sentry_dll  delayed;

//
// Sets a transport.
//
procedure sentry_options_set_transport(
  opts: psentry_options_t;
  transport: psentry_transport_t
); cdecl; external sentry_dll  delayed;

//
// Type of the `before_send` callback.
//
// The callback takes ownership of the `event`, and should usually return that
// same event. In case the event should be discarded, the callback needs to
// call `sentry_value_decref` on the provided event, and return a
// `sentry_value_new_null()` instead.
//
// This function may be invoked inside of a signal handler and must be safe for
// that purpose, see https://man7.org/linux/man-pages/man7/signal-safety.7.html.
// On Windows, it may be called from inside of a `UnhandledExceptionFilter`, see
// the documentation on SEH (structured exception handling) for more information
// https://docs.microsoft.com/en-us/windows/win32/debug/structured-exception-handling
//
type
  sentry_event_function_t = function(
    event: sentry_value_t;
    hint: Pointer;
    closure: Pointer
  ): sentry_value_t; cdecl;

//
// Sets the `before_send` callback.
//
// See the `sentry_event_function_t` typedef above for more information.
//
procedure sentry_options_set_before_send(
  opts: psentry_options_t;
  func: sentry_event_function_t;
  data: Pointer
); cdecl; external sentry_dll  delayed;

//
// Type of the `on_crash` callback.
//
// The `on_crash` callback replaces the `before_send` callback for crash events.
// The interface is analogous to `before_send` in that the callback takes
// ownership of the `event`, and should usually return that same event. In case
// the event should be discarded, the callback needs to call
// `sentry_value_decref` on the provided event, and return a
// `sentry_value_new_null()` instead.
//
// Only the `inproc` backend currently fills the passed-in event with useful
// data and processes any modifications to the return value. Since both
// `breakpad` and `crashpad` use minidumps to capture the crash state, the
// passed-in event is empty when using these backends, and they ignore any
// changes to the return value.
//
// If you set this callback in the options, it prevents a concurrently enabled
// `before_send` callback from being invoked in the crash case. This allows for
// better differentiation between crashes and other events and gradual migration
// from existing `before_send` implementations:
//
//  - if you have a `before_send` implementation and do not define an `on_crash`
//    callback your application will receive both normal and crash events as
//    before
//  - if you have a `before_send` implementation but only want to handle normal
//    events with it, then you can define an `on_crash` callback that returns
//    the passed-in event and does nothing else
//  - if you are not interested in normal events, but only want to act on
//    crashes (within the limits mentioned below), then only define an
//    `on_crash` callback with the option to filter (on all backends) or enrich
//    (only inproc) the crash event
//
// This function may be invoked inside of a signal handler and must be safe for
// that purpose, see https://man7.org/linux/man-pages/man7/signal-safety.7.html.
// On Windows, it may be called from inside of a `UnhandledExceptionFilter`, see
// the documentation on SEH (structured exception handling) for more information
// https://docs.microsoft.com/en-us/windows/win32/debug/structured-exception-handling
//
// Platform-specific behavior:
//
//  - does not work with crashpad on macOS.
//  - for breakpad on Linux the `uctx` parameter is always NULL.
//  - on Windows the crashpad backend can capture fast-fail crashes which
// by-pass SEH. Since `on_crash` is called by a local exception-handler, it will
// not be invoked when such a crash happened, even though a minidump will be
// sent.
//
type
  sentry_crash_function_t = function(
    const uctx: psentry_ucontext_t;
    event: sentry_value_t;
    closure: Pointer
  ): sentry_value_t; cdecl;

//
// Sets the `on_crash` callback.
//
// See the `sentry_crash_function_t` typedef above for more information.
//
procedure {SENTRY_API} sentry_options_set_on_crash(
  opts: psentry_options_t;
  func: sentry_crash_function_t;
  data: Pointer
); cdecl; external sentry_dll  delayed;

//
// Sets the DSN.
//
procedure sentry_options_set_dsn(
  opts: psentry_options_t;
  const dns: PAnsiChar
); cdecl; external sentry_dll  delayed;

//
// Gets the DSN.
//
function sentry_options_get_dsn(
  const opts: psentry_options_t
): PAnsiChar; cdecl; external sentry_dll  delayed;

//
// Sets the sample rate, which should be a double between `0.0` and `1.0`.
// Sentry will randomly discard any event that is captured using
// `sentry_capture_event` when a sample rate < 1 is set.
//
procedure sentry_options_set_sample_rate(
    opts: psentry_options_t;
    sample_rate: double
); cdecl; external sentry_dll  delayed;

//
// Gets the sample rate.
//
function sentry_options_get_sample_rate(
  const opts: psentry_options_t
): double; cdecl; external sentry_dll  delayed;

//
// Sets the release.
//
procedure sentry_options_set_release(
  opts: psentry_options_t;
  const release: PAnsiChar
); cdecl; external sentry_dll  delayed;

//
// Gets the release.
//
function sentry_options_get_release(
  const opts: psentry_options_t
): PAnsiChar; cdecl; external sentry_dll  delayed;

//
// Sets the environment.
//
procedure sentry_options_set_environment(
  opts: psentry_options_t;
  const environment: PAnsiChar
); cdecl; external sentry_dll  delayed;

//
// Gets the environment.
//
function sentry_options_get_environment(
  const opts: psentry_options_t
): PAnsiChar; cdecl; external sentry_dll  delayed;

//
// Sets the dist.
//
procedure sentry_options_set_dist(
  opts: psentry_options_t;
  const dist: PAnsiChar
); cdecl; external sentry_dll  delayed;

//
// Gets the dist.
//
function sentry_options_get_dist(
  const opts: psentry_options_t
): PAnsiChar; cdecl; external sentry_dll  delayed;

//
// Configures the http proxy.
//
// The given proxy has to include the full scheme, eg. `http://some.proxy/`.
//
procedure sentry_options_set_http_proxy(
  opts: psentry_options_t;
  const proxy: PAnsiChar
); cdecl; external sentry_dll  delayed;

//
// Returns the configured http proxy.
//
function sentry_options_get_http_proxy(
  opts: psentry_options_t
): PAnsiChar; cdecl; external sentry_dll  delayed;

//
// Configures the path to a file containing ssl certificates for
// verification.
//
procedure sentry_options_set_ca_certs(
  opts: psentry_options_t;
  const path: PAnsiChar
); cdecl; external sentry_dll  delayed;

//
// Returns the configured path for ca certificates.
//
function sentry_options_get_ca_certs(
  opts: psentry_options_t
): PAnsiChar; cdecl; external sentry_dll  delayed;

//
// Configures the name of the http transport thread.
//
procedure sentry_options_set_transport_thread_name(
    opts: psentry_options_t;
    const name: PAnsiChar
); cdecl; external sentry_dll  delayed;

//
// Returns the configured http transport thread name.
//
function sentry_options_get_transport_thread_name(
    const opts: psentry_options_t
): PAnsiChar; cdecl; external sentry_dll  delayed;

//
// Enables or disables debug printing mode.
//
procedure sentry_options_set_debug(
  opts: psentry_options_t;
  debug: Integer
); cdecl; external sentry_dll  delayed;

//
// Returns the current value of the debug flag.
//
function sentry_options_get_debug(
  const opts: psentry_options_t
): Integer; cdecl; external sentry_dll  delayed;

//
// Sets the number of breadcrumbs being tracked and attached to events.
//
// Defaults to 100.
//
procedure sentry_options_set_max_breadcrumbs(
    opts: psentry_options_t;
    max_breadcrumbs: NativeUInt
); cdecl; external sentry_dll  delayed;

//
// Gets the number of breadcrumbs being tracked and attached to events.
//
function sentry_options_get_max_breadcrumbs(
    const opts: psentry_options_t
): NativeUInt; cdecl; external sentry_dll  delayed;

//
// Type of the callback for logger function.
//
type
  sentry_logger_function_t = procedure(
    level: sentry_level_t;
    const message: PAnsiChar;
    args: va_list;
    userdata: Pointer
  ); cdecl;

//
// Sets the sentry-native logger function.
//
// Used for logging debug events when the `debug` option is set to true.
//
procedure sentry_options_set_logger(
    opts: psentry_options_t;
    func: sentry_logger_function_t;
    userdata: Pointer
); cdecl; external sentry_dll  delayed;

//
// Enables or disables automatic session tracking.
//
// Automatic session tracking is enabled by default and is equivalent to calling
// `sentry_start_session` after startup.
// There can only be one running session, and the current session will always be
// closed implicitly by `sentry_close`, when starting a new session with
// `sentry_start_session`, or manually by calling `sentry_end_session`.
//
procedure sentry_options_set_auto_session_tracking(
    opts: psentry_options_t;
    val: Integer
); cdecl; external sentry_dll  delayed;

//
// Returns true if automatic session tracking is enabled.
//
function sentry_options_get_auto_session_tracking(
    const opts: psentry_options_t
): Integer; cdecl; external sentry_dll  delayed;

//
// Enables or disabled user consent requirements for uploads.
//
// This disables uploads until the user has given the consent to the SDK.
// Consent itself is given with `sentry_user_consent_give` and
// `sentry_user_consent_revoke`.
//
procedure sentry_options_set_require_user_consent(
    opts: psentry_options_t;
    val: Integer); cdecl; external sentry_dll  delayed;

//
// Returns true if user consent is required.
//
function sentry_options_get_require_user_consent(
    const opts: psentry_options_t
): Integer; cdecl; external sentry_dll  delayed;

//
// Enables or disables on-device symbolication of stack traces.
//
// This feature can have a performance impact, and is enabled by default on
// Android. It is usually only needed when it is not possible to provide debug
// information files for system libraries which are needed for serverside
// symbolication.
//
procedure sentry_options_set_symbolize_stacktraces(
    opts: psentry_options_t;
    val: Integer
); cdecl; external sentry_dll  delayed;

//
// Returns true if on-device symbolication of stack traces is enabled.
//
function sentry_options_get_symbolize_stacktraces(
    const opts: psentry_options_t
): Integer; cdecl; external sentry_dll  delayed;


//
// Adds a new attachment to be sent along.
//
// `path` is assumed to be in platform-specific filesystem path encoding.
// API Users on windows are encouraged to use `sentry_options_add_attachmentw`
// instead.
//
procedure sentry_options_add_attachment(
  opts: psentry_options_t;
  const path: PAnsiChar
); cdecl; external sentry_dll  delayed;

//
// Sets the path to the crashpad handler if the crashpad backend is used.
//
// The path defaults to the `crashpad_handler`/`crashpad_handler.exe`
// executable, depending on platform, which is expected to be present in the
// same directory as the app executable.
//
// It is recommended that library users set an explicit handler path, depending
// on the directory/executable structure of their app.
//
// `path` is assumed to be in platform-specific filesystem path encoding.
// API Users on windows are encouraged to use `sentry_options_set_handler_pathw`
// instead.
//
procedure sentry_options_set_handler_path(
  opts: psentry_options_t;
  const path: PAnsiChar
); cdecl; external sentry_dll  delayed;

//
// Sets the path to the Sentry Database Directory.
//
// Sentry will use this path to persist user consent, sessions, and other
// artifacts in case of a crash. This will also be used by the crashpad backend
// if it is configured.
//
// The directory is used for "cached" data, which needs to persist across
// application restarts to ensure proper flagging of release-health sessions,
// but might otherwise be safely purged regularly.
//
// It is roughly equivalent to the type of `AppData/Local` on Windows and
// `XDG_CACHE_HOME` on Linux, and equivalent runtime directories on other
// platforms.
//
// It is recommended that users set an explicit absolute path, depending
// on their apps runtime directory. The path will be created if it does not
// exist, and will be resolved to an absolute path inside of `sentry_init`. The
// directory should not be shared with other application data/configuration, as
// sentry-native will enumerate and possibly delete files in that directory. An
// example might be `$XDG_CACHE_HOME/your-app/sentry`
//
// If no explicit path it set, sentry-native will default to `.sentry-native` in
// the current working directory, with no specific platform-specific handling.
//
// `path` is assumed to be in platform-specific filesystem path encoding.
// API Users on windows are encouraged to use
// `sentry_options_set_database_pathw` instead.
//
procedure sentry_options_set_database_path(
  opts: psentry_options_t;
  const path: PAnsiChar
); cdecl; external sentry_dll  delayed;

{$IF DEFINED(SENTRY_PLATFORM_WINDOWS)}
//
// Wide char version of `sentry_options_add_attachment`.
//
procedure sentry_options_add_attachmentw(
  opts: psentry_options_t;
  const path: PWideChar
); cdecl; external sentry_dll  delayed;

//
// Wide char version of `sentry_options_set_handler_path`.
//
procedure sentry_options_set_handler_pathw(
  opts: psentry_options_t;
  const path: PWideChar
); cdecl; external sentry_dll  delayed;

//
// Wide char version of `sentry_options_set_database_path`.
//
procedure sentry_options_set_database_pathw(
  opts: psentry_options_t;
  const path: PWideChar
); cdecl; external sentry_dll  delayed;
{$ENDIF}

//
// Enables forwarding to the system crash reporter. Disabled by default.
//
// This setting only has an effect when using Crashpad on macOS. If enabled,
// Crashpad forwards crashes to the macOS system crash reporter. Depending on
// the crash, this may impact the crash time. Even if enabled, Crashpad may
// choose not to forward certain crashes.
//
procedure sentry_options_set_system_crash_reporter_enabled(
    opts: psentry_options_t;
    enabled: Integer
); cdecl; external sentry_dll  delayed;

//
// Sets the maximum time (in milliseconds) to wait for the asynchronous tasks to
// end on shutdown, before attempting a forced termination.
//
procedure {SENTRY_API} sentry_options_set_shutdown_timeout(
  opts: psentry_options_t;
  shutdown_timeout: UInt64
); cdecl; external sentry_dll  delayed;

//
// Gets the maximum time (in milliseconds) to wait for the asynchronous tasks to
// end on shutdown, before attempting a forced termination.
//
function {SENTRY_API} sentry_options_get_shutdown_timeout(
  opts: psentry_options_t
): UInt64; cdecl; external sentry_dll  delayed;

//
// Sets a user-defined backend.
//
// Since creation and destruction of backends is not exposed in the API, this
// can only be used to set the backend to `NULL`, which disables the backend in
// the initialization.
//
procedure {SENTRY_API} sentry_options_set_backend(
  opts: psentry_options_t;
  backend: psentry_backend_t
); cdecl; external sentry_dll  delayed;



// -- Global APIs --

//
// Initializes the Sentry SDK with the specified options.
//
// This takes ownership of the options.  After the options have been set they
// cannot be modified any more.
// Depending on the configured transport and backend, this function might not be
// fully thread-safe.
// Returns 0 on success.
//
function sentry_init(
  options: psentry_options_t
): Integer; cdecl; external sentry_dll  delayed;

//
// Instructs the transport to flush its send queue.
//
// The `timeout` parameter is in milliseconds.
//
// Returns 0 on success, or a non-zero return value in case the timeout is hit.
//
function {SENTRY_API} sentry_flush(
  timeout: UInt64
): Integer; cdecl; external sentry_dll  delayed;


//
// Shuts down the sentry client and forces transports to flush out.
//
// Returns 0 on success.
//
function sentry_close: Integer; cdecl; external sentry_dll  delayed;

//
// Shuts down the sentry client and forces transports to flush out.
//
// This is a **deprecated** alias for `sentry_close`.
//
// Returns 0 on success.
//
function sentry_shutdown: Integer; cdecl; external sentry_dll  delayed;

//
// This will lazily load and cache a list of all the loaded libraries.
//
// Returns a new reference to an immutable, frozen list.
// The reference must be released with `sentry_value_decref`.
//
function sentry_get_modules_list: sentry_value_t; cdecl; external sentry_dll  delayed;

//
// Clears the internal module cache.
//
// For performance reasons, sentry will cache the list of loaded libraries when
// capturing events. This cache can get out-of-date when loading or unloading
// libraries at runtime. It is therefore recommended to call
// `sentry_clear_modulecache` when doing so, to make sure that the next call to
// `sentry_capture_event` will have an up-to-date module list.
//
procedure sentry_clear_modulecache; cdecl; external sentry_dll  delayed;

//
// Re-initializes the Sentry backend.
//
// This is needed if a third-party library overrides the previously installed
// signal handler. Calling this function can be potentially dangerous and should
// only be done when necessary.
//
// Returns 0 on success.
//
function sentry_reinstall_backend: Integer; cdecl; external sentry_dll  delayed;

//
// Gives user consent.
//
procedure sentry_user_consent_give; external sentry_dll  delayed;

//
// Revokes user consent.
//
procedure sentry_user_consent_revoke; external sentry_dll  delayed;

//
// Resets the user consent (back to unknown).
//
procedure sentry_user_consent_reset; external sentry_dll  delayed;

//
// Checks the current state of user consent.
//
function sentry_user_consent_get: sentry_user_consent_t; external sentry_dll  delayed;

//
// Sends a sentry event.
//
function sentry_capture_event(
  event: sentry_value_t
): sentry_uuid_t; cdecl; external sentry_dll  delayed;

//
// Captures an exception to be handled by the backend.
//
// This is safe to be called from a crashing thread and may not return.
//
procedure sentry_handle_exception(
  const uctx: psentry_ucontext_t
); cdecl; external sentry_dll  delayed;

//
// Adds the breadcrumb to be sent in case of an event.
//
procedure sentry_add_breadcrumb(
  breadcrumb: sentry_value_t
); cdecl; external sentry_dll  delayed;

//
// Sets the specified user.
//
procedure sentry_set_user(
  user: sentry_value_t
); cdecl; external sentry_dll  delayed;

//
// Removes a user.
//
procedure sentry_remove_user; cdecl; external sentry_dll  delayed;

//
// Sets a tag.
//
procedure sentry_set_tag(
  const key: PAnsiChar;
  const value: PAnsiChar
); cdecl; external sentry_dll  delayed;

//
// Removes the tag with the specified key.
//
procedure sentry_remove_tag(
  const key: PAnsiChar
); cdecl; external sentry_dll  delayed;

//
// Sets extra information.
//
procedure sentry_set_extra(
  const key: PAnsiChar;
  value: sentry_value_t
); cdecl; external sentry_dll  delayed;

//
// Removes the extra with the specified key.
//
procedure sentry_remove_extra(
  const key: PAnsiChar
); cdecl; external sentry_dll  delayed;

//
// Sets a context object.
//
procedure sentry_set_context(
  const key: PAnsiChar;
  value: sentry_value_t
); cdecl; external sentry_dll  delayed;

//
// Removes the context object with the specified key.
//
procedure sentry_remove_context(
  const key: PAnsiChar
); cdecl; external sentry_dll  delayed;

//
// Sets the event fingerprint.
//
// This accepts a variable number of arguments, and needs to be terminated by a
// trailing `NULL`.
//
procedure sentry_set_fingerprint(
  const fingerprint: PAnsiChar
  // ...
); cdecl; varargs; external sentry_dll  delayed;

//
// Removes the fingerprint.
//
procedure sentry_remove_fingerprint; cdecl; external sentry_dll  delayed;

//
// Sets the transaction.
//
procedure sentry_set_transaction(
  const transaction: PAnsiChar
); cdecl; external sentry_dll  delayed;

//
// Removes the transaction.
//
procedure sentry_remove_transaction; cdecl; external sentry_dll  delayed;

//
// Sets the event level.
//
procedure sentry_set_level(
  level: sentry_level_t
); cdecl; external sentry_dll  delayed;


//
// Starts a new session.
//
procedure sentry_start_session; cdecl; external sentry_dll  delayed;

//
// Ends a session.
//
procedure sentry_end_session; cdecl; external sentry_dll  delayed;


//

// Sets the path to sentry.dll; this must

// be called before any other sentry apis

//

procedure sentry_set_library_path(const path: string);


implementation

uses
  System.SysUtils;

procedure sentry_set_library_path(const path: string);
begin
  if LoadLibrary(PChar(path)) = 0 then
    RaiseLastOSError;
end;

procedure sentry_string_free(str: PAnsiChar); cdecl;
begin
  sentry_free(str);
end;

end.
