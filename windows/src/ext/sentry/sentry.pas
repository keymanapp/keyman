{$D-} // Don't include debug information
// Delphi translation of sentry.h
// Sentry Native API 0.2.3
// https://github.com/getsentry/sentry-native
unit sentry;

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
  SENTRY_SDK_VERSION = '0.2.3';
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
// Creates a new empty event value.
//
function sentry_value_new_event: sentry_value_t; cdecl; external sentry_dll  delayed;

//
// Creates a new message event value.
//
// `logger` can be NULL to omit the logger value.
//
function sentry_value_new_message_event(
  level: sentry_level_t;
  const logger: PAnsiChar;
  const text: PAnsiChar
): sentry_value_t; cdecl; external sentry_dll  delayed;

//
// Creates a new breadcrumb with a specific type and message.
//
// Either parameter can be NULL in which case no such attributes is created.
//
function sentry_value_new_breadcrumb(
  const _type: PAnsiChar;
  const message: PAnsiChar
): sentry_value_t; cdecl; external sentry_dll  delayed;

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
// Adds a stacktrace to an event.
//
// If `ips` is NULL the current stacktrace is captured, otherwise `len`
// stacktrace instruction pointers are attached to the event.
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
// The stacktrace is written to `stacktrace_out` with upt o `max_len` frames
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
// The stacktrace is written to `stacktrace_out` with upt o `max_len` frames
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
  sentry_options_s = record end;
  sentry_options_t = sentry_options_s;
  psentry_options_t = ^sentry_options_t;

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
// Serializes the envelope
//
// The return value needs to be freed with sentry_string_free().
//
function sentry_envelope_serialize(
  const envelope: psentry_envelope_t;
  var size_out: size_t
): PAnsiChar; cdecl; external sentry_dll  delayed;

//
// serializes the envelope into a file.
//
// returns 0 on success.
//
function sentry_envelope_write_to_file(
  const envelope: psentry_envelope_t;
  const path: PAnsiChar
): Integer; cdecl; external sentry_dll  delayed;

//
// Type of the callback for transports.
//
type
  sentry_transport_function_t = procedure(
    const envelope: psentry_envelope_t;
    data: Pointer
  ); cdecl;

//
// This represents an interface for user-defined transports.
//
// This type is *deprecated*, and will be replaced by an opaque pointer type and
// builder methods in a future version.
//
type
  psentry_transport_s = ^sentry_transport_s;

  _sentry_transport_send_envelope_func = procedure(t: psentry_transport_s; e: psentry_envelope_t); cdecl;
  _sentry_transport_func = procedure(s: psentry_transport_s); cdecl;

  sentry_transport_s = record
    send_envelope_func: _sentry_transport_send_envelope_func;
    startup_func: _sentry_transport_func;
    shutdown_func: _sentry_transport_func;
    free_func: _sentry_transport_func;
    data: Pointer;
  end;

  sentry_transport_t = sentry_transport_s;
  psentry_transport_t = ^sentry_transport_t;

  _sentry_transport_new_func = procedure(e: psentry_envelope_t; data: Pointer); cdecl;

function sentry_new_function_transport(
  func: _sentry_transport_new_func;
  data: Pointer
): psentry_transport_t; cdecl; external sentry_dll  delayed;

//
// Generic way to free a transport.
//
procedure sentry_transport_free(
  transport: psentry_transport_t
); cdecl; external sentry_dll  delayed;

//
// This represents an opaque backend.
//
// This declaration is *deprecated* and will be removed in a future version.
//
type
  sentry_backend_s = record end;
  sentry_backend_t = sentry_backend_s;
  psentry_backend_t = ^sentry_backend_t;

//
// Generic way to free a backend.
//
// This function is *deprecated* and will be removed in a future version.
//
procedure sentry_backend_free(
  backend: psentry_backend_t
); cdecl; external sentry_dll  delayed;

//
// Type of the callback for modifying events.
//
type
  sentry_event_function_t = function(
    event: sentry_value_t;
    hint: Pointer;
    closure: Pointer
  ): sentry_value_t; cdecl;

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
  func: sentry_transport_function_t
); cdecl; external sentry_dll  delayed;

//
// Sets the before send callback.
//
procedure sentry_options_set_before_send(
  opts: psentry_options_t;
  func: sentry_event_function_t;
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
// Adds a new attachment to be sent along.
//
procedure sentry_options_add_attachment(
  opts: psentry_options_t;
  const name: PAnsiChar;
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
// The path defaults to `.sentry-native` in the current working directory, will
// be created if it does not exist, and will be resolved to an absolute path
// inside of `sentry_init`.
//
// It is recommended that library users set an explicit absolute path, depending
// on their own apps directory.
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
  const name: PAnsiChar;
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

// -- Global APIs --

//
// Initializes the Sentry SDK with the specified options.
//
// This takes ownership of the options.  After the options have been set they
// cannot be modified any more.
//
function sentry_init(
  options: psentry_options_t
): Integer; cdecl; external sentry_dll  delayed;

//
// Shuts down the sentry client and forces transports to flush out.
//
procedure sentry_shutdown; cdecl; external sentry_dll  delayed;

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
// Returns the client options.
//
// This might return NULL if sentry is not yet initialized.
//
function sentry_get_options: psentry_options_t; cdecl; external sentry_dll  delayed;

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
  uctx: psentry_ucontext_t
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
