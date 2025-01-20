---
title: Background - Keyman Core API
---

## Namespace
All calls, types and enums are prefixed with the namespace identifier `km_core_`

## API idioms

### Error Handling

Error handling and success failure notification are communicated through a
general mechanism similar to COM’s `HRESULT` scheme (unlike COM, any non-zero
value is an error). Any functions that can fail will always return a status
value and all results are returned via outparams passed to the function.

### Passing variable length data out

Almost all calls marshalling variable length aggregate data in or out of an API
object take the form:

```c
km_core_status fn_name(object_ref, buffer_ptr, size_ptr)
```
where the `buffer_ptr` is nullable and all other arguments are required (will
result in an [`KM_CORE_STATUS_INVALID_ARGUMENT`](#km_core_status_codes)
status being returned if nulled). When `buffer_ptr` is `nullptr` or `0` the
function will place the size of the required buffer in the variable pointed to
by `size_ptr`.

### Resource management

Calls which result in the allocation of resources, regardless of resulting
ownership, are of the form:
```c
km_core_status fn_name(object_ref, handle_out_ptr)
```
where `handle_out_ptr` is a valid pointer to a caller allocated variable to hold
the resulting resource handle. This is often a reference to a created object.
Unless stated all arguments are required (will result in an
[`KM_CORE_STATUS_INVALID_ARGUMENT`](#km_core_status_codes) status being
returned if nulled).

All dispose calls are designed to accept `nullptr` or `0` as a valid value and
will do nothing in that event.

### Fixed size attribute access

For accessors to fixed size attributes of an object these will take the form:
```c
attr_value fn_name(object_ref)
```
`object_ref` is required to be valid and will result in a nonsense value being returned if `nullptr` or `0`.

### Versioning scheme

This follows the libtool interface versioning scheme of `current.age.revision`:

`current`

The most recent interface number that the engine implements.

`age`

How many interface numbers back from current the library implements. E.g. 5.2.0
would mean the library provides interface versions 3-5 and 5.0.0 would mean just
interface version 5 and nothing older.

`revision`

The implementation version of the current interface. This represents
improvements to the code that don't change the intended behaviour of the
interface such as bug fixes and optimisations.

For Linux and other OS which support this scheme the dynamic linker will
automatically choose the most updated version if more than one implementation is
available. For Windows or dynamic loaded shared objects on Linux you can use the
[km_core_get_engine_attrs] call and [Library version
macros](#lib-version-macros) to check the loaded DLL supplies the correct
interface.

-------------------------------------------------------------------------------

# Common functions, types, and macros


## Basic types

Fundamental types for representing data passed across the API.

### km_core_cp type {#km_core_cp}

`uint16_t/char16_t`

Represents a UTF16 codepoint, most strings are passed as UTF16.

### km_core_usv type {#km_core_usv}

`uint32_t/char32_t`

An integral type capable of holding a single Unicode Scalar Value, a decoded UTF
codepoint.

### km_core_virtual_key type {#km_core_virtual_key}

`uint16_t`

An integral type capable of holding a platform specific virtual key code.

### km_core_status type {#km_core_status}

`uint32_t`

An integral 32 bit wide type capable of holding any valid status code as defined
by the `enum` [km_core_status_codes].

### km_core_modifier_state type {#km_core_modifier_state}

`uint16_t`

An integral type bitmask representing the state of each modifier key.



## Resource types

Opaque types for representing resources provided or created by the keyboard
processor implementation.

### km_core_keyboard struct {#km_core_keyboard}

Represents a keyboard loaded from disk, that can be executed by the keyboard
processor to consume events, update state associated with an insertion point and
produce action items. A keyboard object may be referenced by any number of state
objects but must be disposed of after all state objects referencing it have
first been disposed of.

### km_core_state struct {#km_core_state}

Represents all state associated with an insertion point using a keyboard. This
tracks context, and current action items resulting from a processed keyboard
event. There can be many state objects using the same keyboard. A state object
may not live longer than the keyboard it manages state for.

### km_core_options struct {#km_core_options}

Represents a set of option items for environmental state and keyboard state.

<!--
```c
// Basic types
//
typedef uint16_t    km_core_virtual_key; // A virtual key code.
typedef uint32_t    km_core_status;      // Status return code.

// Opaque object types.
//
typedef struct km_core_keyboard    km_core_keyboard;
typedef struct km_core_state       km_core_state;
typedef struct km_core_options     km_core_options;

```
-->


<!--
```c
// Forward declarations
//
typedef struct km_core_option_item  km_core_option_item;

// Callback function used to to access Input Method eXtension library functions
// from Keyman Core
//
typedef uint8_t (*km_core_keyboard_imx_platform)(km_core_state*, uint32_t, void*);

```
-->

-------------------------------------------------------------------------------

# km_core_status_codes enum {#km_core_status_codes}

## Description

An error code mechanism similar to COM’s `HRESULT` scheme (unlike COM, any
non-zero value is an error).

## Specification

```c
enum km_core_status_codes {
  KM_CORE_STATUS_OK = 0,
  KM_CORE_STATUS_NO_MEM = 1,
  KM_CORE_STATUS_IO_ERROR = 2,
  KM_CORE_STATUS_INVALID_ARGUMENT = 3,
  KM_CORE_STATUS_KEY_ERROR = 4,
  KM_CORE_STATUS_INSUFFICENT_BUFFER = 5,
  KM_CORE_STATUS_INVALID_UTF = 6,
  KM_CORE_STATUS_INVALID_KEYBOARD = 7,
  KM_CORE_STATUS_NOT_IMPLEMENTED = 8,
  KM_CORE_STATUS_OS_ERROR = 0x80000000
};

```

## Values

`KM_CORE_STATUS_OK`

: Success code. Call completed as documented.

`KM_CORE_STATUS_NO_MEM`

: The call failed to allocate memory during its execution, causing it to fail.

`KM_CORE_STATUS_IO_ERROR`

: The call performed an I/O operation which failed, causing it to fail.

`KM_CORE_STATUS_INVALID_ARGUMENT`

: The call detected one of its parameters was invalid or unsafe.

`KM_CORE_STATUS_KEY_ERROR`

: The provided key or index into a collection object was not present.

`KM_CORE_STATUS_INSUFFICENT_BUFFER`

: The provided buffer did not contain enough space to fully encode or copy the
result of this call.

`KM_CORE_STATUS_INVALID_UTF`

: A malformed or partial UTF sequence prevented complete decoding of a unicode
string.

`KM_CORE_STATUS_INVALID_KEYBOARD`

: An attempt to decode a keyboard file failed.

`KM_CORE_STATUS_OS_ERROR`

: This allows encapsulating a platform error code: the remaining 31 low bits are
the error code returned by the OS for cases where the failure mode is platform
specific. For HRESULT codes this only permits failure codes to be passed and not
success codes.

-------------------------------------------------------------------------------

# km_core_attr struct {#km_core_attr}

## Description

A structure describing information about Keyman Core implementing this API.

## Specification

```c

typedef struct {
  size_t      max_context;
  uint16_t    current;
  uint16_t    revision;
  uint16_t    age;
  uint16_t    technology;
  char const *vendor;
} km_core_attr;

```
## Members

`max_context`
: Maximum context size supported by processor.

`current`
: Current API number supported.

`revision`
: Implementation number of current API.

`age`
: current - age == Oldest API number supported.

`technology`
: A bit field of [km_core_tech_value] values,
specifiying which Keyboard technologies the engine supports.

`vendor`
: A UTF-8 encoded string identifying the implementer of the processor.

-------------------------------------------------------------------------------

# km_core_tech_value enum {#km_core_tech_value}

## Description

Values for a bit field indicating which keyboarding technologies a keyboard
processor supports.

## Specification

```c

enum km_core_tech_value {
  KM_CORE_TECH_UNSPECIFIED = 0,
  KM_CORE_TECH_MOCK        = 1 << 0,
  KM_CORE_TECH_KMX         = 1 << 1,
  KM_CORE_TECH_LDML        = 1 << 2
};

```
## Values

`KM_CORE_TECH_UNSPECIFIED`
: The keyboard processor implementation does not disclose which technologies it
implements.

`KM_CORE_TECH_MOCK`
: The keyboard processor implements a simple en-US keyboard for the purposes of
testing the API.

`KM_CORE_TECH_UNSPECIFIED`
: The keyboard processor implements a Keyman KMX compatible engine.

`KM_CORE_TECH_UNSPECIFIED`
: The keyboard processor implements a LDML capable processing engine.

-------------------------------------------------------------------------------

# km_core_get_engine_attrs() {#km_core_get_engine_attrs}

## Description

Get access processors attributes describing version and technology implemented.

## Specification

```c
KMN_API
km_core_attr const *
km_core_get_engine_attrs(km_core_state const *state);

```

## Parameters

`state`
: An opaque pointer to a [km_core_state].

## Returns
A pointer to a [km_core_attr] structure. Do not modify the contents of this
structure.

-------------------------------------------------------------------------------

# km_core_bool enum {#km_core_bool}

## Description

Defines a boolean state.

## Specification
```c
typedef enum { KM_CORE_FALSE = 0, KM_CORE_TRUE = 1 } km_core_bool;

```
-------------------------------------------------------------------------------


[km_core_cp]: background#km_core_cp "km_core_cp type"
[km_core_usv]: background#km_core_usv "km_core_usv type"
[km_core_virtual_key]: background#km_core_virtual_key "km_core_virtual_key type"
[km_core_status]: background#km_core_status "km_core_status type"
[km_core_modifier_state]: background#km_core_modifier_state "km_core_modifier_state type"
[km_core_keyboard]: background#km_core_keyboard "km_core_keyboard struct"
[km_core_state]: background#km_core_state "km_core_state struct"
[km_core_options]: background#km_core_options "km_core_options struct"
[km_core_status_codes]: background#km_core_status_codes "km_core_status_codes enum"
[km_core_attr]: background#km_core_attr "km_core_attr struct"
[km_core_tech_value]: background#km_core_tech_value "km_core_tech_value enum"
[km_core_get_engine_attrs]: background#km_core_get_engine_attrs "km_core_get_engine_attrs function"
[km_core_bool]: background#km_core_bool "km_core_bool enum"
[km_core_caps_state]: state#km_core_caps_state "km_core_caps_state enum"
[km_core_actions]: state#km_core_actions "km_core_actions struct"
[km_core_state_get_actions]: state#km_core_state_get_actions "km_core_state_get_actions function"
[km_core_context_status]: state#km_core_context_status "km_core_context_status enum"
[km_core_state_context_set_if_needed]: state#km_core_state_context_set_if_needed "km_core_state_context_set_if_needed function"
[km_core_state_context_clear]: state#km_core_state_context_clear "km_core_state_context_clear function"
[km_core_option_scope]: options#km_core_option_scope "km_core_option_scope enum"
[km_core_option_item]: options#km_core_option_item "km_core_option_item struct"
[km_core_options_list_size]: options#km_core_options_list_size "km_core_options_list_size function"
[km_core_state_options_update]: options#km_core_state_options_update "km_core_state_options_update function"
[km_core_state_options_to_json]: options#km_core_state_options_to_json "km_core_state_options_to_json function"
[km_core_keyboard_attrs]: keyboards#km_core_keyboard_attrs "km_core_keyboard_attrs struct"
[km_core_keyboard_key]: keyboards#km_core_keyboard_key "km_core_keyboard_key struct"
[km_core_keyboard_imx]: keyboards#km_core_keyboard_imx "km_core_keyboard_imx struct"
[km_core_keyboard_load_from_blob]: keyboards#km_core_keyboard_load_from_blob "km_core_keyboard_load_from_blob function"
[km_core_keyboard_dispose]: keyboards#km_core_keyboard_dispose "km_core_keyboard_dispose function"
[km_core_keyboard_get_attrs]: keyboards#km_core_keyboard_get_attrs "km_core_keyboard_get_attrs function"
[km_core_keyboard_get_key_list]: keyboards#km_core_keyboard_get_key_list "km_core_keyboard_get_key_list function"
[km_core_keyboard_key_list_dispose]: keyboards#km_core_keyboard_key_list_dispose "km_core_keyboard_key_list_dispose function"
[km_core_keyboard_imx_list_dispose]: keyboards#km_core_keyboard_imx_list_dispose "km_core_keyboard_imx_list_dispose function"
[km_core_state_imx_register_callback]: keyboards#km_core_state_imx_register_callback "km_core_state_imx_register_callback function"
[km_core_state_imx_deregister_callback]: keyboards#km_core_state_imx_deregister_callback "km_core_state_imx_deregister_callback function"
[km_core_state_create]: keyboards#km_core_state_create "km_core_state_create function"
[km_core_state_clone]: keyboards#km_core_state_clone "km_core_state_clone function"
[km_core_state_dispose]: keyboards#km_core_state_dispose "km_core_state_dispose function"
[km_core_debug_context_type]: keyboards#km_core_debug_context_type "km_core_debug_context_type enum"
[km_core_state_context_debug]: keyboards#km_core_state_context_debug "km_core_state_context_debug function"
[km_core_cp_dispose]: keyboards#km_core_cp_dispose "km_core_cp_dispose function"
[km_core_state_to_json]: keyboards#km_core_state_to_json "km_core_state_to_json function"
[km_core_event_flags]: processor#km_core_event_flags "km_core_event_flags enum"
[km_core_process_event]: processor#km_core_process_event "km_core_process_event function"
[km_core_event]: processor#km_core_event "km_core_event function"
[km_core_event_code]: processor#km_core_event_code "km_core_event_code enum"