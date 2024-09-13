/*
  Copyright:    © 2018 SIL International.
  Description:  Cross platform API C/C++ declarations for libkeymancore
  Create Date:  2 Oct 2018
  Authors:      Tim Eves (TSE)
*/
#pragma once

#include <stdint.h>
#include <stdlib.h>
#include <keyman/keyman_core_api_bits.h>
#include <keyman/keyman_core_api_vkeys.h>
#include <keyman/keyman_core_api_version.h>

#if defined(__cplusplus)
extern "C"
{
#endif

/*
---
filename: index.md
title: Keyman Core API
---

## Overview
Keyman Core is the component of Keyman Engine that implements keyboarding rules.
It is platform independent and allows support for different keyboard formats to
be implemented within Keyman Engine. Eventually, Keyman Core will be used in
Keyman on all platforms. As of writing, Keyman for Linux, Keyman for Windows,
Keyman for macOS, and Keyman Developer use Keyman Core.

This is an internal API intended for use only within Keyman Engine.

## Reference

* [Background](background)
* [Changes from earlier versions](changes)
* [Keyboards](keyboards)
* [Options](options)
* [Processor](processor)
* [State and Actions](state)
* [JSON introspection Schema](json-schema)
* [Building Keyman Core](building)

## Requirements
1. Cross platform.
2. Cross language.
3. Facilitate stateless operation of the Engine.
4. Keyboard format agnostic -- support both KMN and future LDML based keyboards.
5. Support querying Engine attributes.
6. Support querying Keyboard attributes.
7. Idempotent


## Design decisions in support of requirements
- Use C or C99 types and calling convention for the interface, it has the
  broadest language FFI support. [1,2]
- Have client (Platform layer) code load keyboards, manage & pass state. [3,4,7]
- Provide query calls to return static attributes data for keyboards and
  engine [5,6]
- Provide get/set calls for client accessible keyboard state information [3,4]


## Glossary
- __Platform layer:__
The code that consumes the Keyman Core API, and provides the
operating system-specific handling of keystroke events and integration with
applications.
- __Client Application:__
The application that has the focus and receives text events from the Platform
layer.
- __Context:__ Text preceding the insertion point
- __Marker:__ Positional state that can be placed in the Context.
- __Keyboard:__ A set of rules for execution by an Engine
- __Option:__ A variable in a dynamic or static key value store.
- __Processor:__
The component that implements this API and can parse and execute a particular
keyboard.
- __State:__ An object that holds internal state of the Processor for a given
insertion point
- __Action:__
A directive output by the processor detailing how the Platform layer should
transform the Client Application's text buffer. There may be several items
produced by a single keyboard event.
- __Keyboard Event:__
A virtual key event and modifier map received from the Platform layer to be
processed with the state object for this Client application.
- __Virtual Key:__
A code based on the US English layout, with values matching the Windows
virtual key codes. See `keyman_core_api_vkeys.h` for definitions.
- __Modifier Key:__
The set of Control, Shift, Alt, Caps Lock keys. On some platforms these may
have other names (e.g. Alt is called Option on macOS); other platform-specific
modifiers such as Windows key are excluded from this set. Some modifiers are
transient, such as Control, and others have long-lasting state, such as
Caps Lock.

---
filename: changes.md
title: Changes - Keyman Core API
---

## Changes between 16.0 and 17.0

* The namespace identifier has changed from `km_kbp_` to `km_core_`.
* Most context APIs are now private, and [km_core_state_context_set_if_needed] is the
  primary context function. Private APIs are available in
  `keyman_core_api_context.h`.
* The action queue APIs are now private and deprecated. Instead, use
  [km_core_state_get_actions]. Private APIs are available in
  `keyman_core_api_actions.h`.
* Debug APIs are available in `keyman_core_api_debug.h`.

-------------------------------------------------------------------------------

---
filename: background.md
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

### km_core_cu type

`uint16_t/char16_t`

Represents a UTF16 codepoint, most strings are passed as UTF16.

### km_core_usv type

`uint32_t/char32_t`

An integral type capable of holding a single Unicode Scalar Value, a decoded UTF
codepoint.

### km_core_virtual_key type

`uint16_t`

An integral type capable of holding a platform specific virtual key code.

### km_core_status type

`uint32_t`

An integral 32 bit wide type capable of holding any valid status code as defined
by the `enum` [km_core_status_codes].

### km_core_modifier_state type

`uint16_t`

An integral type bitmask representing the state of each modifier key.



## Resource types

Opaque types for representing resources provided or created by the keyboard
processor implementation.

### km_core_keyboard struct

Represents a keyboard loaded from disk, that can be executed by the keyboard
processor to consume events, update state associated with an insertion point and
produce action items. A keyboard object may be referenced by any number of state
objects but must be disposed of after all state objects referencing it have
first been disposed of.

### km_core_state struct

Represents all state associated with an insertion point using a keyboard. This
tracks context, and current action items resulting from a processed keyboard
event. There can be many state objects using the same keyboard. A state object
may not live longer than the keyboard it manages state for.

### km_core_options struct

Represents a set of option items for environmental state and keyboard state.

<!--
```c */
// Basic types
//
typedef uint16_t    km_core_virtual_key; // A virtual key code.
typedef uint32_t    km_core_status;      // Status return code.

// Opaque object types.
//
typedef struct km_core_keyboard    km_core_keyboard;
typedef struct km_core_state       km_core_state;
typedef struct km_core_options     km_core_options;

/*
```
-->


<!--
```c */
// Forward declarations
//
typedef struct km_core_option_item  km_core_option_item;

// Callback function used to to access Input Method eXtension library functions
// from Keyman Core
//
typedef uint8_t (*km_core_keyboard_imx_platform)(km_core_state*, uint32_t, void*);

/*
```
-->

-------------------------------------------------------------------------------

# km_core_status_codes enum

## Description

An error code mechanism similar to COM’s `HRESULT` scheme (unlike COM, any
non-zero value is an error).

## Specification

```c */
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

/*
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

# km_core_attr struct

## Description

A structure describing information about Keyman Core implementing this API.

## Specification

```c */

typedef struct {
  size_t      max_context;
  uint16_t    current;
  uint16_t    revision;
  uint16_t    age;
  uint16_t    technology;
  char const *vendor;
} km_core_attr;

/*
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

# km_core_tech_value enum

## Description

Values for a bit field indicating which keyboarding technologies a keyboard
processor supports.

## Specification

```c */

enum km_core_tech_value {
  KM_CORE_TECH_UNSPECIFIED = 0,
  KM_CORE_TECH_MOCK        = 1 << 0,
  KM_CORE_TECH_KMX         = 1 << 1,
  KM_CORE_TECH_LDML        = 1 << 2
};

/*
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

# km_core_get_engine_attrs()

## Description

Get access processors attributes describing version and technology implemented.

## Specification

```c */
KMN_API
km_core_attr const *
km_core_get_engine_attrs(km_core_state const *state);

/*
```

## Parameters

`state`
: An opaque pointer to a [km_core_state].

## Returns
A pointer to a [km_core_attr] structure. Do not modify the contents of this
structure.

-------------------------------------------------------------------------------

# km_core_bool enum

## Description

Defines a boolean state.

## Specification
```c */
typedef enum { KM_CORE_FALSE = 0, KM_CORE_TRUE = 1 } km_core_bool;

/*
```
-------------------------------------------------------------------------------

---
filename: state.md
title: State and Actions - Keyman Core API
---

A State object maintains all per keyboard related state including context
and dynamic options ("option stores" in kmn format).

When a keystroke is processed by Keyman Core, Core provides back a set of actions
for the Platform layer to emit to the Client application. These actions are
owned by the state object.

-------------------------------------------------------------------------------

# km_core_caps_state enum

## Description

Describes a change to the hardware caps lock indicator state requested by
Keyman Core to the Platform layer.

## Specification
```c */
typedef enum { KM_CORE_CAPS_UNCHANGED = -1, KM_CORE_CAPS_OFF = 0, KM_CORE_CAPS_ON = 1 } km_core_caps_state;

/*
```
## Values

`KM_CORE_CAPS_UNCHANGED`
: Caps lock state has not changed in this event.

`KM_CORE_CAPS_OFF`
: As a result of processing this event, the Platform layer should switch off
  Caps Lock on the hardware keyboard.

`KM_CORE_CAPS_ON`
: As a result of processing this event, the Platform layer should switch on
  Caps Lock on the hardware keyboard.

-------------------------------------------------------------------------------

# km_core_actions struct

## Description

This structure provides the results of processing a key event to the Platform layer and
should be processed by the Platform layer to issue commands to the os text
services framework to transform the text store in the Client Application, among
other actions.

This API replaces the Action items APIs, which are now deprecated and will be
removed in the future.

## Specification
```c */
typedef struct {
  unsigned int code_points_to_delete;
  const km_core_usv* output;
  km_core_option_item * persist_options;
  km_core_bool do_alert;
  km_core_bool emit_keystroke;
  km_core_caps_state new_caps_lock_state;
  const km_core_usv* deleted_context;
} km_core_actions;

/*
```
## Members

`code_points_to_delete`
: Number of codepoints (not codeunits!) to delete from app context.

`output`
: Null-term string of characters to insert into document.

`persist_options`
: List of options to persist, terminated with `KM_CORE_OPTIONS_END`.

`do_alert`
: Issue a beep, 0 = no, 1 = yes.

`emit_keystroke`
: Emit the (unmodified) input keystroke to the application, 0 = no, 1 = yes.

`new_caps_lock_state`
: -1=unchanged, 0=off, 1=on

`deleted_context`
: Reference copy of actual UTF32 codepoints deleted from end of context
  (closest to caret) exactly code_points_to_delete in length (plus null
  terminator). Used to determine encoding conversion differences when
  deleting; only set when using [km_core_state_get_actions], otherwise nullptr.

-------------------------------------------------------------------------------

# km_core_state_get_actions()

## Description

Returns a pointer to an actions object which details all the actions
that the Platform layer must take after a keystroke. The `code_points_to_delete`
action must be performed before the `output` action, but the other
actions may be performed in any order.

## Specification
```c */
KMN_API
km_core_actions const *
km_core_state_get_actions(
  km_core_state const *state
);

/*
```
## Parameters

`state`
: An opaque pointer to a state object.

## Returns

A pointer to a [km_core_actions] object. This data becomes invalid
when the state object is destroyed, or after a call to
[km_core_process_event]. Do not modify the contents of this data.

-------------------------------------------------------------------------------

# km_core_context_status enum

## Description

Return values for [km_core_state_context_set_if_needed].

## Specification

```c */
typedef enum {
  KM_CORE_CONTEXT_STATUS_UNCHANGED = 0,
  KM_CORE_CONTEXT_STATUS_UPDATED = 1,
  KM_CORE_CONTEXT_STATUS_CLEARED = 2,
  KM_CORE_CONTEXT_STATUS_ERROR = 3,
  KM_CORE_CONTEXT_STATUS_INVALID_ARGUMENT = 4,
} km_core_context_status;

/*
```

## Values

`KM_CORE_CONTEXT_STATUS_UNCHANGED`
: Cached context change was not needed.

`KM_CORE_CONTEXT_STATUS_UPDATED`
: Cached context was set to application context.

`KM_CORE_CONTEXT_STATUS_CLEARED`
: Application context was invalid, perhaps had unpaired surrogates,
  and so cached context was cleared instead.

`KM_CORE_CONTEXT_STATUS_ERROR`
: Internal error.

`KM_CORE_CONTEXT_STATUS_INVALID_ARGUMENT`
: One or more parameters was null.

-------------------------------------------------------------------------------

# km_core_state_context_set_if_needed()

## Description

Sets the internal cached context for the state object, to the passed-in
application context string, if it differs from the codepoints in the
cached context. For the purposes of comparison, (1) cached markers are
ignored, (2) if the cached context is shorter than the application
context, it is considered identical, but (3) if the cached context is
longer, then it is considered different.

If a difference is found, then the cached context will be set to the
application context, and thus any cached markers will be cleared.

[km_core_state_context_set_if_needed] and [km_core_state_context_clear]
will replace most uses of the existing Core context APIs.

## Specification
```c */
KMN_API
km_core_context_status
km_core_state_context_set_if_needed(
  km_core_state *state,
  km_core_cu const *application_context
);

/*
```
## Parameters

`state`
: An opaque pointer to a state object.

`application_context`
: A pointer to an null-terminated array of utf16 encoded data representing
  the current context from the application.

## Returns

A value from the [km_core_context_status] enum.

-------------------------------------------------------------------------------

# km_core_state_context_clear()

## Description

Clears the internal cached context for the state. This is the same as
`km_core_context_clear(km_core_state_context(&state))`.

[km_core_state_context_set_if_needed] and [km_core_state_context_clear]
will replace most uses of the existing Core context APIs.

## Specification
```c */
KMN_API
km_core_status
km_core_state_context_clear(
  km_core_state *state
);

/*
```
## Parameters

`state`:
An opaque pointer to a state object.

## Returns

`KM_CORE_STATUS_OK`
: On success.

`KM_CORE_STATUS_INVALID_ARGUMENT`
: If any parameters are null.

-------------------------------------------------------------------------------

---
filename: options.md
title: Options - Keyman Core API
---

A state’s default options are set from the keyboard at creation time and the
environment. The Platform layer is then is expected to apply any persisted
options it is maintaining.  Options are passed into and out of API functions as
simple C arrays of [km_core_option_item] terminated with a `KM_CORE_OPTIONS_END`
sentinel value. A state's options are exposed and manipulatable via the
[km_core_options] API. All option values are of type C string.

During processing when the Platform layer finds a PERSIST action type it should
store the updated option in the appropriate place, based on its scope.
For RESET the processor will apply the pristine value from the original scope,
the Platform layer should update that only if it manages a previously persisted
value.

-------------------------------------------------------------------------------

# km_core_option_scope enum

## Description

## Specification

```c */
enum km_core_option_scope {
  KM_CORE_OPT_UNKNOWN      = 0,
  KM_CORE_OPT_KEYBOARD     = 1,
  KM_CORE_OPT_ENVIRONMENT  = 2,
  KM_CORE_OPT_MAX_SCOPES
};

/*
```
## Values

`KM_CORE_OPT_UNKNOWN`
: An unknown option type. Reserved.

`KM_CORE_OPT_KEYBOARD`
: An option that is defined for the currently active keyboard;
  not all processors support this type of option. These options
  are specific to the active keyboard.

`KM_CORE_OPT_ENVIRONMENT`
: Properties of the current environment, often but not necessarily
  always read-only.

-------------------------------------------------------------------------------

# km_core_option_item struct

## Description

Defines a single option to be passed into the Keyman Core from the
Platform layer.

## Specification
```c */
struct km_core_option_item {
  km_core_cu const *   key;
  km_core_cu const *   value;
  uint8_t             scope;
};

#define KM_CORE_OPTIONS_END { 0, 0, 0 }
/*
```
## Members

`key`
: Null-terminated string key for the option

`value`
: Null-terminated string value for the option

`scope`
: Scope which an option belongs to, from [km_core_option_scope].

-------------------------------------------------------------------------------

# km_core_options_list_size()

## Description
Return the length of a terminated [km_core_option_item] array (options
list).

## Specification
```c */
KMN_API
size_t
km_core_options_list_size(km_core_option_item const *opts);

/*
```
## Parameters

`opts`
: A pointer to a `KM_CORE_OPTIONS_END` terminated array of
  [km_core_option_item] values.

## Returns

The number of items in the list, not including terminating item,
or 0 if `opts` is null.

-------------------------------------------------------------------------------

# km_core_state_option_lookup

## Description

Lookup an option based on its key, in an options list.

## Specification
```c */
KMN_API
km_core_status
km_core_state_option_lookup(km_core_state const *state,
                      uint8_t scope,
                      km_core_cu const *key,
                      km_core_cu const **value);

/*
```
## Parameters

`state`
: An opaque pointer to a state object.

`scope`
: Which key-value store to interrogate.

`key`
: A UTF-16 string that matches the key in the target [km_core_option_item].

`value`
: A pointer to the result variable: A pointer to a UTF-16 string value owned
  by the state or keyboard object at the time of the call. This pointer is
  only valid *until* the next call to any function on this API and should be
  used immediately.

## Returns

`KM_CORE_STATUS_OK`
: On success.

`KM_CORE_STATUS_INVALID_ARGUMENT`
: If non-optional parameters are null, or if the scope is invalid.

`KM_CORE_STATUS_KEY_ERROR`
: The key cannot be found.

-------------------------------------------------------------------------------

# km_core_state_options_update()

## Description

Adds or updates one or more options from a list of [km_core_option_item]s.

## Specification
``` */
KMN_API
km_core_status
km_core_state_options_update(km_core_state *state,
                      km_core_option_item const *new_opts);

/*
```
## Parameters
`state`
: An opaque pointer to a state object.

`new_opts`
: An array of [km_core_option_item] objects to update or add. Must be
  terminated with `KM_CORE_OPTIONS_END`.

## Returns

`KM_CORE_STATUS_OK`
: On success.

`KM_CORE_STATUS_INVALID_ARGUMENT`
: If non-optional parameters are null.

`KM_CORE_STATUS_NO_MEM`
: In the event an internal memory allocation fails.

`KM_CORE_STATUS_KEY_ERROR`
: The key cannot be found.

-------------------------------------------------------------------------------

# km_core_state_options_to_json()

## Description

Export the contents of a [km_core_options] array to a JSON formatted document and
place it in the supplied buffer, reporting how much space was used. If null is
passed as the buffer the number of bytes required is returned in `space`. If
there is insufficent space to hold the document the contents of the buffer is
undefined. The returned buffer uses UTF-8 encoding.

## Specification

```c
*/
KMN_API
km_core_status
km_core_state_options_to_json(km_core_state const *state,
                       char *buf,
                       size_t *space);

/*
```
## Parameters

`state`
: An opaque pointer to a state object.

`buf`
: A pointer to the buffer to place the C string containing the JSON
  document into, can be null.

`space`
: A pointer to a size_t variable. This variable must contain the
  number of bytes available in the buffer pointed to by `buf`, unless `buf` is
  null. On return it will hold how many bytes were used.

## Returns

`KM_CORE_STATUS_OK`
: On success.

`KM_CORE_STATUS_INVALID_ARGUMENT`
: If non-optional parameters are null.

`KM_CORE_STATUS_NO_MEM`
: In the event an internal memory allocation fails.

-------------------------------------------------------------------------------

---
filename: keyboards.md
title: Keyboards - Keyman Core API
---

A keyboard is a set of rules and transforms in a Processor specific format for
transforming key events into action items. The keyboard is parsed and loaded by
the processsor and made available in an immutable fashion for use with any number
of state objects.

-------------------------------------------------------------------------------

# km_core_keyboard_attrs struct

## Description

Provides read-only information about a keyboard.

## Specification
```c
*/
typedef struct {
  km_core_cu const * version_string;
  km_core_cu const * id;
  km_core_path_name  folder_path;
  km_core_option_item const * default_options;
} km_core_keyboard_attrs;

/*
```
## Members

`version_string`
: Processor specific version string.

`id`
: Keyman keyboard ID string.

`folder_path`
: Path to the unpacked folder containing the keyboard and associated resources.

`default_options`
: Set of default values for any options included in the keyboard.

-------------------------------------------------------------------------------

# km_core_keyboard_key struct

## Description

Describes a single key and modifier combination that a keyboard handles, for
use by the Platform layer. This is used when the Platform layer must know in
advance which keys are used by a given keyboard.

## Specification

```c */
typedef struct {
  km_core_virtual_key key;
  uint32_t modifier_flag;
} km_core_keyboard_key;

#define KM_CORE_KEYBOARD_KEY_LIST_END { 0, 0 }

/*
```

## Members

`key`
: A virtual key.

`modifier_flag`
: A [km_core_modifier_state] bitmask.

-------------------------------------------------------------------------------

# km_core_keyboard_imx struct

## Description

Describes a single Input Method eXtension library and entry point.

## Specification

```c */
typedef struct {
  km_core_cu const * library_name;
  km_core_cu const * function_name;
  uint32_t imx_id;
} km_core_keyboard_imx;

#define KM_CORE_KEYBOARD_IMX_END { 0, 0, 0 }

/*
```
## Members

`library_name`
: The fully-qualified path and filename of the dynamically loaded library file.

`function_name`
: The entry point for the IMX.

`imx_id`
: unique identifier used to call this function

-------------------------------------------------------------------------------

# km_core_keyboard_load()

## Description

Parse and load keyboard from the supplied path and a pointer to the loaded keyboard
into the out paramter.

## Specification

```c */
KMN_API
km_core_status
km_core_keyboard_load(km_core_path_name kb_path,
                     km_core_keyboard **keyboard);

/*
```

## Parameters

`kb_path`
: On Windows, a UTF-16 string; on other platforms, a C string:
  contains a valid path to the keyboard file.

`keyboard`
: A pointer to result variable: A pointer to the opaque keyboard
  object returned by the Processor. This memory must be freed with a
  call to [km_core_keyboard_dispose].

## Returns

`KM_CORE_STATUS_OK`
: On success.

`KM_CORE_STATUS_NO_MEM`
: In the event an internal memory allocation fails.

`KM_CORE_STATUS_IO_ERROR`
: In the event the keyboard file is unparseable for any reason

`KM_CORE_STATUS_INVALID_ARGUMENT`
: In the event the file doesn't exist or is inaccesible or `keyboard` is null.

`KM_CORE_STATUS_OS_ERROR`
: Bit 31 (high bit) set, bits 0-30 are an OS-specific error code.

-------------------------------------------------------------------------------

# km_core_keyboard_load_from_blob()

## Description

Parse and load keyboard from the supplied blob and a pointer to the loaded keyboard
into the out paramter.

## Specification

```c */
KMN_API
km_core_status km_core_keyboard_load_from_blob(void* blob, size_t blob_size,
                                               km_core_keyboard** keyboard);

/*
```

## Parameters

`blob`
: a byte array containing the content of a KMX/KMX+ file

`blob_size`
: A pointer to a size_t variable with the size of the blob in bytes.

`keyboard`
: A pointer to result variable: A pointer to the opaque keyboard
  object returned by the Processor. This memory must be freed with a
  call to [km_core_keyboard_dispose].

## Returns

`KM_CORE_STATUS_OK`
: On success.

`KM_CORE_STATUS_NO_MEM`
: In the event an internal memory allocation fails.

`KM_CORE_STATUS_IO_ERROR`
: In the event the keyboard blob is unparseable for any reason

`KM_CORE_STATUS_INVALID_ARGUMENT`
: In the event one of the required parameters is null.

-------------------------------------------------------------------------------

# km_core_keyboard_dispose()

## Description

Free the allocated memory belonging to an opaque keyboard object previously
returned by [km_core_keyboard_load].

## Specification

```c */
KMN_API
void
km_core_keyboard_dispose(km_core_keyboard *keyboard);

/*
```
## Parameters
`keyboard`
: A pointer to the opaque keyboard object to be disposed of.

-------------------------------------------------------------------------------

# km_core_keyboard_get_attrs()

## Description

Returns the const internal attributes of the keyboard. This structure is valid
for the lifetime of the opaque keyboard object. Do not modify the returned data.

## Specification

```c */
KMN_API
km_core_status
km_core_keyboard_get_attrs(km_core_keyboard const *keyboard,
                          km_core_keyboard_attrs const **out);

/*
```
## Parameters

`keyboard`
: A pointer to the opaque keyboard object to be queried.

`out`
: A pointer to the result: A pointer to a [km_core_keyboard_attrs] structure.

## Returns

`KM_CORE_STATUS_OK`
: On success.

`KM_CORE_STATUS_INVALID_ARGUMENT`
: If non-optional parameters are null.

-------------------------------------------------------------------------------

# km_core_keyboard_get_key_list()

## Description

Returns the unordered full set of modifier+virtual keys that are handled by the
keyboard. The matching dispose call needs to be called to free the memory.

## Specification

```c */
KMN_API
km_core_status
km_core_keyboard_get_key_list(km_core_keyboard const *keyboard,
                            km_core_keyboard_key **out);

/*
```
## Parameters

`keyboard`
: A pointer to the opaque keyboard object to be queried.

`out`
: A pointer to an array of [km_core_keyboard_key] structures,
  terminated by `KM_CORE_KEYBOARD_KEY_LIST_END`.

## Returns

`KM_CORE_STATUS_OK`
: On success.

`KM_CORE_STATUS_INVALID_ARGUMENT`
: If non-optional parameters are null.

-------------------------------------------------------------------------------

# km_core_keyboard_key_list_dispose()

## Description

Free the allocated memory belonging to a keyboard key list previously
returned by [km_core_keyboard_get_key_list].

## Specification

```c */
KMN_API
void km_core_keyboard_key_list_dispose(km_core_keyboard_key *key_list);

/*
```
## Parameters

`key_list`
: A pointer to the keyboard key list to be disposed of.

-------------------------------------------------------------------------------

# km_core_keyboard_get_imx_list

## Description

Returns the list of IMX libraries and function names that are referenced by
the keyboard. The matching dispose call needs to be called to free the memory.

## Specification

```c */
KMN_API
km_core_status km_core_keyboard_get_imx_list(km_core_keyboard const *keyboard, km_core_keyboard_imx **imx_list);

/*
```
## Parameters

`keyboard`
: A pointer to the keyboard

`imx_list`
: A pointer to a variable that will contain a pointer to the IMX list.

## Returns

`KM_CORE_STATUS_OK`
: On success.

`KM_CORE_STATUS_INVALID_ARGUMENT`
: If non-optional parameters are null.

-------------------------------------------------------------------------------

# km_core_keyboard_imx_list_dispose()

## Description

Disposes of the IMX list.

## Specification

```c */
KMN_API
void km_core_keyboard_imx_list_dispose(km_core_keyboard_imx *imx_list);

/*
```
## Parameters

`imx_list`
: A pointer to the IMX list.

-------------------------------------------------------------------------------

# km_core_state_imx_register_callback()

## Description

Register the IMX callback endpoint for the client.

## Specification

```c */
KMN_API
void km_core_state_imx_register_callback(km_core_state *state, km_core_keyboard_imx_platform imx_callback, void *callback_object);

/*
```
## Parameters

`state`
: A pointer to the opaque state object

`imx_callback`
: pointer to a function that implements the IMX callback

`callback_object`
: An opaque pointer that can be used to pass context information to the callback function,
  usually it is a user-defined data structure.

-------------------------------------------------------------------------------

# km_core_state_imx_deregister_callback()

## Description

De-register IMX callback endpoint for the client.

## Specification

```c */
KMN_API
void km_core_state_imx_deregister_callback(km_core_state *state);

/*
```
## Parameters

`state`
: A pointer to the opaque state object

-------------------------------------------------------------------------------

# km_core_state_create()

## Description

Create a keyboard processor state object, maintaining state for the keyboard in
the environment passed.

## Specification

```c */
KMN_API
km_core_status
km_core_state_create(km_core_keyboard *keyboard,
                    km_core_option_item const *env,
                    km_core_state **out);

/*
```
## Parameters

`keyboard`
: A pointer to the opaque keyboard object this object will hold state for.

`env`
: The array of [km_core_option_item] key/value pairs used to initialise the
  environment, terminated by `KM_CORE_OPTIONS_END`.

`out`
: A pointer to result variable: A pointer to the opaque state object
  returned by the Processor, initalised to maintain state for `keyboard`.
  This must be disposed of by a call to [km_core_state_dispose].

## Returns

`KM_CORE_STATUS_OK`
: On success.

`KM_CORE_STATUS_NO_MEM`
: In the event memory is unavailable to allocate a state object.

`KM_CORE_STATUS_INVALID_ARGUMENT`
: In the event the `keyboard` or `out` pointer are null.

-------------------------------------------------------------------------------

# km_core_state_clone()

## Description

Clone an existing opaque state object.

## Specification

```c */
KMN_API
km_core_status
km_core_state_clone(km_core_state const *state,
                   km_core_state **out);

/*
```
## Parameters

`state`
: A pointer to the opaque state object to be cloned.

`out`
: A pointer to result variable: A pointer to the opaque state object
  returned by the Processor, cloned from the existing object `state`. This
  must be disposed of by a call to [km_core_state_dispose].

## Returns

`KM_CORE_STATUS_OK`
: On success.

`KM_CORE_STATUS_NO_MEM`
: In the event memory is unavailable to allocate a state object.

`KM_CORE_STATUS_INVALID_ARGUMENT`
: In the event the `state` or `out` pointer are null.

-------------------------------------------------------------------------------

# km_core_state_dispose()

## Description

Free the allocated resources belonging to a [km_core_state] object previously
returned by [km_core_state_create] or [km_core_state_clone]. After this all
pointers previously returned by any [km_core_state] family of calls will become
invalid.

## Specification

```c */
KMN_API
void
km_core_state_dispose(km_core_state *state);

/*
```
## Parameters

`state`
: A pointer to the opaque state object to be disposed.

-------------------------------------------------------------------------------

# km_core_debug_context_type enum

As of version 17, the cached context is an internal property of the
state, not exposed to the consumer of the API -- apart from the
Keyman Developer Keyboard Debugger. However, for other debug
purposes, it is helpful to be able to examine the cached context, so
a debug-formatted version of the context is made available with
[km_core_state_context_debug]. This is not intended to be parsed for
reading the context for other purposes, and the format may change.

The three context types are: cached, intermediate, and app.

## Specification

```c */
typedef enum {
  KM_CORE_DEBUG_CONTEXT_CACHED        = 0,
  KM_CORE_DEBUG_CONTEXT_INTERMEDIATE  = 1,
  KM_CORE_DEBUG_CONTEXT_APP           = 2
} km_core_debug_context_type;

/*
```
## Values

`KM_CORE_DEBUG_CONTEXT_CACHED`
: the internal context used by Core, which may be normalized
  and may contain markers. This is set via
  [km_core_state_context_set_if_needed], and will be modified
  during keystroke event processing.

`KM_CORE_DEBUG_CONTEXT_INTERMEDIATE`
: internal context used by IMX, only valid during
  keystroke event processing.

`KM_CORE_DEBUG_CONTEXT_APP`
: an exact copy of the current context passed in to
  [km_core_state_context_set_if_needed], which is used to verify
  the precise text manipulations required when emitted changes.
  This input context is in "NFU" -- normalization form unknown,
  and may be mixed normalization so may require fixups when
  it is manipulated by keyboard processors that support
  normalization, such as the LDML keyboard processor.

-------------------------------------------------------------------------------

# km_core_state_context_debug()

## Description

Returns a debug formatted string of the context from the state.

## Specification

```c */
KMN_API
km_core_cu *
km_core_state_context_debug(km_core_state *state, km_core_debug_context_type context_type);

/*
```
## Parameters

`state`
: A pointer to the opaque state object to be queried.

`context_type`
: The type of context to retrieve from the state.

## Returns

A pointer to a [km_core_cu] UTF-16 string. Must be disposed of by a call
to [km_core_cu_dispose].

-------------------------------------------------------------------------------

# km_core_cu_dispose()

## Description

Free the allocated memory belonging to a [km_core_cu] array previously
returned by [km_core_state_context_debug]. May be `nullptr`.

## Specification

```c */
KMN_API
void
km_core_cu_dispose(km_core_cu *cp);

/*
```
## Parameters

`cp`
: A pointer to the start of the [km_core_cu] array to be disposed of.

-------------------------------------------------------------------------------

# km_core_state_to_json()

## Description

Export the internal state of a [km_core_state] object to a JSON format document
and place it in the supplied buffer, reporting how much space was used. If null
is passed as the buffer the number of bytes required is returned. If there is
insufficent space to hold the document, the contents of the buffer is undefined.
The encoding of the returned data is UTF-8.

__WARNING__: The structure and format of the JSON document while independently
versioned is not part of this API and is intended solely for use in diagnostics
or by development and debugging tools which are aware of processor
implementation details.

## Specification

```c */
KMN_API
km_core_status
km_core_state_to_json(km_core_state const *state,
                     char *buf,
                     size_t *space);

/*
```
## Parameters

`state`
: An pointer to an opaque state object.

`buf`
: A pointer to the buffer to place the C string containing the JSON
  document into. May be null.

`space`
: A pointer to a size_t variable. This variable must contain the
  number of bytes available in the buffer pointed to by `buf`, unless `buf` is
  null. On return it will hold how many bytes were used.

## Returns

`KM_CORE_STATUS_OK`
: On success.

`KM_CORE_STATUS_NO_MEM`
: In the event an internal memory allocation fails.

-------------------------------------------------------------------------------

---
filename: processor.md
title: Processor - Keyman Core API
---

# km_core_event_flags enum

## Description

Bit flags to be used with the `event_flags` parameter of [km_core_process_event]

## Specification

```c */
enum km_core_event_flags {
  KM_CORE_EVENT_FLAG_DEFAULT = 0,
  KM_CORE_EVENT_FLAG_TOUCH = 1,
};

/*
```
## Values

`KM_CORE_EVENT_FLAG_DEFAULT`
: default value: hardware

`KM_CORE_EVENT_FLAG_TOUCH`
: set if the event is touch, otherwise hardware

-------------------------------------------------------------------------------

# km_core_process_event()

## Description

Run the keyboard on an opaque state object with the provided virtual key and modifer
key state. Updates the state object as appropriate and fills out its internal set
of actions, which can be retrieved with [km_core_state_get_actions].

The state's actions will be cleared at the start of this call; options and context in
the state may also be modified.

## Specification

```c */
KMN_API
km_core_status
km_core_process_event(km_core_state *state,
                     km_core_virtual_key vk,
                     uint16_t modifier_state,
                     uint8_t is_key_down,
                     uint16_t event_flags);

/*
```
## Parameters

`state`
: A pointer to the opaque state object.

`vk`
: A virtual key to be processed.

`modifier_state`
: The combinations of modifier keys set at the time key `vk` was pressed, bitmask
  from the [km_core_modifier_state] enum.

`event_flags`
: Event level flags, see [km_core_event_flags]

## Returns

`KM_CORE_STATUS_OK`
: On success.

`KM_CORE_STATUS_NO_MEM`
: In the event memory is unavailable to allocate internal buffers.

`KM_CORE_STATUS_INVALID_ARGUMENT`
: In the event the `state` pointer is null or an invalid virtual key or modifier
  state is passed.

-------------------------------------------------------------------------------

# km_core_event()

## Description

Tell the keyboard processor that an external event has occurred, such as a keyboard
being activated through the language switching UI.

The keyboard processor may generate actions which should be processed by the
consumer of the API.

The actions will be cleared at the start of this call; options and context in
the state may also be modified.

## Specification

```c */
KMN_API
km_core_status
km_core_event(
  km_core_state *state,
  uint32_t event,
  void* data
);

/*
```
## Parameters

`state`
: A pointer to the opaque state object.

`event`
: The event to be processed, from [km_core_event_code] enumeration

`data`
: Additional event-specific data. Currently unused, must be nullptr.

## Returns

`KM_CORE_STATUS_OK`
: On success.

`KM_CORE_STATUS_NO_MEM`
: In the event memory is unavailable to allocate internal buffers.

`KM_CORE_STATUS_INVALID_ARGUMENT`
: In the event the `state` pointer is null or an invalid event or data is passed.

-------------------------------------------------------------------------------

# km_core_event_code enum

## Description

Possible events to be passed into Keyman Core from the Platform layer.

## Specification

```c */
enum km_core_event_code {
  KM_CORE_EVENT_KEYBOARD_ACTIVATED = 1,
  //future: KM_CORE_EVENT_KEYBOARD_DEACTIVATED = 2,
};

/*
```
## Values

`KM_CORE_EVENT_KEYBOARD_ACTIVATED`
: A keyboard has been activated by the user. The processor may use this
  event, for example, to switch caps lock state or provide other UX.

$EOF
*/

#if defined(__cplusplus)
} // extern "C"
#endif
/*
```
*/
