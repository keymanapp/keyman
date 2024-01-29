/*
  Copyright:    © 2018 SIL International.
  Description:  Cross platform API C/C++ declarations for libkeymancore keyboard
                processor.
  Create Date:  2 Oct 2018
  Authors:      Tim Eves (TSE)
  History:      18 Oct 2018 - TSE - Finialised verion of API.
                 6 Oct 2018 - TSE - Move into keyman folder.

*/
#pragma once
/*
# Keyman Keyboard Processor API

## Requirements
1. Cross platform.
2. Cross language.
3. Facilitate stateless operation of the Engine.
4. Keyboard format agnostic -- support both KMN and future LDML based keyboards.
5. Support querying Engine attributes.
6. Support querying Keyboard attributes.
7. Idempotent


## Design decisions in support of requirements:
- Use C or C99 types and calling convention for the interface, it has the
  broadest language FFI support. [1,2]
- Have client (Platform layer) code load keyboards, manage & pass state. [3,4,7]
- Provide query calls to return static attributes data for keyboards and
  engine [5,6]
- Provide get/set calls for client accessible keyboard state information [3,4]


## Glossary
- __Platform layer:__
The code that consumes the Keyman Keyboard Processor API, and provides the
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

## API
### Namespace
All calls, types and enums are prefixed with the namespace identifier `km_core_`

### API idioms
Almost all calls marshalling variable length aggregate data in or out of an API
object take the form:
> km_core_status *fn_name*(object_ref, buffer_ptr, size_ptr)

where the buffer is nullable and all other arguments are required (will result
in an `KM_CORE_STATUS_INVALID_ARGUMENT` status being returned if nulled). When
`buffer` is `nullptr` or `0` the function will place the size of the required
buffer in the variable pointed to by `size_ptr`.

Calls which result in the allocation of resources, regardless of resulting
ownership, are of the form:
> km_core_status *fn_name*(object_ref, out_ptr)

where `out_ptr` is a valid pointer to a caller allocated variable to hold the
resulting ouput. This is often a reference to a created object. All arguments
are required (will result in an `KM_CORE_STATUS_INVALID_ARGUMENT` status being
returned if nulled).

For accessors to fixed size attributes of an object these will take the form:
> attr_value __fn_name__(object_ref)

`object_ref` is required to be valid and will result in a nonsense value being
returned if `nullptr` or `0`.

All dispose calls are designed to accept null as a valid value and will do
nothing in that event.
```c
*/
#include <stdint.h>
#include <stdlib.h>
#include <keyman/keyman_core_api_bits.h>
#include <keyman/keyman_core_api_vkeys.h>
#include <keyman/keyman_core_api_version.h>

#if defined(__cplusplus)
extern "C"
{
#endif
// Basic types
//
typedef uint16_t    km_core_virtual_key; // A virtual key code.
typedef uint32_t    km_core_status;      // Status return code.

// Opaque object types.
//
typedef struct km_core_keyboard    km_core_keyboard;
typedef struct km_core_state       km_core_state;
typedef struct km_core_options     km_core_options;

// Forward declarations
//
typedef struct km_core_option_item  km_core_option_item;

// Callback function used to to access Input Method eXtension library functions
// from Keyman Core
//
typedef uint8_t (*km_core_keyboard_imx_platform)(km_core_state*, uint32_t, void*);

/*```
### Error Handling
Error handling and success failure notification are communicated through a
general mechanism similar to COM’s `HRESULT` scheme (unlike COM, any non-zero
value is an error). Any functions that can fail will always return a status
value and all results are returned via outparams passed to the function.
```c
*/
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
The final status code KM_CORE_STATUS_OS_ERROR is intended to allow encapsulating
a platform error code; the remaining 31 low bits are the error code returned by
the OS for cases where the failure mode is platform specific. For HRESULT codes
this only permits failure codes to be passed.
*/

/*
```
### Action Items
These provide the results of processing a key event to the Platform layer and
should be processed by the Platform layer to issue commands to the os text
services framework to transform the text store in the Client Application, among
other actions.
```c
*/

typedef struct {
  uint8_t expected_type;     // km_core_backspace_type
  uintptr_t expected_value;  // used mainly in unit tests
} km_core_backspace_item;

enum km_core_backspace_type {
  KM_CORE_BT_UNKNOWN    = 0,  // Used at beginning of context; user-initiated backspace
  KM_CORE_BT_CHAR       = 1,  // Deleting a character prior to insertion point
  KM_CORE_BT_MARKER     = 2,  // Deleting a marker prior to insertion point
  KM_CORE_BT_MAX_TYPE_ID
};

typedef struct {
  uint8_t   type;
  uint8_t   _reserved[sizeof(void*)-sizeof(uint8_t)];
  union {
    uint32_t               marker;          // MARKER type
    km_core_option_item    const * option;  // OPT types
    km_core_usv            character;       // CHAR type
    uint8_t                capsLock;        // CAPSLOCK type, 1 to turn on, 0 to turn off; re name see #9833
    km_core_backspace_item backspace;       // BACKSPACE type
  };
} km_core_action_item;

enum km_core_action_type {
  KM_CORE_IT_END         = 0,  // Marks end of action items list.
  KM_CORE_IT_CHAR        = 1,  // A Unicode character has been generated.
  KM_CORE_IT_MARKER      = 2,  // Correlates to kmn's "deadkey" markers.
  KM_CORE_IT_ALERT       = 3,  // The keyboard has triggered a alert/beep/bell.
  KM_CORE_IT_BACK        = 4,  // Delete the codepoint preceding the insertion point.
  KM_CORE_IT_PERSIST_OPT = 5,  // The indicated option needs to be stored.
  KM_CORE_IT_EMIT_KEYSTROKE = 6,  // Emit the current keystroke to the application
  KM_CORE_IT_INVALIDATE_CONTEXT = 7,
          // The processor requests that the context buffer be cleared;
          // for applications where context is cached, this clears the context;
          // for applications where context is read from the focused text store,
          // the context is just re-read and markers flushed.
  KM_CORE_IT_CAPSLOCK    = 8,  // Enable or disable capsLock
  KM_CORE_IT_MAX_TYPE_ID
};

/*
```
### Actions
This structure provides the results of processing a key event to the Platform layer and
should be processed by the Platform layer to issue commands to the os text
services framework to transform the text store in the Client Application, among
other actions.

This API replaces the Action items APIs, which is now deprecated and will be
removed in the future.
```c
*/

typedef enum { KM_CORE_FALSE = 0, KM_CORE_TRUE = 1 } km_core_bool;
typedef enum { KM_CORE_CAPS_UNCHANGED = -1, KM_CORE_CAPS_OFF = 0, KM_CORE_CAPS_ON = 1 } km_core_caps_state;

typedef struct {
  // number of codepoints (not codeunits!) to delete from app context.
  unsigned int code_points_to_delete;

  // null-term string of characters to insert into document
  const km_core_usv* output;

  // list of options to persist, terminated with KM_CORE_OPTIONS_END
  km_core_option_item * persist_options;

  // issue a beep, 0 = no, 1 = yes
  km_core_bool do_alert;

  // emit the (unmodified) input keystroke to the application, 0 = no, 1 = yes
  km_core_bool emit_keystroke;

  // -1=unchanged, 0=off, 1=on
  km_core_caps_state new_caps_lock_state;

  // reference copy of actual UTF32 codepoints deleted from rhs of context
  // exactly code_points_to_delete in length (plus null terminator). Used to
  // determine encoding conversion differences when deleting; only set when
  // using km_core_state_get_actions, otherwise nullptr.
  const km_core_usv* deleted_context;
} km_core_actions;

/*
```
### `km_core_state_get_actions`
##### Description:
Returns a pointer to an actions object which details all the actions
that the Platform layer must take after a keystroke. The `code_points_to_delete`
action must be performed before the `output` action, but the other
actions may be performed in any order.
##### Return:
A pointer to a `km_core_actions` object, which must be freed with
`km_core_actions_dispose`.
##### Parameters:
- __state__: An opaque pointer to a state object.

```c
*/
KMN_API
km_core_actions const *
km_core_state_get_actions(
  km_core_state const *state
);

/*
```
### `km_core_actions_dispose`
##### Description:
Free the allocated memory belonging to an actions object previously
returned by `km_core_state_get_actions`.
##### Parameters:
- __actions__: A pointer to the actions object to be disposed of.

```c
*/
KMN_API
km_core_status
km_core_actions_dispose(
  km_core_actions const * actions
);

/*
```
### `km_core_context_status`
##### Description:
Return values for `km_core_state_context_set_if_needed`.

```c
*/

typedef enum {
  KM_CORE_CONTEXT_STATUS_UNCHANGED = 0,  // Cached context change was not needed
  KM_CORE_CONTEXT_STATUS_UPDATED = 1,    // Cached context was set to application context
  KM_CORE_CONTEXT_STATUS_CLEARED = 2,    // Application context was invalid, context was cleared
  KM_CORE_CONTEXT_STATUS_ERROR = 3,      // Internal error
  KM_CORE_CONTEXT_STATUS_INVALID_ARGUMENT = 4, // Invalid arguments
} km_core_context_status;

/*
```
### `km_core_state_context_set_if_needed`
##### Description:
Sets the internal cached context for the state object, to the passed-in
application context string, if it differs from the codepoints in the
cached context. For the purposes of comparison, (1) cached markers are
ignored, (2) if the cached context is shorter than the application
context, it is considered identical, but (3) if the cached context is
longer, then it is considered different.

If a difference is found, then the cached context will be set to the
application context, and thus any cached markers will be cleared.

`km_core_state_context_set_if_needed` and `km_core_state_context_clear`
will replace most uses of the existing Core context APIs.

##### Parameters:
- __state__: An opaque pointer to a state object.
- __application_context__: A pointer to an null-terminated array of
    utf16 encoded data representing the current context from the
    application.
##### Return status:
- `KM_CORE_CONTEXT_STATUS_UNCHANGED`: Cached context change was not needed
- `KM_CORE_CONTEXT_STATUS_UPDATED`: Cached context was set to application
  context
- `KM_CORE_CONTEXT_STATUS_CLEARED`: Application context was invalid, perhaps
  had unpaired surrogates, and so cached context was cleared instead
- `KM_CORE_CONTEXT_STATUS_ERROR`: Internal error
- `KM_CORE_CONTEXT_STATUS_INVALID_ARGUMENT`: One or more parameters was null

```c
*/

KMN_API
km_core_context_status
km_core_state_context_set_if_needed(
  km_core_state *state,
  km_core_cp const *application_context
);

/*
```
### `km_core_state_context_clear`
##### Description:
Clears the internal cached context for the state. This is the same as
`km_core_context_clear(km_core_state_context(&state))`.

`km_core_state_context_set_if_needed` and `km_core_state_context_clear`
will replace most uses of the existing Core context APIs.

##### Parameters:
- __state__: An opaque pointer to a state object.
##### Return status:
- `KM_CORE_STATUS_OK`: On success.
- `KM_CORE_STATUS_INVALID_ARGUMENT`: If any parameters are null.

```c
*/
KMN_API
km_core_status
km_core_state_context_clear(
  km_core_state *state
);

/*
```
### Options
A state’s default options are set from the keyboard at creation time and the
environment. The Platform layer is then is expected to apply any persisted
options it is maintaining.  Options are passed into and out of API functions as
simple C arrays of `km_core_option_item` terminated with a `KM_CORE_OPTIONS_END`
sentinel value. A state's options are exposed and manipulatable via the
`km_core_options` API. All option values are of type C string.

During processing when the Platform layer finds a PERSIST action type it should
store the updated option in the appropriate place, based on its scope.
For RESET the processor will apply the pristine value from the original scope,
the Platform layer should update that only if it manages a previously persisted
value.
```c
*/

enum km_core_option_scope {
  KM_CORE_OPT_UNKNOWN      = 0,
  KM_CORE_OPT_KEYBOARD     = 1,
  KM_CORE_OPT_ENVIRONMENT  = 2,
  KM_CORE_OPT_MAX_SCOPES
};

struct km_core_option_item {
  km_core_cp const *   key;
  km_core_cp const *   value;
  uint8_t             scope;  // Scope which an option belongs to.
};

#define KM_CORE_OPTIONS_END { 0, 0, 0 }


/*
```
### `km_core_options_list_size`
##### Description:
Return the length of a terminated `km_core_option_item` array (options
list).
##### Return:
The number of items in the list, not including terminating item,
or 0 if `opts` is null.
##### Parameters:
- __opts__: A pointer to a `KM_CORE_OPTIONS_END` terminated array of
    `km_core_option_item` values.

```c
*/
KMN_API
size_t
km_core_options_list_size(km_core_option_item const *opts);

/*
```
### `km_core_state_option_lookup`
##### Description:
Lookup an option based on its key, in an options list.
##### Return status:
- `KM_CORE_STATUS_OK`: On success.
- `KM_CORE_STATUS_INVALID_ARGUMENT`: If non-optional parameters are null, or
  if the scope is invalid.
- `KM_CORE_STATUS_KEY_ERROR`: The key cannot be found.
##### Parameters:
- __state__: An opaque pointer to a state object.
- __scope__: Which key-value store to interrogate.
- __key__: A UTF-16 string that matches the key in the target `km_core_option_item`.
- __value__: A pointer to the result variable:
  A pointer to a UTF-16 string value owned by the state or keyboard object at
  the time of the call. This pointer is only valid *until* the next call to any
  function on this API and should be used immediately.
```c
*/
KMN_API
km_core_status
km_core_state_option_lookup(km_core_state const *state,
                      uint8_t scope,
                      km_core_cp const *key,
                      km_core_cp const **value);

/*
```
### `km_core_state_options_update`
##### Description:
Adds or updates one or more options from a list of `km_core_option_item`s.
##### Return status:
- `KM_CORE_STATUS_OK`: On success.
- `KM_CORE_STATUS_INVALID_ARGUMENT`: If non-optional parameters are null.
- `KM_CORE_STATUS_NO_MEM`: In the event an internal memory allocation fails.
- `KM_CORE_STATUS_KEY_ERROR`: The key cannot be found.
##### Parameters:
- __state__: An opaque pointer to a state object.
- __new_opts__: An array of `km_core_option_item` objects to update or add. Must be
    terminated with `KM_CORE_OPTIONS_END`.

```c
*/
KMN_API
km_core_status
km_core_state_options_update(km_core_state *state,
                      km_core_option_item const *new_opts);

/*
```
### `km_core_state_options_to_json`
##### Description:
Export the contents of a `km_core_options` array to a JSON formatted document and
place it in the supplied buffer, reporting how much space was used. If null is
passed as the buffer the number of bytes required is returned in `space`. If
there is insufficent space to hold the document the contents of the buffer is
undefined. The returned buffer uses UTF-8 encoding.
##### Return status:
- `KM_CORE_STATUS_OK`: On success.
- `KM_CORE_STATUS_INVALID_ARGUMENT`: If non-optional parameters are null.
- `KM_CORE_STATUS_NO_MEM`: In the event an internal memory allocation fails.
##### Parameters:
- __state__: An opaque pointer to a state object.
- __buf__: A pointer to the buffer to place the C string containing the JSON
document into, can be null.
- __space__: A pointer to a size_t variable. This variable must contain the
number of bytes available in the buffer pointed to by `buf`, unless `buf` is
null. On return it will hold how many bytes were used.

```c
*/
KMN_API
km_core_status
km_core_state_options_to_json(km_core_state const *state,
                       char *buf,
                       size_t *space);


/*
```
### Keyboards
A keyboard is a set of rules and transforms in a Processor specific format for
transforming key events into action items. The keyboard is parsed and loaded by
the processsor and made available in an immutable fashion for use with any number
of state objects.
```c
*/
typedef struct {
  km_core_cp const * version_string;   // Processor specific version string.
  km_core_cp const * id;               // Keyman keyboard ID string.
  km_core_path_name  folder_path;      // Path to the unpacked folder containing
                                      // the keyboard and associated resources.
  km_core_option_item const * default_options;
} km_core_keyboard_attrs;

typedef struct {
  km_core_virtual_key key;
  uint32_t modifier_flag;
} km_core_keyboard_key;

#define KM_CORE_KEYBOARD_KEY_LIST_END { 0, 0 }

typedef struct {
  km_core_cp const * library_name;
  km_core_cp const * function_name;
  uint32_t imx_id; // unique identifier used to call this function
} km_core_keyboard_imx;

#define KM_CORE_KEYBOARD_IMX_END { 0, 0, 0 }

/*
```
### `km_core_keyboard_load`
##### Description:
Parse and load keyboard from the supplied path and a pointer to the loaded keyboard
into the out paramter.
##### Return status:
- `KM_CORE_STATUS_OK`: On success.
- `KM_CORE_STATUS_NO_MEM`: In the event an internal memory allocation fails.
- `KM_CORE_STATUS_IO_ERROR`:
    In the event the keyboard file is unparseable for any reason
- `KM_CORE_STATUS_INVALID_ARGUMENT`:
    In the event the file doesn't exist or is inaccesible or `keyboard` is null.
- `KM_CORE_STATUS_OS_ERROR`: Bit 31 (high bit) set, bits 0-30 are an OS-specific
    error code.
##### Parameters:
- __kb_path__: On Windows, a UTF-16 string; on other platforms, a C string:
    contains a valid path to the keyboard file.
- __keyboard__: A pointer to result variable:
    A pointer to the opaque keyboard object returned by the Processor. This
    memory must be freed with a call to `km_core_keyboard_dispose`.

```c
*/
KMN_API
km_core_status
km_core_keyboard_load(km_core_path_name kb_path,
                     km_core_keyboard **keyboard);

/*
```
### `km_core_keyboard_dispose`
##### Description:
Free the allocated memory belonging to an opaque keyboard object previously
returned by `km_core_keyboard_load`.
##### Parameters:
- __keyboard__: A pointer to the opaque keyboard object to be
    disposed of.

```c
*/
KMN_API
void
km_core_keyboard_dispose(km_core_keyboard *keyboard);

/*
```
### `km_core_keyboard_get_attrs`
##### Description:
Returns the const internal attributes of the keyboard. This structure is valid
for the lifetime of the opaque keyboard object. Do not modify the returned data.
##### Return status:
- `KM_CORE_STATUS_OK`: On success.
- `KM_CORE_STATUS_INVALID_ARGUMENT`: If non-optional parameters are null.
##### Parameters:
- __keyboard__: A pointer to the opaque keyboard object to be queried.
- __out__: A pointer to the result:
    A pointer to a `km_core_keyboard_attrs` structure.

```c
*/
KMN_API
km_core_status
km_core_keyboard_get_attrs(km_core_keyboard const *keyboard,
                          km_core_keyboard_attrs const **out);

/*
```
### `km_core_keyboard_get_key_list`
##### Description:
Returns the unordered full set of modifier+virtual keys that are handled by the
keyboard. The matching dispose call needs to be called to free the memory.
##### Return status:
- `KM_CORE_STATUS_OK`: On success.
- `KM_CORE_STATUS_INVALID_ARGUMENT`: If non-optional parameters are null.
##### Parameters:
- __keyboard__: A pointer to the opaque keyboard object to be queried.
- __out__: A pointer to an array of `km_core_keyboard_key` structures,
           terminated by `KM_CORE_KEYBOARD_KEY_LIST_END`.

```c
*/
KMN_API
km_core_status
km_core_keyboard_get_key_list(km_core_keyboard const *keyboard,
                            km_core_keyboard_key **out);


/**
```
### `km_core_keyboard_key_list_dispose`
##### Description:
Free the allocated memory belonging to a keyboard key list previously
returned by `km_core_keyboard_get_key_list`.
##### Parameters:
- __key_list__: A pointer to the keyboard key list to be
    disposed of.

```c
*/
KMN_API
void km_core_keyboard_key_list_dispose(km_core_keyboard_key *key_list);

/**
 * km_core_keyboard_get_imx_list:
 *
 * Returns: the list of IMX libraries and function names that are referenced by
 * the keyboard.The matching dispose call needs to be called to free the memory.
 */
KMN_API
km_core_status km_core_keyboard_get_imx_list(km_core_keyboard const *keyboard, km_core_keyboard_imx **imx_list);

/**
 * km_core_keyboard_imx_list_dispose:
 *
 * Disposes of the IMX list
 *
 * Returns: --
 */
KMN_API
void km_core_keyboard_imx_list_dispose(km_core_keyboard_imx *imx_list);

/**
 * km_core_state_imx_register_callback:
 *
 * Register the IMX callback endpoint for the client.
 *
 * Returns: --
 */
KMN_API
void km_core_state_imx_register_callback(km_core_state *state, km_core_keyboard_imx_platform imx_callback, void *callback_object);

/**
 *  km_core_state_imx_deregister_callback:
 *
 * De-register IMX callback endpoint for the client.
 *
 * Returns: --
 */
KMN_API
void km_core_state_imx_deregister_callback(km_core_state *state);

/*
```
### State
A State object maintains all per keyboard related state including context
and dynamic options ("option stores" in kmn format).

```c
*/

/*
```
### `km_core_state_create`
##### Description:
Create a keyboard processor state object, maintaining state for the keyboard in
the environment passed.
##### Return status:
- `KM_CORE_STATUS_OK`: On success.
- `KM_CORE_STATUS_NO_MEM`:
  In the event memory is unavailable to allocate a state object.
- `KM_CORE_STATUS_INVALID_ARGUMENT`:
  In the event the `keyboard` or `out` pointer are null.
##### Parameters:
- __keyboard__:
A pointer to the opaque keyboard object this object will hold state for.
- __env__:
The array of `km_core_option_item` key/value pairs used to initialise the
environment, terminated by `KM_CORE_OPTIONS_END`.
- __out__:
A pointer to result variable: A pointer to the opaque state object
returned by the Processor, initalised to maintain state for `keyboard`.
This must be disposed of by a call to `km_core_state_dispose`.

```c
*/
KMN_API
km_core_status
km_core_state_create(km_core_keyboard *keyboard,
                    km_core_option_item const *env,
                    km_core_state **out);

/*
```
### `km_core_state_clone`
##### Description:
Clone an existing opaque state object.
##### Return status:
- `KM_CORE_STATUS_OK`: On success.
- `KM_CORE_STATUS_NO_MEM`:
In the event memory is unavailable to allocate a state object.
- `KM_CORE_STATUS_INVALID_ARGUMENT`:
In the event the `state` or `out` pointer are null.
##### Parameters:
- __state__:
A pointer to the opaque statea object to be cloned.
- __out__:
A pointer to result variable: A pointer to the opaque state object
returned by the Processor, cloned from the existing object `state`. This
must be disposed of by a call to `km_core_state_dispose`.

```c
*/
KMN_API
km_core_status
km_core_state_clone(km_core_state const *state,
                   km_core_state **out);

/*
```
### `km_core_state_dispose`
##### Description:
Free the allocated resources belonging to a `km_core_state` object previously
returned by `km_core_state_create` or `km_core_state_clone`. After this all
pointers previously returned by any km_core_state family of calls will become
invalid.
##### Parameters:
- __state__: A pointer to the opaque state object to be disposed.

```c
*/
KMN_API
void
km_core_state_dispose(km_core_state *state);

/*
```
### Context Debug Reporting
As of version 17, the cached context is an internal property of the
state, not exposed to the consumer of the API -- apart from the
Keyman Developer Keyboard Debugger. However, for other debug
purposes, it is helpful to be able to examine the cached context, so
a debug-formatted version of the context is made available with
`km_core_state_context_debug`. This is not intended to be parsed for
reading the context for other purposes, and the format may change.

The three context types are:
* cached: the internal context used by Core, which may be normalized
       and may contain markers. This is set via
       km_core_state_context_set_if_needed, and will be modified
       during keystroke event processing.
* intermediate: internal context used by IMX, only valid during
       keystroke event processing.
* app: an exact copy of the current context passed in to
       km_core_state_context_set_if_needed, which is used to verify
       the precise text manipulations required when emitted changes.
       This input context is in "NFU" -- normalization form unknown,
       and may be mixed normalization so may require fixups when
       it is manipulated by keyboard processors that support
       normalization, such as the LDML keyboard processor.
```c
*/
typedef enum {
  KM_CORE_DEBUG_CONTEXT_CACHED        = 0,
  KM_CORE_DEBUG_CONTEXT_INTERMEDIATE  = 1,
  KM_CORE_DEBUG_CONTEXT_APP           = 2
} km_core_debug_context_type;

/*
```
### `km_core_state_context_debug`
##### Description:
Returns a debug formatted string of the context from the state.
##### Return:
A pointer to a km_core_cp UTF-16 string. Must be disposed of by a call
to `km_core_cp_dispose`.
##### Parameters:
- __state__: A pointer to the opaque state object to be queried.
- __context_type__: the type of context to retrieve from the state

```c
*/
KMN_API
km_core_cp *
km_core_state_context_debug(km_core_state *state, km_core_debug_context_type context_type);

/*
```
### `km_core_cp_dispose`
##### Description:
Free the allocated memory belonging to a `km_core_cp` array previously
returned by `km_core_state_context_debug`. May be `nullptr`.
##### Parameters:
- __cp__: A pointer to the start of the `km_core_cp` array
    to be disposed of.

```c
*/
KMN_API
void
km_core_cp_dispose(km_core_cp *cp);

/*
```
### `km_core_state_action_items`
##### Description:
Get the list of action items generated by the last call to
`km_core_process_event`.
##### Return:
A pointer to a `km_core_action_item` list, of `*num_items` in length. This data
becomes invalid when the state object is destroyed, or after a call to
`km_core_process_event`. Do not modify the contents of this data. The returned
array is terminated with a `KM_CORE_IT_END` entry.
##### Parameters:
- __state__: A pointer to the opaque `km_core_state` object to be queried.
- __num_items__:
A pointer to a result variable: The number of items in the action item list
including the `KM_CORE_IT_END` terminator. May be null if not that
information is required.

```c
*/
KMN_API
km_core_action_item const *
km_core_state_action_items(km_core_state const *state,
                          size_t *num_items);

/*
```
### `km_core_state_queue_action_items`
##### Description:
Queue actions for the current keyboard processor state; normally
used in IMX callbacks called during `km_core_process_event`.
##### Return:
- `KM_CORE_STATUS_OK`: On success.
- `KM_CORE_STATUS_INVALID_ARGUMENT`:
In the event the `state` or `action_items` pointer are null.
##### Parameters:
- __state__:        A pointer to the opaque `km_core_state` object to be queried.
- __action_items__: The action items to be added to the core
                    queue. Must be terminated with a `KM_CORE_IT_END` entry.

```c
*/
KMN_API
km_core_status
km_core_state_queue_action_items(km_core_state *state,
                         km_core_action_item const *action_items);

/*
```
### `km_kpb_state_to_json`
##### Description:
Export the internal state of a `km_core_state` object to a JSON format document
and place it in the supplied buffer, reporting how much space was used. If null
is passed as the buffer the number of bytes required is returned. If there is
insufficent space to hold the document, the contents of the buffer is undefined.
The encoding of the returned data is UTF-8.

__WARNING__: The structure and format of the JSON document while independently
versioned is not part of this API and is intended solely for use in diagnostics
or by development and debugging tools which are aware of processor
implementation details.
##### Return status:
- `KM_CORE_STATUS_OK`: On success.
- `KM_CORE_STATUS_NO_MEM`: In the event an internal memory allocation fails.
##### Parameters:
- __state__: An pointer to an opaque state object.
- __buf__: A pointer to the buffer to place the C string containing the JSON
document into. May be null.
- __space__: A pointer to a size_t variable. This variable must contain the
number of bytes available in the buffer pointed to by `buf`, unless `buf` is
null. On return it will hold how many bytes were used.

```c
*/
KMN_API
km_core_status
km_core_state_to_json(km_core_state const *state,
                     char *buf,
                     size_t *space);

/*
```
### Processor
```c
*/
typedef struct {
  size_t      max_context;  // Maximum context size supported by processor.
  uint16_t    current;      // Current API number supported.
  uint16_t    revision;     // Implementation number of current API.
  uint16_t    age;          // current - age == Oldest API number supported.
  uint16_t    technology;   // A bit field specifiying which Keyboard
                            //  technologies the engine supports.
  char const *vendor;       // Implementor of the processor.
} km_core_attr;

enum km_core_tech_value {
  KM_CORE_TECH_UNSPECIFIED = 0,
  KM_CORE_TECH_MOCK        = 1 << 0,
  KM_CORE_TECH_KMX         = 1 << 1,
  KM_CORE_TECH_LDML        = 1 << 2
};

/**
 * km_core_event_flags:
 *
 * Bit flags to be used with the event_flags parameter of km_core_process_event
 */
enum km_core_event_flags {
  KM_CORE_EVENT_FLAG_DEFAULT = 0, // default value: hardware
  KM_CORE_EVENT_FLAG_TOUCH = 1, // set if the event is touch, otherwise hardware
};

/*
```
### `km_core_get_engine_attrs`
##### Description:
Get access processors attributes describing version and technology implemented.
##### Return:
A pointer to a `km_core_attr` structure. Do not modify the contents of this
structure.
##### Parameters:
- __state__: An opaque pointer to an `km_core_state`.
```c
*/
KMN_API
km_core_attr const *
km_core_get_engine_attrs(km_core_state const *state);

/*
```
### `km_core_process_event`
##### Description:
Run the keyboard on an opaque state object with the provided virtual key and modifer
key state. Updates the state object as appropriate and fills out its action list.

The action list will be cleared at the start of this call; options and context in
the state may also be modified.
##### Return status:
- `KM_CORE_STATUS_OK`: On success.
- `KM_CORE_STATUS_NO_MEM`:
In the event memory is unavailable to allocate internal buffers.
- `KM_CORE_STATUS_INVALID_ARGUMENT`:
In the event the `state` pointer is null or an invalid virtual key or modifier
state is passed.

##### Parameters:
- __state__: A pointer to the opaque state object.
- __vk__: A virtual key to be processed.
- __modifier_state__:
The combinations of modifier keys set at the time key `vk` was pressed, bitmask
from the `km_core_modifier_state` enum.
- __event_flags__: Event level flags, see km_core_event_flags

```c
*/
KMN_API
km_core_status
km_core_process_event(km_core_state *state,
                     km_core_virtual_key vk,
                     uint16_t modifier_state,
                     uint8_t is_key_down,
                     uint16_t event_flags);

/*
```
### `km_core_process_queued_actions`
##### Description:
Process the keyboard processors queued actions for the opaque state object.
Updates the state object as appropriate and fills out its action list.
The client can add actions externally via the `km_core_state_queue_action_items` and
then request the processing of the actions with this method.

The state action list will be cleared at the start of this call; options and context in
the state may also be modified.
##### Return status:
- `KM_CORE_STATUS_OK`: On success.
- `KM_CORE_STATUS_NO_MEM`:
In the event memory is unavailable to allocate internal buffers.
- `KM_CORE_STATUS_INVALID_ARGUMENT`:
In the event the `state` pointer is null

##### Parameters:
- __state__: A pointer to the opaque state object.

```c
*/
KMN_API
km_core_status
km_core_process_queued_actions(km_core_state *state);

/*
```
### `km_core_event`
##### Description:
Tell the keyboard processor that an external event has occurred, such as a keyboard
being activated through the language switching UI.
##### Return status:
- `KM_CORE_STATUS_OK`: On success.
- `KM_CORE_STATUS_NO_MEM`:
In the event memory is unavailable to allocate internal buffers.
- `KM_CORE_STATUS_INVALID_ARGUMENT`:
In the event the `state` pointer is null or an invalid event or data is passed.

The keyboard processor may generate actions which should be processed by the
consumer of the API.

The action list will be cleared at the start of this call; options and context in
the state may also be modified.

##### Parameters:
- __state__: A pointer to the opaque state object.
- __event__: The event to be processed, from km_core_event_code enumeration
- __data__: Additional event-specific data. Currently unused, must be nullptr.

```c
*/
KMN_API
km_core_status
km_core_event(
  km_core_state *state,
  uint32_t event,
  void* data
);

enum km_core_event_code {
  // A keyboard has been activated by the user. The processor may use this
  // event, for example, to switch caps lock state or provide other UX.
  KM_CORE_EVENT_KEYBOARD_ACTIVATED = 1,
  //future: KM_CORE_EVENT_KEYBOARD_DEACTIVATED = 2,
};

#if defined(__cplusplus)
} // extern "C"
#endif
/*```
*/
