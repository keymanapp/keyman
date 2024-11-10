---
title: Keyboards - Keyman Core API
---

A keyboard is a set of rules and transforms in a Processor specific format for
transforming key events into action items. The keyboard is parsed and loaded by
the processsor and made available in an immutable fashion for use with any number
of state objects.

-------------------------------------------------------------------------------

# km_core_keyboard_attrs struct {#km_core_keyboard_attrs}

## Description

Provides read-only information about a keyboard.

## Specification
```c
typedef struct {
  km_core_cp const * version_string;
  km_core_cp const * id;
  km_core_path_name  folder_path;
  km_core_option_item const * default_options;
} km_core_keyboard_attrs;

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

# km_core_keyboard_key struct {#km_core_keyboard_key}

## Description

Describes a single key and modifier combination that a keyboard handles, for
use by the Platform layer. This is used when the Platform layer must know in
advance which keys are used by a given keyboard.

## Specification

```c
typedef struct {
  km_core_virtual_key key;
  uint32_t modifier_flag;
} km_core_keyboard_key;

#define KM_CORE_KEYBOARD_KEY_LIST_END { 0, 0 }

```

## Members

`key`
: A virtual key.

`modifier_flag`
: A [km_core_modifier_state] bitmask.

-------------------------------------------------------------------------------

# km_core_keyboard_imx struct {#km_core_keyboard_imx}

## Description

Describes a single Input Method eXtension library and entry point.

## Specification

```c
typedef struct {
  km_core_cp const * library_name;
  km_core_cp const * function_name;
  uint32_t imx_id;
} km_core_keyboard_imx;

#define KM_CORE_KEYBOARD_IMX_END { 0, 0, 0 }

```
## Members

`library_name`
: The fully-qualified path and filename of the dynamically loaded library file.

`function_name`
: The entry point for the IMX.

`imx_id`
: unique identifier used to call this function

-------------------------------------------------------------------------------

# km_core_keyboard_load() {#km_core_keyboard_load}

## Description

Parse and load keyboard from the supplied path and a pointer to the loaded keyboard
into the out paramter.

## Specification

```c
KMN_API
km_core_status
km_core_keyboard_load(km_core_path_name kb_path,
                     km_core_keyboard **keyboard);

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

# km_core_keyboard_dispose() {#km_core_keyboard_dispose}

## Description

Free the allocated memory belonging to an opaque keyboard object previously
returned by [km_core_keyboard_load].

## Specification

```c
KMN_API
void
km_core_keyboard_dispose(km_core_keyboard *keyboard);

```
## Parameters
`keyboard`
: A pointer to the opaque keyboard object to be disposed of.

-------------------------------------------------------------------------------

# km_core_keyboard_get_attrs() {#km_core_keyboard_get_attrs}

## Description

Returns the const internal attributes of the keyboard. This structure is valid
for the lifetime of the opaque keyboard object. Do not modify the returned data.

## Specification

```c
KMN_API
km_core_status
km_core_keyboard_get_attrs(km_core_keyboard const *keyboard,
                          km_core_keyboard_attrs const **out);

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

# km_core_keyboard_get_key_list() {#km_core_keyboard_get_key_list}

## Description

Returns the unordered full set of modifier+virtual keys that are handled by the
keyboard. The matching dispose call needs to be called to free the memory.

## Specification

```c
KMN_API
km_core_status
km_core_keyboard_get_key_list(km_core_keyboard const *keyboard,
                            km_core_keyboard_key **out);

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

# km_core_keyboard_key_list_dispose() {#km_core_keyboard_key_list_dispose}

## Description

Free the allocated memory belonging to a keyboard key list previously
returned by [km_core_keyboard_get_key_list].

## Specification

```c
KMN_API
void km_core_keyboard_key_list_dispose(km_core_keyboard_key *key_list);

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

```c
KMN_API
km_core_status km_core_keyboard_get_imx_list(km_core_keyboard const *keyboard, km_core_keyboard_imx **imx_list);

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

# km_core_keyboard_imx_list_dispose() {#km_core_keyboard_imx_list_dispose}

## Description

Disposes of the IMX list.

## Specification

```c
KMN_API
void km_core_keyboard_imx_list_dispose(km_core_keyboard_imx *imx_list);

```
## Parameters

`imx_list`
: A pointer to the IMX list.

-------------------------------------------------------------------------------

# km_core_state_imx_register_callback() {#km_core_state_imx_register_callback}

## Description

Register the IMX callback endpoint for the client.

## Specification

```c
KMN_API
void km_core_state_imx_register_callback(km_core_state *state, km_core_keyboard_imx_platform imx_callback, void *callback_object);

```
## Parameters

`state`
: A pointer to the opaque state object

`imx_callback`
: pointer to a function that implements the IMX callback

`callback_object`
: TODO

-------------------------------------------------------------------------------

# km_core_state_imx_deregister_callback() {#km_core_state_imx_deregister_callback}

## Description

De-register IMX callback endpoint for the client.

## Specification

```c
KMN_API
void km_core_state_imx_deregister_callback(km_core_state *state);

```
## Parameters

`state`
: A pointer to the opaque state object

-------------------------------------------------------------------------------

# km_core_state_create() {#km_core_state_create}

## Description

Create a keyboard processor state object, maintaining state for the keyboard in
the environment passed.

## Specification

```c
KMN_API
km_core_status
km_core_state_create(km_core_keyboard *keyboard,
                    km_core_option_item const *env,
                    km_core_state **out);

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

# km_core_state_clone() {#km_core_state_clone}

## Description

Clone an existing opaque state object.

## Specification

```c
KMN_API
km_core_status
km_core_state_clone(km_core_state const *state,
                   km_core_state **out);

```
## Parameters

`state`
: A pointer to the opaque statea object to be cloned.

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

# km_core_state_dispose() {#km_core_state_dispose}

## Description

Free the allocated resources belonging to a [km_core_state] object previously
returned by [km_core_state_create] or [km_core_state_clone]. After this all
pointers previously returned by any [km_core_state] family of calls will become
invalid.

## Specification

```c
KMN_API
void
km_core_state_dispose(km_core_state *state);

```
## Parameters

`state`
: A pointer to the opaque state object to be disposed.

-------------------------------------------------------------------------------

# km_core_debug_context_type enum {#km_core_debug_context_type}

As of version 17, the cached context is an internal property of the
state, not exposed to the consumer of the API -- apart from the
Keyman Developer Keyboard Debugger. However, for other debug
purposes, it is helpful to be able to examine the cached context, so
a debug-formatted version of the context is made available with
[km_core_state_context_debug]. This is not intended to be parsed for
reading the context for other purposes, and the format may change.

The three context types are: cached, intermediate, and app.

## Specification

```c
typedef enum {
  KM_CORE_DEBUG_CONTEXT_CACHED        = 0,
  KM_CORE_DEBUG_CONTEXT_INTERMEDIATE  = 1,
  KM_CORE_DEBUG_CONTEXT_APP           = 2
} km_core_debug_context_type;

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

# km_core_state_context_debug() {#km_core_state_context_debug}

## Description

Returns a debug formatted string of the context from the state.

## Specification

```c
KMN_API
km_core_cp *
km_core_state_context_debug(km_core_state *state, km_core_debug_context_type context_type);

```
## Parameters

`state`
: A pointer to the opaque state object to be queried.

`context_type`
: The type of context to retrieve from the state.

## Returns

A pointer to a [km_core_cp] UTF-16 string. Must be disposed of by a call
to [km_core_cp_dispose].

-------------------------------------------------------------------------------

# km_core_cp_dispose() {#km_core_cp_dispose}

## Description

Free the allocated memory belonging to a [km_core_cp] array previously
returned by [km_core_state_context_debug]. May be `nullptr`.

## Specification

```c
KMN_API
void
km_core_cp_dispose(km_core_cp *cp);

```
## Parameters

`cp`
: A pointer to the start of the [km_core_cp] array to be disposed of.

-------------------------------------------------------------------------------

# km_core_state_to_json() {#km_core_state_to_json}

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

```c
KMN_API
km_core_status
km_core_state_to_json(km_core_state const *state,
                     char *buf,
                     size_t *space);

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
[km_core_keyboard_load]: keyboards#km_core_keyboard_load "km_core_keyboard_load function"
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