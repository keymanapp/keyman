---
title: Keyboards - Keyman Core API
---

A keyboard is a set of rules and transforms in a Processor specific format for
transforming key events into action items. The keyboard is parsed and loaded by
the processsor and made available in an immutable fashion for use with any number
of state objects.

-------------------------------------------------------------------------------


# km_core_keyboard_attrs struct {#km_core_keyboard_attrs}

Provides read-only information about a keyboard.

```c
typedef struct {
  /** Processor specific version string. */
  km_core_cu const * version_string;
  /** Keyman keyboard ID string. */
  km_core_cu const * id;
  /** Set of default values for any options included in the keyboard. */
  km_core_option_item const * default_options;
} km_core_keyboard_attrs;
```

# km_core_keyboard_key struct {#km_core_keyboard_key}

Describes a single key and modifier combination that a keyboard handles, for
use by the Platform layer. This is used when the Platform layer must know in
advance which keys are used by a given keyboard.

```c
typedef struct {
  /** A virtual key. */
  km_core_virtual_key key;
  /** A [km_core_modifier_state] bitmask. */
  uint32_t modifier_flag;
} km_core_keyboard_key;
#define KM_CORE_KEYBOARD_KEY_LIST_END { 0, 0 }
```

# km_core_keyboard_imx struct {#km_core_keyboard_imx}

Describes a single Input Method eXtension library and entry point.

```c
typedef struct {
  /** The fully-qualified path and filename of the dynamically loaded library
   *  file. */
  km_core_cu const * library_name;
  /** The entry point for the IMX. */
  km_core_cu const * function_name;
  /** unique identifier used to call this function */
  uint32_t imx_id;
} km_core_keyboard_imx;
#define KM_CORE_KEYBOARD_IMX_END { 0, 0, 0 }
```

# km_core_keyboard_load_from_blob function {#km_core_keyboard_load_from_blob}

Parse and load keyboard from the supplied blob and a pointer to the loaded
keyboard into the out paramter.


## Parameters

### kb_name
a string with the name of the keyboard.

### blob
a byte array containing the content of a KMX/KMX+ file.

### blob_size
a size_t variable with the size of the blob in bytes.

### keyboard
A pointer to result variable: A pointer to the opaque
keyboard object returned by the Processor. This memory
must be freed with a call to [km_core_keyboard_dispose].

## Returns
One of the following values:

`KM_CORE_STATUS_OK`
: On success.

`KM_CORE_STATUS_NO_MEM`
: In the event an internal memory allocation fails.

`KM_CORE_STATUS_IO_ERROR`
: In the event the keyboard file is unparseable for any reason

`KM_CORE_STATUS_INVALID_ARGUMENT`
: In the event `keyboard` is null.

`KM_CORE_STATUS_OS_ERROR`
: Bit 31 (high bit) set, bits 0-30 are an OS-specific error code.

```c
KMN_API
km_core_status km_core_keyboard_load_from_blob(
  const km_core_path_name kb_name,
  const void* blob,
  const size_t blob_size,
  km_core_keyboard** keyboard
);
```

# km_core_keyboard_dispose function {#km_core_keyboard_dispose}

Free the allocated memory belonging to an opaque keyboard object previously
returned by [km_core_keyboard_load_from_blob].


## Parameters

### keyboard
A pointer to the opaque keyboard object to be disposed of.

```c
KMN_API
void
km_core_keyboard_dispose(
  km_core_keyboard const* keyboard
);
```

# km_core_keyboard_get_attrs function {#km_core_keyboard_get_attrs}

Returns the const internal attributes of the keyboard. This structure is
valid for the lifetime of the opaque keyboard object. Do not modify the
returned data.


## Parameters

### keyboard
A pointer to the opaque keyboard object to be queried.

### out
A pointer to the result: A pointer to a
[km_core_keyboard_attrs] structure.

## Returns
One of the following values:

`KM_CORE_STATUS_OK`
: On success.

`KM_CORE_STATUS_INVALID_ARGUMENT`
: If non-optional parameters are null.

```c
KMN_API
km_core_status
km_core_keyboard_get_attrs(
  km_core_keyboard const *keyboard,
  km_core_keyboard_attrs const **out
);
```

# km_core_keyboard_get_key_list function {#km_core_keyboard_get_key_list}

Returns the unordered full set of modifier+virtual keys that are handled by
the keyboard. The matching dispose call needs to be called to free the
memory.


## Parameters

### keyboard
A pointer to the opaque keyboard object to be queried.

### out
A pointer to an array of [km_core_keyboard_key] structures,
terminated by `KM_CORE_KEYBOARD_KEY_LIST_END`.

## Returns
One of the following values:

`KM_CORE_STATUS_OK`
: On success.

`KM_CORE_STATUS_INVALID_ARGUMENT`
: If non-optional parameters are null.

```c
KMN_API
km_core_status
km_core_keyboard_get_key_list(
  km_core_keyboard const *keyboard,
  km_core_keyboard_key **out
);
```

# km_core_keyboard_key_list_dispose function {#km_core_keyboard_key_list_dispose}

Free the allocated memory belonging to a keyboard key list previously
returned by [km_core_keyboard_get_key_list].


## Parameters

### key_list
A pointer to the keyboard key list to be disposed of.

```c
KMN_API
void
km_core_keyboard_key_list_dispose(
  km_core_keyboard_key *key_list
);
```

# km_core_keyboard_get_imx_list function {#km_core_keyboard_get_imx_list}

Returns the list of IMX libraries and function names that are referenced by
the keyboard. The matching dispose call needs to be called to free the
memory.


## Parameters

### keyboard
A pointer to the keyboard

### imx_list
A pointer to a variable that will contain a pointer to
the IMX list.

## Returns
One of the following values:

`KM_CORE_STATUS_OK`
: On success.

`KM_CORE_STATUS_INVALID_ARGUMENT`
: If non-optional parameters are null.

```c
KMN_API
km_core_status
km_core_keyboard_get_imx_list(
  km_core_keyboard const *keyboard,
  km_core_keyboard_imx **imx_list
);
```

# km_core_keyboard_imx_list_dispose function {#km_core_keyboard_imx_list_dispose}

Disposes of the IMX list.


## Parameters

### imx_list
A pointer to the IMX list.

```c
KMN_API
void
km_core_keyboard_imx_list_dispose(
  km_core_keyboard_imx *imx_list
);
```

# km_core_state_imx_register_callback function {#km_core_state_imx_register_callback}

Register the IMX callback endpoint for the client.


## Parameters

### state
A pointer to the opaque state object

### imx_callback
Pointer to a function that implements the IMX
callback

### callback_object
An opaque pointer that can be used to pass context
information to the callback function, usually it is
a user-defined data structure.

```c
KMN_API
void
km_core_state_imx_register_callback(
  km_core_state *state,
  km_core_keyboard_imx_platform imx_callback,
  void *callback_object
);
```

# km_core_state_imx_deregister_callback function {#km_core_state_imx_deregister_callback}

De-register IMX callback endpoint for the client.


## Parameters

### state
A pointer to the opaque state object

```c
KMN_API
void
km_core_state_imx_deregister_callback(
  km_core_state *state
);
```

# km_core_state_create function {#km_core_state_create}

Create a keyboard processor state object, maintaining state for the keyboard
in the environment passed.


## Parameters

### keyboard
A pointer to the opaque keyboard object this object will
hold state for.

### env
The array of [km_core_option_item] key/value pairs used to
initialise the environment, terminated by
`KM_CORE_OPTIONS_END`.

### out
A pointer to result variable: A pointer to the opaque state
object returned by the Processor, initalised to maintain
state for `keyboard`. This must be disposed of by a call to
[km_core_state_dispose].

## Returns
One of the following values:

`KM_CORE_STATUS_OK`
: On success.

`KM_CORE_STATUS_NO_MEM`
: In the event memory is unavailable to allocate a state object.

`KM_CORE_STATUS_INVALID_ARGUMENT`
: In the event the `keyboard` or `out` pointer are null.

```c
KMN_API
km_core_status
km_core_state_create(
  km_core_keyboard const *keyboard,
  km_core_option_item const *env,
  km_core_state **out
);
```

# km_core_state_clone function {#km_core_state_clone}

Clone an existing opaque state object.


## Parameters

### state
A pointer to the opaque state object to be cloned.

### out
A pointer to result variable: A pointer to the opaque state
object returned by the Processor, cloned from the existing
object `state`. This must be disposed of by a call to
[km_core_state_dispose].

## Returns
One of the following values:

`KM_CORE_STATUS_OK`
: On success.

`KM_CORE_STATUS_NO_MEM`
: In the event memory is unavailable to allocate a state object.

`KM_CORE_STATUS_INVALID_ARGUMENT`
: In the event the `state` or `out` pointer are null.

```c
KMN_API
km_core_status
km_core_state_clone(
  km_core_state const *state,
  km_core_state **out
);
```

# km_core_state_dispose function {#km_core_state_dispose}

Free the allocated resources belonging to a [km_core_state] object previously
returned by [km_core_state_create] or [km_core_state_clone]. After this all
pointers previously returned by any [km_core_state] family of calls will
become invalid.


## Parameters

### state
A pointer to the opaque state object to be disposed.

```c
KMN_API
void
km_core_state_dispose(
  km_core_state const *state
);
```

# km_core_debug_context_type enum {#km_core_debug_context_type}


As of version 17, the cached context is an internal property of the state,
not exposed to the consumer of the API -- apart from the Keyman Developer
Keyboard Debugger. However, for other debug purposes, it is helpful to be
able to examine the cached context, so a debug-formatted version of the
context is made available with  [km_core_state_context_debug]. This is not
intended to be parsed for reading the context for other purposes, and the
format may change.

The three context types are: cached, intermediate, and app.

```c
typedef enum {
  /** the internal context used by Core, which may be normalized and may contain
   *  markers. This is set via [km_core_state_context_set_if_needed], and will
   *  be modified during keystroke event processing. */
  KM_CORE_DEBUG_CONTEXT_CACHED        = 0,
  /** internal context used by IMX, only valid during keystroke event
   *  processing. */
  KM_CORE_DEBUG_CONTEXT_INTERMEDIATE  = 1,
  /** an exact copy of the current context passed in to
   *  [km_core_state_context_set_if_needed], which is used to verify the precise
   *  text manipulations required when emitted changes. This input context is in
   *  "NFU" -- normalization form unknown, and may be mixed normalization so may
   *  require fixups when it is manipulated by keyboard processors that support
   *  normalization, such as the LDML keyboard processor. */
  KM_CORE_DEBUG_CONTEXT_APP           = 2
} km_core_debug_context_type;
```

# km_core_state_context_debug function {#km_core_state_context_debug}

Returns a debug formatted string of the context from the state.


## Parameters

### state
A pointer to the opaque state object to be queried.

### context_type
The type of context to retrieve from the state.

## Returns
A pointer to a [km_core_cu] UTF-16 string. Must be disposed of by a
call to [km_core_cu_dispose].

```c
KMN_API
km_core_cu *
km_core_state_context_debug(
  const km_core_state *state,
  km_core_debug_context_type context_type
);
```

# km_core_cu_dispose function {#km_core_cu_dispose}

Free the allocated memory belonging to a [km_core_cu] array previously
returned by [km_core_state_context_debug]. May be `nullptr`.


## Parameters

### cp
A pointer to the start of the [km_core_cu] array to be disposed
of.

```c
KMN_API
void
km_core_cu_dispose(
  km_core_cu *cp
);
```

[km_core_cu]: background#km_core_cu "km_core_cu type"
[km_core_usv]: background#km_core_usv "km_core_usv type"
[km_core_virtual_key]: background#km_core_virtual_key "km_core_virtual_key type"
[km_core_status]: background#km_core_status "km_core_status type"
[km_core_keyboard]: background#km_core_keyboard "km_core_keyboard struct"
[km_core_state]: background#km_core_state "km_core_state struct"
[km_core_options]: background#km_core_options "km_core_options struct"
[km_core_keyboard_imx_platform]: background#km_core_keyboard_imx_platform "km_core_keyboard_imx_platform callback function"
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
[km_core_state_option_lookup]: options#km_core_state_option_lookup "km_core_state_option_lookup function"
[km_core_state_options_update]: options#km_core_state_options_update "km_core_state_options_update function"
[km_core_keyboard_attrs]: keyboards#km_core_keyboard_attrs "km_core_keyboard_attrs struct"
[km_core_keyboard_key]: keyboards#km_core_keyboard_key "km_core_keyboard_key struct"
[km_core_keyboard_imx]: keyboards#km_core_keyboard_imx "km_core_keyboard_imx struct"
[km_core_keyboard_load_from_blob]: keyboards#km_core_keyboard_load_from_blob "km_core_keyboard_load_from_blob function"
[km_core_keyboard_dispose]: keyboards#km_core_keyboard_dispose "km_core_keyboard_dispose function"
[km_core_keyboard_get_attrs]: keyboards#km_core_keyboard_get_attrs "km_core_keyboard_get_attrs function"
[km_core_keyboard_get_key_list]: keyboards#km_core_keyboard_get_key_list "km_core_keyboard_get_key_list function"
[km_core_keyboard_key_list_dispose]: keyboards#km_core_keyboard_key_list_dispose "km_core_keyboard_key_list_dispose function"
[km_core_keyboard_get_imx_list]: keyboards#km_core_keyboard_get_imx_list "km_core_keyboard_get_imx_list function"
[km_core_keyboard_imx_list_dispose]: keyboards#km_core_keyboard_imx_list_dispose "km_core_keyboard_imx_list_dispose function"
[km_core_state_imx_register_callback]: keyboards#km_core_state_imx_register_callback "km_core_state_imx_register_callback function"
[km_core_state_imx_deregister_callback]: keyboards#km_core_state_imx_deregister_callback "km_core_state_imx_deregister_callback function"
[km_core_state_create]: keyboards#km_core_state_create "km_core_state_create function"
[km_core_state_clone]: keyboards#km_core_state_clone "km_core_state_clone function"
[km_core_state_dispose]: keyboards#km_core_state_dispose "km_core_state_dispose function"
[km_core_debug_context_type]: keyboards#km_core_debug_context_type "km_core_debug_context_type enum"
[km_core_state_context_debug]: keyboards#km_core_state_context_debug "km_core_state_context_debug function"
[km_core_cu_dispose]: keyboards#km_core_cu_dispose "km_core_cu_dispose function"
[km_core_event_flags]: processor#km_core_event_flags "km_core_event_flags enum"
[km_core_process_event]: processor#km_core_process_event "km_core_process_event function"
[km_core_event]: processor#km_core_event "km_core_event function"
[km_core_event_code]: processor#km_core_event_code "km_core_event_code enum"
[km_core_action_item]: actions#km_core_action_item "km_core_action_item struct"
[km_core_state_action_items]: actions#km_core_state_action_items "km_core_state_action_items function"
[km_core_state_queue_action_items]: actions#km_core_state_queue_action_items "km_core_state_queue_action_items function"
[km_core_process_queued_actions]: actions#km_core_process_queued_actions "km_core_process_queued_actions function"
[km_core_context_type]: context#km_core_context_type "km_core_context_type enum"
[km_core_context_item]: context#km_core_context_item "km_core_context_item struct"
[KM_CORE_CONTEXT_ITEM_END]: context#KM_CORE_CONTEXT_ITEM_END "KM_CORE_CONTEXT_ITEM_END macro"
[km_core_state_get_intermediate_context]: context#km_core_state_get_intermediate_context "km_core_state_get_intermediate_context function"
[km_core_context_items_dispose]: context#km_core_context_items_dispose "km_core_context_items_dispose function"
[km_core_state_context]: context#km_core_state_context "km_core_state_context function"
[km_core_state_app_context]: context#km_core_state_app_context "km_core_state_app_context function"
[km_core_context_set]: context#km_core_context_set "km_core_context_set function"
[km_core_context_clear]: context#km_core_context_clear "km_core_context_clear function"
[km_core_context_item_list_size]: context#km_core_context_item_list_size "km_core_context_item_list_size function"
[km_core_context_get]: context#km_core_context_get "km_core_context_get function"
[km_core_context_length]: context#km_core_context_length "km_core_context_length function"
[km_core_modifier_state]: virtual-keys#km_core_modifier_state "km_core_modifier_state enum"
[km_core_virtual_key_value]: virtual-keys#km_core_virtual_key_value "km_core_virtual_key_value "