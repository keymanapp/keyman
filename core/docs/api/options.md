---
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

# km_core_option_scope enum {#km_core_option_scope}

## Description

## Specification

```c
enum km_core_option_scope {
  KM_CORE_OPT_UNKNOWN      = 0,
  KM_CORE_OPT_KEYBOARD     = 1,
  KM_CORE_OPT_ENVIRONMENT  = 2,
  KM_CORE_OPT_MAX_SCOPES
};

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

# km_core_option_item struct {#km_core_option_item}

## Description

Defines a single option to be passed into the Keyman Core from the
Platform layer.

## Specification
```c
struct km_core_option_item {
  km_core_cp const *   key;
  km_core_cp const *   value;
  uint8_t             scope;
};

#define KM_CORE_OPTIONS_END { 0, 0, 0 }
```
## Members

`key`
: Null-terminated string key for the option

`value`
: Null-terminated string value for the option

`scope`
: Scope which an option belongs to, from [km_core_option_scope].

-------------------------------------------------------------------------------

# km_core_options_list_size() {#km_core_options_list_size}

## Description
Return the length of a terminated [km_core_option_item] array (options
list).

## Specification
```c
KMN_API
size_t
km_core_options_list_size(km_core_option_item const *opts);

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
```c
KMN_API
km_core_status
km_core_state_option_lookup(km_core_state const *state,
                      uint8_t scope,
                      km_core_cp const *key,
                      km_core_cp const **value);

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

# km_core_state_options_update() {#km_core_state_options_update}

## Description

Adds or updates one or more options from a list of [km_core_option_item]s.

## Specification
``` */
KMN_API
km_core_status
km_core_state_options_update(km_core_state *state,
                      km_core_option_item const *new_opts);

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

# km_core_state_options_to_json() {#km_core_state_options_to_json}

## Description

Export the contents of a [km_core_options] array to a JSON formatted document and
place it in the supplied buffer, reporting how much space was used. If null is
passed as the buffer the number of bytes required is returned in `space`. If
there is insufficent space to hold the document the contents of the buffer is
undefined. The returned buffer uses UTF-8 encoding.

## Specification

```c
KMN_API
km_core_status
km_core_state_options_to_json(km_core_state const *state,
                       char *buf,
                       size_t *space);

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