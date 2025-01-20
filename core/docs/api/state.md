---
title: State and Actions - Keyman Core API
---

A State object maintains all per keyboard related state including context
and dynamic options ("option stores" in kmn format).

When a keystroke is processed by Keyman Core, Core provides back a set of actions
for the Platform layer to emit to the Client application. These actions are
owned by the state object.

-------------------------------------------------------------------------------

# km_core_caps_state enum {#km_core_caps_state}

## Description

Describes the

## Specification
```c
typedef enum { KM_CORE_CAPS_UNCHANGED = -1, KM_CORE_CAPS_OFF = 0, KM_CORE_CAPS_ON = 1 } km_core_caps_state;

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

# km_core_actions struct {#km_core_actions}

## Description

This structure provides the results of processing a key event to the Platform layer and
should be processed by the Platform layer to issue commands to the os text
services framework to transform the text store in the Client Application, among
other actions.

This API replaces the Action items APIs, which are now deprecated and will be
removed in the future.

## Specification
```c
typedef struct {
  unsigned int code_points_to_delete;
  const km_core_usv* output;
  km_core_option_item * persist_options;
  km_core_bool do_alert;
  km_core_bool emit_keystroke;
  km_core_caps_state new_caps_lock_state;
  const km_core_usv* deleted_context;
} km_core_actions;

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

# km_core_state_get_actions() {#km_core_state_get_actions}

## Description

Returns a pointer to an actions object which details all the actions
that the Platform layer must take after a keystroke. The `code_points_to_delete`
action must be performed before the `output` action, but the other
actions may be performed in any order.

## Specification
```c
KMN_API
km_core_actions const *
km_core_state_get_actions(
  km_core_state const *state
);

```
## Parameters

`state`
: An opaque pointer to a state object.

## Returns

A pointer to a [km_core_actions] object. This data becomes invalid
when the state object is destroyed, or after a call to
[km_core_process_event]. Do not modify the contents of this data.

-------------------------------------------------------------------------------

# km_core_context_status enum {#km_core_context_status}

## Description

Return values for [km_core_state_context_set_if_needed].

## Specification

```c
typedef enum {
  KM_CORE_CONTEXT_STATUS_UNCHANGED = 0,
  KM_CORE_CONTEXT_STATUS_UPDATED = 1,
  KM_CORE_CONTEXT_STATUS_CLEARED = 2,
  KM_CORE_CONTEXT_STATUS_ERROR = 3,
  KM_CORE_CONTEXT_STATUS_INVALID_ARGUMENT = 4,
} km_core_context_status;

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

# km_core_state_context_set_if_needed() {#km_core_state_context_set_if_needed}

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
```c
KMN_API
km_core_context_status
km_core_state_context_set_if_needed(
  km_core_state *state,
  km_core_cp const *application_context
);

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

# km_core_state_context_clear() {#km_core_state_context_clear}

## Description

Clears the internal cached context for the state. This is the same as
`km_core_context_clear(km_core_state_context(&state))`.

[km_core_state_context_set_if_needed] and [km_core_state_context_clear]
will replace most uses of the existing Core context APIs.

## Specification
```c
KMN_API
km_core_status
km_core_state_context_clear(
  km_core_state *state
);

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