---
title: Processor - Keyman Core API
---

# km_core_event_flags enum {#km_core_event_flags}

## Description

Bit flags to be used with the `event_flags` parameter of [km_core_process_event]

## Specification

```c
enum km_core_event_flags {
  KM_CORE_EVENT_FLAG_DEFAULT = 0,
  KM_CORE_EVENT_FLAG_TOUCH = 1,
};

```
## Values

`KM_CORE_EVENT_FLAG_DEFAULT`
: default value: hardware

`KM_CORE_EVENT_FLAG_TOUCH`
: set if the event is touch, otherwise hardware

-------------------------------------------------------------------------------

# km_core_process_event() {#km_core_process_event}

## Description

Run the keyboard on an opaque state object with the provided virtual key and modifer
key state. Updates the state object as appropriate and fills out its internal set
of actions, which can be retrieved with [km_core_state_get_actions].

The state's actions will be cleared at the start of this call; options and context in
the state may also be modified.

## Specification

```c
KMN_API
km_core_status
km_core_process_event(km_core_state *state,
                     km_core_virtual_key vk,
                     uint16_t modifier_state,
                     uint8_t is_key_down,
                     uint16_t event_flags);

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

# km_core_event() {#km_core_event}

## Description

Tell the keyboard processor that an external event has occurred, such as a keyboard
being activated through the language switching UI.

The keyboard processor may generate actions which should be processed by the
consumer of the API.

The actions will be cleared at the start of this call; options and context in
the state may also be modified.

## Specification

```c
KMN_API
km_core_status
km_core_event(
  km_core_state *state,
  uint32_t event,
  void* data
);

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

# km_core_event_code enum {#km_core_event_code}

## Description

Possible events to be passed into Keyman Core from the Platform layer.

## Specification

```c
enum km_core_event_code {
  KM_CORE_EVENT_KEYBOARD_ACTIVATED = 1,
  //future: KM_CORE_EVENT_KEYBOARD_DEACTIVATED = 2,
};

```
## Values

`KM_CORE_EVENT_KEYBOARD_ACTIVATED`
: A keyboard has been activated by the user. The processor may use this
  event, for example, to switch caps lock state or provide other UX.


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