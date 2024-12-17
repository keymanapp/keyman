---
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

- __See more in__   [Keyman Glossary](https://github.com/keymanapp/keyman/wiki/Keyman-glossary)

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