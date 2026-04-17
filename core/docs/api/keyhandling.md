---
title: Keyhandling
---

# Key Handling

While Keyman Core does not generally emit actions from key up events,
the `emit_keystroke` value for a key up event will match the corresponding
key down event, in order to avoid 'stuck key' scenarios'.

Usually the processor won't handle any frame keys and let the application
deal with it. This allows shortcut keys like <kbd>Ctrl</kbd>+<kbd>C</kbd>
to work. One exception is the <kbd>Backspace</kbd> key which is
handled internally if enough context is available. Keyboards can define
rules for other frame keys, e.g. <kbd>Ctrl</kbd>+Key which are then
also handled internally. Regular keys will be handled by the processor.

Note that there is a difference between CLDR/LDML and KMN keyboards if
there are no rules/transforms defined for a key: KMN keyboards will
output the cap value of the key (e.g. pressing <kbd>a</kbd> will output
`a` if no rule is defined), whereas CLDR/LDML keyboards will suppress
any output for that key. However, `km_core_actions.emit_keystroke` will
be `FALSE` for both KMN and CLR/LDML keyboards.

The following table lists the state of `km_core_actions.emit_keystroke`
on return of `km_core_process_event` when the following type of key is
pressed:

|Type                         | KeyDown/Up |
|-----------------------------|------------|
|Core key with rule           |    FALSE   |
|Core key w/o rule            |    FALSE   |
|Framekeys:                   |            |
|<kbd>Enter</kbd>             |    TRUE    |
|<kbd>Backspace</kbd>¹        |    FALSE   |
|<kbd>Backspace</kbd>²        |    TRUE    |
|<kbd>Ctrl</kbd>+Key          |    TRUE    |
|Modifier key <kbd>Shift</kbd>|    TRUE    |
|Modifier key <kbd>Ctrl</kbd> |    TRUE    |
|Modifier key <kbd>LAlt</kbd> |    TRUE    |

1: context available<br/>
2: without or with empty context


[km_core_cu]: background#km_core_cu "km_core_cu type"
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
[km_core_cu_dispose]: keyboards#km_core_cu_dispose "km_core_cu_dispose function"
[km_core_event_flags]: processor#km_core_event_flags "km_core_event_flags enum"
[km_core_process_event]: processor#km_core_process_event "km_core_process_event function"
[km_core_event]: processor#km_core_event "km_core_event function"
[km_core_event_code]: processor#km_core_event_code "km_core_event_code enum"