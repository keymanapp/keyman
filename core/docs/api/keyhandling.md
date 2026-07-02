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
