# Key Handling

For each key press the processor will called twice, for the key down event
as well as for the key up event. Depending on the type of key pressed the
processor might handle the key itself or pass it on to the application.
The value of `emit_keystroke` in `km_core_actions` struct tells if the
processor handled the key (`emit_keystroke=0`) or not (`emit_keystroke=1`).
It is important that the same value gets set for both key down and key up
events, otherwise the application might miss some events which then looks
to the user like keys are stuck.

Usually the processor won't handle any frame keys and let the application
deal with it. This allows shortcut keys like <kbd>Ctrl</kbd>+<kbd>C</kbd>
to work. The only exception is the <kbd>Backspace</kbd> key which is
handled internally if enough context is available. Regular keys will be
handled by the processor.

Note that there is a difference between CLDR/LDML and KMN keyboards if
there are no rules/transforms defined for a key: KMN keyboards will
output the cap value of the key (e.g. pressing <kbd>a</kbd> will output
`a` if no rule is defined), whereas CLDR/LDML keyboards will suppress
any output for that key.

The following table lists the state of `km_core_actions.emit_keystroke`
on return of `km_core_process_event` when the following type of key is
pressed:

|Type                         |   KeyDown     |  KeyUp   |
|-----------------------------|---------------|----------|
|Key with rule                |    FALSE      |  FALSE   |
|Key w/o rule                 |    FALSE      |  FALSE   |
|Framekeys:                   |               |          |
|<kbd>Enter</kbd>             |    TRUE       |  TRUE    |
|<kbd>Backspace</kbd>¹        |    FALSE      |  FALSE   |
|<kbd>Backspace</kbd>²        |    TRUE       |  TRUE    |
|<kbd>Ctrl</kbd>+Key          |    TRUE       |  TRUE    |
|Modifier key <kbd>Shift</kbd>|    TRUE       |  TRUE    |
|Modifier key <kbd>Ctrl</kbd> |    TRUE       |  TRUE    |
|Modifier key <kbd>LAlt</kbd> |    TRUE       |  TRUE    |

1: context available<br/>
2: without or with empty context
