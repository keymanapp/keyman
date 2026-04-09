# Communication between Application, IBus, and Keyman

The sequence diagram below shows the sequence of events that happen in
response to IBus sending the "Enable" signal to the Keyman Engine.
This illustrates why `client_supports_surrounding_text()` might return
a wrong value initially.

```mermaid
sequenceDiagram
    autonumber
    box blue Application
    participant A as Client
    participant IMC as IBusIMContext
    end
    box purple IBus
    participant InC as IBusInputContext
    participant E as IBusEngine
    end
    participant K as Keyman Engine

    E->>+K: enable signal
    K->>+E: IBusEngine.get_surrounding_text(NULL, NULL, NULL)
    Note left of K: Keyman wants to use surrounding text
    E->>+InC: RequireSurroundingText
    InC->>InC: set needs_surrounding_text flag = TRUE
    InC->>+IMC: require-surrounding-text signal
    IMC->>+A: retrieve_surrounding signal
    A->>+IMC: gtk_im_context_set_surrounding()
    IMC->>+InC: ibus_input_context_set_surrounding_text()
    InC->>+E: SetSurroundingText
    E->>+K: set_surrounding_text signal
    Note right of K: client_supports_surrounding_text() might<br>report a wrong value<br>because the processing of<br>get_surrounding_text() hasn't finished yet
    K->>-E: return from<br>set_surrounding_text signal
    E->>-InC: return from<br>SetSurroundingText
    InC->>-IMC: return from<br>ibus_input_context_set_surrounding_text()
    IMC->>-A: return from<br>gtk_im_context_set_surrounding()
    A->>-IMC: return from<br>retrieve_surrounding signal
    IMC->>-InC: return from<br>require-surrounding-text signal
    InC->>-E: return from<br>RequireSurroundingText
    E->>-K: return from<br>get_surrounding_text()
    K->>-E: return from<br>enable signal
```

## Links

- <https://docs.gtk.org/gtk3/class.IMContext.html>
- <https://ibus.github.io/docs/ibus-1.5/IBusInputContext.html>
- <https://ibus.github.io/docs/ibus-1.5/IBusEngine.html>
