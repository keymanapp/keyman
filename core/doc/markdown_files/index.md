# Keyman Keyboard Processor API

## Requirements

1. Cross platform.
2. Cross language.
3. Facilitate stateless operation of the Engine.
4. Keyboard format agnostic -- support both KMN and future LDML based keyboards.
5. Support querying Engine attributes.
6. Support querying Keyboard attributes.
7. Idempotent

## Design decisions in support of requirements:

- Use C or C99 types and calling convention for the interface, it has the
  broadest language FFI support. [1,2]
- Have client (platform glue) code load keyboards, manage & pass state. [3,4,7]
- Provide query calls to return static attributes data for keyboards and
  engine [5,6]
- Provide get/set calls for client accessible keyboard state information [3,4]

## Glossary

- __Platform layer:__
the code that consumes the Keyman Keyboard Processor API, and provides the
operating system-specific handling of keystroke events and integration with
applications.
- __Client Application:__
the application that has the focus and receives text events from the Platform
layer.
- __Context:__ Text preceding the insertion point
- __Marker:__ Positional state that can be placed in the Context.
- __Keyboard:__ A set of rules for execution by an Engine
- __Option:__ A variable in a dynamic or static key value store.
- __Processor:__
The component that implements this API and  can parse and execute a particular
keyboard.
- __State:__ An object that holds internal state of the Processor for a given
insertion point
- __Action:__
A directive output by the processor detailing how the Platform layer should
transform the Client Application's text buffer. There may be several items
produced by a single keyboard event.
- __Keyboard Event:__
A virtual key board event and modifier map recevied from the Platform layer to be
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

## API

### Namespace

All calls, types and enums are prefixed with the namespace identifier `km_core_`

### Changes from 16.0

* The namespace identifier has changed from `km_kbp_` to `km_core_`.
* Most context APIs are now private, and `km_core_context_set_if_needed` is the
  primary context function. Private APIs are available in
  `keyman_core_api_context.h`.
* The action queue APIs are now private and deprecated. Instead, use
  `km_core_state_get_actions`. Private APIs are available in
  `keyman_core_api_actions.h`.
* Debug APIs are available in `keyman_core_api_debug.h`.
