---
title: Overview
---

*KeymanWeb* is a cross-browser JavaScript input method solution.

The *KeymanWeb API* provides JavaScript functions to allow a website developer to integrate the
use of *KeymanWeb* multi-lingual keyboard mapping into a website, using either a standard or a
custom-designed user-interface. The functions are exposed as API calls to the *KeymanWeb* core,
the *On-Screen Keyboard* module, a *Utility function* library, or one of the standard *User
Interface* modules.

A *KeymanWeb* instance is automatically constructed when you include the compiled KeymanWeb
script in your web page source.

The *KeymanWeb API* comprises the following objects:

- *Core*
:   Exposed as [`keyman`](core).

- *On-Screen Keyboard*
:   Exposed as [`keyman.osk`](osk).

- *Utility Functions*
:   Exposed as [`keyman.util`](util).

- *User Interface*
:   Exposed as [`keyman.ui`](ui).

The *KeymanWeb Keyboard API* provides endpoints for KeymanWeb keyboards to
interact with KeymanWeb. This API should not be used by website code.

- *Interface*
:   Exposed as [`keyman.interface`](interface)

The KeymanWeb Keyboard API publishes a deprecated global `KeymanWeb` which is
the same as `keyman.interface`. Use `keyman.interface` in preference to
`KeymanWeb`.