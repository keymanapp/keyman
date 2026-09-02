---
title: KeymanWeb Reference
---

[KeymanWeb Overview](overview)
:   KeymanWeb is a cross-browser JavaScript input method solution.

# API Reference

[`keyman`](core)
:   Core APIs for KeymanWeb, including initialization, adding keyboards, focus and element control.

[`keyman.osk`](osk)
:   On-Screen Keyboard module.

[`keyman.util`](util)
:   Utility functions.

[`keyman.interface` (also `KeymanWeb`)](interface)
:   API surface for keyboards, providing a number of functions for low-level processing of input, context and output.

[`keyman.ui` - Desktop User Interfaces](ui)
:   Four different KeymanWeb user interfaces for desktop browsers are included, allowing users to select and enable keyboard mapping from a list of installed keyboards, and to control the visibility of the On-Screen Keyboard.

# API Guides

[Initialization Options](core/init#init_options)
:   The following options can be defined by the site designer in the initialization call to KeymanWeb.

[Events](events)
:   A number of KeymanWeb events are exposed to allow the designer of a user interface to control the appearance and behavior of user interface elements.

[Keyboard Properties](keyboard_properties)
:   Standard properties for a Keyboard object returned from a keyboard .js file.

[Touch Layout Specifications](layoutspec)
:   Touch-screen layouts for KeymanWeb are specified as JSON objects containing a member object for each specified device type.

# See Also

[Keyboard Development Reference](/developer/language/guide/multi-platform)
:   Information on how to explicitly support KeymanWeb in custom keyboards.
