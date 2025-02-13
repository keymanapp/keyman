---
title: KeymanWeb Reference
---

[KeymanWeb Overview](overview)
:   KeymanWeb is a cross-browser JavaScript input method solution.

[KeymanWeb Core Module](core)
:   The KeymanWeb core module is exposed to the developer as `window.keyman`.

[On-Screen Keyboard Module](osk)
:   The On-Screen Keyboard module is exposed to the developer as `window.keyman.osk`.

[KeymanWeb Utility Module](util)
:   The KeymanWeb Utility Function module is exposed to the developer as `window.keyman.util`.

[KeymanWeb Output Functions](interface)
:   KeymanWeb exposes its keyboard interface object through `window.keyman.interface`, providing a number of functions for low-level processing of input, context and output.

[KeymanWeb Compatibility Functions](compatibility)
:   The following KeymanWeb core functions have been retained for compatibility with existing custom keyboards, but should not be used in any new keyboards or user interfaces. Equivalent new function calls are indicated.

[Desktop User Interfaces](ui)
:   Four different KeymanWeb user interfaces for desktop browsers are included, allowing users to select and enable keyboard mapping from a list of installed keyboards, and to control the visibility of the On-Screen Keyboard.

[KeymanWeb Initialization Options](core/init#init_options)
:   The following options can be defined by the site designer in the initialization call to KeymanWeb.

[KeymanWeb Events](events)
:   A number of KeymanWeb events are exposed to allow the designer of a user interface to control the appearance and behavior of user interface elements. Standard event-processing requires all arguments to be passed as an array (object) with named member variables.

[KeymanWeb Keyboard Properties](keyboard_properties)
:   Most keyboards are generated automatically from the Keyman keyboard source by Keyman Developer and contain properties used by KeymanWeb during keyboard mapping.

[KeymanWeb Layout Designer](layouts)
:   One of the main features of KeymanWeb is its ability to support distinct, user-customizable layouts for touch-screen keyboards on phones and tablets.

[KeymanWeb Layout Specifications](layoutspec)
:   Touch-screen layouts for KeymanWeb are specified as JSON objects containing a member object for each specified device type.

[KeymanWeb Keyboard Development Reference](/developer/language/guide/multi-platform)
:   Information on how to explicitly support KeymanWeb in custom keyboards.
