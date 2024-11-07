---
title: Desktop User Interfaces
---
  
Four different KeymanWeb user interfaces for desktop browsers are
included, allowing users to select and enable keyboard mapping from a
list of installed keyboards, and to control the visibility of the
On-Screen Keyboard.

["Button" Interface](button)
:   The simplest user interface is the "button" UI, which appears at a
    fixed position on the web page, and is attached to an empty 'Div'
    element with id='KeymanWebControl' added to the page where
    appropriate.

<!-- -->

["Toggle" Interface](toggle)
:   The "toggle" user interface provides a drop-down list of supported
    input languages and keyboards, and moves with the focus to appear at
    the right hand side of the focused element.

<!-- -->

["Float" Interface](float)
:   The "float" user interface (the original KeymanWeb user interface)
    appears as a drop-down list of available keyboards below the
    currently focused element

<!-- -->

["Toolbar" Interface](toolbar)
:   For pages which must support a large number of languages from many
    regions, we recommend using our "toolbar" user interface, which
    displays a drop-down map allowing users to first choose the region
    from the map or by name, then select input language by country.

Alternatively, developers may implement a custom interface for desktop
browsers with appropriate corporate branding, using the various API
functions to manage keyboard selection and display.

For touch-screen devices, the user-interface is integrated into the
On-Screen Keyboard, and cannot be overridden by the developer.
