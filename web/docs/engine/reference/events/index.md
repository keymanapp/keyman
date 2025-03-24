---
title: Events - Keyman Engine for Web
---

A number of events are exposed to allow the designer of a user
interface to control the appearance and behavior of user interface
elements. Standard event-processing requires all arguments to be passed
as an array (object) with named member variables.

Two components of Keyman Engine for Web specify events: 
* `keyman` object -- the main component
* `keyman.osk` object -- the on-screen keyboard component

Object events are handled in user code by passing the handler entry to
the object, using *addEventListener()*.

---

### `keyman` events

[`beforekeyboardchange`](keyman/beforekeyboardchange)
:   Called when keyboard input language about to change.

<!-- -->

[`controlblurred`](keyman/controlblurred)
:   Called when input element loses focus.

<!-- -->

[`controlfocused`](keyman/controlfocused)
:   Called when input element receives focus.

<!-- -->

[`keyboardchange`](keyman/keyboardchange)
:   Called when keyboard input language changed.

<!-- -->

[`keyboardloaded`](keyman/keyboardloaded)
:   Called when keyboard code loaded.
<!-- -->

[`keyboardregistered`](keyman/keyboardregistered)
:   Called when keyboard 'stub' processed (for listing as available
    keyboard).

<!-- -->

[`loaduserinterface`](keyman/loaduserinterface)
:   Called when allow ui initialization.

<!-- -->

[`unloaduserinterface`](keyman/unloaduserinterface)
:   Called when allow ui clean-up.

For example, to define a user function to handle the KeymanWeb
`keyboardchange` event, include:

``` typescript
keyman.addEventListener('keyboardchange',
  function(p)
  {
    ui.updateMenu(p.internalName,p.languageCode);
  });

```

---

### `keyman.osk` On Screen Keyboard events

[`configclick`](osk/configclick)
:   Called when allows the UI to present KeymanWeb configuration
    options.

<!-- -->

[`helpclick`](osk/helpclick)
:   Called when allows the UI to present a help page.

<!-- -->

[`hide`](osk/hide)
:   Called when OSK hidden.

<!-- -->

[`resizemove`](osk/resizemove)
:   Called when OSK resized or moved on desktop.

<!-- -->

[`show`](osk/show)
:   Called when OSK displayed.


For example, to add an event handler that modifies the user interface when the on-screen
keyboard is displayed:

``` typescript
keyman.osk.addEventListener('show',
  function(p)
  {
    ui.updateUI(p.x, p.y, p.userLocated);
  });

```
