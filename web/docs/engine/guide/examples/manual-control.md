---
title: Manual Mode Example
---

In this example, the web page designer specifies when KeymanWeb's on-screen keyboard may be displayed on non-mobile devices. They have also specified that the LaoKeys keyboard should be activated by default. This example continues to use the KeymanWeb default interface. Please click [this link](__manual-control.html) to open the test page.

## Code Walkthrough

Include the following script in the HEAD of your page:

```js
<script>
  /* SetupDocument: Called when the page finishes loading */
  function SetupDocument()
  {
    /* Make sure that Keyman is initialized (we can't guarantee initialization order) */
    keyman.init();

    KWControl = document.getElementById('KWControl');
    /* Prevents automatic display of the onscreen keyboard. (default automatic) */
    keyman.osk.hide();
    /* Select the LaoKeys keyboard */
    keyman.setActiveKeyboard('laokeys');
  }

  /* KWControlClick: Called when user clicks on the KWControl IMG */
  function KWControlClick()
  {
    if(keyman.osk.isEnabled()) {
      keyman.osk.hide();
    } else {
      keyman.osk.show(true); // Specifies that the OSK should display whenever a valid
                             // control has focus, re-enabling the default behavior.
    }
  }
</script>
```

Also include the following HTML code:

```html
<head>
    <!-- Load the KeymanWeb engine -->
    <script src="keymanweb.js" type="text/javascript"></script>
    <!-- Load the LaoKeys keyboard stub -->
    <script src="laokeys_load.js" type="text/javascript"></script>
</head>

<!-- When the page has finished loading, activate the LaoKeys keyboard, see above -->
<body onload="SetupDocument()">
```

- File: [laokeys_load.js](js/laokeys_load.js)

And finally, include the control img for KeymanWeb:

```html
<!-- Display the KeymanWeb icon for the user to click on -->
<img style="border: solid 1px black; padding: 2px 2px 3px 2px" src='kmicon.png' alt='KeymanWeb' onclick='KWControlClick()' id='KWControl' />
```

## API References

On programmatically setting the keyboard:
- [`keyman.setActiveKeyboard()`](../../reference/core/setActiveKeyboard).

On managing the visibility of the OSK:
- [`keyman.osk.hide()`](../../reference/osk/hide)
- [`keyman.osk.show()`](../../reference/osk/show).

------------------------------------------------------------------------

[On to Full Manual Control Example](full-manual-control)
