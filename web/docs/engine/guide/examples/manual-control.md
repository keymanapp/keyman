---
title: Manual Mode Example
---

In this example, the web page designer specifies when KeymanWeb's on-screen keyboard may be displayed on non-mobile devices. They have also specified that the LaoKeys keyboard should be activated by default. This example continues to use the KeymanWeb default interface. Please click [this link](__manual-control.html) to open the test page.

## Code Walkthrough

Include the following script in the HEAD of your page:

```js
<script>
    keyman.init().then(function() {
      keyman.setActiveKeyboard('laokeys');
      keyman.osk.hide();
    });

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
    <script src="https://s.keyman.com/kmw/engine/17.0.331/keymanweb.js" type="text/javascript"></script>
    <!-- Load the LaoKeys keyboard stub -->
    <script src="laokeys_load.js" type="text/javascript"></script>
</head>
```

- File: [laokeys_load.js](js/laokeys_load.js)

And finally, include the control img for KeymanWeb:

```html
<!-- Display the KeymanWeb icon for the user to click on -->
<img style="border: solid 1px black; padding: 2px 2px 3px 2px" src='kmicon.png' alt='KeymanWeb' onclick='KWControlClick()' />
```

## API References

On programmatically setting the keyboard:
- [`keyman.setActiveKeyboard()`](../../reference/core/setActiveKeyboard).

On managing the visibility of the OSK:
- [`keyman.osk.hide()`](../../reference/osk/hide)
- [`keyman.osk.show()`](../../reference/osk/show).

------------------------------------------------------------------------

[On to Full Manual Control Example](full-manual-control)
