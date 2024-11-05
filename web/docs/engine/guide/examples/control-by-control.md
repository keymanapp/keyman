---
title: Control-by-Control Example
---

In this example, a simulated webmail form, the default and permissible keyboard for each control is managed by the web page. We use the automatic mode for simplicity of demonstration. We also link to the CDN for KeymanWeb in this example. Please click [this link](__control-by-control.html) to open the test page.

## Code Walkthrough

Include the following script in the HEAD of your page:

```js
<script type="text/javascript">
  function SetupDocument() {
    keyman.init({
      root: './',  // Note - if drawing the latest version of KeymanWeb from the CDN, this will
                   // default to the source folder on our servers.
      ui: 'toggle',
      resources: './'
    }).then(function() {
      // Load the keyboards of your choice here.
      loadKeyboards();

      /* Disable KeymanWeb interaction on the 'Email to' TEXT control */
      keyman.disableControl(document.f.address);
      /* Set the default keyboard for the 'Subject' TEXT control to 'off' (i.e. default browser keyboard) */
      /* As KeymanWeb relies on the on-screen keyboard to change languages for touch-based devices, this will
       * not work for them and will default to the first language added to KeymanWeb after initialization. */
      keyman.setKeyboardForControl(document.f.subject, '', '');
      /* Set the default keyboard for the 'Message body' TEXTAREA to the LaoKeys keyboard */
      keyman.setKeyboardForControl(document.f.text, 'Keyboard_laokeys', 'lo');
    });
  }

  window.addEventListener( 'load' , SetupDocument );
</script>
```

Also include the following HTML code:

```html
<head>
    <!-- Load the KeymanWeb engine -->
    <script src="https://s.keyman.com/kmw/engine/17.0.330/keymanweb.js" type="text/javascript"></script>
    <script src="https://s.keyman.com/kmw/engine/17.0.330/kmwuitoggle.js" type="text/javascript"></script>
    <!-- // Also be sure to load your keyboards.  Note that stubs will not work with this
         // example without detailing paths precisely in the init() call's parameter.-->
</head>


<!-- When the page has finished loading, advise KeymanWeb of control settings, see above -->
<body onload="SetupDocument()">
```
---
**Note:** In this example we disabled the first element (`document.f.address`) by API call. A later API call can re-enable KeymanWeb for this control should it fit the page's design. Alternatively, this can be done by instead adding the class `'kmw-disabled'` to the control. This will permanently block KeymanWeb from handling its input.

---

The `loadKeyboards()` function used by this page may be found
[here](js/unified_loader.js).

## API References

On disabling controls:
- [`keyman.disableControl()`](../../reference/core/disableControl).

On setting the keyboard for a single control:
- [`keyman.setKeyboardForControl()`](../../reference/core/setKeyboardForControl).

------------------------------------------------------------------------

[Return to Example index](./)
