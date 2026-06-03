---
title: Control-by-Control Example
---

In this example, a simulated webmail form, the default and permissible keyboard for each control is managed by the web page. We use the automatic mode for simplicity of demonstration. We also link to the CDN for KeymanWeb in this example. Please click [this link](__control-by-control.html) to open the test page.

## Code Walkthrough

Include the following script in the HEAD of your page:

```js
<script type="text/javascript">
  keyman.init({
    root: './',  // Note - if drawing the latest version of KeymanWeb from the CDN, this will
                  // default to the source folder on our servers.
    ui: 'toggle',
    resources: './'
  }).then(async function() {
    // Load the keyboards of your choice here.
    await loadKeyboards();

    // Disable KeymanWeb interaction on the 'Email to' TEXT control
    keyman.disableControl(document.f.address);
    // Set the default keyboard for the 'Subject' TEXT control to 'off' (i.e. default
    // browser keyboard)
    keyman.setKeyboardForControl(document.f.subject, '', '');
    // Set the default keyboard for the 'Message body' TEXTAREA to the LaoKeys keyboard
    keyman.setKeyboardForControl(document.f.text, 'Keyboard_laokeys', 'lo');
  });
</script>
```

Also include the following HTML code:

```html
<head>
    <!-- Load the KeymanWeb engine -->
    <script src="js/keymanweb.js" type="text/javascript"></script>
    <script src="js/kmwuitoggle.js" type="text/javascript"></script>
    <!-- // Also be sure to load your keyboards.  Note that stubs will not work with this
         // example without detailing paths precisely in the init() call's parameter.-->
</head>


<!-- When the page has finished loading, advise KeymanWeb of control settings, see above -->
<body>
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
