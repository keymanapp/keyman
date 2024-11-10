---
title: Manual Control - Custom Interface
---

In this example, the web page designer has opted for their own user interface instead of the KeymanWeb interface. The keyboards in the selector are populated from the KeymanWeb list of keyboards. Please click [this link](__full-manual-control.html) to open the test page.

## Code Walkthrough

Include the following script in the HEAD of your page:

```js
<script>
  var KWControl = null;

  /* SetupDocument: Called when the page finishes loading */
  function SetupDocument()
  {
    keyman.init().then(function(){
      // Load the keyboards of your choice here.
      loadKeyboards();

      KWControl = document.getElementById('KWControl');
      var kbds = keyman.getKeyboards();
      for(var kbd in kbds)
      {
        var opt = document.createElement('OPTION');
        opt.value = kbds[kbd].InternalName + "$$" + kbds[kbd].LanguageCode;
        opt.innerHTML = kbds[kbd].Name;
        KWControl.appendChild(opt);
      }
      document.f.multilingual.focus();

      keyman.setActiveKeyboard('', '');
    });
  }

  /* KWControlChange: Called when user selects an item in the KWControl SELECT */
  function KWControlChange()
  {
    /* Select the keyboard in KeymanWeb */
    var name = KWControl.value.substr(0, KWControl.value.indexOf("$$"));
    var languageCode = KWControl.value.substr(KWControl.value.indexOf("$$"+2));
    keyman.setActiveKeyboard(name, languageCode);
    /* Focus onto the multilingual field in the form */
    document.f.multilingual.focus();
  }
</script>
```

Also include the following HTML code:

```html
<head>
    <!-- Load the KeymanWeb engine -->
    <script src="keymanweb.js" type="text/javascript"></script>
    <!-- Load the your keyboard stubs here -->
    <script src="unified_loader.js" type="text/javascript"></script>
    <!-- ... -->
</head>

<!-- When the page has finished loading, populate the keyboard selector, see above -->
<body onload="SetupDocument()">
```

- File: [unified_loader.js](js/unified_loader.js)

And finally, include the keyboard SELECT and the clickable help img:

```html
<!-- Display the KWControl selector with different keyboards listed -->
<p>Keyboard: <select id='KWControl' onchange='KWControlChange()'><option value=''>English</option></select>
```

On programmatically setting the keyboard:
- [`keyman.setActiveKeyboard()`](../../reference/core/setActiveKeyboard).

On getting KeymanWeb's managed list of keyboards:
- [`keyman.getKeyboards()`](../../reference/core/getKeyboards).

------------------------------------------------------------------------

[On to Control by Control Example](control-by-control)
