---
title: Getting Started With KeymanWeb
---

## Prerequisite

-  First, if you have not yet obtained a copy of KeymanWeb, please visit [the KeymanWeb Developer Home](https://keyman.com/developer/keymanweb/). A link to the "https minified" version of the code will be used here.

## Demo

To setup a basic first page with KeymanWeb, only two lines of code are necessary, but a few more lines will be shown here. Open [an example page](examples/__first-example.html).

The source code for the page may be seen below.

```html
<!DOCTYPE html>
<html lang="en">
<head>
  <meta charset="UTF-8">
  <title>KeymanWeb - A First Example</title>

  <script src='https://s.keyman.com/kmw/engine/17.0.330/keymanweb.js'></script>
  <script src='https://s.keyman.com/kmw/engine/17.0.330/kmwuitoggle.js'></script>
  <script>
    (function() {
      keyman.init({attachType:'auto'}).then(function() {
        keyman.addKeyboards('@en'); // Loads default English keyboard from Keyman Cloud (CDN)
        keyman.addKeyboards('@th'); // Loads default Thai keyboard from Keyman Cloud (CDN)
      });
    })(keyman);
  </script>
</head>
<body>
  <p>Click me and type: <input placeholder="Hello World"/></p>
</body>
</html>
```

## The Breakdown

- The `<script>` inclusion `<script src='https://s.keyman.com/kmw/engine/17.0.330/keymanweb.js'></script>` loads the Keyman Engine for Web script for the page.

- `(function() { keyman.init(); });` serves to initialize the web engine with default settings. By adding the object `{attachType:'auto'}` as a parameter to our `keyman.init()` call, the KeymanWeb engine will then link into any detected input elements automatically, regardless of browser or device, as part of its initialization.

- The other `<script>` inclusion, `<script src='https://s.keyman.com/kmw/engine/17.0.330/kmwuitoggle.js'></script>`, creates the language menu seen on non-mobile devices and the on-screen keyboard toggle button. For other options, see our [User Interface Design](user-interface-design) page.

- The calls to `keyman.addKeyboards()` in the source above link in the two example Keyman keyboards used in this demo from our CDN server. For more info on how to include keyboards in your KeymanWeb installation, check the [Adding Keyboards](adding-keyboards) page.

## API References

- On initialization: [`keyman.init()`](../reference/core/init).

- On including keyboards: [`keyman.addKeyboards()`](../reference/core/addKeyboards).
