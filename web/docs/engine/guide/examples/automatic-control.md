---
title: Automatic Mode Example
---

This page shows how to include a local keyboard from an arbitrary location
in your website's file structure.

In this example, we use only the LaoKey keyboard. Please click
[this link](./__auto-control.html) to open the test page.

## Code Walkthrough

```html
<head>
  <!-- Start of Code -->
  <script src="https://s.keyman.com/kmw/engine/17.0.331/keymanweb.js" type="text/javascript"></script>
  <script>
    keyman.init().then(async function() {
      await keyman.addKeyboards({
        id:'laokeys',
        name:'Lao (Phonetic)',
        languages:{
          id:'lo',
          name:'Lao'
        },
        filename:'./js/laokeys.js'
      });
    });
  </script>
  <!-- End of Code -->
</head>
```

## API References

On initialization: [`keyman.init()`](../../reference/core/init).

On including keyboards: [`keyman.addKeyboards()`](../../reference/core/addKeyboards).

------------------------------------------------------------------------

[On to Manual Control Example](manual-control)
