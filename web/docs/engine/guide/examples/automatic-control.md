---
title: Automatic Mode Example
---

This page shows how to include a local keyboard from an arbitrary location in your website's file structure.

In this example, we use only the LaoKey keyboard. Please click [this link](./__auto-control.html) to open the test page.

## Code Walkthrough

```html
<head>
  <!-- Start of Code -->
  <script src="js/keymanweb.js" type="text/javascript"></script>
  <script>
    window.addEventListener('load', function () {
      keyman.init().then(function() {
        keyman.addKeyboards({
          id:'laokeys',
          name:'Lao (Phonetic)',
          languages:{
            id:'lo',
            name:'Lao'
          },
          filename:'./js/laokeys.js'
        });
      });
    });
  </script>
  <!-- End of Code -->
</head>
```

As you can see above, the second line in the code snippet above references the LaoKey keyboard loader JavaScript file. This is a small stub file, typically less than 200 bytes, that defines the name and actual location of the real keyboard file (in this case, **laokeys.js**). When a page may reference many keyboards, this saves downloading potentially hundreds of kilobytes of unused Javascript keyboards - the keyboard is downloaded when it is first selected by the user.

## API References

On initialization: [`keyman.init()`](../../reference/core/init).

On including keyboards: [`keyman.addKeyboards()`](../../reference/core/addKeyboards).

------------------------------------------------------------------------

[On to Manual Control Example](manual-control)
