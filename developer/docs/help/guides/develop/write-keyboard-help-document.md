---
title: Write a Keyman Keyboard help document
---

This is a guide to "what to put in a keyboard documentation", "how to start composing a PHP help file" from creating a file to a fully written documentation. 

A keyboard help documentation (PHP help file) is a structured help web page about all of the information: 
* Keyboard and language description
* How to use the keyboard
* Graphics for the Keyboard layouts for all platforms, and more. 

The help documentation acts as an online help documentation on help.keyman.com, not to be confused with [welcome.htm and readme.htm](../distribute/tutorial/step-2). However, the help file can be a descriptive version of the `welcome.htm` file; see [here](https://help.keyman.com/developer/keyboards/phphelpfile).

After planning and developing a keyboard in Keyman Developer, consider following the project's [folder and file structure](../../reference/file-layout). The help file should be stored in an independent folder within the source folder called "**help**". The file must have the same name as the keyboard folder name:

```
my_keyboard
|
|
|____source
|    |      files
|    |______help
|           |
|           |____my_keyboard.php
|
|____other folders
```
It would end up in this location: `m/my_keyboard/source/help/my_keyboard.php`. This allows the Keyman online help file to deploy correctly.

In the `.php` file, include the main code:

```php
<?php 
  $pagename = 'Project Name Keyboard Help';
  $pagetitle = $pagename;
  require_once('header.php');
?>

<!-- Write anything related to the keyboard here -->
<div>
  <h2>Desktop Keyboard layouts</h2>
  <div id='osk' data-states='default shift rightalt rightalt-shift'>
      <!-- By including this tag, the images of each layers for the 
      On-screen keyboard will be placed here -->
  </div>

  <h2>Touch Keyboard layouts</h2>
  <div id='osk-phone' data-states='default shift numeric symbol'>
      <!-- By including this tag, the images of each layers for the 
      Touch keyboard will be placed here -->
  </div>
</div>

```

Please feel free to adjust the `data-states` according to the keyboard layers; [see here](https://help.keyman.com/developer/keyboards/phphelpfile#toc-dynamically-constructed-keyboard-images).

### Keep in mind

* Some readers are visual learners, and they might be pleased to find a documentation that has more to the eyes than plain text. Check [Khmer (SIL) Keyboard Help](https://help.keyman.com/keyboard/sil_khmer/) for minimal design of keys (`<kbd>`) and one of the many methods to showcase key combinations.

* Since the PHP file is already using the `id` and `data-states` attributes (above), and to avoid unnecessary duplication, please do not use the following HTML tags:

  ```html
  <h3>Desktop Default</h3>
  <p>
      <a href="LayoutU_.png"><img class="keyboard" src="LayoutU_.png" alt="Default (unshifted) state" /></a>
  </p>
  <h3>Shift</h3>
  <p>
      <a href="LayoutU_S.png"><img class="keyboard" src="LayoutU_S.png" alt="Shift state" />
  </p>
  <h3>Touch Default</h3>
  <p>
      <a href="TouchLayoutU.png"><img class="keyboard" src="TouchLayoutU_.png" alt="Touch default (unshifted) state" />
  </p>
  ```

* Additionally the images of the On-screen keyboard are not require to be exported anymore when `data-states` property is specified in the PHP file, unless the images are referred to in the `welcome.htm` page.

### Requested features for keyboard help documentation

One way to ask a question or request a feature for the keyboard help documentation is to reach out on [Keyman community forum](https://community.software.sil.org/c/keyman/) or [create a feature request/bug report](https://github.com/keymanapp/keyman/issues/new/choose). However, please look out for an existing request to avoid duplication.

Follow the progress for each feature requests [here](https://github.com/keymanapp/keyboards/issues?q=is%3Aissue%20state%3Aopen%20label%3Afeat).

### See also
* The [GFF አማርኛ (Amharic) Keyboard Help](https://help.keyman.com/keyboard/gff_amharic/). (Toggle help for each platforms)
* [CSS Formatting](https://help.keyman.com/developer/keyboards/phphelpfile#toc-css-formatting) from [The BJCreeUNI (East) Keyboard Help](https://help.keyman.com/keyboard/bj_cree_east).
* Follow the [library of keyboard documentation](../../../../keyboard/).