---
title: Preparing a Keyboard Package
---

<link href='walkthrough.css' rel='stylesheet'>
<div class="walkthrough-navigation" markdown="1">
Part 8 of the [Keyman Developer Walkthrough](.).

[← Part 7 - Writing Keyboard Documentation](07-writing-keyboard-documentation) &nbsp; [Part 9 - Distributing a Keyboard Package →](09-distributing-keyboard-package)
</div>

## Step-by-Step

- Select the `dagbani.kpj` project filename at the top (just under the menu and toolbar).
- Select the `Packaging` tab at the bottom, then the `dagbani.kps` file. This will open the Keyboard Package Source file and display some tabs down the left side of the window that will help you manage the content. Most of the information has been set up for you when you first created the keyboard project.
    - The `Files` tab lists the files that will be included in the package.
    - The `Keyboards` tab lists the language codes supported by this keyboard.
- Select the `Details` tab.
- If the value in the `Welcome file` field is `(none)`, then select that field and select `welcome.htm` from the drop-down list.
- Select `File` in the menu bar, then `Save` to save the changes. (The <kbd>Ctrl</kbd>+<kbd>S</kbd> shortcut also works.)
- Select the `Build` tab, then the `Compile Package` button. A **dagbani.kps built successfully** message should be displayed.
- Congratulations! You have successfully built and packaged a keyboard for the Dagbani language.

<div class="walkthrough-navigation" markdown="1">
To continue the Step-by-Step tutorial move to the next page: [Part 9 - Distributing a Keyboard Package](09-distributing-keyboard-package)
</div>

---

## Package Files

See the [Keyman Package Editor documentation](../../../context/package-editor) if you need more details on the following items.

### Files

In addition to the files that are included in the package by default when you create the keyboard project with the New Project feature, you have the option of adding additional files to the package. Files that might be added:

- keyboard image files referenced by the welcome.htm help file
- fonts
- an image file (see under Details)

### Keyboards

Enter a language tag (that is, a BCP 47 tag) for each language supported by the keyboard.

Optionally, you can specify an example of a word the keyboard produces and the keys used to produce it.

If you include fonts in the package, you can specify that Keyman use them for displaying the on-screen keyboard or the touch keyboard.

### Details

Here is where you can enter information for:

- Package name: Initially set to the name of the keyboard, so usually won’t need to change
- Welcome file: normally set to `welcome.htm`
- Readme file: normally set to `readme.htm`
- License file: normally set to `LICENSE.md`
- Version: Recommendation is to tick the `Package version follows keyboard version` box and leave the version field blank
- Copyright: normally established in the New Project dialog, but you can make needed changes here. Current practice is to omit the copyright year from this field (since it is in the [LICENSE.md](http://LICENSE.md) file).
- Author, Email address, Web Site: You can include as much or as little information about the author as you wish
- Image file: If you supply a 140 pixel wide x 250 pixel high image in JPEG or PNG format, then your custom image will be shown during installation (in place of the default Keyman image). Your image file must be added to the Files list before it will be available to be selected here.
- Related packages: If your keyboard is part of a group of keyboards, you can specify which other keyboards are part of the group. (For example, you might have three keyboards for the same language, one based on the QWERTY layout, one AZERTY, one QWERTZ, and you could use this field to note the relationship between them.) You can also use this feature to deprecate another keyboard that your keyboard supersedes.

<div class="walkthrough-navigation" markdown="1">
[← Part 7 - Writing Keyboard Documentation](07-writing-keyboard-documentation) &nbsp; [Part 9 - Distributing a Keyboard Package →](09-distributing-keyboard-package)
</div>