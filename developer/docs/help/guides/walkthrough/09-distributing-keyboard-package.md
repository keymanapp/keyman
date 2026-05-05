---
title: Distributing a Keyboard Package
---

<link href='walkthrough.css' rel='stylesheet'>
<div class="walkthrough-navigation" markdown="1">
Part 9 of the [Keyman Developer Walkthrough](.).

[← Part 8 - Preparing a Keyboard Package](08-preparing-keyboard-package) &nbsp; [Part 10 - Generating a Lexical Model →](10-generating-lexical-model)
</div>

## Step-by-Step

For the Step-by-Step tutorial, there’s really nothing to do for this page. This tutorial is for learning only, therefore you don't want to submit this keyboard to the Keyman keyboards repository (since it already contains a keyboard by that name). If you like, you could look at the other sections on this page before continuing the Step-by-Step tutorial (or come back to them later).

<div class="walkthrough-navigation" markdown="1">
To continue the Step-by-Step tutorial move to the next page: [Part 10 - Generating a Lexical Model](10-generating-lexical-model)
</div>

---

## Submitting a keyboard to the Keyman Keyboards Repository

Follow the instructions at [Working with the Keyman Cloud Keyboard Repository](https://help.keyman.com/developer/keyboards/) to submit your keyboard to the Keyman keyboards repository. This will enable anyone with Internet access to download and use the keyboard. (Please do not submit the Dagbani keyboard used in the Step-by-Step tutorial. There is already a Dagbani keyboard in the repository.)

One requirement for keyboards that are submitted to the Keyman keyboards repository is the addition of a PHP help file that provides the online help available on the keyman.com site. See the **Editing the PHP help file** section of the [Writing Keyboard Documentation](07-writing-keyboard-documentation#toc-editing-the-php-help-file) page for more details.

## Other Distribution Methods

A keyboard package is contained in a file with the file type `.kmp`. The package contains everything needed for any of the supported platforms to run the keyboard. If you want to test a keyboard package, it is possible to transfer the `.kmp` file to a device with Keyman installed and use Keyman on that device to install the keyboard.

With Keyman Developer running, it’s possible to select `Packaging`, and the `.kps` file, then the `Build` tab, and then select the `Test package on web`. This will display a number of possible Internet addresses where the keyboard could be downloaded over a local network to a device running Keyman. There are also options that will install the package on the local machine running Keyman Developer.

The Keyboard App Builder (KAB) lets you put a Keyman keyboard (and optionally a lexical model) into an app for iOS or Android, which can be distributed. More details can be found on the [Keyboard App Builder](https://software.sil.org/keyboardappbuilder/) page.

<div class="walkthrough-navigation" markdown="1">
[← Part 8 - Preparing a Keyboard Package](08-preparing-keyboard-package) &nbsp; [Part 10 - Generating a Lexical Model →](10-generating-lexical-model)
</div>