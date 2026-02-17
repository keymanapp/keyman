# Using the Debugger

<link href='walkthrough.css' rel='stylesheet'>
<div class="walkthrough-navigation" markdown="1">
Part 4 of the [Keyman Developer Walkthrough](.).

[← Part 3 - Creating a Desktop Layout](03-creating-desktop-layout) &nbsp; [Part 5 - Designing a Touch Layout →](05-designing-touch-layout)
</div>

## Step-by-Step

- From the `Debug` menu, select the `Start Debugging` menu item. Some extra fields will be shown.
- Type <kbd>;</kbd>. The Keystroke log will show `;`. The display to the right has two sections. The upper section shows the characters displayed on the screen. The lower section shows a table of characters and their Unicode values. Here the upper section will show `;` and the lower section will show `;` and `U+003B`. (U+003B is the Unicode value for “;”.)
- Type <kbd>e</kbd>. The Keystroke log will show `;` and `E`. The display to the right will show `ɛ` in the upper section and `ɛ` and `U+025B` in the lower section. (U+025B is the Unicode value for “ɛ”.)
- Type <kbd>;</kbd> then <kbd>E</kbd>. The Keystroke log will show `;` `E` `;` `Shift+E`. The display to the right will show `ɛƐ` in the upper section and `ɛ` and `U+025B` then `Ɛ` and `U+0190` in the lower section.

![](images/debugger-output.png)

- From the `Debug` menu, select the `Stop Debugger` menu item.

<div class="walkthrough-navigation" markdown="1">
To continue the Step-by-Step tutorial move to the next page: [Part 5 - Designing a Touch Layout](05-designing-touch-layout)
</div>

---

## Running the Debugger

See the [debugger documentation](../../../context/debug) for more information on running the debugger.

## Interpreting Errors

When compiling your Keyman code, Keyman Developer will report any errors, warnings and hints in the box at the bottom of the window. The message will give a short description of what the compiler found was wrong along with the line number of where the problem was detected. The error, warning or hint has an identifier (“KM02033” for example). This identifier is a link to a more detailed description.

<div class="walkthrough-navigation" markdown="1">
[← Part 3 - Creating a Desktop Layout](03-creating-desktop-layout) &nbsp; [Part 5 - Designing a Touch Layout →](05-designing-touch-layout)
</div>