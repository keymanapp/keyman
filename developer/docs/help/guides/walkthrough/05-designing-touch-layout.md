---
title: Designing a Touch Layout
---

<link href='walkthrough.css' rel='stylesheet'>
<div class="walkthrough-navigation" markdown="1">
Part 5 of the [Keyman Developer Walkthrough](.).

[← Part 4 - Using the Debugger](04-using-debugger) &nbsp; [Next: Part 6 - Creating a Touch Layout →](06-creating-touch-layout)
</div>

## Step-by-Step

In the Dagbani keyboard, `;e` was selected as the way to generate the character `ɛ` , but having to type a `;` on a touch screen followed by `e` to get the `ɛ` character would be tedious. It is preferable to assign `ɛ` as a longpress key on the `e` key.

For the Step-by-Step tutorial, the design decisions have been made for you. We will be adding ten longpress keys to the default touch layout.

<div class="walkthrough-navigation" markdown="1">
To continue the Step-by-Step tutorial move to the next page: [Part 6 - Creating a Touch Layout](06-creating-touch-layout)
</div>

---

## Creating a Basic Visual Layout

Adding a touch keyboard layout to your keyboard project allows a user to input text on touch devices, such as tablets and phones.

A desktop keyboard is dependent on the physical keyboard. With a touch layout, you will be displaying the keys on the device for the user to see and interact with.

There is some flexibility in arranging keys, though adding too many keys to a row or too many rows to a layout will make the keyboard difficult to use. Additionally, there are options for activating a key:

- touch, that is touch and release the key
- longpress, that is touch and hold the key, then select a character from a popup menu
- flick, that is touch the key and slide in one of eight directions
- mulitap, that is rapidly touch and release the key: one tap produces the base key, two taps produces the first multi-tap key, three produces the second multi-tap key, etc.

## Planning Keystrokes

You’ll want to plan your touch layout just as you did for the desktop (physical keyboard) layout. There is some value in making the touch layout similar to the desktop layout, but the primary concern is making it easy to use.

Another possibility, if a small number of additional characters are needed, is to give each of them their own keys. This is possible since a touch layout is displayed on the screen.

If you use Keyman Developer’s New Project feature, a touch layout is added automatically.

## Using the desktop layout as a basis for the touch layout

There are advantages to basing the touch layout on the desktop layout.
Users who switch between the two may benefit from the similarity.
In addition, your touch layout may be able to take advantage of the logic in the desktop layout code you’ve already created.
Specifically, if the `ID` field for a key in the touch layout begins with `K_` then Keyman treats the key as the same as the corresponding desktop key (as modified by the active layer of the touch layout)
according to the rules that are in the .kmn file.

If you establish the desktop keyboard layout and create the On-Screen Keyboard,
you can switch to the Touch Editor and import from the OSK.
This will provide a “tablet” layout, with “default” and “shift” layers.
To switch to a “phone” layout, select the basic template, which rearranges the keys and moves some to a “numeric” layer.
(For more details, see the next topic.)
The result will need to be reviewed and tested, but using this approach may save you some time.

<div class="walkthrough-navigation" markdown="1">
[← Part 4 - Using the Debugger](04-using-debugger) &nbsp; [Part 6 - Creating a Touch Layout →](06-creating-touch-layout)
</div>