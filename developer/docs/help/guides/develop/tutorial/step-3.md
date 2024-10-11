---
title: Step 3: The Keyboard Header
---

## Comments

```keyman
c Simplified French Keyboard for Keyman 9.0
```

Most of the header in this example is made up of
[comments](/developer/language/guide/comments). A comment is used
to make notes about the keyboard, or to provide information on the
workings of the keyboard. The comments are readable by anyone looking at
the source code of the keyboard.

A comment always starts with a lowercase `c`, followed by one or more
spaces, and continues to the end of the line. Keyman Developer will
ignore comments when compiling a keyboard.

Comments can take up a whole line, or can start in the middle of the
line. The latter is useful for making short notes about individual
lines. As you can see we have used both kinds of comments in the header.

## The `&Version` store

```keyman
store(&Version) "9.0"             c This keyboard is for use with Keyman 9.0
```

The [`&Version` store](/developer/language/reference/version)
identifies the Keyman version for which this keyboard was written; this
keyboard is for use with Keyman 9.0. The `&Version` store is an optional
part of the keyboard header, but if present, it should be the first
store in the file.

## The `&Name` store

```keyman
store(&Name)    "Quick French"
```

The [`&Name` store](/developer/language/reference/name) specifies
a descriptive name for the keyboard, which can be up to eighty
characters long. The name we have given to this keyboard is
`"Quick French"`. The `&Name` store is not required but is highly
recommended!

## The `&Bitmap` store

```keyman
store(&Bitmap)  "qfrench.ico"
```

The optional [`&Bitmap` store](/developer/language/reference/bitmap)
tells Keyman which image to use for the keyboard's icon. The picture
should be in the standard Windows .ico format, and should contain at
least a single 16x16 pixel image. It can also contain higher resolution
images for high resolution "High DPI" displays. If you use a modern icon
editor, the icon can use alpha transparency.
For this keyboard we will be using the following bitmap:
![](../../../images/tutorial_keyboard_qfrench.gif); it is
found in the Keyman Developer folder, under
`Samples\Examples\qfrench.ico` - you should copy it into the same folder
in which you will save your keyboard.

## The `&MnemonicLayout` store

```keyman
store(&MnemonicLayout) "1"
```

The [`&MnemonicLayout` store](/developer/language/reference/mnemoniclayout) tells Keyman that
the layout is meant to conform to the user's keyboard layout; for
example, if the user presses the quote key <kbd>'</kbd> on
their keyboard (whether they are using a US English, UK English, French,
German, Swedish, or other keyboard) it should work in the same way. The
opposite of this is a positional layout (which is the default if this
store omitted), which is intended for keyboards for which there is not
necessarily a correspondence between what is printed on the physical
keyboard and what is output when that key is pressed.

## The `begin` statement

```keyman
begin Unicode > use(Main)
```

The [`begin` statement](/developer/language/reference/begin) tells
Keyman which group of rules to process first when it receives a
keystroke. The use of multiple groups is an advanced feature, and
unnecessary for this tutorial, so we will use a single group, called
`Main`. The `begin` statement is required in every keyboard, and marks the
start of the keyboard body. The `begin` statement also tells Keyman
which encoding to use for the keyboard. Nearly all keyboards will use
`Unicode`, today.

-   [Continue with Step 4: The Keyboard Body](step-4)
-   [Back to Step 2: Writing the Header](step-2)