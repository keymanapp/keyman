---
title: Step 2: Writing the Header
---

## Overview of a keyboard file

A keyboard file is divided into two sections: the **header** and the
**rules** section. The header section defines the name of the keyboard,
its bitmap, and other general settings. The rules are used to define how
the keyboard responds to keystrokes from the user, and are divided into
groups. We will start by writing the header.

## The keyboard header

The keyboard header is the first part of a keyboard; it consists of
statements that help Keyman identify the keyboard and set default
options for it. Each statement in the header must be on a separate line.
While there is no technical requirement to put header statements at the
start of a keyboard source file, keeping them there helps you identify
them easily, and keeps them consistent with keyboard programs other
people might write.

We will begin to write the keyboard. If Keyman Developer is not already
running, start it now. Create a new keyboard file, navigate to the
**Layout** tab, and click on the **Code** tab at the bottom of the
screen to switch to the Code view.

Type or paste the following code into the keyboard file. This is the
header of our keyboard file.

```keyman
c Simplified French Keyboard for Keyman 9.0
c
c This keyboard program uses a simplified set of keys
c for typing French, especially for those who don't know the
c standard French keyboard.
c
c NOTE: This keyboard was created from the Keyman keyboard
c programming tutorial.

store(&Version) "9.0"             c This keyboard is for use with Keyman 9.0
store(&Name)    "Quick French"
store(&Bitmap)  "qfrench.ico"
store(&MnemonicLayout) "1"        c This keyboard uses a mnemonic layout.

begin Unicode > use(Main)
```

An explanation of the various parts of the header follows on the next
page.

-   [Continue with Step 3: The Keyboard Header](step-3)
-   [Back to Step 1: Planning the Keyboard](step-1)