---
title: Input Method Extensions
---

Keyman's multiple group processing is powerful, but sometimes you need
to be able to do something a bit more complex, such as a dictionary
lookup. Keyman's DLL interface let you do this. You can call a function
in a DLL in the same way as you call another group. The function can
read the context, deadkeys and the current keystroke, and output
characters, deadkeys, virtual keys, beeps and other items.

The DLL interface also allows you to create a popup Input Method
Composition (IMC) window. This window allows the user to select visually
the characters they are wishing to input. The window can be set to be
visible when the keyboard is active, or after an appropriate key
sequence. When the window is visible, it can be set to capture all
keyboard input, or be passive.

## File locations

The DLL should be placed in any of the following locations:

-  The same directory as the .kmx file (e.g. use a package to install it)
-  The Keyman program directory (same place as keyman32.dll)
-  Anywhere on the path (such as the Windows directory)

The best option is the first, as you can then include the DLL in a
Keyman package for easy installation and uninstallation.

Full x64 support for IMX DLLs was introduced in Keyman Desktop
8.0.333.0. An x64 version of the DLL may be included simply by giving it
the file extension .x64.dll, with the same base name as the x86 DLL.
Keyman will call the appropriate DLL.

## General usage information

DLL functions used in place of groups are called DLL group functions.

All strings, apart from keyboard names, are passed as WCHAR, regardless
of whether the active window is a Unicode window or not. In Keyman 8 and
earlier, ANSI characters are represented as 16-bit WCHAR, with high bits
zeroed out. In Keyman 9, ANSI keyboards are translated to Unicode at
install time, through the CP-1252 code page.

The DLL will be loaded for each process in which the keyboard is
activated. Remember that the DLL will not share memory between these
processes by default, so if you have large memory requirements, you
should use memory mapped files or possibly SHARDATA segments to minimize
the memory consumption.

DLL group functions are called in a fairly time-critical environment. It
is important that you minimise the processing time in these functions.
It is essential that you avoid any window focus or activation – message
boxes are definitely out of the question. For debugging purposes, there
is a Keyman32.dll function exported for writing to the logfile (see the
section titled Keyman32 imports).

DLLs can handle multiple keyboards at once. The keyboards are identified
by a name which is the filename of the keyboard, minus path and
extension. For example, given c:\keyman\imsample\imsample.kmx, the
keyboard name is imsample. These are the same names that Keyman uses
internally, for example in the registry and directory names.

## Registry settings

Parameters for the DLL can be stored in two locations in the registry.
They should always be stored under HKEY_CURRENT_USER, as the user will
not have permission to change machine-wide settings, and the settings
should not affect other users. The following locations are recommended:

> HKEY_CURRENT_USER\Software\Tavultesoft\Keyman Engine\9.0\IMX\\[DLLName]  

> HKEY_CURRENT_USER\Software\Tavultesoft\Keyman Engine\9.0\Installed Keyboards\\[kbdname]

The first key should be used for settings that pertain to any keyboard
associated with the DLL. The second key should be used for any
keyboard-specific settings. Values stored under the second key should be
prefixed with the name of the dll, so that they will not conflict with
Keyman or other dll values.

## The .kmn interface

Inside a .kmn file, you define the DLL group function interface as follows:

```keyman
store(DLLFunction) "myfile.dll:KeyEvent"
```

You can use this anywhere where you would place the use statement
(except in the begin statement), with the call statement. For example,

```keyman
+ 'a' > call(DLLFunction)
```

A single .kmn file can reference multiple DLL group functions, in a single or multiple DLLs.

## See also

-  [DLL Exports](imxdll)
-  [The imlib.cpp library module](imxlib)
