---
title: DLL Exports
---

The DLL is called from Keyman with `LoadLibrary()`. All functions are
then found with `GetProcAddress()`. You must ensure that the function
exports do not have ordinals encoded in the names. The best way to
accomplish this in C/C++ is to use a .def file.

## DLL group function exports

The function declaration for the DLL group function is:

```c
BOOL WINAPI KeyEvent(HWND hwndFocus, WORD KeyStroke, WCHAR KeyChar, DWORD ShiftFlags);
```

Note that `KeyEvent()` is a placeholder for any name that you wish to
use. You can have multiple exports for Keyman use in a single DLL.

|            |       |
|------------|-------|
| hwndFocus  | The currently focused window. You will probably never have a need to use this. |
| KeyStroke  | The virtual key code for the current key. |
| KeyChar    |  The character code for the current key (based on US English layout). <br/> This will be `0` if _KeyStroke_ does not generate a character (e.g. function keys). |
| ShiftFlags |The shift state for the current key. The following shift states are possible:<br/> (see the table below) |

|     Flag     |   Value  | Description                            |
|--------------|----------|----------------------------------------|
| LCTRLFLAG    | `0x0001` | Left Control Flag                      |
| RCTRLFLAG    | `0x0002` | Right Control Flag                     |
| LALTFLAG     | `0x0005` | Left Alt Flag                          |
| RALTFLAG     | `0x0008` | Right Alt Flag                         |
| K_SHIFTFLAG  | `0x0010` | Shift flag                             |
| K_CTRLFLAG   | `0x0020` | Ctrl flag (unused here; see l/r flags) |
| K_ALTFLAG    | `0x0040` | Alt flag (unused here; see l/r flags)  |
| CAPITALFLAG  | `0x0100` | Caps lock on                           |
| NUMLOCKFLAG  | `0x0400` | Num lock on                            |
| SCROLLFLAG   | `0x1000` | Scroll lock on                         |
| ISVIRTUALKEY | `0x4000` | It is a Virtual Key Sequence           |

## Optional DLL Exports

Keyman recognises a number of other exports, if they are defined in the
DLL. None of these are required. These functions will be called when a
keyboard that references the DLL is manipulated. They will not be called
for keyboards that do not reference the DLL.

The following exports are available:

### KeymanIMInit

```c
BOOL WINAPI KeymanIMInit(PSTR keyboardname);
```

`KeymanIMInit()` is called once when the keyboard identified by
`keyboardname` is loaded for a given process. It is called for each
process in which the keyboard is loaded.

### KeymanIMDestroy

```c
BOOL WINAPI KeymanIMDestroy(PSTR keyboardname);
```

This is called once when the keyboard identified by `keyboardname` is
unloaded in a given process. It is called when the process exits
normally, or when Keyman refreshes its keyboard list after keyboards are
added or removed. If the keyboard is subsequently reloaded,
`KeymanIMInit()` will be called again.

### KeymanIMActivate

```c
BOOL WINAPI KeymanIMActivate(PSTR keyboardname);
```

This function is called whenever the user or a program activates the
keyboard. It is never called before `KeymanIMInit()` (unless
`KeymanIMInit()` is not exported). This is an appropriate place to
switch on permanently-visible IMC windows. `KeymanIMActivate()` can also
be called when switching processes and the target process has a related
keyboard already active.

### KeymanIMDeactivate

```c
BOOL WINAPI KeymanIMDeactivate(PSTR keyboardname);
```

This function is called when the user or a program switches off a
related keyboard. It is always called before `KeymanIMActivate()` for
the next keyboard. It will also be called when the user activates
another process, to give the DLL a chance to hide top-most IMC windows.

### KeymanIMConfigure

```c
BOOL WINAPI KeymanIMConfigure(PSTR keyboardname, HWND hwndParent);
```

`KeymanIMConfigure()` is called when the user clicks the Configure
button in Keyman Configuration to configure the DLL-specific
functionality for the keyboard. The appropriate behaviour is to display
a dialog box, and save the settings in the registry.

This function is separate from all the other functions. It can be called
when there are no keyboards loaded, or even if Keyman itself is not
loaded. You should not attempt to call Keyman32.dll from this function.

## See also

-  [DLL Interface for Keyman - Introduction](index)
-  [The imlib.cpp library module](imxlib)