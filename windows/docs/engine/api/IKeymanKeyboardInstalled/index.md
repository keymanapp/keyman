---
title: IKeymanKeyboardInstalled Interface
---

## Introduction

The `IKeymanKeyboardInstalled` interface describes a keyboard file that
has been installed and allows modification of [keyboard option
values](Options) for the keyboard file.

## Interface Hierarchy

`IDispatch`  

> [`IKeymanObject`](../IKeymanObject)  
>
> > [`IKeymanKeyboard`](../IKeymanKeyboard)  
> >
> > > **`IKeymanKeyboardInstalled`**  

## Properties

[`IconFilename`](IconFilename) <span class="readonly">read only</span>
:   Returns the string filename of the icon file associated with the
    keyboard. When the keyboard is installed the icon file is generated
    from the icon resource contained within the keyboard.

<!-- -->

[`KeymanID`](KeymanID) <span class="readonly">read only</span>
:   Returns the integer identifier for the keyboard used internally by
    Keyman Engine.

<!-- -->

[`Languages`](Languages) <span class="readonly">read only</span>
:   Returns an
    [`IKeymanKeyboardLanguagesInstalled`](../IKeymanKeyboardLanguagesInstalled)
    reference which lists the Windows languages with which the keyboard
    is currently associated.

<!-- -->

[`Loaded`](Loaded)
:   Determines whether the keyboard layout is selectable by the user.
    When unloaded, the keyboard is removed from the user's keyboard
    picker.

<!-- -->

[`Options`](Options) <span class="readonly">read only</span>
:   Returns an [`IKeymanKeyboardOptions`](../IKeymanKeyboardOptions)
    reference which lists the user-configurable options for the keyboard
    layout.

<!-- -->

[`OwnerPackage`](OwnerPackage) <span class="readonly">read only</span>
:   If the keyboard was installed as part of a package, returns the
    [`IKeymanPackageInstalled`](../IKeymanPackageInstalled) reference
    for the package; otherwise returns `null`.

<!-- -->

[`VisualKeyboard`](VisualKeyboard) <span class="readonly">read only</span>
:   If an associated visual keyboard is installed, returns the
    [`IKeymanVisualKeyboard`](../IKeymanVisualKeyboard) reference for
    the visual keyboard; otherwise returns `null`.

## Methods

[`InstallVisualKeyboard()`](InstallVisualKeyboard)
:   Installs a visual keyboard file into Keyman Engine for Windows and
    associates it with the keyboard.

<!-- -->

[`Uninstall()`](Uninstall)
:   Uninstalls the keyboard from Keyman Engine for Windows.
