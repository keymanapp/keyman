---
title: IKeymanPackage Interface
---

## Introduction

The `IKeymanPackage` interface describes a keyboard package in Keyman
Engine for Windows API. It is a base class and describes both installed
packages and package files. A keyboard package file is a bundle that
contains keyboard layout(s), visual keyboards, documentation, fonts and
any other related files to simplify distribution of keyboard layouts.

## Interface Hierarchy

`IDispatch`  

> [`IKeymanObject`](../IKeymanObject)  
>
> > **`IKeymanPackage`**  
> >
> > > [`IKeymanPackageFile`](../IKeymanPackageFile)  
> > >
> > > > [`IKeymanPackageFile2`](../IKeymanPackageFile2)  
> > >
> > > [`IKeymanPackageInstalled`](../IKeymanPackageInstalled)  

## Properties

[`Author`](Author) <span class="readonly">read only</span>
:   Returns the author as listed in the package file.

[`AuthorEmail`](AuthorEmail) <span class="readonly">read only</span>
:   Returns the email address of the author as listed in the package
    file.

[`Copyright`](Copyright) <span class="readonly">read only</span>
:   Returns the copyright message from the package file.

[`Filename`](Filename) <span class="readonly">read only</span>
:   Returns the fully-qualified filename of the package file.

[`Files`](Files) <span class="readonly">read only</span>
:   Returns an
    [`IKeymanPackageContentFiles`](../IKeymanPackageContentFiles)
    reference which lists the files included within the package.

[`Fonts`](Fonts) <span class="readonly">read only</span>
:   Returns an
    [`IKeymanPackageContentFonts`](../IKeymanPackageContentFonts)
    reference which lists the fonts included within the package.

[`Graphic`](Graphic) <span class="readonly">read only</span>
:   Returns an `IPicture` reference for the install screen graphic
    included within the package, or `null` if no install screen graphic
    was included.

[`GraphicFile`](GraphicFile) <span class="readonly">read only</span>
:   Returns an
    [`IKeymanPackageContentFile`](../IKeymanPackageContentFile)
    reference for the install screen graphic included within the
    package, or `null` if no install screen graphic was included.

[`ID`](ID) <span class="readonly">read only</span>
:   Returns the identifier for the package, which is the base name of
    the package file without extension.

[`Keyboards`](Keyboards) <span class="readonly">read only</span>
:   Returns an
    [`IKeymanPackageContentKeyboards`](../IKeymanPackageContentKeyboards)
    reference which lists the keyboard layouts included within the
    package.

[`Name`](Name) <span class="readonly">read only</span>
:   Returns the descriptive name of the package file.

[`ReadmeFile`](ReadmeFile) <span class="readonly">read only</span>
:   Returns an
    [`IKeymanPackageContentFile`](../IKeymanPackageContentFile)
    reference for the install Readme HTML file included within the
    package, or `null` if no Readme file was included.

[`Version`](Version) <span class="readonly">read only</span>
:   Returns the version string from the package file.

[`Website`](Website) <span class="readonly">read only</span>
:   Returns the website of the package as listed in the package file.

[`WelcomeFile`](WelcomeFile) <span class="readonly">read only</span>
:   Returns an
    [`IKeymanPackageContentFile`](../IKeymanPackageContentFile)
    reference for the welcome.htm documentation HTML file included
    within the package, or `null` if no documentation was included.
