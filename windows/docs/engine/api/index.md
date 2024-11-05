---
title: Keyman Engine for Windows 14.0 API
---

## Introduction

The Keyman Engine for Windows 18.0 API is implemented in COM. It can be
instantiated with `CreateObject("keymanapi.Keyman")`.

**Note:** This documentation applies to Keyman Engine for Windows
versions 14.0 and up.

## Interface Hierarchy

`IDispatch`

> [`IKeymanObject`](IKeymanObject)
>
> > [`IKeyman`](IKeyman)
> > [`IKeymanCollection`](IKeymanCollection)
> >
> > > [`IKeymanErrors`](IKeymanErrors)
> > > [`IKeymanHotkeys`](IKeymanHotkeys)
> > > [`IKeymanKeyboardLanguages`](IKeymanKeyboardLanguages)
> > >
> > > > [`IKeymanKeyboardLanguagesFile`](IKeymanKeyboardLanguagesFile)
> > > > [`IKeymanKeyboardLanguagesInstalled`](IKeymanKeyboardLanguagesInstalled)
> > > >
> > > > > [`IKeymanKeyboardLanguagesInstalled2`](IKeymanKeyboardLanguagesInstalled2)
> > >
> > > [`IKeymanKeyboardOptions`](IKeymanKeyboardOptions)
> > > [`IKeymanKeyboards`](IKeymanKeyboards)
> > >
> > > > [`IKeymanKeyboardsInstalled`](IKeymanKeyboardsInstalled)
> > > >
> > > > > [`IKeymanKeyboardsInstalled2`](IKeymanKeyboardsInstalled2)
> > > >
> > > > [`IKeymanPackageContentKeyboards`](IKeymanPackageContentKeyboards)
> > >
> > > [`IKeymanLanguages`](IKeymanLanguages)
> > > [`IKeymanOptions`](IKeymanOptions)
> > > [`IKeymanPackageContentFiles`](IKeymanPackageContentFiles)
> > > [`IKeymanPackageContentFonts`](IKeymanPackageContentFonts)
> > > [`IKeymanPackagesInstalled`](IKeymanPackagesInstalled)
> > >
> > > > [`IKeymanPackagesInstalled2`](IKeymanPackagesInstalled2)
> >
> > [`IKeymanControl`](IKeymanControl)
> > [`IKeymanError`](IKeymanError)
> > [`IKeymanHotkey`](IKeymanHotkey)
> > [`IKeymanKeyboard`](IKeymanKeyboard)
> >
> > > [`IKeymanKeyboardFile`](IKeymanKeyboardFile)
> > >
> > > > [`IKeymanKeyboardFile2`](IKeymanKeyboardFile2)
> > >
> > > [`IKeymanKeyboardInstalled`](IKeymanKeyboardInstalled)
> >
> > [`IKeymanKeyboardLanguage`](IKeymanKeyboardLanguage)
> >
> > > [`IKeymanKeyboardLanguageInstalled`](IKeymanKeyboardLanguageInstalled)
> > >
> > > > [`IKeymanKeyboardLanguageInstalled2`](IKeymanKeyboardLanguageInstalled2)
> >
> > [`IKeymanKeyboardOption`](IKeymanKeyboardOption)
> > [`IKeymanLanguage`](IKeymanLanguage)
> > [`IKeymanOption`](IKeymanOption)
> > [`IKeymanPackage`](IKeymanPackage)
> >
> > > [`IKeymanPackageFile`](IKeymanPackageFile)
> > >
> > > > [`IKeymanPackageFile2`](IKeymanPackageFile2)
> > >
> > > [`IKeymanPackageInstalled`](IKeymanPackageInstalled)
> >
> > [`IKeymanPackageContentFile`](IKeymanPackageContentFile)
> > [`IKeymanPackageContentFont`](IKeymanPackageContentFont)
> > [`IKeymanSystemInfo`](IKeymanSystemInfo)
> > [`IKeymanVisualKeyboard`](IKeymanVisualKeyboard)
> > [`IKeymanBCP47Canonicalization`](IKeymanBCP47Canonicalization)


