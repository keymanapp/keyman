---
title: .getInstalledPackage(for:)
---

## Summary

The **`getInstalledPackage<Resource: LanguageResource>()`** method is used to install resources from opened package files.

## Syntax

```swift
ResourceFileManager.shared.getInstalledPackage<Resource: LanguageResource>(for: Resource)
```

### Parameters

`for`
:   Either a [`InstalledKeyboard`](../LanguageResource) or a [`InstalledLexicalModel`](../LanguageResource) that has been previously installed.

### Returns

The package, as currently installed. May return `nil` if the specified resource is not currently installed.

## History

- Added in Keyman Engine for iPhone and iPad 14.0.

## See also

- [`installedPackages: [KeymanPackage]`](installedPackages)
:   Returns an array of all currently-installed packages.
