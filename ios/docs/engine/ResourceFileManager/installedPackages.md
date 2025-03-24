---
title: .installedPackages()
---

## Summary

The **`ResourceFileManager.installedPackages`** field enumerates all packages installed into the KeymanEngine.

## Syntax

```swift
ResourceFileManager.shared.installedPackages: [ KeymanPackage ]
```

### Value

An array of `KeyboardKeymanPackage` and `LexicalModelKeymanPackage` instances.

## History

- Added in Keyman Engine for iPhone and iPad 14.0.

## See also

- [`getInstalledPackage<Resource: LanguageResource>(for: Resource) -> Resource.Package?`](getInstalledPackage)` `
:   Finds the installed package for an already-installed resource.
