---
title: .installState(forPackage:)
---

## Summary

The **`installState()`** method is used to determine whether or not a package has been installed.

## Syntax

```swift
ResourceFileManager.shared.installState(forPackage: KeymanPackage.Key) -> KeymanPackage.InstallationState
```

### Parameter

`forPackage`
:   A [`KeymanPackage.Key`](../KeymanPackage/key) that uniquely identifies the package.

### Returns

Returns one of the following:

`.none`
:   The package has not been installed.

`.downloading`
:   KeymanEngine has been told to download the package, and the download is still pending.

`.pending`
:   The package has been downloaded and temporarily opened, but installation is still pending.

`.installed`
:   The package has been installed.

## History

- Added in Keyman Engine for iPhone and iPad 14.0.
