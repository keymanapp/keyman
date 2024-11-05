---
title: ResourceFileManager class
---

## Summary

The **ResourceFileManager** class manages and installs keyboards and lexical models

## Syntax

```swift
ResourceFileManager.shared.methodName()
```

## Description

The ResourceFileManager is the core class which provides most of the
methods needed to install and manage any keyboard and lexical model
resources you wish to use with Keyman Engine.

With the move to use of package files in 14.0, which contain metadata
about your keyboards and lexical models, far less resource metadata
needs to be specified within your app in comparison to earlier versions
of the engine.

## Field

[`installedPackages: [KeymanPackage]`](installedPackages)
:   Lists packages currently installed within the KeymanEngine.

## Methods

[`getInstalledPackage(for: Resource) -> Resource.Package`](getInstalledPackage)
:   Retrieves the installed package for this resource, if available.

[`install(resourceWithID: LanguageResourceFullID, from: KeymanPackage)`](install)
:   Installs specified resources from opened package files for use within the KeymanEngine.

[`prepareKMPInstall(from: URL)`](prepareKMPInstall)
:   Given a local filesystem URL for a KMP package, this function imports, opens, and parses the file for use within KeymanEngine

## History

- Added in Keyman Engine for iPhone and iPad 14.0.
