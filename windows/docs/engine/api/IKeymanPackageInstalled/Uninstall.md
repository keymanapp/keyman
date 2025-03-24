---
title: IKeymanPackageInstalled::Uninstall Method
---

## Introduction

The `IKeymanPackageInstalled::Uninstall` method uninstalls the package
and its contained keyboards and related artefacts from Keyman Engine for
Windows. This includes deleting the files from the Keyman Engine package
store, removing registry settings, and removing Windows input methods.

Changes associated with calling this method are applied immediately;
[`IKeyman::Apply`](../IKeyman/Apply) does not need to be called.
However, changes to the Windows input methods are applied
asynchronously.

This method requires elevated privileges.

## Specification

``` clike
void Uninstall(bool RemoveFonts)
```

## Parameters

`RemoveFonts`
:   If set to `True`, fonts that were installed with the package are
    removed from the system; otherwise, the fonts remain on the system.
