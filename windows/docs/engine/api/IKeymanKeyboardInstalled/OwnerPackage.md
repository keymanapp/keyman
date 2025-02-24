---
title: IKeymanKeyboardInstalled::OwnerPackage Property
---

## Introduction

The `IKeymanKeyboardInstalled::OwnerPackage` property returns an
[`IKeymanPackageInstalled`](../IKeymanPackageInstalled) reference for
the package which contained this keyboard when it was installed. If the
keyboard was installed standalone, then `OwnerPackage` returns `null`.

## Specification

``` clike
readonly IKeymanPackageInstalled* OwnerPackage
```
