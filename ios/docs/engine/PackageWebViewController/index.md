---
title: PackageWebViewController class
---

## Summary

The **PackageWebViewController** class is used to display web pages
contained within packages, ejecting any links outside the package into
an external browser.

## Syntax

```swift
class PackageWebViewController: UIViewController
```

## Description

The PackageWebViewController class is used to display .html contents
bundled within packages. Links that lead outside the package are
supported, but will be opened externally.

At this time, we directly support opening two types of common pages:
`.readme` and `.welcome`. We also provide `.custom(bundledPath: String)`
to allow use of any custom pages you may wish to embed within your
packages for use within your app.

## Constructor

`public init?(for package: `[`KeymanPackage`](../KeymanPackage)`, page: `[`KeymanPackagePage`](../KeymanPackage/pageURL)
:   Retrieves the corresponding page from the package and prepares a view to display it.

## History

Added in Keyman Engine for iPhone and iPad 14.0.

## See Also

- [`KeymanPackage.pageURL`](../KeymanPackage/pageURL)
:   Retrieves a `URL` for a .html page contained within a package.
