---
title: .pageURL(for:)
---

## Summary

The **`KeymanPackage.pageURL(for: KeymanPackagePage)`** method provides links to web pages contained within a package.

## Syntax

``` swift
KeymanPackage.pageURL(for: KeymanPackagePage) -> URL?
```

### Parameters

`for: KeymanPackagePage`
:   An enum representing the type of page to retrieve.  

    Supported pages: `.readme`, `.welcome`, `.custom(bundledPath: String)`, the latter of which may target arbitrary locations within the package.  
      
    May return `nil` if the corresponding file does not exist.

### Value

A `URL` that may be used to display the page within a `WKWebView` or similar module.

## History

Added in Keyman Engine for iPhone and iPad 14.0.

## See also

- [`PackageWebViewController`](../PackageWebViewController)
:   Used to display web pages contained within a package.
