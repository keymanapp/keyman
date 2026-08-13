---
title: build (deprecated)
---

## Summary

Returns the patch component of the KeymanWeb version (major.minor.patch) as a number.

## Syntax

```js
keyman.build
```

### Type

number

### Access

Read only

### Return Value

The patch component of the version, e.g. if the version is `"18.0.249"`, then returns `249`.

## Description

This function is deprecated; to get the full KeymanWeb version string, use [`keyman.versionInfo`](versionInfo).

## History

19.0: deprecated

## See also

* [`keyman.version`](version)
* [`keyman.versionInfo`](versionInfo)
