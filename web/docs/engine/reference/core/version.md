---
title: version (deprecated)
---

## Summary

Returns the major and minor components of the KeymanWeb version (major.minor.patch) as a string.

## Syntax

```js
keyman.version
```

### Type

string

### Access

Read only

### Return Value

The major and minor components of the version, e.g. if the version is `"18.0.249"`, then returns `"18.0"`.

## Description

This function is deprecated; to get the full KeymanWeb version string, use [`keyman.versionInfo`](versionInfo).

## History

19.0: deprecated

## See also

* [`keyman.build`](build)
* [`keyman.versionInfo`](versionInfo)
