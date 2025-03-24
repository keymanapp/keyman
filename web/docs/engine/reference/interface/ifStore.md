---
title: ifStore (KIFS)
---

## Summary

`ifStore` compares the content of a [system `store`](/developer/language/guide/stores#toc-system-stores) with a string value.

## Syntax

```c
keyman.interface.ifStore(systemId, strValue, Pelem);
```

or

```c
KeymanWeb.KIFS(systemId, strValue, Pelem);
```

### Parameters

`systemId`
:   Type: `number`
:   The ID of the system store to test.

`strValue`
:   Type: `string`
:   The string value to be used for comparison.

`Pelem`
:   Type: `Element`
:   The page element currently active. (This parameter exists for use by possible future extensions.)

### Return Value

`boolean`
:   `true` if the value matches that of the system store, otherwise `false`.

## Description

Only system IDs `31` (`platform`) and `33` (`layer`) are currently supported.

For `platform`, any combination of the following may be tested against the current device and browser:

-   Input method:
:   `['touch', 'hardware']`

-   any combination of OS:
:   `['windows', 'android', 'ios', 'macosx', 'linux']`

-   form factor:
:   `['desktop', 'tablet', 'phone']`

-   browser:
:   `['native', 'web', 'ie', 'chrome', 'firefox', 'safari', 'opera']`

Only one of each category may be matched at a time.

## See also

- [`keyman.interface.setStore()`](setStore)
