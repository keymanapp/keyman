---
title: setStore (KSETS)
---

## Summary

`setStore` sets the value of a [system `store`](/developer/language/guide/stores#toc-system-stores) to a string.

## Syntax

```c
keyman.interface.setStore(systemId, strValue, Pelem);
```

or

```c
KeymanWeb.KSETS(systemId, strValue, Pelem); // Shorthand
```

### Parameters

`systemId`
:   Type: `number`
:   The ID of the system store to set. Only system ID `33` (`layer`) is currently supported.

`strValue`
:   Type: `string`
:   The string value to set as the system store's content.

`Pelem`
:   Type: `Element`
:   The page element currently active. (This parameter exists for use by possible future extensions.)

### Return Value

`boolean`
:   `true` if the operation succeeds, otherwise `false`.

## Description

This function is used to set the value of writeable system stores. Many system stores are readonly, containing metadata about an individual keyboard, so `keyman.interface.ifStore()` will fail upon attempts to modify their values.

This function cannot modify standard keyboard [`stores`](/developer/language/guide/stores) and is limited to system `stores` only, as standard keyboard stores are optimized into Javascript string literals set directly in code with automatically-generated names.

## See also

- [`keyman.interface.ifStore()`](ifStore)
