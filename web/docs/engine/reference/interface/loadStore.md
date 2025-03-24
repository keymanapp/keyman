---
title: loadStore (KLOAD)
---

## Summary

Load an option [`store`](/developer/language/guide/stores) value from a cookie or default value if no prior stored value exists.

## Syntax

```c
keyman.interface.loadStore(kbdName, storeName, value);
```

or

```c
KeymanWeb.KLOAD(kbdName, storeName, value); // Shorthand
```

### Parameters

`kbdName`
:   Type: `string`
:   The keyboard's identifying internal name.

`storeName`
:   Type: `string`
:   The option `store` name that is embedded in the cookie's name.

`value`
:   Type: `string`
:   A value for the store to use as default if no prior value exists.

### Return Value

`string`
:   The current value of the store, or the default `value` provided if the store's value is `undefined`.

## Description

`keyman.interface.loadStore()` and [`keyman.interface.saveStore()`](saveStore) provide API-based functionality similar to that of the keyboard language's option `store`. Values will persist across multiple visits to the site by use of cookies. However, these functions cannot interact with `store`s from existing compiled keyboards due to compilation optimizations. As such, they are most useful for custom-coded web-oriented keyboards.

## See also

- [`keyman.interface.saveStore();`](saveStore)
