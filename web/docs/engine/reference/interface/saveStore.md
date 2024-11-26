---
title: saveStore (KSAVE)
---

## Summary

Save an option [`store`](/developer/language/guide/stores) value to a cookie for the active keyboard.

## Syntax

```c
keyman.interface.saveStore(storeName, value);
```

or

```c
KeymanWeb.KSAVE(storeName, value); // Shorthand
```

### Parameters

`storeName`
:   Type: `string`
:   The option `store` name, which will be embedded in the cookie's name.

`value`
:   Type: `string`
:   The option value to save.

### Return Value

`boolean`
:   `true` if the operation is successful, otherwise `false`.

## Description

[`keyman.interface.loadStore()`](loadStore) and `keyman.interface.saveStore()`provide API-based functionality similar to that of the keyboard language's option `store`. Values will persist across multiple visits to the site by use of cookies. However, these functions cannot interact with `store`s from existing compiled keyboards due to compilation optimizations. As such, they are most useful for custom-coded web-oriented keyboards.

## See also

- [`keyman.interface.loadStore();`](loadStore)
