---
title: isTouchDevice
---

## Summary

Allow external callers (such as UIs) to detect if the active device is considered touch sensitive by KeymanWeb.

## Syntax

```c
keyman.util.isTouchDevice();
```

### Parameters

None.

### Return Value

`boolean`
:   `true` if KMW is targetting touch-sensitivity for the device, otherwise `false`.

```
isTouchDevice ...
```

> [!Note]  
> In current versions of KeymanWeb this function will return `false` for touchscreen laptops; current touch support is specialized for mobile phones and tablets.
