---
title: KMManager.isHelpBubbleEnabled() (Deprecated)
---

## Summary

The **`isHelpBubbleEnabled()`** method returns whether the help bubble
is enabled.

## Syntax

``` javascript
KMManager.isHelpBubbleEnabled()
```

### Returns

Returns `true` if the help bubble is enabled, `false` otherwise.

## Description

Use this method to check if the help bubble is enabled. This method only
works for the in-app keyboard since the system-wide keyboard never
displays a help bubble.

## Examples

### Example: Using `isHelpBubbleEnabled()`

The following script illustrate the use of `isHelpBubbleEnabled()`:

``` javascript
    boolean isHelpBubbleEnabled = KMManager.isHelpBubbleEnabled();
```

## History

Deprecated syntax in Keyman Engine for Android 16.0

## See also

-   [`setHelpBubbleEnabled()`](setHelpBubbleEnabled) (Deprecated)
