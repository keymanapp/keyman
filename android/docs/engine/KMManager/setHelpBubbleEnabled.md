---
title: KMManager.setHelpBubbleEnabled() (Deprecated)
---

## Summary

The **`setHelpBubbleEnabled()`** enables or disables the help bubble.

## Syntax

``` javascript
KMManager.setHelpBubbleEnabled(boolean newValue)
```

### Parameters

`newValue`
:   Set `true` to enable the help bubble, `false` to disable.

## Description

Use this method to enable or disable the help bubble which displays "Tap
here to change keyboard" over the 'Globe' key if the user has never used
it yet. By default it is enabled. This method only works for the in-app
keyboard since the system-wide keyboard never displays a help bubble.

## Examples

### Example: Using `setHelpBubbleEnabled()`

The following script illustrate the use of `setHelpBubbleEnabled()`:

``` javascript
    KMManager.setHelpBubbleEnabled(false);
    // Help bubble is now disabled
```

## History

Deprecated syntax in Keyman Engine for Android 16.0

## See also

-   [isHelpBubbleEnabled()](isHelpBubbleEnabled) (Deprecated)
