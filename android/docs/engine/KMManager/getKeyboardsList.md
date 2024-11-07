---
title: KMManager.getKeyboardsList()
---

## Summary

The **`getKeyboardsList()`** method returns the keyboards list.

## Syntax

``` javascript
KMManager.getKeyboardsList(Context context)
```

### Parameters

`context`
:   The context.

### Returns

Returns keyboards list as `List<Keyboard>` if it exists, `null`
otherwise.

## Description

Use this method to get details of all keyboard's in keyboards menu.

  

------------------------------------------------------------------------

## Syntax (Deprecated)

``` javascript
KMManager.getKeyboardsList(Context context)
```

### Parameters

`context`
:   The context.

### Returns

(Deprecated) Returns keyboards list as
`ArrayList<HashMap<String key, String value>>` if it exists, `null`
otherwise.

## Description

Use this method to get details of all keyboard's in keyboards menu.

  

------------------------------------------------------------------------

## Examples

### Example: Using `getKeyboardsList()`

The following script illustrate the use of `getKeyboardsList()`:

``` javascript
    List<Keyboard> keyboardsList = KMManager.getKeyboardsList(this);
    if ((keyboardsList != null) && keyboardsList.size() < 2) {
        // Add another Keyboard
        Keyboard kbd = new Keyboard(...);
        KMManager.addKeyboard(kbd);
    }
```

## History

Added syntax for returning list of Keyboard type in Keyman Engine for
Android 14.0.

Deprecated syntax for returning the list of HashMap&lt;String key,
String value&gt; in Keyman Engine for Android 14.0

## See also

-   [`getCurrentKeyboardIndex()`](getCurrentKeyboardIndex)
-   [`getCurrentKeyboardInfo()`](getCurrentKeyboardInfo)
-   [`getKeyboardIndex()`](getKeyboardIndex)
-   [`getKeyboardInfo()`](getKeyboardInfo)
-   [`keyboardExists()`](keyboardExists)
