---
title: KMManager.getCurrentKeyboardInfo()
---

## Summary

The **`getCurrentKeyboardInfo()`** method returns keyboard information
of the current keyboard.

## Syntax

``` javascript
KMManager.getCurrentKeyboardInfo(Context context)
```

### Parameters

`context`
:   The context.

### Returns

Returns the current keyboard information of `Keyboard` type.

## Description

Use this method to get details of the currently selected keyboard.
Details include package ID, keyboard ID, language ID, keyboard name,
language name and fonts.

  

------------------------------------------------------------------------

## Syntax (Deprecated)

``` javascript
KMManager.getCurrentKeyboardInfo(Context context)
```

### Parameters

`context`
:   The context.

### Returns

(Deprecated) Returns an information dictionary of the current keyboard
with keys and values defined as `HashMap<String key, String value>`.

## Description

Use this method to get details of the currently selected keyboard.
Details include keyboard ID, language ID, keyboard name, language name
and fonts.

  

------------------------------------------------------------------------

## Examples

### Example: Using `getCurrentKeyboardInfo()`

The following script illustrate the use of `getCurrentKeyboardInfo()`:

``` javascript
    Keyboard keyboardInfo = KMManager.getCurrentKeyboardInfo(this);
    if (keyboardInfo != null) {
        String packageID = keyboardInfo.getPackageID();
        String keyboardId = keyboardInfo.getKeyboardID();
        String keyboardName = keyboardInfo.getKeyboardName();
        String languageId = keyboardInfo.getLanguageID();
        String languageName = keyboardInfo.getLanguageName();
        String version = keyboardInfo.getVersion();
        String font = keyboardInfo.getFont();
        String oskFont = keyboardInfo.getOSKFont();
        //
    }
```

## History

Added syntax for returning Keyboard type in Keyman Engine for Android
14.0.

Deprecated syntax for returning HashMap&lt;String key, String value&gt;
in Keyman Engine for Android 14.0

## See also

-   [`getCurrentKeyboardIndex()`](getCurrentKeyboardIndex)
-   [`getKeyboardIndex()`](getKeyboardIndex)
-   [`getKeyboardInfo()`](getKeyboardInfo)
-   [`getKeyboardsList()`](getKeyboardsList)
-   [`keyboardExists()`](keyboardExists)
