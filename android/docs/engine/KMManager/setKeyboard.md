---
title: KMManager.setKeyboard()
---

## Summary

The `setKeyboard()` method sets the active keyboard.

## Syntax

```java
KMManager.setKeyboard(Context context, Keyboard keyboardInfo)
```

### Parameters

`context`
: The context.

`keyboardInfo`
: `Keyboard` type of the keyboard information. <!-- TODO: add Keyboard type documentation -->

### Returns

Returns `true` if the keyboard was set successfully, `false` otherwise.

## Description

Selects the keyboard identified by the keyboard information, which is
normally returned by [`getKeyboardInfo()`](getKeyboardInfo).

---

## Syntax

```java
KMManager.setKeyboard(String packageID, String keyboardID, String languageID)
KMManager.setKeyboard(String packageID, String keyboardID, String languageID, String keyboardName, String languageName, String kFont, String kOskFont)
KMManager.setKeyboard(String packageID, String keyboardID, String languageID, String keyboardName, String languageName, String kFont, String kOskFont, String displayName)
```

### Parameters

`packageID`
: ID of the keyboard package.

`keyboardID`
: ID of the keyboard.

`languageID`
: ID of the associated language.

`keyboardName`
: Name of the keyboard.

`languageName`
: Name of the associated language.

`kFont`
: Filename or description of the font to type with the keyboard. Can be `null`
  or empty string.

`kOskFont`
: Filename or description of the font displayed on the keyboard. Can be `null`
  or empty string.

`displayName`
: A text string to display on the spacebar identifying this keyboard; if `null`,
  uses engine default from [`setSpacebarText()`](setSpacebarText); if `""`,
  shows no text on the spacebar.

### Returns

Returns `true` if the keyboard was set successfully, `false` otherwise.

## Description

Sets the currently active keyboard, along with font and display details.

---

## Syntax

```
KMManager.setKeyboard(Context context, int position)
```

### Parameters

`context`
: The context.

`position`
: 0-based position of the keyboard in the keyboards list.

### Returns

Returns `true` if the keyboard was set successfully, `false` otherwise.

## Description

Sets the currently active keyboard by position in the keyboards list, as
returned by [`getKeyboardsList()`](getKeyboardsList)

---

## Examples

### Example 1: Using `setKeyboard()`

The following script illustrates the use of `setKeyboard()` with keyboard information:

```java
// Setting a Keyman keyboard
Keyboard keyboardInfo = KMManager.getDefaultKeyboard();
KMManager.setKeyboard(getApplicationContext(), keyboardInfo);
```

### Example 2: Using `setKeyboard()`

The following script illustrates the use of `setKeyboard()` with package ID,
keyboard ID, and language ID:

```java
// Setting a Keyman keyboard
KMManager.setKeyboard("sil_euro_latin", "sil_euro_latin", "en");
```

### Example 3: Using `setKeyboard()`

The following script illustrates the use of `setKeyboard()`, providing
additional font detail:

```java
// Setting a custom keyboard from the tamil99m keyboard package
KMManager.setKeyboard("tamil99m", "tamil99m", "ta", "Tamil 99M", "Tamil", "aava1.ttf", "aava1.ttf");
```

### Example 4: Using `setKeyboard()`

The following script illustrates the use of `setKeyboard()` with keyboard index:

```java
// Setting a custom keyboard which exists in keyboards list
int kbIndex = KMManager.getKeyboardIndex(this, "tamil99m", "ta");
KMManager.setKeyboard(this, kbIndex);
```

## See also

* [`addKeyboard()`](addKeyboard)
* [`getDefaultKeyboard()`](getDefaultKeyboard)
* [`getKeyboardInfo()`](getKeyboardInfo)
* [`getKeyboardsList()`](getKeyboardsList)
* [`switchToNextKeyboard()`](switchToNextKeyboard)
