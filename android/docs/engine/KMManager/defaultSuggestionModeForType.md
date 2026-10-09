---
title: KMManager.defaultSuggestionModeForType()
---

## Summary
The **defaultSuggestionModeForType()** method determines the appropriate suggestion mode to use when providing
input to different types of input-receiving `Editable` objects.

## Syntax
```javascript
KMManager.defaultSuggestionModeForType(InputType inputType)
```

### Parameters

`inputType`
: The current value of the active `Editable`'s `.getInputType()` property.

In addition, the method will retrieve the user's preference for the most-recently activated
lexical model and adjust the returned `SuggestionType` accordingly.

### Returns
`SuggestionType`, which may hold one of the following values:

- `SuggestionType.SUGGESTIONS_DISABLED` - disables predictive-text entirely
    * This is automatically chosen for editables that should not permit
      predictions, such as password editables and those not intended for plain
      text.
    * Selected if the `inputType` value contains one or more of the following
      flags:
        * TYPE_TEXT_VARIATION_PASSWORD
        * TYPE_TEXT_VARIATION_WEB_PASSWORD
        * TYPE_CLASS_NUMBER
        * TYPE_CLASS_PHONE
- `SuggestionType.PREDICTIONS_ONLY` - enables text predictions, but not text
  correction
- `SuggestionType.PREDICTIONS_WITH_CORRECTIONS` - enables text predictions and
  corrections, but does not enable autocorrect.
    * Selected for editables likely to represent text predictive text cannot
    adequately predict, such as address fields, URL fields, and names.
- `SuggestionType.PREDICTIONS_WITH_AUTO_CORRECT`- enables all predictive-text
  features:  predictions, corrections, and auto-correct.
    * Enabled when one of the following flags is detected:
        * TYPE_TEXT_VARIATION_FILTER
        * TYPE_TEXT_VARIATION_NORMAL
        * TYPE_TEXT_VARIATION_SHORT_MESSAGE
        * TYPE_TEXT_VARIATION_LONG_MESSAGE
        * TYPE_TEXT_VARIATION_EMAIL_SUBJECT
        * TYPE_TEXT_VARIATION_WEB_EDIT_TEXT

## Description
Use this method when you need to decide what predictive-text mode should be active.

## Examples

### Example: Using `defaultSuggestionModeForType()`
The following script illustrates the use of `defaultSuggestionModeForType()`:

```java
    int inputType = textView.getInputType();
    SuggestionType suggestionType = KMManager.defaultSuggestionModeForType(inputType);
    KMManager.setSuggestionType(suggestionType);
```

## History
Added syntax in Keyman Engine for Android 19.0.

## See also
* [setSuggestionType()](setSuggestionType)
