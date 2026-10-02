---
title: KMManager.setSuggestionType()
---

## Summary
The **setSuggestionType()** method configures which predictive-text features
will be presented to the user - which types of suggestions will be offered.

## Syntax
```java
KMManager.setSuggestionType(KeyboardType keyboardType, SuggestionType suggestionType)
```

### Parameters

`keyboardType`
:   The keyboard type. `KEYBOARD_TYPE_INAPP` or
    `KEYBOARD_TYPE_SYSTEM`.

`suggestionType`
:   The type of suggestions to offer.  One of the following:
    `SuggestionType.SUGGESTIONS_DISABLED`
    `SuggestionType.PREDICTIONS_ONLY`
    `SuggestionType.PREDICTIONS_WITH_CORRECTIONS`
    `SuggestionType.PREDICTIONS_WITH_AUTO_CORRECT`

## Description
Use this method to configure what types of suggestions may be offered to the
user.  It cannot force predictions to be available if no lexical model is
present, but it can be used to restrict types of suggestions from being offered
in inappropriate contexts.

`suggestionType` may use the result of `defaultSuggestionModeForType` for the
active `Editable`'s input type, which will be sufficient for most cases.

### Example: Using `setSuggestionType()`
The following script illustrates the use of `setSuggestionType()`:

```java
    int inputType = textView.getInputType();
    SuggestionType suggestionType = KMManager.defaultSuggestionModeForType(inputType);
    KMManager.setSuggestionType(suggestionType);
```

## History
Added syntax in Keyman Engine for Android 19.0.

## See also
* [defaultSuggestionModeForType](defaultSuggestionModeForType)
