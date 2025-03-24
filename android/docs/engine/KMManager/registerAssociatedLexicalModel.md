---
title: KMManager.registerAssociatedLexicalModel()
---

## Summary
The **registerAssociatedLexicalModel()** method registers a lexical model with the associated language ID.

## Syntax
```java
KMManager.registerAssociatedLexicalModel(String langId) 
```

### Parameters

`langId`
: The BCP 47 language ID

### Returns
Returns `true` if a new lexical model is loaded (different from the currently loaded lexical model), `false` otherwise.

## Description
Use this method after switching keyboard languages so the LMLayer will load and use the correct lexical model for generating suggestions.

## Examples

### Example: Using `registerAssociatedLexicalModel()`
The following script illustrates the use of `registerAssociatedLexicalModel()`:

```java
    String langId = "ta";
    KMManager.registerAssociatedLexicalModel(langId);
```

## See also
* [getAssociatedLexicalModel()](getAssociatedLexicalModel)
* [registerLexicalModel()](registerLexicalModel)
