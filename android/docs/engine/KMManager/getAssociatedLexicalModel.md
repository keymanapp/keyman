---
title: KMManager.getAssociatedLexicalModel()
---

## Summary
The **getAssociatedLexicalModel()** method searches the installed lexical models list and see if there's an associated model for a given language

## Syntax
```java
HashMap<String, String> getAssociatedLexicalModel(String langId) 
```

### Parameters

`langId`
: The language ID

### Returns
A dictionary of the associated lexical model information with keys and values defined as 
`HashMap<String key, String value)`.

`null` if there's no associated lexical model for a language `langId`.

## Description
Use this method to search the installed lexical models list and see if there's an associated model for a given language ID.

## Examples

### Example: Using `getAssociatedLexicalModel()`
The following script illustrates the use of `getAssociatedLexicalModel()`:

```java
    String langId = "ta";
    HashMap<String, String> lexModelMap = KMManager.getAssociatedLexicalModel(langId);
```

## See also
* [addLexicalModel()](addLexicalModel)
* [registerLexicalModel()](registerLexicalModel)
