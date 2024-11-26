---
title: KMManager.deregisterLexicalModel()
---

## Summary
The **deregisterLexicalModel()** method deregisters the specified lexical model from the LMLayer so it isn't used.

## Syntax
```javascript
KMManager.degisterLexicalModel(String modelID)
```

### Parameters

`modelID`
: The ID of the lexical model to deregister

### Returns
`true`

## Description
Use this method when you want the lexical model to stop generating suggestions. 

## Examples

### Example: Using `deregisterLexicalModel()`
The following script illustrates the use of `dergisterLexicalModel()`:

```java
    String lexicalModelID = "example.ta.wordlist";
    KMManager.deregisterLexicalModel(lexicalModelID);
```

## See also
* [registerLexicalModel()](registerLexicalModel)
