---
title: onPackageInstalled()
---

## Summary
The `onPackageInstalled()` is called when a keyboard package has been installed.

## Syntax
```java
onPackageInstalled(List<HashMap<String key, String value>> keyboardsInstalled)
```

### Parameters
`keyboardsInstalled`

The information dictionaries of the keyboards installed from a keyboard package with keys and values defined as `List<HashMap<String key, String value>>`.

## Description
Implement this method to handle keyboard package installed event.

## Examples ##

### Example: Using `onPackageInstalled()` ###
The following script illustrate the use of `onPackageInstalled()`:

```java
    @Override
    public void onPackageInstalled(List<HashMap<String key, String value>> keyboardsInstalled) {
        // handle keyboard package installed event here
    }
```

## See also
* [onLexicalModelInstalled()](onLexicalModelInstalled)

