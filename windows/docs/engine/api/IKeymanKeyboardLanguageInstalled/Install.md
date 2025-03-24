---
title: IKeymanKeyboardLanguageInstalled::Install Method
---

## Introduction

**Deprecated:** this function is deprecated as of Keyman 14.0. Instead,
use the functions in
[`IKeymanKeyboardLanguageInstalled2`](../IKeymanKeyboardLanguageInstalled2).

The `IKeymanKeyboardLanguageInstalled::Install` method adds this
keyboard layout to the Windows language, and if it is the first input
method for the language, adds the language as well. This is the best way
to install a language that is a suggested language for the keyboard.

This method is asynchronous.

This method requires elevated privileges.

## Specification

``` clike
void Install(void)
```
