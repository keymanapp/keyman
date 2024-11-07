---
title: IKeymanKeyboardLanguageInstalled::Uninstall Method
---

## Introduction

The `IKeymanKeyboardLanguageInstalled::Uninstall` method removes this
keyboard layout from the Windows language, and if it is the last input
method for the language, removes the language as well.

This method is asynchronous.

This method requires elevated privileges.

## Specification

``` clike
void Uninstall(void)
```
