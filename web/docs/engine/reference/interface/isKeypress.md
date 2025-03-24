---
title: isKeypress (KIK)
---

## Summary

Returns `true` if the input event corresponds to a keypress event resulting in character output.

## Syntax

```c
keyman.interface.isKeypress(e);
```

or

```c
KeymanWeb.KIK(e); // Shorthand
```

### Parameters

`e`
:   Type: `Object`
:   The event object to be evaluated.

### Return Value

`boolean`
:   `true` if the event would produce text output, otherwise `false`.

## Description

This function is designed to facilitate filtering of keystrokes, allowing custom-coded keyboards to ignore function-oriented keystroke events that should never be used for text generation, such as for F1-F12.

The logic corresponds to that for the keyboard language [`nomatch`](/developer/language/reference/nomatch) rule.
