---
title: output (KO)
---

## Summary

Outputs the specified string to an element, overwriting `nd` characters before the caret.

## Syntax

```js
keyman.interface.output(nd, Pelem, str);
```

or

```js
KeymanWeb.KO(nd, Pelem, str); // Shorthand
```

### Parameters

`nd`
:   Type: `number`
:   The number of characters to overwrite. Passively ignores values less than zero.

`Pelem`
:   Type: `Element`
:   The input element to receive the output text.

`str`
:   Type: `string`
:   The output text to write to the input element.

### Return Value

`undefined`

## Description

This is a core element of keyboard input management within KeymanWeb, typically called automatically during keystroke processing events. For comparison with [Developer 'rules'](/developer/language/guide/rules) from keyboard source code, in the rule

```keyman
"a" + "'" > "á"
```

a keyboard would, after checking that the initial context (`"a"`) matches, use

```js
keyman.interface.output(1, Pelem, "á");
```

to erase one character `"a"` and output one character `"á"`, effectively performing the desired replacement.

## See also

- [`keyman.interface.contextMatch()`](contextMatch)
