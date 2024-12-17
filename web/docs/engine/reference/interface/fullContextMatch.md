---
title: fullContextMatch (KFCM)
---

## Summary

Context matching: Returns `true` if the current context matches the specified rule context specification.

## Syntax

```c
keyman.interface.fullContextMatch(n, Pelem, rule);
```

or

```c
KeymanWeb.KFCM(n, Pelem, rule); // Shorthand
```

### Parameters

`n`
:   Type: `number`
:   Relative position of the caret for the context match attempt.

`Pelem`
:   Type: `Element`
:   The element being operated upon.

`rule`
:   Type: `Array(ContextEntry)`
:   The rule context specification to be matched.

### Return Value

`boolean`
:   `true` if the context matches the rule specification `rule`, otherwise `false`.

## Rule specification

Each `ContextEntry` in the `rule` array must be one of the following six types:

-   A plain string with one character
-   A `DeadkeyEntry` object with the following members:

    `t`
        :   `'d'`
        :   Indicates to expect a [`deadkey`](/developer/language/reference/deadkey) entry.

    `d`
        :   `number`
        :   The integer ID of the deadkey to match.

-   An `AnyEntry` object with the following members:

    `t`
        :   `'a'`
        :   Indicates the use of an [`any`](/developer/language/reference/any) statement.

    `a`
        :   `string`
        :   The store string to be searched for a context match

    `n`
        :   `boolean` *optional*
        :   `true` signals to negate the match, as with a [`notany`](/developer/language/reference/notany) statement.

-   An `IndexEntry` object with the following members:

    `t`
        :   `'i'`
        :   Indicates the use of an [`index`](/developer/language/reference/index) statement.

    `i`
        :   `string`
        :   the store string to be indexed for output corresponding to a previous `any`

    `o`
        :   `number`
        :   the index of the corresponding `any` in the rule's context, starting from 1.

-   A `ContextEx` object with the following members:

    `t`
        :   `'c'`
        :   Indicates the use of a [`context`](/developer/language/reference/context) statement.

    `c`
        :   `number`
        :   The position in context to be matched by the current context location.

-   A `NulEntry` object with the following member:

    `t`
        :   `'n'`
        :   Indicates the use of a context-based [`nul`](/developer/language/reference/nul) rule.

The rule will be interpreted left to right, starting at the specified relative position to the caret.

## Description

This is a core element of keyboard input management within KeymanWeb introduced with version 10.0, typically called automatically during keystroke processing events. For comparison with [Developer 'rules'](/developer/language/guide/rules) from keyboard source code...

```keyman
store(pair_1) 'aA'
store(pair_2) 'bB'

c Lots of keyboard rules...

dk(money) 'c' any(pair_1) index(pair_2, 3) + '.' > 'taxi'
```

would have Javascript roughly as follows:

```c
// In the keyboard store definitions:
this.s_pair_1 = 'aA';            // store(pair_1) 'aA'
this.s_pair_2 = 'bB';            // store(pair_2) 'bB'

// For the deadkey:
this.d_money = 0;                // dk(money)'s internal id

// Within the keyboard rule-matching function:
var matched = keyman.interface.fullContextMatch(4, element, {
  {t:'d', d:this.d_money},       // dk(money)
  'c',                           // 'c'
  {t:'a', a:this.s_pair_1},      // 'any(pair_1)'
  {t:'i', i:this.s_pair_2, o:3}  // 'index(pair_2, 3)'
});
```

## See also

- [The Keyman Keyboard Language Reference](/developer/language/reference)
- [`keyman.interface.contextMatch()`](contextMatch)
- [`keyman.interface.deleteContext()`](deleteContext)
