---
title: Step 5: Rules with Context
---

## Rules with Context

Very often we want a keyboard to produce different output based on more
than just the last keystroke. For example, in the Quick French keyboard,
we want the key <kbd>e</kbd> to produce one of è, é,
ë, ê, or just e, depending on what was typed immediately before
it. To do this we must make our rules look at the context.

The context is the output from previous rules; that is, the characters
that are displayed on the screen. We can make a rule work with only
certain context by putting this before the plus sign in the rule:

```keyman
"^" + "e" > "ê"
```

With this rule, whenever an <kbd>e</kbd> is typed, if it
was preceded by a caret (^), the output will be ê. It is important
to remember that the context consists of **output
from previous rules**, not the previous keystrokes. To emphasize
this point, consider the four rules below:

```keyman
+ "a" > "b"
+ "b" > "c"
"b" + "c" > "d"
"c" + "d" > "e"
```

With these rules, typing <kbd>b</kbd> <kbd>c</kbd> would produce the
output `cc`, and not `d`, as you might initially expect. This is because
the key <kbd>b</kbd> is converted by the second rule into
the output `c`, while the third rule expects a context of b, and not
c; we would have to type <kbd>a</kbd> <kbd>c</kbd> to get `d`.

However, if a key has no matching rule, the output will be the same as
the key: so the output `e` will be produced for either of the inputs <kbd>c</kbd> <kbd>d</kbd> (because <kbd>c</kbd> by itself has no rule), and <kbd>b</kbd> <kbd>d</kbd>.

## Continuing the Quick French keyboard

Now we know how to create context-dependent rules, we can continue
making the Quick French keyboard. Let's start with the rules for
acute-accented characters, using the ANSI codes from the table we
prepared earlier:

```keyman
c lowercase characters with acute accent
"'" + "a" > U+00E1
"'" + "e" > U+00E9
"'" + "i" > U+00ED
"'" + "o" > U+00F3
"'" + "u" > U+00FA
"'" + "y" > U+00FD

c uppercase characters with acute accent
"'" + "A" > U+00C1
"'" + "E" > U+00C9
"'" + "I" > U+00CD
"'" + "O" > U+00D3
"'" + "U" > U+00DA
"'" + "Y" > U+00DD
```

We can also create similar rules for the other thirty-odd accented
characters.

As you can see, even for a simple keyboard like this we quickly end up
with a large number of rules, which makes for clumsiness. We can make
things simpler using stores, and the `any()` and `index()` statements.

-   [Continue with Step 6: Stores, `any()`, and `index()`](step-6)
-   [Back to Step 4: The Keyboard Body](step-4)