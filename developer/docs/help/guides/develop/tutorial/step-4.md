---
title: Step 4: The Keyboard Body
---

The body of the keyboard is the most important part: it determines the
behaviour of the keyboard. The body consists of groups, which in turn
contain one or more rules which define the responses of the keyboard to
certain keystrokes.

## Groups

There are two types of groups: 
* groups that process the keys pressed and the context.
* groups that process only the context.  

For simple keyboards, the latter type of group will not be required. A group begins
with a [`group` statement](/developer/language/reference/group), and
ends either at the start of another group, or at the end of the keyboard
file.

We will only use one group in the Quick French keyboard, called `Main`.
We mark the start of it with the `group` statement below. Add this line
to the keyboard if it's not already there.

```keyman
group(Main) using keys          
```

The `using keys` clause tells Keyman that this group will process
keystrokes.

## Basic Rules

A rule tells Keyman the output to produce for a certain input. A rule
consists of three parts: the **context**, the **key**, and the
**output**.

* The **context** specifies the conditions under which a rule will act. If
what is shown in the document to the left of the cursor matches the
context of a rule, the rule will be processed.
* The **key** specifies which keystroke the rule will act upon.
* The **output** determines the characters that are produced by a rule.
The output replaces the matched context in the document.

## Simple rules

The simplest rules in Keyman consist of just a key and output, as below.
(The examples in this section are just for illustration, and do not form
part of the Quick French keyboard).

```keyman
+ "a" > "ä"
```

In this rule, the key is <kbd>a</kbd>, and the output is `"ä"`. A simple rule
begins with a plus sign, followed by the key, a greater-than symbol
(suggesting "becomes"), and finally the output. As you might guess, this
rule will change a lowercase <kbd>a</kbd> key typed by the
user into **ä**.

The key and output can be written as a character in single or double
quotes (as above), or as its Unicode character code, or using a named
constant. The rule above could have also been written any of the
following ways, among others:

```keyman
+ 'a' > 'ä'

+ U+0061 > U+00E4

store(ADIERESIS) 'ä'
+ 'a' > $ADIERESIS
```

You can also write the key in one form and the context in another.

## Rules with longer output

The output of a rule is not limited to a single character. You could,
for example, write a rule such as the following:

```keyman
+ "f" > "ph"
```

This would change any <kbd>f</kbd> keys typed into **ph**.
If the output of a rule consists of more than one character, you can
write the characters in different ways if necessary, with a space
separating each part. You can specify multiple characters in quotes, but
if you use the Unicode codes to write the characters, you must separate
each with a space:

```keyman
+ "f" > U+0070 U+0068

+ "f" > U+0070 "h"
```

These rules are functionally identical to the one [further above](#toc-simple-rules).

-   [Continue with Step 5: Rules with Context](step-5)
-   [Back to Step 3: The Keyboard Header](step-3)