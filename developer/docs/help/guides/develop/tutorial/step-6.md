---
title: Step 6: Stores, any(), and index()
---

When we have many similar rules, as in the last example on the previous
page, we can group them together into one rule by using stores. A store
is a set of characters that are grouped under a single name. Stores are
used in rules with the [`any()`](/developer/language/reference/any) and
[`index()`](/developer/language/reference/index) statements. We create a
store with a [`store()` statement](/developer/language/reference/store):

```keyman
store(vowels) "aeiou"
```

This creates a store called `vowels`, which contains the five lowercase
vowels. 
> #### Note:
We could also have written the content of the store
using ANSI or Unicode character codes, in the same way as the output.

The `any()` statement is used to match a character from a specific
store. For example, the following rule will replace any vowel with a
period, when used with the store above:

```keyman
+ any(vowels) > "."
```

The `any()` statement can be used in the context or in the key part of a
rule. It cannot be used in the output.

The second statement that is used with stores is the `index()`
statement. It is usually used in the output of a rule, and will output
the character from a particular store at the same position as the
character matched by a specified `any()` statement. This is best shown
with an example; this rule will convert all input to uppercase:

```keyman
store(lowercase) "abcdefghijklmnopqrstuvwxyz"
store(uppercase) "ABCDEFGHIJKLMNOPQRSTUVWXYZ"

+ any(lowercase) > index(uppercase,1)
```

When a letter, such as <kbd>j</kbd> is typed, the `any()`
statement finds its position in the `lowercase` store; the `index()`
statement then gets this index from the `any()` statement, and outputs
the character at the same position in the `uppercase` store, in this
case `J`.

The `index()` statement has two parts: the store from which it takes the
output character, and the offset of the `any()` statement that it gets
the character position from. This offset is found by counting the
characters and statements in the context and key parts of the rule up to
the `any()` statement. Again, a few examples may help to illustrate
this:

```keyman
"a" + any(somestore) > index(otherstore,2)         c The 'any' statement
                                                   c is character #2
"ab" any(somestore) + "c" > index(otherstore,3)    c The 'any' statement
                                                   c is character #3

c Here the 'index' statement references the second 'any' statement used,
c which is character #4
U+0041 any(somestore) "B" + any(otherstore) > index(thirdstore,4)

c You can have multiple 'index' statements in the output, which can
c reference the same or different 'any's
any(store1) + any(store2) > index(store1,2) index(store2,1) index(store3,2)
```

## Using stores in the Quick French keyboard

We can now reduce the number of rules needed for the Quick French
keyboard by using stores. We will make five stores: one for the
unaccented vowels, and one each for vowels with acute accents, grave
accents, circumflexes, and diereses. For clarity, the `group()`
statement is repeated below:

```keyman
group(Main) using keys

store( plainvowels )  'a'    'e'    'i'    'o'    'u'      'A'    'E'    'I'    'O'    'U'
store( acutevowels )  U+00E1 U+00E9 U+00ED U+00F3 U+00FA   U+00C1 U+00C9 U+00CD U+00D3 U+00DA
store( gravevowels )  U+00E0 U+00E8 U+00EC U+00F2 U+00F9   U+00C0 U+00C8 U+00CC U+00D2 U+00D9
store( circumvowels ) U+00E2 U+00EA U+00EE U+00F4 U+00FB   U+00C2 U+00CA U+00CE U+00D4 U+00DB
store( dresisvowels ) U+00E4 U+00EB U+00EF U+00F6 U+00FC   U+00C4 U+00CB U+00CF U+00D6 U+00DC

"'" + any( plainvowels ) > index( acutevowels, 2 )
"`" + any( plainvowels ) > index( gravevowels, 2 )
"^" + any( plainvowels ) > index( circumvowels, 2 )
'"' + any( plainvowels ) > index( dresisvowels, 2 )
```

This is far clearer than the long list of rules that we used earlier.
Obviously we should add one or two more ordinary rules to produce uppercase
and lower-case ç, ý, and also the angled quotes « and ». Then we
will have almost finished the keyboard:

```keyman
"'" + "y" > U+00FD    c Acute-accented Y
"'" + "Y" > U+00DD

"'" + "c" > U+00E7    c C-cedilla
"'" + "C" > U+00C7

"<" + "<" > U+00AB    c Angled quotes
">" + ">" > U+00BB
```

All we need to do now is to test the keyboard.

-   [Continue with Step 7: Testing the Keyboard](step-7)
-   [Back to Step 5: Rules with Context](step-5)