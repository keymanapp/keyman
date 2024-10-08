---
title: Customization and extension of the Unicode word breaker
---

> ### Note:  
While this offers a great amount of flexibility and customization that may assist predictions for your language, note that this approach may get quite technical!

As mentioned on the [main word breaker page](./word-breaker),
our currently-supported lexical models need to know what a word
is in running text. After all, it is quite difficult to look up a word in a
dictionary when unclear about what the start and end of that word even is.

In languages using the Latin script—like, English, French,
and SENĆOŦEN—finding words is easy. Words are separated by spaces or
punctuation. The actual rules for where to find words can get quite tricky to
describe, but Keyman implements the [Unicode Standard Annex #29 §4.1 Default Word Boundary Specification](https://unicode.org/reports/tr29/#Word_Boundaries) which works well for most languages.

This guide is about techniques that may be used to customize and extend
the behaviors of that specification to be better tailored to your language.
This is done by using similar patterns and structures to the rules found in
the specification itself.

There are three ways - all of them optional - to extend and customize
the word-breaking rules themselves:

* If you need to prevent splits in very specific scenarios and/or add splits in other specific scenarios, you may specify [context-based rules](#rules) to obtain the desired behavior.
* If certain characters are not handled appropriately for their role in
       your language, you may [map characters](#map) to different
       word-breaking character classes - including custom ones.  This will
       override the default property they are assigned by the default
       implementation, with the new property applying for all word-breaking
       rules.
* If the default word-breaking classes from the specification are
       too general for certain aspects of your language, it is possible to
       [define custom character classes](#define) for use in custom
       rules.

## A first example

This example was designed to address the needs of a minority language in the country
of Cambodia. The majority language does not use spaces for wordbreaking, while the
minority language in question does use them.  In addition, hyphens sometimes occur
within words.

The word breaker function can be specified in the
[model definition file](./model-definition-file) as follows:

```typescript
const source: LexicalModelSource = {
  format: 'trie-1.0',
  sources: ['wordlist.tsv'],
  // CUSTOMIZE THIS:
  wordBreaker: (text) => {
    let customization = {

      /*** Definition of extra word-breaking rules ***/
      rules: [{
        match: (context) => {
          if(context.propertyMatch(null, ["ALetter"], ["Hyphen"], ["ALetter"])) {
            return true;
          } else if(context.propertyMatch(["ALetter"], ["Hyphen"], ["ALetter"], null)) {
            return true;
          } else if(context.propertyMatch(null, ["ALetter"], ["Hyphen"], ["eot"])) {
            return true;
          } else {
            return false;
          }
        },
        breakIfMatch: false
      }],

      /*** Character class overrides for specific characters ***/
      propertyMapping: (char) => {
        let hyphens = ['\u002d', '\u2010', '\u058a', '\u30a0'];
        // treats Khmer consonants & independent vowels in the same manner
        // as the basic latin-script based alphabet
        if(char >= '\u1780' && char <= '\u17b3') {
            return "ALetter";
        } else if(hyphens.includes(char)) {
            return "Hyphen";
        } else {
          // The other Khmer characters already have useful word-breaking
          // property assignments.
          return null;
        }
      },

      /*** Declares any new, custom character classes to be recognized by the word-breaker ***/
      customProperties: ["Hyphen"]
    };

    /*** Connects all the pieces together for actual use ***/
    return wordBreakers['default'](text, customization);
  },
  // ...
};

export default source;
```

This example's customization is designed to accomplish two goals:

1. Unicode's wordbreaker does not map base Khmer consonants to any of the
  [relevant wordbreaking character properties](https://unicode.org/reports/tr29/#Table_Word_Break_Property_Values), causing it to be treated as `"Other"`.
  The minority language in question instead wishes for Latin-script-like word-breaking,
  so mapping the consonants to the same property as Latin-script consonants allows
  them to be treated similarly - in the manner they expect.

2. By default, the wordbreaker will automatically insert word boundaries before
  and after a hyphen.  To allow mid-word hyphens, we need to remap them to
  a different character property.

  While the specification itself mentions that we could
  just map hyphens to `"MidLetter"`, this example opts to define
  a custom-tailored property that ensures only hyphens are affected in order to add
  a special, end-of-context rule that may be useful when typing - the rule above
  referencing `"eot"`.

More on the wordbreaking character properties will be covered later.

## Custom word-breaking rules

When defining additional rules for use in word-breaking, it is advisable to reference
the [rules of the Unicode Standard Annex #29 §4.1 Default Word Boundary Specification](https://unicode.org/reports/tr29/#Word_Boundary_Rules).

Rules WB1 through WB4 of the specification will always apply first, before any
custom rules.  Custom rules will then be applied in order of their definition within
the model, with them all being applied before the specification's rules labeled WB5
and onward.

Each rule should be of the following form:

```typescript
{
  // A function that returns 'true' whenever the rule should apply
  match: (context) => {
    // ...
  },
  // Whether to prevent (false) or to enforce (true) a boundary when the rule applies.
  breakIfMatch: false
}
```

Toward this end, the `context` object received by match provides a function called
`propertyMatch` in order to define rules like those of the wordbreaker specification.
Let's take WB6 - "do not break letters across certain punctuation" - as an example.

As written in the spec, WB6 reads **AHLetter x (MidLetter | MidNumLetQ) AHLetter**.

This is simply a series of characters, up to two characters before and after a potential break point. To break that down:
- (implicit) **Any** -- accept any character in this position
- **AHLetter**: `ALetter` or `Hebrew_Letter`
- **x** - "do not break"
- (either) **MidLetter** or **MidNumLetQ**: one of `MidLetter`, `MidNumLet`, or `Single_Quote`.
- **AHLetter**: `ALetter` or `Hebrew_Letter`

The expansions `MidNumLetQ` and `AHLetter` are defined at https://unicode.org/reports/tr29/#WB_Rule_Macros.

If written as a custom rule, rule WB6 takes the following form.  Note the use of the function `context.propertyMatch`,
which takes 4 parameters - two to match characters before and two to match characters after a potential boundary:

```typescript
// Rule WB6 from the Unicode spec, as a custom rule:
{
  match: (context) => {
    return context.propertyMatch(null, // no requirements set, so "Any" character may match
                                ["ALetter", "Hebrew_Letter"],
                                // x
                                ["MidLetter", "MidNumLet", "Single_Quote"],
                                ["ALetter", "Hebrew_Letter"]);
  },
  breakIfMatch: false // do not break
}
```

Note that an empty array `[]` in any slot is not treated the same as `null` -
use of an empty `[]` will prevent the rule from matching.

Almost all of the specification's rules are of this form.  Again, up to two characters
before a potential boundary may be considered alongside up to two characters after
the potential boundary.

### Word-breaking property names
The names used in each array must be defined in one of the following places:
* https://unicode.org/reports/tr29/#Table_Word_Break_Property_Values
* `customProperties` - your [declaration of any custom property types](#define)
* One of the special property types `"Other"`, `"sot"`, or `"eot"`:
    * `Other`:  a character without an associated word-breaking property value
    * `sot`:  "start of text" - a marker indicating the beginning of the string being word-broken
    * `eot`:  "end of text" - a marker indicating the end of the string being word-broken

All rules use case-insensitive matching, so capitalization differences will not affect operation.

### Rule-matching examples

#### A successful rule application
As an example, when determining whether or not to break the English word `don't` when
applying WB6 as written above, this is what happens near the apostrophe:

```typescript
{
  match: (context) => {
    return context.propertyMatch(null, /* match any character */            // "o" - ALetter
                                ["ALetter", "Hebrew_Letter"],               // "n" - ALetter
                                // x
                                ["MidLetter", "MidNumLet", "Single_Quote"], // "'" - Single_Quote
                                ["ALetter", "Hebrew_Letter"]);              // "t" - ALetter
  },
  breakIfMatch: false // do not wordbreak at any position where the rule matches.
}
```

The rule applies at the position between the `n` and the `'` of `don't`, telling the
word-breaker not to word-break if the rule matches at that location.

#### An unsuccessful rule match

Given the input `n't|` (where `|` is the caret), we can see that the rule will not match:
```typescript
{
  match: (context) => {
    return context.propertyMatch(null, /* automatic match */                // "n" - ALetter
                                ["ALetter", "Hebrew_Letter"],               // "'" - Single_Quote
                                // x
                                ["MidLetter", "MidNumLet", "Single_Quote"], // "t" - Single_Quote
                                ["ALetter", "Hebrew_Letter"]);              //     - eot
  },
  breakIfMatch: false // do not break
}
```

The same rule does not apply between the `'` and the `t`, so it does not apply at this position in
the text. This is why the wordbreaking spec includes rule WB7 in addition to WB6 - some scenarios
require multiple context property-matching attempts.

You may define any number of these rule objects in any order for use within the wordbreaker within
the `rules` array.

> **Important note**: When no other rule successfully matches a potential word-boundary position, the spec's rule WB999 applies as a catch-all and will enforce a split.  Preventing a word-break always requires a successfully-matching rule, though this is covered for most cases by the Unicode specification's ruleset.

Defining custom rules is a powerful tool, but it is detailed work and may be somewhat tedious
to get right.  Feel free to [ask for help at our Community Site](https://community.software.sil.org/c/keyman/19) for assistance.

## Character property remapping

Many writing systems in the world are shared by multiple languages, using most of the same
characters in common.  However, sometimes there may be notable differences in how specific
languages wish to treat certain characters.  In order to address these cases, we allow
overriding the "standard" word-breaking property that the Unicode specification gives the
character with one set by the lexical model.

### Default character properties

For reference, [this text file](https://www.unicode.org/Public/UCD/latest/ucd/auxiliary/WordBreakProperty.txt)
provides the standard word-breaking properties for all characters. This is one of many
files Unicode provides publicly here: https://www.unicode.org/reports/tr41/#Props0.

That text file contains many lines of the following form:

```
0041..005A    ; ALetter # L&  [26] LATIN CAPITAL LETTER A..LATIN CAPITAL LETTER Z
```

This says that `0041` through `005A` - or rather, `\u0041` (the code for 'A') through
`\u005A` (the code for 'Z') - are assigned word-breaking property `ALetter`.  That
range covers 26 (`[26]`) characters.  (For our purposes here, the `L&` part is
irrelevant.)

As noted at the top of the file:

```
#  All code points not explicitly listed for Word_Break
#  have the value Other (XX).

# @missing: 0000..10FFFF; Other
```

### Redefining character properties

Of note from [our first example](#example):

```typescript
/*** Character class overrides for specific characters ***/
propertyMapping: (char) => {
  // ...
  // treats Khmer consonants & independent vowels in the same manner
  // as the basic latin-script based alphabet
  if(char >= '\u1780' && char <= '\u17b3') {
      return "ALetter";
  } // ...
},
```

If you search the [property definition text file](https://www.unicode.org/Public/UCD/latest/ucd/auxiliary/WordBreakProperty.txt)
for `1780` or `17b3`, you will find neither.  These correspond to many letters
from the Khmer character set - notably, the 'base' characters used in Khmer's
grapheme clusters.  The other Khmer characters tend to attach at various positions
around these base characters. The majority language for the script - Khmer -
does not follow conventional word-breaking rules; most notably, they do not add
whitespace between each word. (There are other strategies that get utilized for
such scripts.)

As breaks sometimes occur between the base characters while other times do not,
properties for these base characters were not explicitly defined and are thus
treated as class `Other`.
- As other Khmer characters tend to attach around the base characters, they do
  have specified word-breaking properties - they `Extend` the grapheme cluster.
- Searching `17b4` in [the text file mentioned above](https://www.unicode.org/Public/UCD/latest/ucd/auxiliary/WordBreakProperty.txt) will show the closest results to the characters under discussion.

However, there are minority languages that prefer to use whitespaces between words,
meaning that there should never be wordbreaks applied directly between neighboring
characters for their words.  In such cases, we can map them to a pre-existing property
with the desired behavior - `ALetter`.

```typescript
if(char >= '\u1780' && char <= '\u17b3') {
    return "ALetter";  // Maps Khmer-script base characters to ALetter
                       // to allow Latin-script-like word-breaking.
}
```

For another example of character property remapping, consider the use of hyphens with
words (and/or names) in some languages.  Default word-breaking behavior will split
hyphenated words and names apart, but by changing the property of hyphens, it is
possible to disable this behavior.

Noting [rule WB6](#WB6) and WB7, the `MidLetter` class is designed to prevent
word-breaks from occurring when its characters lie directly between letters -
hence the property name.  Assigning hyphens to this class can provide the
desired behavior.

```typescript
let hyphens = ['\u002d', '\u2010', '\u058a', '\u30a0'];
// ...
if(hyphens.includes(char)) {
    return "MidLetter";
} // ...
```

## Defining and using new word-breaking properties

There may be some cases in which none of the default character word-breaking
properties provide the exact behavior that you're wanting, or perhaps you
want only specific characters from that class to match custom rules.  For such
cases, wordbreaker customization also allows definition of new word-breaking
properties.

For one example, note how word-breaking operations affect predictions when
typing new words:

- `can'`, with the intent to type `can't`
- `full-`, with the intent to type `full-scale`

For the first example above, while `'` (property `Single_Quote`) is included within
WB6 and WB7, those rules only apply _between letters_.  If there is no letter
on the right-hand side, `can'` will be interpreted as `can` + `'` by the
word-breaking algorithm.  Similarly, even when remapping `-` to the `MidLetter`
property, `full-` will be remapped to `full` + `-` before additional text
is typed.

Of course, this problem does alleviate itself once another `ALetter`-property
letter is typed, but suppose we wanted a rule to prevent word-breaking for
the second example above.  (After all, `can'` could be the end of a quoted
phrase in English - `'sure you can'` - in which case we might want the split
to occur.)

Revisiting [an earlier example](#example) and simplifying a little bit:

```typescript
/*** Definition of extra word-breaking rules ***/
{
  rules: [{
    match: (context) => {
      if(context.propertyMatch(null, ["ALetter"], ["Hyphen"], ["eot"])) {
        return true;
      } else {
        /* ... */
      } else {
        return false;
      }
    },
    breakIfMatch: false
  }],

  /*** Character class overrides for specific characters ***/
  propertyMapping: (char) => {
    let hyphens = ['\u002d', '\u2010', '\u058a', '\u30a0'];
    if(hyphens.includes(char)) {
        return "Hyphen";
    } else {
      // Use the default properties for anything else.
      return null;
    }
  },

  /*** Declares any new, custom character classes to be recognized by the word-breaker ***/
  customProperties: ["Hyphen"]
}
```

Let's walk through what this simplification is trying to achieve:

1. Hyphens are mapped to their own distinct word-breaking property.
2. The custom rule prevents wordbreaking between a letter and a hyphen at the end of text.
    - It does not include any of the `MidLetter` characters.

Matching the rule against the end of text suggests that what follows the `Hyphen` character
is the point of text insertion.  For this example, assuming that a user has just typed
`full-`, there will be no word-break on the hyphen until either more input is received or
the user changes the site of text entry.

**Important note**: You must declare any custom properties within the `customProperties` array.
If any are missing, the missing custom properties will fail to match against any word-breaking rule.
Make sure you don't misspell it anywhere in your customization code!

### Default rules + custom properties

There is one notable issue with this example, though - whenever you remap a character to
a new property, it is no longer considered to have its old property, and so it will no
longer match rules based on its default property. This is why the original example
included a couple of extra rules:

```typescript
{
  match: (context) => {
    // Extend WB6 - allow "Hyphen" in the same place as "MidLetter"
    if(context.propertyMatch(null, ["ALetter"], ["Hyphen"], ["ALetter"])) {
      return true;
    // Extend WB7 - allow "Hyphen" in the same place as "MidLetter"
    } else if(context.propertyMatch(["ALetter"], ["Hyphen"], ["ALetter"], null)) {
      return true;
    // The same rule from above.
    } else if(context.propertyMatch(null, ["ALetter"], ["Hyphen"], ["eot"])) {
      return true;
    } else {
      return false;
    }
  },
  breakIfMatch: false
}
```

By replicating [WB6](#WB6) and WB7's structure and allowing `Hyphen` to match in the same
position as `MidLetter` in the original rules, we can prevent word-breaking splits
after additional text has been typed after a `Hyphen`-property character.  This does not
_replace_ the behavior of WB6 and WB7 - it merely _extends_ it to include the new property.

### A more complex case

A meatier example may be found as [the specification's hypothetical rule WB5a](https://unicode.org/reports/tr29/#WB999):

> "Break between apostrophe and vowels (French, Italian)"
>
> WB5a:   **Apostrophe ÷ Vowels**.

The idea of this rule is to allow words such as the French `l'objectif` to be split
into the article - `l'` and its following word - `objectif` while preserving other
cases that should still be treated as single words, such as `aujourd'hui`.

To simplify the code needed for customization here somewhat, we will use `Single_Quote`
in place of `Apostrophe`, as well as a few extra simplifications.  The new `Vowels`
property offers enough complexity as it is.

```typescript
let customization = {
  rules: [
    // WB5 extension - ensure `AVowel` is handled like `ALetter`.
    {
      match: (context) => {
        if(context.propertyMatch(null, ["ALetter", "AVowel"], ["ALetter", "AVowel"], null)) {
          return true;
        } else {
          return false;
        }
      },
      breakIfMatch: false
    },
    // Our main goal WB5a
    {
      match: (context) => {
        if(context.propertyMatch(null, ["Single_Quote"], ["AVowel"], null)) {
          return true;
        } else {
          return false;
        }
      },
      breakIfMatch: true
    },
    // WB6, 7 extension - ensure `AVowel` is handled like `ALetter`
    {
      match: (context) => {
        if(context.propertyMatch(null,
                                  ["ALetter", "AVowel"],
                                  ["MidLetter", "MidNumLet", "Single_Quote"],
                                  ["ALetter", "AVowel"])) {
          return true;
        } else if(context.propertyMatch(["ALetter", "AVowel"],
                                        ["MidLetter", "MidNumLet", "Single_Quote"],
                                        ["ALetter", "AVowel"],
                                        null)) {
          return true;
        } else {
          return false;
        }
      },
      breakIfMatch: false
    }
    // Similar extensions to WB9, 10, 13a, and 13b would also be needed for robustness.
    // Note: we have left "Hebrew_Letter" out of the WB5, 6, and 7 rewrites to help
    // simplify this example.
  ],
  propertyMapping: (char) => {
    const vowels = ['a', 'e', 'i', 'o', 'u'];
    // French and Italian allow accented vowels; this will strip off the accent and
    // leave us with the base vowel.
    const baseChar = char.normalize('NFD').charAt(0);
    if(vowels.includes(baseChar)) {
      return "AVowel";
    }

    return null;
  },
  customProperties: ["AVowel"]
}
```

Note that we have left out some of the rule extensions that would help cover certain
less-frequently encountered cases.  These may matter to some parts of a
language community, especially for models targeting a majority language.
Computer programmers in particular tend to care about the un-extended
rules (WB9, 10, 13a, and 13b) more than most.

Remember, remapping characters to a new word-breaking property prevents any default
rule from handling them unless you add custom rules to re-include them as their
new property.

------

[Return to “Advanced Lexical Model Topics”](./)
