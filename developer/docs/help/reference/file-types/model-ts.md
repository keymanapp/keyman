---
title: MODEL.TS files
---

Used by:
:   <span class="application">Keyman Developer</span>.

Description:
:   A .MODEL.TS file is a [lexical model](../../guides/lexical-models)
    definition source file. This holds all the code used by a Keyman
    lexical model, in plain text.

Details:
:   A .MODEL.TS file is written in the
    [TypeScript](https://www.typescriptlang.org/) language.
    <span class="application">Keyman Developer</span> compiles this
    Keyman lexical model source file which can also reference a
    ([.TSV](tsv)) wordlist to make a lexical model
    ([.MODEL.JS](model-js)) file.

## Reference

This is a small [TypeScript](https://www.typescriptlang.org/) source
code file that tells us where to find the word list file, as well as
gives us the option to tell the compiler a little bit more about our
language’s spelling system or *orthography*.

## The model definition template

**Keyman Developer** will provide you with a model definition similar to
the following.

``` typescript
/*
  sencoten 1.0 generated from template.

  This is a minimal lexical model source that uses a tab delimited wordlist.
  See documentation online at https://help.keyman.com/developer/ for
  additional parameters.
*/


const source: LexicalModelSource = {
  format: 'trie-1.0',
  wordBreaker: {
    use: 'default',
  },
  sources: ['wordlist.tsv'],
};
export default source;
```

Let's step through this file, line-by-line.

On the first line, we're declaring the source code of a new lexical
model.

``` typescript
const source: LexicalModelSource = {
```

On the second line, we're saying the lexical model will use the
`trie-1.0` format. The `trie` format creates a lexical model from one or
more word lists; the `trie` structures the lexical model such that it
can predict through thousands of words very quickly.

``` typescript
  format: 'trie-1.0',
```

On lines 3–5, we're specifying the word breaking algorithm that we want
to use. Keyman supplies a default algorithm that conforms to the rules
expected for many Latin-script languages.

``` typescript
  wordBreaker: {
    use: 'default',
  },
```

On the sixth line, we're telling the `trie` where to find our wordlist.

``` typescript
  sources: ['wordlist.tsv'],
```

The seventh line marks the termination of the lexical model source code.
If we specify any customizations, they **must** be declared above this
line:

``` typescript
};
```

The eighth line is necessary to allow external applications to read the
lexical model source code.

``` typescript
export default source;
```

## Customizing our lexical model

The template, as described in the previous section, is a good starting
point, and may be all you need for you language. However, most language
require a few customizations. The `trie` model supports the following
customizations:

word breaking
:   How to determine when words start and end in the writing system.

search term to key
:   How and when to ignore accents and lettercase

### Word breaking

The `trie` family of lexical models needs to know what a word is in
running text. In language using the Latin script—like, English, French,
and SENĆOŦEN—finding words is easy. Words are separated by spaces or
punctuation. The actual rules for where to find words can get quite
tricky to describe, but Keyman implements the [Unicode Standard Annex
\#29 §4.1 Default Word Boundary
Specification](https://unicode.org/reports/tr29/#Word_Boundaries) which
works well for most languages. If the default doesn't *quite* work for
your language, you can [tweak
it](../../guides/lexical-models/advanced/word-breaker#join).

However, in languages written in other scripts—especially East Asian
scripts like Chinese, Japanese, Khmer, Lao, and Thai—there are no
obvious break in between words. For these languages, there must be
special rules for determining when words start and stop. This is what a
<span class="dfn">word breaking function</span> is responsible for. It
is a little bit of code that looks at some text to determine where the
words are.

### Search term to key

To look up words quickly, the `trie` model creates a <span class="dfn">
search key </span> that takes the latest word (as determined by the
[word breaking](#toc-word-breaking) and converts it into a “regular”
form. The purpose of this “regular” form is to make searching for a word
work, regardless of things such as **accents**, **diacritics**,
**lettercase**, and minor **spelling variations**. The ”regular” form is
called the <span class="dfn">key</span>. Typically, the key is always in
lowercase, and lacks all accents and diacritics. For example, the key of
“naïve" is "naive" and the key of Canada is “canada”.

The form of the word that is stored is “regularized” through the use of
a <span class="dfn">key function</span>, which you can define in
TypeScript code.

The key function takes a string, the raw search term, and returns a
string, being the “regular” key. As an example, consider the **default
key function**; that is, the key function that is used if you do not
specify one:

``` 
searchTermToKey: function (term) {
  // Use this pattern to remove common diacritical marks (accents).
  // See: https://www.compart.com/en/unicode/block/U+0300
  const COMBINING_DIACRITICAL_MARKS = /[\u0300-\u036f]/g;

  // Lowercase each letter in the string INDIVIDUALLY.
  // Why individually? Some languages have context-sensitive lowercasing
  // rules (e.g., Greek), which we would like to avoid.
  // So we convert the string into an array of code points (Array.from(term)),
  // convert each individual code point to lowercase (.map(c => c.toLowerCase())),
  // and join the pieces back together again (.join(''))
  let lowercasedTerm = Array.from(term).map(c => c.toLowerCase()).join('');

  // Once it's lowercased, we convert it to NFKD normalization form
  // This does many things, such as:
  //
  //  - separating characters from their accents/diacritics
  //      e.g., "ï" -> "i" + "¨" (U+0308)
  //  - converting lookalike characters to a canonical ("regular") form
  //      e.g., ";" -> ";" (yes, those are two completely different characters -- U+037E and U+003B!)
  //  - converting "compatible" characters to their canonical ("regular") form
  //      e.g., "𝔥𝔢𝔩𝔩𝔬" -> "hello"
  let normalizedTerm = lowercasedTerm.normalize('NFKD');

  // Now, using the pattern defined above, replace each accent and diacritic with the
  // empty string. This effectively removes all accents and diacritics!
  //
  // e.g.,  "i" + "¨" (U+0308) -> "i"
  let termWithoutDiacritics = normalizedTerm.replace(COMBINING_DIACRITICAL_MARKS, '');

  // The resultant key is lowercased, and has no accents or diacritics.
  return termWithoutDiacritics;
},
```

This should be sufficient for most Latin-based writing systems. However,
there are cases, such as with SENĆOŦEN, where some characters do not
decompose into a base letter and a diacritic. In this case, it is
necessary to write your own key function.

## Version History

Keyman 12
:   **Added**: the `.model.ts` file type.

Keyman 13
:   *No changes.*

Keyman 14
:   **Added**: an alternative syntax for specifying word breakers:
    `wordBreaker: { 'use': ... }`.
:   **Added**: specify which characters should be used to join with word
    breakers: `wordBreaker: { 'joinWordsAt': ... }`.
