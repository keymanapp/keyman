---
title: Search term to key
---

To look up words quickly, the `trie` model creates a _search key_
that takes the latest word (as determined by the
[word breaker](word-breaker)) and converts it into an internal form. The
purpose of this internal form is to make searching for a word work, as
expected, regardless of things such as **accents**, **diacritics**,
**letter case**, and minor **spelling variations**. The internal form is
called the _key_. Typically, the key is always in
lowercase, and lacks all accents and diacritics. For example, the key
for “naïve" is `naive` and the key for “Canada” is `canada`.

The form of the word that is stored is “regularized” through the use of a _key function_, which you can define in TypeScript code.

> ### Note: 
This function runs both **on every word when the wordlist is compiled** and **on the input, whenever a suggestion is requested**. This way, whatever a user types is *matched* to something stored in the lexical model, without the user having to type things in a specific way.

The key function takes a string which is the raw search term, and
returns a new string, being the “regularized” key. As an example,
consider the **default key function**; that is, the key function that is
used if you do not specify one:

```typescript
searchTermToKey: function (term: string): string {
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
  //      e.g., "ï" -> "i" + "◌̈"
  //  - converting lookalike characters to a canonical ("regular") form
  //      e.g., ";" -> ";" (yes, those are two completely different characters!)
  //  - converting "compatible" characters to their canonical ("regular") form
  //      e.g., "𝔥𝔢𝔩𝔩𝔬" -> "hello"
  let normalizedTerm = lowercasedTerm.normalize('NFKD');

  // Now, using the pattern defined above, replace each accent and diacritic with the
  // empty string. This effectively removes all accents and diacritics!
  //
  // e.g.,  "i" + "◌̈" -> "i"
  let termWithoutDiacritics = normalizedTerm.replace(COMBINING_DIACRITICAL_MARKS, '');

  // The resultant key is lowercased, and has no accents or diacritics.
  return termWithoutDiacritics;
},
```

This should be sufficient for most Latin-based writing systems. However,
there are cases, such as with SENĆOŦEN, where some characters do not
decompose into a base letter and a diacritic. In this case, it is
necessary to write your own key function.

## Use in your model definition file

To use this in your model definition file, provide a function as the `searchTermToKey` property of the lexical model source:

```typescript
const source: LexicalModelSource = {
  format: 'trie-1.0',
  sources: ['wordlist.tsv'],
  searchTermToKey: function (wordform: string): string {
    // Your searchTermToKey function goes here!
    let key = wordform.toLowerCase();
    return key;
  },
  // other customizations go here:
};

export default source;
```

## Suggested customizations

- For all writing systems, **normalize into NFKD** or **NFKC** form using `wordform = wordform.normalize('NFKD')`.
- For Latin-based scripts, **lowercase** the word, and **remove diacritics**.
- For scripts that use the U+200C zero-width joiner (ZWJ) and/or the U+200D zero-width non-joiner (ZWNJ) (e.g., Brahmic scripts), **remove the ZWJ or ZWNJ** from the **end** of the input with `wordform = wordform.replace(/[\u200C\u200D]+$/`

------------------------------------------------------------------------

[Return to “Advanced Lexical Model Topics”](./)