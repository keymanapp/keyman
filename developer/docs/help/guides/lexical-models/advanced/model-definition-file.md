---
title: The model definition file
---

This is a small [TypeScript](https://www.typescriptlang.org/) source
code file that tells us how to define our model.

In the case of the **wordlist lexical models**, the model definition
file indicates where to find the [TSV source files](../../../reference/file-types/tsv), as well as gives us the option to tell the compiler a little bit more about our language’s spelling system or *orthography*.

## The model definition template

**Keyman Developer** provides a default model definition similar to the
following. If you want to create the file yourself, copy-paste the
following template, and save it as `model.ts`. Place this file in the
same folder as `wordlist.tsv`.

```typescript
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

```typescript
const source: LexicalModelSource = {
```

On the second line, we're saying the lexical model will use the
`trie-1.0` format. The `trie` format creates a lexical model from one or
more word lists; the `trie` structures the lexical model such that it
can predict through thousands of words very quickly.

```typescript
  format: 'trie-1.0',
```

On lines 3–5, we're specifying the word breaking algorithm that we want
to use. Keyman supplies a default algorithm that conforms to the rules
expected for many languages.

```typescript
  wordBreaker: {
  use: 'default',
},
```

On the sixth line, we're telling the `trie` where to find our wordlist.

```typescript
  sources: ['wordlist.tsv'],
```

The seventh line marks the termination of the lexical model source code.
If we specify any customizations, they **must** be declared above this
line:

```typescript
};
```

The eighth line is necessary to allow external applications to read the
lexical model source code.

```typescript
export default source;
```

## Customizing our lexical model

The template, as described in the previous section, is a good starting
point, and may be all you need for your language. However, most language
require a few customizations. The `trie-1.0` wordlist model supports the
following customizations:

[Punctuation](punctuation)
:   How to define certain punctuation in your language.

[Word breaker](word-breaker)
:   How to determine when words start and end in the writing system.

[Search term to key](search-term-to-key)
:   How and when to ignore accents and letter case.

To see all of the things possible in a model definition file, see the [`LexicalModelSource` interface](https://github.com/keymanapp/keyman/blob/stable-15.0/developer/js/source/lexical-model-compiler/lexical-model.ts#L95-L146).