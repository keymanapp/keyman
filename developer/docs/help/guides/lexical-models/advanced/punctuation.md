---
title: Punctuation
---

The lexical models use two different kinds of punctuation:

-   Quotation marks around the “keep” suggestion.
-   What kind of space to insert after words, if any.

Both of these can be customized by adding the punctuation to [model definition file](./model-definition-file). Here is a full example of a model definition file using default punctuation:

```typescript
const source: LexicalModelSource = {
  format: 'trie-1.0',
  sources: ['wordlist.tsv'],
  // CUSTOMIZE THIS:
  punctuation: {
    quotesForKeepSuggestion: {
      open: "“", close: "”"
    },
    insertAfterWord: " ",
  },
  // other customizations go here:
};

export default source;
```

## Customizing `quotesForKeepSuggestion`

These are the quotation marks that surrond the “keep” suggestion when
it's displayed in the suggestion bar. By defaut, the quotations used are
“smart” quotation marks used in English typography. Namely, the **open
quote** is `“` U+201C LEFT DOUBLE QUOTATION MARK, and the **close
quote** is `”` U+201D RIGHT DOUBLE QUOTATION MARK.

Let's customize this to use `«` and `»` for the open and close quote, respectively. To do this, change the part labeled
`quotesForKeepSuggestion`:

```typescript
punctuation: {
  quotesForKeepSuggestion: {
    open: "«", close: "»"
  },
},
```

## Customizing `insertAfterWord`

Many languages insert a space after a word. Some languages, like Thai or Khmer, do not use spaces. To suppress the space, you may set `insertAfterWord` to the empty string:

```typescript
punctuation: {
  insertAfterWord: "",
},
```

You can even use an alternate spacing character, if required by your language:

```typescript
punctuation: {
  insertAfterWord: "\u200B", // U+200B ZERO WIDTH SPACE
},
```

## See also

[The TypeScript definition of
`LexicalModelPunctuation`](https://github.com/keymanapp/keyman/blob/4211b468949860b8fb4a4707710472ab9e33c581/common/lexical-model-types/index.d.ts#L328-L371)

------------------------------------------------------------------------

[Return to “Advanced Lexical Model Topics”](./)