---
title: Word breaker
---

The `trie` family of lexical models needs to know what a word is in
running text. In languages using the Latin script—like, English, French,
and SENĆOŦEN—finding words is easy. Words are separated by spaces or
punctuation. The actual rules for where to find words can get quite
tricky to describe, but Keyman implements the [Unicode Standard Annex #29 §4.1 Default Word Boundary Specification](https://unicode.org/reports/tr29/#Word_Boundaries) 
which works well for most languages.

However, in languages written in other scripts — especially East Asian
scripts like Chinese, Japanese, Khmer, Lao, and Thai — there are no obvious break in between words. For these languages, there must be special rules for determining when words start and stop. This is what a _word breaker function_ is responsible for. It is a little bit of code that looks at some text to determine where the words are.

You can customize the word breaker in three ways:
- If your language uses its writing system in an unconventional way (e.g., use spaces to separate words in Thai, Lao, Burmese, or Khmer), you can [override the script's default behaviour](#overrides)
- If the default word breaker creates **too many splits**, you can [choose which strings join words together](#join).
- If the default word breaker creates **not enough splits**, you must [create your own word breaker function](#custom).
- Alternatively, you may choose to [customize and extend the wordbreaker's behavior](./unicode-breaker-extension) by adding extra rules and changing how it treats specific characters.

## Overriding script defaults

The default word breaker makes assumptions about how each
_script_ (alphabet, syllabary, or writing system)
works. You can override the defaults by specifying the
`overrideScriptDefaults` option.

There is currently only one override:

`'break-words-at-spaces'`
:   Only breaks words at spaces for scripts that otherwise do not use spaces in between words.

### Break words at spaces

This applies only to languages that borrow the **Burmese**, **Khmer**,
**Lao**, or **Thai** scripts. The majority languages for these scripts
do *not* use spaces in between words; hence, the default word breaker
will produce undesired results when breaking words in these scripts.
However, if your language is written in one of these scripts and *does*
use spaces in between words, then you can set
`overrideScriptDefaults: 'break-words-at-spaces',` to ensure word breaks
do not occur in the middle of words, but instead, at spaces.

Your model definition file should look like this:

```typescript
const source: LexicalModelSource = {
  format: 'trie-1.0',
  sources: ['wordlist.tsv'],
  wordBreaker: {
    use: 'default',  // we want to use the default word breaker, BUT!
    // Override the default for Burmese, Khmer, Lao, or Thai:
    overrideScriptDefaults: 'break-words-at-spaces',
  }
};

export default source;
```

## Customize joining rules

The default word breaker is very liberal in what it considers is a word.

For instance, the default word breaker will split words at hyphens.
Consider the following Plains Cree example; this is a single word:

:   amiskwaciy-wâskahikan

However, the default word breaker will produce three words: `amiskwaciy`, `-`, and `wâskahikan`.

To **join words at hyphens and any other punctuation**, provide the
`joinWordsAt` option in the [model definition file](./model-definition-file):

```typescript
const source: LexicalModelSource = {
  format: 'trie-1.0',
  sources: ['wordlist.tsv'],
  wordBreaker: {
    use: 'default',     // we want to use the default word breaker, BUT!
    // CUSTOMIZE THIS:
    joinWordsAt: ['-'], // join words that contain hyphens
  }
};

export default source;
```

You can specify one or more strings to join words at:

```typescript
const source: LexicalModelSource = {
  format: 'trie-1.0',
  sources: ['wordlist.tsv'],
  wordBreaker: {
    use: 'default',
    // CUSTOMIZE THIS:
    joinWordsAt: ['-', ':', '@'], // join words at hyphens, colons, at-signs
  }
};

export default source;
```

## Writing a custom word breaker function

> **Note**: 
If your language uses spaces to denote word breaks, the
default word breaker is probably sufficient. Only customize this if you
know the default word breaker really does not work for your language!

The word breaker function can be specified in the [model definition file](./model-definition-file) as follows:

```typescript
const source: LexicalModelSource = {
  format: 'trie-1.0',
  sources: ['wordlist.tsv'],
  // CUSTOMIZE THIS:
  wordBreaker: {
    use: function(text: string): Span[] {
      // Return zero or more **spans** of text:
      return [];
    },
  },
  // other customizations go here:
};

export default source;
```

The function must return zero or more `Span` objects. The spans,
representing an indivisible span of text, must be in ascending order of
their start point, and they must be non-overlapping.

### A `Span` object

A _span_ is an indivisible piece of a sentence.
This is typically a word, but it can also be a series of spaces, an
emoji, or a series of punctuation characters. **A span that looks like a word is treated like a word in the `trie-1.0` model**.

A `span` has the following properties:

```typescript
{
  start: number;
  end: number;
  length: number;
  text: string;
}
```

The `start` and `end` properties are indices into the original string at
which the span begins, and the index at which the *next* span begins.

`length` is `end - start`.

`text` is the actual text of the string contained within the span.

## Example for English

Here is a full example of word breaker function that returns an array of
spans in an ASCII (English) string. 

> **Note**
This is just an example—please
use the default word breaker for English text!

```typescript
const source: LexicalModelSource = {
  format: 'trie-1.0',
  sources: ['wordlist.tsv'],
  // EXAMPLE BEGINS HERE:
  wordBreaker: function(text: string): Span[] {
    // A span derived from a JavaScript RegExp match array:
    class RegExpDerivedSpan implements Span {
      readonly text: string;
      readonly start: number;

      constructor(text: string, start: number) {
        this.text = text;
        this.start = start;
      }

      get length(): number {
        return this.text.length;
      }

      get end(): number {
        return this.start + this.text.length;
      }
    }

    let matchWord = /[A-Za-z0-9']+/g;
    let words: Span[] = [];
    let match: RegExpExecArray;
    while ((match = matchWord.exec(phrase)) !== null) {
      words.push(new RegExpDerivedSpan(match[0], match.index));
    }

    return words;
  },
  // other customizations go here:
};

export default source;
```

## See also

- [The TypeScript definition of `WordBreakingFunction` and
`Span`](https://github.com/keymanapp/keyman/blob/4211b468949860b8fb4a4707710472ab9e33c581/common/lexical-model-types/index.d.ts#L286-L323)  

- [Extension and customization of the Unicode word-breaker](./unicode-breaker-extension)  

- [The Unicode Standard Annex \#29 §4.1 Default Word Boundary Specification](https://unicode.org/reports/tr29/#Word_Boundaries)

------------------------------------------------------------------------

[Return to “Advanced Lexical Model Topics”](./)