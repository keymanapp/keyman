---
title: Adding Keyboards
---

There are multiple ways to add and install keyboards into your KeymanWeb installation.

## Requesting from the Keyman cloud (CDN)

The easiest way to utilize a keyboard is to request it from the Keyman cloud.

### Requested from the CDN by keyboard name

To obtain a specific keyboard by name or by keyboard name and language code as a pair, see the
following:

```typescript
await keyman.addKeyboards('khmer_angkor','sil_euro_latin@sv','sil_euro_latin@no')
```

This will install three keyboards - the Khmer Angkor one for Khmer and two copies
of the EuroLatin keyboard - one for Swedish and one for Norwegian.

### Requested from the CDN by language name

To obtain the default Keyman keyboard for a given language, call the following function:

```typescript
await keyman.addKeyboardsForLanguage('Dzongkha');
```

This example would find the default keyboard for the Dzongkha language. This method will fail if
the name doesn't perfectly match any language found in the CDN's repository.

Alternatively, languages may be looked up via their BCP 47 language code as follows:

```typescript
await keyman.addKeyboards('@he')
```

The `@` prefix indicates the use of the BCP 47 language code, which in this case corresponds with
Hebrew.

To add all keyboards for a language at once, `$` can be appended to the language
name or language code:

```typescript
await keyman.addKeyboards('@fr$');
// or
await keyman.addKeyboardsForLanguage('French$');
```

## Directly linking a local copy

The most efficient way to utilize a keyboard is to obtain a local copy of it and place this copy
in a static location on your website. Once this is done, it can be directly linked into KeymanWeb
as follows:

```typescript
await keyman.addKeyboards({
  id: 'us',                  // The keyboard's unique identification code.
  name: 'English',           // The keyboard's user-readable name.
  languages: {
    id: 'en',                // A BCP 47 code uniquely identifying the language.
    name: 'English (US)'     // The language's name.
  },
  filename: './us.js'    // A valid path to the compiled *.js file representing the keyboard.
});
```

Custom fonts may also be utilized via the `languages.font` property. For example:

```typescript
languages:{
  id:'lo',
  name:'Lao',
  font:{
    family:'LaoWeb',
    source:['../font/saysettha_web.ttf','../font/saysettha_web.woff']
  }
}
```

[Previous: What is a Keyboard?](what-is-a-keyboard) | [Next: Additional Examples](examples/)
