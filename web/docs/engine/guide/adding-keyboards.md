---
title: Adding Keyboards
---

There are multiple ways to add and install keyboards into your KeymanWeb installation.

In the examples that follow, please note the use of variable `kmw` as shorthand for the `keyman` object.

## Directly linking a local copy

The most efficient way to utilize a keyboard is to obtain a local copy of it and place this copy in a static location on your website. Once this is done, it can be directly linked into KeymanWeb as follows.

```c
keyman.addKeyboards({
    id:'us',                  // The keyboard's unique identification code.
    name:'English',           // The keyboard's user-readable name.
    language:{
      id:'en',                // A BCP 47 code uniquely identifying the language.
      name:'English (US)'     // The language's name.
    },
    filename:'./us-1.0.js'    // A valid path to the compiled *.js file representing the keyboard.
});
```
Custom fonts may also be utilized via the `language.font` property. For example:

```c
font:{
    family:'LaoWeb',
    source:['../font/saysettha_web.ttf','../font/saysettha_web.woff','../font/saysettha_web.eot']
}
```

## Requested from the CDN by language name

To obtain the default Keyman keyboard for a given language, call the following function.

```c
keyman.addKeyboardsForLanguage('Dzongkha');
```

This example would find the default keyboard for the Dzongkha language. This method will fail if the name doesn't perfectly match any language found in the CDN's repository.

Alternatively, languages may be looked up via their BCP 47 language code as follows:

```c
keyman.addKeyboards('@he')
```

The `@` prefix indicates the use of the BCP 47 language code, which in this case corresponds with Hebrew.

## Requested from the CDN by keyboard name

To obtain a specific keyboard by name or by keyboard name and language code as a pair, see the following:

```c
keyman.addKeyboards('french','european2@sv','european2@no')
```

This will install three keyboards - one for French (named, quite simply, "French") and two copies of the EuroLatin2 keyboard - one for Swedish and one for Norwegian.
