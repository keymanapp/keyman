---
title: Custom Editor Themes
---

Keyman Developer embeds the Monaco open source text editor. The
highlighting and styles for the text editor are customisable with a JSON
file configured in the [Options dialog](../context/options). The JSON
file must be a valid [Monaco theme](https://microsoft.github.io/monaco-editor/playground.html#customizing-the-appearence-tokens-and-colors)
(passed as second parameter to `monaco.editor.defineTheme`).

## Example

```js
{
  "base": "vs-dark",
  "inherit": true,
  "rules": [
    { "token": "comment", "foreground": "ffa500", "background": "303030", "fontStyle": "italic underline" },
    { "token": "comment.js", "foreground": "008800", "fontStyle": "bold" },
    { "token": "comment.css", "foreground": "0000ff", "fontStyle": "bold", "inherit": false, "background": "808080" }
  ]
}
```

## Keyman keyboard language tokens

The Keyman keyboard language highlighting uses the following token
names:

| Token          | Source Element                                  |
|----------------|-------------------------------------------------|
| annotation     | System store names                              |
| bracket        |                                                 |
| comment        | Comments                                        |
| identifier     | Store and Group names                           |
| invalid        | Invalid code                                    |
| keyword        | Keywords such as `begin`, `any`                 |
| number         | Decimal character codes, deprecated, e.g. `d65` |
| number.hex     | Unicode character codes, e.g. `U+1234`          |
| number.octal   | Character codes expressed in octal              |
| operator       | `+`, `>` and other operators                    |
| string         | Strings                                         |
| string.invalid | Unterminated strings                            |
| string.quote   | `'` and `"` characters                          |
| tag            | Virtual keys                                    |
| white          |                                                 |