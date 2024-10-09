---
title: Step 1: Planning the Keyboard
---

## Choosing the characters

First of all, we need to decide which characters we want the keyboard to
produce. Next, we must find out the codes used to represent them, using
a program such as Character Map, or with the Character Map in Keyman
Developer (go to **View**, **Character Map**).

French uses the same 26 letters as English, with some additions. As our
keyboard is based on English, we only need to work with these additional
letters. Note that for completeness, we will design our keyboard to
produce a few other accented vowels that are not used in French. Also,
we want our keyboard to include the angled quotes `«` and `»`.

These characters, with both uppercase and lowercase forms, are listed in
the table below along with their Unicode codes.

|     |          |     |          |     |     |          |     |          |
|-----|----------|-----|----------|-----|-----|----------|-----|----------|
| `À` | `U+00C0` | `à` | `U+00E0` |     | `Á` | `U+00C1` | `á` | `U+00E1` |
| `È` | `U+00C8` | `è` | `U+00E8` |     | `É` | `U+00C9` | `é` | `U+00E9` |
| `Ì` | `U+00CC` | `ì` | `U+00EC` |     | `Í` | `U+00CD` | `í` | `U+00ED` |
| `Ò` | `U+00D2` | `ò` | `U+00F2` |     | `Ó` | `U+00D3` | `ó` | `U+00F3` |
| `Ù` | `U+00D9` | `ù` | `U+00F9` |     | `Ú` | `U+00DA` | `ú` | `U+00FA` |
| `Â` | `U+00C2` | `â` | `U+00E2` |     | `Ä` | `U+00C4` | `ä` | `U+00E4` |
| `Ê` | `U+00CA` | `ê` | `U+00EA` |     | `Ë` | `U+00CB` | `ë` | `U+00EB` |
| `Î` | `U+00CE` | `î` | `U+00EE` |     | `Ï` | `U+00CF` | `ï` | `U+00EF` |
| `Ô` | `U+00D4` | `ô` | `U+00F4` |     | `Ö` | `U+00D6` | `ö` | `U+00F6` |
| `Û` | `U+00DB` | `û` | `U+00FB` |     | `Ü` | `U+00DC` | `ü` | `U+00FC` |
| `Ý` | `U+00DD` | `ý` | `U+00FD` |     | `Ç` | `U+00C7` | `ç` | `U+00E7` |
| `«` | `U+00AB` | `»` | `U+00BB` |     |     |          |     |          |

If you are not familiar with the hexadecimal (base-16) numbering system,
don't worry: you can use the Character Map in Keyman Developer to find
the character you want, and then drag-and-drop or copy-and-paste its
character code into your keyboard.

Note that you must be careful to use the right character: The Unicode
standard has many characters with the same shape as another, but a
different meaning; an example of this is the Greek capital letter Sigma
(`U+03A3` Σ) and the mathematical summation symbol (`U+2211`, ∑). An application
supporting Unicode would treat these two characters differently. If in
doubt whether a character is the right one, you can look up the
reference tables at [www.unicode.org](http://www.unicode.org/).

## Designing the layout

After choosing the characters we want our keyboard to use, we must
decide how we want the user to be able to enter them. For some
languages, you might replace each letter on the English keyboard with a
letter from the language. In this case, however, most of the letters are
accented vowels, we will use two keystrokes for each: one for the
accent, and one for the vowel.

| Character   | Keystrokes                    |
|-------------|-------------------------------|
| `À, à, ...` | back-quote (<kbd>`</kbd>), then the vowel key.    |
| `Á, á, ...` | quote (<kbd>'</kbd>), then the vowel key.          |
| `Â, â, ...` | caret (<kbd>^</kbd>), then the vowel key.          |
| `Ä, ä, ...` | double-quote (<kbd>"</kbd>), then the vowel key.   |
| `Ç, ç`      | quote (<kbd>'</kbd>), then lower- or uppercase C.  |
| `«, »`      | two less-than (<kbd>&lt;&lt;</kbd>) or greater-than (<kbd>&gt;&gt;</kbd>) symbols. |

Now that we have decided which character to use and how the user can
enter them, we can start to write the keyboard.

-   [Continue with Step 2: Writing the Header](step-2)
-   [Back to the Introduction](index)