---
title: Designing a Desktop Layout
---

Part 2 of the [Keyman Developer Walkthrough](../walkthrough).

[← Back: Part 1 - Creating a Keyboard Project](01-creating-keyboard-project)

[Next: Part 3 - Creating a Desktop Layout →](03-creating-desktop-layout)

## Step-by-Step

Dagbani uses the characters **ɛ ɔ ŋ ɣ ʒ** and the corresponding capital letters **Ɛ Ɔ Ŋ Ɣ Ʒ**.
Rather than try to assign each of these characters to a key on the physical keyboard,
we will use a pair of keys to produce each of these characters.
We’ll start with the default QWERTY layout and add rules so that typing <kbd>;</kbd> followed by <kbd>e</kbd> will produce “ɛ”,
<kbd>;</kbd> followed by <kbd>o</kbd> will produce “ɔ”, etc.

This Step-by-Step tutorial won’t be using the visual layout features of Keyman Developer for creating the desktop keyboard.

To continue the Step-by-Step tutorial move to the next page: [Part 3 - Creating a Desktop Layout](03-creating-desktop-layout)

---

## Defining a List of Keys

After you have gathered some basic information about the language for which you are creating a keyboard,
along with the needs of the people who will be using it,
the next step is to figure out what format will suit the needs of your keyboard’s users and be most familiar to them.

Keyman is available across [many different platforms](https://help.keyman.com/products/), so consider whether you need to design keyboard formats for computer keyboards, phones, or tablets.
Keyman makes it easy to keep designs consistent across these types, but it is also possible to design each layout individually.

Make a list of all of the characters that your language uses in its written form. For example, the Dagbani language uses the following characters, in addition to a standard A-Z alphabet:

|  Name  |  Character  |
|--------|-------------|
| Open e | ɛ Ɛ         |
| Open o | ɔ Ɔ         |
| Eng    | ŋ Ŋ         |
| Gamma  | ɣ Ɣ         |
| Ezh    | ʒ Ʒ         |


This is a simple example. Some languages may have extensive punctuation marks, special glyphs, or use a non-Latin alphabet.
Be thorough in your list so that you are not missing any keys when you design your keyboard.

Note that Unicode contains some characters that are quite similar, so verify the Unicode values you are using.
For example, this example uses `ɔ` (`U+0254`) "open o", not `ↄ` (`U+2184`) "reversed c".

## Creating a Visual Layout

Once you know what keys your keyboard will need, it is time to determine how to arrange them.
The best way to do this varies widely depending on your language.
If users have been typing with a certain keyboard layout and are familiar with it, that might provide a good starting point.
Research existing keyboard layouts and build off of them where possible.
You may want to use the keyboard layout common in the area (QWERTY in English-speaking areas, AZERTY in French-speaking areas, etc.) as a basis for your keyboard.

For visual mapping, this can be done with tools as rudimentary as a pen and paper, but Keyman Developer also offers a handy visual keyboard for design purposes.

Navigate to your `.kpj` file, go to the `Keyboards` tab, and open your `.kmn` file. This is where your keyboard’s information lies, as well as the code that makes it work.

You can read more about the `.kmn` file type in the [official Keyman documentation](https://help.keyman.com/developer/current-version/reference/file-types/kmn).

Inside your `.kmn` file, select the `Layout` tab on the left.
For a new project, the `Design` tab (at the bottom) should already be selected.
This is a visual representation of your desktop keyboard.
Note that once you add rules using the `Code` tab (as, for example, in the Dagbani keyboard developed in the Step-by-Step tutorial),
Keyman Developer no longer offers the `Design` tab option.

By default, Keyman Developer uses a QWERTY layout, but it is very easy to remap this.
If you are using a non-QWERTY base keyboard layout, select each key and press the corresponding key on your keyboard.
This will automatically change the keyboard code to type the key you have selected.

To access the characters needed for your language, you can paste the Unicode characters you need into the two fields at the bottom.
`Unicode Character Value(s)` is for the Unicode value (for example, `U+0061`) of the character you would like the keyboard to type,
while `Output character(s)` is for the character the keyboard will produce (for example, `a`).
Typing in either field will automatically populate the other one with matching information, so it does not matter which one you use.

If a key needs to produce multiple characters (for example, a key that produces `ch`), you can place multiple characters in the `Output character(s)` field.

After some research, you may find that your language has more characters than there are keys on the keyboard.
This is especially true of languages with many glyphs, combined characters, or diacritical marks on letters.
In this case, it is especially helpful to follow a default keyboard layout for your base design,
and plan out special keystrokes so users can type the characters that are not immediately available on the keyboard.
Try to ensure that the most essential keys (usually the ones used to type the alphabet) are available before worrying about adding less frequent characters.

## Planning Keystrokes

Once you know what you want your keyboard layout to look like, you can begin planning what keystrokes will be needed.
You can specify rules so that Keyman will transform typed keystroke combinations into the desired characters.

For example, you can use the keystrokes `'` + `e` to produce the character `é` rather than dedicating a separate key to `é`.

When choosing keys to use for characters you are adding to your keyboard, be careful not to block the normal function of those keys.
In the above example, consider whether there would be a need to allow someone to type an actual apostrophe.

To easily plan keystrokes, you can reference the list you made of the special characters needed for your language. Disregarding the letters and symbols already included in the base layout you designed, how many more do you need to fit onto your keyboard? You can utilize the number row, punctuation characters, deadkeys, and the Right-Alt, Shift, or Shift+Right-Alt modifier keys in your layout. Remember to make them as intuitive as possible for users. If the key combination is too complex, it will slow users down significantly when attempting to type.

Remember to include capital letters in your list! Keyman treats these separately, so you will have to code uppercase and lowercase letters separately.

Here’s an example of how to list your keystrokes. (This can also be reused when you document your keyboard’s layout later!) If you want to be thorough, you can also list the Unicode values for each character to avoid any confusion about which character is used.

| Base | Modifier | Result |
|------|----------|--------|
| a    | ^        | â      |
| e    | ^        | ê      |
| i    | ^        | î      |
| o    | ^        | ô      |
| u    | ^        | û      |

Once you have planned the visual layout of your keyboard and determined how to assign the rest of the keys on the keyboard, it’s time to begin coding.
If your layout is simple enough that you do not need to program additional keys, you do not need to worry as much about the next section.
In some cases, using Keyman’s visual editor is enough for a desktop keyboard.
Note that once you begin adding rules, the keyboard becomes too complex to modify with the visual layout tools, so it is best to do any visual layout work first using the `Design` tab, then add the additional rules using the `Code` tab.

### Deadkeys

The Step-by-Step tutorial uses `;` plus another key (`e E o O n N g G z Z`) to produce the ten additional characters (`ɛ Ɛ ɔ Ɔ ŋ Ŋ ɣ Ɣ ʒ Ʒ`).
When the “;” is typed it appears on the screen.
When one of the ten keys is typed directly after the “;” then the “;” is replaced by the corresponding additional characters.

In contrast to this approach, the “deadkey” alternative could be used.
A deadkey produces no display when typed, but modifies the keystroke that follows it.
To use a deadkey approach for the Dagbani keyboard, we would use a rule to convert the `;` keystroke into a deadkey, which will not display on the screen, but will then be used to create the appropriate additional character when one of the ten characters is typed.
Some European language keyboards (French, for example) use deadkeys, so users in French-speaking countries may already be familiar with their use.

While useful in some situations, deadkeys can also confuse users.
In this example, when the `;` key is pressed, it will not produce anything.
If you include deadkeys in your keyboard, be sure to document them well so that users understand what is happening when they type them.

For more information, see the Keyman documentation for the [`deadkey()` statement](https://help.keyman.com/developer/language/reference/deadkey).

### Reassigning keys

But consider the Dagbani example again.
Suppose we don’t want to have to type two keys to obtain each of these additional characters (as if they were somehow inferior to the other twenty-six).
We could choose to design the keyboard so that typing `[ { ] } - _ = + \ |` will produce `ɛ Ɛ ɔ Ɔ ŋ Ŋ ɣ Ɣ ʒ Ʒ`, respectively.
That could be done with the `Design` mode using the visual layout tools, or by adding rules.
We would then want to add rules to allow the user to type the characters that were on the ten keys that we repurposed to type the additional characters, for example type the `;` key then the `[` key to get an actual `[` character.


[← Back: Part 1 - Creating a Keyboard Project](01-creating-keyboard-project)

[Next: Part 3 - Creating a Desktop Layout →](03-creating-desktop-layout)