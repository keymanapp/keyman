---
title: Glossary of Key Terms
---

## Core Software & Tools

### Keyman Developer

Keyman Developer is a powerful tool you can use to create custom keyboards optimized to type in any language you choose. Keyboards created with Keyman Developer can be used on Windows, Mac, Linux, iOS, Android, and the web. The Keyman Developer IDE (integrated development environment) is currently Windows-only, however the command line tools it uses can run on other platforms (such as Linux and macOS).

### Keyman

The application that allows users to actually use keyboards created with Keyman Developer. Available on Windows, MacOS, Linux, iOS, Android, and the web. Distinguished from Keyman Developer by its regular (not faded) app icon.

### Keyboard Editor

The primary tool within Keyman Developer used to design and program keyboard layouts, accessible through both drag-and-drop visual design and direct source code editing.

### Package Editor

A tool within Keyman Developer used to bundle keyboard files, fonts, documentation, and other resources into a distributable package file (.kmp).

### Debugger

An interactive tool in Keyman Developer that allows keyboard authors to test keyboard rules, step through complex logic, inspect store contents, and examine context, deadkey state, and output at each point during keyboard operation.

## Keyboard Components & Concepts

### Keyboard Project

A complete project containing all files and resources needed to develop, test, and distribute a keyboard. Organized through a .kpj project file.

### Keyboard Layout

An arrangement of keys (either physical keys on a physical keyboard or key images on the screen of a touch device) allowing a set of characters to be typed. A layout is composed of one or more layers, which are accessed by modifier keys (such as Shift or Alt).

### Keyboard Layer

A subset of the keys in a keyboard layout. This subset is accessed by a modifier key. For example, holding down the Shift key on a physical keyboard gives you access to the Shift layer. Similarly, tapping the “!#1” key on a touch layout presents the Numeric layer.

### Desktop Layout

A keyboard layout designed for use on desktop/laptop computers with physical keyboards (Windows, Mac, Linux). It is also used when a physical keyboard is attached to a touch device. Stored in .kmn files.

### Visual Keyboard / On Screen Keyboard

A graphical representation of the keyboard layout that shows users which keys produce which characters. Files with .kvks extension.

### Touch Layout

A keyboard layout designed for use on touchscreen devices like smartphones and tablets (iOS and Android). Created using the touch layout editor and stored in .keyman-touch-layout files.

### Keyboard Package

A bundled distribution file (.kmp) that contains one or more keyboards along with fonts, documentation, and on-screen keyboard files, ready for installation on user devices.

### Lexical Model Package

A bundled distribution file (.kmp) that contains a lexical model, ready for installation on user devices.

### Keyboard Authors

Individuals or organizations who create custom keyboard layouts using Keyman Developer.

## Lexical Models & Predictive Text

### Lexical Model

A language model that powers predictive text and autocorrect functionality for a keyboard. Also called a “dictionary” in Keyman apps. It uses word lists to suggest completions, corrections, and properly accented variants as a user types.

### Wordlist

A list of words in a language, typically stored in a tab-separated values (.tsv) file, used as the foundation for creating a lexical model. To be effective as a source for suggesting words, each word should have a number indicating its relative frequency in normal usage. This is often accomplished by counting words in a large text sample.

## Programming Concepts

### Deadkey

An invisible placeholder character used in keyboard programming that doesn't appear on screen but can be matched by future rules. Deadkeys allow complex input sequences, such as distinguishing between typing an apostrophe twice for a quote versus once before a vowel for an accent. Keyman supports up to 65,534 unique deadkeys per keyboard.

### Context

The text or characters that precede the current cursor position, which keyboard rules can examine to determine appropriate output.

### Rule

A programming statement in Keyman keyboard language that transforms inputs to outputs, following the pattern: context + keystroke > output.

### Store

A named variable in Keyman keyboard language that holds a set of characters or deadkeys for use in keyboard rules.

### Group

A collection of related keyboard rules organized together for processing keystrokes or manipulating context.

## Language & Encoding

### BCP 47 Tags

Language identification codes following the Best Current Practice 47 standard (e.g., "en" for English, "str-Latn" for Straits Salish in Latin script). These tags consist of:

- **Language subtag**: ISO 639-1 (2-letter) or ISO 639-3 (3-letter) code
- **Script subtag** (optional): ISO 15924 4-letter code for writing system (e.g., "Latn" for Latin, "Cans" for Canadian Aboriginal Syllabics)
- **Region subtag** (optional): ISO 3166-1 alpha-2 country code or UN M49 region code

Keyman uses BCP 47 tags to provide language metadata, facilitate keyboard searches, and associate keyboards with operating system language settings.

### Unicode

The universal character encoding standard that Keyman keyboards fully support, allowing access to characters from any writing system.

## File Types

### .kpj (Keyman Project)

The main project file that contains references to all components: keyboards, models, and packages. Used to organize and manage the entire keyboard development project.

### .kmn (Keyman Source)

A plain text keyboard source file that contains all the code and rules for a Keyman keyboard. This is the human-readable programming file that keyboard authors edit.

### .kvks (Keyman Visual Keyboard Source)

The source file for an on-screen keyboard, created in Keyman Developer's visual keyboard editor.

### .keyman-touch-layout

A JSON format file that describes a keyboard layout specifically for touch devices (phones and tablets).

### .model.ts (Model TypeScript)

A TypeScript source file that defines a lexical model, specifying the format, word-breaking rules, and source files (wordlists) to use for predictive text.

### .tsv (Tab-Separated Values)

A text file containing a list of words, one per line, with an optional tab-separated frequency count, used as source data for creating a lexical model.

### .kps (Keyman Package Source)

A package source file created in the Package Editor that specifies which files (keyboards, fonts, documentation) should be included in a distributable keyboard package or which lexical model files should be included in a distributable lexical model package. 

### .kmx (Keyman Compiled Keyboard)

A compiled binary keyboard file created from a .kmn source file. Used by Keyman on Windows, macOS, and Linux. This is the file used by the Keyman program on the user’s computer to provide keyboard functionality on physical keyboards.

### .kvk (Compiled Keyman Visual Keyboard)

A compiled on-screen keyboard file that shows the visual layout of a keyboard.

### .js (Compiled Keyboard JavaScript)

A JavaScript file created from a .keyman-touch-layout source file, used by Keyman apps on touch devices to provide touch keyboard functionality.

### .model.js (Model JavaScript)

The compiled JavaScript file created from a .model.ts source file, used by Keyman apps to provide predictive text functionality.

### .kmp (Keyman Package)

A compiled, distributable package file that bundles keyboards, fonts, documentation, and other resources into a single installer. This is what end users download and install.

## Distribution & Community

### Keyman Community Forum

An online forum ([community.software.sil.org/c/keyman](http://community.software.sil.org/c/keyman)) where keyboard authors can ask questions, share knowledge, and get support from other developers.

### Keyman GitHub Repository

The open-source code repository ([github.com/keymanapp](http://github.com/keymanapp)) where Keyman software is developed and where users can contribute keyboards, report bugs, and submit improvements.

[**help.keyman.com**](https://help.keyman.com)

The comprehensive online documentation site for Keyman Developer, containing language references, tutorials, guides, and API documentation.

## Advanced Concepts

### Base Layout

The keyboard layout of the system when Keyman is not active. For example, the base layout might be the QWERTY (US English) layout, the AZERTY (French) layout, or a Thai layout, depending on the settings of the device.

### Mnemonic Layout

A keyboard layout where keys are identified by the character produced after the [base layout](#toc-base-layout) is applied, rather than by their physical position. For example, on a US English (QWERTY) keyboard the “A” key is directly to the right of the Caps Lock key and the “Q” key is above it, but on a French (AZERTY) keyboard the “Q” key is directly to the right of the Caps Lock key and the “A” key is above it. If a Keyman keyboard has `&mnemoniclayout` enabled, the rules for “A” will take the base layout into account and respond to the “A” key, even though it is a different physical key on the two keyboards. 

Note that some keyboards in the Keyman keyboards repository use the term “mnemonic” in the title or description to indicate that the characters (usually of a non-Latin script) are assigned to keys according to their correspondence with the US English (QWERTY) layout. For example, the Arabic “beh” character might be assigned to the location of the “B” on the QWERTY keyboard.

### Positional Layout

A keyboard layout where keys are identified by their physical position on the keyboard rather than the character displayed on the keycap. For example, the "Z" position on a US keyboard corresponds to the "Y" position on a German keyboard.

### Transient Language Profiles

Custom language profiles in Windows (limited to 4) for languages that Windows doesn't have built-in support for, automatically managed when installing keyboards with custom BCP 47 tags.

### Word Breaker

An algorithm that determines where word boundaries occur in running text, essential for lexical models. Different languages require different word-breaking rules (e.g., space-separated vs. no obvious breaks in East Asian scripts).

---

*This glossary is based on the Keyman Developer General Tutorial and official Keyman documentation. For the most current information, visit [help.keyman.com](http://help.keyman.com).*