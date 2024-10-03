---
title: keyman-touch-layout files
---

Used by:
:   <span class="application">Keyman Developer</span>.

Description:
:   A .keyman-touch-layout file is a JSON format file that describes a
    keyboard layout for touch devices.

Details:
:   Referenced by a Keyman keyboard ([.KMN](kmn)) and compiles into the
    target keyboard .js file.

Distributed with keyboard:
:   This is a keyboard development file and should not be distributed
    with your package.

---

## Sample JSON

This cut-down sample shows just a single key, with the key cap "ឆ", and a single longpress key under it with the key cap "ឈ".

```json
{
  "phone": {
    "displayUnderlying": false,
    "font": "Khmer Busra Kbd",
    "fontsize": "0.8em",
    "layer": [ {
      "id": "default",
      "row": [ {
        "id": 1,
        "key": [ {
          "id": "K_Q", "text": "ឆ",
          "sk": [ { "text": "ឈ", "id": "K_Q", "layer": "shift" } ]
        } ]
      } ]
    } ]
  }
}
```

<div>
<style>
  /* Custom OSK font for key type */
  @font-face {
    font-family: SpecialOSK;
    font-display: block;
    src: url('https://s.keyman.com/kmw/engine/17.0.185/osk/keymanweb-osk.ttf');
  }

.special-osk {
text-align: center;
font-family: SpecialOSK;
}
</style>
</div>

## Key properties

For each visual key, the appearance and behaviour is determined by a number of
properties:

### Key code

Each key must be given an identifying key code which is unique to the key layer.
Key codes by and large correspond to the virtual key codes used when creating a
keyboard program for a desktop keyboard, and should start with `K_`, for keys
mapped to standard Keyman virtual key names, e.g. `K_HYPHEN`, and `T_` or `U_`
for user-defined names, e.g. `T_ZZZ`. If keyboard rules exist matching the key
code in context, then the output from the key will be determined by the
processing of those rules. It is usually best to include explicit rules to
manage the output from each key, but if no rules matching the key code are
included in the keyboard program, and the key code matches the pattern
`U_xxxx[_yyyy...]` (where `xxxx` and
`yyyy` are 4 to 6-digit hex strings), then the Unicode characters
`U+xxxx` and `U+yyyy` will be output. As of Keyman 15, you
can use more than one Unicode character value in the id (earlier versions
permitted only one). The key code is always required, and a default code will
usually be generated automatically by Keyman Developer.

- `K_xxxx` is used for a standard Keyman Desktop key name, e.g.
  `K_W`, `K_ENTER`. You cannot make up your own `K_xxxx` names.
  Many of the `K_` ids have overloaded output behaviour, for instance, if no
  rule is matched for `K_W`, Keyman will output 'w' when it is touched. The
  standard key names are listed in [Virtual Keys and Virtual Character
  Keys](/developer/language/guide/virtual-keys "Virtual Keys and Virtual
Character Keys"). Typically, you would use only the "common" virtual key
  codes.

- `T_xxxx` is used for any user defined names, e.g. `T_SCHWA`. If you wanted
  to use it, `T_ENTER` would also be valid. If no rule matches it, the key
  will have no output behaviour.

- `U_####[_####]` is used as a shortcut for a key that will output those
  Unicode values, if no rule matches it. This is similar to the overloaded
  behaviour for `K_` ids. Thus `####` must be valid Unicode characters. E.g.
  `U_0259` would generate a schwa if no rule matches. It is still valid to
  have a rule such as `+ [U_0259] > ...`

As noted above, some `K_xxxx` codes emit characters, if no rule is defined.
There are also some codes which have special functions:

<table class="display">
  <thead>
    <tr>
      <th>Identifier</th>
      <th>Meaning</th>
    </tr>
  </thead>
  <tbody>
    <tr>
      <td markdown="1">`K_ENTER`</td>
      <td>Submit a form, or add a new line (multi-line); the key action may vary depending on the situation.</td>
    </tr>
    <tr>
      <td markdown="1">`K_BKSP`</td>
      <td>Delete back a single character. This key, if held down, will repeat. It is the only key code which triggers
        repeat behavior.</td>
    </tr>
    <tr>
      <td markdown="1">`K_LOPT`</td>
      <td>Open the language menu (aka Globe key).</td>
    </tr>
    <tr>
      <td markdown="1">`K_ROPT`</td>
      <td>Hide the on screen keyboard.</td>
    </tr>
    <tr>
      <td markdown="1">`K_TAB`, `K_TABBACK`, `K_TABFWD`</td>
      <td markdown="1">Move to next or previous element in a form. Note that these key functions are normally
        implemented outside the touch layout, so should not typically be used. `K_TAB` will go to previous
        element if used with the `shift` modifier.</td>
    </tr>
  </tbody>
</table>

Any key can be used to switch keyboard layers (see
[`nextlayer`](#toc-nextlayer)), but the following layer-switching key codes have
been added for switching to some commonly used secondary layers. Note that these
keys have no specific meaning; you must still set the `nextlayer` property on
the key.

<table class="display">
  <thead>
    <tr>
      <th>Identifier</th>
      <th>Meaning</th>
    </tr>
  </thead>
  <tbody>
    <tr>
      <td markdown="1">`K_NUMERALS`</td>
      <td>Switch to a numeric layer</td>
    </tr>
    <tr>
      <td markdown="1">`K_SYMBOLS`</td>
      <td>Switch to a symbol layer</td>
    </tr>
    <tr>
      <td markdown="1">`K_CURRENCIES`</td>
      <td>Switch to a currency layer</td>
    </tr>
    <tr>
      <td markdown="1">`K_SHIFTED`</td>
      <td>Switch to a shift layer</td>
    </tr>
    <tr>
      <td markdown="1">`K_ALTGR`</td>
      <td>Switch to a right-alt layer (desktop compatibility)</td>
    </tr>
  </tbody>
</table>

### Key text

The key text is simply the character (or characters) that you want to appear on
the key cap. This will usually be the same as the characters generated when the
key is touched, unless contextual rules are used to generate output according to
a multi-key sequence, as will be true for the GFF Amharic keyboard. Unicode
characters can be specified either as a string using a target font or using the
standard hex notation `\uxxxx`. This may be sometimes more convenient, for
example, for characters from an uninstalled font, or for diacritic characters
that do not render well alone.

A number of special text labels are recognized as identifying special purpose
keys, such as Shift, Backspace, Enter, etc., for which icons are more
appropriately used than a text label. A special font including these icons is
included with Keyman and automatically embedded and used in any web page using
Keyman. The list of icons in the font will probably be extended in future, but
for now the following special labels are recognized:

<table class="display">
  <thead>
    <tr>
      <th>Text String</th>
      <th>Key Cap</th>
      <th>Key Purpose</th>
    </tr>
  </thead>
  <tbody>
    <tr>
      <td markdown="1">`*Shift*`</td>
      <td class="special-osk">&#xE008;</td>
      <td>Select Shift layer (inactive). Use on the Shift key to indicate that it switches to the shift layer.</td>
    </tr>
    <tr>
      <td markdown="1">`*Shifted*`</td>
      <td class="special-osk">&#xE009;</td>
      <td>Select Shift layer (active). Use on the Shift key on the shift layer to switch back to the default layer.</td>
    </tr>
    <tr>
      <td markdown="1">`*ShiftLock*`</td>
      <td class="special-osk">&#xE073;</td>
      <td>Switch to Caps layer (inactive). Not commonly used; generally double-tap on Shift key is used to access the
        caps layer.</td>
    </tr>
    <tr>
      <td markdown="1">`*ShiftedLock*`</td>
      <td class="special-osk">&#xE074;</td>
      <td>Switch to Caps layer (active). Use on the Shift key on the caps layer to switch back to the default layer.
      </td>
    </tr>
    <tr>
      <td markdown="1">`*Enter*`</td>
      <td class="special-osk">&#xE005; or &#xE071;</td>
      <td>Return or Enter key (shape determined by writing system direction)</td>
    </tr>
    <tr>
      <td markdown="1">`*LTREnter*`</td>
      <td class="special-osk">&#xE005;</td>
      <td>Return or Enter key (left-to-right script shape)</td>
    </tr>
    <tr>
      <td markdown="1">`*RTLEnter*`</td>
      <td class="special-osk">&#xE071;</td>
      <td>Return or Enter key (right-to-left script shape)</td>
    </tr>
    <tr>
      <td markdown="1">`*BkSp*`</td>
      <td class="special-osk">&#xE004; or &#xE072;</td>
      <td>Backspace key (shape determined by writing system direction)</td>
    </tr>
    <tr>
      <td markdown="1">`*LTRBkSp*`</td>
      <td class="special-osk">&#xE004;</td>
      <td>Backspace key (left-to-right script shape)</td>
    </tr>
    <tr>
      <td markdown="1">`*RTLBkSp*`</td>
      <td class="special-osk">&#xE072;</td>
      <td>Backspace key (right-to-left script shape)</td>
    </tr>
    <tr>
      <td markdown="1">`*Menu*`</td>
      <td class="special-osk">&#xE00B;</td>
      <td markdown="1">Globe key; display the language menu. Use on the `K_LOPT` key.</td>
    </tr>
    <tr>
      <td markdown="1">`*Hide*`</td>
      <td class="special-osk">&#xE00A;</td>
      <td markdown="1">Hide the on screen keyboard. Use on the `K_ROPT` key.</td>
    </tr>
    <tr>
      <td markdown="1">`*ABC*`</td>
      <td class="special-osk">&#xE010;</td>
      <td>Select alphabetic layer (Uppercase)</td>
    </tr>
    <tr>
      <td markdown="1">`*abc*`</td>
      <td class="special-osk">&#xE011;</td>
      <td>Select alphabetic layer (Lowercase)</td>
    </tr>
    <tr>
      <td markdown="1">`*123*`</td>
      <td class="special-osk">&#xE013;</td>
      <td>Select the numeric layer</td>
    </tr>
    <tr>
      <td markdown="1">`*Symbol*`</td>
      <td class="special-osk">&#xE015;</td>
      <td>Select the symbol layer</td>
    </tr>
    <tr>
      <td markdown="1">`*Currency*`</td>
      <td class="special-osk">&#xE014;</td>
      <td>Select the currency symbol layer</td>
    </tr>
    <tr>
      <td markdown="1">`*ZWNJ*`</td>
      <td class="special-osk">&#xE075; (iOS) or &#xE076; (Android)</td>
      <td>Zero Width Non Joiner (shape determined by current platform)</td>
    </tr>
    <tr>
      <td markdown="1">`*ZWNJiOS*`</td>
      <td class="special-osk">&#xE075;</td>
      <td>Zero Width Non Joiner (iOS style shape)</td>
    </tr>
    <tr>
      <td markdown="1">`*ZWNJAndroid*`</td>
      <td class="special-osk">&#xE076;</td>
      <td>Zero Width Non Joiner (Android style shape)</td>
    </tr>
    <tr>
      <td markdown="1">`*ZWNJGeneric*`</td>
      <td class="special-osk">&#xE079;</td>
      <td>Zero Width Non Joiner (not platform-specific)</td>
    </tr>
    <tr>
      <td markdown="1">`*Sp*`</td>
      <td class="special-osk">&#xE080;</td>
      <td>Regular space</td>
    </tr>
    <tr>
      <td markdown="1">`*NBSp*`</td>
      <td class="special-osk">&#xE082;</td>
      <td>No-Break Space</td>
    </tr>
    <tr>
      <td markdown="1">`*NarNBSp*`</td>
      <td class="special-osk">&#xE083;</td>
      <td>Narrow No-Break Space</td>
    </tr>
    <tr>
      <td markdown="1">`*EnQ*`</td>
      <td class="special-osk">&#xE084;</td>
      <td>En Quad</td>
    </tr>
    <tr>
      <td markdown="1">`*EmQ*`</td>
      <td class="special-osk">&#xE085;</td>
      <td>Em Quad</td>
    </tr>
    <tr>
      <td markdown="1">`*EnSp*`</td>
      <td class="special-osk">&#xE086;</td>
      <td>En Space</td>
    </tr>
    <tr>
      <td markdown="1">`*EmSp*`</td>
      <td class="special-osk">&#xE087;</td>
      <td>Em Space</td>
    </tr>
    <tr>
      <td markdown="1">`*PunctSp*`</td>
      <td class="special-osk">&#xE08C;</td>
      <td>Punctuation Space</td>
    </tr>
    <tr>
      <td markdown="1">`*ThSp*`</td>
      <td class="special-osk">&#xE08D;</td>
      <td>Thin Space</td>
    </tr>
    <tr>
      <td markdown="1">`*HSp*`</td>
      <td class="special-osk">&#xE08E;</td>
      <td>Hair Space</td>
    </tr>
    <tr>
      <td markdown="1">`*ZWSp*`</td>
      <td class="special-osk">&#xE081;</td>
      <td>Zero Width Space</td>
    </tr>
    <tr>
      <td markdown="1">`*ZWJ*`</td>
      <td class="special-osk">&#xE077;</td>
      <td>Zero Width Joiner</td>
    </tr>
    <tr>
      <td markdown="1">`*WJ*`</td>
      <td class="special-osk">&#xE078;</td>
      <td>Word Joiner</td>
    </tr>
    <tr>
      <td markdown="1">`*CGJ*`</td>
      <td class="special-osk">&#xE07A;</td>
      <td>Combining Grapheme Joiner</td>
    </tr>
    <tr>
      <td markdown="1">`*LTRM*`</td>
      <td class="special-osk">&#xE090;</td>
      <td>Left-to-right Mark</td>
    </tr>
    <tr>
      <td markdown="1">`*RTLM*`</td>
      <td class="special-osk">&#xE091;</td>
      <td>Right-to-left Mark</td>
    </tr>
    <tr>
      <td markdown="1">`*SH*`</td>
      <td class="special-osk">&#xE0A1;</td>
      <td>Soft Hyphen</td>
    </tr>
    <tr>
      <td markdown="1">`*HTab*`</td>
      <td class="special-osk">&#xE0A2;</td>
      <td>Horizontal Tabulation</td>
    </tr>

  </tbody>
</table>

The following additional symbols are also available, but intended for working
with legacy desktop layouts, and not recommended for general use:

<table class="display">
  <thead>
    <tr>
      <th>Text String</th>
      <th>Key Cap</th>
      <th>Key Purpose</th>
    </tr>
  </thead>
  <tbody>
    <tr>
      <td markdown="1">`*Tab*`</td>
      <td class="special-osk">&#xE006;</td>
      <td>Move to next input element in tab order</td>
    </tr>
    <tr>
      <td markdown="1">`*TabLeft*`</td>
      <td class="special-osk">&#xE007;</td>
      <td>Move to previous input element in tab order</td>
    </tr>
    <tr>
      <td markdown="1">`*Caps*`</td>
      <td class="special-osk">&#xE003;</td>
      <td>Select caps layer (legacy)</td>
    </tr>
    <tr>
      <td markdown="1">`*AltGr*`</td>
      <td class="special-osk">&#xE002;</td>
      <td>Select AltGr (Right-Alt) layer (desktop layout compatibility)</td>
    </tr>
    <tr>
      <td markdown="1">`*Alt*`</td>
      <td class="special-osk">&#xE019;</td>
      <td>Select Alt layer (desktop layout compatibility)</td>
    </tr>
    <tr>
      <td markdown="1">`*Ctrl*`</td>
      <td class="special-osk">&#xE001;</td>
      <td>Select Ctrl layer (desktop layout compatibility)</td>
    </tr>
    <tr>
      <td markdown="1">`*LAlt*`</td>
      <td class="special-osk">&#xE056;</td>
      <td>Select Left-Alt layer (desktop layout compatibility)</td>
    </tr>
    <tr>
      <td markdown="1">`*RAlt*`</td>
      <td class="special-osk">&#xE057;</td>
      <td>Select Right-Alt layer (desktop layout compatibility)</td>
    </tr>
    <tr>
      <td markdown="1">`*LCtrl*`</td>
      <td class="special-osk">&#xE058;</td>
      <td>Select Left-Ctrl layer (desktop layout compatibility)</td>
    </tr>
    <tr>
      <td markdown="1">`*RCtrl*`</td>
      <td class="special-osk">&#xE059;</td>
      <td>Select Right-Ctrl layer (desktop layout compatibility)</td>
    </tr>
    <tr>
      <td markdown="1">`*LAltCtrl*`</td>
      <td class="special-osk">&#xE060;</td>
      <td>Select Left-Alt-Ctrl layer (desktop layout compatibility)</td>
    </tr>
    <tr>
      <td markdown="1">`*RAltCtrl*`</td>
      <td class="special-osk">&#xE061;</td>
      <td>Select Right-Alt-Ctrl layer (desktop layout compatibility)</td>
    </tr>
    <tr>
      <td markdown="1">`*LAltCtrlShift*`</td>
      <td class="special-osk">&#xE062;</td>
      <td>Select Left-Alt-Ctrl-Shift layer (desktop layout compatibility)</td>
    </tr>
    <tr>
      <td markdown="1">`*RAltCtrlShift*`</td>
      <td class="special-osk">&#xE063;</td>
      <td>Select Right-Alt-Ctrl-Shift layer (desktop layout compatibility)</td>
    </tr>
    <tr>
      <td markdown="1">`*AltShift*`</td>
      <td class="special-osk">&#xE064;</td>
      <td>Select Alt-Shift layer (desktop layout compatibility)</td>
    </tr>
    <tr>
      <td markdown="1">`*CtrlShift*`</td>
      <td class="special-osk">&#xE065;</td>
      <td>Select Ctrl-Shift layer (desktop layout compatibility)</td>
    </tr>
    <tr>
      <td markdown="1">`*AltCtrlShift*`</td>
      <td class="special-osk">&#xE066;</td>
      <td>Select Alt-Ctrl-Shift layer (desktop layout compatibility)</td>
    </tr>
    <tr>
      <td markdown="1">`*LAltShift*`</td>
      <td class="special-osk">&#xE067;</td>
      <td>Select Left-Alt-Shift layer (desktop layout compatibility)</td>
    </tr>
    <tr>
      <td markdown="1">`*RAltShift*`</td>
      <td class="special-osk">&#xE068;</td>
      <td>Select Right-Alt-Shift layer (desktop layout compatibility)</td>
    </tr>
    <tr>
      <td markdown="1">`*LCtrlShift*`</td>
      <td class="special-osk">&#xE069;</td>
      <td>Select Left-Ctrl-Shift layer (desktop layout compatibility)</td>
    </tr>
    <tr>
      <td markdown="1">`*RCtrlShift*`</td>
      <td class="special-osk">&#xE070;</td>
      <td>Select Right-Ctrl-Shift layer (desktop layout compatibility)</td>
    </tr>
  </tbody>
</table>

### Key type

The general appearance of each key is determined by the key type, which is
selected (in Keyman Developer) from a drop-down list. While generally behavior
is not impacted by the key type, Spacer keys cannot be selected.

<table class="display">
  <thead>
    <tr>
      <th>Key Type</th>
      <th>Value</th>
      <th>Meaning</th>
    </tr>
  </thead>
  <tbody>
    <tr>
      <td>Default</td>
      <td markdown="1">`0`</td>
      <td>Any normal key that emits a character</td>
    </tr>
    <tr>
      <td>Special</td>
      <td markdown="1">`1`</td>
      <td>The frame keys such as Shift, Enter, BkSp.</td>
    </tr>
    <tr>
      <td>Special (active)</td>
      <td markdown="1">`2`</td>
      <td>A frame key which is currently active, such as the Shift key on the shift layer.</td>
    </tr>
    <tr>
      <td>Deadkey</td>
      <td markdown="1">`8`</td>
      <td>Does not impact behavior, but colors the key differently to indicate it has a special function, such as a
        desktop-style deadkey.</td>
    </tr>
    <tr>
      <td>Blank</td>
      <td markdown="1">`9`</td>
      <td>A blank key, which may be used to maintain a layout shape. Usually colored differently. Does not impact
        behavior.</td>
    </tr>
    <tr>
      <td>Spacer</td>
      <td markdown="1">`10`</td>
      <td>Does not render the key, but leaves a same-sized gap in its place. The key cannot be selected.</td>
    </tr>
  </tbody>
</table>

The colour, shading and borders of each key type is actually set by a style
sheet which can be customized by the page developer.

### font-family

If a different font is required for a particular key text, the `font-family`
name can be specified. The font used to display icons for the special keys (as
mentioned above) does not need to be specified, as it will be automatically
applied to a key that uses any of the special key text labels.

### font-size

If a particular key cap text requires a different font size from the default for
the layout, it should be specified in em units. This can be helpful if a the key
text is either an unusually large character or, alternatively, a word or string
of several characters that would not normally fit on the key.

### width

The layout is scaled to fit the widest row of keys in the device width, assuming
a default key width of 100 units. Keys that are to be wider or narrower than the
default width should have width specified as a percentage of the default width.
For any key row that is narrower than the widest row, the width of the last key
in the row will be automatically increased to align the right hand side of the
key with the key with the right edge of the keyboard. However, where this is not
wanted, a "spacer" key can be inserted to leave a visible space instead. As
shown in the above layouts, where the spacer key appears on the designer screen
as a narrow key, but will not be visible in actual use.

### pad

Padding to the left of each key can be adjusted, and specified as a percentage
of the default key width. If not specified, a standard padding of 5% of the key
width is used between adjacent keys.

### layer

To simplify correspondence with desktop keyboards and avoid the need for using a
separate keyboard mapping program, touch layout keys can specify a desktop
keyboard layer that the keystroke should be interpreted as coming from. Layer
names of `shift`, `ctrl`, `alt`, `ctrlshift`, `altshift`, `ctrlalt` and
`ctrlaltshift` can be used to simulate use of the appropriate modifier keys when
processing rules.

### nextlayer

The virtual keys `K_SHIFT`, `K_CONTROL`, `K_MENU`, etc. are normally used to
switch to another key layer, which is implied by the key code. The left and
right variants of those key codes, and also additional layer-switching keys
mentioned above (`K_NUMERALS`, `K_SYMBOLS`, `K_CURRENCIES`, `K_ALTGR`) can also
be used to automatically switch to the appropriate key layer instead of
outputting a character. However, it is sometimes useful for a key to output a
character first, then switch to a new layer, for example, switching back to the
default keyboard layer after a punctuation key on a secondary layer had been
used. Specifying the `nextlayer` for a key allows a different key layer to be
selected automatically following the output of the key. Of course, that can be
manually overridden by switching to a different layer if preferred.

Another way the `nextlayer` property can be used is for a non-standard layer
switching key. So, for example, for the GFF Amharic keyboard phone layout,
switching back to the base layer uses a `T_ALPHA` key code, in which `nextlayer`
is set as default. In this case, it is also necessary to add a rule to the
keyboard program:

```keyman
+ [T_ALPHA] > nul
```

to ensure that the key's scan code is ignored by the keyboard mapping.

When a key in a touch layout definition includes a **Next Layer** control, this
takes precedence over setting layer via the
[`layer`](/developer/language/reference/layer) store (as the **Next Layer**
control is applied once the rule has finished processing).

### subkey

Arrays of longpress 'subkeys' or pop-up keys can be defined for any key, and
will appear momentarily after the key is touched if not immediately released.
This provides a major advantage over physical desktop keyboards in that many
more keys can be made available from a single layer, without cluttering up the
basic appearance of the layout. For the GFF Amharic keyboard, we have already
noted how such subkey arrays are used to manage the extra keys that, on the
desktop keyboard, would appear in the shift layer. But they are also used to
provide another way to enter the two different types of each syllable-initial
vowels (glottal or pharyngeal), as a visual alternative to pressing the key
twice.

The same properties that are defined for standard keys can also be specified for
each subkey except that the width of each key in a subkey array will always be
the same as the width of the key that causes the subkeys to be shown, and key
spacing always uses the default padding value.

The GFF Amharic keyboard, like many others, is mnemonic, so it is useful to also
display the standard key cap letter that would appear on the key of a desktop
keyboard. This is enabled globally in the On-Screen layout editor and applies to
both the On-Screen keyboard and touch layouts.

## JSON Schema

```json
{
  "$schema": "http://json-schema.org/schema#",
  "$ref": "#/definitions/touch-layout",
  "$comment": "Version: 16.0",
  "description": "A Keyman Touch Layout file, per version 16.0, clean spec, no legacy data or types",

  "definitions": {
    "touch-layout": {
      "type": "object",
      "properties": {
        "tablet": { "$ref": "#/definitions/platform" },
        "phone": { "$ref": "#/definitions/platform" },
        "desktop": { "$ref": "#/definitions/platform" }
      },
      "minProperties": 1,
      "additionalProperties": false
    },

    "platform": {
      "type": "object",
      "properties": {
        "font": { "$ref": "#/definitions/font-spec" },
        "fontsize": { "$ref": "#/definitions/fontsize-spec" },
        "layer": { "$ref": "#/definitions/layers" },
        "displayUnderlying": { "type": "boolean" },
        "defaultHint": { "type": "string", "enum": ["none","dot","longpress","multitap","flick","flick-n","flick-ne","flick-e","flick-se","flick-s","flick-sw","flick-w","flick-nw"] }
      },
      "required": ["layer"],
      "additionalProperties": false
    },

    "layers": {
      "type": "array",
      "items": { "$ref": "#/definitions/layer" },
      "minItems": 1
    },

    "layer": {
      "type": "object",
      "properties": {
        "id": { "$ref": "#/definitions/layer-id" },
        "row": { "$ref": "#/definitions/rows" }
      },
      "required": ["id", "row"],
      "additionalProperties": false
    },

    "layer-id": {
      "type": "string",
      "pattern": "^[a-zA-Z0-9_-]+$"
    },

    "rows": {
      "type": "array",
      "items": { "$ref": "#/definitions/row" },
      "minItems": 1
    },

    "row": {
      "type": "object",
      "properties": {
        "id": { "$ref": "#/definitions/row-id" },
        "key": { "$ref": "#/definitions/keys" }
      },
      "required": ["id", "key"],
      "additionalProperties": false
    },

    "row-id": {
      "type": "integer",
      "minimum": 0,
      "maximum": 100
    },

    "keys": {
      "type": "array",
      "items": { "$ref": "#/definitions/key" },
      "minItems": 1
    },

    "key": {
      "type": "object",
      "properties": {
        "id": { "$ref": "#/definitions/key-id" },
        "text": { "type": "string" },
        "layer": { "$ref": "#/definitions/layer-id" },
        "nextlayer": { "$ref": "#/definitions/layer-id" },
        "font": { "$ref": "#/definitions/font-spec" },
        "fontsize": { "$ref": "#/definitions/fontsize-spec" },
        "sp": { "$ref": "#/definitions/key-sp" },
        "pad": { "$ref" : "#/definitions/key-pad" },
        "width": { "$ref" : "#/definitions/key-width" },
        "sk": { "$ref": "#/definitions/subkeys" },
        "flick": { "$ref": "#/definitions/flick" },
        "multitap": { "$ref": "#/definitions/subkeys" },
        "hint": { "type": "string" }
      },
      "anyOf": [
        {"required": ["id"]},
        {"required": ["sp"]},
        {"required": ["sk"]},
        {"required": ["flick"]},
        {"required": ["multitap"]}
      ],
      "additionalProperties": false
    },

    "key-id": {
      "type": "string",
      "pattern": "^[TKUtku]_[a-zA-Z0-9_]+$"
    },

    "key-sp": {
      "type": "integer",
      "enum": [0, 1, 2, 8, 9, 10]
    },

    "key-pad": {
      "type": "number",
      "minimum": 0,
      "maximum": 100000
    },

    "key-width": {
      "type": "number",
      "minimum": 0,
      "maximum": 100000
    },

    "subkeys": {
      "type": "array",
      "items": { "$ref": "#/definitions/subkey" },
      "minItems": 1
    },

    "subkey": {
      "type": "object",
      "properties": {
        "id": { "$ref": "#/definitions/key-id" },
        "text": { "type": "string" },
        "layer": { "$ref": "#/definitions/layer-id" },
        "nextlayer": { "$ref": "#/definitions/layer-id" },
        "font": { "$ref": "#/definitions/font-spec" },
        "fontsize": { "$ref": "#/definitions/fontsize-spec" },
        "sp": { "$ref": "#/definitions/key-sp" },
        "pad": { "$ref" : "#/definitions/key-pad" },
        "width": { "$ref" : "#/definitions/key-width" }
      },
      "required": ["id"],
      "additionalProperties": false
    },

    "flick": {
      "type": "object",
      "patternProperties": {
        "^(n|s|e|w|ne|nw|se|sw)$": { "$ref": "#/definitions/subkey" }
      },
      "minProperties": 1,
      "additionalProperties": false
    },

    "font-spec": {
      "type": "string"
    },

    "fontsize-spec": {
      "type": "string"
    }
  }
}
```
