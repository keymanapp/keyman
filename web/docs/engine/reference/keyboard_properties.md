---
title: Keyboard Properties
---

Most keyboards are generated automatically from the Keyman keyboard source by
_Keyman Developer_ and contain properties used by _KeymanWeb_ during keyboard
mapping. Keyboards implement mapping by invoking functions within _KeymanWeb_,
but do not interact directly with the user interface. However, user interfaces
may need to interact with custom developed keyboards if, for example, they use a
"pick list" or have other IME-like behavior.

Each registered keyboard object defines some or all of the following exposed
string properties:

`KN`

: `string`, (`name`) visible name of the keyboard, required.

`KI`

: `string`, (`internalName`) identifier of the keyboard, starting with `Keyboard_`, required.

`KMINVER`

: `string`, minimum version that the keyboard will run on, `2.0` if not present, optional.

`KV`

: `object`, on screen keyboard definition, optional.

`KDU`

: `number`, `1` to display underlying characters on On Screen Keyboard, `0` or omitted to hide them, optional

`KH`

: `string`, Keyboard help, in HTML; if present replaces `KV` in the On Screen Keyboard, optional

`KM`

: `number`, `1` to use mnemonic layout, `0` or omitted for positional, optional

`KBVER`

: `string`, version of the keyboard (should be dotted decimal format), optional

`KMBM`

: `number`, bitmask denoting modifiers used in the keyboard, if not present defaults to 0x0070, optional

`KVKL`

: `object`, touch layout definition, optional.

`KVER`

: `string`, version of Keyman Developer used to compile the keyboard, optional

`KVS`

: `string[]`, array of all variable store names found in the keyboard (15.0 and later), optional

`KS`

: `number`, `1` means Unicode characters U+10000-U+10FFFF, including the Supplementary Multilingual Plane (SMP), are used in the keyboard, optional

`KVKD`

: `object`, Virtual key dictionary listing custom touch keys used in the keyboard, optional

`KCSS`

: `string`, Custom CSS defined by the keyboard, optional

`KFont`

: `object`, Embedded font specification for mapped input elements and on-screen keyboard, optional

`KOskFont`

: `object`, Embedded font specification for on-screen keyboard, optional

For most keyboards which require an embedded font, the same font will be used
for mapped elements and the on-screen keyboard, and only the `KFont` property
wil be defined. The additional property `KOskFont` property would only be used
where it may be helpful to use a different embedded font for the on-screen
keyboard.

The `KFont` and `KOskFont` members are objects with the following members:

`family`

: `string`, font-family name for embedded font, e.g. `'LatinWeb'`

`files`

: `string` or `string[]`, Font file name or names, e.g. `['DejaVuSans.ttf','DejaVuSans.woff','DejaVuSans.eot']`
