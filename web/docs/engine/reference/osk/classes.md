---
title: Class Names and Identifiers for On-Screen Keyboard and Other KeymanWeb Elements
---

The appearance of the _KeymanWeb_ on-screen keyboard, menu amd other elements can be customized
by a web designer by redefining (or adding to) the default styles.  Styles and named elements
all have the class name (or id) prefix `kmw-` joined to the object and item names,
for example `kmw-key-text`, to set the basic style for the keycap text on each on-screen keyboard key.
Multiple classes are applied to many objects, with top-level classes of `tablet`,
`phone` and `desktop` used to set different element styles according to the device type.
Unique elements, such as the menu and pop-up key frame, are named rather than referred to by class.

Most visual attributes such as text size and colour, background colour, shading, margins, etc. can be varied
according to preference but developers should avoid changing positional attributes.

### Class names applied to on-screen keyboard elements:

|  Class name             | Keyboard element                    |
| ----------------------- | ----------------------------------- |
| `kmw-osk-frame`         | Outer keyboard frame                |
| `kmw-osk-inner-frame`   | Inner keyboard frame                |
| `kmw-key-layer-group`   | Keyboard layers container element   |
| `kmw-key-layer`         | Keyboard layer element              |
| `kmw-key-row`           | Keyboard row container element      |
| `kmw-key-square`        | Keyboard key container element      |
| `kmw-key-square-ex`     | Pop-up key container element        |
| `kmw-key`               | Key element                         |
| `kmw-key-label`         | Key label (desktop OSK and phonetic layouts) |
| `kmw-key-text`          | Keycap text                         |
| `kmw-key-popup-icon`    | Text or graphic to indicate that a keyboard key has associated popup keys |
| `kmw-spacebar-caption`  | Style applied to caption on spacebar |

### Class names applied to different types of keys:

|  Class name             | Key style                       |
| ----------------------- | ------------------------------- |
| `kmw-key-default`       | Default                         |
| `kmw-key-shift`         | Shift key, layer-switching keys |
| `kmw-key-shift-on`      | Active shift key                |
| `kmw-key-deadkey`       | [Dead key](/developer/language/reference/deadkey) |
| `kmw-key-blank`         | Blank, unmapped key             |
| `kmw-key-hidden`        | Hidden spacer key               |
| `kmw-key-touched`       | Active (touched) key            |

### Element identifiers for keys:

Useful reference: [Keys, Virtual Keys, and Virtual Character Keys](/developer/language/guide/virtual-keys#toc-key-codes)

Each key element (`kmw-key`) has an ID based on the key code used for it within Keyman Developer.  Each ID is based on
two or three pieces:
- The key's layer
- The key's code
- _If_ it differs from the key's actual layer, the "modifier" setting for the key as set within the Touch Layout editor.

The format:
- `<display layer>`-`<key code>`
- `<display layer>`-`<key code>`+`<modifier layer>` (if different from `<display layer>`)

Some example IDs:
- `default-K_K` - the `K_K` key on the `default` layer
- `currency-U_20B2` - the `U_20B2` key (₲) on the `currency` layer
- `numeric-K_0+default` - the `K_0` key displayed on the `numeric` layer, using the same modifiers as the `default` layer.

For keys reached by longpress, the form of the ID is the same, just with one extra piece: it is prepended with `popup-`.
- `popup-default-U_1E0F` - the `U_1E0F` subkey (ḏ) on the `default` layer
    - This is one of the subkeys of the 'd' key on the `default` layer on the EuroLatin (SIL) keyboard.

You can set styles for specific keys by using CSS selectors against specific key IDs.

The following selector will find the key element for all keys with the key code `K_RBRKT` of the keyboard's phone layout.

```css
.phone div.kmw-key.kmw-key-default[id*='K_RBRKT']
```

Note the `[id*='K_RBRKT']` part.  There can be a few variations here:
- `*=` - "contains"
- `^=` - "starts with"
- `$=` - "ends with"

So, to match all keys reached by longpress, you could use:

```css
div.kmw-key.kmw-key-default[id^='popup-']
```

Also, `:not(...)` can be used to invert a selector.  The following selector will match all base keys, but not
any longpress keys.

```css
div.kmw-key.kmw-key-default:not([id^='popup-'])
```

### Element identifiers and class names for desktop on-screen keyboard elements:

|  Class name or id       | Desktop OSK element                             |
| ----------------------- | -------------------------------                 |
| `kmw-title-bar`         | OSK title bar                                   |
| `kmw-title-bar-caption` | OSK title bar caption (keyboard name)           |
| `kmw-title-bar-actions` | Container for image elements in OSK title bar   |
| `kmw-title-bar-image`   | Active image element (button) in OSK title bar  |
| `kmw-footer`            | Desktop OSK footer element                      |
| `kmw-footer-caption`    | Caption in desktop OSK footer                   |
| `kmw-footer-resize`     | Style for the footer's resizing icon            |
| `kmw-osk-static`        | Container style for non-sizable keyboards       |
| `kmw-osk-none`          | Empty keyboard container style                  |
| `#kmw-pin-image`        | Style of icon to return OSK to default position |
| `#kmw-config-image`     | Style of icon to open configuration options window |
| `#kmw-help-image`       | Style of icon to open keymanweb help window     |
| `#kmw-close-button`     | Style of close button image                     |

### Element identifiers and class names applied to pop-up key container elements:

|  Class name             | Element                             |
| ----------------------- | -------------------------------     |
| `#kmw-popup-keys`       | Pop-up key container                |
| `arrow-border`          | Style setting callout arrow border  |
| `arrow-content`         | Style setting callout arrow background |

### Element identifiers and class names applied to language menu elements:

|  Class name                     | Menu element                          |
| ---------------------------     | -------------------------------       |
| `#kmw-language-menu`            | Language menu container element       |
| `#kmw-language-menu-background` | Hidden background layer preventing unwanted action while selecting a language or keyboard  |
| `#kmw-menu-scroll-container`    | Container for language list scroller  |
| `#kmw-menu-scroller`            | Langauge list scroller                |
| `#kmw-menu-index`               | Container for language index          |
| `#kmw-menu-footer`              | Placeholder at bottom of scroller     |
| `kbd-list`                      | Style for language-list element       |
| `kmw-list-open`                 | Style for list element when expanded  |
| `kmw-list-closed`               | Style for list element when not expanded  |
| `kmw-list-entry`                | Language entry in language list       |
| `kmw-single-entry`              | Single keyboard entry in language list    |
| `current`                       | Active keyboard entry in language list    |
| `selected`                      | Touched entry in language list        |

### Message box class names:

|  Class name             | Element                         |
| ----------------------- | ------------------------------- |
| `kmw-wait-box`          | Message box                     |
| `kmw-wait-background`   | Message box background          |
| `kmw-wait-text`         | Message box wait text           |
| `kmw-wait-graphic`      | Message box wait icon           |
| `kmw-alert-text`        | Message box alert text          |
| `kmw-alert-close`       | Message box close icon          |

The default styles for _KeymanWeb_ can be found online at
https://github.com/keymanapp/keyman/blob/master/web/src/resources/osk/kmwosk.css.

For the styles for specific released versions, they can also be found through
s.keyman.com, such as https://s.keyman.com/kmw/engine/17.0.326/osk/kmwosk.css
for version 17.0.326.