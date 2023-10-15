# LDML Markers in Keyman

## Background

- This is the proposed implementation of markers for Keyman

> Markers are placeholders which record some state, but without producing normal visible text output. They were designed particularly to support dead-keys.

- See [tr35-keyboards-markers][] for source spec.
    - [CLDR-16837][] has been filed to capture feedback on the marker spec, please add to that with any commnets.

### Format

- The general format is `\m{id}` where `id` is presumed to be `[0-9A-Za-z_]{1,32}`
- `\m{.}` in matching matches a single marker.

### Location

Markers can appear in both 'emitting' and 'matching-only' areas:

#### Emitting

- `key to=` for emitting markers
- `transform to=` to emit markers
- `string value=` for matching or emitting markers

#### Match only

- `transform from=` to match markers
- `display to=` for matching keys which contain markers

## Theory / Encoding

- Keyman already uses `UC_SENTINEL` `U+FFFF` (noncharacter), with `CODE_DEADKEY` (0x0008)
- The general proposal here is to use the sequence `U+FFFF U+0008 U+XXXX` to represent marker #XXXX (starting with `U+0001`)
- `U+FFFF` cannot otherwise occur in text, so it is unique
- `U+FFFF U+0008 U+D7FF` to indicate 'any marker'  corresponds to `\m{.}`
- The max marker identifier will be `0xD7FE`, with `0xD7FF` reserved to represent 'any marker' if that is needed in the text stream.
- This scheme allows for 55,294 unique markers, from `U+FFFF U+0008 U+0001` through `U+FFFF U+0008 U+D7FE` inclusive.
- This scheme avoids the Unicode surrogate space beginning at `U+D800` and other noncharacters.

## Terminology
- A marker's "number" is its position in the `markers` list, starting at index 1 (U+0001) being the first element in that list.
Note that this is different from other 0-based indices in KMX+. If there are three markers in a keyboard file, they will be numbered 1, 2, 3.
- A marker's "id" is its string name such as `a` or `acute`

## Compiler (kmc)

- `U+FFFF` needs to be illegal as a literal or escaped sequence. So `\u{FFFF}` is not allowed, for example, nor as a literal in the UTF-8 .xml stream.
- Matching `\m{abc}` (some marker) will turn into a match for `U+FFFF U+0008 U+XXXX` for that match.
- Matching `\m{.}` (_any_ marker) will turn into the special sequence `U+FFFF U+0008 [U+0001-U+D7FE]` where the latter is a range

### `vars`

- `vars` compiler will prescan all of the elements listed under [Emitting](#emitting), but not match-only.
- All found markers will be added to a single master `list`, which will be added under the `vars.markers` section.
- Subsequent processes/compilers will be able to check this list for any missing markers. For example, a `<display to='\m{nonExistentMarker}'/>` can then trigger an error failing validation, because nothing is defined which emits that marker.

### Other sections

- `string value='\m{…}'` will simply store `\m{…}`, for application when expanded as with other variables.
- Other emitters, such as `key`, `transform` will include the string `U+FFFF U+0008 U+XXXX` where XXXX corresponds to the marker's 1-based number.
- Transforms will need to match against the marker or markers desired, so may need to emit sequences such as `(?:\uFFFF\u0008\u0123)` meaning a match to marker #0x0123
- matching `\m{.}` may then expand to `(?:\uFFFF\u0008.)` (match a single codepoint after `UC_SENTINEL CODE_DEADKEY`)

## Binary (.kmx plus)

- The `vars.markers` is a pointer into the `list` section with a list (binary order) of the marker names
- Other strings will be in `U+FFFF U+0008 U+0123` form etc. as if it was in the original text stream as such.

## Implementation (core)

- Core needs to recognize `U+FFFF U+0008 …` sequences and convert them to markers in the context stream, with `state->context().push_marker(marker_number)`
- For normal processing, Core does _not_ need to correlate the marker _number_ with a marker _id_, although this would be helpful for a debugging or tracing facility.  I.e. `U+FFFF U+0008 U+0123` corresponding to entry 0x0123 in the `vars.markers` -> `list` table.
- Core needs to remove `U+FFFF U+0008 …` sequences before they are passed to the OS.

- Transform processing needs to recognize these markers in the context stream and pass them to the transforms appropriately.
- User-defined backspace processing `<transform type="backspace"/> may specifically operate on backspaces in the context stream, just as with other transform processing.
- The default backspace processing needs to recognize these markers in the context stream and remove them as appropriate.


[tr35-keyboards-markers]: https://github.com/unicode-org/cldr/blob/keyboard-preview/docs/ldml/tr35-keyboards.md#markers
[CLDR-16837]: https://unicode-org.atlassian.net/browse/CLDR-16837
