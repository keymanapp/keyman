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
- `transform after=` to match markers
- `display to=` for matching keys which contain markers

## Theory / Encoding

- Keyman already uses U_SENTINEL `U+FFFF` (noncharacter)
- The general proposal here is to use the sequence `U+FFFF U+EXXX` to represent marker #XXX
- `U+FFFF` cannot otherwise occur in text, so it is unique
- `U+EXXX` is always a private use character, so the marker number cannot collide with non-PUA text, however it may certainly collide with PUA text. The intention here is to reduce this possibility of collision, at least when (a) humans read a binary stream or (b) human _error_ causes the marker to show up in acutal text.  As a counter example, if we used `U+FFFF U+0022` to indicate marker 0x22, then the marker might show up as a doublequote (`"`).
- `U+FFFF U+EFFF` to indicate 'any marker'  corresponds to `\m{.}`
- this scheme allows for 4,095 (0xFFF) unique markers, from `U+FFFF U+E000` through `U+FFFF U+EFFE`

## Compiler (kmc)

- `U+FFFF` needs to be illegal as a literal or escaped sequence. So `\u{FFFF}` is not allowed, for example, nor as a literal in the UTF-8 .xml stream.

### `vars`

- `vars` compiler will prescan all of the elements listed under [Emitting](#emitting), but not match-only.
- All found markers will be added to a single master `list`, which will be added under the `vars.markers` section.
- Subsequent processes/compilers will be able to check this list for any missing markers. For example, a `<display to='\m{nonExistentMarker}'/>` can then trigger an error failing validation, because nothing is defined whith emits that marker.

### Other sections

- `string value='\m{…}'` will simply store `\m{…}`, for application when expanded as with other variables.
- Other emitters, such as `key`, `transform` will include the string `U+FFFF U+EXXX` where XXX corresponds to the marker's number.
- Transforms will need to match against the marker or markers desired, so may need to emit sequences such as `(?:\uFFFF\uE123)` meaning a match to marker #0x123
- matching `\m{.}` may need to expand to `(?:\uFFFF[\uE000-\uEFFE])`

## Binary (.kmx plus)

- The `vars.markers` is a pointer into the `list` section with a list (binary order) of the marker names
- Other strings will be in `U+FFFF U+E123` form etc. as if it was in the original text stream as such.

## Implementation (core)

- Core needs to recognize `U+FFFF …` sequences and convert them to markers in the context stream.
- For normal processing, Core does _not_ need to correlate the marker _number_ with a marker _id_, although this would be helpful for a debugging or tracing facility.  I.e. `U+FFFF U+E123` corresponding to entry 0x123 in the `vars.markers` -> `list` table.
- Core needs to remove `U+FFFF …` sequences before they are passed to the OS.
- The default backspace processing needs to ignore `U+FFFF …` markers as it is deleting.


[tr35-keyboards-markers]: https://github.com/unicode-org/cldr/blob/keyboard-preview/docs/ldml/tr35-keyboards.md#markers
[CLDR-16837]: https://unicode-org.atlassian.net/browse/CLDR-16837
