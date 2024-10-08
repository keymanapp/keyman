---
title: BCP 47 language tags
---

Packages and lexical models may reference a [BCP 47 language tag][1]. A BCP
47 tag is a standard way of referencing a language, used widely in the computer
industry. Keyman uses BCP 47 tags in a number of areas, including:

* providing language metadata about installed keyboards to the operating system
  and to applications, so that, for example, spell checking has the correct
  language, through [package metadata](../guides/distribute/)
* [linking lexical models to keyboard layouts](../guides/lexical-models/)
* facilitating searches for keyboards and lexical models on the Keyman site,
  through a [.keyboard_info file](/developer/cloud/keyboard_info)

A BCP 47 language tag is made up of multiple subtags. There are many possible
subtags, but only three types are currently used in most places in Keyman
Developer:

* [language subtag](#toc-the-language-subtag)
* [script subtag](#toc-the-script-subtag)
* [region subtag](#toc-the-region-subtag)

BCP 47 tags are case insensitive, but there are conventions for casing which you
should use for readability; see the subtag descriptions for details.

The following are all examples of valid BCP 47 tags:

* `en`: English
* `en-US`: English, in United States
* `km-Khmr-KH`: Khmer, written in the Khmer script, in Cambodia
* `km-fonipa`: Khmer, transcribed in IPA

### The language subtag

The only required option is the Language subtag, which is an [ISO 639-1][2] or
[ISO 639-3][3] code.

ISO 639-1 tags are a two-letter code. ISO 639-3 tags are a three-letter code.
First, try to find your language on the list of two-letter ISO 639-1 codes.
[This Wikipedia page][4] lists all of the two-letter codes.

If you can't find a two-letter code, you'll need to find the closest
three-letter code. You can use [Glottolog][5] to search for your language, and
it will give you an appropriate code. In this example, I searched Glottolog for
“[Saanich][6]” (name of the First Nations that speak SENĆOŦEN) and found `str`
as the code for all Straits Salish languages.

The Language subtag is conventionally written in lower case.

The next two subtags are **optional**, however, they allow you to be more
specific about your language.

### The script subtag

The Script subtag allows you to specify the writing system used in your language
model or keyboard. If your language only uses one writing system, omit the
Script subtag.

Otherwise, in cases where a language can be written in many different writing
systems, you can choose the four letter [ISO 15924][7] script tag that your
keyboard or lexical model produces.

For example, Plains Cree can either be written in _standard Roman orthography_,
a **Latin** derived script, or it can be written in _syllabics_, which is part
of the **Canadian Aboriginal syllabics** family of writing systems. If I wrote a
keyboard or lexical model that produced syllabics, I would choose `Cans`, as
that is the **ISO 15924** tag for Canadian Aboriginal syllabics.

The Script subtag is conventionally written in title case - first letter
capitalized.

### The region subtag

The Region subtag allows you to specify the region your language or dialect is
spoken in. If your language is only spoken in one region, omit the Region
subtag.

Otherwise, some languages vary between different regions and countries. In our
example, SENĆOŦEN describes the language that covers entire W̱SÁNEĆ region, so
this field may be left blank.

However, large languages, like English, Spanish, or French have quite different
vocabulary and even different grammatical rules from region to region and
country to country. For example, the variety of Spanish spoken in Spain
regularly uses words that are uncommon or even vulgar in both in Mexico, and in
Latin America. Additionally, regions may have vocabulary that doesn't exist in
the other regions where the language is spoken.

If I were working with a language specific to one country, I would use the [ISO
3166-1 alpha-2][8] country code for the region subtag. For example, `ES` for
Spain or `MX` for Mexico.

However, if I were working with Latin American Spanish (a group of countries), I
would need to specify Latin America's [UN M49][9] region code. For Latin
America, its code is `419`. My lexical model would not suggest words that are
common in Spain, but vulgar in Latin America, however it would predict words
like "pupupsas" and "chuchitos", which are words that are uncommon in both Spain
and Mexico.

Another common UN M49 region code is `001` for the whole world.

Alphabetic region subtags are conventionally written in upper case.

[1]: https://en.wikipedia.org/wiki/IETF_language_tag
[2]: https://en.wikipedia.org/wiki/ISO_639-1
[3]: https://en.wikipedia.org/wiki/ISO_639-3
[4]: https://en.wikipedia.org/wiki/List_of_ISO_639-1_codes
[5]: https://glottolog.org/glottolog/language
[6]: https://glottolog.org/resource/languoid/id/saan1246
[7]: https://en.wikipedia.org/wiki/ISO_15924
[8]: https://en.wikipedia.org/wiki/ISO_3166-1_alpha-2
[9]: https://en.wikipedia.org/wiki/UN_M49
