# Keyman langtags

This package provides a wrapper for the
[langtags.json](https://ldml.api.sil.org/langtags.json) dataset from
https://github.com/silnrsi/langtags.

This package is published in sync with Keyman, so changes in langtags may not
propagate immediately.

## Usage

* `getLangtagByTag(tag)`: Find a language tag from a given tag; matches on tag,
  tags, full properties, with a case-insensitive search. Returns unmodified
  object from langtags.json on match.

* `metadata.conformance()`: Returns script and region conformance data from
  langtags.json

* `metadata.globalvar()`: Returns global variant data from langtags.json

* `metadata.phonvar()`: Returns phonetic variant data from langtags.json

* `metadata.version()`: Returns version data from langtags.json

* `langtags`: Raw access to the langtags.json array of objects

## Background

Key documents for further reading (from langtags repository):

* [Overview of language tags and how this project works with them](https://github.com/silnrsi/langtags/blob/master/doc/tagging.md)

* [How to use langtags.json](https://github.com/silnrsi/langtags/blob/master/doc/langtags.md)
