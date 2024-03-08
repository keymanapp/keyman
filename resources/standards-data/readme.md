# standards-data

The subfolders of this directory contain data from various standards
that are used in Keyman. If you update any files, be sure to update
the version data in this file also.

## Current Versions

| data                       | version     |
|----------------------------|-------------|
| ethnologue                 | ?           |
| iso639-3                   | 2023        |
| langtags                   | 2023-05-04  |
| langtags-subtag-registry   | 2023-05-11  |
| ldml-keyboards             | 45          |
| unicode-character-database | 15          |
| windows-lcid-to-bcp-47     | ?           |

## File Sources

### Ethnologue

This data comes from https://www.ethnologue.com/#resources, LanguageIndex.tab. It has been hand-converted from TSV to CSV, and the header line has been replaced.


### iso639-3

The file can be updated with:

```bash
wget https://iso639-3.sil.org/sites/iso639-3/files/downloads/iso-639-3.tab -O iso639-3/iso639-3.tab
```

### langtags

The file is retrieved from <https://ldml.api.sil.org/langtags.json>:

```bash
wget https://ldml.api.sil.org/langtags.json -O langtags/langtags.json
```

See also [langtags/readme.md](langtags/readme.md).

### language-subtag-registry

The file can be updated with:

```bash
wget https://www.iana.org/assignments/language-subtag-registry/language-subtag-registry \
  -O language-subtag-registry/language-subtag-registry
```

### ldml-keyboards

This data is from <https://github.com/unicode-org/cldr.git>

See also [ldml-keyboards/readme.md](ldml-keyboards/readme.md).

### unicode-character-database

Data from <https://www.unicode.org/Public/15.0.0/ucd/>:

```bash
UNICODE_VERSION=15.0.0
wget https://www.unicode.org/Public/${UNICODE_VERSION}/ucd/Blocks.txt -O unicode-character-database/Blocks.txt
wget https://www.unicode.org/Public/${UNICODE_VERSION}/ucd/UnicodeData.txt -O unicode-character-database/UnicodeData.txt
```

### windows-lcid-to-bcp-47

See [readme.md](windows-lcid-to-bcp-47/readme.md).
