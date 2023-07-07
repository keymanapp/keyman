# standards-data

The subfolders of this directory contain data from various standards
that are used in Keyman.

## Current Versions

| data                       | version     |
|----------------------------|-------------|
| ethnologue                 | ?           |
| iso639-3                   | 2023        |
| langtags                   | 2023-05-04  |
| langtags-subtag-registry   | 2023-05-11  |
| ldml-keyboards             | techpreview |
| unicode-character-database | 15          |
| windows-lcid-to-bcp-47     | ?           |

## File Sources

### Ethnologue

??? (I'm not sure how to update this file or where it comes from)

### iso639-3

The file can be update with:

```bash
wget https://iso639-3.sil.org/sites/iso639-3/files/downloads/iso-639-3.tab -O iso639-3/iso639-3.tab
```

### langtags

The file is retrieved from <https://ldml.api.sil.org/langtags.json>:

```bash
wget https://ldml.api.sil.org/langtags.json -O langtags/langtags.json
```

### language-subtag-registry

The file can be updated with:

```bash
wget https://www.iana.org/assignments/language-subtag-registry/language-subtag-registry \
  -O language-subtag-registry/language-subtag-registry
```

### ldml-keyboards

This data is from <https://github.com/unicode-org/cldr.git>

### unicode-character-database

Data from <https://www.unicode.org/Public/15.0.0/ucd/>:

```bash
UNICODE_VERSION=15.0.0
wget https://www.unicode.org/Public/${UNICODE_VERSION}/ucd/Blocks.txt -O unicode-character-database/Blocks.txt
wget https://www.unicode.org/Public/${UNICODE_VERSION}/ucd/UnicodeData.txt -O unicode-character-database/UnicodeData.txt
```

### windows-lcid-to-bcp-47

See [readme.md](windows-lcid-to-bcp-47/readme.md).
