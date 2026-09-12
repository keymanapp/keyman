# standards-data

The subfolders of this directory contain data from various standards
that are used in Keyman. If you update any files, be sure to update
the version data in this file also.

## Current Versions

See [minimum-versions.md](../../docs/minimum-versions.md)

Unversioned data:

| data                       | version         |
|----------------------------|-----------------|
| iso639-3                   | (download date) |
| ethnologue                 | unknown         |
| windows-lcid-to-bcp-47     | unknown         |

## File Sources

### Ethnologue

* See: [ethnologue/README.md](ethnologue/README.md)

### iso639-3

* Source: https://iso639-3.sil.org/sites/iso639-3/files/downloads/iso-639-3.tab
* Update:

  ```bash
  ./iso639-3/download.sh
  ```

* See also: [iso639-3/README.md](iso639-3/README.md).

### langtags

* Source: <https://ldml.api.sil.org/langtags.json>
* Update:

  ```bash
  ./langtags/download.sh
  ```

* See also: [langags/README.md](langtags/README.md).

### language-subtag-registry

* Source: https://www.iana.org/assignments/language-subtag-registry/language-subtag-registry
* Update:

  ```bash
  ./language-subtag-registry/download.sh
  ```

* See also: [language-subtag-registry/README.md](language-subtag-registry/README.md).

### ldml-keyboards

* Source: <https://github.com/unicode-org/cldr.git>
* Update:

  ```bash
  ./ldml-keyboards/download.sh
  ```

* See also: [ldml-keyboards/README.md](ldml-keyboards/README.md).

### unicode-character-database

* Source: <https://www.unicode.org/Public/17.0.0/ucd/>
* Update:

  ```bash
  ./unicode-character-database/download.sh
  ```

* See also [unicode-character-database/README.md](unicode-character-database/README.md).

### windows-lcid-to-bcp-47

* See [windows-lcid-to-bcp-47/README.md](windows-lcid-to-bcp-47/README.md).
