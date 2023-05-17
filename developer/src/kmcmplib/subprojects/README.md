# developer/src/kmcmplib/subprojects/README.md

## What's this? What _is_ this?

- These are subprojects accessible to the `kmcmplib` meson project.

## icu

- ICU is used for the UnicodeSet parser, only.
- Patches for icu (i.e. meson build files) are in [`packagefiles/icu`](./packagefiles/icu/)
- ICU is lightly patched to remove support for UnicodeSet properties.
