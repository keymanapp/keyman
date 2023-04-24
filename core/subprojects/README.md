# core/subprojects

## What's this? What _is_ this?

- These are subprojects accessible to the `core` meson project.

## icu

- When building for Linux, [ICU](https://icu.unicode.org) must already be installed. The system ICU will be used instead of the subproject.
- Patches for icu (i.e. meson build files) are in [`packagefiles/icu`](./packagefiles/icu/)
- It would be preferable to use meson's [wrapdb](https://mesonbuild.com/Wrapdb-projects.html), and in fact the patchfiles here are sourced from that project.
- However, having our own patchfiles allows for slimming down ICU, at least until the wrapdb version is more configurable [mesonbuild/wrapdb#950](https://github.com/mesonbuild/wrapdb/issues/950)
