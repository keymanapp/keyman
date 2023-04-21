# core/subprojects

## What's this? What _is_ this?

- These are subprojects accessible to the `core` meson project.

## icu

- Ideally we would use meson's [wrapdb](https://mesonbuild.com/Wrapdb-projects.html), and in fact the files here are sourced from that project.
- However, it requires a much newer meson version to work properly: 0.55 at least.
- Patches for icu (i.e. meson build files) are in [`packagefiles/icu`](./packagefiles/icu/)
