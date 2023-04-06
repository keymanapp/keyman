# core/subprojects

## What's this? What _is_ this?

- These are subprojects accessible to the `core` meson project.

## icu

### Why are there several files for ICU?

- Ideally we would use meson's [wrapdb](https://mesonbuild.com/Wrapdb-projects.html), and in fact the files here are sourced from that project.
- However, it requires a much newer meson version to work properly: 0.55 at least.
- So the file `packagefiles/icu-minimal.zip` is generated from the files in [`packagefiles/icu`](./packagefiles/icu/)
- Regenerate it with these commands:

```shell
cd core/subprojects/packagefiles
rm icu-minimal.zip
zip icu-minimal.zip -r ./icu .
```

