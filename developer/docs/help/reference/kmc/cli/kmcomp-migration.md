---
title: Migrating from kmcomp to kmc
---

`kmcomp` was the command-line compiler for Keyman Developer through version
16.0. Version 17.0 replaces `kmcomp` with `kmc`.

The lexical model command-line tooling, `kmlmc`, `kmlmp`, and `kmlmi`, are all
still present in version 17, but are deprecated, as the same tasks can be
performed with `kmc`.

## Benefits

* Unlike `kmcomp`, which was a Windows-only console application, `kmc` is a
  cross-platform application, running on Windows, Linux, and macOS, built in
  node.js.
* `kmc` parameters have been redesigned to be more consistent, and easily
  extensible for new functionality.
* `kmc` can run in a batch mode that is up to 100x faster than `kmcomp`.
* `kmc` can be easily integrated into GitHub Actions and other continuous
  integration tooling.

## Compiling a project

To compile a project, `kmcomp` used a command line such as:

```bash
kmcomp path\to\my_keyboard.kpj
```

With `kmc`, you would use:

```bash
kmc build path/to/my_keyboard.kpj
# or you can specify just the folder:
kmc build path/to/
# If you are in the same folder as the .kpj:
kmc build .
```

This is the recommended way of using `kmc` -- always build an entire project,
rather than just the keyboard.

## Compiling a keyboard

To compile a keyboard, `kmcomp` builds used command lines such as:

```bash
# building a .kmx
kmcomp my_keyboard.kmn my_keyboard.kmx
# building a .js (for KeymanWeb)
kmcomp my_keyboard.kmn my_keyboard.js
```

With `kmc`, you would use:

```bash
# building all outputs
kmc build my_keyboard.kmn
# or to specify output files (.js inferred from .kmx, and always built)
kmc build my_keyboard.kmn -o my_keyboard.kmx
```

## Compiling a package

To compile a package, `kmcomp` builds used command lines such as:

```bash
kmcomp my_keyboard.kps
```

With `kmc`, you would use:

```bash
kmc build my_keyboard.kps
# to specify output file or path
kmc build my_keyboard.kps -o path/to/my_keyboard.kmp
```

## Map of parameters

The following table lists common parameters in `kmcomp`, and the corresponding
parameter in `kmc`:

kmcomp                 | kmc                                   | notes
-----------------------|---------------------------------------|-------------
`-h`, `-help`          | `-h`, `--help`                        | Note that `kmc --help` can be used for further detail on subcommands, e.g. `kmc build --help`
`-s`                   | `-l warn`, `--log-level warn`         | Silent option, suppresses informational and hint-level messages
`-ss`                  | `-l silent`, `--log-level silent`     | Super-silent option, suppresses all messages, except fatal internal errors
`-nologo`              | not required                          | `kmc` does not emit a compiler description
`-c`                   | N/A                                   | `kmc` does not currently support the `clean` command, this will be supported in future versions
`-d`                   | `-d`, `--debug`                       | Include debug information in output files
`-w`                   | `-w`, `--compiler-warnings-as-errors` | Causes warnings to fail the build; overrides project-level option
`-cfc`                 | not required                          | Filename convention checks are now standard hints/warnings in the build
`-t`                   | N/A                                   | Build specified target from .kpj, not supported in kmc
`-color`               | `--color`                             | Force colorization for log messages
`-no-color`            | `--no-color`                          | No colorization for log messages
`-no-compiler-version` | `--no-compiler-version`               | Exclude compiler version metadata from output files

## Compiling a lexical model

`kmlmc` and `kmlmp` were separate tools in earlier versions of Keyman Developer,
for compiling lexical models and lexical model packages. They have both been
replaced with `kmc`.

Old method, using kmlmc and kmlmp:

```bash
kmlmc file.model.ts
# or specifying output filename
kmlmc -o output/path/file.model.js file.model.ts
kmlmp file.model.kps
```

New method, using kmc:

```bash
# recommended, build the model project:
kmc build .
# building single files:
kmc build file.model.ts
kmc build file.model.ts -o output/path/file.model.js
kmc build file.model.kps
```

