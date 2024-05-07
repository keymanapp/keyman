# Keyman Core API verification

During GHA Debian package build we verify that the API didn't change
without being documented in `linux/debian/libkeymancore.symbols`.

[Debian policy](https://www.debian.org/doc/debian-policy/ch-sharedlibs#run-time-shared-libraries)
mandates that the API version of a shared library gets changed when the shared
library ABI changes in a way that could break binaries linked against
older versions of the shared library. This means that the API version should
change any time an interface is removed from the shared library or the
signature of an interface changes.

## Format of the `libkeymancore.symbols` file

The format of the file is documented in the
[deb-src-symbols man page](https://www.man7.org/linux/man-pages/man5/deb-src-symbols.5.html).

The majority of the lines are API method names followed by the
Keyman version where that symbol got introduced. Note that the lines
have to begin with a space character.

Example:

```
 km_core_state_actions_get@Base 17.0.197
```

## What to do when the API verification check fails

If you change the API but forget to update the `.symbols` file or the
API version number, the API verfication step in the GitHub Action
Ubuntu packaging workflow will fail.

There are different reasons why the API verification step can fail:

### You added a new API method

If you added a new API method to Keyman Core but didn't update the
`libkeymancore.symbols` file, `dpkg-gensymbols` will output a diff
between the old and the new API. You can edit `libkeymancore.symbols`
and add the missing line(s) that the diff shows. Note that the version
number should be the Keyman version that is current when the PR gets
merged.

### `dpkg-gensymbols` complains about C++ symbols

Unfortunately, `dpkg-gensymbols` which processes `libkeymancore.symbols` doesn't
work particularly well with C++, so it might flag some C++ symbols even though
they aren't part of the API. This happens particularly with C++ template
instantiations.

To work around this, we list the C++ symbols as `optional` that get flagged:

```
 (c++|optional)"typeinfo name for std::codecvt_utf8_utf16<char16_t, 1114111ul, (std::codecvt_mode)0>@Base" 17.0.244
```

`dpkg-gensymbols` will report the mangled C++ name in the diff, e.g.

```
 + _ZTSSt18codecvt_utf8_utf16IDsLm1114111ELSt12codecvt_mode0EE@Base 17.0.244
 ```

but for `libkeymancore.symbols` we need the demangled name. This can be
retrieved by running:

```bash
echo "_ZTSSt18codecvt_utf8_utf16IDsLm1114111ELSt12codecvt_mode0EE@Base" | c++filt
```

which will output the demangled name:

```
typeinfo name for std::codecvt_utf8_utf16<char16_t, 1114111ul, (std::codecvt_mode)0>@Base
```

Add the full output in quotes to `libkeymancore.symbols`, preceded by
`(c++|optional)` and followed by the version number.

Note that here again a space at the beginning of the line is required.

### You added an old Keyman version number

The API verification will also fail if you add a new line to the
`libkeymancore.symbols` file but don't update the version number at the
end of the line to the current Keyman version. In this case simply
modify the version number to the value you find in `VERSION.md`.

### You forgot to update the API version number in the `.symbols` file

When you remove or modify existing lines in the `libkeymancore.symbols`
file, you'll have to update the API version number at the top of the file
(the "1" in `libkeymancore.so.1`).

### You forgot to update `CORE_API_VERSION.md`

When you remove or modify API methods, you'll also have to update
the API version number in `CORE_API_VERSION.md`. If the API version
numbers in `CORE_API_VERSION.md` and in `libkeymancore.symbols` don't
match the API verification will fail.

## How this works

`.github/workflows/deb-packaging.yml` contains a `Verify API` step
which will call `linux/scripts/deb-packaging.sh` with the `verify`
parameter. Before calling the script it will download and extract
the binary package artifacts.

`deb-packaging.sh` will call `dpkg-gensymbols` which will compare the
previous API with the current one. Then the script will run some
additional verifications:

- checks that the version number of inserted lines matches `VERSION.md`
- checks that the API version number in `libkeymancore.symbols` got
  updated if necessary
- checks that `CORE_API_VERSION.md` got updated if necessary

`dpkg-gensymbols` will generate a new `symbols` file which the GHA
verification will archive.

Note that this `symbols` file can't be directly copied over
`libkeymancore.symbols` since it removes the C++ regex lines from the template.

### Running the API verification locally

It's possible to run the API verification locally. To do that, download
(or build) the binary package for at least one platform .

Then run

```bash
cd linux
dpkg -x libkeymancore*.deb /tmp
mkdir -p debian/tmp/DEBIAN
dpkg-gensymbols -v17.0.244 -plibkeymancore -e/tmp/usr/lib/x86_64-linux-gnu/libkeymancore.so* -c4
```
