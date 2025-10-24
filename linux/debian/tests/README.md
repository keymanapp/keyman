# autopkgtest

This directory contains tests for the packages.

See <https://packaging.ubuntu.com/html/auto-pkg-test.html> and
<https://salsa.debian.org/ci-team/autopkgtest/blob/master/doc/README.package-tests.rst>

**NOTE:** Runtime test names are only allowed to contain decimal digits,
lowercase ASCII letters, plus or minus signs, dots or slashes.

## Manually run the tests

- build source and binary packages (see [packaging.md](../../../docs/linux/packaging.md))

- install dependencies

  ```bash
  sudo apt install autopkgtest qemu-system qemu-utils autodep8
  ```

- create environment. Several different options are available, see
  [README.running-tests](https://salsa.debian.org/ci-team/autopkgtest/blob/master/doc/README.running-tests.rst)

  For example:

  ```bash
  autopkgtest-buildvm-ubuntu-cloud -v --release=noble
  ```

- Run tests

  ```bash
  autopkgtest --apt-upgrade --no-built-binaries *.deb keyman_*.dsc -- qemu autopkgtest-noble-amd64.img
  ```

## Developing tests

While developing tests, it is possible to run the tests directly from
the source tree:

```bash
cd linux
autopkgtest --apt-upgrade --no-built-binaries ../artifacts/*.deb . -- qemu /path/to/autopkgtest-noble-amd64.img
```

This requires packages to be available in `${KEYMAN_ROOT}/artifacts`.
These can be built from source with:

```bash
cd linux
rm -rf debian/patches/ keyman-config/keyman_config/.pc
scripts/deb-packaging.sh source
DIST=noble
cd ..
docker run -v $(pwd):/source -i -t -w /source --platform=amd64 \
  --env INPUT_DIST=$DIST --env INPUT_SOURCE_DIR=. \
  --env INPUT_SOURCEPACKAGE=$(ls keyman_*.dsc | sort | tail -1) \
  sillsdev/$DIST
```

New packages have to be built for changes outside of `linux/debian`.
Changes in files in `linux/debian` are automatically considered when
running `autopkgtest`.
