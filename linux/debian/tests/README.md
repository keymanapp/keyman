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
  autopkgtest-buildvm-ubuntu-cloud -v --release=jammy
  ```

- Run tests

  ```bash
  autopkgtest -B *.deb keyman_*.dsc -- qemu autopkgtest-jammy-amd64.img
  ```
