# Keyman for Linux

## Projects

- [keyman-config](../../linux/keyman-config) - `km-config` and some other tools
  to install, uninstall and view information about Keyman keyboard packages.
- [ibus-keyman](../../linux/ibus-keyman) - IBUS integration to use .kmp Keyman keyboards
- [keyman-system-service](../../linux/keyman-system-service) - A DBus system service
  that allows to perform keyboard related actions when running under Wayland.
- [core](../../core) - common keyboardprocessor library

See [license information](../../linux/LICENSE.md) about licensing.

## Linux Requirements/Setup

See [document in ../build](../build/linux-ubuntu.md).

## Compiling from Command Line

### Build script

#### Installing for ibus to use ibus-keyman

- The process to build and install everything is:

  - `linux/build.sh configure` to set the version and setup the build system
  - `linux/build.sh build` to build
  - `sudo linux/build.sh install` to install to `/usr/local`

- Some of the files must be installed to `/usr/share/` or `/usr/local/share`
  so `linux/build.sh install` must be run as `sudo`.

  To do this run `sudo linux/build.sh install`

  - This will install to `/usr/local` and
    `/usr/local/share/ibus/component/keyman.xml` and `/usr/local/share/keyman/icons`

- run `sudo linux/build.sh uninstall` to uninstall everything and put it back again

## Continuous integration

Teamcity PR builds will run `linux/build.sh install`

Master builds run `linux/build.sh install` and create source packages

Nightly and release builds upload the most recent new master build to <https://downloads.keyman.com>

## Building packages

See [Linux packaging doc](packaging.md)
for details on building Linux packages for Keyman.

## Testing

The tests can be run with the following command:

```bash
linux/build.sh test
```

To just run the unit tests without integration tests, add the
`--no-integration` parameter.

It's also possible to only run the tests for one of the subprojects. You
can use `build.sh` in the subdirectory for that.

Unit tests should be named after the file/class they are testing and
follow the pattern `*.tests.<ext>` (e.g. `keymanutil.tests.c`) or
for Python `*_tests.py`.

### ibus-keyman

If you want to run the ibus-keyman tests with Wayland, you'll have to
have `mutter` installed (version >=40). Otherwise you'll only be able
to run the tests with X11.

The tests can be run with the following command:

```bash
cd linux/ibus-keyman
./build.sh test
```

or with a script that accepts additional parameters:

```bash
cd linux/ibus-keyman/tests
scripts/run-tests
```

The `run-tests` script accepts different arguments which can be seen with
`scripts/run-tests --help`.

### Code Coverage Report

#### Prerequisites

Code coverage reports require some additional tools: lcov, gcovr,
libdatetime-perl, and coverage.

You can install these with:

```bash
sudo apt update
sudo apt install -y lcov libdatetime-perl gcovr
pip3 install coverage
```

**Note:** You want lcov > 1.16, so you might have to download and install
a newer version e.g. from <https://packages.ubuntu.com/mantic/lcov>.

#### Creating and displaying code coverage reports

All three projects (ibus-keyman, keyman-config, and keyman-system-service)
can produce code coverage reports.

Run `./build.sh --debug --coverage --report test` to create the report.

**Note:** You might have to run `./build.sh clean` first.

There's also an index page (`linux/CodecoverageReports.html`) that links to all
three reports. You can run
`linux/build.sh --debug --coverage --report --open --no-integration test`
to create the coverage reports for all three Linux projects and open the
index page in the browser.

## Running Keyman for Linux

### Setting up Ibus

Ibus should be running on a default install of Ubuntu.

You may want to install extra packages to get other ibus input methods e.g
**ibus-unikey** for VN.

Run `ibus restart` after installing any of them.

### Getting a Keyman keyboard

- After installing a Keyman keyboard usually ibus will be restarted
  automatically so that ibus will look for the new keyboard. If for
  whatever reason the keyboard doesn't show up in the system
  keyboard list, you can manually run `ibus restart`.

### Activating a Keyman keyboard

Keyman tries to activate the keyboard automatically. If you want to activate
it for a different language, you can do so by following the steps below.

#### GNOME3 (Ubuntu default)

- Click the connection/sound/shutdown section in the top right. Then the tools
  icon for Settings.

- In `Language and Region` click `+` to add a keyboard.
- Click the 3 dots expander then search for "Other" and click it
- The Keyman keyboards should be listed here to choose

- Use `Win-space` to switch between keyboards.

#### Cinnamon (wasta default)

- Open `Menu` and find `IBus Keyboards` (`IBus Preferences`) and run it
- Make sure `Show icon on system tray` is checked
- Select the tab `Input Method`.
- Click `Add` to add a keyman keyboard.
- Click the 3 dots expander then search for "Other" and click it
- The Keyman keyboards should be listed here to choose

#### MATE (alternative)

- Open `System-> Preferences -> Other -> IBus Preferences`.
- Make sure `Show icon on system tray` is checked
- Select the tab `Input Method`.
- Click `Add` to add a keyman keyboard.
- Click the 3 dots expander then search for "Other" and click it
- The Keyman keyboards should be listed here to choose
