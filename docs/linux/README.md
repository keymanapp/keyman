# Keyman for Linux

## Projects

- [keyman-config](../../linux/keyman-config) - km-config and some other tools to install, uninstall
  and view information about Keyman keyboard packages.
- [ibus-keyman](../../linux/ibus-keyman) - IBUS integration to use .kmp Keyman keyboards
- [core](../../core) - common keyboardprocessor library

See [license information](../../linux/LICENSE.md) about licensing.

## Linux Requirements/Setup

- It is helpful to be using the [packages.sil.org](http://packages.sil.org) repo

- Install packages required for building and developing Keyman for Linux.
  The list of required packages can be seen in `linux/debian/control`.
  It is easiest to use the `mk-build-deps` tool to install the
  dependencies:

  ```bash
  sudo apt update
  sudo apt install devscripts equivs
  sudo mk-build-deps --install linux/debian/control
  ```

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

### keyman-config

The unit tests can be run with the following command:

```bash
cd linux/keyman-config
./run-tests.sh
```

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

#### GNOME3 (focal and bionic default, also newer Ubuntu versions)

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
