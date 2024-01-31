# Keyman for Linux

## Projects

- [keyman-config](./keyman-config) - km-config and some other tools to install, uninstall
  and view information about Keyman keyboard packages.
- [ibus-keyman](./ibus-keyman) - IBUS integration to use .kmp Keyman keyboards
- [core](../core) - common keyboardprocessor library
- [legacy/kmflcomp](./legacy/kmflcomp) - KMFL keyboard compiler
- [legacy/libkmfl](./legacy/libkmfl) - older KMFL core library
- [legacy/ibus-kmfl](./legacy/ibus-kmfl) - IBUS integration to use KMFL

See [license information](./LICENSE.md) about licensing.

## Linux Requirements/Setup

- It is helpful to be using the [packages.sil.org](http://packages.sil.org) repo

- Install packages required for building and developing Keyman for Linux

  ```bash
  sudo apt install cdbs debhelper libx11-dev autotools-dev build-essential \
    dh-autoreconf flex bison libibus-1.0-dev python3-setuptools meson \
    libjson-glib-dev libgtk-3-dev libxml2-utils help2man python3-lxml \
    python3-magic python3-numpy python3-pil python3-pip python3-qrcode \
    python3-requests python3-requests-cache python3 python3-gi dconf-cli \
    dconf-editor cargo python3-dbus gir1.2-webkit2-4.1 ibus libglib2.0-bin \
    liblocale-gettext-perl xvfb xserver-xephyr metacity mutter
  ```

## Compiling from Command Line

### Build script

#### Installing for ibus to use ibus-keyman

- The process to build and install everything is:

  - `make reconf` to create the build system and set the version
  - `make fullbuild` to configure and build
  - `sudo make install` to install to `/usr/local`

- Some of the files must be installed to `/usr/share/` so `make install` must be run as `sudo`.

  To do this run `sudo make install`

  - This will install to `/usr/local`
    - and `/usr/share/ibus/component/keyman.xml` and `/usr/share/keyman/icons`

  - If you already have the ibus-keyman package installed then it will move the file `/usr/share/ibus/component/keyman.xml` to `/usr/share/doc/ibus-keyman/`

- run `sudo make uninstall` to uninstall everything and put it back again

#### Tmp install

Used by TC for validating PRs

Run `make tmpinstall` to build and install **keyboardprocessor** and **ibus-keyman** to `/tmp/keyman`

This is only for testing the build, not for running **ibus-keyman** in ibus

### Manually

- **ibus-keyman** requires headers and lib from **keyboardprocessor**

So

- **keyboardprocessor** must be built before **ibus-keyman**

For each project run `./configure && make && make install`.

You may prefer to create a different directory to build in and run configure from there e.g.

```bash
mkdir ../build-ibus-keyman
cd ../build-ibus-keyman
../ibus-keyman/configure
make
make install
```

## Continuous integration

Teamcity PR builds will run `make tmpinstall`

Master builds run `make tmpinstall` and create source packages

Nightly and release builds upload the most recent new master build to <https://downloads.keyman.com>

## Building packages

See [Linux packaging doc](../docs/linux-packaging.md)
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
cd linux/ibus-keyman/tests
./run-tests
```

The `run-tests` script accepts different arguments which can be seen with
`./run-tests --help`.

## Running Keyman for Linux

### Setting up Ibus

Ibus should be running on a default install of Ubuntu

You may want to install extra packages to get other ibus input methods e.g **ibus-unikey** for VN

Run `ibus restart` after installing any of them

### Getting a Keyman keyboard

- After installing a Keyman keyboard usually ibus will be restarted automatically so that ibus will
  look for the new keyboard. If for whatever reason the keyboard doesn't show up in the system
  keyboard list, you can manually run `ibus restart`.

### Activating a Keyman keyboard

Keyman tries to activate the keyboard automatically. If you want to activate it for a different
language, you can do so by following the steps below.

#### GNOME3 (focal and bionic default, also newer Ubuntu versions)

- Click the connection/sound/shutdown section in the top right. Then the tools icon for Settings.

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
