# Keyman for Linux

## Projects

 * [kmflcomp](./kmflcomp) - KMFL keyboard compiler
 * [libkmfl](./libkmfl) - core library
 * [ibus-kmfl](./ibus-kmfl) - IBUS integration to use KMFL
 * [ibus-keyman](./ibus-keyman) - IBUS integration to use .kmx Keyman keyboards
 * scim\_kmfl\_imengine - deprecated SCIM engine to use KMFL

 See [license information](./LICENSE.md) about licensing.

## Linux Requirements/Setup

- It is helpful to be using the [packages.sil.org](http://packages.sil.org) repo

- Install packages required for building and developing KMFL and Keyman for Linux
`sudo apt install cdbs debhelper libx11-dev autotools-dev build-essential dh-autoreconf flex bison libibus-1.0-dev python3-setuptools meson`


### Compiling from Command Line

#### Build script

##### Tmp install

Run `make tmpinstall` to build and install keyboardprocessor, kmflcomp, libkmfl, ibus-kmfl and ibus-keyman to `/tmp/kmfl`

This is only for testing the build, not for running ibus-kmfl or ibus-keyman in ibus


##### Installing for ibus to use ibus-kmfl or ibus-keyman

- The process to build and install everything is:

    * `make devreconf` (or `make reconf`) to create the build system and set the version
    * `make configure`
    * `make` to build
    * `sudo make install` to install to /usr/local

- Some of the files must be installed to `/usr/share/` so `make install` must be run as `sudo`. 

 - To do this run ` sudo make install`

    * This will install to /usr/local
        * and `/usr/share/ibus/component/kmfl.xml` and `/usr/share/kmfl/icons`
        * and `/usr/share/ibus/component/keyman.xml` and `/usr/share/keyman/icons`

    * If you already have the ibus-kmfl package installed then it will move the file `/usr/share/ibus/component/kmfl.xml` to `/usr/share/doc/ibus-kmfl/`
    * If you already have the ibus-keyman package installed then it will move the file `/usr/share/ibus/component/keyman.xml` to `/usr/share/doc/ibus-keyman/`

        * run `make uninstall` to uninstall everything and put it back again

#### Manually

 * libkmfl requires kmfl.h header from kmflcomp
 * ibus-kfml requires kmfl.h and headers and lib from libkmfl
 * ibus-keyman requires headers and lib from keyboardprocessor
 
 So 
  * kmflcomp must be built and installed before libkmfl
  * libkmfl must be built and installed before ibus-kmfl
  * keyboardprocessor must be built before ibus-keyman
  
 For each project run `./configure && make && make install`.
 
 You may prefer to create a different directory to build in and run configure from there e.g. 
 
 `mkdir ../build-kmflcomp;`
 `cd ../build-kmflcomp`
 `../kmflcomp/configure` 
 `make `
 `make install`
  
  The install of ibus-kmfl doesn't install everything to the correct location for it to be used - to be fixed

### Continuous integration

Teamcity PR builds will run `make tmpinstall`
Master builds run `make tmpinstall` and create source packages
Nightly builds upload the most recent new master build to https://downloads.keyman.com

### Building packages

Jenkins now continuously builds Debian packages on every commit to master
Periodically test packages will be uploaded to https://launchpad.net/~keymanapp/+archive/ubuntu/keyman-daily

### Testing
Tests to be created as there are no current tests

### Running Keyman for Linux

#### Setting up Ibus

Ibus should be running on a default install of Ubuntu

You may want to install extra packages to get other ibus input methods e.g ibus-unikey for VN

Run `ibus restart` after installing any of them

#### Getting a Keyman keyboard

- There are some kmfl keyboards available on packages.sil.org. Search for "kmfl-keyboard". These will also install the ibus-kmfl package.

- You can find more `.kmn` source keyboards at the [Keyboards repo](https://github.com/keymanapp/keyboards). They may or may not currently work with KMFL.

- The `.kmn` file should be put in `/usr/share/kmfl` or `~/.kmfl`, preferably `~/.kmfl`
    * TODO: let ibus-kmfl find keyboards in `/usr/local/share/kmfl` as well

- After installing a Keyman keyboard you need to run `ibus restart` so that ibus will look for it. (TODO: double check this)

#### Activating a Keyman keyboard

##### GNOME3 (bionic default)

 * Click the connection/sound/shutdown section in the top right. Then the tools icon for Settings.

 * In `Language and Region` click `+` to add a keyboard.
 * Click the 3 dots expander then search for "Other" and click it
 * The Keyman keyboards should be listed here to choose

 * Use `Win-space` to switch between keyboards.

##### Unity (xenial default)

 * Run `Text Entry`
 * Click `+` to add a keyboard.
 * In `Choose an input source` search for "Other" or the keyboard name, or "Ibus" to find Keyman keyboards.

Check `Show current input source in the menu bar` to see what keyboard you are using and have a menu for keyboards.

Use `Win-space` to switch between keyboards.

##### Cinnamon (wasta xenial default)

 * Open `Menu` and find `IBus Preferences`and run it
 * Make sure `Show icon on system tray` is checked
 * Select the tab `Input Method`.
 * Click `Add` to add a keyman keyboard.
 * Click the 3 dots expander then search for "Other" and click it
 * The Keyman keyboards should be listed here to choose

##### MATE (alternative)

 * Open `System-> Preferences -> Other -> IBus Preferences`.
 * Make sure `Show icon on system tray` is checked
 * Select the tab `Input Method`.
 * Click `Add` to add a keyman keyboard.
 * Click the 3 dots expander then search for "Other" and click it
 * The Keyman keyboards should be listed here to choose
  
