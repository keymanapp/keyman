# Keyman for Linux

## Projects

 * kmflcomp - KMFL keyboard compiler
 * libkmfl - core library
 * ibus-kmfl - IBUS integration to use KMFL
 * scim\_kmfl\_imengine - deprecated SCIM engine to use KMFL

## Linux Requirements/Setup

Install packages required for building and developing KMFL
`sudo apt install cdbs debhelper libx11-dev autotools-dev build-essential dh-autoreconf flex bison libibus-1.0-dev`


### Compiling from Command Line

#### Build script

Run `./build.sh` to build and install kmflcomp, libkmfl and ibus-kmfl to `/tmp/kmfl`

set `$INSTALLDIR` to use a different install directory.
e.g. `INSTALLDIR=/home/me/kmfl ./build.sh`

The install of ibus-kmfl doesn't install everything to the correct location for it to be used - to be fixed

Also describe how to get from compiling to having something that ibus will use.

#### Manually

 * libkmfl requires kmfl.h header from kmflcomp
 * ibus-kfml requires kmfl.h and headers and lib from libkmfl
 
 So 
  * kmflcomp must be built and installed before libkmfl
  * libkmfl must be built and installed before ibus-kmfl
  
 For each project run `./configure && make && make install`.
 
 You may prefer to create a different directory to build in and run configure from there e.g. 
 
 `mkdir ../build-kmflcomp;`
 `cd ../build-kmflcomp`
 `../kmflcomp/configure` 
 `make `
 `make install`
  
  The install of ibus-kmfl doesn't install everything to the correct location for it to be used - to be fixed

### Continuous integration

Teamcity will run build.sh


### Building packages

TBD, hopefully jenkins

### Testing
Tests to be created

### Running Keyman for Linux

#### Setting up Ibus

Ibus should be running on a default install of Ubuntu

You may want to install extra packages to get other ibus input methods e.g ibus-unikey for VN

#### Getting a Keyman keyboard

TODO

After installing a Keyman keyboard you need to run `ibus restart` so that ibus will look for it.

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
  