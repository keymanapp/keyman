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

 * Right-click on the ibus status icon and open `Preferences`.
 * Select the tab `Input Method`.
 * Click `Add` to add a keyman keyboard.
 * Click the 3 dots expander then `Other` which is at the bottom
 * The list of keyboards will include the Keyman ones
  