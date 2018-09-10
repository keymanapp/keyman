# Linux KMP installer

## Preparing to run

If you are running from the repo or installing keyman-config manually rather than from a package
then you will need to

`sudo apt install python3-lxml python3-magic python3-numpy python3-pil python3-requests 
python3-requests-cache python3 python3-gi gir1.2-webkit-3.0 dconf-cli python3-setuptools`

You will also need kmflcomp either from a package or built and installed locally.

run the script `./createkeymandirs.sh` to create the directories for these programs to
install the packages to

### Installing manually

`make && sudo make install` will install locally to /usr/local
`python3 setup.py --help install` will give you more install options

## Things to run

### km-config

`./km-config`

This shows a list of installed kmps.
For each one it has buttons to `show the welcome page`, `show more information` and `uninstall`.

##### Buttons

* `Refresh` - useful if you install or uninstall on the commandline while running km-config.
* `Download keyboard...` - runs `DownloadKmpWindow` (see below)
* `Install keyboard...` - opens a file choose dialog to choose a kmp file to install and bring up the `InstallKmpWindow` for more details and to confirm installing.

##### Download window

This uses the keyman.com website to install kmps.

The website doesn't know about linux yet (until after 10.0 release) so
pretending to be a mac for now.

Search for a language or keyboard in the search box
Select a keyboard from the list
In 'Downloads for your device' there will be a 'Install keyboard' button for the keyboard for macOS
Click it to download the keyboard and bring up the `InstallKmpWindow` for more details and to confirm installing.

Secondary-click gives you a menu including 'Back' to go back a page.


### km-package-install

Command line installer for kmp

`km-package-install -k <keyboard id>`
or
`km-package-install -f <kmp file>`

### km-package-uninstall

Command line uninstaller for kmp

`km-package-uninstall <keyboard id>`

### km-package-list-installed

`km-package-list-installed` shows name, version, id, description of each installed keyboard

`km-package-list-installed -s` shows name, version, id of each installed keyboard

### km-package-get

Download Keyman keyboard package to ~/Downloads

`km-package-get <keyboard id>`

### km-kvk2ldml

Convert a Keyman kvk on-screen keyboard file to an LDML file. Optionally print
the details of the kvk file [-p] optionally with all keys [-k].

`km-kvk2ldml [-p] [-k] [-o LDMLFILE] <kvk file>`

## Building the Debian package

You will need the build dependencies as well as the runtime dependencies above

`sudo apt install dh-python python3-all debhelper help2man`

Run `make deb`. This will build the Debian package in the make_deb directory.
