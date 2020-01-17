# Linux KMP installer

## Preparing to run

If you are running from the repo or installing keyman-config manually rather than from a package
then you will need to

`sudo apt install python3-lxml python3-magic python3-numpy python3-pil python3-requests 
python3-requests-cache python3 python3-gi gir1.2-webkit-3.0 dconf-cli python3-setuptools`

You will also need kmflcomp either from a package or built and installed locally.

Run the script `./createkeymandirs.sh` to create the directories for these programs to
install the packages to

Also copy and compile the GSettings schema
```bash
cd keyman_config
sudo cp com.keyman.gschema.xml /usr/share/glib-2.0/schemas
sudo glib-compile-schemas /usr/share/glib-2.0/schemas
```

### Installing manually from the repo

`make && sudo make install` will install locally to /usr/local
`python3 setup.py --help install` will give you more install options
You will need `sudo apt install python3-pip` to `make uninstall`

## Things to run from the command line

### km-config

`./km-config`

This displays a configuration panel that shows the currently installed Keyman keyboard packages and can download and install additional keyboards.

##### Buttons

* `Uninstall` - uninstall selected keyboard
* `About` - show information about selected keyboard
* `Help` - display help documentation about selected keyboard
* `Options` - display options.htm form for setting keyboard options
-----------------------------------

* `Refresh` - useful if you install or uninstall on the commandline while running km-config.
* `Download` - runs `DownloadKmpWindow` (see below)
* `Install` - opens a file choose dialog to choose a kmp file to install and bring up the `InstallKmpWindow` for more details and to confirm installing.
* `Close` - close the configuration panel

##### Download window

This uses the keyman.com website to install kmps.

Search for a language or keyboard in the search box.
Select a keyboard from the list.
In 'Downloads for your device' there will be an 'Install keyboard' button for the keyboard for Linux.
Click it to download the keyboard and bring up the `InstallKmpWindow` for more details and to confirm installing.

Secondary-click gives you a menu including 'Back' to go back a page.


### km-package-install

`km-package-install -p <keyboard package id>` install Keyman keyboard package from the keyman.com server

`km-package-install -f <kmp file>` install Keyman keyboard package from a local .kmp file

### km-package-uninstall

`km-package-uninstall <keyboard id>` uninstall Keyman keyboard package

`km-package-uninstall -s <keyboard id>` uninstall from shared area `/usr/local`

### km-package-list-installed

`km-package-list-installed` shows name, version, id, description of each installed keyboard

`km-package-list-installed -s` shows those installed in shared areas

`km-package-list-installed -os` shows those installed by the OS

`km-package-list-installed -u` shows those installed in user areas

### km-package-get

`km-package-get <keyboard id>` download Keyman keyboard package to `~/.cache/keyman`

### km-kvk2ldml

`km-kvk2ldml [-p] [-k] [-o LDMLFILE] <kvk file>` Convert a Keyman kvk on-screen keyboard file to an LDML file. Optionally print the details of the kvk file [-p] optionally with all keys [-k].


## Building the Debian package

You will need the build dependencies as well as the runtime dependencies above

`sudo apt install dh-python python3-all debhelper help2man`

Run `make deb`. This will build the Debian package in the make_deb directory.
