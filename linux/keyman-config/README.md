# Linux KMP installer

## Preparing to run

If you are running from the repo or installing keyman-config manually rather than from a package
then you will need to:

```bash
sudo apt install python3-lxml python3-magic python3-numpy python3-qrcode python3-pil \
    python3-requests python3-requests-cache python3 python3-gi gir1.2-webkit2-4.0 dconf-cli \
    python3-setuptools python3-pip python3-dbus ibus
```

Either `python3-raven` or `python3-sentry-sdk` is required as well. To install it on Ubuntu 18.04 and earlier run:

```bash
sudo apt install python3-raven
```

On Ubuntu 20.04 and later:

```bash
sudo apt install python3-sentry-sdk
```

(It's also possible to install it with pip: `pip3 install sentry-sdk`)

Run the script `./createkeymandirs.sh` to create the directories for these programs to
install the packages to.

Also copy and compile the GSettings schema:

```bash
cd keyman_config
sudo cp com.keyman.gschema.xml /usr/share/glib-2.0/schemas
sudo glib-compile-schemas /usr/share/glib-2.0/schemas
```

### Standards data file

Running `km-config` requires a language tag mapping file
`keyman_config/standards/lang_tags_map.py`. This file gets generated during a package
build, and also when running `make`.

### Installing manually from the repo

`make && sudo make install` will install locally to `/usr/local`

`python3 setup.py --help install` will give you more install options

You will need `sudo apt install python3-pip` to `make uninstall`

## Things to run from the command line

### km-config

`./km-config`

This displays a configuration panel that shows the currently installed Keyman keyboard packages and can download and install additional keyboards.

#### Buttons

* `Uninstall` - uninstall selected keyboard
* `About` - show information about selected keyboard
* `Help` - display help documentation about selected keyboard
* `Options` - display options.htm form for setting keyboard options

-----------------------------------

* `Refresh` - useful if you install or uninstall on the commandline while running km-config.
* `Download` - runs `DownloadKmpWindow` (see below)
* `Install` - opens a file choose dialog to choose a kmp file to install and bring up the `InstallKmpWindow` for more details and to confirm installing.
* `Close` - close the configuration panel

#### Download window

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

`km-kvk2ldml [-p] [-k] [-o LDMLFILE] <kvk file>` Convert a Keyman kvk on-screen keyboard file to an LDML file. Optionally print the details of the kvk file (`-p`) optionally with all keys (`-k`).

## Building the Debian package

You will need the build dependencies as well as the runtime dependencies above

`sudo apt install dh-python python3-all debhelper help2man`

Run `make deb`. This will build the Debian package in the `make_deb` directory.

## Internationalization

### Create or update i18n template file

Run

```bash
make update-template
```

This will create or update the file `locale/keyman-config.pot`.

### Add translations for a new language

To add translations for a new language run (replacing `de_DE` with the desired locale):

```bash
cd locale
msginit --locale=de_DE.UTF-8 --width=98 --input keyman-config.pot
```

This will create the file `locale/de.po`.

**NOTE:** Specifying _UTF-8_ is important if any non-ASCII characters will be used in the
translation, i.e. always.

**NOTE:** This step is not necessary when using Crowdin

### Update translations

After strings were added or modified the translated po files need to be updated. For this
call, replacing `de` with the desired locale:

```bash
make locale/de.po
```

Alternatively you can also update all po files at once:

```bash
make update-po
```

**NOTE:** This step is not necessary when using Crowdin

### Compile translations

To create the binary files for the translations, run:

```bash
make compile-po
```

This will create `.mo` files, e.g. `locale/de/LC_MESSAGES/keyman-config.mo`.

### Testing localization

```bash
LANGUAGE=de ./km-config
```
