# Linux KMP installer

## Preparing to run

If you are running from the repo or installing keyman-config manually rather than from a package
then you will need to

`sudo apt install python3-lxml python3-magic python3-numpy python3-pil python3-requests 
python3-requests-cache python3 python3-gi gir1.2-webkit-3.0 dconf-cli python3-setuptools`

You will also need kmflcomp either from a package or built and installed locally.

run the script `./createkeymandirs.sh` to create the directories for these programs to
install the packages to

## Things to run

### keyman-config

`./keyman-config`

This shows a list of installed kmps.
For each one it has buttons to `show the welcome page`, `show more information` and `uninstall`.

##### Buttons

* `Refresh` - useful if you install or uninstall on the commandline while running keyman-config.
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


### install_kmp

Command line installer for kmp

`install_kmp -k <keyboard id>`
or
`install_kmp -f <kmp file>`

### uninstall_kmp

Command line uninstaller for kmp

`.uninstall_kmp <keyboard id>`

### list_installed_kmp

`list_installed_kmp` shows name, version, id, description of each installed keyboard

`list_installed_kmp.py` shows name, version, id of each installed keyboard

## Building the Debian package

You will need the build dependencies as well as the runtime dependencies above

`sudo apt install dh-python python3-all debhelper`

Run `make deb`. This will build the Debian package in the make_deb directory.
