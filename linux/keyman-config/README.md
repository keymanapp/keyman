# Linux KMP installer

## Preparing to run

Requires python3-requests and python3-requests-cache packages

If not already installed it needs python3, python3-gi

## Things to run

### view_installed.py

`./view_installed.py`

This shows a list of installed kmps.
For each one it has buttons to `show the welcome page`, `show more information` and `uninstall`.

##### Buttons

* `Refresh` - useful if you install or uninstall on the commandline while running view_installed.
* `Download keyboard...` - runs `DownloadKmpWindow` (see below)
* `Install keyboard...` - opens a file choose dialog to choose a kmp file to install and bring up the `InstallKmpWindow` for more details and to confirm installing.

### downloadkeyboard.py

`./downloadkeyboard.py` runs `DownloadKmpWindow`

This uses the keyman.com website to install kmps.

The website doesn't know about linux yet (until after 10.0 release) so
pretending to be a mac for now.

Search for a language or keyboard in the search box
Select a keyboard from the list
In 'Downloads for your device' there will be a 'Install keyboard' button for the keyboard for macOS
Click it to download the keyboard and bring up the `InstallKmpWindow` for more details and to confirm installing.

Secondary-click gives you a menu including 'Back' to go back a page.


### install_kmp.py

Command line installer for kmp

`./install_kmp.py -k <keyboard id>`
or
`./install_kmp.py -f <kmp file>`

### uninstall_kmp.py

Command line uninstaller for kmp

`./uninstall_kmp.py <keyboard id>`

### list_installed_kmp.py

`./list_installed_kmp.py` shows name, version, id, description of each installed keyboard

`./list_installed_kmp.py -s` shows name, version, id of each installed keyboard
