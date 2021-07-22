# Keyman for Windows and Keyman Developer

## Build Prerequisites

1. Install [VS2019 Community Edition](#visual-studio-2019-community-edition-setup-requirements).
2. Install [Delphi 10.3](#delphi-setup-requirements).
3. Install [git](https://git-scm.com/download/win).
4. Install [nodejs](https://nodejs.org/en/download/).
5. Follow steps in /docs/build/old/web-notes.md to install prerequisites for
   building KeymanWeb (included in Keyman Developer)
6. Follow steps in /docs/build/old/core-desktop-notes.md to install
   prerequisites for building Keyman Core.
7. Add the Keyman root folder to antivirus exclusions for performance and file
   lock reasons (optional - but highly recommended).
8. Start Delphi IDE once after installation, so that Delphi can automatically
   create required default environment files; it will also ensure registration
   is complete.
9. Set environment variables per [notes below](#environment-variables):
   `KEYMAN_ROOT`, `USERDEFINES`, `GIT_BASH_FOR_KEYMAN`,
   `KEYMAN_CEF4DELPHI_ROOT`.
10. Add the **windows/lib** folder in the Keyman repository to your `PATH`
    environment variable (required for packages in Delphi).
11. In order to run Keyman in the development build, you need to specify where
    the https://github.com/keymanapp/CEF4Delphi_binary repo is on your system,
    with the registry setting `HKCU\Software\Keyman\Debug`, `Debug_CEFPath`.
    (Alternatively, install release builds of Keyman for Windows and Keyman
    Desktop.)
12. If you have configured npm to use bash or another alternate shell, you do
    need to avoid this for the Keyman repo. npm must use cmd for its shell: `npm
    config delete script-shell` (we may change this in the future)

### Release build prerequisites

For local development you do not need to perform a release build so these are
optional.

1. Install [7-Zip](http://www.7-zip.org/) 64-bit (or 32-bit on x86 Windows).
   7-Zip is used for archiving build files -- may be eliminated in future.
2. Install [HTML Help Workshop](https://www.microsoft.com/en-us/download/details.aspx?id=21138).
3. Install [pandoc](https://pandoc.org/) (`choco install pandoc`) 2.11.2 or
   later
4. Install [WiX 3.11.1](https://github.com/wixtoolset/wix3/releases/tag/wix3111rtm)
   to **C:\Program Files (x86)\WiX Toolset v3.11**.
5. Add the environment variable `KEYMAN_CEF4DELPHI_ROOT`.

## Building Keyman for Windows and Keyman Developer

1. Start 'Developer Command Prompt for VS 2019'.
2. Run `make build` from the **windows/src** folder.
3. Artifacts from a successful build will be placed in **windows/bin** folder.

*Note*: running `make build` will currently reset the packages and path settings
in your Delphi environment. If you use Delphi for other projects, you should
consider building Keyman under a login user dedicated to it, or in a VM.

Type `make` to see build targets. Common build targets are:

  `make build` <-- builds Keyman for Windows and Keyman Developer
  `make clean` <-- remove temporary files and build artifacts
  `make release` <-- makes a release of all Keyman Windows projects

The version will have a major and minor component that reflects the current
release version -- but may not match the actual version.

### Release build

To perform a release build, you will need to obtain valid certificates. A
release build is unnecessary for local development. Release builds are currently
run from a TeamCity CI environment.

1. Start 'Developer Command Prompt for VS 2019'.
2. Run `make release` from the **windows/src** folder.
3. Artifacts from a successful build will be placed in **windows/release**
   folder.
4. **buildtools/help-keyman-com.sh** will push updated documentation to
   help.keyman.com. Environment variable `HELP_KEYMAN_COM` needs to be set to
   the root of the local help.keyman.com git repository.

Note: by default, the version number generated may vary from the current release
version. You will not be able to install it over a later version of Keyman, and
will need to uninstall and reinstall.

## Installing Keyman

These steps are only required the first time you install Keyman:
1. Install release versions of Keyman for Windows and Keyman Developer for the
   branch in which you are working (e.g. for master branch, download latest
   alpha, for beta, download latest beta, etc).
   * Download from https://keyman.com/downloads/ and run the installers.
2. Install the Keyman test certificates. Do the following for each KeymanTestCA
   cert in **windows/src/buildtools/certificates**:
    1. Open the certificate and click 'Install certificate...' to open the
       Certificate Import Wizard.
    2. Select either 'Current User' or 'Local Machine' for 'Store Location'
       depending on your preference and click Next.
    3. Select 'Place all certificates in the following store' and 'Browse...' to
       select 'Trusted Root Certification Authorities'. Click Next and finish
       the wizard.

To deploy a development build of Keyman and Keyman Developer,
1. Start 'Developer Command Prompt for VS 2019'.
2. Run `make signcode` from the **windows/src** folder.
3. Ensure that Keyman and Keyman Developer are not running.
4. Run `make install` in **windows/src/engine**, **windows/src/desktop** and
   **windows/src/developer**.

## Detailed Prerequisite Notes

### Visual Studio 2019 Community Edition setup requirements

In Visual Studio 2019, you need to have the following installed:
#### Workloads
* Desktop development with C++
* Universal Windows Platform development

#### Individual components
--* Windows Universal CRT SDK
* Windows 10.0.17763.0 SDK

Configure Visual Studio to use two-space tab stops:
1. Open the options dialog: Tools > Options.
2. Navigate to Text Editor > All Languages > Tabs.
3. Change 'Tab size' to 2 and 'Indent size' to 2.
4. Select 'Insert spaces'.

### Delphi setup requirements

Delphi Community Edition is free and can be downloaded
[here](https://www.embarcadero.com/products/delphi/starter/free-download).

Install Delphi using the following options:
* Windows 32 and 64 bit
* DUnit components
* No other 3rd party components required
* No Interbase components required

Delphi 10.3 is supported by default. Delphi 10.2 is no longer supported. (The
environment variable `DELPHI_VERSION=19.0` is still possible, but there are some
dependencies on 10.3 which we have not fixed. This variable can also be
added to your `UserDefines.mak`.)

## Environment Variables

To check whether these variables are set, run `SET NAME_OF_VAR` in command
prompt.

### KEYMAN_ROOT - Locating the source

If you pull the entire `keyman.git` repo to `c:\keyman`, then the paths by
default will work without changes. Otherwise, you will need to set an
environment variable `KEYMAN_ROOT` to the root path of the Keyman repo. For
example:

```
SET KEYMAN_ROOT=c:\projects\keyman
```

### GIT_BASH_FOR_KEYMAN

This environment variable is optional: the build will run bash in a separate
window in order to build KeymanWeb if it isn't present, but you'll lose logging
and have the annoyance of a window popping up halfway through the build. To
resolve both of those issues, set the environment variable to:

```
SET GIT_BASH_FOR_KEYMAN="C:\Program Files\Git\bin\bash.exe" --init-file "c:\Program Files\Git\etc\profile" -l
```

You should verify the install location of Git on your computer as it may vary.

### USERDEFINES - User Defines

You can specify defines that will not be added to the git repository and will be
used in the build in the UserDefines.mak file in the root folder. This is used
mostly for code signing certificates. If not specified, a test certificate will
be used to sign executables when you build a release.

To include UserDefines.mak in the build, use the command line parameter
`-DUSERDEFINES`. You can also set an environment variable `USERDEFINES=1` to get
the same result.

### KEYMAN_CEF4DELPHI_ROOT - Chromium Embedded Framework

Keyman and Keyman Developer use Chromium Embedded Framework. The source repo is
at https://github.com/keymanapp/CEF4Delphi. In order to build the installers, we
need to source the binary files from the
https://github.com/keymanapp/CEF4Delphi_binary repo. The
`KEYMAN_CEF4DELPHI_ROOT` environment variable should be set to the root of this
repo on your local machine.

The version of CEF in use is determined by CEF_VERSION.md. This maps to a branch
prefixed with `v` e.g. `v89.0.18` in the CEF4Delphi_binary repository. During a
release build, the buildtools/cef-checkout.sh script will checkout the correct
branch of the repository automatically and extract any compressed files found in
it.

### Crash Reporting

Crash report dialogs as shown by tsysinfo have a few special secret hotkeys
useful for development purposes:

1. Ctrl+C - copy details to clipboard
2. Ctrl+Shift+C - copy more details to clipboard
3. Alt+DblClk on icon - open sentry site to event
4. Ctrl+DblClk on icon - show more details in message box
