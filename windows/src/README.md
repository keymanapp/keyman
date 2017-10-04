# Keyman Desktop 10.0 and Keyman Developer 10.0

## Build Prerequisites

1. Install [VS2017 Community Edition](#visual-studio-2017-community-edition-setup-requirements).
2. Install [Delphi 10.2](#delphi-setup-requirements).
3. Install [git](https://git-scm.com/download/win).
4. Add the Keyman root folder to antivirus exclusions for performance and file lock reasons (optional - but highly recommended).
5. Start Delphi 10.2 IDE once after installation to create default environment files and ensure registration is complete.
6. Set environment variables per [notes below](#environment-variables): `KEYMAN_ROOT`, `DELPHI_STARTER`, `USERDEFINES`, (`USE_PLUSMEMO`, `KEYMAN_ENCUMBERED_ROOT`).
7. Add the **windows/lib** folder in the Keyman repository to your `PATH` environment variable (required for packages in Delphi).

### Release build prerequisites

For local development you do not need to perform a release build so these are optional.

1. Install [7-Zip](http://www.7-zip.org/) 64-bit (or 32-bit on x86 Windows). 7-Zip is used for archiving build files -- may be eliminated in future.
2. Install [HTML Help Workshop](https://www.microsoft.com/en-us/download/details.aspx?id=21138).
4. Install [WiX](https://wix.codeplex.com/releases/view/60102) to **C:\Program Files (x86)\Windows Installer XML v3.5**.

## Building Keyman Desktop and Keyman Developer

1. Start 'x64_x86 Cross Tools Command Prompt for VS 2017'.
2. Run `make build` from the **windows/src** folder.
3. Artifacts from a successful build will be placed in **windows/bin** folder.

Type `make` to see build targets. Common build targets are:

  `make build` <-- builds Keyman Desktop and Keyman Developer
  `make clean` <-- remove temporary files and build artifacts
  `make release` <-- makes a release of all Keyman Windows projects

By default the version generated will be 10.0.700.0.

### Release build

To perform a release build, you will need to obtain valid certificates. A release build is
unnecessary for local development.

1. Start 'x64_x86 Cross Tools Command Prompt for VS 2017'.
2. Run `make release` from the **windows/src** folder.
3. Artifacts from a successful build will be placed in **windows/release** folder.

Note: by default, the version generated will be 10.0.700.0. You will not be able to
install it over a later version of Keyman, and will need to uninstall and reinstall.

## Installing Keyman

These steps are only required the first time you install Keyman:
1. Install release versions of Keyman 10.0 and Keyman Developer 10.0.
  * Download the [latest official builds (alpha)](https://keyman.com/beta/) and run the installers.
2. Install the Keyman test certificates. Do the following for each KeymanTestCA cert in
**windows/src/buildtools/certificates**:
    1. Open the certificate and click 'Install certificate...' to open the Certificate Import Wizard.
    2. Select either 'Current User' or 'Local Machine' for 'Store Location' depending on your preference and
    click Next.
    3. Select 'Place all certificates in the following store' and 'Browse...' to select
    'Trusted Root Certification Authorities'. Click Next and finish the wizard.

To deploy a development build of Keyman and Keyman Developer,
1. Start 'x64_x86 Cross Tools Command Prompt for VS 2017'.
2. Run `make signcode` from the **windows/src** folder.
3. Ensure that Keyman and Keyman Developer are not running.
4. Run `make install` in **windows/src/engine**, **windows/src/desktop** and **windows/src/developer**.

## Detailed Prerequisite Notes

### Visual Studio 2017 Community Edition setup requirements

In Visual Studio 2017, you need to have the following components installed:
* Universal Windows Platform development
    * Desktop development with C++ > Windows 8.1 SDK and UCRT SDK
* Desktop development with C++

Configure Visual Studio to use two-space tab stops:
1. Open the options dialog: Tools > Options.
2. Navigate to Text Editor > All Languages > Tabs.
3. Change 'Tab size' to 2 and 'Indent size' to 2.
4. Select 'Insert spaces'.

### Delphi setup requirements

Delphi Starter Edition is free and can be downloaded [here](https://www.embarcadero.com/products/delphi/starter).
Some features of Keyman are [limited](#delphi_starter---building-with-delphi-starter-edition) when built using
the Starter Edition. You must set the `DELPHI_STARTER` environment variable when using the Starter Edition.

Install Delphi using the following options:
* Windows 32 and 64 bit (note: Delphi Starter only has 32 bit)
* No 3rd party components required
* No Interbase components required

## Environment Variables

To check whether these variables are set, run `SET NAME_OF_VAR` in command prompt.

### KEYMAN_ROOT - Locating the source

If you pull the entire `keyman.git` repo to `c:\keyman`, then the paths by default will
work without changes. Otherwise, you will need to set an environment variable
`KEYMAN_ROOT` to the root path of the Keyman repo. For example:

```
SET KEYMAN_ROOT=c:\projects\keyman
```

### DELPHI_STARTER - Building with Delphi Starter Edition

Keyman can be built from the command line with Delphi Starter Edition. You will need
to set the environment variable `DELPHI_STARTER` to enable a command line build with
Delphi Starter Edition.

However, there are three limitations:

1. As the command line compiler is not included in Delphi Starter Edition, the build 
   launches an instance of the IDE to run the build. This means that while you are doing
   a build, the Delphi IDE will be continually starting and stopping; it is also 
   significantly slower than the command line compiler.  After you complete an initial
   full build from the command line, you should be able to build individual projects 
   within the IDE.
   
2. Delphi Starter Edition does not include the x64 compiler, so tsysinfox64 will be 
   copied from a binary already in the repository. tsysinfox64 is the only 64-bit
   Delphi component.

3. The encumbered components cannot be built with Delphi Starter Edition.

### USERDEFINES - User Defines

You can specify defines that will not be added to the git repository and will be used in
the build in the UserDefines.mak file in the root folder. This is used mostly for 
code signing certificates. If not specified, a test certificate will be used to sign
executables when you build a release.

To include UserDefines.mak in the build, use the command line parameter `-DUSERDEFINES`. You
can also set an environment variable `USERDEFINES=1` to get the same result.

### KEYMAN_ENCUMBERED_ROOT and USE_PLUSMEMO - Encumbered components

Keyman Developer can be built with an encumbered text editor, `TPlusMemo`. The standard 
open source release uses a very basic `TMemo` without syntax highlighting and lots of 
other niceties. To build with the `TPlusMemo` version, use the `-DUSE_PLUSMEMO` define, and
make sure you have pulled the private `keyman-encumbered-components` repo to `<path>\src`. 
(That is, set the base folder of the repo to have the name `src`.)
You will also need to set the `KEYMAN_ENCUMBERED_ROOT` environment variable to `<path>`,
the repo's parent folder. If you will be working with this regularly, you should set an 
environment variable `USE_PLUSMEMO=USE_PLUSMEMO`, and then the command line build and the
IDE build will include the component correctly.

At some point, this editor will be replaced with an unencumbered one.

Keyman Desktop does not depend on this component.
