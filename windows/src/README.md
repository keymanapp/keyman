# Keyman for Windows and Keyman Developer

## Build Prerequisites

* See [Windows Build Environment Configuration](../../docs/build/windows.md).

## Building Keyman for Windows and Keyman Developer

1. Start 'Developer Command Prompt for VS 2019'.
2. Run `nmake build` from the **windows/src** folder.
3. Artifacts from a successful build will be placed in **windows/bin** folder.

*Note*: running `nmake build` will currently reset the packages and path settings
in your Delphi environment. If you use Delphi for other projects, you should
consider building Keyman under a login user dedicated to it, or in a VM.

Type `nmake` to see build targets. Common build targets are:

* `nmake build`
: builds Keyman for Windows and Keyman Developer

* `nmake clean`
: remove temporary files and build artifacts

* `nmake release`
: makes a release of all Keyman Windows projects

* `nmake install`
: install some or all components to Program Files (requires elevated command prompt).

### Building without Delphi

It is possible to build all components that do _not_ require Delphi by adding
the environment variable `NODELPHI=1` before starting the build. Currently many
components are Delphi-based, but if you are working just in Keyman Core, the
compiler, or Keyman Engine's C++ components, you may be able to get away without
building them. In this situation, we recommend copying the relevant Delphi-built
components into windows/bin folders from a compatible installed version of
Keyman for testing and debugging purposes.

## Release builds

A release build is not usually needed for local development. To run a release
build, you will need to obtain valid code signing certificates. See
Certificates, below. Official release builds for Keyman are built in the Keyman
project's CI environment.

1. Start bash
2. Run `./build.sh :publish` from the **windows/src** folder.
3. Artifacts from a successful build will be placed in **windows/release**
   folder.
4. **buildtools/help-keyman-com.sh** will push updated documentation to
   help.keyman.com. Environment variable `HELP_KEYMAN_COM` needs to be set to
   the root of the local help.keyman.com git repository.

Note: by default, the version number generated may vary from the current release
version. You will not be able to install it over a later version of Keyman, and
will need to uninstall and reinstall.

## Certificates

In order to create a release build, you will need a code signing certificate.
You can use your own certificate, or you can use test certificates which are not
globally trusted. The environment variables `SC_PFX_SHA1` and `SC_PFX_SHA256`
can be set to custom certificate paths.

The Keyman repo can build test certificates for you.  To build your own, run
`./common/windows/delphi/tools/certificates/build.sh certificates` to
build and install your own local root CA "**KeymanTestCA**" certificates. If you
specify a password for the certificate, you'll need to set that in the
environment variable `SC_PWD`.

### Manual certificate installation

You do not need to install the **KeymanTest** certificates, only the
**KeymanTestCA** certificates.

If you have not already installed the **KeymanTestCA** certificates using the
`nmake` command above, to manually install the Keyman **KeymanTestCA**
certificates, do the following in **common/windows/delphi/tools/certificates**:

   1. Open the certificate and click 'Install certificate...' to open the
      Certificate Import Wizard.
   2. Select either 'Current User' or 'Local Machine' for 'Store Location'
      depending on your preference and click Next.
   3. Select 'Place all certificates in the following store' and 'Browse...' to
      select 'Trusted Root Certification Authorities'. Click Next and finish
      the wizard.

## Crash Reporting

Crash report dialogs as shown by tsysinfo have a few special secret hotkeys
useful for development purposes:

1. Ctrl+C - copy details to clipboard
2. Ctrl+Shift+C - copy more details to clipboard
3. Alt+DblClk on icon - open sentry site to event
4. Ctrl+DblClk on icon - show more details in message box
