#
# Keyman root Makefile
#

# ----------------------------------------------------------------------

default:
    @echo The following make targets are available:
    @echo .   build                 builds everything except help files (BUILDHELP=1 to build help)
    @echo .   install               installs packages into Delphi IDE (run after a build)
    @echo .   clean                 removes all temp files
    @echo .   release               make a release
    @echo .   web-install           sends the documentation to the website
    @echo .   signcode              sign the build artifacts
    @echo.
    @echo .   Build flags:
    @echo .         DEBUG           Include debug information
    @echo .         SC_TIMESTAMP    Don't timestamp certificates (for internal releases only, offline)
    @echo .         BUILDHELP       Build the helpfiles (implicit when 'make release')
    @echo .         LINT            Turns on ^$MESSAGE hints and warnings and all extra string warnings
    @echo .         QUIET           Ignore hints and warnings
    @echo .         RELEASE_OEM     Build executables for third party products in oem folder (e.g. FirstVoices)
    @echo.
    @echo .   To view help on the build process, in /src/inst, run:
    @echo .         make help
    @echo.

# ----------------------------------------------------------------------

!ifdef QUICK_BUILD_KEYMAN
TARGETS=ext global buildtools engine desktop
MANIFESTS=engine desktop
!else
TARGETS=ext global buildtools engine desktop support
MANIFESTS=engine desktop
!endif

RELEASE_TARGETS=desktop
CLEANS=test unit-tests cleanroot

EXCLUDEPATHDEFINES=1
BUILDPREREQ=global-versions

!include Header.mak

# ----------------------------------------------------------------------

cleanroot:
    -del tds.zip
    -del tds.exe
    -del PathDefines.mak
    -del version.txt

global: build-tools
    cd $(ROOT)\src\global
    $(MAKE) $(TARGET)
    cd $(ROOT)\src

ext: build-tools
    cd $(ROOT)\src\ext
    $(MAKE) $(TARGET)
    cd $(ROOT)\src

buildtools: global
    cd $(ROOT)\src\buildtools
    $(MAKE) $(TARGET)
    cd $(ROOT)\src

engine: global message-constants
    cd $(ROOT)\src\engine
    $(MAKE) $(TARGET)
    cd $(ROOT)\src

desktop: global message-constants
    cd $(ROOT)\src\desktop
    $(MAKE) $(TARGET)
    cd $(ROOT)\src

support: global
    cd $(ROOT)\src\support
    $(MAKE) $(TARGET)
    cd $(ROOT)\src

test: .virtual
    cd $(ROOT)\src\test
    $(MAKE) $(TARGET)
    cd $(ROOT)\src

unit-tests:
    cd $(ROOT)\src\test\unit-tests
    $(MAKE) $(TARGET)
    cd $(ROOT)\src

# ----------------------------------------------------------------------
# release
# ----------------------------------------------------------------------

release: clean-pathdefines test-release-cef-path test-releasebuildcheck test-uiaccess test-klog test-releaseexists test-cert test-hhc test-7zip test-wix clean
    cd $(ROOT)\src
    $(MAKE) devtools-app
    $(MAKE) build-release-wrapper
#   $(MAKE) clean  <-- this makes it harder to do OEM builds
    cd $(ROOT)\src

clean-pathdefines: test-releasebuildcheck
# test-releasebuildcheck builds devtools for us
    $(DEVTOOLS) -ri

#
# Test that the CEF file path is accessible from env var KEYMAN_CEF4DELPHI_ROOT
# and checkout the appropriate version of CEF as determined by CEF_VERSION.md
#
test-release-cef-path:
!ifndef SC_TIMESTAMP
!ifdef GIT_BASH_FOR_KEYMAN
    $(GIT_BASH_FOR_KEYMAN) $(KEYMAN_ROOT)\common\windows\cef-checkout.sh
!else
    start /wait $(KEYMAN_ROOT)\common\windows\cef-checkout.sh
!endif
!endif

#
# Test that certificate checking is enabled and that certificate is accessible
#

test-cert:
    copy $(SC_PFX_SHA1) $(ROOT)\src\pfx.tmp
    del $(ROOT)\src\pfx.tmp

#
# Test that hhc is installed
#

test-hhc:
    if not exist $(HHC) exit 1

#
# Test that WiX is installed
#

test-wix:
    if not exist $(WIXPATH)\candle.exe exit 1

#
# Test that 7-zip is installed
#

test-7zip:
    if not exist $(WZZIPPATH) exit 1

#
# Test that klog is disabled
#

test-klog:
    cd $(COMMON_ROOT)\tools\test-klog
    $(MAKE) verify
    cd $(ROOT)\src

#
# Test that we haven't already built this release
#

test-releaseexists:
    cd $(ROOT)\src\desktop\inst
    $(MAKE) test-releaseexists
    cd $(ROOT)\src

#
# Test various release prerequisites, e.g. Debug reg key not set
#

test-releasebuildcheck: global-versions
    cd $(COMMON_ROOT)\tools\devtools
    $(MAKE) test-releasebuildcheck
    cd $(ROOT)\src

#
# Test uiaccess property in keyman manifest.xml and keyman64 manifest.xml
#

test-uiaccess:
    cd $(ROOT)\src\engine\keyman
    $(MAKE) test-uiaccess
    cd $(ROOT)\src\engine\keymanx64
    $(MAKE) test-uiaccess
    cd $(ROOT)\src

#
# Build Keyman projects for coverity; call this with
#   make test-coverity
# * Set environment variable COVBUILD to path to cov-build.exe
# * tar must be on path
#

!ifndef COVBUILD
test-coverity:
    echo Set variable COVBUILD to covbuild.exe
    exit 1
!else
test-coverity:
    cd $(ROOT)\src
    if exist cov-int rd /s/q cov-int
    if exist keyman-windows-coverity.tgz del keyman-windows-coverity.tgz
    $(COVBUILD) --dir cov-int $(MAKE) test-coverity-internal
    tar cvzf keyman-windows-coverity.tgz cov-int
    rd /s/q cov-int
    echo Manual upload keyman-windows-coverity.tgz to coverity
# TODO: automated upload as part of build

test-coverity-internal:
    cd $(ROOT)\src\engine\keyman32
    $(MAKE) build
    cd $(ROOT)\src\engine\keyman64
    $(MAKE) build
    cd $(ROOT)\src\engine\keymanx64
    $(MAKE) build
    cd $(ROOT)\src\engine\kmtip
    $(MAKE) build
    cd $(ROOT)\src\engine\mcompile
    $(MAKE) build
!endif

# Build a release

build-release-wrapper: clean-output global-versions
    cd $(ROOT)\src
    $(MAKE) dirs
    $(MAKE) build-tools
!ifdef QUICK_BUILD_KEYMAN
    $(MAKE) "SIGNCODE_BUILD=SIGNCODE_BUILD" build
    $(MAKE) "SIGNCODE_BUILD=SIGNCODE_BUILD" build-release
    $(MAKE) signcode
!else
    $(MAKE) "SIGNCODE_BUILD=SIGNCODE_BUILD" build unit-tests
    $(MAKE) "SIGNCODE_BUILD=SIGNCODE_BUILD" build-release
    $(MAKE) signcode
    $(MAKE) "TARGET=test" test
!endif

    cd $(ROOT)\src\engine\inst
    $(MAKE)

    cd $(ROOT)\src\desktop\inst
    $(MAKE) setup

!ifdef QUICK_BUILD_KEYMAN
!else
    cd $(ROOT)\src
    $(MAKE) build-release-wrapper-oem
!endif

build-release-wrapper-oem:
!ifdef RELEASE_OEM
    cd $(ROOT)\..\oem\firstvoices\windows\src\inst
    $(MAKE) setup
!else
    echo Not building OEM releases
!endif

#
# Tools needed to complete a build
#

build-tools:
!ifdef NODELPHI
    echo Skipping build-tools
!else
# we need to build devtools directly because otherwise we have a
# chicken-and-egg issue with PathDefines.mak
    $(MAKE) devtools-app
# We'll build a default PathDefines.mak if it is missing here so we can
# go on and do the rest of the build
    $(DEVTOOLS) -ti
!endif

global-versions:
    cd $(COMMON_ROOT)
    @if not exist $(KEYMAN_ROOT)\include\keymanversion_build.h $(MAKE) global-versions
    cd $(ROOT)\src

message-constants: build-tools
!ifdef NODELPHI
    echo Skipping message-constants
!else
!if "$(TARGET)" != "clean"
    $(MAKE) devtools-app
    $(DEVTOOLS) -buildmessageconstants $(ROOT)\src\desktop\kmshell\xml\strings.xml $(ROOT)\src\global\delphi\cust\MessageIdentifierConsts.pas
!endif
!endif

#
# Remove all files from output folders to ensure no legacy files left in a release
#

clean-output:
    rd /s/q $(ROOT)\bin
    rd /s/q $(ROOT)\debug
    rd /s/q $(ROOT)\build
    rd /s/q $(ROOT)\lib

# ----------------------------------------------------------------------
# install
# ----------------------------------------------------------------------

web-install:
    cd $(ROOT)\src\desktop\help
    $(MAKE) html install

# ----------------------------------------------------------------------

!include Target.mak

# ----------------------------------------------------------------------
# EOF
# ----------------------------------------------------------------------
