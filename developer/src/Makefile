#
# Keyman dev Makefile
#

!ifdef NODELPHI
DELPHI_TARGETS=
!else
# note: inst is not really a delphi target but depends on delphi
DELPHI_TARGETS=kmconvert tike setup inst
!endif

TARGETS=build-tools kmcmplib kmanalyze $(DELPHI_TARGETS) kmdecomp samples server kmc api

EXCLUDEPATHDEFINES=1
MANIFESTS=tike setup
CLEANS=clean-developer

EXCLUDEPATHDEFINES=1
BUILDPREREQ=global-versions

!include Header.mak

# ----------------------------------------------------------------------

common: .virtual
    cd $(KEYMAN_ROOT)\common\windows\delphi\components
    $(MAKE) $(TARGET)
    cd $(DEVELOPER_ROOT)\src\common\delphi\components
    $(MAKE) $(TARGET)

imsample: .virtual
    cd $(DEVELOPER_ROOT)\src\imsample
    $(MAKE) $(TARGET)

kmcmplib: global-versions .virtual
    cd $(DEVELOPER_ROOT)\src\kmcmplib
    $(MAKE) $(TARGET)

kmdecomp: .virtual
    cd $(DEVELOPER_ROOT)\src\kmdecomp
    $(MAKE) $(TARGET)

kmanalyze: .virtual
    cd $(DEVELOPER_ROOT)\src\kmanalyze
    $(MAKE) $(TARGET)

kmconvert: .virtual
    cd $(DEVELOPER_ROOT)\src\kmconvert
    $(MAKE) $(TARGET)

tike: redist
    cd $(DEVELOPER_ROOT)\src\tike
    $(MAKE) $(TARGET)

samples: .virtual
    cd $(DEVELOPER_ROOT)\src\samples
    $(MAKE) $(TARGET)

setup: .virtual
    cd $(DEVELOPER_ROOT)\src\setup
    $(MAKE) $(TARGET)

server: .virtual
    cd $(DEVELOPER_ROOT)\src\server
    $(MAKE) $(TARGET)

kmc: kmc-analyze kmc-ldml kmc-keyboard-info kmc-kmn kmc-model kmc-model-info kmc-package
    cd $(DEVELOPER_ROOT)\src\kmc
    $(MAKE) $(TARGET)

kmc-analyze: .virtual
    cd $(DEVELOPER_ROOT)\src\kmc-analyze
    $(MAKE) $(TARGET)

kmc-keyboard-info: .virtual
    cd $(DEVELOPER_ROOT)\src\kmc-keyboard-info
    $(MAKE) $(TARGET)

kmc-kmn: .virtual
    cd $(DEVELOPER_ROOT)\src\kmc-kmn
    $(MAKE) $(TARGET)

kmc-ldml: .virtual
    cd $(DEVELOPER_ROOT)\src\kmc-ldml
    $(MAKE) $(TARGET)

kmc-model: .virtual
    cd $(DEVELOPER_ROOT)\src\kmc-model
    $(MAKE) $(TARGET)

kmc-model-info: .virtual
    cd $(DEVELOPER_ROOT)\src\kmc-model-info
    $(MAKE) $(TARGET)

kmc-package: .virtual
    cd $(DEVELOPER_ROOT)\src\kmc-package
    $(MAKE) $(TARGET)

api: .virtual
    cd $(DEVELOPER_ROOT)
    if $(TARGET) == build $(GIT_BASH_FOR_KEYMAN) build.sh api --no-deps

# ----------------------------------------------------------------------

ext: .virtual
    cd $(DEVELOPER_ROOT)\src\ext
    $(MAKE) $(TARGET)

# ----------------------------------------------------------------------

test: .virtual
    cd $(DEVELOPER_ROOT)\src\test\auto
    $(MAKE) $(TARGET)
    cd $(DEVELOPER_ROOT)\src

# ----------------------------------------------------------------------

redist:
    copy $(KEYMAN_ROOT)\common\windows\delphi\ext\sentry\sentry.dll $(DEVELOPER_ROOT)\bin
    copy $(KEYMAN_ROOT)\common\windows\delphi\ext\sentry\sentry.x64.dll $(DEVELOPER_ROOT)\bin
    copy $(KEYMAN_ROOT)\common\windows\delphi\ext\sentry\crashpad_handler.exe $(DEVELOPER_ROOT)\bin

# ----------------------------------------------------------------------

inst: redist
    cd $(DEVELOPER_ROOT)\src\inst
    $(MAKE) $(TARGET)

# ----------------------------------------------------------------------
# ----------------------------------------------------------------------
# release
# ----------------------------------------------------------------------

release: clean-pathdefines test-release-cef-path test-releasebuildcheck test-releaseexists test-cert test-7zip test-wix test-klog clean clean-output
    cd $(DEVELOPER_ROOT)\src
    echo Continuing Release
    $(MAKE) do-build-release
    echo After build-release
    cd $(DEVELOPER_ROOT)\src

# Build a release

do-build-release:
    echo Starting Release Build
    cd $(COMMON_ROOT)
    $(MAKE) global-versions
    cd $(DEVELOPER_ROOT)\src
    $(MAKE) dirs
    cd $(WINDOWS_ROOT)\src
    $(MAKE) build-tools
    cd $(DEVELOPER_ROOT)\src
    $(MAKE) build-release-dependencies
    $(MAKE) "SIGNCODE_BUILD=SIGNCODE_BUILD" build test
    $(MAKE) signcode
    cd $(DEVELOPER_ROOT)\src\tools\verify_signatures
    $(MAKE) test

    cd $(DEVELOPER_ROOT)\src\inst
    $(MAKE)
    cd $(DEVELOPER_ROOT)\src

#
# Dependencies on Windows are now only ext,global,buildtools and kmbrowserhost
#
build-release-dependencies:
    cd $(WINDOWS_ROOT)\src\ext
    $(MAKE) build

    cd $(WINDOWS_ROOT)\src\global
    $(MAKE) build

    cd $(WINDOWS_ROOT)\src\buildtools
    $(MAKE) build

# TODO: kmbrowserhost should be in global
    cd $(WINDOWS_ROOT)\src\desktop\kmbrowserhost
    $(MAKE) build signcode

    cd $(DEVELOPER_ROOT)\src


clean-output:
    rd /s/q $(DEVELOPER_ROOT)\bin
    rd /s/q $(DEVELOPER_ROOT)\debug
    rd /s/q $(DEVELOPER_ROOT)\build
    rd /s/q $(DEVELOPER_ROOT)\lib

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
    copy $(SC_PFX_SHA1) $(WINDOWS_ROOT)\src\pfx.tmp
    del $(WINDOWS_ROOT)\src\pfx.tmp

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
    cd $(DEVELOPER_ROOT)\src

#
# Test that we haven't already built this release
#

test-releaseexists:
    cd $(DEVELOPER_ROOT)\src\inst
    $(MAKE) test-releaseexists
    cd $(DEVELOPER_ROOT)\src

#
# Test various release prerequisites, e.g. Debug reg key not set
#

test-releasebuildcheck: global-versions
    cd $(COMMON_ROOT)\tools\devtools
    $(MAKE) test-releasebuildcheck
    cd $(DEVELOPER_ROOT)\src

#
# Tools needed to complete a build
#

build-tools:
    $(MAKE) global-versions
    $(MAKE) common
!ifdef NODELPHI
    echo Skipping build-tools
!else
    $(MAKE) build-tools-internal
!endif

global-versions:
    cd $(COMMON_ROOT)
    @if not exist $(KEYMAN_ROOT)\include\keymanversion_build.h $(MAKE) global-versions
    cd $(DEVELOPER_ROOT)\src

build-tools-internal:
# We'll build a default PathDefines.mak if it is missing here so we can
# go on and do the rest of the build. Build all the ext/global/build-tools
# things we need...
    $(MAKE) devtools-app
    $(DEVTOOLS) -ti
    cd $(WINDOWS_ROOT)\src
    $(MAKE) ext global buildtools
    cd $(DEVELOPER_ROOT)\src
    $(MAKE) ext

# ----------------------------------------------------------------------

clean-developer:
    cd $(DEVELOPER_ROOT)\src
    -del version.txt

!include Target.mak

# ----------------------------------------------------------------------
# EOF
# ----------------------------------------------------------------------
