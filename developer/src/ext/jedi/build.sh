#!/usr/bin/env bash
## START STANDARD BUILD SCRIPT INCLUDE
# adjust relative paths as necessary
THIS_SCRIPT="$(readlink -f "${BASH_SOURCE[0]}")"
. "${THIS_SCRIPT%/*}/../../../../resources/build/build-utils.sh"
## END STANDARD BUILD SCRIPT INCLUDE

cd "$THIS_SCRIPT_PATH"

builder_describe "Build jedi components" clean configure build test
builder_parse "$@"

#-------------------------------------------------------------------------------------------------------------------

source "$KEYMAN_ROOT/resources/build/win/environment.inc.sh"

builder_describe_outputs \
  configure:project    /resources/build/win/delphi_environment_generated.inc.sh \
  build:project        /developer/lib/JvDockingDesign.bpl

#-------------------------------------------------------------------------------------------------------------------

function do_clean() {
  rm -rf obj manifest.res manifest.xml *.dproj.local version.res icons.RES icons.res *.identcache
}

function do_build() {
  do_install_paths
  do_build_jvcl_docking
  do_install_packages
}

do_install_paths() {
  "$DEVTOOLS" -ai "$DEVELOPER_ROOT/src/ext/jedi"

  "$DEVTOOLS" -ai "$DEVELOPER_ROOT/src/ext/jedi/jcl/jcl/source/common"
  "$DEVTOOLS" -ai "$DEVELOPER_ROOT/src/ext/jedi/jcl/jcl/source/include"
  "$DEVTOOLS" -ai "$DEVELOPER_ROOT/src/ext/jedi/jcl/jcl/source/vcl"
  "$DEVTOOLS" -ai "$DEVELOPER_ROOT/src/ext/jedi/jcl/jcl/source/windows"

  "$DEVTOOLS" -ai "$DEVELOPER_ROOT/src/ext/jedi/jvcl/jvcl/common"
  "$DEVTOOLS" -ai "$DEVELOPER_ROOT/src/ext/jedi/jvcl/jvcl/resources"
  "$DEVTOOLS" -ai "$DEVELOPER_ROOT/src/ext/jedi/jvcl/jvcl/run"

  "$DEVTOOLS" -ai "$DEVELOPER_ROOT/src/ext/jedi/packages"
}

do_build_jvcl_docking() {
  pushd packages
  mkdir -p obj/Win32/$TARGET_PATH
  delphi_msbuild Jcl.dproj "//p:Platform=Win32"
  delphi_msbuild JclVcl.dproj "//p:Platform=Win32"
  delphi_msbuild JvCore.dproj "//p:Platform=Win32"
  delphi_msbuild JvCoreDesign.dproj "//p:Platform=Win32"
  delphi_msbuild JvDocking.dproj "//p:Platform=Win32"
  delphi_msbuild JvDockingDesign.dproj "//p:Platform=Win32"
  popd
}

do_install_packages() {
  "$DEVTOOLS" -ip "$DEVELOPER_OUTLIB/JvCoreDesign.bpl"
  "$DEVTOOLS" -ip "$DEVELOPER_OUTLIB/JvDockingDesign.bpl"
}

builder_run_action clean:project        do_clean
builder_run_action configure:project    configure_windows_build_environment
builder_run_action build:project        do_build
# builder_run_action test:project         do_test
