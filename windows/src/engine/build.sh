#!/usr/bin/env bash
## START STANDARD BUILD SCRIPT INCLUDE
# adjust relative paths as necessary
THIS_SCRIPT="$(readlink -f "${BASH_SOURCE[0]}")"
. "${THIS_SCRIPT%/*}/../../../resources/build/builder.inc.sh"
## END STANDARD BUILD SCRIPT INCLUDE

builder_describe \
  "Keyman Engine for Windows" \
  \
  clean \
  configure \
  build \
  test \
  "publish                 Prepare files for distribution, publish symbols, and build installer module" \
  "install                 Install built programs locally" \
  \
  ":insthelper             Installation helper module" \
  ":keyman                 Main host process (32 bit)" \
  ":keyman32               Keystroke processing engine (32 bit)" \
  ":keyman64               Keystroke processing engine (64 bit)" \
  ":keymanmc               Message library" \
  ":keymanx64              Host process (64 bit)" \
  ":kmcomapi               COM API library" \
  ":kmrefresh              Helper app to refresh Windows language settings" \
  ":kmtip                  Text Services Framework Text Input Processor" \
  ":mcompile               Mnemonic layout recompiler" \
  ":testhost               Test application for interacting with Keyman Engine" \
  ":tsysinfo               Diagnostic report tool (32 bit)" \
  ":tsysinfox64            Diagnostic report tool (64 bit)" \
  ":inst                   Windows Installer Module"

builder_parse "$@"

builder_run_child_actions  clean configure build test publish install
