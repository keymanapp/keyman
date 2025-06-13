#!/usr/bin/env bash
# Keyman is copyright (C) SIL Global. MIT License.

download_symbol_server_index() {
  # Download symbol server index from symbol server
  builder_echo start "download symbol server index" "Downloading symbol server index"

  (
    # shellcheck disable=SC2164
    cd "${KEYMAN_ROOT}/.."
    # shellcheck disable=SC2154
    powershell -NonInteractive -ExecutionPolicy Bypass -File "${THIS_SCRIPT_PATH}/../includes/download-symbol-server-index.ps1"
  )

  builder_echo end "download symbol server index" success "Finished downloading symbol server index"
}

publish_new_symbols() {
  # Publish new symbols to symbol server
  builder_echo start "publish new symbols" "Publishing new symbols to symbol server"

  (
    cd "${KEYMAN_ROOT}/../symbols"
    # shellcheck disable=SC2154
    powershell -NonInteractive -ExecutionPolicy Bypass -File "${THIS_SCRIPT_PATH}/../includes/publish-new-symbols.ps1"
  )

  builder_echo end "publish new symbols" success "Finished publishing new symbols to symbol server"
}
