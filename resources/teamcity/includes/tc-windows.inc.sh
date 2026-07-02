# shellcheck shell=bash
# Keyman is copyright (C) SIL Global. MIT License.
#
# Shared functions for any builds that run on Windows agents

ba_win_download_symbol_server_index() {
  if ! builder_is_windows; then
    builder_die "ba_win_download_symbol_server_index should only be run on Windows agents"
  fi

  builder_echo start "download symbol server index" "Downloading symbol server index"
  (
    # shellcheck disable=SC2154
    mkdir -p "${LOCAL_SYMBOLS_PATH}/000admin"
    # shellcheck disable=SC2164
    cd "${LOCAL_SYMBOLS_PATH}/000admin"

    # shellcheck disable=SC2154
    tc_rsync_download "${REMOTE_SYMBOLS_PATH}/000admin/lastid.txt" "."
    tc_rsync_download "${REMOTE_SYMBOLS_PATH}/000admin/history.txt" "."
    tc_rsync_download "${REMOTE_SYMBOLS_PATH}/000admin/server.txt" "."
  )
  builder_echo end "download symbol server index" success "Finished downloading symbol server index"
}

ba_win_publish_new_symbols() {
  if ! builder_is_windows; then
    builder_die "ba_win_publish_new_symbols should only be run on Windows agents"
  fi

  builder_echo start "publish new symbols" "Publishing new symbols to symbol server"
  (
    # shellcheck disable=SC2164
    cd "${LOCAL_SYMBOLS_PATH}"

    # ensure lower case 000admin (it may become capitalized from symsrv touching it)
    mv 000Admin 000admin

    tc_rsync_upload "." "${REMOTE_SYMBOLS_PATH}"
  )
  builder_echo end "publish new symbols" success "Finished publishing new symbols to symbol server"
}
