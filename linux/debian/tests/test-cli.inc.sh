# shellcheck shell=bash

verify_ibus_restarted() {
  local ibus_count new_pid

  ibus_count=$(pgrep --count ibus-daemon)
  if ! (( ibus_count == 1 )); then
    echo "FAIL ${TEST_SCRIPT}/${FUNCNAME[1]}: ibus-daemon not running"
    exit 1
  fi
  new_pid=$(pgrep ibus-daemon)
  if [[ "${new_pid}" == "${ibus_pid}" ]]; then
    echo "FAIL ${TEST_SCRIPT}/${FUNCNAME[1]}: ibus-daemon was not restarted"
    exit 1
  fi
  ibus_pid="${new_pid}"
}

verify_gnome_input_sources() {
  local expected_sources="$1"
  local sources

  sources=$(gsettings get org.gnome.desktop.input-sources sources)
  if [[ "${sources}" != "${expected_sources}" ]]; then
    echo "FAIL ${TEST_SCRIPT}/${FUNCNAME[1]}: input-sources wrong. Got ${sources} instead of ${expected_sources}"
    exit 1
  fi
}

verify_ibus_preload_engines() {
  local expected="$1"
  local sources

  sources=$(gsettings get org.freedesktop.ibus.general preload-engines)
  if [[ "${sources}" != "${expected}" ]]; then
    echo "FAIL ${TEST_SCRIPT}/${FUNCNAME[1]}: preload-engines wrong. Got ${sources} instead of ${expected}"
    exit 1
  fi
}

verify_custom_keyboards() {
  local expected_custom_keyboards="$1"
  local custom_keyboards

  custom_keyboards=$(gsettings get com.keyman.engine additional-keyboards)
  if [[ "${custom_keyboards}" != "${expected_custom_keyboards}" ]]; then
    echo "FAIL ${TEST_SCRIPT}/${FUNCNAME[1]}: custom keyboards wrong. Got ${custom_keyboards} instead of ${expected_custom_keyboards}"
    exit 1
  fi
}

verify_keyboard_file_exists() {
  local expected_keyboard_file="$1"

  if [[ ! -f "${expected_keyboard_file}" ]]; then
    echo "FAIL ${TEST_SCRIPT}/${FUNCNAME[1]}: keyboard file ${expected_keyboard_file} not found"
    exit 1
  fi
}

verify_keyboard_file_not_exists() {
  local expected_keyboard_file="$1"

  if [[ -f "${expected_keyboard_file}" ]]; then
    echo "FAIL ${FUNCNAME[1]}: keyboard file ${expected_keyboard_file} still exists"
    exit 1
  fi
}

verify_no_error() {
  local errocode="$1"

  if [[ "${errocode}" != "0" ]]; then
    echo "FAIL ${TEST_SCRIPT}/${FUNCNAME[1]}: non-zero exit code ${errocode}."
    exit 1
  fi
}

run_tests() {
  export SLEEP_TIME=5
  export TEST_SCRIPT
  TEST_SCRIPT="$(basename "${BASH_SOURCE[${#BASH_SOURCE[@]}-1]}")"

  gsettings reset com.keyman.engine additional-keyboards

  echo "Keyman Version: $(km-config --version)"
  if ! pgrep --uid "$(id -u)" ibus-daemon > /dev/null; then
    echo "Starting ibus"
    ibus start -d
    sleep "${SLEEP_TIME}"
  else
    echo "ibus-daemon already running"
  fi

  km_package_install__user
  km_package_install__shared
  km_package_list_installed
  km_package_uninstall__user
  km_package_uninstall__shared
}
