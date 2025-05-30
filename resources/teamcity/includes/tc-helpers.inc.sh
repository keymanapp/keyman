#!/usr/bin/env bash

# Returns 0 if we're running on Ubuntu.
is_ubuntu() {
  if [[ "${OSTYPE:-}" == "linux-gnu" ]]; then
    return 0
  else
    return 1
  fi
}

# Returns 0 if we're running on Windows, i.e. if the environment variable
# `OSTYPE` is set to "msys" or "cygwin".
is_windows() {
  if [[ "${OSTYPE:-}" == "msys" ]] || [[ "${OSTYPE:-}" == "cygwin" ]]; then
    return 0
  else
    return 1
  fi
}

# Returns 0 if we're running on macOS.
is_macos() {
  if [[ "${OSTYPE:-}" == "darwin" ]]; then
    return 0
  else
    return 1
  fi
}
