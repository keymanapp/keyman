#!/usr/bin/env bash
#
# Setup 7z environment variable according to the user's system
#
# Windows: $SEVENZ_HOME/7z.exe
# Linux/macOS: 7z
#

if [[ -z "${SEVENZ+x}" ]]; then
  case "${OSTYPE}" in
    "cygwin")
      SEVENZ=${SEVENZ_HOME}/7z.exe
      ;;
    "msys")
      SEVENZ=${SEVENZ_HOME}/7z.exe
      ;;
    *)
      SEVENZ=7z
      ;;
  esac

  readonly SEVENZ

fi
