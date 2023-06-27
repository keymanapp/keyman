#!/usr/bin/env bash

THIS_SCRIPT="$(readlink -f "${BASH_SOURCE[0]}")"

TARGET=KMSample1
source "${THIS_SCRIPT%/*}/../build_common.sh"
