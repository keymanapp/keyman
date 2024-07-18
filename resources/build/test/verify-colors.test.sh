#!/usr/bin/env bash

set -eu

## START STANDARD BUILD SCRIPT INCLUDE
# adjust relative paths as necessary
THIS_SCRIPT="$(readlink -f "${BASH_SOURCE[0]}")"
. "${THIS_SCRIPT%/*}/../../../resources/build/build-utils.sh"
# END STANDARD BUILD SCRIPT INCLUDE

builder_use_color true
builder_echo "${COLOR_RED}COLOR_RED${COLOR_RESET}"
builder_echo "${COLOR_GREEN}COLOR_GREEN${COLOR_RESET}"
builder_echo "${COLOR_YELLOW}COLOR_YELLOW${COLOR_RESET}"
builder_echo "${COLOR_BLUE}COLOR_BLUE${COLOR_RESET}"
builder_echo "${COLOR_PURPLE}COLOR_PURPLE${COLOR_RESET}"
builder_echo "${COLOR_TEAL}COLOR_TEAL${COLOR_RESET}"
builder_echo "${COLOR_WHITE}COLOR_WHITE${COLOR_RESET}"
builder_echo "${COLOR_BRIGHT_WHITE}COLOR_BRIGHT_WHITE${COLOR_RESET}"
builder_echo "${COLOR_GREY}COLOR_GREY${COLOR_RESET}"
builder_echo "${COLOR_RESET}COLOR_RESET${COLOR_RESET}"
builder_echo "${BUILDER_BOLD}BUILDER_BOLD${COLOR_RESET}"
builder_echo "${HEADING_SETMARK}HEADING_SETMARK${COLOR_RESET}"
