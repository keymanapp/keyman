#!/usr/bin/env bash

#
# Setup git hooks in this or all repositories.
#
# Run this script once to configure your system to work with our Git workflow.
#

if [[ -n "$WINDIR" ]]; then
  # https://stackoverflow.com/a/39160850/1836776
  SCRIPT_DIR=$(cmd //C cd)
else
  SCRIPT_DIR="$(dirname "$(readlink -f "${BASH_SOURCE[0]}")")"
fi

case $1 in
  --global)
    # Requires Git 2.9 or later.
    coreHooksPath=`git config --global core.hooksPath`
    if [[ -z "$coreHooksPath" ]]; then coreHooksPath="(not set)"; fi
    echo "Current Git core.hooksPath is: $coreHooksPath"

    git config --global core.hooksPath $SCRIPT_DIR/resources/git-hooks
    echo "Git is now configured to use $SCRIPT_DIR/resources/git-hooks for all repositories"

    ;;
  --local)
    if [[ -n "$WINDIR" ]]; then
      # https://stackoverflow.com/a/39160850/1836776
      WinPWD=$(cmd //C cd)
      cmd //C "mklink $WinPWD\\.git\\hooks\\commit-msg $WinPWD\\resources\\git-hooks\\commit-msg"
      cmd //C "mklink $WinPWD\\.git\\hooks\\prepare-commit-msg $WinPWD\\resources\\git-hooks\\prepare-commit-msg"
      cmd //C "mklink $WinPWD\\.git\\hooks\\pre-commit $WinPWD\\resources\\git-hooks\\pre-commit"
    else
      HOOKSDIR=$(git rev-parse --git-path hooks)
      ln -sf "$SCRIPT_DIR/resources/git-hooks/commit-msg" "$HOOKSDIR/commit-msg"
      ln -sf "$SCRIPT_DIR/resources/git-hooks/prepare-commit-msg" "$HOOKSDIR/prepare-commit-msg"
      ln -sf "$SCRIPT_DIR/resources/git-hooks/pre-commit" "$HOOKSDIR/pre-commit"
    fi
    ;;
  *)
    echo ""
    echo "Usage: configure-repo.sh --global|--local"
    echo ""
    echo "  --local    sets up sym links to resources/git-hooks/ for this repo"
    echo "             On Windows, this requires an elevated shell (Run as Administrator)"
    echo "  --global   configures Git 2.9+ to check resources/git-hooks for all repos on your system (Beware!)"
    echo ""
    echo "The scripts check that their upstream or origin repos are in fact from keymanapp,"
    echo "and explicitly exclude keyboards and lexical-models at this time."
    echo ""
esac
