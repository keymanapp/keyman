#!/bin/bash

#
# Setup git hooks in this or all repositories.
#
# Run this script once to configure your system to work with our Git workflow.
#

case $1 in
  --global)
    # Requires Git 2.9 or later.
    coreHooksPath=`git config --global core.hooksPath`
    if [[ -z "$coreHooksPath" ]]; then coreHooksPath="(not set)"; fi
    echo "Current Git core.hooksPath is: $coreHooksPath"

    git config --global core.hooksPath $PWD/resources/git-hooks
    echo "Git is now configured to use $PWD/resources/git-hooks for all repositories"

    ;;
  --local)
    if [[ -n "$WINDIR" ]]; then
      # https://stackoverflow.com/a/39160850/1836776
      WinPWD=$(cmd //C cd)
      cmd //C "mklink $WinPWD\\.git\\hooks\\commit-msg $WinPWD\\resources\\git-hooks\\commit-msg"
    else
      ln -sf "$PWD/resources/git-hooks/commit-msg" "$PWD/.git/hooks/commit-msg"
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
