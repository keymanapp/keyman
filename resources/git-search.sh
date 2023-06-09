#!/usr/bin/env bash
# Adapted from https://stackoverflow.com/a/50569950/1836776

if [[ $1 == '--help' ]]; then
  script=`basename "$0"`
  echo "USAGE"
  echo "  $script term [git diff parameters]"
  echo
  echo "DESCRIPTION"
  echo "  Searches git diff results for <term>, returning only lines that have changed"
  echo
  echo "EXAMPLES"
  echo "  - Search all changes from master in current branch for 'TODO':"
  echo "    $script TODO master..."
  echo
  echo "  - As above, with pagination (color support):"
  echo "    $script TODO master... | less -r"
  exit 1
fi

colored=$'(\e\[[0-9;]*[a-zA-Z])'
marker="^$colored+diff"
pattern="^$colored+.*(\+|\-).*$1"
shift
# Yeah, okay, both sed and awk for maximum dork here, feel free to combine!
git diff --color $* | sed -rn -e "/$marker/! H; /$marker/ ba; $ ba; b; :a; x; /$pattern/ p" | awk "/\+\+\+/{f = \$2 \":\\n\"}; /$pattern/ {print f \$0; f = \"\"}"
