#!/bin/bash

#
# Define terminal colours
#

if [ -t 2 ]; then
  t_red=$'\e[1;31m'
  t_grn=$'\e[1;32m'
  t_yel=$'\e[1;33m'
  t_blu=$'\e[1;34m'
  t_mag=$'\e[1;35m'
  t_cyn=$'\e[1;36m'
  t_end=$'\e[0m'
fi

function compile {
  local keyboard=$1
  $KMCOMP_LAUNCHER "$KMCOMP" -s -nologo "$keyboard" || die "Could not compile keyboard"
}

function die {
  local rc=$?
  local msg=$1

  # We are dying, so if previous command didn't actually give
  # an error code, we still want to give an error. We'll give
  # an arbitrary exit code to indicate this
  if [ $rc == 0 ]; then
    rc=999
  fi
  
  (>&2 echo "${t_red}$msg${t_end}")
  (>&2 echo "${t_red}Aborting with error $rc${t_end}")
  exit $rc
}

if [ -z "$KMXTEST" ]; then
  KMXTEST=../x64/Debug/kmxtest.exe
  #KMXTEST=../Debug/kmxtest.exe
fi

if [ -z "$KMCOMP" ]; then
  KEYBOARDROOT="/c/Projects/Keyman/keyboards"
  KMCOMP="$KEYBOARDROOT/tools/kmcomp.exe"
fi

if [ -z "$KMCOMP_LAUNCHER" ]; then
  if [[ "${OSTYPE}" != "darwin"* ]]; then
    KMCOMP_LAUNCHER=
  else
    KMCOMP_LAUNCHER=wine
  fi
fi

function run_test {
  local INFILE=$1
  local SILENT=$2
  local OUTFILE=`basename "$INFILE" .kmn`.kmx
  local CONTEXT=`grep "c context: " "$INFILE" | cut -b 12-`
  local KEYS=`grep "c keys: " "$INFILE" | cut -b 9-`
  local OUTPUT=`grep "c expected: " "$INFILE" | cut -b 13-`

  test -f "$INFILE" || die "File $INFILE does not exist"
  
  echo "Testing $INFILE"
  
  compile "$INFILE"

# TODO: environment tests
# TODO: platform tests
# TODO: options
# TODO: input context
# TODO: deadkey in final output
# TODO: surrogate pairs

  test ! -z "$OUTPUT" || die "Missing 'expected' line in .kmn"
  test ! -z "$KEYS" || die "Missing 'keys' line in .kmn"
  test -z "$CONTEXT" && local CONTEXTPARAM=-context

  $KMXTEST $SILENT -kmx "$OUTFILE" $CONTEXTPARAM "$CONTEXT" -keys "$KEYS" -expected-output "$OUTPUT" || die "Failed testing $INFILE"
}

if [ -f "$1" ]; then
  run_test "$1"
else
  for keyboard in *.kmn ; do
    run_test "$keyboard" -s
  done
fi
