#!/usr/bin/env bash

echo "Testing missing builder_describe"
./builder-invalid-script-1.test.sh
RES=$?
if [[ $RES -ne 1 ]]; then
  echo "ERROR: expected builder-invalid-script-1.test.sh to exit with code 1 but instead got $RES"
  exit 1
fi

echo "Testing missing builder_parse"
./builder-invalid-script-2.test.sh
RES=$?
if [[ $RES -ne 1 ]]; then
  echo "ERROR: expected builder-invalid-script-2.test.sh to exit with code 1 but instead got $RES"
  exit 2
fi

echo "Testing base case"
./builder-invalid-script-3.test.sh
RES=$?
if [[ $RES -ne 0 ]]; then
  echo "ERROR: expected builder-invalid-script-3.test.sh to exit with code 0 but instead got $RES"
  exit 3
fi

exit 0
