#!/usr/bin/env bash

echo "**TODO** Update script to work with new history/versioning"
exit 1

FAIL=0
./validateHistory.sh android || FAIL=$(($FAIL + 1))
./validateHistory.sh desktop || FAIL=$(($FAIL + 2))
./validateHistory.sh developer || FAIL=$(($FAIL + 4))
./validateHistory.sh ios || FAIL=$(($FAIL + 8))
./validateHistory.sh linux || FAIL=$(($FAIL + 16))
./validateHistory.sh mac || FAIL=$(($FAIL + 32))
./validateHistory.sh web || FAIL=$(($FAIL + 64))
echo Final status = $FAIL
exit $FAIL
