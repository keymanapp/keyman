#!/usr/bin/env bash
PID_FILE=$1

echo "Shutting down processes..."
bash "$PID_FILE" > /dev/null 2>&1
rm "$PID_FILE"
echo "Finished shutdown of processes."
