#!/usr/bin/env bash
set -e
echo "Starting Xvfb..."
Xvfb -screen 0 1024x768x24 :33 &> /dev/null &
sleep 1
echo "Starting Xephyr..."
DISPLAY=:33 Xephyr :32 -screen 1024x768 &> /dev/null &
sleep 1
echo "Starting metacity"
metacity --display=:32 &> /dev/null &
export DISPLAY=:32
"${@:-bash}"
