#!/usr/bin/env bash
if [[ -z "${DOCKER_RUNNING:-}" ]]; then
  echo "This script is intended to be run inside a docker container."
  exit 0
fi

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
