#!/usr/bin/env bash
set -e

# Start system dbus
sudo dbus-daemon --system --fork

# Start session dbus
# shellcheck disable=SC2046 # SC2046: quote this to prevent word-splitting
export $(dbus-launch)

# Start Wayland
weston --no-config --socket=wayland-0 --backend=headless  &
export WAYLAND_DISPLAY=wayland-0

# Start X11 (on Wayland)
Xwayland &
export DISPLAY=:0

"${@:-bash}"
