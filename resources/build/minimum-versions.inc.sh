# Required minimum versions (also used as default version)
# Minimum versions as of Keyman 18
# (https://docs.google.com/document/d/1Uy3U2YXeA4rCEbUbT7O6QzUGJDZBeViFecL8fjUOLkE/edit?usp=sharing)

# shellcheck shell=bash disable=SC2034 # SC2034: X appears unused.

MIN_NODE_MAJOR_VERSION=18
MIN_EMSCRIPTEN_VERSION=3.1.44    # 3.1.45 has problems, newer versions will work

MIN_VISUAL_STUDIO_VERSION=2019
MIN_MESON_VERSION=1.0.0
MIN_CHROME_VERSION=95.0

MIN_ANDROID_VERSION=5
MIN_ANDROID_SDK_VERSION=21
MIN_IOS_VERSION=12.2
MIN_WINDOWS_VERSION=10   # Windows 10
MIN_MACOS_VERSION=10.13  # MacOS 10.13 (High Sierra)
MIN_UBUNTU_VERSION=20.04 # Ubuntu 20.04 Focal

# We're using Java/OpenJDK 11
KEYMAN_JAVA_VERSION=11

DEFAULT_UBUNTU_VERSION=noble
