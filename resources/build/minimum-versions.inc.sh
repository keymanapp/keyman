# Required minimum versions (also used as default version)
# Minimum versions as of Keyman 18
# (https://docs.google.com/document/d/1Uy3U2YXeA4rCEbUbT7O6QzUGJDZBeViFecL8fjUOLkE/edit?usp=sharing)

# shellcheck shell=bash disable=SC2034 # SC2034: X appears unused.

# Target operating system and platform versions
KEYMAN_MIN_TARGET_VERSION_ANDROID=5       # Lollipop
KEYMAN_MIN_TARGET_VERSION_IOS=12.2        # iOS 12.2
KEYMAN_MIN_TARGET_VERSION_WINDOWS=10      # Windows 10
KEYMAN_MIN_TARGET_VERSION_MAC=10.13       # MacOS 10.13 (High Sierra)
KEYMAN_MIN_TARGET_VERSION_UBUNTU=20.04    # Ubuntu 20.04 Focal
KEYMAN_MIN_TARGET_VERSION_CHROME=95.0     # Final version that runs on Android 5.0

# Dependency versions
KEYMAN_MIN_VERSION_NODE_MAJOR=18
KEYMAN_MIN_VERSION_NPM=10.5.1             # 10.5.0 has bug, discussed in #10350
KEYMAN_MIN_VERSION_EMSCRIPTEN=3.1.44      # Warning: 3.1.45 is bad (#9529); newer versions work
KEYMAN_MAX_VERSION_EMSCRIPTEN=3.1.58      # See #9529
KEYMAN_MIN_VERSION_VISUAL_STUDIO=2019
KEYMAN_MIN_VERSION_MESON=1.0.0

# Language and runtime versions
KEYMAN_MIN_VERSION_JAVA=11                # We're using Java/OpenJDK 11
KEYMAN_MIN_VERSION_CPP=17                 # C++17
KEYMAN_MIN_VERSION_ANDROID_SDK=21

# Default version used in Docker containers
KEYMAN_DEFAULT_VERSION_CONTAINER=noble    # Ubuntu 24.04 Noble
