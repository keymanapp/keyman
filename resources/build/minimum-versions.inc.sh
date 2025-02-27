# Required minimum versions as of Keyman 19.0
#
# This is a list of minimum, maximum, and specific versions of any external
# components or dependencies found in Keyman.
#
# https://docs.google.com/document/d/1Uy3U2YXeA4rCEbUbT7O6QzUGJDZBeViFecL8fjUOLkE/edit?usp=sharing

# NOTE: After changing any values, you should run publish-minimum-versions.sh.

# shellcheck shell=bash disable=SC2034 # SC2034: X appears unused.

# Target operating system and platform versions
KEYMAN_MIN_TARGET_VERSION_ANDROID=5.0         # Lollipop
KEYMAN_MIN_TARGET_VERSION_IOS=12.2            # iOS 12.2
KEYMAN_MIN_TARGET_VERSION_WINDOWS=10          # Windows 10
KEYMAN_MIN_TARGET_VERSION_MAC=10.13           # MacOS 10.13 (High Sierra)
KEYMAN_MIN_TARGET_VERSION_UBUNTU=22.04        # Ubuntu 22.04 Jammy
KEYMAN_MIN_TARGET_VERSION_ANDROID_CHROME=53.0 # min version of Chrome for Keyman for Android

# Target web browsers for KeymanWeb -- we do not have polyfills for
# earlier versions, YMMV
KEYMAN_MIN_TARGET_VERSION_WEB_CHROME=95.0     # Note: 95.0 is final version that runs on Android 5.0
KEYMAN_MIN_TARGET_VERSION_WEB_FIREFOX=79.0    # TBD
KEYMAN_MIN_TARGET_VERSION_WEB_OPERA=47.0      # TBD
KEYMAN_MIN_TARGET_VERSION_WEB_SAFARI=13.0     # iOS 13.0, macOS 10.13.6+

# Dependency versions
KEYMAN_MIN_VERSION_NODE_MAJOR=20              # node version source of truth is /package.json:/engines/node; use KEYMAN_USE_NVM to automatically update
KEYMAN_MIN_VERSION_NPM=10.5.1                 # 10.5.0 has bug, discussed in #10350
KEYMAN_MIN_VERSION_EMSCRIPTEN=3.1.64          # Use KEYMAN_USE_EMSDK to automatically update to this version
KEYMAN_MIN_VERSION_VISUAL_STUDIO=2019
KEYMAN_MIN_VERSION_MESON=1.0.0

KEYMAN_VERSION_GRADLE=7.6.4                   # See /android/KMEA/gradle/wrapper/gradle-wrapper.properties
KEYMAN_VERSION_ICU=73.1                       # See /core/subprojects/icu-minimal.wrap

# Language and runtime versions
KEYMAN_VERSION_JAVA=11                        # We're using Java/OpenJDK 11
KEYMAN_MIN_VERSION_CPP=17                     # C++17
KEYMAN_MIN_VERSION_ANDROID_SDK=21

# Default version used in Docker containers
KEYMAN_DEFAULT_VERSION_UBUNTU_CONTAINER=noble # Ubuntu 24.04 Noble

# Data versions -- see resources/standards-data/readme.md
KEYMAN_VERSION_CLDR=45                        # LDML Keyboards version
KEYMAN_VERSION_ISO639_3=2024-05-22            # Date of last import
KEYMAN_VERSION_LANGTAGS=2024-05-22            # Date of last import
KEYMAN_VERSION_LANGTAGS_SUBTAG_REGISTRY=2024-05-16  # Date of last import
KEYMAN_VERSION_UNICODE=16.0.0                 # UCD + related data
