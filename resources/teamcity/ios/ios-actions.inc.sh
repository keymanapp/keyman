# shellcheck shell=bash
# Keyman is copyright (C) SIL Global. MIT License.

ios_build() {
  builder_echo start "build" "Building KeymanEngine + Keyman for iOS"
  builder_launch /ios/ci.sh build
  builder_echo end "build" success "Finished building KeymanEngine + Keyman for iOS"
}

ios_capture_build_artifacts() {
  builder_echo start "prep-release" "Using prep-release script to capture build artifacts"
  builder_launch /ios/tools/prepRelease.sh
  builder_echo end "prep-release" success "Finished capturing build artifacts"
}

ios_publish_symbols() {
  builder_echo start "publish to Sentry" "Publishing source map to Sentry"
  builder_launch /ios/build.sh "publish-symbols"
  builder_echo end "publish to Sentry" success "Finished publishing source map to Sentry"
}
