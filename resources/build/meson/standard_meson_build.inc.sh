#
# Keyman is copyright (C) SIL Global. MIT License.
#
# Created by mcdurdin on 2025-04-30
#
# This import script helps us setup a consistent meson environment across all
# our build scripts. It will generate two files in the calling scripts
# resources/ folder (which should be git-ignored):
#
#   * resources/meson.build - a copy of standard.meson.build from this folder
#   * resources/VERSION_WITH_TAG.md - the value of the env var $VERSION_WITH_TAG
#
# We do this because meson doesn't allow us to:
#
#   * reference a file outside the project root, or
#   * read environment variables
#
# Sample of normal usage:
#
#     if builder_has_action configure; then
#       # Import our standard compiler defines
#       source "$KEYMAN_ROOT/resources/build/meson/standard_meson_build.inc.sh"
#       standard_meson_build
#     fi
#
# ${THIS_SCRIPT_PATH}/meson.build then includes `resources` as a subdir:
#
#     subdir('resources')
#

standard_meson_build() {
  mkdir -p "$THIS_SCRIPT_PATH/resources"
  cp "$KEYMAN_ROOT/resources/build/meson/standard.meson.build" "$THIS_SCRIPT_PATH/resources/meson.build"
  echo "$VERSION_WITH_TAG" > "$THIS_SCRIPT_PATH/resources/VERSION_WITH_TAG.md"
}
