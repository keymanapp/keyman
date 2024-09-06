# List of all NPM packages that need to be published
#
# TODO: this should really be somewhere else, but right now Developer CI is
#       responsible for pulling the publish lever

readonly PACKAGES=(
  common/web/keyman-version
  common/web/types
  core/include/ldml
  developer/src/common/web/utils
  developer/src/kmc-analyze
  developer/src/kmc-keyboard-info
  developer/src/kmc-kmn
  developer/src/kmc-ldml
  developer/src/kmc-model
  developer/src/kmc-model-info
  developer/src/kmc-package
  developer/src/kmc
)
