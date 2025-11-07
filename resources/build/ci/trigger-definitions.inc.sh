# shellcheck shell=bash
# Keyman is copyright (C) SIL Global. MIT License.
#
# This file maps specific paths to build triggers
#
# Maps to ci/cancel-builds/trigger-definitions.mjs and must be kept in sync
#

#
# # Build levels
#
# See /docs/build-bot.md
#

readonly build_level_skip=skip
readonly build_level_build=build
readonly build_level_release=release
# TODO: future build_level_fulltest=fulltest --> do all expensive e2e tests as well as producing release artifacts
readonly valid_build_levels="$build_level_skip|$build_level_build|$build_level_release"

#
# Target platforms
#

available_platforms=(android common_web common_windows common_mac common_linux ios linux mac web windows developer)

# For test builds on master, beta, and stable-x.y branches, we always "build",
# not "release" -- that is, we don't produce artifacts
declare -Ag main_branch_platform_build_levels
for available_platforms_i in "${available_platforms[@]}"; do
  main_branch_platform_build_levels[$available_platforms_i]=$build_level_build
done
readonly main_branch_platform_build_levels
readonly available_platforms


available_platforms_regex=`echo "${available_platforms[@]}" | sed 's/ /|/g'`

# We also allow 'common' and 'core' platforms for Build-bot: commands
readonly available_platforms_regex="$available_platforms_regex|common|core"

# the base folder for each pattern does not need to be included, nor oem
# folders, nor resources/teamcity folders
#
# e.g. watch_android='web|comon/web' will expand to cover:
#   android
#   oem/*/android
#   web
#   common/web
#   resources/teamcity/android
#   resources/teamcity/includes


watch_android='web|common/web'
watch_ios='web|common/web'
watch_linux='core|common/test/keyboards/baseline'
watch_mac='core'
watch_web='common/web|core'
watch_windows='common|core'
watch_developer='common|core|web'

# Note, currently common_web build is a no-op because we rely on running the web
# tests on all three of our build platforms as seen below
watch_common_web='common/web'
# Currently, we run web tests on all three of our build platforms, to ensure
# that they work in all our environments. So we need to include common/web in
# the matches for these, as well as resources/teamcity/common
watch_common_windows='common/windows|common/web|resources/teamcity/common'
watch_common_mac='common/mac|common/web|resources/teamcity/common'
watch_common_linux='common/linux|common/web|resources/teamcity/common'

#
# Available build configurations and VCS identifiers; identifiers are somewhat inconsistent due
# to legacy updates and changes.
#
# These bc_x_y variables ARE used in trigger-builds.inc.sh by pattern so the names are important,
# and you won't find them directly in a grep search.
#
# _GitHub should be appended to any build configuration name that is from GitHub, not TeamCity.

# Test Build Configurations

# One day we may have tests that run on all platform changes
bc_test_all=()

bc_test_android=(KeymanAndroid_TestPullRequests KeymanAndroid_TestSamplesAndTestProjects)
bc_test_ios=(Keyman_iOS_TestPullRequests Keyman_iOS_TestSamplesAndTestProjects)
bc_test_linux=(KeymanLinux_TestPullRequests Keyman_Linux_Test_Integration Keyman_Common_KPAPI_TestPullRequests_Linux deb-pr-packaging_GitHub)
bc_test_mac=(Keyman_KeymanMac_PullRequests Keyman_Common_KPAPI_TestPullRequests_macOS)
bc_test_windows=(KeymanDesktop_TestPullRequests KeymanDesktop_TestPrRenderOnScreenKeyboards Keyman_Common_KPAPI_TestPullRequests_Windows)
bc_test_web=(Keymanweb_TestPullRequests Keyman_Common_KPAPI_TestPullRequests_WASM)
bc_test_developer=(Keyman_Developer_Test Keyman_Test_Developer_Mac Keyman_Test_Developer_Linux npm-publish_GitHub)

bc_test_common_web=(Keyman_Test_Common_Web)
bc_test_common_windows=(Keyman_Test_Common_Windows)
bc_test_common_mac=(Keyman_Test_Common_Mac)
bc_test_common_linux=(Keyman_Test_Common_Linux)

# These configuration arrays are triggered only by Build-bot commands:

bc_test_common=(Keyman_Test_Common_Web Keyman_Test_Common_Windows Keyman_Test_Common_Mac Keyman_Test_Common_Linux)
bc_test_core=(Keyman_Common_KPAPI_TestPullRequests_Linux Keyman_Common_KPAPI_TestPullRequests_macOS Keyman_Common_KPAPI_TestPullRequests_Windows)

# Core is tested directly for target platforms

# Keymanweb_TestPullRequestRegressions : currently this is timing out so disabled until we have
#                                        time to investigate further

vcs_test=HttpsGithubComKeymanappKeymanPRs

# Master (Alpha) Build Configurations; these may diverge when we need new build configurations for new versions

bc_master_android=(KeymanAndroid_Build)
bc_master_ios=(Keyman_iOS_Master)
bc_master_linux=(KeymanLinux_Master deb-release-packaging_GitHub build-test-publish-docker_GitHub)
bc_master_mac=(KeymanMac_Master)
bc_master_windows=(Keyman_Build)
bc_master_web=(Keymanweb_Build)
bc_master_developer=(Keyman_Developer_Release npm-publish_GitHub)

vcs_master=HttpsGithubComKeymanappKeyman

# Beta Build Configurations

bc_beta_android=(KeymanAndroid_Build)
bc_beta_ios=(Keyman_iOS_Master)
bc_beta_linux=(KeymanLinux_Master deb-release-packaging_GitHub)
bc_beta_mac=(KeymanMac_Master)
bc_beta_windows=(Keyman_Build)
bc_beta_web=(Keymanweb_Build)
bc_beta_developer=(Keyman_Developer_Release npm-publish_GitHub)

vcs_beta=HttpsGithubComKeymanappKeyman

# Stable 18.0 Build Configurations

bc_stable_18_0_android=(KeymanAndroid_Build)
bc_stable_18_0_ios=(Keyman_iOS_Master)
bc_stable_18_0_linux=(KeymanLinux_Master deb-release-packaging_GitHub)
bc_stable_18_0_mac=(KeymanMac_Master)
bc_stable_18_0_windows=(Keyman_Build)
bc_stable_18_0_web=(Keymanweb_Build)
bc_stable_18_0_developer=(Keyman_Developer_Release npm-publish_GitHub)

vcs_stable_18_0=HttpsGithubComKeymanappKeyman

# Stable 19.0 Build Configurations

bc_stable_19_0_android=(KeymanAndroid_Build)
bc_stable_19_0_ios=(Keyman_iOS_Master)
bc_stable_19_0_linux=(KeymanLinux_Master deb-release-packaging_GitHub)
bc_stable_19_0_mac=(KeymanMac_Master)
bc_stable_19_0_windows=(Keyman_Build)
bc_stable_19_0_web=(Keymanweb_Build)
bc_stable_19_0_developer=(Keyman_Developer_Release npm-publish_GitHub)

vcs_stable_19_0=HttpsGithubComKeymanappKeyman
