#!/usr/bin/env bash
#
# This file maps specific paths to build triggers
#

available_platforms=(android ios linux mac web windows developer)

# the base folder for each pattern does not need to be included, nor oem folders
# e.g. android='common/models|common/predictive-text'
# will expand into android='^(android|(oem/[^/]+/android)|common/models|common/predictive-text)'

watch_android='web|common/models|common/predictive-text|common/web'
watch_ios='web|common/models|common/predictive-text|common/web'
watch_linux='core'
watch_mac='core'
watch_web='common/models|common/predictive-text|common/web|core'
watch_windows='common|core|web'
watch_developer='common|core|web'

#
# Available build configurations and VCS identifiers; identifiers are somewhat inconsistent due
# to legacy updates and changes.
#
# These bc_x_y variables ARE used in trigger-builds.inc.sh by pattern so the names are important,
# and you won't find them directly in a grep search.
#
# _Jenkins should be appended to any build configuration (pipeline) name that is from Jenkins,
# not TeamCity.
#

# Test Build Configurations

# One day we may have tests that run on all platform changes
bc_test_all=()

bc_test_android=(KeymanAndroid_TestPullRequests KeymanAndroid_TestSamplesAndTestProjects)
bc_test_ios=(Keyman_iOS_TestPullRequests Keyman_iOS_TestSamplesAndTestProjects)
bc_test_linux=(KeymanLinux_TestPullRequests Keyman_Common_KPAPI_TestPullRequests_Linux pipeline-keyman-packaging_Jenkins)
bc_test_mac=(Keyman_KeymanMac_PullRequests Keyman_Common_KPAPI_TestPullRequests_macOS)
bc_test_windows=(KeymanDesktop_TestPullRequests KeymanDesktop_TestPrRenderOnScreenKeyboards Keyman_Common_KPAPI_TestPullRequests_Windows)
bc_test_web=(Keymanweb_TestPullRequests Keyman_Common_LMLayer_TestPullRequests Keyman_Common_KPAPI_TestPullRequests_WASM)
bc_test_developer=(Keyman_Developer_Test)
# Keymanweb_TestPullRequestRegressions : currently this is timing out so disabled until we have
#                                        time to investigate further

vcs_test=HttpsGithubComKeymanappKeymanPRs

# Master (Alpha) Build Configurations; these diverge when we need new build configurations for new versions

bc_master_android=(KeymanAndroid_Build)
bc_master_ios=(Keyman_iOS_Master)
bc_master_linux=(KeymanLinux_Master pipeline-keyman-packaging_Jenkins)
bc_master_mac=(KeymanMac_Master)
bc_master_windows=(Keyman_Build)
bc_master_web=(Keymanweb_Build)
bc_master_developer=(Keyman_Developer_Release)

vcs_master=HttpsGithubComKeymanappKeyman

# Beta Build Configurations

bc_beta_android=(KeymanAndroid_Build)
bc_beta_ios=(Keyman_iOS_Master)
bc_beta_linux=(KeymanLinux_Master pipeline-keyman-packaging_Jenkins)
bc_beta_mac=(KeymanMac_Master)
bc_beta_windows=(Keyman_Build)
bc_beta_web=(Keymanweb_Build)
bc_beta_developer=(Keyman_Developer_Release)

vcs_beta=HttpsGithubComKeymanappKeyman

# Stable 14.0 Build Configurations

bc_stable_14_0_android=(KeymanAndroid_Build)
bc_stable_14_0_ios=(Keyman_iOS_Master)
bc_stable_14_0_linux=(KeymanLinux_Master pipeline-keyman-packaging_Jenkins)
bc_stable_14_0_mac=(KeymanMac_Master)
bc_stable_14_0_windows=(Keyman_Build)
bc_stable_14_0_web=(Keymanweb_Build)

# 16.0+ Keyman Developer is split from Windows, but TC configuration will not fail
# on older versions, just no-op
bc_stable_14_0_developer=(Keyman_Developer_Release)

vcs_stable_14_0=HttpsGithubComKeymanappKeyman

# Stable 15.0 Build Configurations

bc_stable_15_0_android=(KeymanAndroid_Build)
bc_stable_15_0_ios=(Keyman_iOS_Master)
bc_stable_15_0_linux=(KeymanLinux_Master pipeline-keyman-packaging_Jenkins)
bc_stable_15_0_mac=(KeymanMac_Master)
bc_stable_15_0_windows=(Keyman_Build)
bc_stable_15_0_web=(Keymanweb_Build)

vcs_stable_15_0=HttpsGithubComKeymanappKeyman

# Stable 16.0 Build Configurations

bc_stable_16_0_android=(KeymanAndroid_Build)
bc_stable_16_0_ios=(Keyman_iOS_Master)
bc_stable_16_0_linux=(KeymanLinux_Master pipeline-keyman-packaging_Jenkins)
bc_stable_16_0_mac=(KeymanMac_Master)
bc_stable_16_0_windows=(Keyman_Build)
bc_stable_16_0_web=(Keymanweb_Build)
bc_stable_16_0_developer=(Keyman_Developer_Release)

vcs_stable_16_0=HttpsGithubComKeymanappKeyman
