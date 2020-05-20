#!/bin/bash
#
# This file maps specific paths to build triggers
#

available_platforms=(android ios linux mac web windows)

# the base folder for each pattern does not need to be included, nor oem folders
# e.g. android='common/predictive-text|common/lexical-model-types'
# will expand into android='^(android|(oem/[^/]+/android)|common/predictive-text|common/lexical-model-types)'

watch_android='web|common/predictive-text|common/lexical-model-types'
watch_ios='web|common/predictive-text|common/lexical-model-types'
watch_linux='common/engine'
watch_mac='common/engine'
watch_web='common/predictive-text|common/lexical-model-types'

# Windows currently builds Developer and Desktop, so we need everything from common,developer,web
watch_windows='common|developer|web'

#
# Available build configurations and VCS identifiers; identifiers are somewhat inconsistent due
# to legacy updates and changes.
#

# Test Build Configurations

# One day we may have tests that run on all platform changes
bc_test_all=()

bc_test_android=(KeymanAndroid_TestPullRequests KeymanAndroid_TestSamplesAndTestProjects)
bc_test_ios=(Keyman_iOS_TestPullRequests Keyman_iOS_TestSamplesAndTestProjects)
bc_test_linux=(KeymanLinux_TestPullRequests Keyman_Common_KPAPI_TestPullRequests_Linux pipeline-keyman-packaging_Jenkins)
bc_test_mac=(Keyman_KeymanMac_PullRequests Keyman_Common_KPAPI_TestPullRequests_macOS)
bc_test_windows=(KeymanDesktop_TestPullRequests KeymanDesktop_TestPrRenderOnScreenKeyboards Keyman_Common_KPAPI_TestPullRequests_Windows)
bc_test_web=(Keymanweb_TestPullRequests Keyman_Common_LMLayer_TestPullRequests)
# Keymanweb_TestPullRequestRegressions : currently this is timing out so disabled until we have
#                                        time to investigate further

vcs_test=HttpsGithubComKeymanappKeymanPRs

# Master (Alpha) Build Configurations

bc_master_android=(KeymanAndroid_Build)
bc_master_ios=(Keyman_iOS_Master)
bc_master_linux=(KeymanLinux_Master pipeline-keyman-packaging_Jenkins)
bc_master_mac=(KeymanMac_Master)
bc_master_windows=(Keyman_Build)
bc_master_web=(Keymanweb_Build)

vcs_master=HttpsGithubComKeymanappKeyman

# Beta Build Configurations

bc_beta_android=(KeymanAndroid_Beta)
bc_beta_ios=(Keyman_iOS_Beta)
bc_beta_linux=(KeymanLinux_Beta pipeline-keyman-packaging_Jenkins)
bc_beta_mac=(KeymanMac_Beta)
bc_beta_windows=(KeymanDesktop_Beta)
bc_beta_web=(Keymanweb_Beta)

vcs_beta=Keyman_KeymanappKeymanBeta

# TODO: Stable Build Configurations
