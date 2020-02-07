#!/bin/bash
#
# This file maps specific paths to build triggers
#

# the base folder for each pattern does not need to be included, nor oem folders
# e.g. android='common/predictive-text|common/lexical-model-types'
# will expand into android='^(android|(oem/[^/]+/android)|common/predictive-text|common/lexical-model-types)'

watch_android='android|common/predictive-text|common/lexical-model-types'
watch_ios='common/predictive-text|common/lexical-model-types'
watch_linux='common/engine'
watch_mac='common/engine'
watch_web='common/predictive-text|common/lexical-model-types'

# Windows currently builds Developer and Desktop, so we need everything from common,developer,web
watch_windows='common|developer|web'
