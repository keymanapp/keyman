#
# Keyman is copyright (C) SIL International. MIT License.
#
# Cross platform build script to build headers
#

project_ver = meson.project_version().split('.')

cfg = configuration_data()
cfg.set('majorver', project_ver[0])
cfg.set('minorver', project_ver[1])
cfg.set('patchver', project_ver[2])

configure_file(
  configuration: cfg,
  input: 'keyman_version.h.in',
  output: 'keyman_version.h',
)

inc = include_directories(
  '.',
  '../../common/include',
  '../../../../common/include'
)
