# Copyright:    © 2018 SIL International.
# Description:  Cross platform build script to generate installable API header
#               files with versioning information templated in.
# Create Date:  2 Oct 2018
# Authors:      Tim Eves (TSE)
# History:     6 Oct 2018 - TSE - Move into keyman folder.
#

ver = lib_version.split('.')

# TODO: consider if 0.0.0 is an appropriate or useful library version

cfg = configuration_data()
cfg.set('lib_curr', ver[0])
cfg.set('lib_age', ver[1])
cfg.set('lib_rev', ver[2])

configure_file(
  configuration: cfg,
  input: 'keyboardprocessor.h.in',
  output: 'keyboardprocessor.h',
)

project_ver = meson.project_version().split('.')

vercfg = configuration_data()
vercfg.set('majorver', project_ver[0])
vercfg.set('minorver', project_ver[1])
vercfg.set('patchver', project_ver[2])

configure_file(
  configuration: vercfg,
  input: 'keyboardprocessor_version.h.in',
  output: 'keyboardprocessor_version.h',
)

install_headers(join_paths(meson.current_build_dir(), 'keyboardprocessor.h'),
                'keyboardprocessor_vkeys.h',
                'keyboardprocessor_bits.h',
                subdir: 'keyman')
