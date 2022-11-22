# Copyright:    © 2018 SIL International.
# Description:  Cross platform build script to generate installable API header
#               files with versioning information templated in.
# Create Date:  2 Oct 2018
# Authors:      Tim Eves (TSE)
# History:     6 Oct 2018 - TSE - Move into keyman folder.
#

ver = lib_version.split('.')
project_ver = meson.project_version().split('.')

cfg = configuration_data()
cfg.set('lib_curr', ver[0])
cfg.set('lib_age', ver[1])
cfg.set('lib_rev', ver[2])
cfg.set('majorver', project_ver[0])
cfg.set('minorver', project_ver[1])
cfg.set('patchver', project_ver[2])

configure_file(
  configuration: cfg,
  input: 'keyboardprocessor_version.h.in',
  output: 'keyboardprocessor_version.h',
)

install_headers('keyboardprocessor.h',
                'keyboardprocessor_bits.h',
                'keyboardprocessor_consts.h',
                'keyboardprocessor_debug.h',
                'keyboardprocessor_vkeys.h',
                join_paths(meson.current_build_dir(), 'keyboardprocessor_version.h'),
                '../../../common/include/km_types.h',
                '../../../common/include/kmx_file.h',
                subdir: 'keyman')
