ver = meson.project_version().split('.')

cfg = configuration_data()
cfg.set('lib_curr', ver[0])
cfg.set('lib_age', ver[1])
cfg.set('lib_rev', ver[2])

configure_file(
  configuration: cfg,
  input: 'keyboardprocessor.h.in',
  output: 'keyboardprocessor.h',
)
inc = include_directories('.')

install_headers(join_paths(meson.current_build_dir(), 'keyboardprocessor.h'),
                'keyboardprocessor_vkeys.h',
                'keyboardprocessor_bits.h',
                subdir: 'keyman')
