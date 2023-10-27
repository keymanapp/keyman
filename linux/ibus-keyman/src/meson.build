util_files = files(
  'keymanutil.c',
  'kmpdetails.c',
  'bcp47util.c',
)

engine_files = files(
  'main.c',
  'engine.c',
  'keyman-service.c',
  'KeymanSystemServiceClient.cpp',
)

include_dirs = [
  core_include_dirs,
  include_directories('..'),
  include_directories(meson.current_build_dir() / '..'),
]

deps = [gtk, ibus, icu, json_glib, keymancore_lib, systemd]

prefix = get_option('prefix')
cfg = configuration_data()
cfg.set('VERSION', meson.project_version())
cfg.set('LIBEXECDIR', get_option('prefix') / get_option('libexecdir'))

configure_file(
  configuration: cfg,
  input: 'keyman.xml.in',
  output: 'keyman.xml',
  install: true,
  install_dir: get_option('datadir') / 'ibus/component',
)

configure_file(
  configuration: cfg,
  input: 'keyman-version.h.in',
  output: 'keyman-version.h'
)

exe = executable(
  'ibus-engine-keyman',
  sources: [engine_files, util_files],
  dependencies: deps,
  include_directories: include_dirs,
  install: true,
  install_dir: get_option('libexecdir'),
)

subdir('test')
