utilfiles = files(
  'keymanutil.c',
  'keymanutil.h',
  'kmpdetails.c',
  'kmpdetails.h',
)

engine_sources = files(
  'main.c',
  'engine.c',
  'engine.h',
  'keycodes.h',
  'keyman-service.c',
  'keyman-service.h',
)

deps = [ibus, gtk, x11, json_glib, kmnkbp]

prefix = get_option('prefix')
cfg = configuration_data()
cfg.set('VERSION', meson.project_version())
cfg.set('libexecdir', join_paths(prefix, get_option('libexecdir')))

configure_file(
  configuration: cfg,
  input: 'keyman.xml.in',
  output: 'keyman.xml'
)

configure_file(
  configuration: cfg,
  input: 'keyman-version.h.in',
  output: 'keyman-version.h'
)

exe = executable(
  'ibus-engine-keyman',
  sources: [engine_sources, utilfiles],
  dependencies: deps,
  include_directories: include_directories('@0@/..'.format(meson.current_build_dir())),
)

subdir('test')
