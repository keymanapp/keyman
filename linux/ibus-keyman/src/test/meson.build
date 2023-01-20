keymanutil_sources = [
  'keymanutil_tests.c',
  utilfiles,
]

keymanutil_deps = [ibus, gtk, x11, json_glib, kmnkbp]

include_dirs = [
  include_directories('@0@/..'.format(meson.current_build_dir())),
  '..'
]

executable(
  'keymanutil-tests',
  sources: keymanutil_sources,
  dependencies: keymanutil_deps,
  include_directories : include_dirs
)

test(
  'keymanutil-tests',
  find_program('run-tests.sh'),
  env: [
    'G_TEST_SRCDIR=@0@'.format(meson.current_source_dir()),
    'G_TEST_BUILDDIR=@0@'.format(meson.current_build_dir()),
  ],
  protocol: 'tap',
)

test(
  'print-kmpdetails-test',
  executable(
    'print_kmpdetails',
    sources: [
      'print_kmpdetails.c',
      '../kmpdetails.c'
    ],
    dependencies: [ json_glib ],
    include_directories: include_dirs
  ),
  args: [ '@0@'.format(meson.current_source_dir()) ],
  env: [
    'G_TEST_SRCDIR=@0@'.format(meson.current_source_dir()),
    'G_TEST_BUILDDIR=@0@'.format(meson.current_build_dir()),
  ],
  protocol: 'exitcode',
)

test(
  'print-kmp-test',
  executable(
    'print_kmp',
    sources: [
      'print_kmp.c',
    ],
    dependencies: [ json_glib ],
    include_directories: include_dirs
  ),
  args: [ join_paths('@0@'.format(meson.current_source_dir()), 'kmp.json') ],
  env: [
    'G_TEST_SRCDIR=@0@'.format(meson.current_source_dir()),
    'G_TEST_BUILDDIR=@0@'.format(meson.current_build_dir()),
  ],
  protocol: 'exitcode',
)
