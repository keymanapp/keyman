keymanutil_sources = [
  'keymanutil_tests.c',
  util_files,
]

keymanutil_deps = [ibus, gtk, json_glib, kmnkbp_lib]

test_env = [
  'G_TEST_SRCDIR=' + meson.current_source_dir(),
  'G_TEST_BUILDDIR=' + meson.current_build_dir(),
]

test_include_dirs = [
  include_dirs,
  include_directories('..'),
  include_directories(meson.current_build_dir() / '..'),
]

executable(
  'keymanutil-tests',
  sources: keymanutil_sources,
  dependencies: keymanutil_deps,
  include_directories : test_include_dirs
)

test(
  'keymanutil-tests',
  find_program('run-tests.sh'),
  env: test_env,
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
    include_directories: test_include_dirs
  ),
  args: [ meson.current_source_dir() ],
  env: test_env,
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
    include_directories: test_include_dirs
  ),
  args: [ meson.current_source_dir() / 'kmp.json' ],
  env: test_env,
  protocol: 'exitcode',
)
