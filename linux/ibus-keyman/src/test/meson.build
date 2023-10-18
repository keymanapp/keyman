keymanutil_sources = [
  'keymanutil_tests.c',
  util_files,
]

keymanutil_deps = [gtk, ibus, icu, json_glib, keymancore_lib]

test_env = [
  'G_TEST_SRCDIR=' + meson.current_source_dir(),
  'G_TEST_BUILDDIR=' + meson.current_build_dir(),
  'TOP_SRCDIR=' + meson.global_source_root(),
  'TOP_BINDIR=' + meson.build_root(),
]

test_include_dirs = [
  include_dirs,
  include_directories('..'),
  include_directories(meson.current_build_dir() / '..'),
]

env_file = '/tmp/env-src-test.txt'
cleanup_file = '/tmp/ibus-keyman-src-test-pids'
pid_file = '/tmp/ibus-keyman-src-test-pids.pids'

setup_src_test_tests = find_program('setup-tests.sh', dirs: [meson.current_source_dir()])
teardown_tests = find_program('teardown-tests.sh', dirs: [meson.current_source_dir() / '../../tests/scripts'])
run_src_test = find_program('run-single-test.sh', dirs: [meson.current_source_dir()])

keymanutil_tests = executable(
  'keymanutil-tests',
  sources: keymanutil_sources,
  dependencies: keymanutil_deps,
  include_directories : test_include_dirs
)

print_kmpdetails_test = executable(
  'print_kmpdetails',
  sources: [
    'print_kmpdetails.c',
    '../kmpdetails.c'
  ],
  dependencies: [ json_glib ],
  include_directories: test_include_dirs
)

print_kmp_test = executable(
  'print_kmp',
  sources: [
    'print_kmp.c',
  ],
  dependencies: [ json_glib ],
  include_directories: test_include_dirs
)

bcp47_util_tests = executable(
  'bcp47-util-tests',
  sources: [
    'bcp47util_tests.c',
    '../bcp47util.c'
  ],
  dependencies: [ gtk, icu ],
  include_directories: test_include_dirs
)

test(
  'setup-src-test',
  setup_src_test_tests,
  args: ['--x11', env_file, cleanup_file, pid_file],
  env: test_env,
  priority: -1,
  is_parallel: false,
  protocol: 'exitcode'
)

test(
  'teardown-src-test',
  teardown_tests,
  args: [cleanup_file],
  priority: -9,
  is_parallel: false,
  protocol: 'exitcode'
)

test(
  'keymanutil-tests',
  run_src_test,
  args: [ '--tap', '-k', '--env', env_file, '--', keymanutil_tests, '--testdata', meson.current_source_dir() / 'testdata'],
  env: test_env,
  priority: -2,
  is_parallel: false,
  protocol: 'tap',
)

test(
  'print-kmpdetails-test',
  run_src_test,
  args: [ '--', print_kmpdetails_test, meson.current_source_dir() ],
  env: test_env,
  priority: -2,
  protocol: 'exitcode',
)

test(
  'print-kmp-test',
  run_src_test,
  args: [ '--', print_kmp_test, meson.current_source_dir() / 'testdata/kmp.json' ],
  env: test_env,
  priority: -2,
  protocol: 'exitcode',
)

test(
  'bcp47-util-tests',
  run_src_test,
  args: [ '--tap', '-k', '--env', env_file, '--', bcp47_util_tests ],
  env: test_env,
  priority: -2,
  is_parallel: false,
  protocol: 'tap',
)
