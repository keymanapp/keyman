sources = [
  'ibusimcontext.c',
  'ibusimcontext.h',
  'testfixture.cpp',
  'testmodule.c',
  'testmodule.h',
  kmxfiles,
  corefiles,
  utilfiles,
  coretestfiles,
]

deps = [ibus, gtk, x11, json_glib, kmnkbp]

include_dirs = [
  include_directories('../../../core/src'),
  include_directories('../../../core/src/kmx'),
  include_directories('../../../core/tests/unit/kmx'),
  include_directories('../../../common/include'),
  include_directories('../src'),
  include_directories('..'),
]

test_exe = executable(
  'ibus-keyman-tests',
  sources: sources,
  dependencies: deps,
  include_directories: include_dirs
)

env_file = '/tmp/env.txt'
pid_file = '/tmp/ibus-keyman-test-pids'

setup_tests = find_program('setup-tests.sh', dirs: [join_paths(meson.current_source_dir(), 'scripts')])
teardown_tests = find_program('teardown-tests.sh', dirs: [join_paths(meson.current_source_dir(), 'scripts')])
run_test = find_program('run-single-test.sh', dirs: [join_paths(meson.current_source_dir(), 'scripts')])
find_tests = find_program('find-tests.sh', dirs: [join_paths(meson.current_source_dir(), 'scripts')])

test(
  'setup-x11',
  setup_tests,
  args: ['--x11', env_file, pid_file],
  env: [
    'G_TEST_SRCDIR=@0@'.format(meson.current_source_dir()),
    'G_TEST_BUILDDIR=@0@'.format(meson.current_build_dir()),
  ],
  priority: -10,
  is_parallel: false,
  protocol: 'exitcode'
)

test(
  'setup-wayland',
  setup_tests,
  args: ['--wayland', env_file, pid_file],
  env: [
    'G_TEST_SRCDIR=@0@'.format(meson.current_source_dir()),
    'G_TEST_BUILDDIR=@0@'.format(meson.current_build_dir()),
  ],
  priority: -20,
  is_parallel: false,
  protocol: 'exitcode'
)

test(
  'teardown-x11',
  teardown_tests,
  args: [pid_file],
  priority: -19,
  is_parallel: false,
  protocol: 'exitcode'
)

test(
  'teardown-wayland',
  teardown_tests,
  args: [pid_file],
  priority: -29,
  is_parallel: false,
  protocol: 'exitcode'
)

kmx_dir = join_paths(meson.current_source_dir(), '../../../core/build/arch/debug/tests/unit/kmx')
kmx_files = run_command(
  find_tests,
  kmx_dir,
  check: true,
).stdout().split('\n')

foreach kmx: kmx_files
  filename = kmx.split('\t')
  if filename[0] == ''
    continue
  endif
  testname = filename[1].split('.kmx')[0]
  test(
    'X11-' + testname + '__surrounding-text',
    run_test,
    args: [ '--tap', '-k', '--x11', '--env', env_file, '--surrounding-text', '--', filename],
    env: [
      'G_TEST_SRCDIR=@0@'.format(meson.current_source_dir()),
      'G_TEST_BUILDDIR=@0@'.format(meson.current_build_dir()),
    ],
    depends: [test_exe],
    priority: -11,
    is_parallel: false,
    timeout: 120,
    protocol: 'tap',
  )
  test(
    'X11-' + testname + '__no-surrounding-text',
    run_test,
    args: [ '--tap', '-k', '--x11', '--env', env_file, '--no-surrounding-text', '--', filename],
    env: [
      'G_TEST_SRCDIR=@0@'.format(meson.current_source_dir()),
      'G_TEST_BUILDDIR=@0@'.format(meson.current_build_dir()),
    ],
    depends: [test_exe],
    priority: -12,
    is_parallel: false,
    timeout: 120,
    protocol: 'tap',
  )
  test(
    'Wayland-' + testname + '__surrounding-text',
    run_test,
    args: [ '--tap', '-k', '--wayland', '--env', env_file, '--surrounding-text', '--', filename],
    env: [
      'G_TEST_SRCDIR=@0@'.format(meson.current_source_dir()),
      'G_TEST_BUILDDIR=@0@'.format(meson.current_build_dir()),
    ],
    depends: [test_exe],
    priority: -21,
    is_parallel: false,
    timeout: 120,
    protocol: 'tap',
  )
  test(
    'Wayland-' + testname + '__no-surrounding-text',
    run_test,
    args: [ '--tap', '-k', '--wayland', '--env', env_file, '--no-surrounding-text', '--', filename],
    env: [
      'G_TEST_SRCDIR=@0@'.format(meson.current_source_dir()),
      'G_TEST_BUILDDIR=@0@'.format(meson.current_build_dir()),
    ],
    depends: [test_exe],
    priority: -22,
    is_parallel: false,
    timeout: 120,
    protocol: 'tap',
  )
endforeach
