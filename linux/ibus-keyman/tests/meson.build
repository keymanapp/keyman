test_files = [
  'ibusimcontext.c',
  'testfixture.cpp',
  'testmodule.c',
  '../src/KeymanSystemServiceClient.cpp',
]

common_test_files = [ meson.global_source_root() / '../../common/include/test_color.cpp' ]

keymancore_tests_lib = c_compiler.find_library(
  'keymancore-tests', # meson will prefix 'lib'
  dirs: [ core_dir / 'build/arch' / get_option('buildtype') / 'tests/kmx_test_source' ]
)

test_deps = [gtk, ibus, icu, json_glib, keymancore_lib, keymancore_tests_lib, systemd]
dbus_deps = [gtk, systemd]

test_env = [
  'G_TEST_SRCDIR=' + meson.current_source_dir(),
  'G_TEST_BUILDDIR=' + meson.current_build_dir(),
]

test_include_dirs = [
  core_include_dirs,
  include_directories('../../../core/tests/kmx_test_source'),
  include_directories('../src'),
  include_directories('..'),
]

test_exe = executable(
  'ibus-keyman-tests',
  test_files, util_files, common_test_files,
  dependencies: test_deps,
  include_directories: test_include_dirs,
)

system_service_dir = meson.current_source_dir() / '../../keyman-system-service/build' / build_machine.cpu_family() / get_option('buildtype') / 'tests/services'

dbus_test_server = executable(
  'km-dbus-test-server',
  'KmDbusTestServer.cpp',
  dependencies: dbus_deps,
  include_directories: ['..'],
  cpp_args: [
  '-DKEYMAN_TEST_SERVICE_PATH="' + system_service_dir + '"'
 ]
)

stop_test_server = executable(
  'stop-test-server',
  'StopTestServer.cpp',
  dependencies: dbus_deps,
  include_directories: ['..'],
)

env_file = '/tmp/env.txt'
cleanup_file = '/tmp/ibus-keyman-test-pids'
pid_file = '/tmp/ibus-keyman-test-pids.pids'

setup_tests = find_program('setup-tests.sh', dirs: [meson.current_source_dir() / 'scripts'])
teardown_tests = find_program('teardown-tests.sh', dirs: [meson.current_source_dir() / 'scripts'])
run_test = find_program('run-single-test.sh', dirs: [meson.current_source_dir() / 'scripts'])
find_tests = find_program('find-tests.sh', dirs: [meson.current_source_dir() / 'scripts'])

# Mutter 40.x added the --headless option wich we need in order to be able to run the Wayland tests
mutter = find_program('mutter', required: false, version: '>=40')
can_build_wayland = mutter.found()

# TODO: re-enable and fix test setup
# test(
#   'setup-x11',
#   setup_tests,
#   args: ['--x11', env_file, cleanup_file, pid_file],
#   env: test_env,
#   priority: -10,
#   is_parallel: false,
#   protocol: 'exitcode'
# )

# test(
#   'teardown-x11',
#   teardown_tests,
#   args: [cleanup_file],
#   priority: -19,
#   is_parallel: false,
#   protocol: 'exitcode'
# )

# if can_build_wayland
#   test(
#     'setup-wayland',
#     setup_tests,
#     args: ['--wayland', env_file, cleanup_file, pid_file],
#     env: test_env,
#     priority: -20,
#     is_parallel: false,
#     protocol: 'exitcode'
#   )

#   test(
#     'teardown-wayland',
#     teardown_tests,
#     args: [cleanup_file],
#     priority: -29,
#     is_parallel: false,
#     protocol: 'exitcode'
#   )
# endif

# kmxtest_files = run_command(
#   find_tests,
#   [ common_dir / 'test/keyboards/baseline' ],
#   check: true,
# ).stdout().split('\n')

# foreach kmx: kmxtest_files
#   filename = kmx.split('\t')
#   if filename[0] == ''
#     continue
#   endif
#   testname = filename[1].split('.kmx')[0]
#   test_args = [ '--tap', '-k', '--env', env_file, '--cleanup', cleanup_file, '--check', pid_file, '--', filename]
#   test(
#     'X11-' + testname + '__surrounding-text',
#     run_test,
#     args: [ '--testname', testname, '--x11', '--surrounding-text', test_args],
#     env: test_env,
#     depends: [test_exe],
#     priority: -11,
#     is_parallel: false,
#     timeout: 120,
#     protocol: 'tap',
#   )
#   test(
#     'X11-' + testname + '__no-surrounding-text',
#     run_test,
#     args: [ '--testname', testname, '--x11', '--no-surrounding-text', test_args],
#     env: test_env,
#     depends: [test_exe],
#     priority: -12,
#     is_parallel: false,
#     timeout: 120,
#     protocol: 'tap',
#   )
#   if can_build_wayland
#     test(
#       'Wayland-' + testname + '__surrounding-text',
#       run_test,
#       args: [ '--testname', testname, '--wayland', '--surrounding-text', test_args],
#       env: test_env,
#       depends: [test_exe],
#       priority: -21,
#       is_parallel: false,
#       timeout: 120,
#       protocol: 'tap',
#     )
#     test(
#       'Wayland-' + testname + '__no-surrounding-text',
#       run_test,
#       args: [ '--testname', testname, '--wayland', '--no-surrounding-text', test_args],
#       env: test_env,
#       depends: [test_exe],
#       priority: -22,
#       is_parallel: false,
#       timeout: 120,
#       protocol: 'tap',
#     )
#   endif
# endforeach
