test_service_files = files(
  '../src/main.cpp',
  '../src/KeymanSystemService.cpp',
  '../src/OrderedOutputDevice.cpp',
  'KeyboardDeviceMock.cpp',
  'OrderedOutputDeviceMock.cpp',
)

deps = [evdev, systemd]

test_c_args = [
  '-DKEYMAN_TESTING',
  '-DKeyboardDevice=KeyboardDeviceMock',
]

exe = executable(
  'keyman-test-service',
  sources: [test_service_files],
  c_args: test_c_args,
  cpp_args: test_c_args,
  dependencies: deps,
  include_directories: [ '..', '../src' ]
)

# we currently don't have any real unit tests for keyman-system-service.
# The keyman-test-service we build here gets used by the ibus-keyman
# integration tests.
# The unit test we have here is just a stub to make coverage reporting
# happy.

gtk = dependency('gtk+-3.0', version: '>= 2.4')

test_deps = [gtk]
test_env = [
  'G_TEST_SRCDIR=' + meson.current_source_dir(),
  'G_TEST_BUILDDIR=' + meson.current_build_dir(),
]

test_exe = executable(
  'keyman-system-service-tests',
  'testfixture.cpp',
  dependencies: test_deps,
)

test(
  'test-stub',
  test_exe,
  protocol: 'tap'
)

subdir('services')
