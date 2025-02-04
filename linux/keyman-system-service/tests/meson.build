test_service_files = files(
  '../src/main.cpp',
  '../src/KeymanSystemService.cpp',
  'KeyboardDeviceMock.cpp',
)

deps = [evdev, systemd]

test_c_args = [
  '-DKEYMAN_TESTING',
  '-DKeyboardDevice=KeyboardDeviceMock'
]

exe = executable(
  'keyman-test-service',
  sources: [test_service_files],
  c_args: test_c_args,
  cpp_args: test_c_args,
  dependencies: deps,
  include_directories: [ '..', '../src' ]
)

# we currently don't have any unit tests for keyman-system-service.
# The keyman-test-service we build here gets used by the ibus-keyman
# integration tests.

subdir('services')
