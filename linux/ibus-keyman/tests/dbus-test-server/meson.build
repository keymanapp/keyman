system_service_dir = meson.global_source_root() / '../keyman-system-service/build' / build_machine.cpu_family() / get_option('buildtype') / 'tests/services'

dbus_test_server = executable(
  'km-dbus-test-server',
  'KmDbusTestServer.cpp',
  dependencies: dbus_deps,
  include_directories: ['../..'],
  cpp_args: [
  '-DKEYMAN_TEST_SERVICE_PATH="' + system_service_dir + '"'
 ]
)

stop_test_server = executable(
  'stop-test-server',
  'StopTestServer.cpp',
  dependencies: dbus_deps,
  include_directories: ['../..'],
)
