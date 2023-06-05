dbus_service_files = [
  'at-spi-dbus-bus.service',
  'gvfs-daemon.service',
  'keyman-test.service',
]

config_data = configuration_data()
config_data.set('installed_tests_dir', meson.current_build_dir() / '..')

foreach service_file : dbus_service_files
  configure_file(
    input: service_file + '.in',
    output: service_file,
    configuration: config_data,
  )
endforeach
