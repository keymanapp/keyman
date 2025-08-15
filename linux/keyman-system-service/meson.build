project('keyman-system-service', 'c', 'cpp',
        version: files('../../VERSION.md'),
        default_options: ['cpp_std=c++17'],
        license: 'MIT',
        meson_version: '>=1.0')

conf = configuration_data()

evdev    = dependency('libevdev', version: '>= 1.9')

systemd  = dependency('libsystemd', 'libelogind', required: false)
if systemd.found()
  conf.set('DBUS_IMPLEMENTATION', 'SYSTEMD')
else
  # Gentoo can use libelogind or basu
  systemd = dependency('basu', required: true)
  conf.set('DBUS_IMPLEMENTATION', 'BASU')
endif

configure_file(output : 'config.h',
               configuration : conf)

linux_include_dirs = [
  include_directories('../include')
]

subdir('resources')
subdir('src')
subdir('tests')
