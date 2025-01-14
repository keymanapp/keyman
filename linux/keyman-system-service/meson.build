project('keyman-system-service', 'c', 'cpp',
        version: run_command('cat', '../../VERSION.md', check: true).stdout().strip(),
        license: 'GPL-2+',
        meson_version: '>=0.61')

conf = configuration_data()

evdev    = dependency('libevdev', version: '>= 1.9')

systemd  = dependency('libsystemd', 'libelogind', required: false)
if systemd.found()
  conf.set('DBUS_IMPLEMENTATION', 'SYSTEMD')
else
  # Gentoo can use libelogind or basu
  systemd = dependency('basu')
  conf.set('DBUS_IMPLEMENTATION', 'BASU')
endif

configure_file(output : 'config.h',
               configuration : conf)

subdir('resources')
subdir('src')
subdir('tests')
