project('keyman-system-service', 'c', 'cpp',
        version: run_command('cat', '../../VERSION.md', check: true).stdout().strip(),
        license: 'GPL-2+',
        meson_version: '>=1.0')

evdev    = dependency('libevdev', version: '>= 1.9')
systemd  = dependency('libsystemd')

subdir('resources')
subdir('src')
subdir('tests')
