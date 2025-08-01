project('ibus-keyman', 'c', 'cpp',
        version: files('../../VERSION.md'),
        default_options: ['cpp_std=c++17'],
        license: 'GPL-2+',
        meson_version: '>=1.0')

# Import our standard compiler defines; this is copied from
# /resources/build/standard.meson.build by build.sh, because
# meson doesn't allow us to reference a file outside its root
subdir('resources')

if get_option('buildtype') != 'debug'
  # Disable assertions on release builds
  defns += ['-DG_DISABLE_ASSERT']
endif

conf = configuration_data()

ibus =           dependency('ibus-1.0',             version: '>= 1.2.0')
gtk =            dependency('gtk+-3.0',             version: '>= 2.4')
json_glib =      dependency('json-glib-1.0',        version: '>= 1.0')
icu =            [dependency('icu-i18n'), dependency('icu-uc')]

systemd  =       dependency('libsystemd', 'libelogind', required: false)
if systemd.found()
  conf.set('DBUS_IMPLEMENTATION', 'SYSTEMD')
else
  # Gentoo can use libelogind or basu
  systemd = dependency('basu', required: true)
  conf.set('DBUS_IMPLEMENTATION', 'BASU')
endif

core_dir = meson.current_source_dir() / '../../core'
common_dir = meson.current_source_dir() / '../../common'

keymancore_lib = c_compiler.find_library(
  'keymancore', # meson will prefix 'lib'
  dirs: [ core_dir / 'build/arch' / get_option('buildtype') / 'src' ]
)

if get_option('keyman_deb_pkg_build')
  add_global_arguments('-DKEYMAN_PKG_BUILD', language: ['cpp', 'c'])
endif

env = find_program('env')

conf.set('HAVE_CONFIG_H', 1)
configure_file(output : 'config.h',
               configuration : conf)

core_include_dirs = [
  include_directories('../../core/src'),
  include_directories('../../core/src/kmx'),
  include_directories('../../core/include'),
  include_directories('../../common/include'),
  include_directories('../../core/build/arch' / get_option('buildtype') / 'include'),
]

linux_include_dirs = [
  include_directories('../include')
]

subdir('icons')
subdir('src')
subdir('tests')
