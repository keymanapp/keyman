project('ibus-keyman', 'c', 'cpp',
        version: run_command('cat', '../../VERSION.md', check: true).stdout().strip(),
        license: 'GPL-2+',
        # default_options : ['buildtype=release',
        #                    'cpp_std=c++14',
        #                    'b_vscrt=static_from_buildtype',
        #                    'warning_level=2'],
        meson_version: '>=0.53.0')
cc = meson.get_compiler('c')
conf = configuration_data()

ibus =           dependency('ibus-1.0',             version: '>= 1.2.0')
gtk =            dependency('gtk+-3.0',             version: '>= 2.4')
json_glib =      dependency('json-glib-1.0',        version: '>= 1.0')
systemd  =       dependency('libsystemd')

core_dir = meson.current_source_dir() / '../../core'
common_dir = meson.current_source_dir() / '../../common'

kmnkbp_lib = cc.find_library(
  'kmnkbp0', # meson will prefix 'lib'
  dirs: [ core_dir / 'build/arch' / get_option('buildtype') / 'src' ]
)

env = find_program('env')

# Check if we have patched ibus (https://github.com/ibus/ibus/pull/2440)
if cc.has_header_symbol('ibus.h', 'IBUS_CAP_PREFILTER', dependencies: [ibus], required: false)
  conf.set('IBUS_HAS_PREFILTER', 1)
endif

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

subdir('icons')
subdir('src')
subdir('tests')
