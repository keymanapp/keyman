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

ibus =        dependency('ibus-1.0',             version: '>= 1.2.0')
gtk =         dependency('gtk+-3.0',             version: '>= 2.4')
x11 =         dependency('x11',                  version: '>= 1.6')
json_glib =   dependency('json-glib-1.0',        version: '>= 1.0')
kmnkbp =      dependency('keyman_kmn_processor', version: '>= 0.0.0')

env = find_program('env')

# define some variables. We wouldn't need this if we'd have a meson.build file
# in the Keyman root directory...
coredir = '@0@/../../core'.format(meson.current_source_dir())

kmxfiles = files(
  join_paths(coredir, 'src/kmx/kmx_consts.cpp'),
  join_paths(coredir, 'src/kmx/kmx_processevent.cpp'),
  join_paths(coredir, 'src/kmx/kmx_actions.cpp'),
  join_paths(coredir, 'src/kmx/kmx_capslock.cpp'),
  join_paths(coredir, 'src/kmx/kmx_context.cpp'),
  join_paths(coredir, 'src/kmx/kmx_conversion.cpp'),
  join_paths(coredir, 'src/kmx/kmx_debug.cpp'),
  join_paths(coredir, 'src/kmx/kmx_debugger.cpp'),
  join_paths(coredir, 'src/kmx/kmx_environment.cpp'),
  join_paths(coredir, 'src/kmx/kmx_file.cpp'),
  join_paths(coredir, 'src/kmx/kmx_modifiers.cpp'),
  join_paths(coredir, 'src/kmx/kmx_options.cpp'),
  join_paths(coredir, 'src/kmx/kmx_processor.cpp'),
  join_paths(coredir, 'src/kmx/kmx_xstring.cpp'),
)
corefiles = files(
  join_paths(coredir, 'src/option.cpp'),
  join_paths(coredir, 'src/keyboard.cpp'),
  join_paths(coredir, 'src/state.cpp'),
  join_paths(coredir, 'src/jsonpp.cpp'),
  join_paths(coredir, 'src/utfcodec.cpp'),
)
coretestfiles = files(
  join_paths(coredir, 'tests/unit/kmx/kmx_test_source.cpp'),
)

# Check if we have patched ibus
code = '''#include <ibus.h>
int x = IBUS_PREFILTER_MASK | IBUS_CAP_PREFILTER;
'''
if cc.compiles(code, dependencies: [ibus], name: 'check for patched ibus')
  conf.set('IBUS_HAS_PREFILTER', 1)
endif

conf.set('HAVE_CONFIG_H', 1)
configure_file(output : 'config.h',
               configuration : conf)

subdir('icons')
subdir('src')
subdir('tests')
