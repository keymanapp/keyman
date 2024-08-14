project('mcompile', 'c', 'cpp',
        license: 'MIT',
        meson_version: '>=1.0')

gtk =         dependency('gtk+-3.0', version: '>= 2.4')
xkb =         dependency('xkbcommon')
libxklavier = dependency('libxklavier')

deps = [gtk, xkb,libxklavier]

subdir('resources')

cpp_files = files(
    'keymap.cpp',
    'deadkey.cpp',
    'mcompile.cpp',
    'mc_kmxfile.cpp',
    'mc_import_rules.cpp',
    'u16.cpp',)

comon_include_dir = [
  include_directories('../../../common/include'),
  include_directories('../../../core/src'),
]

mcompile = executable(
    'mcompile',
    sources: [cpp_files],
    dependencies: deps,
    include_directories : comon_include_dir
    )
