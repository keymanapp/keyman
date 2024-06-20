project('mcompile', 'c', 'cpp',
        license: 'MIT',
        meson_version: '>=1.0')

#do I need a Version of dependency?
gtk =         dependency('gtk+-3.0', version: '>= 2.4')
x11 =         dependency('x11',  version: '>= 1.6')
xkb =         dependency('xkbcommon')
libxklavier = dependency('libxklavier')

deps = [gtk, x11, xkb,libxklavier]

cpp_files = files(
    'keymap.cpp',
    'deadkey.cpp',
    'mcompile.cpp',
    'filesystem.cpp',
    'mc_kmxfile.cpp',
    'mc_import_rules.cpp',
    'u16.cpp',)

comon_include_dir = [
  include_directories('../../../../common/include'),
]


mcompile = executable(
    'mcompile',
    sources: [cpp_files],
    dependencies: deps,
    include_directories : comon_include_dir
    )
