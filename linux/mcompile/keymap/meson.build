

project('mcompile', 'c', 'cpp',
        license: 'MIT',
        meson_version: '>=1.0')

#do I need a Version of dependency?
gtk =         dependency('gtk+-3.0', version: '>= 2.4')
x11 =         dependency('x11',  version: '>= 1.6')
xkb =         dependency('xkbcommon',  version: '>= 0.0')
libxklavier = dependency('libxklavier',  version: '>= 0.0')

deps = [gtk, x11, xkb,libxklavier]

cpp_files = files(
    'keymap.cpp',
    'mcompile.cpp',
    'filesystem.cpp',
    'mc_kmxfile.cpp',
    'mc_savekeyboard.cpp',
    'u16.cpp',)

mcompile = executable(
    'mcompile',
    sources: [cpp_files],
    dependencies: deps
    )

