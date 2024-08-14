project('mcompile', 'c', 'cpp',
        license: 'MIT',
        meson_version: '>=1.0')

carbon =         dependency('Carbon')

deps = [carbon]

subdir('resources')

cpp_files = files(
    'keymap.cpp',
    'mcompile.cpp',
    'mc_kmxfile.cpp',
    'mc_import_rules.cpp',
    '../../common/cpp/km_u16.cpp',
    )

comon_include_dir = [
  include_directories('../../common/include')
]

mcompile = executable(
    'mcompile',
    cpp_args: ['-std=c++17'], # this should be defined from resources/standard.meson.build
    sources: [cpp_files],
    dependencies: deps,
    include_directories : comon_include_dir
    )