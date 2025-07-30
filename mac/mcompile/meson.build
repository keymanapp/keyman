project('mcompile', 'c', 'cpp',
        version: files('../../VERSION.md'),
        license: 'MIT',
        meson_version: '>=1.0')

# see https://github.com/keymanapp/keyman/pull/11334#issuecomment-2290784399
add_project_arguments('-O2', language : 'cpp')

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
    sources: [cpp_files],
    dependencies: deps,
    include_directories : comon_include_dir
    )
