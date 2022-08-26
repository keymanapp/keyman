# Copyright:    © SIL International.
# Description:  Cross platform build script to compile kmkbpldml API unit tests.
# Create Date:  5 Aug 2022
# Authors:      Marc Durdin
#

if compiler.get_id() == 'gcc' or compiler.get_id() == 'clang' or compiler.get_id() == 'emscripten'
  warns = [
     '-Wno-missing-field-initializers',
     '-Wno-unused-parameter'
  ]
else
  warns = []
endif

# Build all keyboards in output folder

tests = [
  'k_000_null_keyboard',
  'k_001_tiny',
  'k_002_null_invalid'
]

subdir('keyboards')

# Build ldml test executable

if compiler.get_id() == 'emscripten'
  tests_flags = ['--embed-file', join_paths(meson.current_build_dir(),'keyboards','@')]
  #source_path = '/'
  test_path = '/'
else
  tests_flags = []
  #source_path = meson.current_source_dir()
  test_path = join_paths(meson.current_build_dir(),'keyboards')
endif

ldml = executable('ldml',
    'ldml.cpp',
    'ldml_test_source.cpp',
    cpp_args: defns + warns,
    include_directories: [inc, libsrc],
    link_args: links + tests_flags,
    objects: lib.extract_all_objects())

foreach kbd : tests
  kbd_src = join_paths(test_path, kbd) + '.xml'
  kbd_obj = join_paths(test_path, kbd) + '.kmx'
  test(kbd, ldml, args: [kbd_src, kbd_obj])
endforeach

e = executable('test_kmx_plus', 'test_kmx_plus.cpp',
    cpp_args: defns + warns,
    include_directories: [inc, libsrc],
    link_args: links + tests_flags,
    objects: lib.extract_all_objects())
test('test_kmx_plus', e)
