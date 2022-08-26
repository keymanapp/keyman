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

# Build all keyboards in output folder; these are defined here and used in the
# keyboards subdir

subdir('keyboards')

# Build ldml test executable

if compiler.get_id() == 'emscripten'
  tests_flags = ['--embed-file', join_paths(meson.current_build_dir(),'keyboards','@')]
  test_path = '/'
else
  tests_flags = []
  test_path = join_paths(meson.current_build_dir(),'keyboards')
endif

ldml = executable('ldml',
    'ldml.cpp',
    'ldml_test_source.cpp',
    cpp_args: defns + warns,
    include_directories: [inc, libsrc],
    link_args: links + tests_flags,
    objects: lib.extract_all_objects())

# Run tests on all keyboards (`tests` defined in keyboards/meson.build)

foreach kbd : tests
  kbd_src = join_paths(test_path, kbd) + '.xml'
  kbd_obj = join_paths(test_path, kbd) + '.kmx'
  test(kbd, ldml, args: [kbd_src, kbd_obj])
endforeach

# Build and run additional test_kmx_plus test

e = executable('test_kmx_plus', 'test_kmx_plus.cpp',
    cpp_args: defns + warns,
    include_directories: [inc, libsrc],
    link_args: links + tests_flags,
    objects: lib.extract_all_objects())
test('test_kmx_plus', e)
