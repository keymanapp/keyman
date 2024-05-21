# Copyright:    © SIL International.
# Description:  Cross platform build script to compile kmcoreldml API unit tests.
# Create Date:  5 Aug 2022
# Authors:      Marc Durdin
#

# TODO -- why are these differing from the standard.meson.build flags?
if cpp_compiler.get_id() == 'gcc' or cpp_compiler.get_id() == 'clang' or cpp_compiler.get_id() == 'emscripten'
  warns = [
     '-Wno-missing-field-initializers',
     '-Wno-unused-parameter'
  ]
else
  warns = []
endif

# Build all keyboards in output folder; these are defined here and used in the
# keyboards subdir

tests = []
invalid_tests = []

# Setup copying of source files, used in child subdir calls

if build_machine.system() == 'windows'
  copy_cmd = [find_program('cmd.exe', required: true), '/c', 'copy']
else
  copy_cmd = [find_program('cp', required: true)]
endif

if node.found()
  # Note: if node is not available, we cannot build the keyboards; build.sh
  # emits a warning that the ldml keyboard tests will be skipped
  subdir('keyboards')
  subdir('invalid-keyboards')
endif


# Build ldml test executable

if cpp_compiler.get_id() == 'emscripten'
  tests_flags = ['--embed-file', join_paths(meson.current_build_dir(),'keyboards','@')]
  tests_flags += ['--embed-file', join_paths(meson.current_build_dir(),'invalid-keyboards','@')]
  test_path = '/'
  invalid_test_path = '/'
else
  tests_flags = []
  test_path = join_paths(meson.current_build_dir(),'keyboards')
  invalid_test_path = join_paths(meson.current_build_dir(),'invalid-keyboards')
endif

ldml = executable('ldml',
    ['ldml.cpp',
    'ldml_test_source.cpp',
    'ldml_test_utils.cpp',
    common_test_files],
    cpp_args: defns + warns,
    include_directories: [inc, libsrc, '../../../../developer/src/ext/json'],
    link_args: links + tests_flags,
    dependencies: [icu_uc, icu_i18n],
    # link_with: [lib],
    objects: lib.extract_all_objects(recursive: false),
    )


# Build and run additional test_kmx_plus test

e = executable('test_kmx_plus', 'test_kmx_plus.cpp',
    'ldml_test_utils.cpp',
    common_test_files,
    cpp_args: defns + warns,
    include_directories: [inc, libsrc, '../../../../developer/src/ext/json'],
    link_args: links + tests_flags,
    dependencies: [icu_uc, icu_i18n],
    objects: lib.extract_all_objects(recursive: false))
test('test_kmx_plus', e, suite: 'ldml')

# run transforms / ldml utilities unit test

t = executable('test_transforms', 'test_transforms.cpp',
    common_test_files,
    cpp_args: defns + warns,
    include_directories: [inc, libsrc, '../../../../developer/src/ext/json'],
    link_args: links + tests_flags,
    dependencies: [icu_uc, icu_i18n],
    objects: lib.extract_all_objects(recursive: false))
test('test_transforms', t, suite: 'ldml')

# run test_context_normalization ldml unit test

normalization_tests_flags = tests_flags

if cpp_compiler.get_id() == 'emscripten'
  normalization_tests_flags += ['-lnodefs.js', '-sEXPORTED_RUNTIME_METHODS=[\'UTF8ToString\']']
endif

t = executable('test_context_normalization',
    ['test_context_normalization.cpp', common_test_files],
    cpp_args: defns + warns,
    include_directories: [inc, libsrc, '../../../../developer/src/ext/json'],
    link_args: links + normalization_tests_flags,
    dependencies: [icu_uc, icu_i18n],
    objects: lib.extract_all_objects(recursive: false))
test('test_context_normalization', t, suite: 'ldml')

# Run tests on all keyboards (`tests` defined in keyboards/meson.build)

foreach kbd : tests
  kbd_src = join_paths(test_path, kbd) + '.xml'
  kbd_obj = join_paths(test_path, kbd) + '.kmx'
  test(kbd, ldml, args: [kbd_src, kbd_obj], suite: 'ldml-keyboards')
endforeach

# Run tests on all invalid keyboards (`invalid_tests` defined in invalid-keyboards/meson.build)

foreach kbd : invalid_tests
  kbd_src = join_paths(invalid_test_path, kbd) + '.xml'
  kbd_obj = join_paths(invalid_test_path, kbd) + '.kmx'
  test(kbd, ldml, args: [kbd_src, kbd_obj], suite: 'ldml-invalid-keyboards')
  # todo: consider if we should use `should_fail: true`?
endforeach
