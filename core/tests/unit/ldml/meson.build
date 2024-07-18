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

if node.found()
  # Note: if node is not available, we cannot build the keyboards; build.sh
  # emits a warning that the ldml keyboard tests will be skipped
  subdir('keyboards')
  subdir('invalid-keyboards')
endif

# Build ldml test executable

keyboard_build_path = meson.current_build_dir() / 'keyboards'

if cpp_compiler.get_id() == 'emscripten'
  tests_flags = ['--embed-file',  meson.current_build_dir() / 'keyboards' / '@']
  tests_flags += ['--embed-file', meson.current_build_dir() / 'invalid-keyboards' / '@']
  tests_flags += ['--embed-file', meson.current_build_dir() / 'nodeversions.json' + '@nodeversions.json']
  tests_flags += ['--embed-file', meson.current_build_dir() / 'package.json' + '@package.json']
  tests_flags += ['--embed-file', meson.current_build_dir() / 'Blocks.txt' + '@Blocks.txt']
  test_path = '/'
  test_unicode_path = '/'
  invalid_test_path = '/'
  tests_flags += ['-lnodefs.js',
                  '-sNO_DISABLE_EXCEPTION_CATCHING', # for test exceptions
                  wasm_exported_runtime_methods]
else
  tests_flags = []
  test_path         = meson.current_build_dir() / 'keyboards'
  test_unicode_path = meson.current_build_dir()
  invalid_test_path = meson.current_build_dir() / 'invalid-keyboards'
endif

# copy package.json into build dir for test use
configure_file(
  copy: true,
  input: '../../../../package.json',
  output: 'package.json',
)
configure_file(
  copy: true,
  input: '../../../../resources/standards-data/unicode-character-database/Blocks.txt',
  output: 'Blocks.txt',
)

configure_file(
  command: [node, meson.current_source_dir() / 'write_node_versions.js','@OUTPUT@'],
  output: 'nodeversions.json',
)

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

core_ldml_min = executable('core_ldml_min',
    ['core_ldml_min.cpp'],
    cpp_args: defns + warns,
    include_directories: [inc, libsrc],
    link_args: links,
    dependencies: [],
    link_with: [lib],
    # objects: lib.extract_all_objects(recursive: false),
    )
test('core_ldml_min', core_ldml_min, suite: 'ldml', should_fail: true)


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
  normalization_tests_flags += ['-lnodefs.js', wasm_exported_runtime_methods]
endif

test_context_normalization = executable('test_context_normalization',
    ['test_context_normalization.cpp', common_test_files],
    cpp_args: defns + warns,
    include_directories: [inc, libsrc, '../../../../developer/src/ext/json'],
    link_args: links + normalization_tests_flags,
    dependencies: [icu_uc, icu_i18n],
    objects: lib.extract_all_objects(recursive: false))
test('test_context_normalization', test_context_normalization, suite: 'ldml')

# Build and run additional test_unicode test

test_unicode = executable('test_unicode', 'test_unicode.cpp',
    ['test_unicode.cpp', common_test_files, generated_headers],
    cpp_args: defns + warns,
    include_directories: [inc, libsrc, '../../../../developer/src/ext/json'],
    link_args: links + tests_flags,
    dependencies: [icu_uc, icu_i18n],
    objects: lib.extract_all_objects(recursive: false),

)

test('test_unicode', test_unicode, suite: 'ldml',
  args: [
    test_unicode_path / 'nodeversions.json',
    test_unicode_path / 'package.json',
    test_unicode_path / 'Blocks.txt',
  ],
)

# Run tests on all keyboards (`tests` defined in keyboards/meson.build)

foreach kbd : tests
  kbd_src = test_path / kbd + '.xml'
  kbd_obj = test_path / kbd + '.kmx'
  test(kbd, ldml, args: [kbd_src, kbd_obj], suite: 'ldml-keyboards')
endforeach

# Run tests on all invalid keyboards (`invalid_tests` defined in invalid-keyboards/meson.build)

foreach kbd : invalid_tests
  kbd_src = invalid_test_path / kbd  + '.xml'
  kbd_obj = invalid_test_path / kbd  + '.kmx'
  test(kbd, ldml, args: [kbd_src, kbd_obj], suite: 'ldml-invalid-keyboards')
  # todo: consider if we should use `should_fail: true`?
endforeach
