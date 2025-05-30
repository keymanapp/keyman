# Copyright:    © SIL International.
# Description:  Cross platform build script to compile kmcoreldml API unit tests.
# Create Date:  5 Aug 2022
# Authors:      Marc Durdin
#

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

gtest = subproject('gtest')
gtest_dep = gtest.get_variable('gtest_dep')
gmock_dep = gtest.get_variable('gmock_dep')

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
fs.copyfile('../../../../package.json', 'package.json')
fs.copyfile('../../../../resources/standards-data/unicode-character-database/Blocks.txt', 'Blocks.txt')

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

core_ldml_min = executable('core_ldml_min_tests',
    [
      'core_ldml_min.tests.cpp',
      common_test_files,
    ],
    cpp_args: defns + warns,
    include_directories: [inc, libsrc],
    link_args: links,
    dependencies: [],
    link_with: [lib],
    # objects: lib.extract_all_objects(recursive: false),
    )
test('core_ldml_min_tests', core_ldml_min, suite: 'ldml', should_fail: true)


# Build and run additional test_kmx_plus test

e = executable('kmx_plus_tests', 'kmx_plus.tests.cpp',
    'ldml_test_utils.cpp',
    common_test_files,
    cpp_args: defns + warns,
    include_directories: [inc, libsrc, '../../../../developer/src/ext/json'],
    link_args: links + tests_flags,
    dependencies: [icu_uc, icu_i18n, gtest_dep, gmock_dep],
    objects: lib.extract_all_objects(recursive: false))
test('kmx_plus_tests', e, suite: 'ldml')

# run transforms / ldml utilities unit test

t = executable('transforms_tests', 'transforms.tests.cpp',
    common_test_files,
    cpp_args: defns + warns,
    include_directories: [inc, libsrc, '../../../../developer/src/ext/json'],
    link_args: links + tests_flags,
    dependencies: [icu_uc, icu_i18n],
    objects: lib.extract_all_objects(recursive: false))
test('transforms_tests', t, suite: 'ldml')

# run test_context_normalization ldml unit test

normalization_tests_flags = tests_flags

if cpp_compiler.get_id() == 'emscripten'
  normalization_tests_flags += ['-lnodefs.js', wasm_exported_runtime_methods]
endif

test_context_normalization = executable('context_normalization_tests',
    ['context_normalization.tests.cpp', common_test_files],
    cpp_args: defns + warns,
    include_directories: [inc, libsrc, '../../../../developer/src/ext/json'],
    link_args: links + normalization_tests_flags,
    dependencies: [icu_uc, icu_i18n],
    objects: lib.extract_all_objects(recursive: false))
test('context_normalization_tests', test_context_normalization, suite: 'ldml')

# Build and run additional test_unicode test

test_unicode = executable('unicode_tests', 'unicode.tests.cpp',
    ['unicode.tests.cpp', common_test_files, generated_headers],
    cpp_args: defns + warns,
    include_directories: [inc, libsrc, '../../../../developer/src/ext/json'],
    link_args: links + tests_flags,
    dependencies: [icu_uc, icu_i18n],
    objects: lib.extract_all_objects(recursive: false),

)

test('unicode_tests', test_unicode, suite: 'ldml',
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
