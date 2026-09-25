#
# Keyman is copyright (C) SIL Global. MIT License.
#
# Created by Marc Durdin on 2022-08-05
#
# Cross platform build script to compile ldml processor unit tests.
#

# Build all keyboards in output folder; these are defined here and used in the
# keyboards subdir

ldml_tests = []
ldml_invalid_tests = []

# Prepare all fixtures
subdir('fixtures')

# Build ldml test executable

ldml_test_path         = meson.current_build_dir() / 'fixtures' / 'keyboards'
ldml_invalid_test_path = meson.current_build_dir() / 'fixtures' / 'invalid-keyboards'

ldml_tests_executable = executable('ldml_tests',
    ['ldml.tests.cpp',
    'ldml_test_source.cpp',
    'ldml_test_utils.cpp',
    common_test_files_without_main],
    cpp_args: defns + test_warns,
    include_directories: [test_includes, '../../../../developer/src/ext/json'],
    link_args: links + test_link_args,
    dependencies: test_dependencies,
    # link_with: [lib],
    objects: lib.extract_all_objects(recursive: false),
    )

# Run tests on all keyboards (`tests` defined in keyboards/meson.build)

foreach kbd : ldml_tests
  kbd_src = ldml_test_path / kbd + '.xml'
  kbd_obj = ldml_test_path / '17.0' / kbd + '.kmx'
  test('ldml-keyboards-17/' + kbd, ldml_tests_executable, args: [kbd_src, kbd_obj], suite: 'ldml-keyboards-17', protocol: 'gtest', )
  kbd_obj = ldml_test_path / '19.0' / kbd + '.kmx'
  test('ldml-keyboards-19/' + kbd, ldml_tests_executable, args: [kbd_src, kbd_obj], suite: 'ldml-keyboards-19', protocol: 'gtest')
endforeach

# Run tests on all invalid keyboards (`ldml_invalid_tests` defined in invalid-keyboards/meson.build)

foreach kbd : ldml_invalid_tests
  kbd_src = ldml_invalid_test_path / kbd  + '.xml'
  kbd_obj = ldml_invalid_test_path / kbd  + '.kmx'
  test('ldml-invalid-keyboards/' + kbd, ldml_tests_executable, args: [kbd_src, kbd_obj], suite: 'ldml-invalid-keyboards', protocol: 'gtest')
  # todo: consider if we should use `should_fail: true`?
endforeach

#
# LDML processor internal tests
#

# extract version information from various sources into build dir for unicode.tests use
configure_file(
  input:'../../../../package.json',
  output: 'package.json',
  copy: true
)
configure_file(
  input: '../../../../resources/standards-data/unicode-character-database/Blocks.txt',
  output: 'Blocks.txt',
  copy: true
)
configure_file(
  command: [node, meson.current_source_dir() / 'write_node_versions.js','@OUTPUT@'],
  output: 'nodeversions.json',
)

# Test definitions

ldml_defns = ['-DKM_CORE_LIBRARY_STATIC']
ldml_unit_tests = [
  ['kmx-plus-tests', ['kmx_plus.tests.cpp', 'ldml_test_utils.cpp']],
  ['transforms-tests', 'transforms.tests.cpp'],
  ['context-normalization-tests', 'context_normalization.tests.cpp'],
  ['unicode-tests', 'unicode.tests.cpp'],
]

foreach t : ldml_unit_tests
  bin = executable(t[0], [t[1], common_test_files],
    cpp_args: ldml_defns + defns + test_warns,
    include_directories: [test_includes, '../../../../developer/src/ext/json'],
    link_args: links + test_link_args,
    dependencies: test_dependencies,
    objects: lib.extract_all_objects(recursive: false))

  test(t[0], bin, suite: 'ldml', protocol: 'gtest')
endforeach
