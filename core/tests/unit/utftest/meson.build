#
# Keyman is copyright (C) SIL Global. MIT License.
#
# Created by Tim Eves (TSE) on 2018-10-02
#
# Cross platform build script to compile utfcodec unit test.
#

utftest_tests = executable('utftest_tests',
  ['utftest.tests.cpp', '../helpers/main.cpp', '../helpers/emscripten_filesystem.cpp'],
  objects: lib.extract_objects('../../common/cpp/utfcodec.cpp'),
  dependencies: test_dependencies,
  include_directories: test_includes,
  link_args: links + test_link_args,
)

test('utftest_tests', utftest_tests, suite: 'utf', protocol: 'gtest')
