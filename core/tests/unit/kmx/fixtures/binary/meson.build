#
# Keyman is copyright (C) SIL Global. MIT License.
#
# Created by Tim Eves (TSE) on 2018-10-19
#
# Cross platform build script to compile libkeymancore API unit tests.
#

# Build all binary test keyboards

binary_test_path = meson.current_build_dir()

binary_tests = [
  'b_000_zero_checksum'
]

foreach kbd : binary_tests
  configure_file(
    input: kbd + '.kmn',
    output: kbd + '.kmn',
    copy: true
  )

  configure_file(
    command: hextobin_cmd + ['@INPUT@', '@OUTPUT@'],
    output: kbd + '.kmx',
    input: kbd + '.txt'
  )
endforeach

# Load all binary test keyboards

foreach kbd : binary_tests
  kbd_src = join_paths(binary_test_path, kbd) + '.kmn'
  kbd_obj = join_paths(binary_test_path, kbd) + '.kmx'
  test('kmx-binary/' + kbd, kmx_tests_executable, args: [kbd_src, kbd_obj], suite: 'kmx', protocol: 'gtest')
endforeach
