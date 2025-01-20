# Copyright:    © 2018 SIL International.
# Description:  Cross platform build script to compile libkeymancore API unit tests.
# Create Date:  19 Oct 2018
# Authors:      Marc Durdin, Tim Eves (TSE)
# History:      19  Oct 2018 - TSE - Added test for context API functions.
#

# binary file unit tests

# Build all binary test keyboards

binary_test_path = meson.current_build_dir()

binary_tests = [
  'b_000_zero_checksum'
]

foreach kbd : binary_tests
  fs.copyfile(kbd + '.kmn', kbd + '.kmn')

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
  test(kbd, kmx, args: [kbd_src, kbd_obj])
endforeach
