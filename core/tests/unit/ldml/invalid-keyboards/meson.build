# Copyright:    © SIL International.
# Description:  Cross platform build script to copy invalid keyboards for unit tests.
# Create Date:  5 Aug 2022
# Authors:      Marc Durdin
#

invalid_tests = [
  'ik_000_null_invalid',
  'ik_001_bad_key',
  'ik_002_no_keys',
]

# Build all keyboards in output folder

foreach kbd : invalid_tests
  fs.copyfile(kbd + '.xml', kbd + '.xml')

  configure_file(
    command: hextobin_cmd + ['@INPUT@', '@OUTPUT@'],
    output: kbd + '.kmx',
    input: kbd + '.txt'
  )
endforeach
