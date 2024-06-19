# Copyright:    © SIL International.
# Description:  Cross platform build script to copy invalid keyboards for unit tests.
# Create Date:  5 Aug 2022
# Authors:      Marc Durdin
#

invalid_tests = [
  'ik_000_null_invalid'
]

# Build all keyboards in output folder

foreach kbd : invalid_tests
  configure_file(
    input: kbd + '.xml',
    output: kbd + '.xml',
    copy: true
  )

  configure_file(
    command: hextobin_cmd + ['@INPUT@', '@OUTPUT@'],
    output: kbd + '.kmx',
    input: kbd + '.txt'
  )
endforeach
