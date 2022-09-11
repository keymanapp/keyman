# Copyright:    © SIL International.
# Description:  Cross platform build script to copy invalid keyboards for unit tests.
# Create Date:  5 Aug 2022
# Authors:      Marc Durdin
#

invalid_tests = [
  'ik_000_null_invalid'
]

if build_machine.system() == 'windows'
  copy_cmd = [find_program('cmd.exe', required: true), '/c', 'copy']
else
  copy_cmd = [find_program('cp', required: true)]
endif

# Build all keyboards in output folder

foreach kbd : invalid_tests
  configure_file(
    command: copy_cmd + ['@INPUT@', '@OUTPUT@'],
    input: kbd + '.xml',
    output: kbd + '.xml'
  )

  configure_file(
    command: hextobin_cmd + ['@INPUT@', '@OUTPUT@'],
    output: kbd + '.kmx',
    input: kbd + '.txt'
  )
endforeach
