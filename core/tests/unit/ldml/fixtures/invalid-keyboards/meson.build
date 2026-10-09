# Keyman is copyright (C) SIL Global. MIT License.

ldml_invalid_tests = [
  'ik_000_null_invalid',
  'ik_001_bad_key',
  'ik_002_no_keys',
]

# Build all keyboards in output folder

foreach kbd : ldml_invalid_tests
  configure_file(
    input: kbd + '.xml',
    output: kbd + '.xml',
    copy: true
  )
#   fs.copyfile(kbd + '.xml', kbd + '.xml')

  configure_file(
    command: hextobin_cmd + ['@INPUT@', '@OUTPUT@'],
    output: kbd + '.kmx',
    input: kbd + '.txt'
  )
endforeach
