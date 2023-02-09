# Copyright:    © SIL International.
# Description:  Cross platform build script to compile kmkbpldml API unit tests.
# Create Date:  5 Aug 2022
# Authors:      Marc Durdin
#

tests = [
# disabling 000 until we have updates to core or to the keyboard so that it passes
#  'k_000_null_keyboard',
  'k_001_tiny',
  'k_002_tinyu32',
  'k_003_transform',
  'k_004_tinyshift',
  'k_010_mt',
  'k_011_mt_iso',
  'k_100_keytest',
  'k_101_keytest',
  'k_102_keytest',
  'k_103_vkey',
]

# Setup kmc

kmc_root = join_paths(meson.source_root(),'..','developer','src','kmc')
kmc_cmd = [node, kmc_root]

# Build all keyboards in output folder

foreach kbd : tests
  configure_file(
    command: copy_cmd + ['@INPUT@', '@OUTPUT@'],
    input: kbd + '.xml',
    output: kbd + '.xml'
  )

  configure_file(
    command: kmc_cmd + ['@INPUT@', '--out-file', '@OUTPUT@'],
    output: kbd + '.kmx',
    input: kbd + '.xml'
  )
endforeach
