# Copyright:    © SIL International.
# Description:  Cross platform build script to compile kmkbpldml API unit tests.
# Create Date:  5 Aug 2022
# Authors:      Marc Durdin
#

tests = [
# disabling 000 until we have updates to core or to the keyboard so that it passes
#  'k_000_null_keyboard',
  'k_001_tiny'
]

# Setup kmldmlc

kmldmlc_root = join_paths(meson.source_root(),'..','developer','src','kmldmlc')
kmldmlc_cmd = [node, kmldmlc_root]

# Build all keyboards in output folder

foreach kbd : tests
  configure_file(
    command: copy_cmd + ['@INPUT@', '@OUTPUT@'],
    input: kbd + '.xml',
    output: kbd + '.xml'
  )

  configure_file(
    command: kmldmlc_cmd + ['@INPUT@', '--outFile', '@OUTPUT@'],
    output: kbd + '.kmx',
    input: kbd + '.xml'
  )
endforeach
