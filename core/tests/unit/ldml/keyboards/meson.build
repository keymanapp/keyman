# Copyright:    © SIL International.
# Description:  Cross platform build script to compile kmkbpldml API unit tests.
# Create Date:  5 Aug 2022
# Authors:      Marc Durdin
#


# keyboards in resources/standards-data/ldml-keyboards/techpreview/3.0/
# tests     in resources/standards-data/ldml-keyboards/techpreview/test/
tests_from_cldr = [
# TODO-LDML: These don't work on Windows yet due to path issues=> <https://github.com/keymanapp/keyman/issues/8524>
  # 'ja-Latn',
  # 'pt-BR-k0-abnt2',
  # 'fr-t-k0-azerty', # vkey issues
]

tests_without_testdata = [
# disabling 000 until we have updates to core or to the keyboard so that it passes
#  'k_000_null_keyboard',
  'k_002_tinyu32',
  'k_003_transform',
  'k_004_tinyshift',
  'k_005_modbittest',
  'k_010_mt',
  'k_011_mt_iso',
  'k_100_keytest',
  'k_101_keytest',
  'k_102_keytest',
]

# These tests have a k_001_tiny-test.xml file as well.
tests_with_testdata = [
  'k_001_tiny',
  # TODO-LDML: fix the path issue=> <https://github.com/keymanapp/keyman/issues/8524> and move these to tests_from_cldr
  'fr-t-k0-azerty',
  'ja-Latn',
  'pt-BR-k0-abnt2',
]

tests =  tests_without_testdata
tests += tests_with_testdata
tests_needing_copy = tests # not including cldr

tests += tests_from_cldr

# Setup kmc

kmc_root = join_paths(meson.source_root(),'..','developer','src','kmc')
ldml_root = join_paths(meson.source_root(),'..','resources','standards-data','ldml-keyboards','techpreview')
ldml_data = join_paths(ldml_root, '3.0')
ldml_testdata = join_paths(ldml_root, 'test')
kmc_cmd = [node, kmc_root]

# Build all keyboards in output folder

foreach kbd : tests_needing_copy
  configure_file(
    command: copy_cmd + ['@INPUT@', '@OUTPUT@'],
    input: kbd + '.xml',
    output: kbd + '.xml'
  )

  configure_file(
    command: kmc_cmd + ['build', '@INPUT@', '--out-file', '@OUTPUT@'],
    input: kbd + '.xml',
    output: kbd + '.kmx',
  )
endforeach

foreach kbd : tests_with_testdata
  configure_file(
    command: kmc_cmd + ['build-test-data', '@INPUT@', '--out-file', '@OUTPUT@'],
    input: kbd + '-test.xml',
    output: kbd + '-test.json',
  )
endforeach


foreach kbd : tests_from_cldr
  configure_file(
    command: copy_cmd + ['@INPUT@', '@OUTPUT@'],
    input: join_paths(ldml_data, kbd + '.xml'),
    output: kbd + '.xml'
  )
  configure_file(
    command: kmc_cmd + ['build', '@INPUT@', '--out-file', '@OUTPUT@'],
    input: join_paths(ldml_data, kbd + '.xml'),
    output: kbd + '.kmx',
  )
  configure_file(
    command: kmc_cmd + ['build-test-data', '@INPUT@', '--out-file', '@OUTPUT@'],
    input: join_paths(ldml_testdata, kbd + '-test.xml'),
    output: kbd + '-test.json',
  )
endforeach
