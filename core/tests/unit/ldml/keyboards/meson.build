# Copyright:    © SIL International.
# Description:  Cross platform build script to compile kmcoreldml API unit tests.
# Create Date:  5 Aug 2022
# Authors:      Marc Durdin
#


# keyboards in resources/standards-data/ldml-keyboards/45/3.0/
# tests     in resources/standards-data/ldml-keyboards/45/test/
tests_from_cldr = [
  'ja-Latn',
  'pt-t-k0-abnt2',
  'fr-t-k0-test',
  'pcm',
  'bn',
]

# these have 'embedded' (@@) testdata instead of a separate file
tests_without_testdata = [
# disabling 000 until we have updates to core or to the keyboard so that it passes
#  'k_000_null_keyboard',
  'k_002_tinyu32',
  'k_003_transform',
  'k_004_tinyshift',
  'k_005_modbittest',
  'k_010_mt',
  'k_011_mt_iso',
  'k_012_other',
  'k_030_transform_plus',
  'k_100_keytest',
  'k_101_keytest',
  'k_102_keytest',
]

# These tests have a *-test.xml file as well.
tests_with_testdata = [
  'ldml_test',
  'k_001_tiny',
  'k_006_backspace',
  'k_007_transform_rgx',
  'k_008_transform_norm',
  'k_009_transform_nfc',
  'k_020_fr', # TODO-LDML: move to cldr above (fix vkey)
  'k_200_reorder_nod_Lana',
  'k_201_reorder_esk',
  'k_210_marker',
  'k_211_marker_escape',
  'k_212_marker_11057',
]

tests =  tests_without_testdata
tests += tests_with_testdata
tests_needing_copy = tests # not including cldr

tests += tests_from_cldr

# Setup kmc

kmc_root = meson.global_source_root() / '../developer/src/kmc'
ldml_root = meson.global_source_root() / '../resources/standards-data/ldml-keyboards/45'
ldml_data = join_paths(ldml_root, '3.0')
ldml_testdata = join_paths(ldml_root, 'test')
kmc_cmd = [node, '--enable-source-maps', kmc_root]

# Build all keyboards in output folder

foreach kbd : tests_needing_copy
  configure_file(
    copy: true,
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
    command: kmc_cmd + ['build', 'ldml-test-data', '@INPUT@', '--out-file', '@OUTPUT@'],
    input: kbd + '-test.xml',
    output: kbd + '-test.json',
  )
endforeach


foreach kbd : tests_from_cldr
  configure_file(
    copy: true,
    input: ldml_data / kbd + '.xml',
    output: kbd + '.xml'
  )
  configure_file(
    command: kmc_cmd + ['build', '@INPUT@', '--out-file', '@OUTPUT@'],
    input: join_paths(ldml_data, kbd + '.xml'),
    output: kbd + '.kmx',
  )
  configure_file(
    command: kmc_cmd + ['build', 'ldml-test-data', '@INPUT@', '--out-file', '@OUTPUT@'],
    input: join_paths(ldml_testdata, kbd + '-test.xml'),
    output: kbd + '-test.json',
  )
endforeach
