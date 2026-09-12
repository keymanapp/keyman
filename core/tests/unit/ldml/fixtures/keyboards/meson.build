# Keyman is copyright (C) SIL Global. MIT License.

# keyboards in resources/standards-data/ldml-keyboards/46/3.0/
# tests     in resources/standards-data/ldml-keyboards/46/test/
ldml_tests_from_cldr = [
  'ja-Latn',
  'pt-t-k0-abnt2',
  'fr-t-k0-test',
  'pcm',
  'bn',
]

# these have 'embedded' (@@) testdata instead of a separate file
ldml_tests_without_testdata = [
  'k_000_minimal_keyboard',
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
  'k_213_backspace_decomposition',
]

# These tests have a *-test.xml file as well.
ldml_tests_with_testdata = [
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

ldml_tests =  ldml_tests_without_testdata
ldml_tests += ldml_tests_with_testdata
ldml_tests_needing_copy = ldml_tests # not including cldr

ldml_tests += ldml_tests_from_cldr

# Setup kmc

ldml_root = meson.global_source_root() / '../resources/standards-data/ldml-keyboards/46'
ldml_data = join_paths(ldml_root, '3.0')
ldml_testdata = join_paths(ldml_root, 'test')

# Build all keyboards in output folder

foreach kbd : ldml_tests_needing_copy
  # This will output a (wrong) deprecation warning with meson < 1.5 - see #13250
  configure_file(input: kbd + '.xml', output: kbd + '.xml', copy: true)
endforeach

foreach kbd : ldml_tests_with_testdata
  configure_file(
    command: kmc_cmd + ['build', 'ldml-test-data', '@INPUT@', '--out-file', '@OUTPUT@'],
    input: kbd + '-test.xml',
    output: kbd + '-test.json',
  )
endforeach

foreach kbd : ldml_tests_from_cldr
  # This will output a (wrong) deprecation warning with meson < 1.5 - see #13250
  configure_file(input: ldml_data / kbd + '.xml', output: kbd + '.xml', copy: true)
  configure_file(
    command: kmc_cmd + ['build', 'ldml-test-data', '@INPUT@', '--out-file', '@OUTPUT@'],
    input: join_paths(ldml_testdata, kbd + '-test.xml'),
    output: kbd + '-test.json',
  )
endforeach

# Build a single keyboards.kpj which lists all keyboard sources

test_ldml_kmx_targets = ldml_tests_needing_copy + ldml_tests_from_cldr

test_ldml_keyboards_kpj = ''
foreach kbd : test_ldml_kmx_targets
  test_ldml_keyboards_kpj += '''
    <File>
      <ID>keyboard_'''+kbd+'''</ID>
      <Filename>'''+kbd+'''.xml</Filename>
      <Filepath>'''+kbd+'''.xml</Filepath>
    </File>
  '''
endforeach

cfg = configuration_data()
cfg.set('keyboards_kpj', test_ldml_keyboards_kpj)

# Build v17.0 keyboard fixtures

ldml_keyboards_kpj_17 = configure_file(
  configuration: cfg,
  input: 'keyboards-17.kpj.in',
  output: 'keyboards-17.kpj'
)

configure_file(
  input: ldml_keyboards_kpj_17,
  # note that this does not actually rewrite the file in this location; it goes
  # into the 17.0/ folder
  output: ldml_tests_from_cldr[0]+'.kmx',
  command: kmc_cmd + ['build', '--target-version', '17', '@INPUT@']
)

# Build v19.0 keyboard fixtures

ldml_keyboards_kpj_19 = configure_file(
  configuration: cfg,
  input: 'keyboards-19.kpj.in',
  output: 'keyboards19.kpj'
)

configure_file(
  input: ldml_keyboards_kpj_19,
  # note that this does not actually rewrite the file in this location; it goes
  # into the 19.0/ folder. We use [1] here so that we don't get a spurious
  # warning about conflict with the 17.0 configure step above
  output: ldml_tests_from_cldr[1]+'.kmx',
  command: kmc_cmd + ['build', '--target-version', '19', '@INPUT@']
)
