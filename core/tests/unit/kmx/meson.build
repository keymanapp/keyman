# Copyright:    © 2018 SIL International.
# Description:  Cross platform build script to compile libkeymancore API unit tests.
# Create Date:  19 Oct 2018
# Authors:      Marc Durdin, Tim Eves (TSE)
# History:      19  Oct 2018 - TSE - Added test for context API functions.
#

# TODO -- why are these differing from the standard.meson.build flags?
if cpp_compiler.get_id() == 'gcc' or cpp_compiler.get_id() == 'clang' or cpp_compiler.get_id() == 'emscripten'
  warns = [
     '-Wno-missing-field-initializers',
     '-Wno-unused-parameter'
  ]
else
  warns = []
endif

source_path = meson.current_source_dir()
test_path = meson.current_build_dir()
tests_flags = []
common_test_keyboards_baseline = source_path / '../../../../common/test/keyboards/baseline'

if cpp_compiler.get_id() == 'emscripten'
  # TODO: we will move the tests to ES6 in the future
  # '-sMODULARIZE', '-sEXPORT_ES6',
  tests_flags += ['-lnodefs.js', wasm_exported_runtime_methods]
endif

kmx = executable('kmx',
    ['kmx.cpp', common_test_files],
    cpp_args: defns + warns,
    include_directories: [inc, libsrc, '../../kmx_test_source'],
    link_args: links + tests_flags,
    dependencies: [icu_uc, icu_i18n],
    objects: [lib.extract_all_objects(recursive: false), kmx_test_source_lib.extract_all_objects(recursive: false)])

tests = [
  'k_000___null_keyboard',
  'k_001___basic_input_unicodei',
  'k_002___basic_input_unicode',
  'k_003___nul',
  'k_004___basic_input__shift_2_',
  'k_005___nul_with_initial_context',
  'k_006___vkey_input__shift_ctrl_',
  'k_007___vkey_input__ctrl_alt_',
  'k_008___vkey_input__ctrl_alt_2_',
  'k_012___ralt',
  'k_013___deadkeys',
  'k_014___groups_and_virtual_keys',
  'k_015___ralt_2',
  'k_017___space_mnemonic_kbd',
  'k_018___nul_testing',
  'k_019___multiple_deadkeys',
  'k_020___deadkeys_and_backspace',
  'k_021___options',
  'k_022___options_with_preset',
  'k_023___options_with_save',
  'k_024___options_with_save_and_preset',
  'k_025___options_with_reset',
  'k_026___system_stores',
  'k_027___system_stores_2',
  'k_028___smp',
  'k_029___beep',
  'k_030___multiple_groups',
  'k_031___caps_lock',
  'k_032___caps_control',
  'k_033___caps_always_off',
  'k_034___options_double_set_reset',
  'k_035___options_double_set_staged',
  'k_036___options___double_reset_staged',
  'k_037___options___double_reset',
  'k_038___punctkeys',
  'k_039___generic_ctrlalt',
  'k_040___long_context',
  'k_041___long_context_and_deadkeys',
  'k_042___long_context_and_split_deadkeys',
  'k_043___output_and_keystroke',
  'k_044___if_and_context',
  'k_045___deadkey_and_context',
  'k_046___deadkey_and_contextex',
  'k_047___caps_always_off_initially_on',
  'k_048___modifier_keys_keep_context',
  'k_049___enter_invalidates_context'
]

node = find_program('node', required: true)

kmc_root = meson.current_source_dir() / '../../../../developer/src/kmc/build/src/kmc.js'
kmc_cmd = [node, '--enable-source-maps', kmc_root]

if build_machine.system() == 'windows'
  cat_cmd = [find_program(meson.current_source_dir() / 'cat.bat', required: true)]
else
  cat_cmd = [find_program('cat', required: true)]
endif

test_keyboards = ''
foreach kbd : tests
  if test_keyboards.startswith('{')
    test_keyboards += ''',
  '''
  endif
  test_keyboards +=  '''{
    "name": "''' + kbd + '''",
    "id": "''' + kbd + '''",
    "version": "0.0",
    "languages": [
      {
        "name": "Undetermined",
        "id": "und"
      }
    ]
  }'''
endforeach

cfg = configuration_data()
cfg.set('keyboards', test_keyboards)
configure_file(
  configuration: cfg,
  input: 'kmp.json.in',
  output: 'kmp.json'
)

foreach kbd : tests
  kbd_src_path = common_test_keyboards_baseline / kbd + '.kmn'

  content = run_command(
    cat_cmd, files(kbd_src_path), check: true,
  ).stdout().strip()

  cfg = configuration_data()
  cfg.set('keyboard', kbd)
  cfg.set('keyboard_underscore', kbd)
  cfg.set('id1', 'id_' + kbd + '_1')
  cfg.set('id2', 'id_' + kbd + '_2')
  cfg.set('id3', 'id_' + kbd + '_3')
  cfg.set('build_dir', meson.current_build_dir())
  cfg.set('NAME', kbd)
  cfg.set('CONTENT', content)

  kbd_kmn = configure_file(
    copy: true,
    input: kbd_src_path,
    output: kbd + '.kmn'
  )
  kbd_kpj = configure_file(
    configuration: cfg,
    input: join_paths('template', 'keyboard.kpj.in'),
    output: kbd + '.kpj'
  )
  kbd_kps = configure_file(
    configuration: cfg,
    input: join_paths('template', 'keyboard.kps.in'),
    output: kbd + '.kps'
  )
  kbd_kmp = custom_target(kbd + '.kmp',
    input: kbd_kpj,
    output: [kbd + '.kmp', kbd + '.kmx'],
    depend_files: [kbd_kps, kbd_kmn],
    depfile: kbd + '.dep',
    command: kmc_cmd + ['build', '--debug', '--no-compiler-version', '@INPUT@'])

  test(kbd, kmx, depends: [kbd_kmp], args: [test_path / kbd + '.kmn', test_path / kbd + '.kmx'])
endforeach

# binary file unit tests

subdir('fixtures')

# test for km_core_keyboard_get_key_list

# For following tests, we need to be copying from source_path,
# not build_path, so we'll just update test_path accordingly. This
# should work for Linux, macOS, and WASM.
test_path = source_path

key_e = executable('key_list_tests', ['kmx_key_list.tests.cpp', common_test_files],
                cpp_args: defns + warns,
                include_directories: [inc, libsrc],
                link_args: links + tests_flags,
                dependencies: [icu_uc, icu_i18n],
                objects: lib.extract_all_objects(recursive: false))
test_kbd = 'kmx_key_list'

kbd_src = files(test_kbd + '.kmn')
kbd_obj = join_paths(meson.current_build_dir(), test_kbd) + '.kmx'
kbd_log = custom_target(test_kbd + '.kmx'.underscorify(),
  output: test_kbd + '.log',
  input: kbd_src,
  command: kmc_cmd + ['build', '--debug', '--no-compiler-version', '@INPUT@', '--out-file', kbd_obj]
)
test('key_list', key_e,  depends: kbd_log, args: [kbd_obj] )

# test for imx list

imx_e = executable('imx_list_tests', ['kmx_imx.tests.cpp', common_test_files],
                cpp_args: defns + warns,
                include_directories: [inc, libsrc],
                link_args: links + tests_flags,
                dependencies: [icu_uc, icu_i18n],
                objects: lib.extract_all_objects(recursive: false))

test_kbd = 'kmx_imsample'

kbd_src = files(test_kbd + '.kmn')
kbd_obj = join_paths(meson.current_build_dir(), test_kbd) + '.kmx'
kbd_log = custom_target(test_kbd + '.kmx'.underscorify(),
  output: test_kbd + '.log',
  input: kbd_src,
  command: kmc_cmd + ['build', '--debug', '--no-compiler-version', '@INPUT@', '--out-file', kbd_obj]
)
  test('imx_list', imx_e,  depends: kbd_log, args: [kbd_obj] )

external_e = executable('ext_event_tests', ['kmx_external_event.tests.cpp', common_test_files],
                cpp_args: defns + warns,
                include_directories: [inc, libsrc],
                link_args: links + tests_flags,
                dependencies: [icu_uc, icu_i18n],
                objects: lib.extract_all_objects(recursive: false))

test_kbd = 'k_033___caps_always_off'


kbd_src = common_test_keyboards_baseline + '/' + test_kbd + '.kmn'
kbd_obj = join_paths(meson.current_build_dir(), test_kbd) + '.kmx'
kbd_log = custom_target(test_kbd + '.kmx'.underscorify(),
  output: test_kbd + '.log',
  input: kbd_src,
  command: kmc_cmd + ['build', '--debug', '--no-compiler-version', '@INPUT@', '--out-file', kbd_obj]
)
  test('ext_event', external_e,  depends: kbd_log, args: [kbd_obj] )
