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
  'k_0000___null_keyboard',
  'k_0001___nul',
  'k_0002___nul_with_initial_context',
  'k_0003___nul_testing',
  'k_0004___space_mnemonic_kbd',
  'k_0005___beep',
  'k_0100___basic_input_unicodei',
  'k_0101___basic_input_unicode',
  'k_0102___basic_input__shift_2_',
  'k_0103___vkey_input__shift_ctrl_',
  'k_0104___vkey_input__ctrl_alt_',
  'k_0105___vkey_input__ctrl_alt_2_',
  'k_0106___smp',
  'k_0107___punctkeys',
  'k_0200___ralt',
  'k_0201___ralt_2',
  'k_0203___generic_ctrlalt',
  'k_0300___deadkeys',
  'k_0301___multiple_deadkeys',
  'k_0302___deadkeys_and_backspace',
  'k_0400___groups_and_virtual_keys',
  'k_0401___multiple_groups',
  'k_0402___output_and_keystroke',
  'k_0500___options',
  'k_0501___options_with_preset',
  'k_0502___options_with_save',
  'k_0503___options_with_save_and_preset',
  'k_0504___options_with_reset',
  'k_0505___options_double_set_reset',
  'k_0506___options_double_set_staged',
  'k_0507___options___double_reset_staged',
  'k_0508___options___double_reset',
  'k_0600___system_stores',
  'k_0601___system_stores_2',
  'k_0700___caps_lock',
  'k_0701___caps_control',
  'k_0702___caps_always_off',
  'k_0703___caps_always_off_initially_on',
  'k_0800___long_context',
  'k_0801___long_context_and_deadkeys',
  'k_0802___long_context_and_split_deadkeys',
  'k_0803___if_and_context',
  'k_0804___deadkey_and_context',
  'k_0805___deadkey_and_contextex',
  'k_0806___modifier_keys_keep_context',
  'k_0807___enter_invalidates_context',
  'k_0808___nul_and_context',
  'k_0809___if_and_context',
  'k_0810___nul_and_index',
  'k_0811___if_and_index',
  'k_0812___nul_and_contextex',
  # TODO-web-core: Skipped: 'k_0813___deadkey_cancelled_by_arrow',
]


node = find_program('node', required: true)

kmc_root = meson.current_source_dir() / '../../../../developer/src/kmc/build/src/kmc.js'
kmc_cmd = [node, '--enable-source-maps', kmc_root]

if build_machine.system() == 'windows'
  cat_cmd = [find_program(meson.current_source_dir() / 'cat.bat', required: true)]
else
  cat_cmd = [find_program('cat', required: true)]
endif

test_keyboards_kpj = ''
test_keyboards_kmp_json = ''
foreach kbd : tests
  if test_keyboards_kmp_json.startswith('{')
    test_keyboards_kmp_json += ''',
  '''
  endif
  test_keyboards_kmp_json +=  '''{
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

  test_keyboards_kpj += '''
    <File>
      <ID>keyboard_'''+kbd+'''</ID>
      <Filename>'''+kbd+'''.kmn</Filename>
      <Filepath>'''+kbd+'''.kmn</Filepath>
    </File>
    <File>
      <ID>package_'''+kbd+'''</ID>
      <Filename>'''+kbd+'''.kps</Filename>
      <Filepath>'''+kbd+'''.kps</Filepath>
    </File>
  '''
endforeach

cfg = configuration_data()
cfg.set('keyboards', test_keyboards_kmp_json)
cfg.set('keyboards_kpj', test_keyboards_kpj)

# Build a single kmp.json which lists all compiled keyboards
configure_file(
  configuration: cfg,
  input: 'kmp.json.in',
  output: 'kmp.json'
)

# Build a single keyboards.kpj which lists all keyboard + package sources
keyboards_kpj = configure_file(
  configuration: cfg,
  input: 'keyboards.kpj.in',
  output: 'keyboards.kpj'
)

all_keyboards = custom_target('all_keyboards',
  input: keyboards_kpj,
  output: [tests[0]+'.kmp'],
  depfile: 'keyboards.dep',
  command: kmc_cmd + ['build', '--debug', '--no-compiler-version', '@INPUT@']
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
    # not really a file that needs to be configured, but the `copy` parameter
    # is deprecated and `fs.copyfile` doesn't work with files that are not in
    # the source directory
    configuration: cfg,
    input: kbd_src_path,
    output: kbd + '.kmn'
  )
  kbd_kps = configure_file(
    configuration: cfg,
    input: join_paths('template', 'keyboard.kps.in'),
    output: kbd + '.kps'
  )
endforeach

foreach kbd : tests
  test(kbd, kmx, depends: [all_keyboards], args: [test_path / kbd + '.kmn', test_path / kbd + '.kmx'])
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

test_kbd = 'k_0702___caps_always_off'


kbd_src = common_test_keyboards_baseline + '/' + test_kbd + '.kmn'
kbd_obj = join_paths(meson.current_build_dir(), test_kbd) + '.kmx'
kbd_log = custom_target(test_kbd + '.kmx'.underscorify(),
  output: test_kbd + '.log',
  input: kbd_src,
  command: kmc_cmd + ['build', '--debug', '--no-compiler-version', '@INPUT@', '--out-file', kbd_obj]
)
test('ext_event', external_e,  depends: kbd_log, args: [kbd_obj] )
