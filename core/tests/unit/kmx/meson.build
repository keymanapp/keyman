#
# Keyman is copyright (C) SIL Global. MIT License.
#
# Created by Tim Eves (TSE) on 2018-10-19
#
# Cross platform build script to compile kmx processor unit tests.
#

kmx_test_path = meson.current_build_dir()

kmx_tests_executable = executable('kmx_tests',
    ['kmx.tests.cpp', common_test_files_without_main],
    cpp_args: defns + test_warns,
    include_directories: [test_includes, '../../kmx_test_source'],
    link_args: links + test_link_args,
    dependencies: test_dependencies,
    objects: [lib.extract_all_objects(recursive: false), kmx_test_source_lib.extract_all_objects(recursive: false)])

kmx_tests = [
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
  'k_0202___alt',
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

if build_machine.system() == 'windows'
  cat_cmd = [find_program(meson.current_source_dir() / 'cat.bat', required: true)]
else
  cat_cmd = [find_program('cat', required: true)]
endif

test_keyboards_kpj = ''
test_keyboards_kmp_json = ''
foreach kbd : kmx_tests
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
  output: [kmx_tests[0]+'.kmp'],
  depfile: 'keyboards.dep',
  command: kmc_cmd + ['build', '--debug', '--no-compiler-version', '@INPUT@']
)

foreach kbd : kmx_tests
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

foreach kbd : kmx_tests
  test('kmx/' + kbd, kmx_tests_executable, depends: [all_keyboards], args: [kmx_test_path / kbd + '.kmn', kmx_test_path / kbd + '.kmx'], suite: 'kmx', protocol: 'gtest')
endforeach

# binary file unit tests

subdir('fixtures')

