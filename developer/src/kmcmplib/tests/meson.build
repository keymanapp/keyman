#
# Keyman is copyright (C) SIL International. MIT License.
#
# Cross platform build script to compile and run tests for kmcompx
#

fs = import('fs')

common_test_files = [ meson.global_source_root() / '../../../common/include/test_color.cpp' ]

tests_links = []

if cpp_compiler.get_id() == 'emscripten'
  tests_links += ['-lnodefs.js']
endif

input_path = meson.current_source_dir() / '../../../../common/test/keyboards/baseline'
output_path = meson.current_build_dir()

kmcompxtest = executable('kmcompxtests', ['kmcompx.tests.cpp','util_filesystem.cpp','util_callbacks.cpp',common_test_files],
    cpp_args: defns + flags,
    include_directories: inc,
    name_suffix: name_suffix,
    link_args: links + tests_links,
    objects: lib.extract_all_objects(recursive: false),
    dependencies: icuuc_dep,
  )

# Test keyboards that we have in Core

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
  'k_0813___deadkey_cancelled_by_arrow',
]

foreach kbd : tests
  kbd_src = join_paths(input_path, kbd) + '.kmn'
  kbd_obj = join_paths(output_path, kbd) + '.kmx'
  test(kbd, kmcompxtest, args: [kbd_src, kbd_obj, join_paths(input_path, kbd) + '.kmx'])
endforeach

valid_keyboard_tests = [
  'k001_utf16',
  'k002_utf8_without_bom',
  'k003_utf8_with_bom',
  'k004_ansi',
  'k005_bitmap',
  'k006_icon',
  'k007_includecodes_r_n',
  'k008_includecodes_n',
  'k009_long_lines',
]

fixtures_path = meson.current_source_dir() / 'fixtures/valid-keyboards'

foreach kbd : valid_keyboard_tests
  kbd_src = join_paths(fixtures_path, kbd) + '.kmn'
  kbd_obj = join_paths(output_path, kbd) + '.kmx'
  test(kbd, kmcompxtest, args: [kbd_src, kbd_obj, join_paths(fixtures_path, kbd) + '.kmx'])
endforeach

# Test fixtures that come from keyboards repo -- but only for a "full" test,
# which typically we run on CI no more than once a day, because it's expensive.

if get_option('full_test')

  keyboards_root = meson.current_source_dir() / 'keyboards'

  if build_machine.system() == 'windows'
    test_source = run_command('run-shell.bat', 'get-test-source.sh', keyboards_root, capture: true, check: true)
  else
    test_source = run_command('get-test-source.sh', keyboards_root, capture: true, check: true)
  endif

  tests = test_source.stdout().strip().split('\n')

  foreach kbd : tests
    test_basename = fs.replace_suffix(fs.name(kbd), '')

    # kbd is going to be an absolute path
    kbd_obj = output_path / test_basename + '.kmx'
    reference_kmx = meson.current_source_dir() / 'fixtures/keyboards-repo' / test_basename + '.kmx'

    if fs.is_file(reference_kmx)
      test(test_basename, kmcompxtest, args: [kbd, kbd_obj, reference_kmx])
    endif
  endforeach

endif

# Test the API endpoints

apitest = executable('api-tests', ['api.tests.cpp','util_filesystem.cpp','util_callbacks.cpp', common_test_files],
    cpp_args: defns + flags,
    include_directories: inc,
    name_suffix: name_suffix,
    link_args: links + tests_links,
    objects: lib.extract_all_objects(recursive: false),
    dependencies: icuuc_dep
  )

test('api-tests', apitest, args: [output_path / 'blank_keyboard.kmx'])

usetapitest = executable('uset-api-tests', 'uset-api.tests.cpp', common_test_files,
    cpp_args: defns + flags,
    include_directories: inc,
    name_suffix: name_suffix,
    link_args: links + tests_links,
    objects: lib.extract_all_objects(recursive: false),
    dependencies: icuuc_dep,
  )

test('uset-api-tests', usetapitest)

# Google Test

gtestcompilertest = executable('gtest-compiler-tests', [
    'gtest-compiler.tests.cpp',
    'gtest-validation.tests.cpp',
  ],
    cpp_args: defns + flags,
    include_directories: inc,
    name_suffix: name_suffix,
    link_args: links + tests_links,
    objects: lib.extract_all_objects(recursive: false),
    dependencies: [ icuuc_dep, gtest_dep, gmock_dep ],
  )

test('gtest-compiler-tests', gtestcompilertest)

gtest_km_u16_test = executable('gtest-km_u16-tests', 'gtest-km_u16.tests.cpp',
    cpp_args: defns + flags,
    include_directories: inc,
    name_suffix: name_suffix,
    link_args: links + tests_links,
    objects: lib.extract_all_objects(recursive: false),
    dependencies: [ icuuc_dep, gtest_dep, gmock_dep ],
  )

test('gtest-km_u16-tests', gtest_km_u16_test)
