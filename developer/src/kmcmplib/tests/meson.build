#
# Keyman is copyright (C) SIL International. MIT License.
#
# Cross platform build script to compile and run tests for kmcompx
#

fs = import('fs')

tests_links = []

if cpp_compiler.get_id() == 'emscripten'
  tests_links += ['-lnodefs.js']
endif

input_path = meson.current_source_dir() / '../../../../common/test/keyboards/baseline'
output_path = meson.current_build_dir()

kmcompxtest = executable('kmcompxtest', ['kmcompxtest.cpp','util_filesystem.cpp','util_callbacks.cpp'],
    cpp_args: defns + flags,
    include_directories: inc,
    name_suffix: name_suffix,
    link_args: links + tests_links,
    objects: lib.extract_all_objects(),
    dependencies: icuuc_dep,
  )

# Test keyboards that we have in Core

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
  'k_048___modifier_keys_keep_context'
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

common_test_files = [ meson.source_root() / '../../../common/include/test_color.cpp' ]

# Test the API endpoints

apitest = executable('api-test', ['api-test.cpp','util_filesystem.cpp','util_callbacks.cpp', common_test_files],
    cpp_args: defns + flags,
    include_directories: inc,
    name_suffix: name_suffix,
    link_args: links + tests_links,
    objects: lib.extract_all_objects(),
    dependencies: icuuc_dep
  )

test('api-test', apitest, args: [output_path / 'blank_keyboard.kmx'])

usetapitest = executable('uset-api-test', 'uset-api-test.cpp', common_test_files,
    cpp_args: defns + flags,
    include_directories: inc,
    name_suffix: name_suffix,
    link_args: links + tests_links,
    objects: lib.extract_all_objects(),
    dependencies: icuuc_dep,
  )

test('uset-api-test', usetapitest)

# Google Test

gtestcompmsgtest = executable('gtest-compmsg-test', 'gtest-compmsg-test.cpp',
    cpp_args: defns + flags,
    include_directories: inc,
    name_suffix: name_suffix,
    link_args: links + tests_links,
    objects: lib.extract_all_objects(),
    dependencies: [ icuuc_dep, gtest_dep, gmock_dep ],
  )

test('gtest-compmsg-test', gtestcompmsgtest)
