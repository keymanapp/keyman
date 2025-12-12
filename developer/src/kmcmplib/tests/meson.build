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
  'k_0000___null_keyboard.kmn',
  'k_0001___nul.kmn',
  'k_0002___nul_with_initial_context.kmn',
  'k_0003___nul_testing.kmn',
  'k_0004___space_mnemonic_kbd.kmn',
  'k_0005___beep.kmn',
  'k_0100___basic_input_unicodei.kmn',
  'k_0101___basic_input_unicode.kmn',
  'k_0102___basic_input__shift_2_.kmn',
  'k_0103___vkey_input__shift_ctrl_.kmn',
  'k_0104___vkey_input__ctrl_alt_.kmn',
  'k_0105___vkey_input__ctrl_alt_2_.kmn',
  'k_0106___smp.kmn',
  'k_0107___punctkeys.kmn',
  'k_0200___ralt.kmn',
  'k_0201___ralt_2.kmn',
  'k_0203___generic_ctrlalt.kmn',
  'k_0300___deadkeys.kmn',
  'k_0301___multiple_deadkeys.kmn',
  'k_0302___deadkeys_and_backspace.kmn',
  'k_0400___groups_and_virtual_keys.kmn',
  'k_0401___multiple_groups.kmn',
  'k_0402___output_and_keystroke.kmn',
  'k_0500___options.kmn',
  'k_0501___options_with_preset.kmn',
  'k_0502___options_with_save.kmn',
  'k_0503___options_with_save_and_preset.kmn',
  'k_0504___options_with_reset.kmn',
  'k_0505___options_double_set_reset.kmn',
  'k_0506___options_double_set_staged.kmn',
  'k_0507___options___double_reset_staged.kmn',
  'k_0508___options___double_reset.kmn',
  'k_0600___system_stores.kmn',
  'k_0601___system_stores_2.kmn',
  'k_0700___caps_lock.kmn',
  'k_0701___caps_control.kmn',
  'k_0702___caps_always_off.kmn',
  'k_0703___caps_always_off_initially_on.kmn',
  'k_0800___long_context.kmn',
  'k_0801___long_context_and_deadkeys.kmn',
  'k_0802___long_context_and_split_deadkeys.kmn',
  'k_0803___if_and_context.kmn',
  'k_0804___deadkey_and_context.kmn',
  'k_0805___deadkey_and_contextex.kmn',
  'k_0806___modifier_keys_keep_context.kmn',
  'k_0807___enter_invalidates_context.kmn',
  'k_0808___nul_and_context.kmn',
  'k_0809___if_and_context.kmn',
  'k_0810___nul_and_index.kmn',
  'k_0811___if_and_index.kmn',
  'k_0812___nul_and_contextex.kmn',
  'k_0813___deadkey_cancelled_by_arrow.kmn',
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
