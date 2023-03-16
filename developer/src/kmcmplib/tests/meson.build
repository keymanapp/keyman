#
# Keyman is copyright (C) SIL International. MIT License.
#
# Cross platform build script to compile and run tests for kmcompx
#

fs = import('fs')

tests_flags = []

input_path = meson.current_source_dir() / '../../../../core/tests/unit/kmx'
output_path = meson.current_build_dir()

kmcompxtest = executable('kmcompxtest', 'kmcompxtest.cpp',
    cpp_args: defns,
    include_directories: inc,
    link_args: links + tests_flags,
    objects: lib.extract_all_objects())

# Test keyboards that we have in Core

tests = [
  '000 - null keyboard',
  '001 - basic input UnicodeI',
  '002 - basic input Unicode',
  '003 - nul',
  '004 - basic input (shift 2)',
  '005 - nul with initial context',
  '006 - vkey input (shift ctrl)',
  '007 - vkey input (ctrl alt)',
  '008 - vkey input (ctrl alt 2)',
  '012 - ralt',
  '013 - deadkeys',
  '014 - groups and virtual keys',
  '015 - ralt 2',
  '017 - space mnemonic kbd',
  '018 - nul testing',
  '019 - multiple deadkeys',
  '020 - deadkeys and backspace',

  '021 - options',
  '022 - options with preset',
  '023 - options with save',
  '024 - options with save and preset',
  '025 - options with reset',

  '026 - system stores',
  '027 - system stores 2',

  '028 - smp',
  '029 - beep',

  '030 - multiple groups',
  '031 - caps lock',
  '032 - caps control',
  '033 - caps always off',
  '034 - options double set reset',
  '035 - options double set staged',
  '036 - options - double reset staged',
  '037 - options - double reset',
  '038 - punctkeys',
  '039 - generic ctrlalt',
  '040 - long context',
  '041 - long context and deadkeys',
  '042 - long context and split deadkeys',
  '043 - output and keystroke',

  '044 - if and context',
  '045 - deadkey and context',
  '046 - deadkey and contextex',
  '047 - caps always off initially on',
  '048 - modifier keys keep context'
]

foreach kbd : tests
  kbd_src = join_paths(input_path, kbd) + '.kmn'
  kbd_obj = join_paths(output_path, kbd) + '.kmx'
  test(kbd, kmcompxtest, args: [kbd_src, kbd_obj, join_paths(input_path, kbd) + '.kmx'])
endforeach

# Test fixtures that come from keyboards repo

keyboards_root = meson.current_source_dir() / 'keyboards'

if build_machine.system() == 'windows'
  test_source = run_command('run-shell.bat', 'get-test-source.inc.sh', keyboards_root, capture: true, check: true)
else
  test_source = run_command('get_test_source.inc.sh', keyboards_root, capture: true, check: true)
endif

tests = test_source.stdout().strip().split('\n')

foreach kbd : tests
  test_basename = fs.replace_suffix(fs.name(kbd), '')

  # kbd is going to be an absolute path
  kbd_obj = output_path / test_basename + '.kmx'
  reference_kmx = meson.current_source_dir() / 'fixtures' / test_basename + '.kmx'

  if fs.is_file(reference_kmx)
    test(test_basename, kmcompxtest, args: [kbd, kbd_obj, reference_kmx])
  endif
endforeach

