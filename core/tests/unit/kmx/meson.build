# Copyright:    © 2018 SIL International.
# Description:  Cross platform build script to compile libkmnkbp API unit tests.
# Create Date:  19 Oct 2018
# Authors:      Marc Durdin, Tim Eves (TSE)
# History:      19  Oct 2018 - TSE - Added test for context API functions.
#

if compiler.get_id() == 'gcc' or compiler.get_id() == 'clang' or compiler.get_id() == 'emscripten'
  warns = [
     '-Wno-missing-field-initializers',
     '-Wno-unused-parameter'
  ]
else
  warns = []
endif


if compiler.get_id() == 'emscripten'
  tests_flags = ['--embed-file', join_paths(meson.current_source_dir(),'@/')]
  source_path = '/'
  test_path = '/'
else
  tests_flags = []
  source_path = meson.current_source_dir()
  test_path = meson.current_build_dir()
endif

coretestfiles = files(
    'kmx_test_source.cpp',
)

kmx = executable('kmx',
    'kmx.cpp',
    coretestfiles,
    cpp_args: defns + warns,
    include_directories: [inc, libsrc],
    link_args: links + tests_flags,
    objects: lib.extract_all_objects())

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

if build_machine.system() == 'windows'
  kmcomp = find_program(join_paths(meson.source_root(),'..','..','..','windows','bin','developer','kmcomp.exe'), 'kmcomp.exe', required: false)
  kmcomp_cmd = [kmcomp]
  copy_cmd = [find_program('cmd.exe', required: true), '/c', 'copy']
  cat_cmd = [find_program('cmd.exe', required: true), '/c', 'type']
else
  kmcomp = find_program('kmcomp', required: false)
  kmcomp_cmd = [kmcomp]
  if not kmcomp.found()
    wine = find_program('wine', required: false)
    kmcomp = find_program('kmcomp.exe', required: false)
    kmcomp_cmd = [wine, kmcomp]
    if not wine.found()
      kmcomp = disabler()
    endif
  endif
  copy_cmd = [find_program('cp', required: true)]
  cat_cmd = [find_program('cat', required: true)]
endif

if compiler.get_id() == 'emscripten'
  # The .kmx files are linked in to the unit test,
  # so kmcomp is too late here
  kmcomp = disabler()
endif

test_keyboards = ''
foreach kbd : tests
  if test_keyboards.startswith('{')
    test_keyboards += ''',
  '''
  endif
  kbd_basename = 'k_' + kbd.underscorify().to_lower()
  test_keyboards +=  '''{
    "name": "''' + kbd_basename + '''",
    "id": "''' + kbd_basename + '''",
    "version": "0.0",
    "languages": [
      {
        "name": "Undetermined",
        "id": "und"
      }
    ]
  }'''
  if not kmcomp.found()
    configure_file(
      command: copy_cmd + ['@INPUT@', '@OUTPUT@'],
      input: kbd + '.kmn',
      output: kbd_basename + '.kmn'
    )
    configure_file(
      command: copy_cmd + ['@INPUT@', '@OUTPUT@'],
      input: kbd + '.kmx',
      output: kbd_basename + '.kmx'
    )
  endif
endforeach

cfg = configuration_data()
cfg.set('keyboards', test_keyboards)
configure_file(
  configuration: cfg,
  input: 'kmp.json.in',
  output: 'kmp.json'
)

if kmcomp.found()
  foreach kbd : tests
    kbd_basename = 'k_' + kbd.underscorify().to_lower()

    content = run_command(
      cat_cmd, files(kbd + '.kmn'),
    ).stdout().strip()

    cfg = configuration_data()
    cfg.set('keyboard', kbd)
    cfg.set('keyboard_underscore', kbd_basename)
    cfg.set('id1', 'id_' + kbd_basename + '_1')
    cfg.set('id2', 'id_' + kbd_basename + '_2')
    cfg.set('id3', 'id_' + kbd_basename + '_3')
    cfg.set('build_dir', meson.current_build_dir())
    cfg.set('NAME', kbd)
    cfg.set('CONTENT', content)

    kbd_kmn = configure_file(
      configuration: cfg,
      input: join_paths('template', 'keyboard.kmn.in'),
      output: kbd_basename + '.kmn'
    )
    kbd_kpj = configure_file(
      configuration: cfg,
      input: join_paths('template', 'keyboard.kpj.in'),
      output: kbd_basename + '.kpj'
    )
    kbd_kps = configure_file(
      configuration: cfg,
      input: join_paths('template', 'keyboard.kps.in'),
      output: kbd_basename + '.kps'
    )
    kbd_kmp = custom_target(kbd_basename + '.kmp',
      input: kbd_kpj,
      output: [kbd_basename + '.kmp', kbd_basename + '.kmx'],
      depend_files: [kbd_kps, kbd_kmn],
      depfile: kbd_basename + '.dep',
      command: kmcomp_cmd + ['-s', '-d', '@INPUT@'])

    kbd_src = files(join_paths(test_path, kbd_basename) + '.kmn')
    kbd_obj = join_paths(test_path, kbd_basename) + '.kmx'
    # kbd_log = custom_target(kbd.underscorify() + '.kmx',
    #   output: kbd + '.log',
    #   input: kbd_src,
    #   command: kmcomp_cmd + ['-s', '-d', '@INPUT@', kbd_obj, '@OUTPUT@']
    # )

    test(kbd, kmx, depends: [kbd_kmp], args: [kbd_src, kbd_obj])
  endforeach
else
  foreach kbd : tests
    if compiler.get_id() == 'emscripten'
      kbd_basename = kbd
    else
      kbd_basename = 'k_' + kbd.underscorify().to_lower()
    endif
    kbd_src = join_paths(test_path, kbd_basename) + '.kmn'
    kbd_obj = join_paths(test_path, kbd_basename) + '.kmx'

    test(kbd, kmx, args: [kbd_src, kbd_obj])
  endforeach
endif


# test for km_kbp_keyboard_get_key_list

# For following tests, we need to be copying from source_path,
# not build_path, so we'll just update test_path accordingly. This
# should work for Linux, macOS, and WASM.
test_path = source_path

key_e = executable('key_list', 'kmx_key_list.cpp',
                cpp_args: defns + warns,
                include_directories: [inc, libsrc],
                link_args: links + tests_flags,
                objects: lib.extract_all_objects())
test_kbd = 'kmx_key_list'
if kmcomp.found()
  kbd_src = files(test_kbd + '.kmn')
  kbd_obj = join_paths(meson.current_source_dir(), test_kbd) + '.kmx'
  kbd_log = custom_target(test_kbd + '.kmx'.underscorify(),
    output: test_kbd + '.log',
    input: kbd_src,
    command: kmcomp_cmd + ['-s', '-d', '@INPUT@', kbd_obj, '@OUTPUT@']
  )
  test('key_list', key_e,  depends: kbd_log, args: [kbd_obj] )
else
  kbd_obj = join_paths(test_path, test_kbd) + '.kmx'
  test('key_list', key_e, args: [kbd_obj] )
endif

# test for imx list

imx_e = executable('imx_list', 'kmx_imx.cpp',
                cpp_args: defns + warns,
                include_directories: [inc, libsrc],
                link_args: links + tests_flags,
                objects: lib.extract_all_objects())

test_kbd = 'kmx_imsample'
if kmcomp.found()
  kbd_src = files(test_kbd + '.kmn')
  kbd_obj = join_paths(meson.current_source_dir(), test_kbd) + '.kmx'
  kbd_log = custom_target(test_kbd + '.kmx'.underscorify(),
    output: test_kbd + '.log',
    input: kbd_src,
    command: kmcomp_cmd + ['-s', '-d', '@INPUT@', kbd_obj, '@OUTPUT@']
  )
    test('imx_list', imx_e,  depends: kbd_log, args: [kbd_obj] )
else
  kbd_obj = join_paths(test_path, test_kbd) + '.kmx'
  test('imx_list', imx_e, args: [kbd_obj] )
endif



