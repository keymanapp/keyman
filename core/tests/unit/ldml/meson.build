# Copyright:    © SIL International.
# Description:  Cross platform build script to compile kmkbpldml API unit tests.
# Create Date:  5 Aug 2022
# Authors:      Marc Durdin
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

ldml = executable('ldml',
    'ldml.cpp',
    'ldml_test_source.cpp',
    cpp_args: defns + warns,
    include_directories: [inc, libsrc],
    link_args: links + tests_flags,
    objects: lib.extract_all_objects())

tests = [
  '000_null_keyboard',
  '001_tiny',
  '002_null_invalid'
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
  if true # TODO: not kmcomp.found()
    configure_file(
      command: copy_cmd + ['@INPUT@', '@OUTPUT@'],
      input: kbd + '.xml',
      output: kbd_basename + '.xml'
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

if false # TODO: kmcomp.found()
  foreach kbd : tests
    kbd_basename = 'k_' + kbd.underscorify().to_lower()

    content = run_command(
      cat_cmd, files(kbd + '.xml'),
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
      input: join_paths('template', 'keyboard.xml.in'),
      output: kbd_basename + '.xml'
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

    kbd_src = files(join_paths(test_path, kbd_basename) + '.xml')
    kbd_obj = join_paths(test_path, kbd_basename) + '.kmx'
    # kbd_log = custom_target(kbd.underscorify() + '.kmx',
    #   output: kbd + '.log',
    #   input: kbd_src,
    #   command: kmcomp_cmd + ['-s', '-d', '@INPUT@', kbd_obj, '@OUTPUT@']
    # )

    test(kbd, ldml, depends: [kbd_kmp], args: [kbd_src, kbd_obj])
  endforeach
else
  foreach kbd : tests
    if compiler.get_id() == 'emscripten'
      kbd_basename = kbd
    else
      kbd_basename = 'k_' + kbd.underscorify().to_lower()
    endif
    kbd_src = join_paths(test_path, kbd_basename) + '.xml'
    kbd_obj = join_paths(test_path, kbd_basename) + '.kmx'

    test(kbd, ldml, args: [kbd_src, kbd_obj])
  endforeach
endif


e = executable('test_kmx_plus', 'test_kmx_plus.cpp',
    cpp_args: defns + warns,
    include_directories: [inc, libsrc],
    link_args: links + tests_flags,
    objects: lib.extract_all_objects())
test('test_kmx_plus', e)
