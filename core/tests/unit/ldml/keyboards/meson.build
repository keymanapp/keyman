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

test_file_path = meson.current_build_dir()

# Setup copying of source .xml files

if build_machine.system() == 'windows'
  kmldmlc_host = find_program('node.exe', required: true)
  copy_cmd = [find_program('cmd.exe', required: true), '/c', 'copy']
else
  kmldmlc_host = find_program('node', required: true)
  copy_cmd = [find_program('cp', required: true)]
endif

# Setup kmldmlc

kmldmlc_root = join_paths(meson.source_root(),'..','developer','src','kmldmlc')
kmldmlc_cmd = [kmldmlc_host, kmldmlc_root]

# Build all keyboards in output folder

foreach kbd : tests
  kbd_src = join_paths(meson.current_source_dir(), kbd) + '.xml'
  kbd_dst = join_paths(test_file_path, kbd) + '.xml'
  kbd_obj = join_paths(test_file_path, kbd) + '.kmx'

  configure_file(
    command: copy_cmd + ['@INPUT@', '@OUTPUT@'],
    input: kbd + '.xml',
    output: kbd + '.xml'
  )

  configure_file(
    command: kmldmlc_cmd + ['@INPUT@', '--outFile', kbd_obj],
    output: kbd + '.kmx',
    input: kbd_src
  )
endforeach
