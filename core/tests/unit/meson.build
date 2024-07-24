node = find_program('node', required: true)

common_test_files = [
  meson.current_source_dir() / 'emscripten_filesystem.cpp',
  meson.global_source_root() / '../common/include/test_color.cpp'
]

hextobin_root = meson.global_source_root() / '../common/tools/hextobin/build/hextobin.js'
hextobin_cmd = [node, hextobin_root]

subdir('json')
subdir('utftest')
#subdir('kmnkbd')
#subdir('kmx')
#subdir('ldml')
