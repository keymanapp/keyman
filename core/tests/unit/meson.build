#
# Keyman is copyright (C) SIL Global. MIT License.
#
# Cross platform build script to compile unit tests
#

common_test_files_without_main = [
  meson.current_source_dir() / 'helpers' / 'emscripten_filesystem.cpp',
  meson.current_source_dir() / 'helpers' / 'load_kmx_file.cpp',
  meson.current_source_dir() / 'helpers' / 'printers.cpp',
  meson.current_source_dir() / 'helpers' / 'action_items.cpp',
  meson.current_source_dir() / 'helpers' / 'debug_items.cpp',
  meson.current_source_dir() / 'helpers' / 'core_test_helpers.cpp',
]

common_test_files = [
  common_test_files_without_main,
  meson.current_source_dir() / 'helpers' / 'main.cpp',
]

common_test_keyboards_baseline = meson.global_source_root() / '../common/test/keyboards/baseline'

# shared commands for building fixtures

node = find_program('node', required: true)

hextobin_root = meson.global_source_root() / '../common/tools/hextobin/build/hextobin.js'
hextobin_cmd = [node, hextobin_root]

kmc_root = meson.global_source_root() / '../developer/src/kmc/build/src/kmc.js'
kmc_cmd = [node, '--enable-source-maps', kmc_root]

subdir('utftest')
subdir('api')
subdir('kmx')
subdir('ldml')
subdir('wasm')
