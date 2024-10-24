node = find_program('node', required: true)

gtest = subproject('gtest')
gtest_dep = gtest.get_variable('gtest_dep')
gmock_dep = gtest.get_variable('gmock_dep')

test_util_files = [
  meson.current_source_dir() / 'emscripten_filesystem.cpp',
  meson.current_source_dir() / 'load_kmx_file.cpp',
]

common_test_files = [
  test_util_files,
  meson.global_source_root() / '../common/include/test_color.cpp'
]

hextobin_root = meson.global_source_root() / '../common/tools/hextobin/build/hextobin.js'
hextobin_cmd = [node, hextobin_root]

if cpp_compiler.get_id() == 'emscripten'
  extra_link_args = [ '-lnodefs.js' ]
else
  extra_link_args = []
endif

kmcorekeyboardapitests = executable('km_core_keyboard_api.tests',
  [
    'km_core_keyboard_api.tests.cpp',
    test_util_files,
  ],
  include_directories: [inc, libsrc],
  link_args: [ links, extra_link_args ],
  dependencies: [icu_uc, icu_i18n, gtest_dep, gmock_dep],
  objects: lib.extract_all_objects(recursive: false),
)

test('km-core-keyboard-api-tests', kmcorekeyboardapitests)

subdir('json')
subdir('utftest')
subdir('kmnkbd')
subdir('kmx')
subdir('ldml')
