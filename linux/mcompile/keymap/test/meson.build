gtest = subproject('gtest')

gtest_main_dep = gtest.get_variable('gtest_main_dep')

keymaptest = executable('keymaptest',
    sources: ['keymap.tests.cpp', test_files],
    include_directories: [common_include_dir],
    include_directories: [common_include_dir],
    dependencies: [ gtest_main_dep, gtk, xkb, giomm_dep, glibmm_dep ],
  )

test('keymaptest', keymaptest)
