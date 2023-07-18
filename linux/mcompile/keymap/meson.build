

# mcompile +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

project('mcompile', 'c', 'cpp',        license: 'MIT',        meson_version: '>=1.0')
gtk = dependency('gtk+-3.0', version: '>= 2.4')
cpp_files = files(  'keymap.cpp',  'mcompile.cpp',  'filesystem.cpp',  'mc_kmxfile.cpp',  'mc_savekeyboard.cpp',  'helpers.cpp',  'u16.cpp',)
mcompile = executable(  'mcompile',  sources: [cpp_files],  dependencies: [gtk])

# keymapWC +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

#project('keymapWC', 'c', 'cpp',        license: 'MIT',       meson_version: '>=1.0')
#gtk = dependency('gtk+-3.0', version: '>= 2.4')
#cpp_files = files(   'keymapWC.cpp','mc_kmxfile.cpp',  'mc_savekeyboard.cpp',  'helpers.cpp',  'u16.cpp',)
#keymapWC = executable(  'keymapWC',  sources: [cpp_files],  dependencies: [gtk])

# keymap +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

#project('keymap', 'c', 'cpp',        license: 'MIT',       meson_version: '>=1.0')
#gtk = dependency('gtk+-3.0', version: '>= 2.4')
#cpp_files = files(  'keymap.cpp',   'mcompile.cpp',    'filesystem.cpp',   'mc_kmxfile.cpp',  'mc_savekeyboard.cpp',  'helpers.cpp',  'u16.cpp',)
#keymap = executable(  'keymap',  sources: [cpp_files],   dependencies: [gtk])
