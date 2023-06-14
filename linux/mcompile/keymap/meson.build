#project('mcompile', 'c', 'cpp',        license: 'MIT',        meson_version: '>=1.0')

#gtk = dependency('gtk+-3.0', version: '>= 2.4')

#cpp_files = files(  'keymap.cpp',  'mcompile.cpp',  'mc_kmxfile.cpp',  'mc_savekeyboard.cpp',  'helpers.cpp',)

#mcompile = executable(  'mcompile',  sources: [cpp_files],  dependencies: [gtk])

# old +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#project('keymapWC', 'c', 'cpp',        license: 'MIT',       meson_version: '>=1.0')

#gtk = dependency('gtk+-3.0', version: '>= 2.4')

#keymapWC = executable(  'keymapWC',  sources: ['keymapWC.cpp'],  dependencies: [gtk])



# old +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
project('keymap', 'c', 'cpp',        license: 'MIT',       meson_version: '>=1.0')

gtk = dependency('gtk+-3.0', version: '>= 2.4')

keymap = executable(  'keymap',  sources: ['keymap.cpp'],  dependencies: [gtk])
