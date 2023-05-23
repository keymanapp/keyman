project('keymap', 'c', 'cpp',
        license: 'MIT',
        meson_version: '>=1.0')

gtk = dependency('gtk+-3.0', version: '>= 2.4')

keymap = executable(
  'keymap',
  sources: ['keymap.cpp'],
  dependencies: [gtk]
)
