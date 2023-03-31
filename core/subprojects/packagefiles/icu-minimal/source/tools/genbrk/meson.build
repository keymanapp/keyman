genbrk_exe = executable(
  'genbrk',
  'genbrk.cpp',
  dependencies: toolutil_dep,
  install: true,
)

meson.override_find_program('genbrk', genbrk_exe)
