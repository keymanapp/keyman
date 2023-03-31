gencmn_exe = executable(
  'gencmn',
  'gencmn.c',
  dependencies: toolutil_dep,
  install: true,
)

meson.override_find_program('gencmn', gencmn_exe)
