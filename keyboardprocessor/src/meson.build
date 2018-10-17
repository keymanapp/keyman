defns = []

if compiler.get_id() == 'gcc' or compiler.get_id() == 'clang'
  warns = [
     '-Wall',
     '-Wctor-dtor-privacy',
     '-Wdouble-promotion',
     '-Wendif-labels',
     '-Wextra',
     '-Wno-unknown-pragmas',
     '-Wnon-virtual-dtor',
     '-Wshadow'
  ]
  flags = [
    '-fvisibility=hidden',
    '-fvisibility-inlines-hidden'
  ]

  if compiler.get_id() == 'clang'
    warns += [
      '-Wimplicit-fallthrough',
      '-Wno-double-promotion',
      '-Wshorten-64-to-32'
    ]
  endif

  if host_machine.system() == 'darwin'
    warns += ['-Wno-ctor-dtor-privacy', '-Wno-non-virtual-dtor ']
  endif
endif

if compiler.get_id() == 'msvc'
  warns = []
  flags = []
  defns = [
    '-D_SCL_SECURE_NO_WARNINGS',
    '-D_CRT_SECURE_NO_WARNINGS',
    '-DUNICODE'
  ]
endif

pkg = import('pkgconfig')
pkg.generate(
    name: 'keyman_kmn_processor',
    version: meson.project_version(),
    description: 'Keyman processor for KMN keyboards.')

lib = library('kmnkbp0',
    'keyboardprocessor.cpp',
    'km_kbp_context_api.cpp',
    'km_kbp_keyboard_api.cpp',
    'km_kbp_options_api.cpp',
    'km_kbp_state_api.cpp',
    'json.cpp',
    cpp_args: defns + warns + flags,
    link_args: ['-lstdc++fs'],
    version: meson.project_version(),
    include_directories: inc,
    install: true)

#kmnkbd = declare_dependency(link_with: lib, include_directories: inc)
