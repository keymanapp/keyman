# Copyright:    © 2018 SIL International.
# Description:  Cross platform build script to compile libkmnkbp.
# Create Date:  2 Oct 2018
# Authors:      Tim Eves (TSE)
#

defns = ['-DKMN_KBP_EXPORTING']

if compiler.get_id() == 'gcc' or compiler.get_id() == 'clang'
  warns = [
     '-Wall',
     '-Wctor-dtor-privacy',
     '-Wdouble-promotion',
     '-Wendif-labels',
     '-Wextra',
     '-Wno-unknown-pragmas',
     '-Wno-missing-field-initializers',
     '-Wnon-virtual-dtor',
     '-Wshadow'
  ]
  flags = [
    '-fvisibility=hidden',
    '-fvisibility-inlines-hidden'
  ]
  links = []

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
  defns += [
    '-D_SCL_SECURE_NO_WARNINGS',
    '-D_CRT_SECURE_NO_WARNINGS',
    '-DUNICODE'
  ]
  links = []
endif

lib = library('kmnkbp0',
    'option.cpp',
    'keyboard.cpp',
	'state.cpp',
    'km_kbp_context_api.cpp',
    'km_kbp_keyboard_api.cpp',
    'km_kbp_options_api.cpp',
    'km_kbp_state_api.cpp',
	'km_kbp_processevent_api.cpp',
    'json.cpp',
	'mock/mock_processor.cpp',
	'kmx/kmx_consts.cpp',
	'kmx/kmx_processevent.cpp',
	'kmx/kmx_actions.cpp',
	'kmx/kmx_capslock.cpp',
	'kmx/kmx_context.cpp',
	'kmx/kmx_debug.cpp',
    'kmx/kmx_environment.cpp',
	'kmx/kmx_file.cpp',
	'kmx/kmx_modifiers.cpp',
	'kmx/kmx_options.cpp',
	'kmx/kmx_processor.cpp',
	'kmx/kmx_xstring.cpp',
    'utfcodec.cpp',
    cpp_args: defns + warns + flags,
    link_args: links,
    version: lib_version,
    include_directories: inc,
    install: true)

kmnkbp = declare_dependency(link_with: lib, include_directories: inc)

pkg = import('pkgconfig')
pkg.generate(
    name: 'keyman_kmn_processor',
    version: meson.project_version(),
    description: 'Keyman processor for KMN keyboards.',
    libraries: lib)
