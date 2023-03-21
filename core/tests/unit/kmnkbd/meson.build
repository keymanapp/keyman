# Copyright:    © 2018 SIL International.
# Description:  Cross platform build script to compile libkmnkbp API unit tests.
# Create Date:  19 Oct 2018
# Authors:      Tim Eves (TSE)
#

# TODO -- why are these differing from the standard.meson.build flags?
if cpp_compiler.get_id() == 'gcc' or cpp_compiler.get_id() == 'clang' or cpp_compiler.get_id() == 'emscripten'
  warns = [
     '-Wno-missing-field-initializers',
     '-Wno-unused-parameter'
  ]
else
  warns = []
endif

local_defns = ['-DKMN_KBP_STATIC']
tests = [
  ['context-api', 'context_api.cpp'],
  ['keyboard-api', 'keyboard_api.cpp'],
  ['options-api', 'options_api.cpp'],
  ['state-api', 'state_api.cpp'],
  ['debug-api', 'debug_api.cpp'],
  ['kmx_xstring', 'test_kmx_xstring.cpp'],
  ['kmx_context', 'test_kmx_context.cpp'],
]

if cpp_compiler.get_id() == 'emscripten'
  tests_flags = ['--embed-file', join_paths(meson.current_source_dir(),'..','kmx','@/')]
  source_path = '/'
  test_path = '/'
else
  tests_flags = []
  source_path = meson.current_source_dir()
  test_path = join_paths(meson.current_source_dir(), '..', 'kmx')
endif

foreach t : tests
  bin = executable(t[0], t[1],
    cpp_args: local_defns + defns + warns,
    include_directories: [inc, libsrc],
    link_args: links + tests_flags,
    objects: lib.extract_all_objects(recursive: false))

  test(t[0], bin, args: ['--color', test_path])
endforeach
