# Copyright:    © 2018 SIL International.
# Description:  Cross platform build script to compile libkeymancore API unit tests.
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

local_defns = ['-DKM_CORE_LIBRARY_STATIC']
tests = [
  ['action-api', 'action_api.cpp'],
  ['action-set-api', 'action_set_api.cpp'],
  ['context-api', 'context_api.cpp'],
  ['keyboard-api', 'keyboard_api.cpp'],
  ['options-api', 'options_api.cpp'],
  ['state-api', 'state_api.cpp'],
  ['state-context-api', 'state_context_api.cpp'],
  ['debug-api', 'debug_api.cpp'],
  ['kmx_xstring', 'test_kmx_xstring.cpp'],
  ['kmx_context', 'test_kmx_context.cpp'],
  ['test_actions_normalize', 'test_actions_normalize.cpp'],
  ['test_actions_get_api', 'test_actions_get_api.cpp'],
]

test_path = join_paths(meson.current_build_dir(), '..', 'kmx')
tests_flags = []

if cpp_compiler.get_id() == 'emscripten'
  tests_flags += ['-lnodefs.js', wasm_exported_runtime_methods]
endif

foreach t : tests
  bin = executable(t[0], [t[1], common_test_files],
    cpp_args: local_defns + defns + warns,
    include_directories: [inc, libsrc],
    link_args: links + tests_flags,
    dependencies: [icu_uc, icu_i18n],
    objects: lib.extract_all_objects(recursive: false))

  test(t[0], bin, args: ['--color', test_path])
endforeach
