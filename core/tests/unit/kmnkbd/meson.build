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
  ['action-api-tests', 'action_api.tests.cpp'],
  ['action-set-api-tests', 'action_set_api.tests.cpp'],
  ['context-api-tests', 'context_api.tests.cpp'],
  ['keyboard-api-tests', 'keyboard_api.tests.cpp'],
  ['options-api-tests', 'options_api.tests.cpp'],
  ['state-api-tests', 'state_api.tests.cpp'],
  ['state-context-api-tests', 'state_context_api.tests.cpp'],
  ['debug-api-tests', 'debug_api.tests.cpp'],
  ['kmx_xstring-tests', 'kmx_xstring.tests.cpp'],
  ['kmx_context-tests', 'kmx_context.tests.cpp'],
  ['actions_normalize-tests', 'actions_normalize.tests.cpp'],
  ['actions_get_api-tests', 'actions_get_api.tests.cpp'],
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
