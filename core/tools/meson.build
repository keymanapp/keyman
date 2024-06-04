# Copyright:    © 2024 SIL International.
# Description:  Cross platform build script to compile tool(s).
# Create Date:  31 May 2024
# Authors:      Steven R. Loomis (SRL)
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


if cpp_compiler.get_id() == 'emscripten'
  tests_flags += ['-lnodefs.js',
                  '-sNO_DISABLE_EXCEPTION_CATCHING', # for test exceptions
                  wasm_exported_runtime_methods]
endif

norm_unicode_update = executable('norm_unicode_update',
    ['norm_unicode_update.cpp'],
    cpp_args: defns + warns,
    include_directories: [inc, libsrc, '../../developer/src/ext/json'],
    link_args: links + tests_flags,
    dependencies: [icu_uc, icu_i18n],
    # link_with: [lib],
    objects: lib.extract_all_objects(recursive: false),
    )


# ../../resources/standards-data/unicode-character-database/
norm_data = custom_target('norm_data', output: 'nfd_table.h', command: [norm_unicode_update, '@OUTPUT@'])


# TODO: execute it
