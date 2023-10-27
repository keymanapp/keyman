
# TODO -- why are these differing from the standard.meson.build flags?
if cpp_compiler.get_id() == 'gcc' or cpp_compiler.get_id() == 'clang' or cpp_compiler.get_id() == 'emscripten'
  warns = [
     '-Wno-missing-field-initializers',
     '-Wno-unused-parameter'
  ]
else
  warns = []
endif

coretest_files = files(
    'kmx_test_source.cpp',
)

kmx_test_source_lib = static_library('keymancore-tests',
  coretest_files,
  cpp_args: defns + warns + flags,
  include_directories: [inc, libsrc],
  link_args: links,
  objects: lib.extract_all_objects(recursive: false),
  pic: true,
  install: false
)

