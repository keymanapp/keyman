#
# Keyman is copyright (C) SIL Global. MIT License.
#
# Build kmx_test_source library (used by core, linux)
#
coretest_files = files(
    'kmx_test_source.cpp',
)

kmx_test_source_lib = static_library('keymancore-tests',
  coretest_files,
  cpp_args: defns + test_warns + flags,
  include_directories: test_includes,
  link_args: links,
  objects: lib.extract_all_objects(recursive: false),
  pic: true,
  install: false
)

