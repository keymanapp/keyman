#
# Keyman is copyright (C) SIL Global. MIT License.
#
# Created by Tim Eves (TSE) on 2018-10-02
#
# Cross platform build script to compile unit tests.
#

# Include Google Test

gtest = subproject('gtest')
gtest_dep = gtest.get_variable('gtest_dep')
gmock_dep = gtest.get_variable('gmock_dep')

# Shared test variables

test_includes = [inc, include_directories('../src')]
test_dependencies = [icu_uc, icu_i18n, gtest_dep, gmock_dep]
test_warns = []
test_link_args = []

if cpp_compiler.get_id() == 'gcc' or cpp_compiler.get_id() == 'clang' or cpp_compiler.get_id() == 'emscripten'
  # TODO: these two flags should be cleaned up
  # -Wno-missing-field-initializers --> action_items.cpp
  # -Wno-unused-parameter --> state_api.tests.cpp for test_imx_callback
  test_warns = [
     '-Wno-missing-field-initializers',
     '-Wno-unused-parameter'
  ]
 endif


if cpp_compiler.get_id() == 'emscripten'
  test_link_args += ['-lnodefs.js', wasm_exported_runtime_methods]
endif

# kmx_test_source is required for linux builds, so always enable it even when we
# disable all other tests
subdir('kmx_test_source')

if get_option('keyman_core_tests')
  message('option "keyman_core_tests" is true, enabling tests')
  subdir('unit')
else
  message('option "keyman_core_tests" is false, disabling tests')
endif
