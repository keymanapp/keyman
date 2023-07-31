sources = files(
  'stubdata.cpp',
)

stubdata_lib = static_library(
  icudata_name,
  sources,
  c_args: ['-DSTUBDATA_BUILD'] + usage_args,
  cpp_args:[ '-DSTUBDATA_BUILD' ]+ usage_args,
  include_directories: incdir,
  # version: U_ICU_VERSION,
)

stubdata_dep = declare_dependency(
  link_with: stubdata_lib,
)


# if meson.can_run_host_binaries()
  stubdata_native_lib = stubdata_lib
# else
#   stubdata_native_lib = library(
#     '@0@-native'.format(icudata_name),
#     sources,
#     c_args: '-DSTUBDATA_BUILD',
#     cpp_args: '-DSTUBDATA_BUILD',
#     include_directories: incdir,
#     version: U_ICU_VERSION,
#     native: true,
#   )
# endif
