sources = files(
  'stubdata.cpp',
)

stubdata_lib = library(
  icudata_name,
  sources,
  c_args: '-DSTUBDATA_BUILD',
  cpp_args: '-DSTUBDATA_BUILD',
  include_directories: incdir,
  version: U_ICU_VERSION,
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
