# Copyright:    © 2018 SIL International.
# Description:  Cross platform build script to compile unit tests.
# Create Date:  2 Oct 2018
# Authors:      Tim Eves (TSE)
# History:      2  Oct 2018 - TSE - Added tests for utfcodec & json.
#               19 Oct 2018 - TSE - Added kmnkbd directory for API unit tests.
#

# Note: this version of cmpfiles ignores line endings, which is better for platform independence
cmpfiles = ['-c', 'import sys; a = open(sys.argv[1], \'r\').read(); b = open(sys.argv[2], \'r\').read(); exit(not (a==b))']
stnds = join_paths(meson.current_source_dir(), 'standards')

libsrc = include_directories(
  '../src',
  '../../common/include'
)

# kmx_test_source is required for linux builds, so always enable it even when we
# disable all other tests
subdir('kmx_test_source')

if get_option('keyman_core_tests')
  message('option "keyman_core_tests" is true, enabling tests')

  if get_option('default_library') != 'static'
    ctypes_void_p_size = ['-c', 'import ctypes; print(ctypes.sizeof(ctypes.c_void_p))']
    r = run_command(python, ctypes_void_p_size, check: true)
    python_ctypes_compatible = r.stdout().to_int() == cpp_compiler.sizeof('void *')
    if not python_ctypes_compatible
      message('Python ctypes is incompatible with built shared object. Disabling some tests.')
    endif
  endif

  subdir('unit')
else
  message('option "keyman_core_tests" is false, disabling tests')
endif
