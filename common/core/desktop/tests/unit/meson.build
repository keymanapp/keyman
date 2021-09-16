# Copyright:    © 2018 SIL International.
# Description:  Cross platform build script to compile unit tests.
# Create Date:  2 Oct 2018
# Authors:      Tim Eves (TSE)
# History:      2  Oct 2018 - TSE - Added tests for utfcodec & json.
#               19 Oct 2018 - TSE - Added kmnkbd directory for API unit tests.
#

# Note: this version of cmpfiles ignores line endings, which is better for platform independence
cmpfiles = ['-c', 'import sys; a = open(sys.argv[1], \'r\').read(); b = open(sys.argv[2], \'r\').read(); exit(not (a==b))']
stnds = join_paths(meson.source_root(), 'tests', 'standards')

libsrc = include_directories(join_paths('../', 'src'))

if get_option('default_library') != 'static'
  ctypes_void_p_size = ['-c', 'import ctypes; print(ctypes.sizeof(ctypes.c_void_p))']
  r = run_command(python, ctypes_void_p_size)
  python_ctypes_compatible = r.stdout().to_int() == compiler.sizeof('void *')
  if not python_ctypes_compatible
    message('Python ctypes is incompatible with built shared object. Disabling some tests.')
  endif
endif
 
subdir('unit/json')
subdir('unit/utftest')
subdir('unit/kmnkbd')
subdir('unit/kmx')
subdir('unit/rust_mock')
subdir('unit/kbpkmx')
