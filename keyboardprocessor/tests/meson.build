cmpfiles = ['-c', 'import sys, filecmp; exit(not filecmp.cmp(*sys.argv[1:]))']
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
