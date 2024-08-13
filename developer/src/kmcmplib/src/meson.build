#
# Keyman is copyright (C) SIL International. MIT License.
#
# Cross platform build script to compile kmcompx
#

# TODO: is this required? It should be Keyman Core only
defns += ['-DKMN_KBP_EXPORTING']
version_res = []
lib_links = []

if cpp_compiler.get_id() == 'gcc' or cpp_compiler.get_id() == 'clang'
  warns += [
     '-Wall',
     '-Wextra',
     '-Wno-ignored-qualifiers'
  ]
  flags += ['-D__cdecl= ']
endif

if cpp_compiler.get_id() == 'msvc'
  # /n = append null
  # /c65001 = utf-8 file format
  version_res += import('windows').compile_resources('version.rc', args:['/n','/c65001'])
endif

name_suffix = []

if cpp_compiler.get_id() == 'emscripten'
  # wasm-exceptions supported in Node 18+, Chrome 95+, Firefox 100+, Safari 15.2+

  # For additional (slow) sanitize, add:
  # Additional sanitize required on both flags and links: '-fsanitize=undefined', '-fsanitize=address'
  #sanitize = ['-fsanitize=undefined', '-fsanitize=address']
  sanitize = []

  flags += ['-fwasm-exceptions'] + sanitize

  lib_links = ['--whole-archive', '-sALLOW_MEMORY_GROWTH', '-sMODULARIZE', '-sEXPORT_ES6']
  links += [
    # Forcing inclusion of debug symbols
    '-g', '-Wlimited-postlink-optimizations',
    '-fwasm-exceptions', '--bind'] + sanitize

  # Alternative to sanitize is adding '-sASSERTIONS', '-sSAFE_HEAP' to lib_links:
  # lib_links += ['-sASSERTIONS', '-sSAFE_HEAP']

  # For profiling, add:
  # lib_links += ['--profiling-funcs', '-sDEMANGLE_SUPPORT=1']

  if cpp_compiler.version().version_compare('>=3.1.44')
    # emscripten 3.1.44 removes .asm object and so we need to export `wasmExports`
    # #9375; https://github.com/emscripten-core/emscripten/blob/main/ChangeLog.md#3144---072523
    links += ['-sEXPORTED_RUNTIME_METHODS=[\'UTF8ToString\',\'wasmExports\']']
  else
    # emscripten < 3.1.44 does not include `wasmExports`
    links += ['-sEXPORTED_RUNTIME_METHODS=[\'UTF8ToString\']']
  endif

  # For Google Test
  add_global_arguments('-pthread', language: [ 'cpp', 'c' ] )

endif

icu = subproject('icu-for-uset', default_options: [ 'default_library=static', 'cpp_std=c++17', 'warning_level=0', 'werror=false'])
icuuc_dep = icu.get_variable('icuuc_dep')

lib = library('kmcmplib',
  'CasedKeys.cpp',
  'CharToKeyConversion.cpp',
  'CheckForDuplicates.cpp',
  'CheckNCapsConsistency.cpp',
  'CompileKeyboardBuffer.cpp',
  'Compiler.cpp',
  'CompilerErrors.cpp',
  'CompilerInterfaces.cpp',
  'CompilerInterfacesWasm.cpp',
  'cp1252.cpp',
  'DeprecationChecks.cpp',
  'Edition.cpp',
  'kmx_u16.cpp',
  'NamedCodeConstants.cpp',
  'UnreachableRules.cpp',
  'uset-api.cpp',
  'versioning.cpp',
  'virtualcharkeys.cpp',
  'xstring.cpp',
  '../../../../common/cpp/utfcodec.cpp',
  '../../../../common/windows/cpp/src/ConvertUTF.c',
  '../../../../common/windows/cpp/src/crc32.cpp',
  '../../../../common/windows/cpp/src/vkeys.cpp',

  version_res,
  cpp_args: defns + warns + flags,
  link_args: links + lib_links,
  version: meson.project_version(),
  include_directories: inc,
  install: true,
  dependencies: icuuc_dep)

kmcmplib = declare_dependency(link_with: lib, include_directories: inc)


if cpp_compiler.get_id() == 'emscripten'
  # Build an executable
  # TODO: rename wasm-host to kmcmplib.wasm
  host = executable('wasm-host',
    cpp_args: defns,
    include_directories: inc,
    link_args: links + lib_links,
    objects: lib.extract_all_objects(recursive: false),
    dependencies: icuuc_dep)

  if get_option('buildtype') == 'release'
    # Split debug symbols into separate wasm file for release builds only
    # as the release symbols will be uploaded to sentry
    # custom_target('kmcmplib.wasm',
    #   depends: host,
    #   input: host,
    #   output: 'kmcmplib.wasm',
    #   command: ['wasm-split', '@OUTDIR@/wasm-host.wasm', '-o', '@OUTPUT@', '--strip', '--debug-out=@OUTDIR@/kmcmplib.debug.wasm'],
    #   build_by_default: true)
  endif
endif


gtest = subproject('gtest')
gtest_dep = gtest.get_variable('gtest_main_dep')
gmock_dep = gtest.get_variable('gmock_dep')
