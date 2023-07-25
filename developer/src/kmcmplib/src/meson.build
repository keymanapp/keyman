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
     '-Wextra'
  ]
endif

if cpp_compiler.get_id() == 'msvc'
  # /n = append null
  # /c65001 = utf-8 file format
  version_res += import('windows').compile_resources('version.rc', args:['/n','/c65001'])
endif

name_suffix = []

if cpp_compiler.get_id() == 'emscripten'
  # wasm-exceptions supported in Node 18+, Chrome 95+, Firefox 100+, Safari 15.2+
  flags += ['-fwasm-exceptions']
  lib_links = ['--whole-archive', '-sMODULARIZE', '-sEXPORT_ES6']
  links += ['-fwasm-exceptions', '--bind', '-sEXPORTED_RUNTIME_METHODS=[\'UTF8ToString\']']
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
  'CompilerInterfaces.cpp',
  'CompilerInterfacesWasm.cpp',
  'CompMsg.cpp',
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
  host = executable('wasm-host', #'wasm-host.cpp',
    cpp_args: defns,
    include_directories: inc,
    link_args: links + lib_links,
    objects: lib.extract_all_objects(),
    dependencies: icuuc_dep)
endif

