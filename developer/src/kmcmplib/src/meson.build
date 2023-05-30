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
  lib_links = ['--whole-archive', '--bind', '-sMODULARIZE', '-sEXPORT_ES6']
  links += ['-lnodefs.js', '--bind', '-sEXPORTED_RUNTIME_METHODS=[\'UTF8ToString\']']
  # tests are building as ES6 so we need to declare the file extension
  # note that meson currently struggles with the sanitycheckc_cross.exe
  # program, because it has a hard coded extension (.exe) which is not
  # valid for node programs in module mode.
  # name_suffix = '.mjs'
endif

icu = subproject('icu-for-uset', default_options: [ 'default_library=static', 'cpp_std=c++17', 'warning_level=0', 'werror=false'])
icuuc_dep = icu.get_variable('icuuc_dep')

lib = library('kmcmplib',
  'CasedKeys.cpp',
  'CharToKeyConversion.cpp',
  'Compiler.cpp',
  'CompilerInterfaces.cpp',
  'DeprecationChecks.cpp',
  'Edition.cpp',
  'filesystem.cpp',
  'NamedCodeConstants.cpp',
  'versioning.cpp',
  'virtualcharkeys.cpp',
  'xstring.cpp',
  'CharToKeyConversion.cpp',
  'CheckFilenameConsistency.cpp',
  'CheckForDuplicates.cpp',
  'CheckNCapsConsistency.cpp',
  'UnreachableRules.cpp',
  'kmx_u16.cpp',
  'CompMsg.cpp',
  'uset-api.cpp',

  '../../../../common/windows/cpp/src/ConvertUTF.c',
  '../../../../common/windows/cpp/src/crc32.cpp',
  '../../../../common/windows/cpp/src/vkeys.cpp',
  'xstring.cpp',

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
    link_args: links,
    objects: lib.extract_all_objects(),
    dependencies: icuuc_dep)
endif

