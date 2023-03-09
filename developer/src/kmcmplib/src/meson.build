#
# Keyman is copyright (C) SIL International. MIT License.
#
# Cross platform build script to compile kmcompx
#

# TODO: is this required? It should be Keyman Core only
defns += ['-DKMN_KBP_EXPORTING']
version_res = []

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

if cpp_compiler.get_id() == 'emscripten'
  links += ['-lnodefs.js', '-sEXPORTED_RUNTIME_METHODS=[\'UTF8ToString\']']
endif


lib = library('kmcmplib',
  'CasedKeys.cpp',
  'CharToKeyConversion.cpp',
  'Compiler.cpp',
  'DeprecationChecks.cpp',
  'Edition.cpp',
  'filesystem.cpp',
  'json-validation.cpp',
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

  '../../../../common/windows/cpp/src/ConvertUTF.c',
  '../../../../common/windows/cpp/src/crc32.cpp',
  '../../../../common/windows/cpp/src/vkeys.cpp',
  'xstring.cpp',

  '../../ext/json-schema-validator/json-schema-draft4.json.cpp',
  '../../ext/json-schema-validator/json-uri.cpp',
  '../../ext/json-schema-validator/json-validator.cpp',
  version_res,
  cpp_args: defns + warns + flags,
  link_args: links,
  version: meson.project_version(),
  include_directories: inc,
  install: true)

kmcmplib = declare_dependency(link_with: lib, include_directories: inc)

pkg = import('pkgconfig')
pkg.generate(
    name: 'keyman_kmn_compiler',
    version: meson.project_version(),
    description: 'Keyman compiler for KMN keyboards.',
    libraries: lib)
