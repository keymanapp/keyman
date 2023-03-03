#
# Keyman is copyright (C) SIL International. MIT License.
#
# Cross platform build script to compile kmcompx
#

defns = ['-DKMN_KBP_EXPORTING']
version_res = []

# TODO: move these shared defines to a resources/meson folder?
if compiler.get_id() == 'gcc' or compiler.get_id() == 'clang'
  warns = [
     '-Wall',
     '-Wctor-dtor-privacy',
     '-Wdouble-promotion',
     '-Wendif-labels',
     '-Wextra',
     '-Wno-unknown-pragmas',
     '-Wno-missing-field-initializers',
     '-Wnon-virtual-dtor',
     '-Wshadow'
  ]
  flags = [
    '-fvisibility=hidden',
    '-fvisibility-inlines-hidden'
  ]
  defns = []
  links = []

  if compiler.get_id() == 'clang'
    warns += [
      '-Wimplicit-fallthrough',
      '-Wno-double-promotion',
      '-Wshorten-64-to-32'
    ]
  endif

  if host_machine.system() == 'darwin'
    warns += ['-Wno-ctor-dtor-privacy', '-Wno-non-virtual-dtor']
  endif
endif

if compiler.get_id() == 'msvc'
  warns = []
  flags = []
  defns += [
    '-D_SCL_SECURE_NO_WARNINGS',
    '-D_CRT_SECURE_NO_WARNINGS',
    #'-DUNICODE'
  ]
  links = []

  # /n = append null
  # /c65001 = utf-8 file format
  version_res += import('windows').compile_resources('version.rc', args:['/n','/c65001'])
endif


if compiler.get_id() == 'emscripten'
  warns = []
  flags = []
  defns = []
  links = []
endif


lib = library('kmcmplib',
  'CasedKeys.cpp',
  'CharToKeyConversion.cpp',
  'Compiler.cpp',
  'DeprecationChecks.cpp',
  'Edition.cpp',
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
  '../../../../common/windows/cpp/src/registry.cpp',
  '../../../../common/windows/cpp/src/unicode.cpp',
  '../../../../common/windows/cpp/src/vkeys.cpp',
  '../../../../common/windows/cpp/src/xstring.cpp',

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
