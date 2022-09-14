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
    '-D_CRT_SECURE_NO_WARNINGS'
    #'-DUNICODE'
  ]
  links = []

  # /n = append null
  # /c65001 = utf-8 file format
  version_res += import('windows').compile_resources('version.rc', args:['/n','/c65001'], include_directories: inc)
endif

if compiler.get_id() == 'emscripten'
  warns = []
  flags = []
  defns = []
  links = []
endif

version_lib = compiler.find_library('version')

lib = library('kmcompx',
  '../../src/kmcmpdll/CasedKeys.cpp',
  '../../src/kmcmpdll/CharToKeyConversion.cpp',
  '../../src/kmcmpdll/Compiler.cpp',
  '../../src/kmcmpdll/DeprecationChecks.cpp',
  '../../src/kmcmpdll/Edition.cpp',
  '../../src/kmcmpdll/json-validation.cpp',
  '../../src/kmcmpdll/NamedCodeConstants.cpp',
  '../../src/kmcmpdll/versioning.cpp',
  '../../src/kmcmpdll/virtualcharkeys.cpp',
  '../../src/kmcmpdll/xstring.cpp',
  '../../src/kmcmpdll/CharToKeyConversion.cpp',
  '../../src/kmcmpdll/CheckFilenameConsistency.cpp',
  '../../src/kmcmpdll/CheckForDuplicates.cpp',
  '../../src/kmcmpdll/CheckNCapsConsistency.cpp',
  '../../src/kmcmpdll/UnreachableRules.cpp',
  '../../src/kmcmpdll/kmx_u16.cpp',
  '../../src/kmcmpdll/CompMsg.cpp',

  '../../../common/windows/cpp/src/ConvertUTF.c',
  '../../../common/windows/cpp/src/crc32.cpp',
  '../../../common/windows/cpp/src/registry.cpp',
  '../../../common/windows/cpp/src/unicode.cpp',
  '../../../common/windows/cpp/src/vkeys.cpp',
  '../../../common/windows/cpp/src/xstring.cpp',

  '../../src/ext/json-schema-validator/json-schema-draft4.json.cpp',
  '../../src/ext/json-schema-validator/json-uri.cpp',
  '../../src/ext/json-schema-validator/json-validator.cpp',
  version_res,
  cpp_args: defns + warns + flags,
  link_args: links,
  dependencies: [version_lib],
  version: meson.project_version(),
  include_directories: inc,
  install: true)

kmcompx = declare_dependency(link_with: lib, include_directories: inc)

pkg = import('pkgconfig')
pkg.generate(
    name: 'keyman_kmn_compiler',
    version: meson.project_version(),
    description: 'Keyman compiler for KMN keyboards.',
    libraries: lib)
