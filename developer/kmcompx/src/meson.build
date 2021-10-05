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
  '../../../windows/src/developer/kmcmpdll/CasedKeys.cpp',
  '../../../windows/src/developer/kmcmpdll/CharToKeyConversion.cpp',
  '../../../windows/src/developer/kmcmpdll/Compiler.cpp',
  '../../../windows/src/developer/kmcmpdll/DeprecationChecks.cpp',
  '../../../windows/src/developer/kmcmpdll/Edition.cpp',
  '../../../windows/src/developer/kmcmpdll/json-validation.cpp',
  '../../../windows/src/developer/kmcmpdll/NamedCodeConstants.cpp',
  '../../../windows/src/developer/kmcmpdll/versioning.cpp',
  '../../../windows/src/developer/kmcmpdll/virtualcharkeys.cpp',

  '../../../windows/src/global/vc/ConvertUTF.c',
  '../../../windows/src/global/vc/crc32.cpp',
  '../../../windows/src/global/vc/rc4.cpp',
  '../../../windows/src/global/vc/registry.cpp',
  '../../../windows/src/global/vc/unicode.cpp',
  '../../../windows/src/global/vc/xstring.cpp',

  '../../../windows/src/ext/json-schema-validator/json-schema-draft4.json.cpp',
  '../../../windows/src/ext/json-schema-validator/json-uri.cpp',
  '../../../windows/src/ext/json-schema-validator/json-validator.cpp',
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
