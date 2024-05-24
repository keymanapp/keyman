# Copyright:    © 2018-2022 SIL International.
# Description:  Cross platform build script to compile libkeymancore.
# Create Date:  2 Oct 2018
# Authors:      Tim Eves (TSE)
#

version_res = []

# TODO: shouldn't this be defined only for dylib and not static lib?
defns += ['-DKM_CORE_LIBRARY_EXPORTING']

if cpp_compiler.get_id() == 'msvc'
  defns += ['-DUNICODE']
  # /n = append null
  # /c65001 = utf-8 file format
  version_res += import('windows').compile_resources('version.rc', args:['/n','/c65001'])
endif

if cpp_compiler.get_id() == 'emscripten'
  # TODO: why do we need this defn here?
  defns += ['-DKM_CORE_LIBRARY']
endif

# ICU4C is used for repertoire tests and core implementation

if target_machine.system() == 'linux'
  # use pkg-config when targetting linux
  pkgconfig = 'pkg-config'
  icu_uc = dependency('icu-uc', required: true)
  icu_i18n = dependency('icu-i18n', required: true)
else
  # load ICU from wrap
  icu4c = subproject('icu-minimal', default_options: [ 'default_library=static', 'cpp_std=c++17', 'warning_level=0',
'werror=false']) # TODO-LDML: options: static, no data (these are in the meson build files)
  icu_uc = icu4c.get_variable('icuuc_dep')
  icu_i18n = icu4c.get_variable('icui18n_dep')
endif

if icu_uc.found()
  defns += '-DHAVE_ICU4C'
endif


kmx_files = files(
  'actions_normalize.cpp',
  'action.cpp',
  'context_helpers.cpp',
  'option.cpp',
  'keyboard.cpp',
  'state.cpp',
  'debuglog.cpp',
  'vkey_to_contextreset.cpp',
  'km_core_action_api.cpp',
  'km_core_context_api.cpp',
  'km_core_keyboard_api.cpp',
  'km_core_options_api.cpp',
  'km_core_state_api.cpp',
  'km_core_state_context_set_if_needed.cpp',
  'km_core_debug_api.cpp',
  'km_core_processevent_api.cpp',
  'jsonpp.cpp',
  'util_normalize.cpp',
  'core_icu.cpp',
  'ldml/ldml_processor.cpp',
  'ldml/ldml_transforms.cpp',
  'ldml/ldml_markers.cpp',
  'ldml/ldml_vkeys.cpp',
  'mock/mock_processor.cpp',
  'kmx/kmx_consts.cpp',
  'kmx/kmx_processevent.cpp',
  'kmx/kmx_actions.cpp',
  'kmx/kmx_capslock.cpp',
  'kmx/kmx_context.cpp',
  'kmx/kmx_conversion.cpp',
  'kmx/kmx_debugger.cpp',
  'kmx/kmx_environment.cpp',
  'kmx/kmx_file.cpp',
  'kmx/kmx_modifiers.cpp',
  'kmx/kmx_options.cpp',
  'kmx/kmx_plus.cpp',
  'kmx/kmx_processor.cpp',
  'kmx/kmx_xstring.cpp',
)

api_files = files(
  'km_core_action_api.cpp',
  'km_core_context_api.cpp',
  'km_core_keyboard_api.cpp',
  'km_core_options_api.cpp',
  'km_core_state_api.cpp',
  'km_core_debug_api.cpp',
  'km_core_processevent_api.cpp',
)

core_files = files(
  'action.cpp',
  'option.cpp',
  'keyboard.cpp',
  'state.cpp',
  'jsonpp.cpp',
  'utfcodec.cpp',
)

mock_files = files(
  'mock/mock_processor.cpp',
)

lib = library('keymancore',
  api_files,
  core_files,
  kmx_files,
  mock_files,
  version_res,
  cpp_args: defns + warns + flags,
  link_args: links,
  version: lib_version,
  include_directories: inc,
  pic: true,
  install: true,
  dependencies: [icu_uc, icu_i18n],
  )

headerdirs = [ '.', 'keyman' ] # subdirectories of ${prefix}/include to add to header path

keymancore = declare_dependency(link_with: lib, include_directories: inc, dependencies: [icu_uc, icu_i18n])

pkg = import('pkgconfig')
pkg.generate(
    name: 'keyman_core',
    version: meson.project_version(),
    description: 'Keyman processor for KMN keyboards.',
    subdirs: headerdirs,
    libraries: lib)
