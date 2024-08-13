project(
  'icu',
  'c',
  'cpp',
  version: '73.1',
  meson_version: '>=0.57.0',  # TODO-LDML: Too high for debian
  default_options: [ 'cpp_std=c++17' ],
)

U_ICU_VERSION = meson.project_version()
PACKAGE_ICU_DESCRIPTION = 'International Components for Unicode'
PACKAGE_ICU_URL = 'http://icu-project.org'

cpp = meson.get_compiler('cpp')
# cpp_native = meson.get_compiler('cpp', native: true)

have_elf_h = cpp.has_header('elf.h')

if host_machine.system() == 'windows'
  add_project_arguments('-DU_PLATFORM_USES_ONLY_WIN32_API', language: 'cpp')
  add_project_arguments('-DWIN32', '-DWIN64', '-D_MBCS', language: 'cpp')
  add_project_arguments('-DU_PLATFORM_USES_ONLY_WIN32_API', language: 'c')
  add_project_arguments('-DWIN32', '-DWIN64', '-D_MBCS', language: 'c')
endif


# per icudefs.mk.in:
# "U_ATTRIBUTE_DEPRECATED is defined to hide warnings about deprecated API warnings."
add_project_arguments('-DU_NOEXCEPT=', '-DU_ATTRIBUTE_DEPRECATED=', '-DUCONFIG_USE_LOCAL=1', language: 'c')
add_project_arguments('-DU_NOEXCEPT=', '-DU_ATTRIBUTE_DEPRECATED=', '-DUCONFIG_USE_LOCAL=1', language: 'cpp')

uconfig = configuration_data()

uconfig.set('U_ENABLE_DYLOAD', 0) # no DLL
uconfig.set('U_CHECK_DYLOAD', 0) # no DLL
uconfig.set('UCONFIG_NO_FILE_IO', 1)
uconfig.set('UCONFIG_NO_LEGACY_CONVERSION', 1) # turn off file based codepage conversion
uconfig.set('UCONFIG_NO_NORMALIZATION', 0)
uconfig.set('UCONFIG_NO_BREAK_ITERATION', 1) # TODO-LDML: may want this
uconfig.set('UCONFIG_NO_IDNA', 1)
uconfig.set('UCONFIG_NO_COLLATION', 1)
uconfig.set('UCONFIG_NO_FORMATTING', 1)
uconfig.set('UCONFIG_NO_TRANSLITERATION', 1)
uconfig.set('UCONFIG_NO_REGULAR_EXPRESSIONS', 0) # want these for transforms #7375
uconfig.set('UCONFIG_NO_SERVICE', 1)
uconfig.set('U_OVERRIDE_CXX_ALLOCATION', 1)
uconfig.set('UCONFIG_NO_CONVERSION', 1)
uconfig.set('UCONFIG_USE_WINDOWS_LCID_MAPPING_API', 0)



uconfig_local = configure_file(
  configuration : uconfig,
  # dir : 'source/common/unicode',
  output : 'uconfig_local.h',
)


if cpp.get_argument_syntax() == 'msvc'
# TODO-LDML:   cl : Command line error D8016 : '/utf-8' and '/source-charset:utf-8' command-line options are incompatible
# see note in core/meson.build
  # add_project_arguments('/utf-8', language: 'cpp')
  # add_project_arguments('/utf-8', language: 'c')
endif

# TODO-LDML: skip this check
# if meson.version().version_compare('>= 0.62')
#   dl_dep = dependency('dl', required: false)
#   dl_native_dep = dependency('dl', required: false, native: true)
# else
#   dl_dep = cpp.find_library('dl', required: false)
#   dl_native_dep = cpp_native.find_library('dl', required: false, native: true)
# endif

windows = import('windows')
pkg = import('pkgconfig')

# Compiler flags the users of this library must use.
usage_args = []

if get_option('default_library') == 'static'
  add_project_arguments('-DU_STATIC_IMPLEMENTATION', language: 'c')
  add_project_arguments('-DU_STATIC_IMPLEMENTATION', language: 'cpp')
  usage_args = ['-DU_STATIC_IMPLEMENTATION']
  if cpp.get_argument_syntax() == 'msvc'
    library_prefix = ''
    library_suffix = ''
  elif host_machine.system() == 'darwin'
    # TODO-LDML: should be switchable via meson parameters.
    # TODO-LDML: xcode has a flexibility problem here, so droppinh the suffixes
    library_prefix = 's'
    library_suffix = ''
  else
    library_prefix = 's'
    if get_option('buildtype') == 'debug'
      library_suffix = 'd'
    else
      library_suffix = ''
    endif
  endif
else
  library_prefix = ''
  library_suffix = ''
endif

subdir('source')
