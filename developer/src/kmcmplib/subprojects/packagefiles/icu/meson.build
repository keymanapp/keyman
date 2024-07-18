project(
  'icu',
  'c',
  'cpp',
  version: '73.1',
  meson_version: '>=0.57.0',
  default_options: [ 'cpp_std=c++17' ],
)

U_ICU_VERSION = meson.project_version()
PACKAGE_ICU_DESCRIPTION = 'International Components for Unicode'
PACKAGE_ICU_URL = 'http://icu-project.org'

if host_machine.system() == 'windows'
  add_project_arguments('-DU_PLATFORM_USES_ONLY_WIN32_API', language: 'cpp')
  add_project_arguments('-DWIN32', '-DWIN64', '-D_MBCS', language: 'cpp')
  add_project_arguments('-DU_PLATFORM_USES_ONLY_WIN32_API', language: 'c')
  add_project_arguments('-DWIN32', '-DWIN64', '-D_MBCS', language: 'c')
endif

usage_args = [
  '-DU_STATIC_IMPLEMENTATION',
  '-DU_ATTRIBUTE_DEPRECATED=',
  '-DUCONFIG_USE_LOCAL=1',
  '-DU_NOEXCEPT='
]

library_prefix = ''
library_suffix = ''

subdir('source')
