incdir = include_directories('..', 'common')

if host_machine.system() == 'windows'
  icudata_name = '@0@icudt@1@'.format(library_prefix, library_suffix)
else
  icudata_name = '@0@icudata@1@'.format(library_prefix, library_suffix)
endif

subdir('stubdata')
subdir('common')

subdir('i18n')

## Note: The following subdirs are not used by Keyman (at present)

# subdir('data')
# subdir('io')
# subdir('layout')
# subdir('layoutex')
# subdir('test')
# subdir('tools')
