# Copyright:    © 2018 SIL International.
# Description:  Cross platform build script to compile libkmnkbp API unit tests.
# Create Date:  19 Oct 2018
# Authors:      Marc Durdin, Tim Eves (TSE)
# History:      19  Oct 2018 - TSE - Added test for context API functions.
#

kmx = executable('kmx', 'kmx.cpp',
    cpp_args: defns,
    include_directories: [inc, libsrc],
    link_args: links,
    objects: lib.extract_all_objects())
    
kmns = [
  '000 - null keyboard',
  '001 - basic input UnicodeI',
  '002 - basic input Unicode',
  '003 - nul',
  '004 - basic input (shift 2)',
  '005 - nul with initial context',
  '006 - vkey input (shift ctrl)',
  '007 - vkey input (ctrl alt)',
  '008 - vkey input (ctrl alt 2)',
  '012 - ralt',
  '013 - deadkeys',
  '014 - groups and virtual keys',
  '015 - ralt 2',
  '017 - space mnemonic kbd',
  '018 - nul testing',
  '019 - multiple deadkeys',
  '020 - deadkeys and backspace',
  
  '021 - options',
  '022 - options with preset',
  '023 - options with save',
  '024 - options with save and preset',
  '025 - options with reset',

  '026 - system stores',
  '027 - system stores 2',
  
  '028 - smp',
  '029 - beep',
  
  '030 - multiple groups',
  '031 - caps lock'
#  '032 - caps control',   # TODO: support capsononly, capsalwaysoff, shiftfreescaps
#  '033 - caps alwyas off' # TODO: support capsononly, capsalwaysoff, shiftfreescaps
]

prog_kmcomp = find_program('kmcomp', required: false)

if prog_kmcomp.found()
  warning('Compiled keyboards are not automatically added to the repo. Copy them from the build dir if you are committing any changes to tests.')
  foreach kmn : kmns 
    custom_target(
      kmn + '.kmx',
      build_by_default: true,
      input: kmn + '.kmn',
      output: kmn + '.kmx',
      command : [prog_kmcomp, '-s', '@INPUT@', '@OUTPUT@']
    )
    configure_file(
      copy: true,
      input:  kmn + '.kmn',
      output: kmn + '.kmn'
    )

    test(kmn, kmx, args: [kmn, meson.build_root()])
  endforeach
else
  warning('kmcomp was not found. Using existing compiled keyboards and changes to kmn files will be ignored.')
  foreach kmn : kmns
    test(kmn, kmx, args: [kmn, meson.source_root()])
  endforeach
endif
