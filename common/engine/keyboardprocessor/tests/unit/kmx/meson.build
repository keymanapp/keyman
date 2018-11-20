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
  '025 - options with reset'
]

kmn_tests = []
kmn_sources = []

prog_kmcomp = find_program('kmcomp')

foreach kmn : kmns 
  kmn_tests += configure_file(
    output: kmn + '.kmx',
    input: 'kmn/' + kmn + '.kmn',
    command : [prog_kmcomp, '-s', '@INPUT@', '@OUTPUT@']
  )
  
  kmn_sources += configure_file(
    input: 'kmn/' + kmn + '.kmn',
    output: kmn + '.kmn',
    configuration: configuration_data()
  )
endforeach

test('kmx', kmx, args: [kmns])
