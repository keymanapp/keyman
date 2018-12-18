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

copyfile = ['-c', 'import sys, shutil', 'shutil.copyfile(*sys.argv[1:])']
kmcomp = find_program('kmcomp.exe', required: false)

tests = [
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
  '031 - caps lock',
#  '032 - caps control',   # TODO: support capsononly, capsalwaysoff, shiftfreescaps
#  '033 - caps alwyas off' # TODO: support capsononly, capsalwaysoff, shiftfreescaps
  '034 - options double set reset',
  '035 - options double set staged',
  '036 - options - double reset staged',
  '037 - options - double reset',
  '038 - punctkeys',
  '039 - generic ctrlalt'
]

if build_machine.system() == 'windows'
  kmcomp_cmd = [kmcomp]
else
  wine = find_program('wine', required: false)
  kmcomp_cmd = [wine, kmcomp]
  if not wine.found()
    # We assign the unfound wine external program object to kmcomp
    # to cause the kmcomp object to fail too, as without wine we
    # cannot execute kmcomp.exe on non-Windows systems.
    # NOTE: a disabler() would be ideal here, but we need to support
    # pre 0.45 versions of meson.
    kmcomp = wine
  endif
endif

if kmcomp.found()
  foreach kbd : tests
    kbd_src = files(kbd + '.kmn')
    kbd_obj = join_paths(meson.current_source_dir(),kbd) + '.kmx'
    kbd_log = custom_target(kbd + '.kmx'.underscorify(),
      output: kbd + '.log',
      input: kbd_src,
      command: kmcomp_cmd + ['-s', '@INPUT@', kbd_obj, '@OUTPUT@']
    )

    test(kbd, kmx, depends: kbd_log, args: [kbd_src, kbd_obj])
  endforeach
else
  foreach kbd : tests
    kbd_src = files(kbd + '.kmn')
    kbd_obj = files(kbd + '.kmx')

    test(kbd, kmx, args: [kbd_src, kbd_obj])
  endforeach
endif
