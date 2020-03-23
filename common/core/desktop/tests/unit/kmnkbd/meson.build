# Copyright:    © 2018 SIL International.
# Description:  Cross platform build script to compile libkmnkbp API unit tests.
# Create Date:  19 Oct 2018
# Authors:      Tim Eves (TSE)
# History:      19  Oct 2018 - TSE - Added test for context API functions.
#

defns=['-DKMN_KBP_STATIC']
tests = [
  ['context-api', 'context_api.cpp'],
  ['keyboard-api', 'keyboard_api.cpp'],
#  ['options-api', 'options_api.cpp'],
  ['state-api', 'state_api.cpp']
]

foreach t : tests
  bin = executable(t[0], t[1],
    cpp_args: defns,
    include_directories: [inc, libsrc],
    link_args: links,
    objects: lib.extract_all_objects())

  test(t[0], bin)
endforeach
