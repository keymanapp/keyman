# Copyright:    © 2018 SIL International.
# Description:  Cross platform build script to compile libkmnkbp API unit tests.
# Create Date:  19 Oct 2018
# Authors:      Tim Eves (TSE)
# History:      19  Oct 2018 - TSE - Added test for context API functions.
#

defns=['-DKMN_KBP_STATIC']


ctxt = executable('context-api', 'context_api.cpp',
    cpp_args: defns,
    include_directories: [inc, libsrc],
    link_args: links,
    objects: lib.extract_all_objects())
keyb = executable('keyboard-api', 'keyboard_api.cpp',
    cpp_args: defns,
    include_directories: [inc, libsrc],
    link_args: links,
    objects: lib.extract_all_objects())
opts = executable('options-api', 'options_api.cpp',
    cpp_args: defns,
    include_directories: [inc, libsrc],
    link_args: links,
    objects: lib.extract_all_objects())
state = executable('state-api', 'state_api.cpp',
    cpp_args: defns,
    include_directories: [inc, libsrc],
    link_args: links,
    objects: lib.extract_all_objects())

test('context-api', ctxt)
test('keyboard-api', keyb)
test('options-api', opts)
test('state-api', state)
