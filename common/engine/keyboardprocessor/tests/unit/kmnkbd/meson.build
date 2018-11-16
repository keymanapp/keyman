# Copyright:    © 2018 SIL International.
# Description:  Cross platform build script to compile libkmnkbp API unit tests.
# Create Date:  19 Oct 2018
# Authors:      Tim Eves (TSE)
# History:      19  Oct 2018 - TSE - Added test for context API functions.
#

ctxt = executable('context-api', 'context_api.cpp',
    include_directories: [inc, libsrc],
    objects: lib.extract_objects('km_kbp_context_api.cpp', 'json.cpp'))
keyb = executable('keyboard-api', 'keyboard_api.cpp',
    include_directories: [inc, libsrc],
    link_args: links,
    #objects: lib.extract_objects('km_kbp_keyboard_api.cpp', 'json.cpp', 'keyboard.cpp', 'keyboardprocessor.cpp', 'mock/mock_processor.cpp'))
    objects: lib.extract_all_objects())
opts = executable('options-api', 'options_api.cpp',
    include_directories: [inc, libsrc],
    link_args: links,
    objects: lib.extract_objects('km_kbp_options_api.cpp', 'option.cpp', 'json.cpp'))
state = executable('state-api', 'state_api.cpp',
    include_directories: [inc, libsrc],
    link_args: links,
    objects: lib.extract_all_objects())

test('context-api', ctxt)
test('keyboard-api', keyb)
test('options-api', opts)
test('state-api', state)
