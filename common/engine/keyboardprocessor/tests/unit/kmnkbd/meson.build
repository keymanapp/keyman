# Copyright:    © 2018 SIL International.
# Description:  Cross platform build script to compile libkmnkbp API unit tests.
# Create Date:  19 Oct 2018
# Authors:      Tim Eves (TSE)
# History:      19  Oct 2018 - TSE - Added test for context API functions.
#

ctxt = executable('context-api', 'context_api.cpp',
    include_directories: [inc, libsrc],
    dependencies: [kmnkbp])
test('context-api', ctxt)
