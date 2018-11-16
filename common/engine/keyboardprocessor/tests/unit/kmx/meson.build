# Copyright:    © 2018 SIL International.
# Description:  Cross platform build script to compile libkmnkbp API unit tests.
# Create Date:  19 Oct 2018
# Authors:      Marc Durdin, Tim Eves (TSE)
# History:      19  Oct 2018 - TSE - Added test for context API functions.
#

kmx = executable('kmx', 'kmx.cpp',
    include_directories: [inc, libsrc],
    link_args: links,
    objects: lib.extract_all_objects())

kmn_file = configure_file (
    input : '000.kmn',
    output : '000.kmn',
	copy : true,
	install : true)

kmx_file = configure_file (
    input : '000.kmx',
    output : '000.kmx',
	copy : true,
	install : true)

test('kmx', kmx)
