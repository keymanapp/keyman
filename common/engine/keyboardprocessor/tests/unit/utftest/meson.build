# Copyright:    © 2018 SIL International.
# Description:  Cross platform build script to compile utfcode unit test.
# Create Date:  2 Oct 2018
# Authors:      Tim Eves (TSE)
#

e = executable('utftest', 'utftest.cpp',
               objects: lib.extract_objects('utfcodec.cpp'),
               include_directories: [libsrc])
test('utftest', e)
