# Copyright:    © 2018 SIL International.
# Description:  Cross platform build script to compile utfcode unit test.
# Create Date:  2 Oct 2018
# Authors:      Tim Eves (TSE)
#

e = executable('utftest', 'utftest.cpp',
               objects: lib.extract_objects('../../common/cpp/utfcodec.cpp'),
               include_directories: [inc, libsrc])
test('utftest', e)
