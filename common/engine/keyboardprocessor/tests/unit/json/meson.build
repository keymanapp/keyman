# Copyright:    © 2018 SIL International.
# Description:  Cross platform build script to compile json unit tests.
# Create Date:  2 Oct 2018
# Authors:      Tim Eves (TSE)
#

e = executable('jsontest', 'jsontest.cpp',
    include_directories: [libsrc],
    objects: lib.extract_objects('json.cpp'))
test('jsontest', e, args: 'jsontest.json')
test('jsontestOutput', python, is_parallel: false, args:
    cmpfiles + ['jsontest.json', join_paths(stnds, 'jsontest.json')])
