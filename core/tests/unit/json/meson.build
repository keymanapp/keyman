# Copyright:    © 2018 SIL International.
# Description:  Cross platform build script to compile json unit tests.
# Create Date:  2 Oct 2018
# Authors:      Tim Eves (TSE)
#

if cpp_compiler.get_id() == 'emscripten'
  tests_flags = ['-lnodefs.js']
else
  tests_flags = []
endif

e = executable('jsontest', 'jsontest.cpp',
    include_directories: [libsrc],
    link_args: links + tests_flags,
    objects: lib.extract_objects('jsonpp.cpp'))
test('jsontest', e, args: 'jsontest.json')
test('jsontestOutput', python, is_parallel: false, args:
    cmpfiles + ['jsontest.json', join_paths(stnds, 'jsontest.json')])
