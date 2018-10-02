e = executable('jsontest', 'jsontest.cpp',
    include_directories: [inc, libsrc],
    objects: lib.extract_objects('json.cpp'))
test('jsontest', e, args: 'jsontest.json')
test('jsontestOutput', python, is_parallel: false, args:
    cmpfiles + ['jsontest.json', join_paths(stnds, 'jsontest.json')])
