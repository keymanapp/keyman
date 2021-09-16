# Copyright:    © 2018 SIL International.
# Description:  Cross platform build script to compile libkmnkbp API unit tests.
# Create Date:  19 Oct 2018
# Authors:      Tim Eves (TSE)
#

defns=['-DKMN_KBP_STATIC']
tests = [
  ['kmx_xstring', 'test_kmx_xstring.cpp'],
]
 
foreach t : tests
  bin = executable(t[0], t[1],
    cpp_args: defns,
    include_directories: [inc, libsrc],
    link_args: links,
    dependencies: [rust_mock_processor],
    objects: lib.extract_all_objects())

  test(t[0], bin, args: ['--color', meson.current_source_dir()])
endforeach
