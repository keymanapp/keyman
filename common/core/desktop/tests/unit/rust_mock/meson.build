# Copyright:    © 2021 SIL International.
# Description:  Cross platform build script to compile rust mock API unit tests.
#

defns=['-DKMN_KBP_STATIC']
tests = [
  ['rust-keyboard-api', 'keyboard_api.cpp'],
  ['rust-state-api', 'state_api.cpp']
]

foreach t : tests
  bin = executable(t[0], t[1],
    cpp_args: defns,
    include_directories: [inc, libsrc],
    link_args: links,
    dependencies: [rust_mock_processor],
    objects: lib.extract_all_objects())

  test(t[0], bin)
endforeach
