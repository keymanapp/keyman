#
# Keyman is copyright (C) SIL Global. MIT License.
#
# Created by Tim Eves (TSE) on 2018-10-19
#
# Cross platform build script to compile libkeymancore API unit tests.
#

# build .kmn test fixtures for various tests

keyboard_fixtures = [
  'kmx_key_list',  # used by key_list_tests
  'kmx_imsample',  # used by kmx_imx_tests
  common_test_keyboards_baseline / 'k_0702___caps_always_off' # used by kmx_external_event_tests
]

keyboard_fixture_log = []

foreach keyboard_fixture : keyboard_fixtures
  kbd_name = fs.name(keyboard_fixture)
  kbd_src = files(keyboard_fixture + '.kmn')
  kbd_obj = join_paths(meson.current_build_dir(), kbd_name) + '.kmx'
  keyboard_fixture_log += custom_target((keyboard_fixture + '.kmx').underscorify(),
    output: kbd_name + '.kmx',
    input: kbd_src,
    command: kmc_cmd + ['build', '--debug', '--no-compiler-version', '@INPUT@', '--out-file', kbd_obj]
  )
endforeach


test_api_defns = ['-DKM_CORE_LIBRARY_STATIC']
tests = [
  ['action-api-tests', 'action_api.tests.cpp'],
  ['action-set-api-tests', 'action_set_api.tests.cpp'],
  ['context-api-tests', 'context_api.tests.cpp'],
  ['keyboard-api-tests', 'keyboard_api.tests.cpp'],
  ['options-api-tests', 'options_api.tests.cpp'],
  ['state-api-tests', 'state_api.tests.cpp'],
  ['state-context-api-tests', 'state_context_api.tests.cpp'],
  ['debug-api-tests', 'debug_api.tests.cpp'],
  ['kmx_xstring-tests', 'kmx_xstring.tests.cpp'],
  ['kmx_context-tests', 'kmx_context.tests.cpp'],
  ['actions_normalize-tests', 'actions_normalize.tests.cpp'],

  # renamed to avoid heuristic that causes "update" in a filename to require
  # elevation on Windows! this could also be avoided by adding a manifest
  # resource to each executable (e.g. with mt.exe)
  # https://learn.microsoft.com/en-us/windows/security/application-security/application-control/user-account-control/architecture#installer-detection-technology
  ['actions_updatx_app_context_nfu-tests', 'actions_update_app_context_nfu.tests.cpp'],

  ['actions_get_api-tests', 'actions_get_api.tests.cpp'],
  ['km_core_keyboard_api-tests', 'km_core_keyboard_api.tests.cpp'],
  ['km_core_process_event-tests', 'km_core_process_event.tests.cpp'],
  ['key_list-tests', 'kmx_key_list.tests.cpp'],
  ['kmx_imx-tests', 'kmx_imx.tests.cpp'],
  ['kmx_external_event-tests', 'kmx_external_event.tests.cpp'],
]

foreach t : tests
  bin = executable(t[0], [t[1], common_test_files],
    cpp_args: defns + test_api_defns + test_warns,
    include_directories: test_includes,
    link_args: links + test_link_args,
    dependencies: test_dependencies,
    objects: lib.extract_all_objects(recursive: false))

  test(t[0], bin, depends: keyboard_fixture_log, suite: 'api', protocol: 'gtest')
endforeach

