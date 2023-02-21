# Copyright:    © 2018-2022 SIL International.
# Description:  Cross platform build script to compile libkmnkbp, documentation
#               and tests.
# Create Date:  2 Oct 2018
# Authors:      Tim Eves (TSE)
#

project('keyboardprocessor', 'cpp', 'c',
        version: run_command(find_program('getversion.bat', 'getversion.sh'), check:true).stdout().strip(),
        license: 'MIT',
        default_options : ['buildtype=release',
                           'cpp_std=c++14',
                           'b_vscrt=static_from_buildtype',
                           'warning_level=2'],
        meson_version: '>=0.53.0')

compiler = meson.get_compiler('cpp')

lib_version = '0.0.0'

py = import('python')
python = py.find_installation()

# Once we can assume meson 0.60 we can delete this
# (https://mesonbuild.com/Release-notes-for-0-60-0.html#msvc-compiler-now-assumes-utf8-source-code-by-default)
if compiler.get_id() == 'msvc'
  add_global_arguments('/source-charset:utf-8', language: ['c', 'cpp'])
endif

message('host_machine.system(): ' + host_machine.system())
message('compiler.get_id(): ' + compiler.get_id())

cc = meson.get_compiler('c')

# TODO: Shared includes may use namespaces, etc which need future tidyup.
# For now, we use KMN_KBP to inject the km::kbp::kmx namespace
defns = ['-DKMN_KBP']

# #define DEBUG when we are on a debug build
if get_option('buildtype') == 'debug'
  add_global_arguments('-DDEBUG', language : 'cpp')
endif

subdir('doc')
subdir('include')
subdir('src')

if get_option('keyman_core_tests')
  message('option "keyman_core_tests" is true, enabling tests')
  subdir('tests')
else
  message('option "keyman_core_tests" is false, disabling tests')
endif